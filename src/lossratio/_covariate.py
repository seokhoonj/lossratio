"""Covariate fixed-effect intensity kernel (planned ``covariates=`` feature).

The credible / smooth loss rungs model an incremental-loss intensity
``E[dLoss] = exposure * g_k`` (``g_k`` = per-duration intensity, exposure =
predetermined cumulative premium, log-offset). A ``covariate`` adds cell-level
fixed effects ``X'beta`` to that log-mean:

    E[dLoss_cell] = exposure_cell * exp( s_k + X_cell' beta )

with ``s_k`` the *saturated* per-duration intercept (one free intercept per
observed duration, UNpenalized -- it carries the pooled shape) and ``beta`` the
covariate log-relativities (treatment-coded against a reference level per
covariate, RIDGE-penalized by ``lam`` so sparse / separated high-cardinality
levels shrink toward the reference instead of running to +/-inf).

The estimation is the kept penalized quasi-Poisson IRLS
(:func:`lossratio._smooth.penalized_irls`): the design is
``B = [duration one-hot | covariate dummies]`` and the penalty is the identity
on the covariate block, zero on the duration block. With NO covariates the
design is the saturated duration one-hot and ``exp(s_k) = sum(dLoss_k) /
sum(exposure_k)`` -- exactly :func:`lossratio._engine.saturated_intensity`, so
the covariate path nests the current pooled intensity cell-for-cell.

This module is the standalone, separately-tested numeric core; the
``CredibleLoss`` / ``SmoothLoss`` integration (design assembly from a
disaggregated source frame, the credibility level on the covariate-adjusted
mean, the marginalized projection) is wired on top of it.
"""
from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass

import numpy as np
import polars as pl

from ._io import normalize_groups
from ._period import (
    coerce_cols_to_date,
    count_periods,
    floor_cols_to_period,
    infer_grain,
    resolve_grain,
)
from ._smooth import bspline_design, penalized_irls


@dataclass
class CovariateFit:
    """Result of :func:`fit_covariate_intensity`.

    ``durations`` are the sorted observed from-durations; ``s`` is the
    per-duration intercept aligned to them (``g_k`` at the reference covariate
    cell = ``exp(s)``). ``levels`` maps each covariate name to its level list
    (the first is the dropped reference); ``beta`` maps ``(covariate, level)`` to
    its log-relativity (the reference level is implicitly 0.0 and absent from the
    dict). ``converged`` is the IRLS flag.
    """

    durations: list[int]
    s: np.ndarray
    levels: "dict[str, list]"
    beta: "dict[tuple[str, object], float]"
    converged: bool

    def intensity(self, duration: int, cell: "dict[str, object] | None" = None) -> float:
        """``g_k(x) = exp(s_k + sum_c beta[c, x_c])`` for a covariate cell ``x``
        (a ``{covariate: level}`` mapping; missing / reference levels contribute
        0). Returns ``nan`` for an unobserved duration."""
        idx = {d: i for i, d in enumerate(self.durations)}
        if duration not in idx:
            return float("nan")
        eta = float(self.s[idx[duration]])
        if cell:
            for c, lv in cell.items():
                eta += self.beta.get((c, lv), 0.0)
        return float(np.exp(eta))


def _covariate_dummies(
    covariates: "dict[str, np.ndarray]",
) -> "tuple[list[np.ndarray], dict[str, list], list[tuple[str, object]]]":
    """Treatment-coded covariate dummy columns (reference level dropped).

    Returns ``(blocks, levels, beta_cols)``: one ``(n, 1)`` indicator block per
    non-reference level, ``levels`` the per-covariate sorted level list
    (reference = first), ``beta_cols`` the ``(covariate, level)`` labels in
    column order."""
    blocks: "list[np.ndarray]" = []
    levels: "dict[str, list]" = {}
    beta_cols: "list[tuple[str, object]]" = []
    for name, codes in covariates.items():
        uniq = sorted(np.unique(codes).tolist(), key=lambda v: str(v))
        levels[name] = uniq
        for lv in uniq[1:]:                       # drop the first level (reference)
            blocks.append((codes == lv).astype(np.float64)[:, None])
            beta_cols.append((name, lv))
    return blocks, levels, beta_cols


def _covariate_design(
    duration: np.ndarray,
    covariates: "dict[str, np.ndarray]",
) -> "tuple[np.ndarray, np.ndarray, list[int], dict[str, list], list[tuple[str, object]]]":
    """Build ``B = [duration one-hot | covariate treatment dummies]`` and the
    ridge penalty mask (0 on the saturated duration block, 1 on the covariate
    block).

    Returns ``(B, penalty_diag, durations, levels, beta_cols)`` where
    ``durations`` indexes the duration columns, ``levels`` maps each covariate
    to its sorted level list (reference = first), and ``beta_cols`` labels the
    covariate columns ``(covariate, level)`` in design order.
    """
    n = duration.size
    durations = sorted(int(d) for d in np.unique(duration))
    dcol = {d: j for j, d in enumerate(durations)}
    Xd = np.zeros((n, len(durations)), dtype=np.float64)
    Xd[np.arange(n), [dcol[int(d)] for d in duration]] = 1.0

    blocks, levels, beta_cols = _covariate_dummies(covariates)
    B = np.hstack([Xd, *blocks]) if blocks else Xd
    penalty_diag = np.concatenate([
        np.zeros(len(durations)),                 # saturated duration block: unpenalized
        np.ones(len(beta_cols)),                  # covariate block: ridge-shrunk
    ])
    return B, penalty_diag, durations, levels, beta_cols


def fit_covariate_intensity(
    response: np.ndarray,
    exposure: np.ndarray,
    duration: np.ndarray,
    covariates: "dict[str, np.ndarray]",
    *,
    lam: float = 1.0,
    n_basis: "int | None" = None,
    lam_smooth: "float | str" = "auto",
    degree: int = 3,
) -> CovariateFit:
    """Penalized quasi-Poisson log-link GLM of the covariate-adjusted intensity.

    ``response`` = per-cell incremental loss, ``exposure`` = per-cell from-cell
    cumulative premium (> 0), ``duration`` = per-cell from-duration,
    ``covariates`` = ``{name: per-cell level codes}`` (empty dict = pooled
    intensity, exact nesting). ``lam`` is the ridge strength on the covariate
    coefficients. Cells with non-positive exposure are dropped.

    With ``n_basis=None`` (default) the duration shape is the saturated one-hot
    (unpenalized) -- the credible / pooled covariate path. With ``n_basis`` set
    the duration block is a clamped B-spline of the given ``degree`` with a
    2nd-difference (P-spline) penalty whose strength ``lam_smooth`` is either a
    fixed float or GCV-selected (``"auto"``) -- the smooth covariate path. Either
    way the covariate block stays treatment-coded + ridge-shrunk, so the fitted
    ``s_d`` (smooth or saturated) plus ``X'beta`` give ``g_d(x)`` identically.
    """
    response = np.asarray(response, dtype=np.float64)
    exposure = np.asarray(exposure, dtype=np.float64)
    duration = np.asarray(duration)
    keep = np.isfinite(exposure) & (exposure > 0) & np.isfinite(response)
    if not keep.all():
        response, exposure, duration = response[keep], exposure[keep], duration[keep]
        covariates = {k: np.asarray(v)[keep] for k, v in covariates.items()}
    else:
        covariates = {k: np.asarray(v) for k, v in covariates.items()}
    offset = np.log(exposure)

    if n_basis is None:
        B, penalty_diag, durations, levels, beta_cols = _covariate_design(
            duration, covariates
        )
        fit = penalized_irls(response, offset, B, np.diag(penalty_diag), float(lam))
        n_dur = len(durations)
        s = fit.beta[:n_dur]
        beta = {col: float(fit.beta[n_dur + j]) for j, col in enumerate(beta_cols)}
        return CovariateFit(
            durations=durations, s=s, levels=levels, beta=beta,
            converged=bool(fit.converged),
        )

    # smooth duration basis: B = [B-spline | covariate dummies], penalty =
    # blockdiag(lam_smooth * P2, lam * I); pass lam = 1 so the assembled penalty
    # applies as-is (the cov block stays ridged even at lam_smooth = 0).
    durations = sorted(int(d) for d in np.unique(duration))
    nb = max(degree + 1, min(int(n_basis), len(durations)))
    Bdur, P2 = bspline_design(duration, nb, degree)
    nb = Bdur.shape[1]                            # bspline collapses to 1 col for a single duration
    blocks, levels, beta_cols = _covariate_dummies(covariates)
    ncov = len(beta_cols)
    B = np.hstack([Bdur, *blocks]) if blocks else Bdur

    def _pen(ls: float) -> np.ndarray:
        pen = np.zeros((nb + ncov, nb + ncov), dtype=np.float64)
        pen[:nb, :nb] = ls * P2
        if ncov:
            pen[nb:, nb:] = float(lam) * np.eye(ncov)
        return pen

    if lam_smooth != "auto":
        fit = penalized_irls(response, offset, B, _pen(float(lam_smooth)), 1.0)
    else:
        f0 = penalized_irls(response, offset, B, _pen(0.0), 1.0)
        BtWB = (Bdur.T * f0.mu) @ Bdur
        tr_p = float(np.trace(P2))
        scale = (float(np.trace(BtWB)) / tr_p) if tr_p > 0 else 1.0
        n = response.size
        best_gcv = np.inf
        fit = f0
        for lg in scale * np.geomspace(1e-4, 1e4, 13):
            f = penalized_irls(response, offset, B, _pen(float(lg)), 1.0)
            denom = n - f.edf
            gcv = np.inf if denom <= 0 else n * f.pearson / denom ** 2
            if gcv < best_gcv:
                best_gcv, fit = gcv, f

    spline_coef = fit.beta[:nb]
    Bk, _ = bspline_design(np.array(durations, dtype=np.float64), nb, degree)
    s = Bk @ spline_coef
    beta = {col: float(fit.beta[nb + j]) for j, col in enumerate(beta_cols)}
    return CovariateFit(
        durations=durations, s=s, levels=levels, beta=beta,
        converged=bool(fit.converged),
    )


# ---------------------------------------------------------------------------
# Side-channel integration (architecture B): the estimator keeps the Triangle
# aggregated + byte-identical and reads the RAW disaggregated source frame at
# fit time to recover the per-covariate sub-cells. These two helpers are the
# bridge: re-aggregate the source to (groups, cohort, duration, *covariates)
# cells matching the Triangle's own binning, then per segment collapse the
# fitted per-cell intensity g_k(x) and the cohort's premium mix into a single
# 2-D effective-intensity matrix g_eff[cohort, link] -- so the existing
# cohort x duration credibility + projection machinery is reused unchanged
# (it only sees a per-cohort-modulated intensity, no third axis).
# ---------------------------------------------------------------------------


def reaggregate_source(
    source: "pl.DataFrame | object",
    *,
    groups: "str | list[str] | None",
    cohort: str,
    calendar: "str | None",
    duration: str,
    loss: str,
    premium: str,
    grain: str,
    covariates: "list[str]",
) -> pl.DataFrame:
    """Re-aggregate a raw source frame to disaggregated covariate sub-cells.

    Mirrors the Triangle's own aggregation (floor cohort / calendar to ``grain``,
    derive the 1-based duration, sum loss + premium) but keeps the covariate
    columns as additional grouping keys, so summing the result over the
    covariates reproduces the Triangle's ``(groups, cohort, duration)`` cells
    exactly. Returns a polars frame with the standardized columns
    ``[*group_cols, "cohort", "duration", *covariates, "incr_loss",
    "incr_premium"]`` (``"cohort"`` = the floored cohort Date, ``"duration"`` =
    Int64). ``calendar`` may be ``None`` (mode-2 triangle: the ``duration``
    column is taken directly).
    """
    if not isinstance(source, pl.DataFrame):
        source = pl.from_pandas(source)              # pandas-in mirroring
    group_cols = normalize_groups(groups)

    missing = [c for c in (*group_cols, cohort, loss, premium, *covariates)
               if c not in source.columns]
    if calendar is not None and calendar not in source.columns:
        missing.append(calendar)
    if calendar is None and duration not in source.columns:
        missing.append(duration)
    if missing:
        raise ValueError(
            f"source is missing column(s) required to build the covariate "
            f"cells: {missing}"
        )

    date_cols = [cohort] + ([calendar] if calendar is not None else [])
    df = coerce_cols_to_date(source, date_cols).with_columns(
        pl.col(loss).cast(pl.Float64),
        pl.col(premium).cast(pl.Float64),
    )
    input_grain = infer_grain(df[cohort])
    grain = resolve_grain(input_grain, grain)
    if date_cols and grain != input_grain:
        df = floor_cols_to_period(df, date_cols, grain)

    if calendar is not None:
        df = df.with_columns(
            count_periods(pl.col(cohort), pl.col(calendar), grain)
            .alias("_duration")
        )
    else:
        df = df.with_columns(pl.col(duration).cast(pl.Int64).alias("_duration"))

    keys = [*group_cols, cohort, "_duration", *covariates]
    agg = (
        df.group_by(keys)
        .agg(
            pl.col(loss).sum().alias("incr_loss"),
            pl.col(premium).sum().alias("incr_premium"),
        )
        .rename({cohort: "cohort", "_duration": "duration"})
        .sort([*group_cols, "cohort", "duration", *covariates])
    )
    return agg


def reconcile_coverage(
    cov_cells: pl.DataFrame,
    triangle_frame: pl.DataFrame,
    *,
    groups: "str | list[str] | None",
    rtol: float = 1e-6,
) -> None:
    """Fail fast if the covariate sub-cells do not roll up to the Triangle.

    Summing ``cov_cells`` over the covariates must reproduce the Triangle's own
    ``(groups, cohort, duration)`` increments -- otherwise the covariate mix and
    the marginalized prior mean ``m0_adj`` would be built on cells that disagree
    with the headline fit, and an uncovered cohort would silently project to
    ``nan``. Raises with the first offending key. ``triangle_frame`` is
    ``triangle.to_polars()`` (carries ``incr_loss`` / ``incr_premium``).
    """
    group_cols = normalize_groups(groups)
    keys = [*group_cols, "cohort", "duration"]
    rolled = cov_cells.group_by(keys).agg(
        pl.col("incr_loss").sum().alias("_cl"),
        pl.col("incr_premium").sum().alias("_cp"),
    )
    tri = triangle_frame.select([*keys, "incr_loss", "incr_premium"])
    joined = tri.join(rolled, on=keys, how="left")

    missing = joined.filter(pl.col("_cp").is_null())
    if missing.height:
        raise ValueError(
            f"source does not cover {missing.height} Triangle cell(s) "
            f"(the covariate frame is missing them); first uncovered key "
            f"{dict(zip(keys, missing.select(keys).row(0)))}."
        )
    bad = joined.filter(
        ((pl.col("incr_premium") - pl.col("_cp")).abs()
         > rtol * pl.col("incr_premium").abs() + 1e-6)
        | ((pl.col("incr_loss") - pl.col("_cl")).abs()
           > rtol * pl.col("incr_loss").abs() + 1e-6)
    )
    if bad.height:
        raise ValueError(
            f"source does not sum to the Triangle in {bad.height} cell(s) "
            f"(covariate cells must aggregate to the same loss / premium); "
            f"first mismatch key {dict(zip(keys, bad.select(keys).row(0)))}."
        )


@dataclass
class SegmentCovData:
    """Per-segment covariate link data (reused by the point fit and the
    bootstrap). ``resp`` / ``expo`` / ``dur`` are the sub-cell loss links
    (response = ``dLoss``, exposure = from-cell cumulative premium, ``dur`` =
    1-based from-duration), ``coh_idx`` the cohort row index per link, ``codes``
    the per-covariate level codes per link. ``by_cd`` maps ``(cohort_value,
    from-duration) -> [(cell, from-premium)]`` and ``last_obs`` the cohort's last
    observed from-duration -- together they carry the premium mix used to
    collapse ``g_d(x)`` to ``g_eff``.
    """

    resp: np.ndarray
    expo: np.ndarray
    dur: np.ndarray
    coh_idx: np.ndarray
    codes: "dict[str, np.ndarray]"
    by_cd: "dict[tuple, list[tuple[dict, float]]]"
    last_obs: "dict[object, int]"
    cohorts: list
    n_links: int


def _covariate_segment_data(
    sub_cells: pl.DataFrame,
    covariates: "list[str]",
    cohorts: list,
    n_links: int,
) -> SegmentCovData:
    """Build the sub-cell loss links + premium-mix structure for one segment."""
    keys = ["cohort", *covariates]
    s = sub_cells.sort([*keys, "duration"]).with_columns(
        pl.col("incr_loss").cum_sum().over(keys).alias("_cl"),
        pl.col("incr_premium").cum_sum().over(keys).alias("_cp"),
    )
    # links: row at from-duration d joined with the to-cell cumulative loss at
    # d+1 -- response = dLoss over the link, exposure = from-cell cumulative
    # premium, from-duration = d (mirrors _segment_factor_links exactly).
    nxt = s.select([
        *keys,
        (pl.col("duration") - 1).alias("duration"),
        pl.col("_cl").alias("_cl_to"),
    ])
    links = (
        s.join(nxt, on=[*keys, "duration"], how="inner")
        .with_columns((pl.col("_cl_to") - pl.col("_cl")).alias("_resp"))
        .filter(pl.col("_cp") > 0)
    )
    coh_pos = {c: i for i, c in enumerate(cohorts)}
    coh_idx = np.array(
        [coh_pos.get(c, -1) for c in links["cohort"].to_list()], dtype=np.int64
    )

    by_cd: "dict[tuple, list[tuple[dict, float]]]" = defaultdict(list)
    last_obs: "dict[object, int]" = {}
    for r in s.select(["cohort", "duration", "_cp", *covariates]).iter_rows(named=True):
        cp = float(r["_cp"])
        d = int(r["duration"])
        cell = {c: r[c] for c in covariates}
        by_cd[(r["cohort"], d)].append((cell, cp))
        if cp > 0 and d > last_obs.get(r["cohort"], 0):
            last_obs[r["cohort"]] = d

    return SegmentCovData(
        resp=links["_resp"].to_numpy(),
        expo=links["_cp"].to_numpy(),
        dur=links["duration"].to_numpy(),
        coh_idx=coh_idx,
        codes={c: links[c].to_numpy() for c in covariates},
        by_cd=by_cd,
        last_obs=last_obs,
        cohorts=cohorts,
        n_links=n_links,
    )


def _build_g_eff(covfit: CovariateFit, data: SegmentCovData) -> np.ndarray:
    """Collapse the per-cell intensity ``g_d(x)`` to the 2-D effective intensity
    ``g_eff[i, k] = sum_x g_d(x) * share[i, d, x]`` (mix frozen beyond a cohort's
    last observed from-duration). ``nan`` where the from-duration is unfittable
    or the cohort has no premium mix."""
    est_durs = set(covfit.durations)
    g_eff = np.full((len(data.cohorts), data.n_links), np.nan, dtype=np.float64)
    for i, coh in enumerate(data.cohorts):
        for k in range(data.n_links):
            d = k + 1                                # from-duration label
            if d not in est_durs:
                continue
            cells = data.by_cd.get((coh, d))
            if cells is None:                        # projected link: freeze mix
                ld = data.last_obs.get(coh)
                if ld is None:
                    continue
                cells = data.by_cd.get((coh, ld))
            tot = sum(cp for _, cp in cells if cp > 0)
            if tot <= 0:
                continue
            g_eff[i, k] = sum(
                covfit.intensity(d, cell) * (cp / tot)
                for cell, cp in cells if cp > 0
            )
    return g_eff


def segment_effective_intensity(
    sub_cells: pl.DataFrame,
    covariates: "list[str]",
    cohorts: list,
    n_links: int,
    *,
    lam: float = 1.0,
) -> "tuple[np.ndarray, CovariateFit]":
    """Per-cohort effective intensity ``g_eff[i, k]`` for one segment.

    The covariate intensity kernel is fit on the sub-cell links, then the
    per-cell intensity ``g_d(x) = exp(s_d + X'beta)`` is marginalized by each
    cohort's premium mix into the 2-D ``g_eff`` (see :func:`_build_g_eff`) that
    the kept credibility + projection kernels consume unchanged. Returns
    ``(g_eff, CovariateFit)``."""
    data = _covariate_segment_data(sub_cells, covariates, cohorts, n_links)
    covfit = fit_covariate_intensity(
        data.resp, data.expo, data.dur, data.codes, lam=lam,
    )
    return _build_g_eff(covfit, data), covfit
