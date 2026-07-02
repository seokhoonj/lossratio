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
(:func:`lossratio._kernels.smooth.penalized_irls`): the design is
``B = [duration one-hot | covariate dummies]`` and the penalty is the identity
on the covariate block, zero on the duration block. With NO covariates the
design is the saturated duration one-hot and ``exp(s_k) = sum(dLoss_k) /
sum(exposure_k)`` -- exactly :func:`lossratio._kernels.engine.saturated_intensity`, so
the covariate path nests the current pooled intensity cell-for-cell.

This module is the standalone, separately-tested numeric core; the
``CredibleLoss`` / ``SmoothLoss`` integration (design assembly from the
triangle's own reporting x covariate sub-cells, the credibility level on the
covariate-adjusted mean, the marginalized projection) is wired on top of it.
"""
from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass

import numpy as np
import polars as pl

from .smooth import bspline_design, penalized_irls

# weak-prior cap on the EB covariate-effect variance (dimensionless, on the
# log-relativity scale): bounds a level's typical |log-relativity| to ~sqrt(cap)
# (~e^5 ~ 150x) so a near-separated level stays finite, without over-shrinking a
# real data-rich signal (whose sigma^2 is far smaller). Book-invariant.
_SIGMA2_MAX = 25.0


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
    levels: dict[str, list]
    beta: dict[tuple[str, object], float]
    converged: bool

    def intensity(self, duration: int, cell: dict[str, object] | None = None) -> float:
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
    covariates: dict[str, np.ndarray],
) -> tuple[list[np.ndarray], dict[str, list], list[tuple[str, object]]]:
    """Treatment-coded covariate dummy columns (reference level dropped).

    Returns ``(blocks, levels, beta_cols)``: one ``(n, 1)`` indicator block per
    non-reference level, ``levels`` the per-covariate sorted level list
    (reference = first), ``beta_cols`` the ``(covariate, level)`` labels in
    column order."""
    blocks: list[np.ndarray] = []
    levels: dict[str, list] = {}
    beta_cols: list[tuple[str, object]] = []
    for name, codes in covariates.items():
        uniq = sorted(np.unique(codes).tolist(), key=lambda v: str(v))
        levels[name] = uniq
        for lv in uniq[1:]:                       # drop the first level (reference)
            blocks.append((codes == lv).astype(np.float64)[:, None])
            beta_cols.append((name, lv))
    return blocks, levels, beta_cols


def _covariate_design(
    duration: np.ndarray,
    covariates: dict[str, np.ndarray],
) -> tuple[np.ndarray, np.ndarray, list[int], dict[str, list], list[tuple[str, object]]]:
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


def _cov_ridge_diag(
    lam: float | str | dict,
    beta_cols: list[tuple[str, object]],
    B: np.ndarray,
    n_shape: int,
    response: np.ndarray,
    offset: np.ndarray,
    shape_penalty: np.ndarray | None = None,
) -> tuple[np.ndarray, bool]:
    """Per-covariate-column ridge penalty diagonal. Returns ``(ridge, converged)``.

    ``lam = "auto"`` -> data-estimated random-effect shrinkage (Schall 1991 EB,
    see :func:`_eb_ridge_diag`; ``shape_penalty`` carries the smooth shape
    penalty so the EB variance matches the deployed shape). Otherwise ``lam`` is
    a fixed dimensionless ridge -- a scalar (uniform) or a ``{covariate: lam}``
    dict (unlisted -> 0). The fixed penalty on a column is ``lam_c * w_j`` where
    ``w_j`` is that level's fitted data weight (``sum mu``), so a fixed ``lam`` is
    book-invariant. All-zero (the default ``lam = 0``) is the fixed-effect MLE.
    ``converged`` is ``True`` for the fixed paths and the EB flag for ``"auto"``.
    """
    ncov = len(beta_cols)
    if ncov == 0:
        return np.zeros(0, dtype=np.float64), True
    # "auto" (scalar or any dict value) -> EB fixed point over the whole block.
    auto = (lam == "auto") or (
        isinstance(lam, dict) and any(v == "auto" for v in lam.values())
    )
    if auto:
        return _eb_ridge_diag(
            response, offset, B, n_shape, beta_cols, shape_penalty=shape_penalty
        )
    if isinstance(lam, dict):
        mult = np.array([float(lam.get(name, 0.0)) for name, _lv in beta_cols])
    else:
        mult = np.full(ncov, float(lam), dtype=np.float64)
    if not np.any(mult > 0.0):
        return np.zeros(ncov, dtype=np.float64), True
    f0 = penalized_irls(
        response, offset, B, np.zeros((B.shape[1], B.shape[1])), 0.0
    )
    # AVERAGE covariate-level weight (tr(B_cov' W B_cov) / ncov, the smooth
    # scaling). A per-column weight would give a sparse / separated level a near-
    # zero ridge -- no protection where it is needed most; the average gives it a
    # typical-strength ridge that dominates its thin likelihood and stays finite.
    scale = float((B[:, n_shape:].T @ f0.mu).sum()) / ncov
    return mult * scale, True


def _eb_ridge_diag(
    response: np.ndarray,
    offset: np.ndarray,
    B: np.ndarray,
    n_shape: int,
    beta_cols: list[tuple[str, object]],
    *,
    shape_penalty: np.ndarray | None = None,
    max_iter: int = 40,
    tol: float = 1e-2,
) -> tuple[np.ndarray, bool]:
    """Empirical-Bayes (random-effect) ridge for the covariate block.

    Treats each covariate's level effects as a random effect with its own
    variance ``sigma_c^2`` and estimates it from the data by Schall's (1991)
    PQL variance-component fixed point -- the GLM realization of credibility for
    a multi-level factor (Ohlsson 2008). With ``X`` the unpenalized duration
    shape and ``Z`` the covariate dummies, iterate::

        ridge_c = phi / sigma_c^2                     (penalty on covariate c)
        fit (X|Z) by penalized IRLS;  C = (B'WB + diag(ridge))^{-1}
        sigma_c^2 <- (b_c' b_c) / (q_c - ridge_c * sum_j C_jj)   (Schall 1991)
        phi <- Pearson / (n - edf)

    A data-rich, well-separated covariate keeps a large ``sigma_c^2`` (light
    shrinkage); a noisy / sparse high-cardinality covariate gets a small
    ``sigma_c^2`` (strong shrinkage toward the reference) -- automatic, not a
    hand-set ``lam``. Returns ``(ridge, converged)`` -- the per-column ridge
    diagonal (length ``ncov``) and the EB fixed-point convergence flag.

    ``shape_penalty`` (a full ``n_shape x n_shape`` matrix, e.g. ``lam_smooth *
    P2`` on the smooth path) is applied to the duration-shape block so the EB
    variance is estimated against the SAME shape model that is deployed; the
    Schur complement is then against ``X'WX + shape_penalty`` (still exact). For
    the saturated / credible / pooled paths it is ``None`` (unpenalized shape).
    """
    ncov = len(beta_cols)
    names = [nm for nm, _lv in beta_cols]
    uniq = list(dict.fromkeys(names))
    cols_of = {c: np.array([j for j, nm in enumerate(names) if nm == c], dtype=int)
               for c in uniq}
    n = response.size
    p = B.shape[1]
    pen_shape = np.zeros((p, p))
    if shape_penalty is not None:
        pen_shape[:n_shape, :n_shape] = shape_penalty
    cov_idx = n_shape + np.arange(ncov)

    # seed: a moderate Tikhonov ridge (average covariate-level data weight) so
    # the first fit is well-conditioned even under separation pressure.
    f0 = penalized_irls(response, offset, B, pen_shape, 1.0)
    scale = max(float((B[:, n_shape:].T @ f0.mu).sum()) / max(ncov, 1), 1e-8)
    ridge = np.full(ncov, scale, dtype=np.float64)

    converged = False
    for _ in range(max_iter):
        pen_mat = pen_shape.copy()
        pen_mat[cov_idx, cov_idx] = ridge
        fit = penalized_irls(response, offset, B, pen_mat, 1.0)
        phi = max(fit.pearson / max(n - fit.edf, 1.0), 1e-8)
        A = (B.T * fit.mu) @ B
        M = A + pen_mat
        try:
            Cd = np.diag(np.linalg.inv(M))
        except np.linalg.LinAlgError:
            Cd = np.diag(np.linalg.pinv(M))
        new = ridge.copy()
        for c in uniq:
            cc = cols_of[c]
            full = n_shape + cc
            b = fit.beta[full]
            tr = float(np.sum(ridge[cc] * Cd[full]))      # ridge_c * sum_j C_jj
            denom = max(len(cc) - tr, 1e-3)
            s2 = max(float(b @ b) / denom, 1e-12)
            # cap sigma^2 (floor the ridge) so a near-separated level cannot run
            # to no-shrinkage / +-inf -- the EB analogue of a weak prior. The cap
            # is on the DIMENSIONLESS log-relativity variance (sigma^2 of beta),
            # so it is book-invariant: ``_SIGMA2_MAX`` bounds a level's typical
            # |log-relativity| to ~sqrt(cap) and never over-shrinks a real,
            # data-rich signal on a large-premium book.
            s2 = min(s2, _SIGMA2_MAX)
            new[cc] = phi / s2
        if np.allclose(new, ridge, rtol=tol):
            ridge = new
            converged = True
            break
        ridge = 0.5 * ridge + 0.5 * new                    # damped fixed point
    return ridge, converged


def fit_covariate_intensity(
    response: np.ndarray,
    exposure: np.ndarray,
    duration: np.ndarray,
    covariates: dict[str, np.ndarray],
    *,
    lam: float | str | dict = 0.0,
    n_basis: int | None = None,
    lam_smooth: float | str = "auto",
    degree: int = 3,
) -> CovariateFit:
    """Penalized quasi-Poisson log-link GLM of the covariate-adjusted intensity.

    ``response`` = per-cell incremental loss, ``exposure`` = per-cell from-cell
    cumulative premium (> 0), ``duration`` = per-cell from-duration,
    ``covariates`` = ``{name: per-cell level codes}`` (empty dict = pooled
    intensity, exact nesting). Cells with non-positive exposure are dropped.

    Covariates enter as treatment-coded FIXED effects on a shared duration shape,
    with the per-cohort credibility level the only shrinkage: the exposure-
    weighted credibility of Buhlmann-Straub (1970) on the cohort level, combined
    with a GLM for the duration shape and the covariate effects (the GLM +
    credibility combination, Ohlsson 2008). ``lam`` (default ``0`` = pure MLE
    fixed effects) optionally shrinks the covariate block toward zero relativity.
    Shrinking a high-cardinality rating factor's level effects this way is
    established actuarial practice -- GLM + credibility for multi-level factors
    (Ohlsson 2008), the credibility / ridge realization of a random factor effect
    (cf. Hachemeister 1975 regression credibility). ``lam`` is a scalar (uniform)
    or a ``{covariate: lam}`` dict (per-covariate), data-scaled by the average
    covariate-level weight so the value is dimensionless / book-invariant.

    With ``n_basis=None`` (default) the duration shape is the saturated one-hot
    (unpenalized). With ``n_basis`` set the duration block is a clamped B-spline
    with a 2nd-difference (P-spline) penalty whose strength ``lam_smooth`` is a
    fixed float or GCV-selected (``"auto"``) -- the smooth covariate path.
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
    if response.size == 0:
        # no fittable cells (every cell dropped by the exposure>0 / finite
        # filter, e.g. an empty or too-thin segment): return a degenerate fit
        # with no durations, so intensity() is NaN everywhere and the projection
        # falls to a gap -- never build an empty design / call the IRLS solver.
        levels = {
            name: sorted(np.unique(c).tolist(), key=str)
            for name, c in covariates.items()
        }
        return CovariateFit(
            durations=[], s=np.zeros(0, dtype=np.float64), levels=levels,
            beta={}, converged=False,
        )
    offset = np.log(exposure)

    if n_basis is None:
        B, penalty_diag, durations, levels, beta_cols = _covariate_design(
            duration, covariates
        )
        n_dur = len(durations)
        penalty_diag = penalty_diag.copy()
        ridge_d, eb_conv = _cov_ridge_diag(
            lam, beta_cols, B, n_dur, response, offset
        )
        penalty_diag[n_dur:] = ridge_d
        fit = penalized_irls(response, offset, B, np.diag(penalty_diag), 1.0)
        s = fit.beta[:n_dur]
        beta = {col: float(fit.beta[n_dur + j]) for j, col in enumerate(beta_cols)}
        return CovariateFit(
            durations=durations, s=s, levels=levels, beta=beta,
            converged=bool(fit.converged) and eb_conv,
        )

    # smooth duration basis: B = [B-spline | covariate dummies], penalty =
    # blockdiag(lam_smooth * P2, diag(ridge)); pass lam = 1 so the assembled
    # penalty applies as-is.
    durations = sorted(int(d) for d in np.unique(duration))
    nb = max(degree + 1, min(int(n_basis), len(durations)))
    Bdur, P2 = bspline_design(duration, nb, degree)
    nb = Bdur.shape[1]                            # bspline collapses to 1 col for a single duration
    blocks, levels, beta_cols = _covariate_dummies(covariates)
    ncov = len(beta_cols)
    B = np.hstack([Bdur, *blocks]) if blocks else Bdur
    is_auto_cov = (lam == "auto") or (
        isinstance(lam, dict) and any(v == "auto" for v in lam.values())
    )
    # initial ridge (EB against the unpenalized shape, or fixed) to seed the
    # lam_smooth GCV.
    ridge_diag, eb_conv = _cov_ridge_diag(lam, beta_cols, B, nb, response, offset)

    def _pen(ls: float) -> np.ndarray:
        pen = np.zeros((nb + ncov, nb + ncov), dtype=np.float64)
        pen[:nb, :nb] = ls * P2
        if ncov:
            pen[nb:, nb:] = np.diag(ridge_diag)
        return pen

    # select the smoothing parameter lam_smooth (GCV or fixed)
    if lam_smooth != "auto":
        ls_star = float(lam_smooth)
    else:
        f0 = penalized_irls(response, offset, B, _pen(0.0), 1.0)
        BtWB = (Bdur.T * f0.mu) @ Bdur
        tr_p = float(np.trace(P2))
        scale = (float(np.trace(BtWB)) / tr_p) if tr_p > 0 else 1.0
        n = response.size
        best_gcv = np.inf
        ls_star = 0.0
        for lg in scale * np.geomspace(1e-4, 1e4, 13):
            f = penalized_irls(response, offset, B, _pen(float(lg)), 1.0)
            denom = n - f.edf
            gcv = np.inf if denom <= 0 else n * f.pearson / denom ** 2
            if gcv < best_gcv:
                best_gcv, ls_star = gcv, float(lg)

    # auto: re-estimate the EB ridge under the SELECTED smoothing penalty so the
    # covariate variance matches the deployed (penalized) shape, then refit.
    if is_auto_cov and ncov:
        ridge_diag, eb_conv = _cov_ridge_diag(
            lam, beta_cols, B, nb, response, offset, shape_penalty=ls_star * P2,
        )
    fit = penalized_irls(response, offset, B, _pen(ls_star), 1.0)

    spline_coef = fit.beta[:nb]
    Bk, _ = bspline_design(np.array(durations, dtype=np.float64), nb, degree)
    s = Bk @ spline_coef
    beta = {col: float(fit.beta[nb + j]) for j, col in enumerate(beta_cols)}
    return CovariateFit(
        durations=durations, s=s, levels=levels, beta=beta,
        converged=bool(fit.converged) and eb_conv,
    )


@dataclass
class SegmentCovariateData:
    """Per-segment covariate link data (reused by the point fit and the
    bootstrap). ``response`` / ``exposure`` / ``duration`` are the sub-cell loss links
    (response = ``dLoss``, exposure = from-cell cumulative premium, ``duration`` =
    1-based from-duration), ``coh_idx`` the cohort row index per link, ``codes``
    the per-covariate level codes per link. ``by_cohort_duration`` maps ``(cohort_value,
    from-duration) -> [(cell, from-premium)]`` and ``last_obs`` the cohort's last
    observed from-duration -- together they carry the premium mix used to
    collapse ``g_d(x)`` to ``g_marginal``.
    """

    response: np.ndarray
    exposure: np.ndarray
    duration: np.ndarray
    coh_idx: np.ndarray
    codes: dict[str, np.ndarray]
    by_cohort_duration: dict[tuple, list[tuple[dict, float]]]
    last_obs: dict[object, int]
    cohorts: list
    n_links: int


def covariate_segment_data(
    sub_cells: pl.DataFrame,
    covariates: list[str],
    cohorts: list,
    n_links: int,
) -> SegmentCovariateData:
    """Build the sub-cell loss links + premium-mix structure for one segment."""
    keys = ["cohort", *covariates]
    s = sub_cells.sort([*keys, "duration"]).with_columns(
        pl.col("incr_loss").cum_sum().over(keys).alias("_cum_loss"),
        pl.col("incr_premium").cum_sum().over(keys).alias("_cum_premium"),
    )
    # links: row at from-duration d joined with the to-cell cumulative loss at
    # d+1 -- response = dLoss over the link, exposure = from-cell cumulative
    # premium, from-duration = d (mirrors _segment_factor_links exactly).
    nxt = s.select([
        *keys,
        (pl.col("duration") - 1).alias("duration"),
        pl.col("_cum_loss").alias("_cum_loss_to"),
    ])
    links = (
        s.join(nxt, on=[*keys, "duration"], how="inner")
        .with_columns((pl.col("_cum_loss_to") - pl.col("_cum_loss")).alias("_resp"))
        .filter(pl.col("_cum_premium") > 0)
    )
    coh_pos = {c: i for i, c in enumerate(cohorts)}
    coh_idx = np.array(
        [coh_pos.get(c, -1) for c in links["cohort"].to_list()], dtype=np.int64
    )

    by_cohort_duration: dict[tuple, list[tuple[dict, float]]] = defaultdict(list)
    last_obs: dict[object, int] = {}
    for r in s.select(["cohort", "duration", "_cum_premium", *covariates]).iter_rows(named=True):
        cp = float(r["_cum_premium"])
        d = int(r["duration"])
        cell = {c: r[c] for c in covariates}
        by_cohort_duration[(r["cohort"], d)].append((cell, cp))
        if cp > 0 and d > last_obs.get(r["cohort"], 0):
            last_obs[r["cohort"]] = d

    return SegmentCovariateData(
        response=links["_resp"].to_numpy(),
        exposure=links["_cum_premium"].to_numpy(),
        duration=links["duration"].to_numpy(),
        coh_idx=coh_idx,
        codes={c: links[c].to_numpy() for c in covariates},
        by_cohort_duration=by_cohort_duration,
        last_obs=last_obs,
        cohorts=cohorts,
        n_links=n_links,
    )


def build_g_marginal(cov_fit: CovariateFit, cov_data: SegmentCovariateData) -> np.ndarray:
    """Collapse the per-cell intensity ``g_d(x)`` to the 2-D marginal intensity
    ``g_marginal[i, k] = sum_x g_d(x) * share[i, d, x]`` (mix frozen beyond a cohort's
    last observed from-duration). ``nan`` where the from-duration is unfittable
    or the cohort has no premium mix."""
    est_durs = set(cov_fit.durations)
    g_marginal = np.full((len(cov_data.cohorts), cov_data.n_links), np.nan, dtype=np.float64)
    for i, coh in enumerate(cov_data.cohorts):
        for k in range(cov_data.n_links):
            d = k + 1                                # from-duration label
            if d not in est_durs:
                continue
            cells = cov_data.by_cohort_duration.get((coh, d))
            if cells is None:                        # projected link: freeze mix
                ld = cov_data.last_obs.get(coh)
                if ld is None:
                    continue
                cells = cov_data.by_cohort_duration.get((coh, ld))
                if cells is None:
                    continue
            tot = sum(cp for _, cp in cells if cp > 0)
            if tot <= 0:
                continue
            g_marginal[i, k] = sum(
                cov_fit.intensity(d, cell) * (cp / tot)
                for cell, cp in cells if cp > 0
            )
    return g_marginal
