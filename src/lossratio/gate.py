"""Ladder-governance gate (charter Sec.6.4).

The structure ladder (PooledLoss -> CredibleLoss -> SmoothLoss) admits a more
complex rung only when it EARNS its place out of sample: it must beat the
simpler rung on the question's primary metric AND not materially degrade the
rest of the panel. :func:`gate` is the machine that decides that -- no human
picks a winner.

It reads the MATCHED per-cell A/E of an
:class:`~lossratio.comparison.EstimatorComparisonFit` (every estimator scored
the same held-out keys) and runs a **cohort-cluster paired bootstrap**: the
resampling unit is the ``(group, cohort)`` cluster (cell-level resampling
understates variance -- cells in a cohort share a refit, a calendar shock, and
the cohort level), the same resampled clusters score both estimators (paired),
and each draw RE-AGGREGATES the matched cells (no refit). From the bootstrap
distribution of the relative metric change it issues a **3-state verdict**:

* ``PASS`` -- primary superiority met and every panel metric non-inferior;
* ``PASS_WITH_TRADEOFF`` -- superiority met but some panel metric degraded
  beyond tolerance (the degraded metrics are named, so a citation that omits
  them is mechanically impossible);
* ``FAIL`` -- superiority not met; ``NO_WINNER`` when the improvement CI
  straddles 0 (the simpler rung wins by default).

Two robustness gates layer on top of the verdict (charter Sec.6.4):

* **convergence hygiene** -- each ``(estimator, holdout)`` refit carries a
  ``converged`` flag; cells from a non-converged fold are dropped before the
  bootstrap, counted (``n_nonconverged`` folds, ``n_excluded`` cells), and if
  the excluded share crosses ``max_nonconverged_frac`` (default 5%) the verdict
  is forced to ``FAIL``;
* **matched-vs-own sensitivity** -- the primary improvement is recomputed on
  each estimator's OWN population (every cell it projected over the common
  folds, not just the matched intersection) via an unpaired cluster bootstrap.
  A challenger that wins on the matched cells but NOT on its own population is
  riding a favourable subset (the canonical case: a benchmark that only reaches
  the easy early durations) -- the split blocks acceptance (forced ``FAIL``).

Deferred (charter Sec.6.4, follow-ups): Holm multiplicity over several primary
metrics, zero-loss handling, per-group verdicts. (The non-negotiable
naive-baseline row lives in :class:`~lossratio.naive_baseline.NaiveBaseline`,
slotted into the comparison upstream.)

Internal during the additive build phase: not exported.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import normalize_groups

if TYPE_CHECKING:
    from .comparison import EstimatorComparisonFit

# Metric -> the per-cluster sufficient-statistic pair (numerator, denominator)
# whose ratio is the pooled metric value. abs_err / ae_err are means (denom = a
# non-null contribution count); bias is the pooled |sum gap / sum expected|.
_METRICS = ("abs_err", "ae_err", "bias")
_LANE_PREFIX = {"cumulative": "", "incremental": "incr_"}


@dataclass
class GateReport:
    """Outcome of a ladder gate (charter Sec.6.4).

    Attributes
    ----------
    verdict
        ``"PASS"`` / ``"PASS_WITH_TRADEOFF"`` / ``"FAIL"``.
    no_winner
        ``True`` when the verdict is ``FAIL`` because the primary improvement
        CI INCLUDES 0 -- the simpler rung is not distinguishably beaten. A CI
        entirely below 0 (challenger decidedly worse) and a CI entirely above 0
        but below ``practical_tol`` (a real-but-immaterial win) are also
        ``FAIL`` but are NOT flagged ``no_winner``.
    challenger, baseline
        The estimator labels compared (challenger vs the simpler rung).
    primary
        The primary metric whose superiority gates acceptance.
    improvement
        Point fractional reduction of the primary metric (challenger vs
        baseline; positive = challenger better) and its bootstrap CI
        ``(lo, hi)``.
    superiority
        ``True`` when the improvement CI excludes 0 (lo > 0) AND the point
        improvement meets ``practical_tol``.
    degraded
        Panel metrics that FAILED non-inferiority (relative-degradation CI
        upper >= ``ni_delta``), each with its point degradation and CI.
    panel
        Every panel metric's non-inferiority read (degraded or not).
    fail_reasons
        The robustness gate(s) that forced ``FAIL`` even though the primary
        bootstrap met superiority -- ``"convergence"`` (excluded share over
        ``max_nonconverged_frac``) and/or ``"sensitivity_split"`` (the
        matched win did not survive on the own population). Empty when the
        verdict is decided by the primary bootstrap alone.
    sensitivity
        The matched-vs-own read of the primary metric: ``own_improvement`` /
        ``own_improvement_ci`` (the unpaired own-population bootstrap -- CI
        reported, not gated), ``own_superiority`` (the own point improvement
        meets ``practical_tol``), ``matched_superiority``, and ``split``
        (``True`` when the matched win did not survive as an effect on the own
        population). When both estimators reach the same cells the own point
        equals the matched point, so no split is possible.
    n_nonconverged
        Number of ``(estimator, holdout)`` folds (over challenger + baseline)
        whose refit did not converge.
    n_excluded
        Matched cells dropped because their fold did not converge.
    excluded_frac
        ``n_excluded / n_matched`` -- the convergence auto-FAIL trigger.
    max_nonconverged_frac
        The excluded-share threshold above which the verdict is forced to
        ``FAIL`` (default 0.05).
    n_clusters, n_matched, n_replicates, seed
        The resampling provenance (``n_clusters`` is post convergence filter).
    """

    verdict: str
    no_winner: bool
    challenger: str
    baseline: str
    primary: str
    improvement: float
    improvement_ci: tuple[float, float]
    superiority: bool
    practical_tol: float
    ni_delta: float
    degraded: list[str]
    panel: dict[str, dict[str, Any]]
    fail_reasons: list[str]
    sensitivity: dict[str, Any]
    n_nonconverged: int
    n_excluded: int
    excluded_frac: float
    max_nonconverged_frac: float
    n_clusters: int
    n_matched: int
    n_replicates: int
    seed: int

    def __repr__(self) -> str:
        lo, hi = self.improvement_ci
        tag = " NO_WINNER" if self.no_winner else ""
        deg = f" degraded={self.degraded}" if self.degraded else ""
        rsn = f" fail_reasons={self.fail_reasons}" if self.fail_reasons else ""
        return (
            f"<GateReport {self.verdict}{tag}: {self.challenger!r} vs "
            f"{self.baseline!r} on {self.primary!r} "
            f"improvement={self.improvement:+.1%} "
            f"CI=({lo:+.1%}, {hi:+.1%}){deg}{rsn}>"
        )


_STAT_COLS = ("sum_abs", "n_abs", "sum_ae", "n_ae", "sum_gap", "sum_exp")


def _cluster_agg(cells: pl.DataFrame, by: list[str], pre: str) -> pl.DataFrame:
    """Per-cluster sufficient statistics for the bootstrap.

    A cluster is a ``(group, cohort)`` unit. For each cluster the metrics
    reduce to weighted sums: ``abs_err`` / ``ae_err`` are ``sum / count``
    means (count = non-null contributions, mirroring a null-skipping mean),
    ``bias`` is ``sum(gap) / sum(expected)``. ``by`` is the grouping key set --
    ``["estimator", *cluster_keys]`` for the paired matched population (one
    block per estimator over a shared cluster set) or just ``cluster_keys`` for
    a single estimator's own population.
    """
    gap = pl.col(f"{pre}actual") - pl.col(f"{pre}expected")
    ae = pl.col(f"{pre}ae_err")
    return cells.group_by(by).agg(
        gap.abs().sum().alias("sum_abs"),
        gap.abs().is_not_null().sum().alias("n_abs"),
        ae.abs().sum().alias("sum_ae"),
        ae.abs().is_not_null().sum().alias("n_ae"),
        gap.sum().alias("sum_gap"),
        pl.col(f"{pre}expected").sum().alias("sum_exp"),
    )


def _fold_converged(comparison: "EstimatorComparisonFit", label: str) -> dict[int, bool]:
    """``{holdout: converged}`` for one estimator's per-fold refits.

    Each rolling-origin fold is one ``Backtest`` refit of the estimator on a
    masked triangle; its ``LossFit.converged`` flag rides on the held
    ``BacktestFit``. A fold missing the attribute (a non-LossFit result) is
    treated as converged -- only an explicit ``converged=False`` excludes it.
    """
    rbt = comparison._fits[label]
    out: dict[int, bool] = {}
    for holdout, bt_fit in rbt._fits.items():
        refit = getattr(bt_fit, "_refit", None)
        out[int(holdout)] = bool(getattr(refit, "converged", True))
    return out


def _metric_values(
    counts: np.ndarray, stats: dict[str, np.ndarray]
) -> dict[str, np.ndarray]:
    """Pooled metric value(s) for a (B, K) cluster-count matrix.

    ``counts`` is the bootstrap multiplicity of each of ``K`` clusters over
    ``B`` draws (one row = one resample; the full-data point estimate is the
    all-ones row). Each metric is the ratio of two count-weighted cluster sums,
    null-guarded to ``nan`` on a zero / non-finite denominator."""
    def ratio(num_key: str, den_key: str, absolute: bool = False) -> np.ndarray:
        num = counts @ stats[num_key]
        den = counts @ stats[den_key]
        with np.errstate(divide="ignore", invalid="ignore"):
            out = np.where(
                np.isfinite(den) & (den != 0), num / den, np.nan
            )
        return np.abs(out) if absolute else out

    return {
        "abs_err": ratio("sum_abs", "n_abs"),
        "ae_err": ratio("sum_ae", "n_ae"),
        "bias": ratio("sum_gap", "sum_exp", absolute=True),
    }


def gate(
    comparison: "EstimatorComparisonFit",
    *,
    challenger: str | None = None,
    primary: str = "abs_err",
    panel: "tuple[str, ...]" = ("ae_err", "bias"),
    lane: str = "cumulative",
    practical_tol: float = 0.05,
    ni_delta: float = 0.02,
    max_nonconverged_frac: float = 0.05,
    n_replicates: int = 2000,
    seed: int = 0,
) -> GateReport:
    """Decide whether ``challenger`` earns its place over the baseline rung.

    Parameters
    ----------
    comparison
        A fitted :class:`~lossratio.comparison.EstimatorComparisonFit`; its
        baseline label is the simpler rung.
    challenger
        The estimator label being gated. Optional when the comparison holds
        exactly two estimators (defaults to the non-baseline one); required
        otherwise.
    primary
        The metric whose superiority gates acceptance: ``"abs_err"`` (default,
        cell accuracy), ``"ae_err"`` (relative cell accuracy), or ``"bias"``
        (pooled calibration).
    panel
        The metrics held to non-inferiority (default the two non-primary
        metrics).
    lane
        ``"cumulative"`` (default) or ``"incremental"``.
    practical_tol
        The minimum point improvement (fractional metric reduction) for
        superiority (default 0.05 = 5%).
    ni_delta
        The non-inferiority margin: a panel metric fails when its
        relative-degradation CI upper bound reaches ``ni_delta`` (default
        0.02 = 2%).
    max_nonconverged_frac
        Convergence hygiene: if the share of matched cells dropped for landing
        in a non-converged fold exceeds this (default 0.05 = 5%), the verdict
        is forced to ``FAIL``.
    n_replicates, seed
        Bootstrap replicate count (default 2000) and RNG seed (recorded in the
        report).

    Returns
    -------
    GateReport
        The 3-state verdict with the primary improvement CI, the panel
        non-inferiority reads, and the resampling provenance.
    """
    if primary not in _METRICS:
        raise ValueError(f"primary must be one of {_METRICS}, got {primary!r}")
    bad = [m for m in panel if m not in _METRICS]
    if bad:
        raise ValueError(f"panel metrics must be in {_METRICS}, got {bad!r}")
    if lane not in _LANE_PREFIX:
        raise ValueError(
            f'lane must be "cumulative" or "incremental", got {lane!r}'
        )
    if lane == "incremental" and not comparison._has_incr:
        raise ValueError(
            'lane="incremental" is unavailable: not every estimator carried '
            "an incremental projection on the matched cells"
        )
    if n_replicates < 1:
        raise ValueError(f"n_replicates must be >= 1, got {n_replicates}")

    baseline = comparison.baseline
    labels = [lbl for lbl in comparison._labels if lbl != baseline]
    if challenger is None:
        if len(labels) != 1:
            raise ValueError(
                "challenger is required when the comparison holds more than "
                f"two estimators; non-baseline labels are {labels}"
            )
        challenger = labels[0]
    elif challenger == baseline:
        raise ValueError(
            f"challenger {challenger!r} must differ from the baseline"
        )
    elif challenger not in comparison._labels:
        raise ValueError(
            f"challenger must be one of {comparison._labels}, got {challenger!r}"
        )

    gcols = normalize_groups(comparison._groups)
    cluster_keys = [*gcols, "cohort"]
    pre = _LANE_PREFIX[lane]

    cells = comparison._cells.filter(
        pl.col("estimator").is_in([challenger, baseline])
    )
    if cells.height == 0:
        raise ValueError("no matched cells to gate on")

    # --- Convergence hygiene (charter Sec.6.4): a fold is one (estimator,
    # holdout) refit; cells landing in a non-converged fold (of EITHER
    # estimator -- the pair must stay aligned) are dropped before the
    # bootstrap, counted, and the excluded share gates an auto-FAIL.
    ch_conv = _fold_converged(comparison, challenger)
    base_conv = _fold_converged(comparison, baseline)
    ch_nonconv = sorted(h for h, ok in ch_conv.items() if not ok)
    base_nonconv = sorted(h for h, ok in base_conv.items() if not ok)
    n_nonconverged = len(ch_nonconv) + len(base_nonconv)
    nonconv_holdouts = sorted(set(ch_nonconv) | set(base_nonconv))
    if nonconv_holdouts:
        n_excluded = cells.filter(
            (pl.col("estimator") == challenger)
            & pl.col("holdout").is_in(nonconv_holdouts)
        ).height
        cells = cells.filter(~pl.col("holdout").is_in(nonconv_holdouts))
    else:
        n_excluded = 0
    excluded_frac = (
        n_excluded / comparison._n_matched if comparison._n_matched else 0.0
    )
    conv_fail = excluded_frac > max_nonconverged_frac

    stats = _cluster_agg(cells, ["estimator", *cluster_keys], pre)

    # Align clusters: the matched population shares keys, so the cluster set is
    # identical per estimator. Pivot each estimator's per-cluster sums onto the
    # common cluster index so a single (B, K) count matrix scores both.
    clusters = stats.select(cluster_keys).unique().sort(cluster_keys)
    n_clusters = clusters.height

    rng = np.random.default_rng(seed)

    def side(label: str) -> dict[str, np.ndarray]:
        s = clusters.join(
            stats.filter(pl.col("estimator") == label), on=cluster_keys,
            how="left",
        )
        return {
            c: s[c].fill_null(0).to_numpy().astype(np.float64)
            for c in _STAT_COLS
        }

    def ci(arr: np.ndarray) -> tuple[float, float]:
        draws = arr[1:][np.isfinite(arr[1:])]
        if draws.size == 0:
            return (float("nan"), float("nan"))
        return (float(np.percentile(draws, 2.5)),
                float(np.percentile(draws, 97.5)))

    panel_reads: dict[str, dict[str, Any]] = {}
    degraded: list[str] = []

    if n_clusters == 0:
        # The convergence filter removed every matched cell -- nothing to score
        # (excluded_frac is ~1, so conv_fail is already True).
        imp_point, imp_lo, imp_hi = float("nan"), float("nan"), float("nan")
        superiority = False
        no_winner = False
    else:
        # Full-data point estimate (all clusters once) + the paired bootstrap
        # draws (multinomial cluster counts -- a K-cluster resample with
        # replacement, the SAME draw applied to both estimators).
        point = np.ones((1, n_clusters), dtype=np.float64)
        boot = rng.multinomial(
            n_clusters, np.full(n_clusters, 1.0 / n_clusters), size=n_replicates
        ).astype(np.float64)
        counts = np.vstack([point, boot])

        ch_m = _metric_values(counts, side(challenger))
        base_m = _metric_values(counts, side(baseline))

        def improvement(metric: str) -> np.ndarray:
            # fractional reduction: positive = challenger better (smaller).
            c, b = ch_m[metric], base_m[metric]
            with np.errstate(divide="ignore", invalid="ignore"):
                return np.where(np.isfinite(b) & (b != 0), 1.0 - c / b, np.nan)

        imp = improvement(primary)
        imp_point = float(imp[0])
        imp_lo, imp_hi = ci(imp)
        superiority = (imp_lo > 0.0) and (imp_point >= practical_tol)
        # NO_WINNER (charter Sec.6.4): the improvement CI INCLUDES 0, so the
        # baseline is not beaten. A CI entirely BELOW 0 is a decided baseline
        # win (challenger worse), and a CI entirely ABOVE 0 but with the point
        # below practical_tol is a real-but-immaterial win -- both are FAIL,
        # but neither is a no-winner.
        no_winner = (imp_lo <= 0.0) and (imp_hi >= 0.0)

        for m in panel:
            # degradation: positive = challenger WORSE (metric larger).
            deg = -improvement(m)
            deg_point = float(deg[0])
            deg_lo, deg_hi = ci(deg)
            ni_pass = deg_hi < ni_delta
            panel_reads[m] = {
                "degradation": deg_point,
                "ci": (deg_lo, deg_hi),
                "ni_pass": ni_pass,
            }
            if not ni_pass:
                degraded.append(m)

    # --- Matched-vs-own sensitivity (charter Sec.6.4): recompute the primary
    # improvement on each estimator's OWN population (every cell it projected
    # over the common folds, minus its own non-converged folds) via an UNPAIRED
    # cluster bootstrap. A matched win that does not survive on the own
    # population is riding a favourable subset and may not PASS.
    def own_draws(label: str, nonconv: list[int]) -> np.ndarray | None:
        df = comparison._fits[label]._ae_err.filter(
            pl.col("holdout").is_in(list(comparison._common_holdouts))
        )
        if nonconv:
            df = df.filter(~pl.col("holdout").is_in(nonconv))
        s = _cluster_agg(df, cluster_keys, pre)
        if s.height == 0:
            return None
        stat = {
            c: s[c].fill_null(0).to_numpy().astype(np.float64)
            for c in _STAT_COLS
        }
        k = s.height
        cnts = np.vstack([
            np.ones((1, k), dtype=np.float64),
            rng.multinomial(
                k, np.full(k, 1.0 / k), size=n_replicates
            ).astype(np.float64),
        ])
        return _metric_values(cnts, stat)[primary]

    ch_own = own_draws(challenger, ch_nonconv)
    base_own = own_draws(baseline, base_nonconv)
    if ch_own is None or base_own is None:
        own_point, own_lo, own_hi = float("nan"), float("nan"), float("nan")
        own_superiority = False
        sens_split = False
    else:
        with np.errstate(divide="ignore", invalid="ignore"):
            own = np.where(
                np.isfinite(base_own) & (base_own != 0),
                1.0 - ch_own / base_own, np.nan,
            )
        own_point = float(own[0])
        own_lo, own_hi = ci(own)
        # POINT-based survival: the own comparison is UNPAIRED (challenger and
        # baseline resampled independently), so its CI is far wider than the
        # paired matched CI and gating on it would fire spuriously even when the
        # two estimators reach the SAME cells -- there own_point == matched_point
        # exactly, so the effect-size survival is the faithful check (the CI is
        # reported for transparency, not gated). When reaches differ, own_point
        # measures whether the win survives on the full own population.
        own_superiority = own_point >= practical_tol
        # Split = the matched win did not survive on the own population.
        sens_split = superiority and not own_superiority

    sensitivity = {
        "own_improvement": own_point,
        "own_improvement_ci": (own_lo, own_hi),
        "own_superiority": own_superiority,
        "matched_superiority": superiority,
        "split": sens_split,
    }

    fail_reasons: list[str] = []
    if conv_fail:
        fail_reasons.append("convergence")
    if sens_split:
        fail_reasons.append("sensitivity_split")

    if fail_reasons or not superiority:
        verdict = "FAIL"
    elif degraded:
        verdict = "PASS_WITH_TRADEOFF"
    else:
        verdict = "PASS"

    return GateReport(
        verdict=verdict,
        no_winner=no_winner,
        challenger=challenger,
        baseline=baseline,
        primary=primary,
        improvement=imp_point,
        improvement_ci=(imp_lo, imp_hi),
        superiority=superiority,
        practical_tol=practical_tol,
        ni_delta=ni_delta,
        degraded=degraded,
        panel=panel_reads,
        fail_reasons=fail_reasons,
        sensitivity=sensitivity,
        n_nonconverged=n_nonconverged,
        n_excluded=n_excluded,
        excluded_frac=excluded_frac,
        max_nonconverged_frac=max_nonconverged_frac,
        n_clusters=n_clusters,
        n_matched=comparison._n_matched,
        n_replicates=n_replicates,
        seed=seed,
    )
