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

Deferred (charter Sec.6.4, follow-ups): Holm multiplicity over several primary
metrics, the matched-vs-challenger-own sensitivity recompute, convergence
hygiene, the non-negotiable naive-baseline row, and zero-loss handling.

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
    n_clusters, n_matched, n_boot, seed
        The resampling provenance.
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
    n_clusters: int
    n_matched: int
    n_boot: int
    seed: int

    def __repr__(self) -> str:
        lo, hi = self.improvement_ci
        tag = " NO_WINNER" if self.no_winner else ""
        deg = f" degraded={self.degraded}" if self.degraded else ""
        return (
            f"<GateReport {self.verdict}{tag}: {self.challenger!r} vs "
            f"{self.baseline!r} on {self.primary!r} "
            f"improvement={self.improvement:+.1%} "
            f"CI=({lo:+.1%}, {hi:+.1%}){deg}>"
        )


def _cluster_stats(
    cells: pl.DataFrame, cluster_keys: list[str], pre: str
) -> pl.DataFrame:
    """Per-(estimator, cluster) sufficient statistics for the bootstrap.

    A cluster is a ``(group, cohort)`` unit. For each estimator and cluster the
    metrics reduce to weighted sums: ``abs_err`` / ``ae_err`` are
    ``sum / count`` means (count = non-null contributions, mirroring a
    null-skipping mean), ``bias`` is ``sum(gap) / sum(expected)``. The matched
    population shares keys across estimators, so the cluster set is identical
    per estimator (asserted by the alignment in :func:`gate`).
    """
    gap = pl.col(f"{pre}actual") - pl.col(f"{pre}expected")
    ae = pl.col(f"{pre}ae_err")
    return cells.group_by(["estimator", *cluster_keys]).agg(
        gap.abs().sum().alias("sum_abs"),
        gap.abs().is_not_null().sum().alias("n_abs"),
        ae.abs().sum().alias("sum_ae"),
        ae.abs().is_not_null().sum().alias("n_ae"),
        gap.sum().alias("sum_gap"),
        pl.col(f"{pre}expected").sum().alias("sum_exp"),
    )


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
    n_boot: int = 2000,
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
    n_boot, seed
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
    if n_boot < 1:
        raise ValueError(f"n_boot must be >= 1, got {n_boot}")

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
    stats = _cluster_stats(cells, cluster_keys, pre)

    # Align clusters: the matched population shares keys, so the cluster set is
    # identical per estimator. Pivot each estimator's per-cluster sums onto the
    # common cluster index so a single (B, K) count matrix scores both.
    clusters = (
        stats.select(cluster_keys).unique().sort(cluster_keys)
    )
    n_clusters = clusters.height
    if n_clusters == 0:
        raise ValueError("no matched cells to gate on")

    stat_cols = ["sum_abs", "n_abs", "sum_ae", "n_ae", "sum_gap", "sum_exp"]

    def side(label: str) -> dict[str, np.ndarray]:
        s = clusters.join(
            stats.filter(pl.col("estimator") == label), on=cluster_keys,
            how="left",
        )
        return {
            c: s[c].fill_null(0).to_numpy().astype(np.float64) for c in stat_cols
        }

    ch_stats = side(challenger)
    base_stats = side(baseline)

    # Full-data point estimate (all clusters once) + the paired bootstrap draws
    # (multinomial cluster counts -- a K-cluster resample with replacement,
    # the SAME draw applied to both estimators).
    rng = np.random.default_rng(seed)
    point = np.ones((1, n_clusters), dtype=np.float64)
    boot = rng.multinomial(
        n_clusters, np.full(n_clusters, 1.0 / n_clusters), size=n_boot
    ).astype(np.float64)
    counts = np.vstack([point, boot])

    ch_m = _metric_values(counts, ch_stats)
    base_m = _metric_values(counts, base_stats)

    def improvement(metric: str) -> np.ndarray:
        # fractional reduction: positive = challenger better (smaller metric).
        c, b = ch_m[metric], base_m[metric]
        with np.errstate(divide="ignore", invalid="ignore"):
            return np.where(np.isfinite(b) & (b != 0), 1.0 - c / b, np.nan)

    def ci(arr: np.ndarray) -> tuple[float, float]:
        draws = arr[1:][np.isfinite(arr[1:])]
        if draws.size == 0:
            return (float("nan"), float("nan"))
        return (float(np.percentile(draws, 2.5)),
                float(np.percentile(draws, 97.5)))

    imp = improvement(primary)
    imp_point = float(imp[0])
    imp_lo, imp_hi = ci(imp)
    superiority = (imp_lo > 0.0) and (imp_point >= practical_tol)
    # NO_WINNER (charter Sec.6.4): the improvement CI INCLUDES 0, so the
    # baseline is not beaten. A CI entirely BELOW 0 is a decided baseline win
    # (challenger worse), and a CI entirely ABOVE 0 but with the point below
    # practical_tol is a real-but-immaterial win -- both are FAIL, but neither
    # is a no-winner.
    no_winner = (imp_lo <= 0.0) and (imp_hi >= 0.0)

    panel_reads: dict[str, dict[str, Any]] = {}
    degraded: list[str] = []
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

    if not superiority:
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
        n_clusters=n_clusters,
        n_matched=comparison._n_matched,
        n_boot=n_boot,
        seed=seed,
    )
