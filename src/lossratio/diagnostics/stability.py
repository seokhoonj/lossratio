"""Go-forward stability gate.

The honest go-forward beyond the observed loss-ratio frontier is NOT a fitted
tail curve: real-data OOS shows that extrapolating the loss and premium
separately and dividing makes the loss RATIO *worse* and can blow
up, because loss and premium co-develop and their tails cancel in
``R = L / P``. The accurate go-forward is to hold the frontier loss ratio flat
-- BUT only once the observed loss ratio has SETTLED (the recent loss ratio
has stopped moving). The loss ratio swings early; freezing a still-swinging ratio
is wrong.

This module decides that gate. For each segment it measures the loss ratio's
own per-step development ``rho_k = f^L_k / f^P_k`` (the cumulative loss ratio is
flat exactly when loss and premium develop at the same rate, ``rho_k -> 1``).
If the recent window of ``rho_k`` is flat within tolerance, the frozen
go-forward is trustworthy (``stable``); otherwise the loss ratio is still
emerging and the go-forward is flagged uncertain -- no fabricated tail.

This is an EMPIRICAL settling check (has the observed series stopped moving),
NOT a claim of convergence to a structural ultimate -- long-term health has no
ultimate (the loss ratio can resume rising if a trend/selection regime shifts;
that is the explicit caveat of a stable verdict).
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._kernels.io import collapse_groups, mirror_output, normalize_groups

if TYPE_CHECKING:
    from .._kernels.io import FrameLike
    from ..core.triangle import Triangle


def _segment_matrices(sub: pl.DataFrame) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """cohort x duration cumulative loss / premium matrices + duration axis."""
    durations = sorted(sub.get_column("duration").unique().to_list())
    cohorts = sub.get_column("cohort").unique().sort().to_list()
    cohort_idx = {c: i for i, c in enumerate(cohorts)}
    duration_idx = {d: j for j, d in enumerate(durations)}
    L = np.full((len(cohorts), len(durations)), np.nan)
    P = np.full((len(cohorts), len(durations)), np.nan)
    for c, d, loss_val, premium_val in zip(
        sub.get_column("cohort").to_list(),
        sub.get_column("duration").to_list(),
        sub.get_column("loss").to_list(),
        sub.get_column("premium").to_list(),
        strict=False,
    ):
        L[cohort_idx[c], duration_idx[d]] = loss_val
        P[cohort_idx[c], duration_idx[d]] = premium_val
    return L, P, np.asarray(durations)


def _ratio_step(L: np.ndarray, P: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    """per-link ratio development rho_k = f^L_k / f^P_k (same cohorts each link).

    ``rho_k - 1`` is the loss ratio's fractional change from duration k to k+1;
    ``rho_k -> 1`` means loss and premium are developing in lockstep, i.e. the
    cumulative loss ratio is flat. Returns ``(rho, n_cohorts)`` per link.
    """
    nlink = L.shape[1] - 1
    rho = np.full(nlink, np.nan)
    nco = np.zeros(nlink, dtype=int)
    for k in range(nlink):
        both = (
            ~np.isnan(L[:, k]) & ~np.isnan(L[:, k + 1])
            & ~np.isnan(P[:, k]) & ~np.isnan(P[:, k + 1])
        )
        nco[k] = both.sum()
        sLk, sLk1 = np.nansum(L[both, k]), np.nansum(L[both, k + 1])
        sPk, sPk1 = np.nansum(P[both, k]), np.nansum(P[both, k + 1])
        if sLk > 0 and sPk > 0 and sLk1 > 0 and sPk1 > 0:
            rho[k] = (sLk1 / sLk) / (sPk1 / sPk)
    return rho, nco


@dataclass(kw_only=True)
class Stability:
    """Go-forward stability gate for the loss ratio.

    ``assess(triangle)`` reports, per segment, whether the observed loss ratio
    has SETTLED at the frontier -- if so, freezing the frontier loss
    ratio is a trustworthy go-forward; if not, the go-forward is flagged
    uncertain.

    Parameters
    ----------
    window
        Number of most-recent duration links whose loss-ratio step
        ``rho_k`` must be flat for a ``stable`` verdict (default 6).
    tol
        Maximum allowed loss-ratio fractional change per link (``|rho_k - 1|``)
        inside the window for ``stable`` (default 0.01 = 1% per period).

        The default is calibrated on real books (4 portfolios, ~2000
        segment-by-cut samples): tying the verdict to a 12-month out-of-sample
        freeze error (acceptable < 5%), ``tol = 0.01`` gives ~95% precision --
        when the gate calls a segment ``stable``, freezing its ratio is actually
        accurate ~95% of the time -- while flagging ~half the freezable cases
        (recall ~50%, i.e. deliberately conservative). Loosening to ~0.012 keeps
        precision >= 90% with more recall; past ~0.015 precision falls off
        (0.02 -> ~74%). The ``max|rho_k - 1|`` window statistic was chosen over a
        net-trend statistic because it flags still-volatile (not just trending)
        segments, which a trend average misses -- it was the better OOS
        predictor of freeze error at every threshold.
    min_links
        Minimum number of usable duration links a segment needs before a
        verdict is issued; shallower segments report ``insufficient_depth``
        (default 8).
    min_cohorts
        Minimum cohorts contributing to a link for it to count (default 3).
    """

    window: int = 6
    tol: float = 0.01
    min_links: int = 8
    min_cohorts: int = 3

    def __post_init__(self) -> None:
        if self.window < 1:
            raise ValueError(f"window must be >= 1, got {self.window}")
        if not (0.0 < self.tol < 1.0):
            raise ValueError(f"tol must be in (0, 1), got {self.tol}")
        if self.min_links < self.window:
            raise ValueError("min_links must be >= window")

    def assess(self, triangle: Triangle) -> StabilityReport:
        """Assess go-forward stability per segment on ``triangle``."""
        df = triangle.to_polars() if hasattr(triangle, "to_polars") else triangle._df
        groups = triangle.groups
        group_cols = normalize_groups(groups)
        rows: list[dict[str, Any]] = []

        if group_cols:
            seg_keys = df.select(group_cols).unique(maintain_order=True).rows()
        else:
            seg_keys = [()]
        for key in seg_keys:
            if group_cols:
                mask = pl.lit(True)
                for col, val in zip(group_cols, key, strict=False):
                    mask = mask & (pl.col(col) == val)
                sub = df.filter(mask)
            else:
                sub = df
            L, P, durations = _segment_matrices(
                sub.select(["cohort", "duration", "loss", "premium"])
            )
            rho, nco = _ratio_step(L, P)
            usable = np.isfinite(rho) & (nco >= self.min_cohorts)
            ks = np.where(usable)[0]
            row: dict[str, Any] = {}
            for col, val in zip(group_cols, key, strict=False):
                row[col] = val
            # frontier = deepest duration that carries a pooled ratio
            front_j = max((j for j in range(len(durations)) if np.nansum(P[:, j]) > 0
                           and np.nansum(L[:, j]) >= 0), default=None)
            row["frontier_duration"] = int(durations[front_j]) if front_j is not None else None
            sL = np.nansum(L[:, front_j]) if front_j is not None else np.nan
            sP = np.nansum(P[:, front_j]) if front_j is not None else np.nan
            row["frontier_ratio"] = float(sL / sP) if (front_j is not None and sP > 0) else None

            if len(ks) < self.min_links:
                row["stable"] = None
                row["recent_drift"] = None
                row["stable_from_duration"] = None
                row["status"] = "insufficient_depth"
                rows.append(row)
                continue

            recent = ks[-self.window:]
            drift = float(np.max(np.abs(rho[recent] - 1.0)))
            stable = drift < self.tol
            # earliest duration after which every usable link stayed within tol
            excess = np.abs(rho - 1.0)
            stable_from = None
            for start in ks:
                tail = ks[ks >= start]
                if np.all(excess[tail] < self.tol):
                    stable_from = int(durations[start])
                    break
            row["stable"] = bool(stable)
            row["recent_drift"] = drift
            row["stable_from_duration"] = stable_from if stable else None
            row["status"] = "stable" if stable else "developing"
            rows.append(row)

        return StabilityReport(
            pl.DataFrame(rows),
            groups=collapse_groups(groups),
            window=self.window,
            tol=self.tol,
            output_type=triangle._output_type,
        )


class StabilityReport:
    """Per-segment go-forward stability verdict.

    Columns: the segment keys, ``frontier_duration`` (deepest observed
    duration), ``frontier_ratio`` (the loss ratio to FREEZE forward if stable),
    ``stable`` (bool / null), ``recent_drift`` (max ``|rho_k - 1|`` in the
    window -- how far from flat), ``stable_from_duration`` (earliest duration the
    ratio settled, when stable), and ``status``
    (``stable`` / ``developing`` / ``insufficient_depth``).

    A ``stable`` verdict means: hold ``frontier_ratio`` flat as the go-forward
    loss ratio for any horizon beyond the frontier. ``developing`` means the
    ratio has not settled -- the go-forward is genuinely uncertain and no flat
    freeze (or fitted tail) should be trusted.
    """

    def __init__(
        self,
        df: pl.DataFrame,
        *,
        groups: str | list[str] | None,
        window: int,
        tol: float,
        output_type: str,
    ) -> None:
        self._df = df
        self.groups = groups
        self.window = window
        self.tol = tol
        self._output_type = output_type

    @property
    def df(self) -> FrameLike:
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def frozen_ratio(self) -> FrameLike:
        """The go-forward loss ratio per segment: ``frontier_ratio`` where the
        verdict is ``stable``, null otherwise (no trustworthy freeze)."""
        out = self._df.with_columns(
            go_forward_ratio=pl.when(pl.col("stable") == True)  # noqa: E712
            .then(pl.col("frontier_ratio"))
            .otherwise(None)
        ).select(
            [*(normalize_groups(self.groups) or []),
             "frontier_duration", "go_forward_ratio", "status"]
        )
        return mirror_output(out, self._output_type)

    def __repr__(self) -> str:
        n = self._df.height
        if "status" in self._df.columns:
            counts = self._df.get_column("status").value_counts()
            parts = ", ".join(
                f"{r[0]}={r[1]}" for r in counts.rows()
            )
        else:
            parts = ""
        return f"StabilityReport(segments={n}, {parts})"
