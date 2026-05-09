"""Regime: structural change-point detection across underwriting cohorts.

Each cohort is treated as a feature vector (the chosen ``loss_var`` over
development periods 1, ..., K). The ordered sequence of cohort vectors
is then tested for structural shifts using one of two methods:

* ``"e_divisive"`` — E-Divisive (Matteson & James 2014). Multivariate
  non-parametric divisive change-point detection with permutation
  significance. Default. Implemented in :mod:`._e_divisive`.
* ``"hclust"`` — Ward hierarchical clustering on the standardised
  cohort matrix, cut at ``n_regimes`` clusters. Ignores time ordering
  — useful as a sanity check, not as the primary regime detector.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.cluster.hierarchy import fcluster, linkage
from scipy.spatial.distance import pdist

from ._e_divisive import e_divisive
from ._io import mirror_output

if TYPE_CHECKING:
    from .triangle import Triangle


_VALID_METHODS = ("e_divisive", "hclust")


def _build_feature_matrix(
    tri_df: pl.DataFrame,
    loss_var: str,
    K: int,
) -> tuple[np.ndarray, list, list]:
    """Pivot the long-format Triangle into a (n_cohorts, K) feature matrix.

    Cohorts with fewer than K observed development periods are dropped
    and returned in the ``dropped`` list. Cohorts are returned ordered
    by cohort value.
    """
    if loss_var not in tri_df.columns:
        raise ValueError(
            f"loss_var={loss_var!r} not found in Triangle columns: "
            f"{tri_df.columns}"
        )

    df = tri_df.filter(pl.col("dev") <= K)

    # Count observations per cohort
    counts = df.group_by("cohort").agg(pl.len().alias("n")).sort("cohort")
    eligible = counts.filter(pl.col("n") >= K)["cohort"].to_list()
    all_cohorts = counts["cohort"].to_list()
    dropped = [c for c in all_cohorts if c not in set(eligible)]

    if not eligible:
        raise ValueError(
            f"No cohorts have >= K={K} observed development periods. "
            f"Reduce K."
        )

    # Pivot to wide form: rows = cohort, cols = dev 1..K
    wide = (
        df.filter(pl.col("cohort").is_in(eligible))
        .pivot(on="dev", index="cohort", values=loss_var)
        .sort("cohort")
    )

    cohorts = wide["cohort"].to_list()
    mat = wide.drop("cohort").to_numpy()

    if np.isnan(mat).any():
        raise ValueError(
            "Feature matrix contains NaN — reduce K or check input."
        )

    return mat, cohorts, dropped


def _e_divisive_breakpoints(
    mat: np.ndarray,
    sig_level: float,
    R: int,
    min_size: int,
    seed: int | None,
) -> list[int]:
    """E-Divisive breakpoints (indices of right-side starts)."""
    res = e_divisive(
        mat,
        sig_level=sig_level,
        R=R,
        min_size=min_size,
        alpha=1.0,
        seed=seed,
    )
    return res.breakpoints


def _hclust_breakpoints(
    mat: np.ndarray,
    n_regimes: int,
) -> list[int]:
    """Ward hierarchical clustering, cut at n_regimes; report indices
    where the cluster id changes in sequential cohort order.

    Note that hclust ignores time ordering, so non-adjacent cohorts may
    end up in the same cluster.
    """
    if n_regimes < 2:
        raise ValueError(f"n_regimes must be >= 2, got {n_regimes}")

    # Standardise columns (zero mean, unit variance)
    std = mat.std(axis=0, ddof=1)
    std[std == 0] = 1.0
    scaled = (mat - mat.mean(axis=0)) / std

    Z = linkage(pdist(scaled, metric="euclidean"), method="ward")
    cluster_id = fcluster(Z, t=n_regimes, criterion="maxclust")

    # Indices where cluster id changes between consecutive cohorts
    changes = np.where(np.diff(cluster_id) != 0)[0] + 1
    return changes.tolist()


def _regime_ids_from_breaks(n: int, breaks: list[int]) -> np.ndarray:
    """Assign 1-based regime ids in cohort order."""
    ids = np.empty(n, dtype=np.int64)
    cur = 1
    bk = sorted(set(breaks))
    j = 0
    for i in range(n):
        if j < len(bk) and i >= bk[j]:
            cur += 1
            j += 1
        ids[i] = cur
    return ids


class Regime:
    """Detected cohort regime structure.

    Use :meth:`Triangle.detect_regime` to construct.

    Attributes
    ----------
    method : str
        ``"e_divisive"`` or ``"hclust"``.
    loss_var : str
        Trajectory variable name used.
    K : int
        Development-period window.
    cohort_var : str
        Original cohort variable name (e.g. ``"uym"``).
    breakpoints : list
        Cohort values at which a new regime starts (excluding the first
        cohort).
    n_regimes : int
        Number of regimes detected.
    dropped : list
        Cohorts excluded because they had fewer than ``K`` observed
        development periods.
    """

    def __init__(self) -> None:
        self._labels_df: pl.DataFrame
        self._output_type: str
        self.method: str
        self.loss_var: str
        self.K: int
        self.cohort_var: str
        self.dev_var: str
        self.group_var: str | None
        self.breakpoints: list[Any]
        self.n_regimes: int
        self.dropped: list[Any]

    @classmethod
    def _from_triangle(
        cls,
        triangle: "Triangle",
        *,
        loss_var: str = "lr",
        K: int = 12,
        method: str = "e_divisive",
        n_regimes: int | None = None,
        sig_level: float = 0.05,
        R: int = 999,
        min_size: int = 3,
        seed: int | None = None,
    ) -> "Regime":
        if method not in _VALID_METHODS:
            raise ValueError(
                f"method must be one of {_VALID_METHODS}, got {method!r}"
            )
        if K < 2:
            raise ValueError(f"K must be >= 2, got {K}")

        tri_df = triangle.to_polars()

        # If grouped, require single group
        if triangle.group_var is not None:
            n_groups = tri_df[triangle.group_var].n_unique()
            if n_groups > 1:
                raise ValueError(
                    f"Triangle has {n_groups} groups; subset to a single "
                    f"group before calling detect_regime()."
                )

        # Build feature matrix
        mat, cohorts, dropped = _build_feature_matrix(tri_df, loss_var, K)
        n = len(cohorts)

        # Dispatch to method
        if method == "e_divisive":
            breaks_idx = _e_divisive_breakpoints(
                mat, sig_level=sig_level, R=R, min_size=min_size, seed=seed
            )
        else:  # hclust
            n_reg = 2 if n_regimes is None else int(n_regimes)
            breaks_idx = _hclust_breakpoints(mat, n_regimes=n_reg)

        # Regime ids per cohort (1-based)
        regime_ids = _regime_ids_from_breaks(n, breaks_idx)
        breakpoints = [cohorts[i] for i in breaks_idx]

        labels_df = pl.DataFrame(
            {
                "cohort": cohorts,
                "regime_id": regime_ids,
            }
        )

        self = cls.__new__(cls)
        self._labels_df = labels_df
        self._output_type = triangle._output_type
        self.method = method
        self.loss_var = loss_var
        self.K = K
        self.cohort_var = triangle.cohort_var
        self.dev_var = triangle.dev_var
        self.group_var = triangle.group_var
        self.breakpoints = breakpoints
        self.n_regimes = int(regime_ids.max()) if n > 0 else 0
        self.dropped = dropped
        return self

    @property
    def df(self):
        """Per-cohort regime labels in the original input format."""
        return mirror_output(self._labels_df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._labels_df

    def to_pandas(self):
        return self._labels_df.to_pandas()

    def __repr__(self) -> str:
        n_cohorts = self._labels_df.height
        bits = [
            f"method={self.method}",
            f"loss_var={self.loss_var!r}",
            f"K={self.K}",
            f"{n_cohorts} cohorts",
            f"{self.n_regimes} regimes",
        ]
        if self.dropped:
            bits.append(f"{len(self.dropped)} dropped")
        return f"<Regime: {', '.join(bits)}>"
