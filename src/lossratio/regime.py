"""Regime: structural change-point detection across underwriting cohorts.

Each cohort is treated as a feature vector (the chosen ``target`` over
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

from collections.abc import Callable, Mapping, Sequence
from datetime import date, datetime
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
_VALID_TREATMENTS = ("latest_only", "segment_wise")


def _build_feature_matrix(
    tri_df: pl.DataFrame,
    target: str,
    K: int,
) -> tuple[np.ndarray, list, list]:
    """Pivot the long-format Triangle into a (n_cohorts, K) feature matrix.

    Cohorts with fewer than K observed development periods are dropped
    and returned in the ``dropped`` list. Cohorts are returned ordered
    by cohort value.
    """
    if target not in tri_df.columns:
        raise ValueError(
            f"target={target!r} not found in Triangle columns: "
            f"{tri_df.columns}"
        )

    df = tri_df.filter(pl.col("dev") <= K)

    # Count *non-null target* observations per cohort. Counting raw rows
    # would include cells masked by a backtest hold-out (cells exist with
    # NaN), giving false eligibility that fails the no-NaN check later.
    counts = (
        df.group_by("cohort")
        .agg(pl.col(target).is_not_null().sum().alias("n"))
        .sort("cohort")
    )
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
        .pivot(on="dev", index="cohort", values=target)
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
    target : str
        Trajectory variable name used.
    K : int
        Development-period window.
    cohort : str
        Original cohort variable name (e.g. ``"uy_m"``).
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
        self._changes_df: pl.DataFrame
        self._output_type: str
        self.method: str
        self.target: str
        self.K: int
        self.cohort: str
        self.dev: str
        self.groups: str | None
        self.breakpoints: list[Any]
        self.n_regimes: int
        self.dropped: list[Any]
        self.treatment: str

    @classmethod
    def _from_triangle(
        cls,
        triangle: "Triangle",
        *,
        target: str = "lr",
        K: int = 12,
        method: str = "e_divisive",
        n_regimes: int | None = None,
        sig_level: float = 0.05,
        R: int = 999,
        min_size: int = 3,
        seed: int | None = None,
        treatment: str = "latest_only",
    ) -> "Regime":
        if method not in _VALID_METHODS:
            raise ValueError(
                f"method must be one of {_VALID_METHODS}, got {method!r}"
            )
        if treatment not in _VALID_TREATMENTS:
            raise ValueError(
                f"treatment must be one of {_VALID_TREATMENTS}, got {treatment!r}"
            )
        if K < 2:
            raise ValueError(f"K must be >= 2, got {K}")

        tri_df = triangle.to_polars()

        # If grouped, require single group
        if triangle.groups is not None:
            n_groups = tri_df[triangle.groups].n_unique()
            if n_groups > 1:
                raise ValueError(
                    f"Triangle has {n_groups} groups; subset to a single "
                    f"group before calling detect_regime()."
                )

        # Build feature matrix
        mat, cohorts, dropped = _build_feature_matrix(tri_df, target, K)
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

        # `_changes_df` mirrors R's `Regime$changes` slot: one row per
        # detected change point. For auto-detected regimes we only know
        # the change cohort; pre/post statistics are left null.
        changes_df = pl.DataFrame(
            {
                "change": breakpoints,
                "regime_id": list(range(2, 2 + len(breakpoints))),
            },
            schema_overrides={"regime_id": pl.Int64},
        )

        self = cls.__new__(cls)
        self._labels_df = labels_df
        self._changes_df = changes_df
        self._output_type = triangle._output_type
        self.method = method
        self.target = target
        self.K = K
        self.cohort = triangle.cohort
        self.dev = triangle.dev
        self.groups = triangle.groups
        self.breakpoints = breakpoints
        self.n_regimes = int(regime_ids.max()) if n > 0 else 0
        self.dropped = dropped
        self.treatment = treatment
        return self

    @classmethod
    def _manual(
        cls,
        *,
        changes_df: pl.DataFrame,
        treatment: str,
        groups: str | None,
    ) -> "Regime":
        """Construct a Regime by hand (no auto-detection).

        Used by :func:`regime_at` to wrap user-supplied change points.
        ``changes_df`` carries one row per change point with at least a
        ``change`` (Date) column plus the group column if any.
        """
        self = cls.__new__(cls)
        self._changes_df = changes_df
        self._labels_df = pl.DataFrame(
            {"cohort": [], "regime_id": []},
            schema={"cohort": pl.Date, "regime_id": pl.Int64},
        )
        self._output_type = "polars"
        self.method = "manual"
        self.target = ""
        self.K = 0
        self.cohort = ""
        self.dev = ""
        self.groups = groups
        self.breakpoints = changes_df["change"].to_list()
        self.n_regimes = 0
        self.dropped = []
        self.treatment = treatment
        return self

    @property
    def changes(self):
        """Detected (or manually specified) change points as a frame."""
        return mirror_output(self._changes_df, self._output_type)

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
            f"target={self.target!r}",
            f"K={self.K}",
            f"{n_cohorts} cohorts",
            f"{self.n_regimes} regimes",
        ]
        if self.dropped:
            bits.append(f"{len(self.dropped)} dropped")
        return f"<Regime: {', '.join(bits)}>"


# ---------------------------------------------------------------------------
# Helper factories (R parity: regime_at + regime_spec)
# ---------------------------------------------------------------------------


def _coerce_to_date(value: Any) -> date:
    """Coerce str / datetime / date to ``datetime.date``."""
    if isinstance(value, date) and not isinstance(value, datetime):
        return value
    if isinstance(value, datetime):
        return value.date()
    if isinstance(value, str):
        try:
            return date.fromisoformat(value)
        except ValueError as exc:
            raise ValueError(
                f"could not parse {value!r} as ISO date (YYYY-MM-DD)"
            ) from exc
    raise TypeError(
        f"expected date / datetime / 'YYYY-MM-DD' str, got {type(value).__name__}"
    )


def regime_at(
    change: Any,
    *,
    groups: Mapping[str, Sequence[Any]] | None = None,
    treatment: str = "latest_only",
) -> "Regime":
    """Build a :class:`Regime` from explicit, user-supplied change points.

    Use this when you already know where the cohort regime shifts (e.g.
    a policy revision date) and want a *fixed* regime tested across
    backtest folds. Contrast with :func:`regime_spec`, which defers
    detection to fit / backtest time so each fold uses change points
    derived from its own masked training data.

    Parameters
    ----------
    change
        Cohort date(s) where a new regime starts. A single value (str
        ``"YYYY-MM-DD"`` / ``date`` / ``datetime``) or a sequence of
        such values.
    groups
        Optional mapping ``{column_name: [values]}`` of group columns
        aligned 1:1 with ``change``. Required when the Triangle is
        grouped and different groups carry different change points.
    treatment
        Regime application mode. ``"latest_only"`` (default) drops
        cohorts before the most recent change. ``"segment_wise"``
        (R parity, not yet wired into Python fit / backtest) estimates
        factors separately per segment.

    Returns
    -------
    Regime
        A manually-constructed Regime suitable for the same 4-type
        dispatch slots that accept auto-detected Regimes.

    Examples
    --------
    >>> regime_at(change="2024-07-01")
    >>> regime_at(
    ...     change=["2024-07-01", "2024-10-01"],
    ...     groups={"coverage": ["SUR", "CI"]},
    ... )
    """
    if treatment not in _VALID_TREATMENTS:
        raise ValueError(
            f"treatment must be one of {_VALID_TREATMENTS}, got {treatment!r}"
        )

    if isinstance(change, (str, date, datetime)) or not isinstance(
        change, Sequence
    ):
        change_seq: list[Any] = [change]
    else:
        change_seq = list(change)
    if not change_seq:
        raise ValueError("`change` must have length >= 1")
    parsed = [_coerce_to_date(v) for v in change_seq]
    n = len(parsed)

    groups = dict(groups) if groups else {}
    for col, vals in groups.items():
        if not isinstance(vals, Sequence) or isinstance(vals, str):
            vals = [vals]
            groups[col] = vals
        if len(vals) != n:
            raise ValueError(
                f"All arguments must have equal length; "
                f"`change`={n} but `groups[{col!r}]`={len(vals)}"
            )

    # `regime_id = 2` per row mirrors R's `regime_at()`: each change row
    # marks "transition into the next regime". The id is not a segment
    # counter — segment_wise consumers index off `change`, not the id.
    columns: dict[str, Any] = dict(groups)
    columns["change"] = parsed
    columns["regime_id"] = [2] * n
    changes_df = pl.DataFrame(
        columns,
        schema_overrides={"regime_id": pl.Int64, "change": pl.Date},
    )

    group_col = next(iter(groups)) if groups else None
    return Regime._manual(
        changes_df=changes_df,
        treatment=treatment,
        groups=group_col,
    )


def regime_spec(
    target: str = "lr",
    K: int = 12,
    method: str = "e_divisive",
    *,
    n_regimes: int | None = None,
    sig_level: float = 0.05,
    R: int = 999,
    min_size: int = 3,
    seed: int | None = None,
    treatment: str = "latest_only",
) -> Callable[["Triangle"], "Regime"]:
    """Build a lazy regime-detection spec.

    Captures :meth:`Triangle.detect_regime` arguments without running
    detection. The returned closure is invoked by the consumer
    (fit / backtest) on its own *internal* triangle -- crucially, inside
    backtest this is the **masked** training triangle of each fold, so
    change points never peek at held-out cells.

    Contrast with :func:`regime_at`, which produces an eager Regime
    fixed at construction time.
    """
    def _spec(tri: "Triangle") -> "Regime":
        regime = tri.detect_regime(
            target=target,
            K=K,
            method=method,
            n_regimes=n_regimes,
            sig_level=sig_level,
            R=R,
            min_size=min_size,
            seed=seed,
        )
        regime.treatment = treatment
        return regime

    return _spec


# ---------------------------------------------------------------------------
# 4-type dispatch resolver (used by fit / backtest)
# ---------------------------------------------------------------------------


def _resolve_regime(
    regime_input: Any,
    triangle: "Triangle",
) -> "Regime | None":
    """Normalise a regime argument to ``Regime | None``.

    Accepts the four shapes the public API offers:

    - ``None``: no regime filter (returned as-is).
    - ``"auto"``: run :meth:`Triangle.detect_regime` on ``triangle``.
    - :class:`Regime`: returned as-is (already eagerly built).
    - callable ``f(triangle) -> Regime``: invoked on ``triangle``.

    Used by ``LR`` / ``Loss`` / ``Premium`` / ``Backtest`` to flatten
    user-supplied regime args before the cohort filter runs. When the
    triangle is a masked backtest fold, the callable / "auto" paths
    re-detect on the masked data -- the leakage-safe contract.
    """
    if regime_input is None:
        return None
    if isinstance(regime_input, Regime):
        return regime_input
    if isinstance(regime_input, str):
        if regime_input == "auto":
            return triangle.detect_regime()
        raise ValueError(
            f"regime string sentinel must be 'auto'; got {regime_input!r}"
        )
    if callable(regime_input):
        result = regime_input(triangle)
        if not isinstance(result, Regime):
            raise TypeError(
                f"regime spec callable must return Regime, got "
                f"{type(result).__name__}"
            )
        return result
    raise TypeError(
        f"regime must be None / 'auto' / Regime / Callable, got "
        f"{type(regime_input).__name__}"
    )


# ---------------------------------------------------------------------------
# Cohort filter (Phase B — `latest_only` mode only; segment_wise later)
# ---------------------------------------------------------------------------


def _regime_cutoff_map(regime: "Regime") -> pl.DataFrame | None:
    """Per-group ``(group_value, cutoff)`` table.

    For ``latest_only`` treatment the cutoff is the **latest** change
    point per group -- cohorts strictly before it are dropped. Returns
    ``None`` when the regime has no change points to apply.
    """
    if not regime.breakpoints:
        return None

    changes = regime._changes_df
    if regime.groups is None or regime.groups not in changes.columns:
        cutoff = max(regime.breakpoints)
        return pl.DataFrame({"__cutoff": [cutoff]}, schema={"__cutoff": pl.Date})

    return (
        changes.group_by(regime.groups)
        .agg(pl.col("change").max().alias("__cutoff"))
    )


def _apply_regime_filter(
    triangle: "Triangle",
    regime: "Regime | None",
) -> "Triangle":
    """Drop cohorts before the regime's cutoff (``latest_only`` mode).

    Returns a fresh ``Triangle`` over the surviving cells. When
    ``regime`` is ``None`` or has no change points, returns the input
    triangle unchanged.

    ``segment_wise`` treatment is recognised as a slot on the Regime
    but its per-segment factor mechanics are not yet wired -- callers
    that pass ``treatment="segment_wise"`` will currently see the same
    ``latest_only`` behaviour (followed by a per-segment phase in a
    later sweep).
    """
    if regime is None or not regime.breakpoints:
        return triangle

    from .triangle import Triangle

    df = triangle.to_polars()
    cutoff_map = _regime_cutoff_map(regime)
    if cutoff_map is None:
        return triangle

    if "__cutoff" in df.columns:  # defensive: should never collide
        df = df.drop("__cutoff")

    if regime.groups is not None and regime.groups in df.columns:
        df = df.join(cutoff_map, on=regime.groups, how="left")
    else:
        df = df.with_columns(
            pl.lit(cutoff_map["__cutoff"][0]).alias("__cutoff")
        )

    df = df.filter(
        pl.col("__cutoff").is_null() | (pl.col("cohort") >= pl.col("__cutoff"))
    ).drop("__cutoff")

    return Triangle._from_masked(triangle, df)
