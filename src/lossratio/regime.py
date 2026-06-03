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
from ._io import (
    _iter_group_frames,
    collapse_groups,
    fill_group_columns,
    group_eq,
    mirror_output,
    normalize_groups,
)

if TYPE_CHECKING:
    from .triangle import Triangle


_VALID_METHODS = ("e_divisive", "hclust")
_VALID_TREATMENTS = ("segment_bridged", "segment_bridged_borrowed")
_DERIVED_TARGETS = ("loss_ata", "premium_ata", "loss_ed")
# When ``window="auto"`` cannot resolve via the elbow heuristic (flat
# change-count curve, sweep failure, too few cohorts), fall back to this
# trajectory window. Matches R's ``WINDOW_AUTO_FALLBACK``.
_WINDOW_AUTO_FALLBACK = 6
_WINDOW_AUTO_SEQ = tuple(range(2, 25))  # 2..24, mirrors R default


def _derive_regime_target(
    df: pl.DataFrame,
    target: str,
    groups: str | list[str] | None,
) -> tuple[pl.DataFrame, str]:
    """Compute a diagnostic derived metric column on ``df``.

    Mirrors R's ``.derive_regime_target``. The first dev row per (group,
    cohort) is NA (no predecessor), so it is dropped and ``dev`` is
    re-indexed so the first surviving observation becomes ``dev = 1``.
    This lets the downstream eligibility filter (``n >= window``) and the
    feature-matrix pivot use the same code path as the native columns.

    Supported metrics:

    - ``"loss_ata"``: ``loss[k] / loss[k-1]`` per (group, cohort).
    - ``"premium_ata"``: ``premium[k] / premium[k-1]`` per (group, cohort).
    - ``"loss_ed"``: ``(loss[k] - loss[k-1]) / premium[k-1]``.

    ``"premium_ed"`` is an alias of ``"premium_ata"`` (constant offset of
    1; PCA standardisation removes the shift, so detection results
    coincide). The caller resolves the alias before calling.
    """
    if target not in _DERIVED_TARGETS:
        raise ValueError(
            f"Unknown derived metric: {target!r}. "
            f"Expected one of {_DERIVED_TARGETS}."
        )

    by_cols = [*normalize_groups(groups), "cohort"]

    if target == "loss_ata":
        derived = pl.col("loss") / pl.col("loss").shift(1).over(by_cols)
    elif target == "premium_ata":
        derived = pl.col("premium") / pl.col("premium").shift(1).over(by_cols)
    else:  # loss_ed
        derived = (
            (pl.col("loss") - pl.col("loss").shift(1).over(by_cols))
            / pl.col("premium").shift(1).over(by_cols)
        )

    out = df.with_columns(derived.alias(target))
    # Drop rows where the derived metric is non-finite (first dev per
    # cohort, plus any zero-denominator cases). polars treats NaN and
    # null separately, so test both.
    out = out.filter(
        pl.col(target).is_not_null() & pl.col(target).is_finite()
    )
    # Re-index dev so the first surviving period per cohort becomes 1.
    out = out.with_columns((pl.col("dev") - 1).alias("dev"))
    return out, target


def _kneedle_elbow(
    window: np.ndarray, change_count: np.ndarray
) -> int | None:
    """Kneedle elbow on a (decreasing) ``change_count`` vs ``window`` curve.

    Direct port of R's ``.kneedle_elbow`` in
    ``R/regime-optimal-window.R``. Both axes are normalised to ``[0, 1]``
    and the elbow is the index with the maximum vertical *deficit*
    below the diagonal line ``y = 1 - x``. Returns ``None`` when the
    curve is flat (zero range on y) or has fewer than 3 points; the
    caller then falls back to ``_WINDOW_AUTO_FALLBACK``.

    Tie-breaking deviates from R only in that numpy's ``argmax``
    returns the first maximum, matching R's ``which.max`` semantics.
    """
    window = np.asarray(window, dtype=float)
    change_count = np.asarray(change_count, dtype=float)
    n = window.size
    if n < 3:
        return None
    y_min = float(np.nanmin(change_count))
    y_max = float(np.nanmax(change_count))
    if not np.isfinite(y_max - y_min) or y_max == y_min:
        return None
    x_min = float(np.nanmin(window))
    x_max = float(np.nanmax(window))
    if x_max == x_min:
        return None

    k_norm = (window - x_min) / (x_max - x_min)
    bc_norm = (change_count - y_min) / (y_max - y_min)
    deficit = (1.0 - k_norm) - bc_norm
    idx = int(np.argmax(deficit))
    return int(window[idx])


def _resolve_by(
    by: str | Sequence[str] | None,
    triangle: "Triangle",
) -> "str | list[str] | None":
    """Normalise the ``by`` argument to a group spec or ``None``.

    Mirrors R's resolution:

    - ``None``: defer to ``triangle.groups`` (per-group when set, pooled
      otherwise) -- returns whatever the Triangle stores (a multi-column
      list once the Triangle carries multi-column groups).
    - ``""`` or empty sequence: force pooled detection on a grouped
      triangle.
    - ``str``: explicit single group column.
    - length-1 sequence: the single column name (``str``).
    - non-empty multi-element sequence: an EXPLICIT multi-column ``by`` --
      returned as a ``list[str]`` (the per-combination detection machinery
      handles it). Same scalar-vs-list collapse the Triangle uses for stored
      ``groups``.
    """
    if by is None:
        return triangle.groups
    if isinstance(by, str):
        if by == "":
            return None
        return by
    if isinstance(by, Sequence):
        seq = normalize_groups(by)  # validates str-only + no duplicates
        if not seq:
            return None
        if len(seq) == 1:
            return seq[0]
        return seq
    raise TypeError(
        f"`by` must be None, str, or sequence of str; got "
        f"{type(by).__name__}"
    )


def _detect_regime_single(
    sub: pl.DataFrame,
    *,
    target: str,
    window: int,
    method: str,
    n_regimes: int | None,
    sig_level: float,
    n_permutations: int,
    min_size: int,
    seed: int | None,
) -> dict[str, Any]:
    """Single-combo detection. Returns the per-combo result dict.

    Used both by :meth:`Regime._from_triangle` (multi-combo dispatch)
    and by :func:`_detect_regime_optimal_window` (per-window sweep).
    Raises ``ValueError`` when the feature matrix cannot be built; the
    caller decides whether to skip or propagate.
    """
    mat, cohorts, dropped = _build_feature_matrix(sub, target, window)
    n = len(cohorts)
    if method == "e_divisive":
        breaks_idx = _e_divisive_breakpoints(
            mat,
            sig_level=sig_level,
            n_permutations=n_permutations,
            min_size=min_size,
            seed=seed,
        )
    else:  # hclust
        n_reg = 2 if n_regimes is None else int(n_regimes)
        breaks_idx = _hclust_breakpoints(mat, n_regimes=n_reg)

    regime_ids = _regime_ids_from_breaks(n, breaks_idx)
    breakpoints = [cohorts[i] for i in breaks_idx]
    return {
        "cohorts": cohorts,
        "regime_ids": regime_ids,
        "breakpoints": breakpoints,
        "dropped": dropped,
        "n_regimes": int(regime_ids.max()) if n > 0 else 0,
    }


def _combine_combo_results(
    per_combo_results: list[tuple[Any, dict[str, Any]]],
    grp: str | None,
) -> tuple[pl.DataFrame, pl.DataFrame, list[Any], int, list[Any] | dict[Any, list[Any]]]:
    """Stack per-combo (labels, changes) into one frame each.

    For pooled detection (``grp is None``) the schemas match the
    single-group case and ``dropped`` is a flat list. For per-combo
    detection the group column is *prepended* to both frames, and
    ``dropped`` becomes ``{combo_value: [cohort, ...]}`` -- mirrors
    R's named-list shape for multi-group.
    """
    label_frames: list[pl.DataFrame] = []
    change_frames: list[pl.DataFrame] = []
    all_breakpoints: list[Any] = []
    n_regimes_max = 0
    dropped_out: list[Any] | dict[Any, list[Any]]

    if grp is None:
        # Pooled: single combo expected.
        _, res = per_combo_results[0]
        cohorts = res["cohorts"]
        regime_ids = res["regime_ids"]
        breakpoints = res["breakpoints"]

        label_frames.append(
            pl.DataFrame({"cohort": cohorts, "regime_id": regime_ids})
        )
        change_frames.append(
            pl.DataFrame(
                {
                    "change": breakpoints,
                    "regime_id": list(range(2, 2 + len(breakpoints))),
                },
                schema_overrides={"regime_id": pl.Int64},
            )
        )
        all_breakpoints.extend(breakpoints)
        n_regimes_max = max(n_regimes_max, res["n_regimes"])
        dropped_out = list(res["dropped"])
    else:
        dropped_dict: dict[Any, list[Any]] = {}
        for combo, res in per_combo_results:
            cohorts = res["cohorts"]
            regime_ids = res["regime_ids"]
            breakpoints = res["breakpoints"]

            # Group column(s) first (single col for a str grp, one per
            # name for a multi-column list grp with a tuple combo).
            lab_data: dict[str, Any] = {}
            fill_group_columns(lab_data, grp, combo, len(cohorts))
            lab_data["cohort"] = cohorts
            lab_data["regime_id"] = regime_ids
            label_frames.append(pl.DataFrame(lab_data))

            if breakpoints:
                ch_data: dict[str, Any] = {}
                fill_group_columns(ch_data, grp, combo, len(breakpoints))
                ch_data["change"] = breakpoints
                ch_data["regime_id"] = list(range(2, 2 + len(breakpoints)))
                change_frames.append(
                    pl.DataFrame(
                        ch_data,
                        schema_overrides={"regime_id": pl.Int64},
                    )
                )
                all_breakpoints.extend(breakpoints)
            n_regimes_max = max(n_regimes_max, res["n_regimes"])
            dropped_dict[combo] = list(res["dropped"])
        dropped_out = dropped_dict

    labels_df = (
        pl.concat(label_frames, how="vertical_relaxed")
        if label_frames
        else pl.DataFrame()
    )
    changes_df = (
        pl.concat(change_frames, how="vertical_relaxed")
        if change_frames
        else pl.DataFrame(
            {"change": [], "regime_id": []},
            schema={"change": pl.Date, "regime_id": pl.Int64},
        )
    )
    return labels_df, changes_df, all_breakpoints, n_regimes_max, dropped_out


def _detect_regime_optimal_window(
    tri_df: pl.DataFrame,
    *,
    target: str,
    window_seq: Sequence[int] = _WINDOW_AUTO_SEQ,
    method: str = "e_divisive",
    sig_level: float = 0.05,
    min_size: int = 3,
    n_permutations: int = 999,
    seed: int | None = None,
) -> int | None:
    """Pick a trajectory window via Kneedle elbow on the change-count curve.

    Internal helper, intentionally not exported. Mirrors R's
    ``detect_regime_optimal_window`` + ``.kneedle_elbow`` pair from
    ``R/regime-optimal-window.R``. Returns the elbow window or ``None``
    when no sweep value produced a usable detection (the caller falls
    back to ``_WINDOW_AUTO_FALLBACK``).

    Errors at individual sweep values (too few cohorts for that window,
    feature matrix containing NaN, etc.) are swallowed -- the window is
    simply omitted from the diagnostics curve.
    """
    sweep: list[tuple[int, int]] = []  # (window, change_count)
    for k in window_seq:
        try:
            res = _detect_regime_single(
                tri_df,
                target=target,
                window=int(k),
                method=method,
                n_regimes=None,
                sig_level=sig_level,
                n_permutations=n_permutations,
                min_size=min_size,
                seed=seed,
            )
        except ValueError:
            continue
        sweep.append((int(k), len(res["breakpoints"])))

    if not sweep:
        return None

    windows = np.array([w for w, _ in sweep])
    counts = np.array([c for _, c in sweep])
    return _kneedle_elbow(windows, counts)


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

    # Count *distinct dev values* with a non-null target per cohort.
    # When the input frame carries multiple rows per (cohort, dev)
    # (pooled detection on a multi-group triangle), raw row counts
    # would overstate eligibility -- the pivot aggregates to one cell
    # per (cohort, dev), so the eligibility test must do the same.
    counts = (
        df.filter(pl.col(target).is_not_null())
        .group_by("cohort")
        .agg(pl.col("dev").n_unique().alias("n"))
        .sort("cohort")
    )
    eligible = counts.filter(pl.col("n") >= K)["cohort"].to_list()
    # ``all_cohorts`` from the unfiltered frame: includes cohorts with
    # zero non-null target cells (which the filter above would drop).
    all_cohorts = (
        df["cohort"].unique().sort().to_list()
    )
    dropped = [c for c in all_cohorts if c not in set(eligible)]

    if not eligible:
        raise ValueError(
            f"No cohorts have >= K={K} observed development periods. "
            f"Reduce K."
        )

    # Pivot to wide form: rows = cohort, cols = dev 1..K. Use mean as
    # aggregator so pooled detection on a multi-group triangle (where
    # each (cohort, dev) cell has one row per group) collapses to a
    # single value per (cohort, dev). Matches R's
    # ``fun.aggregate = mean(..., na.rm=TRUE)``.
    wide = (
        df.filter(pl.col("cohort").is_in(eligible))
        .pivot(on="dev", index="cohort", values=target, aggregate_function="mean")
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
    n_permutations: int,
    min_size: int,
    seed: int | None,
) -> list[int]:
    """E-Divisive breakpoints (indices of right-side starts)."""
    res = e_divisive(
        mat,
        sig_level=sig_level,
        n_permutations=n_permutations,
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
    """Assign 1-based regime ids in cohort order.

    Cohort ``i`` gets ``1 + (number of break indices <= i)`` -- a
    right-side searchsorted over the sorted unique breaks.
    """
    bk = np.array(sorted(set(breaks)), dtype=np.int64)
    return (1 + np.searchsorted(bk, np.arange(n), side="right")).astype(
        np.int64
    )


class Regime:
    """Detected cohort regime structure.

    Use :meth:`Triangle.detect_regime` to construct.

    Attributes
    ----------
    method : str
        ``"e_divisive"`` or ``"hclust"``.
    target : str
        Trajectory variable name used.
    window : int
        Development-period window length (number of dev cells per cohort
        used as the feature vector). Mirrors R's ``window`` arg.
    cohort : str
        Original cohort variable name (e.g. ``"uy_m"``).
    breakpoints : list
        Cohort values at which a new regime starts (excluding the first
        cohort).
    n_regimes : int
        Number of regimes detected.
    dropped : list
        Cohorts excluded because they had fewer than ``window`` observed
        development periods.
    """

    def __init__(self) -> None:
        raise TypeError(
            "Regime is produced by `triangle.regime()` / `Regime.at()` / `.detect()`, not a direct constructor."
        )

    @classmethod
    def _from_triangle(
        cls,
        triangle: "Triangle",
        *,
        target: str = "ratio",
        window: int | str = "auto",
        by: str | Sequence[str] | None = None,
        method: str = "e_divisive",
        n_regimes: int | None = None,
        sig_level: float = 0.05,
        n_permutations: int = 999,
        min_size: int = 3,
        seed: int | None = None,
        treatment: str = "segment_bridged",
    ) -> "Regime":
        if method not in _VALID_METHODS:
            raise ValueError(
                f"method must be one of {_VALID_METHODS}, got {method!r}"
            )
        if treatment not in _VALID_TREATMENTS:
            raise ValueError(
                f"treatment must be one of {_VALID_TREATMENTS}, got {treatment!r}"
            )

        # `premium_ed` is an alias of `premium_ata` (constant offset of 1
        # absorbed by PCA standardisation -- detection produces identical
        # changes). Resolve before validation. Mirrors R behaviour.
        if target == "premium_ed":
            target = "premium_ata"

        # `window="auto"` defers to the optimal-window elbow heuristic.
        # Plain integer windows pass through after a >= 2 check.
        window_is_auto = isinstance(window, str) and window == "auto"
        if isinstance(window, str) and not window_is_auto:
            raise ValueError(
                f"`window` must be an integer >= 2 or 'auto'; got {window!r}"
            )
        if not window_is_auto:
            if not isinstance(window, (int, np.integer)) or window < 2:
                raise ValueError(
                    f"window must be an integer >= 2, got {window!r}"
                )
            window = int(window)

        # Resolve `by`:
        #   None (default) -> use triangle.groups if set, else pooled
        #   "" / []        -> force pooled
        #   str            -> single group column
        #   list[str]      -> multi-column grouping (per-combination detect)
        grp = _resolve_by(by, triangle)

        tri_df = triangle.to_polars()

        # Build the source frame: either native column or derived metric.
        if target in _DERIVED_TARGETS:
            tri_df, target = _derive_regime_target(
                tri_df, target, groups=grp
            )
        elif target not in tri_df.columns:
            raise ValueError(
                f"target={target!r} not found in Triangle columns: "
                f"{tri_df.columns}"
            )

        # Per-combo dispatch. Empty `combos` => pooled (single combo over
        # the whole frame).
        if grp is None:
            combos: list[Any] = [None]
        elif isinstance(grp, str):
            combos = (
                tri_df[grp]
                .unique()
                .sort()
                .to_list()
            )
        else:
            # multi-column: unique group-combination TUPLES (sorted).
            combos = [
                tuple(r)
                for r in tri_df.select(grp).unique().sort(grp).iter_rows()
            ]

        # Partition the frame ONCE into a {combo: sub-frame} map and reuse it
        # in both the window-auto sweep and the detection loop, instead of a
        # full-frame `filter` per combo (which was O(combos x rows)). `combos`
        # stays the authoritative ORDERED list -- only the sub-frame retrieval
        # changes -- so the per-combo result order (and thus output) is
        # unchanged. Keys from `_iter_group_frames` match the `combos` entries
        # (scalar for a str group, tuple for multi-column).
        if grp is None:
            sub_by_combo: dict[Any, pl.DataFrame] = {None: tri_df}
        else:
            sub_by_combo = dict(_iter_group_frames(tri_df, grp))

        # Resolve trajectory window per combo. ``window="auto"`` first
        # tries the maturity point (``detect_maturity`` on the Triangle)
        # for each combo. When maturity is unavailable (pooled
        # detection, NA maturity, or `by` mismatching the Triangle's
        # stored groups) it falls back to the Kneedle elbow on the
        # change-count sweep; if the elbow is also undefined, falls
        # back to ``_WINDOW_AUTO_FALLBACK``. Mirrors R's
        # ``detect_regime(window="auto")`` fallback chain in
        # ``R/regime.R``.
        if window_is_auto:
            # Map the regime target -> a valid cumulative target for
            # detect_maturity (which supports cumulative metrics only).
            # Derived targets fall back to "ratio".
            _MAT_LOSS_MAP = {
                "ratio": "ratio",
                "loss": "loss",
                "premium": "premium",
                "incr_ratio": "ratio",
                "incr_loss": "loss",
                "incr_premium": "premium",
            }
            mat_loss = _MAT_LOSS_MAP.get(target, "ratio")

            # First pass: try maturity. Pooled detection (grp is None)
            # cannot match maturity rows back to combos, so skip.
            maturity_by_combo: dict[Any, int | None] = {}
            if grp is not None:
                try:
                    mat = triangle.detect_maturity(target=mat_loss)
                    mat_df = mat.summary()
                    # Coerce mat_df to polars if needed (mirror_output
                    # may have returned pandas).
                    if not isinstance(mat_df, pl.DataFrame):
                        mat_df = pl.from_pandas(mat_df)
                    gcols = normalize_groups(grp)
                    if (
                        all(g in mat_df.columns for g in gcols)
                        and "change" in mat_df.columns
                    ):
                        # Vectorised: per group key, change -> int, mapping
                        # null / NaN to None (mirrors the per-row int(v)
                        # with the None / NaN guards). The key is a scalar
                        # for a str group, a tuple for a multi-column group
                        # (matching the `combos` entries).
                        change_f = pl.col("change").cast(
                            pl.Float64, strict=False
                        )
                        mats = (
                            mat_df.select(
                                pl.when(change_f.is_null() | change_f.is_nan())
                                .then(None)
                                .otherwise(change_f.cast(pl.Int64))
                                .alias("_mat")
                            )["_mat"].to_list()
                        )
                        if isinstance(grp, str):
                            keys: list[Any] = mat_df[grp].to_list()
                        else:
                            keys = [
                                tuple(r) for r in mat_df.select(grp).iter_rows()
                            ]
                        maturity_by_combo = dict(zip(keys, mats))
                except (ValueError, KeyError, RuntimeError):
                    # detect_maturity may raise on degenerate input
                    # (no valid links, single-cohort triangle, etc.).
                    # The elbow fallback handles these.
                    maturity_by_combo = {}

            window_per_combo: list[int] = []
            for combo in combos:
                # Maturity-first path (per-combo when grp is set).
                mat_k = maturity_by_combo.get(combo) if grp is not None else None
                if mat_k is not None and mat_k >= 2:
                    window_per_combo.append(mat_k)
                    continue
                # Elbow fallback.
                sub = sub_by_combo[combo]
                k = _detect_regime_optimal_window(
                    sub,
                    target=target,
                    window_seq=_WINDOW_AUTO_SEQ,
                    method="e_divisive",
                    sig_level=sig_level,
                    min_size=min_size,
                    n_permutations=n_permutations,
                    seed=seed,
                )
                window_per_combo.append(
                    k if k is not None else _WINDOW_AUTO_FALLBACK
                )
        else:
            window_per_combo = [int(window)] * len(combos)

        # Run detection per combo. Failures are skipped (per-combo
        # robustness: a single short coverage shouldn't kill the whole
        # multi-group call). Skipped combos contribute zero rows to
        # `changes`/`labels`.
        per_combo_results: list[tuple[Any, dict[str, Any]]] = []
        for combo, k in zip(combos, window_per_combo):
            sub = sub_by_combo[combo]
            try:
                res = _detect_regime_single(
                    sub,
                    target=target,
                    window=k,
                    method=method,
                    n_regimes=n_regimes,
                    sig_level=sig_level,
                    n_permutations=n_permutations,
                    min_size=min_size,
                    seed=seed,
                )
            except ValueError:
                continue
            per_combo_results.append((combo, res))

        if not per_combo_results:
            raise ValueError(
                "No group / combo produced a usable detection result. "
                "Check `window`, `min_size`, and input coverage."
            )

        labels_df, changes_df, breakpoints, n_regimes_total, dropped = (
            _combine_combo_results(per_combo_results, grp)
        )

        self = cls.__new__(cls)
        self._labels_df = labels_df
        self._changes_df = changes_df
        self._output_type = triangle._output_type
        self.method = method
        self.target = target
        # ``window`` is scalar for single-combo, list[int] for multi-combo.
        # Matches R's single-vs-vector unwrap.
        self.window = (
            window_per_combo[0]
            if len(per_combo_results) == 1 and grp is None
            else (
                window_per_combo[0]
                if len(window_per_combo) == 1
                else list(window_per_combo)
            )
        )
        self.cohort = triangle.cohort
        self.dev = triangle.dev
        self.groups = grp
        self.breakpoints = breakpoints
        self.n_regimes = n_regimes_total
        self.dropped = dropped
        self.treatment = treatment
        return self

    @classmethod
    def _manual(
        cls,
        *,
        changes_df: pl.DataFrame,
        treatment: str,
        groups: str | list[str] | None,
    ) -> "Regime":
        """Construct a Regime by hand (no auto-detection).

        Used by :meth:`Regime.at` to wrap user-supplied change points.
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
        self.window = 0
        self.cohort = ""
        self.dev = ""
        self.groups = groups
        self.breakpoints = changes_df["change"].to_list()
        self.n_regimes = 0
        self.dropped = []
        self.treatment = treatment
        return self

    @classmethod
    def at(
        cls,
        change: Any,
        *,
        groups: Mapping[str, Sequence[Any]] | None = None,
        treatment: str = "segment_bridged",
    ) -> "Regime":
        """Build a :class:`Regime` from explicit, user-supplied change points.

        Use this when you already know where the cohort regime shifts (e.g.
        a policy revision date) and want a *fixed* regime tested across
        backtest folds. Contrast with :meth:`detect`, which defers
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
            Regime application mode. Both modes mask the triangle to a
            *bridged* development band -- each segment's mini-triangle wall
            (``dev >= max_cal - seg_last + 1``) widened by a
            calendar-diagonal bridge to the next segment's first-cohort
            midpoint dev, which closes the factor gaps at the segment
            boundaries so every cohort projects to full development.
            ``"segment_bridged"`` (default) pools the whole band into a
            single factor set (the development pattern is shared across
            regimes). ``"segment_bridged_borrowed"`` estimates factors per
            segment (early-dev factors stay regime-specific) and borrows
            the late-dev factors a segment cannot reach from a donor
            segment that can.

        Returns
        -------
        Regime
            A manually-constructed Regime suitable for the same 4-type
            dispatch slots that accept auto-detected Regimes.

        Examples
        --------
        >>> Regime.at(change="2024-07-01")
        >>> Regime.at(
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
        # counter -- segment_wise consumers index off `change`, not the id.
        columns: dict[str, Any] = dict(groups)
        columns["change"] = parsed
        columns["regime_id"] = [2] * n
        changes_df = pl.DataFrame(
            columns,
            schema_overrides={"regime_id": pl.Int64, "change": pl.Date},
        )

        group_spec = collapse_groups(list(groups.keys())) if groups else None
        return cls._manual(
            changes_df=changes_df,
            treatment=treatment,
            groups=group_spec,
        )

    @classmethod
    def detect(
        cls,
        target: str = "ratio",
        window: int = 12,
        method: str = "e_divisive",
        *,
        n_regimes: int | None = None,
        sig_level: float = 0.05,
        n_permutations: int = 999,
        min_size: int = 3,
        seed: int | None = None,
        treatment: str = "segment_bridged",
    ) -> Callable[["Triangle"], "Regime"]:
        """Build a lazy regime-detection spec.

        Captures :meth:`Triangle.detect_regime` arguments without running
        detection. The returned closure is invoked by the consumer
        (fit / backtest) on its own *internal* triangle -- crucially, inside
        backtest this is the **masked** training triangle of each fold, so
        change points never peek at held-out cells.

        Contrast with :meth:`at`, which produces an eager Regime fixed at
        construction time.
        """
        def _spec(tri: "Triangle") -> "Regime":
            regime = tri.detect_regime(
                target=target,
                window=window,
                method=method,
                n_regimes=n_regimes,
                sig_level=sig_level,
                n_permutations=n_permutations,
                min_size=min_size,
                seed=seed,
            )
            regime.treatment = treatment
            return regime

        return _spec

    @property
    def changes(self):
        """Detected (or manually specified) change points as a frame."""
        return mirror_output(self._changes_df, self._output_type)

    @property
    def df(self):
        """Per-cohort regime labels in the original input format."""
        return mirror_output(self._labels_df, self._output_type)

    def plot(
        self,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
        palette: str = "tab10",
    ) -> Any:
        """Cohort timeline plot, backed by matplotlib.

        One panel per group; each panel shows cohorts coloured by
        their detected ``regime_id`` with vertical dashed lines at
        change points.

        **R divergence:** the R sibling's ``plot.Regime`` draws a PCA
        scatter of cohort trajectories with loading arrows and 90%
        ellipses, sourced from the Regime object's ``trajectory`` /
        ``pca`` / ``labels`` slots. Python ``Regime`` carries only
        the per-cohort labels + change points; the trajectory + PCA
        would require a heavier-state refactor of ``_from_triangle``.
        The cohort-timeline plot answers the same "what regimes / where
        do they switch" question without that cost.

        Parameters
        ----------
        nrow, ncol
            Facet layout. Defaults to one row per group.
        figsize
            Passed to ``plt.subplots``.
        palette
            Matplotlib colormap name used for regime_id colouring.
            Default ``"tab10"``.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from ._regime_vis import plot_regime
        return plot_regime(
            self, nrow=nrow, ncol=ncol, figsize=figsize, palette=palette
        )

    def to_polars(self) -> pl.DataFrame:
        return self._labels_df

    def to_pandas(self):
        return self._labels_df.to_pandas()

    def __repr__(self) -> str:
        n_cohorts = self._labels_df.height
        bits = [
            f"method={self.method}",
            f"target={self.target!r}",
            f"window={self.window}",
            f"{n_cohorts} cohorts",
            f"{self.n_regimes} regimes",
        ]
        if self.dropped:
            bits.append(f"{len(self.dropped)} dropped")
        return f"<Regime: {', '.join(bits)}>"


# ---------------------------------------------------------------------------
# Internal helper for Regime.at
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

    Used by ``Ratio`` / ``Loss`` / ``Premium`` / ``Backtest`` to flatten
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
# Cohort filter + segment annotation
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
    gcols = normalize_groups(regime.groups)
    if not gcols or not all(g in changes.columns for g in gcols):
        cutoff = max(regime.breakpoints)
        return pl.DataFrame({"_cutoff": [cutoff]}, schema={"_cutoff": pl.Date})

    return (
        changes.group_by(gcols)
        .agg(pl.col("change").max().alias("_cutoff"))
    )


def _segment_id_expr(
    regime: "Regime",
    cohort_col: str = "cohort",
) -> pl.Expr | None:
    """Build a polars expression that assigns 1-based segment ids to
    rows based on ``cohort`` and ``regime._changes_df``.

    Implements R's ``findInterval(cohort, sorted_changes) + 1L``: a row
    earlier than the first change is segment 1, between the k-th and
    (k+1)-th change is segment k+1, on or after the K-th change is
    segment K+1.

    For multi-group regimes the expression is built per-group via
    nested ``when`` chains. Returns ``None`` when the regime has no
    changes (caller should default everything to segment 1).
    """
    if not regime.breakpoints:
        return None

    changes = regime._changes_df
    gcols = normalize_groups(regime.groups)
    is_multi = bool(gcols) and all(g in changes.columns for g in gcols)

    def _single_group_expr(sorted_changes: list[Any]) -> pl.Expr:
        # cohort >= ch_K  -> K+1
        # ch_{k-1} <= cohort < ch_k -> k
        # cohort < ch_1 -> 1
        expr = pl.lit(1, dtype=pl.Int64)
        for i, ch in enumerate(sorted_changes, start=2):
            expr = (
                pl.when(pl.col(cohort_col) >= ch).then(i).otherwise(expr)
            )
        return expr

    if not is_multi:
        sorted_changes = sorted(regime.breakpoints)
        return _single_group_expr(sorted_changes)

    # Per-group: build nested when-chain keyed by group value (scalar for
    # a str group, tuple for a multi-column group).
    grp = regime.groups
    expr = pl.lit(1, dtype=pl.Int64)  # default: groups not in regime → seg 1
    for grp_val, sub in _iter_group_frames(changes, grp):
        sub = sub.sort("change")
        sorted_changes = sub["change"].to_list()
        grp_expr = _single_group_expr(sorted_changes)
        expr = pl.when(group_eq(grp, grp_val)).then(grp_expr).otherwise(expr)
    return expr


def _compute_segment_mini_tri_bounds(
    coh_ranks: np.ndarray,
    seg_ids: np.ndarray,
    max_cal: int,
    bridge: bool = False,
) -> np.ndarray:
    """Per-cell effective ``dev_min`` for the segment mini-triangle band.

    For each cell, returns the minimum dev that keeps it inside its
    segment's fit mask. The mask is the union of two regions:

    1. The segment's natural mini-triangle:
       ``dev >= max_cal - seg_last + 1``.
    2. A *bridge* extension along the calendar diagonal anchored at the
       *next* (newer) segment's first-cohort midpoint dev. The bridge
       lets each older segment connect to its successor, filling the
       late-dev cells of its early cohorts that would otherwise be cut
       by the natural mini-triangle wall.

    Bridge construction (segments ordered by ``seg_id``, lower id =
    older cohorts). For each segment ``s`` except the newest (no
    successor), find segment ``s+1``'s

    * ``first_rank`` -- cohort rank of ``s+1``'s first cohort,
    * ``seg_dev_min`` -- ``max_cal - last_rank(s+1) + 1``,
    * ``first_cohort_dev_max`` -- ``max_cal - first_rank(s+1) + 1``,
    * ``mid_dev`` -- ``(seg_dev_min + first_cohort_dev_max) // 2``.

    The bridge diagonal for segment ``s`` is at
    ``ext_cal_idx(s) = first_rank(s+1) + mid_dev(s+1) - 2`` (the cell
    one cohort earlier than ``s+1``'s first cohort, at the same dev as
    that first cohort's mini-triangle midpoint). Each cell in segment
    ``s`` then takes
    ``effective_dev_min =
        min(seg_dev_min(s), ext_cal_idx(s) - coh_rank + 1)``.
    For the newest segment ``ext_cal_idx`` is undefined and only the
    natural wall applies.

    Bridges do not cascade: segment ``s`` is bridged only from segment
    ``s+1``, not from ``s+2``. The bridge only ever *widens* a
    segment's mini-triangle.

    Mirrors R's ``.compute_segment_mini_tri_bounds`` in ``R/utils.R``.

    Parameters
    ----------
    coh_ranks
        Per-cell cohort rank within the group (1-based, dense).
    seg_ids
        Per-cell segment id (1 = oldest).
    max_cal
        Maximum calendar index in the group.
    bridge
        When ``True``, widen each older segment's mini-triangle with
        the calendar-diagonal bridge anchored at the next segment's
        first-cohort midpoint dev. When ``False`` (default), return
        the natural mini-triangle wall only -- the pure
        natural mini-triangle wall (no boundary-gap closure); retained
        for diagnostics and the helper unit tests. Both segment
        treatments pass ``True``.

    Returns
    -------
    np.ndarray
        Integer per-cell effective ``dev_min`` for the mini-triangle
        filter (bridged or pure). Same length as ``coh_ranks``.
    """
    coh_ranks = np.asarray(coh_ranks, dtype=np.int64)
    seg_ids = np.asarray(seg_ids, dtype=np.int64)
    if coh_ranks.size == 0:
        return np.empty(0, dtype=np.int64)

    max_cal = int(max_cal)
    unique_segs = np.unique(seg_ids)
    seg_first: dict[int, int] = {}
    seg_last: dict[int, int] = {}
    for s in unique_segs:
        mask = seg_ids == s
        seg_first[int(s)] = int(coh_ranks[mask].min())
        seg_last[int(s)] = int(coh_ranks[mask].max())

    seg_dev_min = {s: max_cal - seg_last[s] + 1 for s in seg_first}
    natural = np.fromiter(
        (seg_dev_min[int(s)] for s in seg_ids),
        dtype=np.int64,
        count=seg_ids.size,
    )

    if not bridge:
        return natural

    first_cohort_dev_max = {s: max_cal - seg_first[s] + 1 for s in seg_first}
    mid_dev = {
        s: (seg_dev_min[s] + first_cohort_dev_max[s]) // 2
        for s in seg_first
    }

    seg_sorted = sorted(seg_first)
    ext_cal_idx: dict[int, int | None] = {s: None for s in seg_sorted}
    for i in range(len(seg_sorted) - 1):
        s_curr = seg_sorted[i]
        s_next = seg_sorted[i + 1]
        ext_cal_idx[s_curr] = seg_first[s_next] + mid_dev[s_next] - 2

    out = np.empty_like(natural)
    for i in range(seg_ids.size):
        s = int(seg_ids[i])
        ext = ext_cal_idx[s]
        if ext is None:
            out[i] = natural[i]
        else:
            out[i] = min(int(natural[i]), int(ext) - int(coh_ranks[i]) + 1)
    return out


def _apply_mini_triangle_filter(
    df: pl.DataFrame,
    regime: "Regime",
) -> pl.DataFrame:
    """Mask the triangle to the bridged development band.

    For each (group, segment) cell, keeps rows where
    ``dev >= effective_dev_min``, where the bound is the per-segment
    mini-triangle wall (``max_cal - seg_last + 1``) widened by the
    calendar-diagonal bridge to the next segment's first-cohort midpoint
    dev (see :func:`_compute_segment_mini_tri_bounds`). Both segment
    treatments use the bridged band.

    Under ``treatment="segment_bridged"`` the ``segment_id`` tag is
    dropped after masking so downstream estimation pools the whole band
    into one factor set. Under ``treatment="segment_bridged_borrowed"``
    the tag is kept for per-segment estimation.
    """
    if df.height == 0:
        return df
    bridge = True
    gcols = normalize_groups(regime.groups)
    grouped = bool(gcols) and all(g in df.columns for g in gcols)
    rank_keys = gcols if grouped else []

    df = df.with_columns(
        pl.col("cohort")
        .rank(method="dense")
        .over(rank_keys or pl.lit(1))
        .cast(pl.Int64)
        .alias("_coh_rank"),
    )
    df = df.with_columns(
        (pl.col("_coh_rank") + pl.col("dev") - 1).alias("_cal_idx"),
    )
    df = df.with_columns(
        pl.col("_cal_idx").max().over(rank_keys or pl.lit(1)).alias("_max_cal"),
    )

    # Bridge requires cross-segment access (next-segment anchor) that
    # is awkward in pure polars `over`; compute per-group via numpy. The
    # natural wall flows through the same helper for parity with R.
    if grouped:
        parts: list[pl.DataFrame] = []
        for _, sub in df.group_by(gcols, maintain_order=True):
            bounds = _compute_segment_mini_tri_bounds(
                coh_ranks=sub["_coh_rank"].to_numpy(),
                seg_ids=sub["segment_id"].to_numpy(),
                max_cal=int(sub["_max_cal"][0]),
                bridge=bridge,
            )
            parts.append(sub.with_columns(pl.Series("_dev_min", bounds)))
        df = pl.concat(parts, how="vertical_relaxed")
    else:
        bounds = _compute_segment_mini_tri_bounds(
            coh_ranks=df["_coh_rank"].to_numpy(),
            seg_ids=df["segment_id"].to_numpy(),
            max_cal=int(df["_max_cal"][0]),
            bridge=bridge,
        )
        df = df.with_columns(pl.Series("_dev_min", bounds))

    df = df.filter(pl.col("dev") >= pl.col("_dev_min")).drop(
        "_coh_rank", "_cal_idx", "_max_cal", "_dev_min"
    )
    # segment_bridged pools the masked band -- drop the per-segment tag
    # so downstream estimation does not group by segment_id.
    if regime.treatment == "segment_bridged" and "segment_id" in df.columns:
        df = df.drop("segment_id")
    return df


def _apply_regime_filter(
    triangle: "Triangle",
    regime: "Regime | None",
) -> "Triangle":
    """Mask ``triangle`` to the bridged development band for a regime.

    Both treatments mask the triangle to the bridged band -- each
    segment's mini-triangle wall (``dev >= max_cal - seg_last + 1``)
    widened by a calendar-diagonal bridge to the next segment's
    first-cohort midpoint dev. The bridge closes the factor gaps at the
    segment boundaries so a continuous factor run covers every dev and
    every cohort projects to full development.

    - ``"segment_bridged"`` (default): the masked band drops its
      ``segment_id`` tag so downstream estimation pools the band into a
      single factor set. This function returns the pooled masked
      triangle directly.

    - ``"segment_bridged_borrowed"``: keeps ``segment_id`` so the fit
      dispatcher routes to per-segment estimation + late-dev borrow (it
      does not reach this function; the dispatcher calls
      :func:`_split_into_segment_triangles` instead). When it does reach
      here (e.g. premium-side fits without a borrow path), the masked
      triangle carries ``segment_id``.

    When ``regime`` is ``None`` or has no change points, the input
    triangle is returned unchanged.
    """
    if regime is None or not regime.breakpoints:
        return triangle

    from .triangle import Triangle

    df = triangle.to_polars()
    seg_expr = _segment_id_expr(regime)
    if seg_expr is None:
        return triangle
    df = df.with_columns(seg_expr.alias("segment_id"))
    df = _apply_mini_triangle_filter(df, regime)
    return Triangle._from_masked(triangle, df)


def _split_into_segment_triangles(
    triangle: "Triangle",
    regime: "Regime",
) -> dict[int, "Triangle"]:
    """Split a Triangle into per-segment mini-Triangles.

    Used by fit consumers for ``treatment="segment_bridged_borrowed"``:
    each segment is fit independently. Returns a mapping
    ``{segment_id: mini_triangle}`` ordered by segment_id ascending.
    The mini-triangle filter is applied first so each segment's cells
    obey the equal-trapezoid window.

    Each returned Triangle is built by ``Triangle._from_masked`` and
    shares metadata (groups / cohort / grain / dev) with the source.
    The ``segment_id`` column is dropped before the mini-Triangle
    leaves this helper -- the standard Triangle schema has no such
    column, so downstream fits treat each piece as a normal Triangle.
    """
    from .triangle import Triangle

    if not regime.breakpoints:
        return {1: triangle}

    df = triangle.to_polars()
    seg_expr = _segment_id_expr(regime)
    if seg_expr is None:
        return {1: triangle}
    df = df.with_columns(seg_expr.alias("segment_id"))
    df = _apply_mini_triangle_filter(df, regime)

    out: dict[int, Triangle] = {}
    for seg_id in sorted(df["segment_id"].unique().to_list()):
        sub = df.filter(pl.col("segment_id") == seg_id).drop("segment_id")
        if sub.height == 0:
            continue
        out[int(seg_id)] = Triangle._from_masked(triangle, sub)
    return out
