"""Regime: structural change-point detection across underwriting cohorts.

Each cohort is treated as a feature vector (the chosen ``target`` over
durations 1, ..., K). The ordered sequence of cohort vectors
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
from dataclasses import dataclass
from datetime import date, datetime
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.cluster.hierarchy import fcluster, linkage
from scipy.spatial.distance import pdist
from scipy.stats import f as _f_dist
from scipy.stats import ttest_ind

from .._kernels.e_divisive import e_divisive
from .._kernels.io import (
    collapse_groups,
    fill_group_columns,
    iter_group_frames,
    mirror_output,
    normalize_groups,
)

if TYPE_CHECKING:
    from ..core.triangle import Triangle


_VALID_METHODS = ("e_divisive", "hclust")
_DERIVED_TARGETS = ("loss_ata", "premium_ata", "loss_intensity")
# When ``window="auto"`` cannot resolve via the elbow heuristic (flat
# change-count curve, sweep failure, too few cohorts), fall back to this
# trajectory window.
_WINDOW_AUTO_FALLBACK = 6
_WINDOW_AUTO_SEQ = tuple(range(2, 25))  # 2..24, the default sweep range

# Step-vs-drift gate for `_assess_change`. A change is a discrete regime
# (kind="step") when a level step at the change cohort, added over a linear
# cohort trend, improves the fit at F-test significance `_STEP_SIG`; else it
# is "drift" (the trend already explains the cohort series). The F-test is
# n-aware (calibrated), unlike a raw delta-R^2 threshold which is blind to
# sample size and noise scale. Needs `n - 3 >= 1` residual dof, i.e. at
# least `_MIN_ASSESS_N` cohorts total (and >= 2 on each side).
_STEP_SIG = 0.05
_MIN_ASSESS_N = 4


def _derive_regime_target(
    df: pl.DataFrame,
    target: str,
    groups: str | list[str] | None,
) -> tuple[pl.DataFrame, str]:
    """Compute a diagnostic derived metric column on ``df``.

    The first duration row per (group, cohort) is NA (no predecessor), so it
    is dropped and ``duration`` is
    re-indexed so the first surviving observation becomes ``duration = 1``.
    This lets the downstream eligibility filter (``n >= window``) and the
    feature-matrix pivot use the same code path as the native columns.

    Supported metrics:

    - ``"loss_ata"``: ``loss[k] / loss[k-1]`` per (group, cohort).
    - ``"premium_ata"``: ``premium[k] / premium[k-1]`` per (group, cohort).
    - ``"loss_intensity"``: ``(loss[k] - loss[k-1]) / premium[k-1]``.

    ``"premium_intensity"`` is an alias of ``"premium_ata"`` (constant offset of
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
    else:  # loss_intensity
        derived = (
            (pl.col("loss") - pl.col("loss").shift(1).over(by_cols))
            / pl.col("premium").shift(1).over(by_cols)
        )

    out = df.with_columns(derived.alias(target))
    # Drop rows where the derived metric is non-finite (first duration per
    # cohort, plus any zero-denominator cases). polars treats NaN and
    # null separately, so test both.
    out = out.filter(
        pl.col(target).is_not_null() & pl.col(target).is_finite()
    )
    # Re-index duration so the first surviving period per cohort becomes 1.
    out = out.with_columns((pl.col("duration") - 1).alias("duration"))
    return out, target


def _kneedle_elbow(
    window: np.ndarray, change_count: np.ndarray
) -> int | None:
    """Kneedle elbow on a (decreasing) ``change_count`` vs ``window`` curve.

    Both axes are normalised to ``[0, 1]`` and the elbow is the index with
    the maximum vertical *deficit* below the diagonal line ``y = 1 - x``.
    Returns ``None`` when the curve is flat (zero range on y) or has fewer
    than 3 points; the caller then falls back to ``_WINDOW_AUTO_FALLBACK``.

    Ties are broken toward the first maximum (``argmax`` returns the
    earliest index).
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
    count_norm = (change_count - y_min) / (y_max - y_min)
    deficit = (1.0 - k_norm) - count_norm
    idx = int(np.argmax(deficit))
    return int(window[idx])


def _resolve_by(
    by: str | Sequence[str] | None,
    triangle: Triangle,
) -> str | list[str] | None:
    """Normalise the ``by`` argument to a group spec or ``None``.

    - ``None``: defer to ``triangle.groups`` (per-group when set, pooled
      otherwise) -- returns whatever the Triangle stores (a multi-column
      list once the Triangle carries multi-column groups).
    - ``""`` or empty sequence: force pooled detection on a grouped
      triangle.
    - ``str``: explicit single group column.
    - length-1 sequence: the single column name (``str``).
    - non-empty multi-element sequence: an EXPLICIT multi-column ``by`` --
      returned as a ``list[str]`` (the per-combination detection machinery
      handles it). Uses the same scalar-vs-list collapse the Triangle
      applies to stored ``groups``.
    """
    if by is None:
        return triangle.groups
    if isinstance(by, str):
        if by == "":
            return None
        seq = [by]
    elif isinstance(by, Sequence):
        seq = normalize_groups(by)  # validates str-only + no duplicates
        if not seq:
            return None
    else:
        raise TypeError(
            f"`by` must be None, str, or sequence of str; got "
            f"{type(by).__name__}"
        )
    # `by` selects a coarser detection grain: it must be a subset of the
    # triangle's own group columns (else the group_by hits a missing column
    # and polars raises a bare ColumnNotFoundError far downstream).
    group_cols = normalize_groups(triangle.groups)
    extra = [c for c in seq if c not in group_cols]
    if extra:
        raise ValueError(
            f"by={seq} must be a subset of the triangle's groups {group_cols}; "
            f"{extra} {'is' if len(extra) == 1 else 'are'} not a group column."
        )
    return seq[0] if len(seq) == 1 else seq


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
    edge_scan: bool = False,
    edge_threshold: float = 10.0,
    with_assess: bool = False,
) -> dict[str, Any]:
    """Single-combo detection. Returns the per-combo result dict.

    Used both by :meth:`Regime._from_triangle` (multi-combo dispatch)
    and by :func:`_detect_regime_optimal_window` (per-window sweep).
    Raises ``ValueError`` when the feature matrix cannot be built; the
    caller decides whether to skip or propagate.

    When ``edge_scan`` is set (E-Divisive only), a 1-vs-rest effect-size
    scan (:func:`_edge_scan_change_points`) augments the permutation change
    points with any boundary regime E-Divisive structurally cannot reach. Edge
    change points carry a ``NaN`` p-value (they are effect-size gated, not
    permutation tested) so a downstream FDR pass leaves them untouched.
    """
    mat, cohorts, dropped = _build_feature_matrix(sub, target, window)
    n = len(cohorts)
    if method == "e_divisive":
        change_idxs, p_vals = _e_divisive_change_points(
            mat,
            sig_level=sig_level,
            n_permutations=n_permutations,
            min_size=min_size,
            seed=seed,
        )
        if edge_scan:
            for e in _edge_scan_change_points(mat, threshold=edge_threshold, min_size=min_size):
                if e not in change_idxs:
                    change_idxs.append(e)
                    p_vals.append(float("nan"))
            if change_idxs:
                paired = sorted(zip(change_idxs, p_vals, strict=False), key=lambda t: t[0])
                change_idxs = [b for b, _ in paired]
                p_vals = [p for _, p in paired]
    else:  # hclust
        n_reg = 2 if n_regimes is None else int(n_regimes)
        change_idxs = _hclust_change_points(mat, n_regimes=n_reg)
        p_vals = [float("nan")] * len(change_idxs)

    regime_ids = _regime_ids_from_changes(n, change_idxs)
    change_points = [cohorts[i] for i in change_idxs]
    result: dict[str, Any] = {
        "cohorts": cohorts,
        "regime_ids": regime_ids,
        "change_points": change_points,
        "p_values": p_vals,
        "dropped": dropped,
        "n_regimes": int(regime_ids.max()) if n > 0 else 0,
    }
    if with_assess:
        # Quantify each change point on the cohort-level scalar (mean over the
        # window) via the shared kernel. Aligned with ``change_points``.
        scalar = mat.mean(axis=1)
        result["assess"] = [_assess_change(scalar, i) for i in change_idxs]
    return result


_ASSESS_KEYS = (
    "n_pre", "n_post", "level_shift", "t_stat", "p_value",
    "delta_r2", "step_p", "curved_drift_suspect", "kind",
)


def _build_candidates_df(
    per_combo_results: list[tuple[Any, dict[str, Any]]],
    grp: str | list[str] | None,
) -> pl.DataFrame:
    """Candidate table: one row per detected change with assess metrics.

    Each row carries the change cohort value, optional group column(s), and
    the :func:`_assess_change` columns (``level_shift``, ``t_stat``,
    ``p_value``, ``delta_r2``, ``step_p``, ``curved_drift_suspect``,
    ``kind``, ``n_pre``, ``n_post``). This is the transparent table the
    evaluation layer scores; it is a *superset* of the accepted
    ``changes`` (which the FDR / evaluation filter narrows). Empty frame
    when no combo produced an assessed change point.
    """
    frames: list[pl.DataFrame] = []
    for combo, res in per_combo_results:
        assess = res.get("assess")
        change_points = res["change_points"]
        if not assess or not change_points:
            continue
        data: dict[str, Any] = {}
        fill_group_columns(data, grp, combo, len(change_points))
        data["change"] = change_points
        for key in _ASSESS_KEYS:
            data[key] = [a[key] for a in assess]
        frames.append(pl.DataFrame(data))
    if not frames:
        return pl.DataFrame()
    return pl.concat(frames, how="vertical_relaxed")


def _sweep_window_candidates(
    sub: pl.DataFrame,
    target: str,
    windows: Sequence[int],
    *,
    method: str,
    n_regimes: int | None,
    sig_level: float,
    n_permutations: int,
    min_size: int,
    seed: int | None,
    edge_scan: bool,
    edge_threshold: float,
) -> list[dict[str, Any]]:
    """Detect across each window in ``windows``, merge by change date.

    A change that recurs across many windows is robust; a window-specific
    artifact appears at one or two. Returns one row per distinct change
    cohort value with ``window_stability`` (fraction of usable windows that
    detected it), ``n_windows`` (how many), and the :func:`_assess_change`
    columns taken from the window with the most cohorts (most reliable
    estimate). Failed windows (too few cohorts for that window) are skipped
    and excluded from the stability denominator.
    """
    by_date: dict[Any, dict[str, Any]] = {}
    n_win = 0
    for w in windows:
        try:
            res = _detect_regime_single(
                sub,
                target=target,
                window=int(w),
                method=method,
                n_regimes=n_regimes,
                sig_level=sig_level,
                n_permutations=n_permutations,
                min_size=min_size,
                seed=seed,
                edge_scan=edge_scan,
                edge_threshold=edge_threshold,
                with_assess=True,
            )
        except ValueError:
            continue
        n_win += 1
        n_coh = len(res["cohorts"])
        for change_point, a in zip(res["change_points"], res["assess"], strict=False):
            rec = by_date.setdefault(change_point, {"count": 0, "best_n": -1, "assess": None})
            rec["count"] += 1
            if n_coh > rec["best_n"]:
                rec["best_n"] = n_coh
                rec["assess"] = a

    rows: list[dict[str, Any]] = []
    for change_point in sorted(by_date):
        rec = by_date[change_point]
        rows.append({
            "change": change_point,
            "window_stability": (rec["count"] / n_win) if n_win else float("nan"),
            "n_windows": int(rec["count"]),
            **rec["assess"],
        })
    return rows


_SWEEP_KEYS = ("change", "window_stability", "n_windows") + _ASSESS_KEYS


def _build_sweep_candidates_df(
    sub_by_combo: dict[Any, pl.DataFrame],
    combos: list[Any],
    grp: str | list[str] | None,
    *,
    target: str,
    windows: Sequence[int],
    method: str,
    n_regimes: int | None,
    sig_level: float,
    n_permutations: int,
    min_size: int,
    seed: int | None,
    edge_scan: bool,
    edge_threshold: float,
) -> pl.DataFrame:
    """Candidate table from a WINDOW sweep, with ``window_stability``.

    Per combo, :func:`_sweep_window_candidates` runs detection across
    ``windows`` and merges by change date; the rows are stacked with the
    group column(s) prepended. Carries ``window_stability`` / ``n_windows``
    on top of the assess columns. Empty frame when nothing is detected.
    """
    frames: list[pl.DataFrame] = []
    for combo in combos:
        rows = _sweep_window_candidates(
            sub_by_combo[combo],
            target,
            windows,
            method=method,
            n_regimes=n_regimes,
            sig_level=sig_level,
            n_permutations=n_permutations,
            min_size=min_size,
            seed=seed,
            edge_scan=edge_scan,
            edge_threshold=edge_threshold,
        )
        if not rows:
            continue
        data: dict[str, Any] = {}
        fill_group_columns(data, grp, combo, len(rows))
        for key in _SWEEP_KEYS:
            data[key] = [r[key] for r in rows]
        frames.append(pl.DataFrame(data))
    if not frames:
        return pl.DataFrame()
    return pl.concat(frames, how="vertical_relaxed")


_GRAIN_MONTHS = {"M": 1, "Q": 3, "H": 6, "Y": 12}


def _coarsen_triangle(tri, target_grain: str):
    """Re-aggregate a Triangle to a COARSER cohort grain (M -> Q -> H -> Y).

    Reconstructs the per-cell incremental experience (cohort, calendar,
    incr loss / premium) from the Triangle and rebuilds at ``target_grain``,
    reusing the Triangle constructor's binning so the coarser table is
    exactly a direct grain build. Returns the Triangle unchanged when
    ``target_grain`` equals the source grain. Raises if asked to REFINE
    (a coarser triangle cannot be un-aggregated to a finer one).
    """
    if target_grain not in _GRAIN_MONTHS:
        raise ValueError(f"unknown grain {target_grain!r}")
    src = tri.grain
    if target_grain == src:
        return tri
    if _GRAIN_MONTHS[target_grain] < _GRAIN_MONTHS[src]:
        raise ValueError(
            f"cannot refine grain {src!r} -> {target_grain!r} (coarsen only)"
        )
    mpp = _GRAIN_MONTHS[src]
    groups = normalize_groups(tri.groups)
    # calendar of cell (cohort, duration) = cohort + (duration - 1) source periods.
    recon = tri._df.select(
        *groups,
        pl.col("cohort"),
        pl.col("cohort")
        .dt.offset_by(pl.format("{}mo", (pl.col("duration") - 1) * mpp))
        .alias("_calendar"),
        pl.col("incr_loss"),
        pl.col("incr_premium"),
    )
    from ..core.triangle import Triangle

    return Triangle(
        recon,
        groups=tri.groups,
        cohort="cohort",
        calendar="_calendar",
        loss="incr_loss",
        premium="incr_premium",
        grain=target_grain,
        basis="incremental",
    )


def _grain_sweep_candidates(
    triangle,
    grains: Sequence[str],
    *,
    target: str,
    by: Any,
    method: str,
    n_regimes: int | None,
    sig_level: float,
    n_permutations: int,
    min_size: int,
    seed: int | None,
    window: Any,
    window_floor: int | None,
    fdr: bool,
    edge_scan: bool,
    edge_threshold: float,
    window_sweep: Sequence[int] | None,
) -> pl.DataFrame:
    """Candidate table from a COHORT-GRAIN sweep (M / Q / H / Y).

    For each grain the triangle is coarsened (:func:`_coarsen_triangle`)
    and detection re-run; per-grain candidates are tagged with ``grain``
    and merged by change date (floored to the coarsest swept grain), so a
    change detected at several grains is robust. ``grain_stability`` counts
    how many grains found it; the representative row is the finest-grain,
    most window-stable one. Coarser grains average cohort-level noise, so a
    noisy coverage can surface at Q / H even when invisible at M. Grains
    finer than the triangle's own grain are skipped (cannot un-aggregate).
    """
    frames: list[pl.DataFrame] = []
    seen: list[str] = []
    for g in grains:
        try:
            tri_g = _coarsen_triangle(triangle, g)
        except ValueError:
            continue
        try:
            reg_g = Regime._from_triangle(
                tri_g,
                target=target,
                window=window,
                by=by,
                method=method,
                n_regimes=n_regimes,
                sig_level=sig_level,
                n_permutations=n_permutations,
                min_size=min_size,
                seed=seed,
                window_floor=window_floor,
                fdr=fdr,
                edge_scan=edge_scan,
                edge_threshold=edge_threshold,
                window_sweep=window_sweep,
                grain_sweep=None,
            )
        except ValueError:
            continue
        cand = reg_g._candidates_df
        if cand.is_empty():
            continue
        frames.append(cand.with_columns(pl.lit(g).alias("grain")))
        seen.append(g)

    if not frames:
        return pl.DataFrame()

    allc = pl.concat(frames, how="vertical_relaxed")
    align_mpp = max(_GRAIN_MONTHS[g] for g in seen)
    group_cols = normalize_groups(triangle.groups)
    allc = allc.with_columns(
        pl.col("change").dt.truncate(f"{align_mpp}mo").alias("_key"),
        pl.col("grain").replace_strict(_GRAIN_MONTHS).alias("_gm"),
        # sort helper: most step-like first (NaN step_p -> last).
        pl.col("step_p").fill_nan(2.0).fill_null(2.0).alias("_sp"),
    )
    keys = group_cols + ["_key"]

    # Representative row = the grain with the STRONGEST step evidence
    # (lowest step_p), tie-broken by finest grain then most window-stable.
    # So a phased-in transition's coarse-grain step is the primary signal, while
    # `change_type` (below) carries the full per-grain profile.
    sort_cols, desc = ["_sp", "_gm"], [False, False]
    if "window_stability" in allc.columns:
        sort_cols.append("window_stability")
        desc.append(True)
    allc_sorted = allc.sort(sort_cols, descending=desc, nulls_last=True)
    value_cols = [
        c for c in allc_sorted.columns if c not in keys and c not in ("_gm", "_sp")
    ]
    agg = allc_sorted.group_by(keys, maintain_order=True).agg(
        pl.col("grain").n_unique().alias("grain_stability"),
        *[pl.col(c).first() for c in value_cols],
    )

    # Per-grain kind profile: the kind at each grain (null if that grain did
    # not place a change in this bucket). Surfaces "drift at M, step at H"
    # (a bounded transition) vs "drift at every grain" (genuine decline).
    kind_piv = allc.pivot(
        values="kind", index=keys, on="grain", aggregate_function="first"
    )
    present = [g for g in ("M", "Q", "H", "Y") if g in kind_piv.columns]
    kind_piv = kind_piv.rename({g: f"kind_{g}" for g in present})
    kcols = [f"kind_{g}" for g in present]
    out = agg.join(kind_piv.select(keys + kcols), on=keys, how="left")

    any_step = pl.any_horizontal([pl.col(c) == "step" for c in kcols])
    any_drift = pl.any_horizontal([pl.col(c) == "drift" for c in kcols])
    out = out.with_columns(
        pl.when(any_step & any_drift)
        .then(pl.lit("transition"))   # step at some grains, drift at others
        .when(any_step)
        .then(pl.lit("step"))
        .when(any_drift)
        .then(pl.lit("drift"))
        .otherwise(pl.lit("edge"))
        .alias("change_type")
    ).drop("_key")

    preferred = (
        group_cols
        + ["change", "grain", "change_type", "grain_stability"]
        + kcols
        + [
            "window_stability", "n_windows", "n_pre", "n_post", "level_shift",
            "t_stat", "p_value", "delta_r2", "step_p", "curved_drift_suspect",
            "kind",
        ]
    )
    order = [c for c in preferred if c in out.columns]
    order += [c for c in out.columns if c not in order]
    return out.select(order)


def _combine_combo_results(
    per_combo_results: list[tuple[Any, dict[str, Any]]],
    grp: str | list[str] | None,
) -> tuple[pl.DataFrame, pl.DataFrame, list[Any], int, list[Any] | dict[Any, list[Any]]]:
    """Stack per-combo (labels, changes) into one frame each.

    For pooled detection (``grp is None``) the schemas match the
    single-group case and ``dropped`` is a flat list. For per-combo
    detection the group column is *prepended* to both frames, and
    ``dropped`` becomes ``{combo_value: [cohort, ...]}`` -- a per-group
    mapping for multi-group input.
    """
    label_frames: list[pl.DataFrame] = []
    change_frames: list[pl.DataFrame] = []
    all_change_points: list[Any] = []
    n_regimes_max = 0
    dropped_out: list[Any] | dict[Any, list[Any]]

    if grp is None:
        # Pooled: single combo expected.
        _, res = per_combo_results[0]
        cohorts = res["cohorts"]
        regime_ids = res["regime_ids"]
        change_points = res["change_points"]

        label_frames.append(
            pl.DataFrame({"cohort": cohorts, "regime_id": regime_ids})
        )
        change_frames.append(
            pl.DataFrame(
                {
                    "change": change_points,
                    "regime_id": list(range(2, 2 + len(change_points))),
                },
                schema_overrides={"regime_id": pl.Int64},
            )
        )
        all_change_points.extend(change_points)
        n_regimes_max = max(n_regimes_max, res["n_regimes"])
        dropped_out = list(res["dropped"])
    else:
        dropped_dict: dict[Any, list[Any]] = {}
        for combo, res in per_combo_results:
            cohorts = res["cohorts"]
            regime_ids = res["regime_ids"]
            change_points = res["change_points"]

            # Group column(s) first (single col for a str grp, one per
            # name for a multi-column list grp with a tuple combo).
            lab_data: dict[str, Any] = {}
            fill_group_columns(lab_data, grp, combo, len(cohorts))
            lab_data["cohort"] = cohorts
            lab_data["regime_id"] = regime_ids
            label_frames.append(pl.DataFrame(lab_data))

            if change_points:
                ch_data: dict[str, Any] = {}
                fill_group_columns(ch_data, grp, combo, len(change_points))
                ch_data["change"] = change_points
                ch_data["regime_id"] = list(range(2, 2 + len(change_points)))
                change_frames.append(
                    pl.DataFrame(
                        ch_data,
                        schema_overrides={"regime_id": pl.Int64},
                    )
                )
                all_change_points.extend(change_points)
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
    return labels_df, changes_df, all_change_points, n_regimes_max, dropped_out


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

    Internal helper, intentionally not exported. Sweeps the candidate
    windows, then applies :func:`_kneedle_elbow` to the change-count
    curve. Returns the elbow window or ``None`` when no sweep value
    produced a usable detection (the caller falls back to
    ``_WINDOW_AUTO_FALLBACK``).

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
        sweep.append((int(k), len(res["change_points"])))

    if not sweep:
        return None

    windows = np.array([w for w, _ in sweep])
    counts = np.array([c for _, c in sweep])
    return _kneedle_elbow(windows, counts)


def _build_feature_matrix(
    tri_df: pl.DataFrame,
    target: str,
    window: int,
) -> tuple[np.ndarray, list, list]:
    """Pivot the long-format Triangle into a (n_cohorts, window) feature matrix.

    Cohorts with fewer than window observed durations are dropped
    and returned in the ``dropped`` list. Cohorts are returned ordered
    by cohort value.
    """
    if target not in tri_df.columns:
        raise ValueError(
            f"target={target!r} not found in Triangle columns: "
            f"{tri_df.columns}"
        )

    df = tri_df.filter(pl.col("duration") <= window)

    # Count *distinct duration values* with a non-null target per cohort.
    # When the input frame carries multiple rows per (cohort, duration)
    # (pooled detection on a multi-group triangle), raw row counts
    # would overstate eligibility -- the pivot aggregates to one cell
    # per (cohort, duration), so the eligibility test must do the same.
    counts = (
        df.filter(pl.col(target).is_not_null())
        .group_by("cohort")
        .agg(pl.col("duration").n_unique().alias("n"))
        .sort("cohort")
    )
    eligible = counts.filter(pl.col("n") >= window)["cohort"].to_list()
    eligible_set = set(eligible)
    # ``all_cohorts`` from the unfiltered frame: includes cohorts with
    # zero non-null target cells (which the filter above would drop).
    all_cohorts = (
        df["cohort"].unique().sort().to_list()
    )
    dropped = [c for c in all_cohorts if c not in eligible_set]

    if not eligible:
        raise ValueError(
            f"No cohorts have >= window={window} observed durations. "
            f"Reduce window."
        )

    # Pivot to wide form: rows = cohort, cols = duration 1..window. Use mean as
    # aggregator so pooled detection on a multi-group triangle (where
    # each (cohort, duration) cell has one row per group) collapses to a
    # single value per (cohort, duration), averaging over the groups while
    # ignoring missing cells.
    wide = (
        df.filter(pl.col("cohort").is_in(eligible))
        .pivot(on="duration", index="cohort", values=target, aggregate_function="mean")
        .sort("cohort")
    )

    cohorts = wide["cohort"].to_list()
    mat = wide.drop("cohort").to_numpy()

    if np.isnan(mat).any():
        raise ValueError(
            "Feature matrix contains NaN — reduce K or check input."
        )

    return mat, cohorts, dropped


def _cohort_level_scalar(
    tri_df: pl.DataFrame,
    target: str,
    window: int,
) -> tuple[list, np.ndarray]:
    """Reduce each cohort to one scalar level: mean ``target`` over duration 1..window.

    Parameters
    ----------
    tri_df
        Long-format Triangle frame with ``cohort`` (Date), ``duration`` (int) and
        the metric column named by ``target``.
    target
        Metric column to average (e.g. ``"ratio"``).
    window
        Number of leading durations that define the level. A cohort
        is kept only if it has at least ``window`` DISTINCT non-null ``duration``
        cells in ``1..window`` -- so the early trajectory is fully observed
        and the level is comparable across cohorts (no incomplete-duration
        bias).

    Returns
    -------
    (cohorts, scalar)
        ``cohorts`` is the sorted-ascending Python list of kept cohort keys
        (a list, matching :func:`_build_feature_matrix`, so the caller can
        ``.index(date)`` and build polars Date columns); ``scalar`` is the
        aligned float numpy array of per-cohort means. Empty list / array
        when no cohort qualifies.
    """
    if window < 1:
        raise ValueError(f"window must be >= 1, got {window!r}")
    for col in ("cohort", "duration", target):
        if col not in tri_df.columns:
            raise KeyError(f"tri_df missing required column {col!r}")

    agg = (
        tri_df.lazy()
        .select(["cohort", "duration", target])
        .filter(
            pl.col("duration").is_between(1, window)
            & pl.col(target).is_not_null()
            & pl.col(target).is_not_nan()
        )
        # distinct duration (not row count) so duplicate (cohort, duration) rows cannot
        # fake full early coverage.
        .group_by("cohort")
        .agg(
            pl.col("duration").n_unique().alias("_n_duration"),
            pl.col(target).mean().alias("_level"),
        )
        .filter(pl.col("_n_duration") >= window)
        .sort("cohort")
        .collect()
    )
    cohorts = agg.get_column("cohort").to_list()
    scalar = agg.get_column("_level").to_numpy().astype(float)
    return cohorts, scalar


def _fit_sse(y: np.ndarray, X: np.ndarray) -> float:
    """Residual sum of squares of OLS ``y ~ X`` (X includes the intercept).

    Single source of truth for both ``delta_r2`` and the step F-test, so the
    F numerator (``sse_trend - sse_step``) cannot drift negative from two
    independently-conditioned fits. ``rcond=None`` gives a least-norm
    solution on a rank-deficient design rather than raising.
    """
    beta, _, _, _ = np.linalg.lstsq(X, y, rcond=None)
    resid = y - X @ beta
    return float(resid @ resid)


def _assess_change(scalar: np.ndarray, change_idx: int) -> dict:
    """Quantify the change at ``change_idx`` in a cohort-level scalar series.

    Pure statistics kernel (no group / date handling -- the caller attaches
    the change date via ``cohorts[change_idx]`` and any group columns). The
    split is ``pre = scalar[:change_idx]`` / ``post = scalar[change_idx:]``;
    ``change_idx`` is the first cohort of the new regime.

    Returns a flat dict of NATIVE python scalars (so a list of these rows
    builds a clean polars ``candidates`` frame -- Int64 / Float64 / Utf8, no
    Object columns):

    - ``n_pre`` / ``n_post`` (int).
    - ``level_shift`` (float): magnitude, ``(mean_post - mean_pre)/abs(mean_pre)``;
      NaN if ``mean_pre == 0`` or a side is empty.
    - ``t_stat`` / ``p_value`` (float): Welch two-sample test, REPORTED ONLY.
      A trend induces serial correlation and a ramp split has different-mean
      halves, so the Welch test fires on *drift* too -- it must NOT gate the
      step/drift decision. It is an n-aware separation gauge (better than
      Cohen's d). NaN unless n>=2 on each side.
    - ``delta_r2`` (float): the extra R^2 a step adds over a linear trend,
      ``R2(1+idx+step) - R2(1+idx)``. REPORTED (an interior step is only
      ~0.25, an edge step ~0.7 -- the idx slope absorbs a monotone jump, so
      do not threshold this directly).
    - ``step_p`` (float): F-test p-value of the step coefficient over the
      trend -- THE step/drift gate.
    - ``curved_drift_suspect`` (bool): the F-test is calibrated against a
      *linear* null, so smooth curvature (e.g. exponential decay, no step)
      can read as a step. Set when a step located at a cohort extreme fits
      about as well as the detected change point. Report-only -- does not flip
      ``kind``; the trend basis stays linear.
    - ``kind`` (str): ``"edge"`` (a side < 2, or n < `_MIN_ASSESS_N`) /
      ``"step"`` (``step_p < _STEP_SIG``) / ``"drift"``.
    """
    y = np.asarray(scalar, dtype=float)
    n = y.size
    if not np.isfinite(y).all():
        raise ValueError("scalar must be finite (pass _cohort_level_scalar output)")
    if change_idx < 0 or change_idx > n:
        raise ValueError(f"change_idx {change_idx} out of range [0, {n}]")
    change_idx = int(change_idx)
    n_pre, n_post = change_idx, n - change_idx

    out: dict = {
        "n_pre": int(n_pre),
        "n_post": int(n_post),
        "level_shift": float("nan"),
        "t_stat": float("nan"),
        "p_value": float("nan"),
        "delta_r2": float("nan"),
        "step_p": float("nan"),
        "curved_drift_suspect": False,
        "kind": "edge",
    }

    pre, post = y[:change_idx], y[change_idx:]
    if n_pre >= 1 and n_post >= 1:
        mean_pre = float(pre.mean())
        if mean_pre != 0.0:
            out["level_shift"] = (float(post.mean()) - mean_pre) / abs(mean_pre)

    # Edge gate FIRST -- before any lstsq, so a degenerate (collinear) design
    # is never fitted. A side < 2, or fewer than `_MIN_ASSESS_N` cohorts (no
    # F-test dof; at n == 3 the step model saturates and delta_r2 is
    # information-free), is "edge" -- not enough evidence to separate a step
    # from a trend.
    if n_pre < 2 or n_post < 2 or n < _MIN_ASSESS_N:
        return out

    # Welch t -- reported only, never gates (see docstring).
    tt = ttest_ind(post, pre, equal_var=False)
    t_raw = float(tt.statistic)
    out["t_stat"] = abs(t_raw) if np.isfinite(t_raw) else float("nan")
    out["p_value"] = float(tt.pvalue) if np.isfinite(tt.pvalue) else float("nan")

    # Nested models, fitted ONCE, reused for delta_r2 AND the F-test.
    idx = np.arange(n, dtype=float)
    step = (idx >= change_idx).astype(float)
    ones = np.ones(n)
    sse_trend = _fit_sse(y, np.column_stack([ones, idx]))
    sse_step = _fit_sse(y, np.column_stack([ones, idx, step]))
    sst = float(((y - y.mean()) ** 2).sum())
    if sst > 0.0:
        out["delta_r2"] = max(0.0, (sse_trend - sse_step) / sst)

    # Relative tolerance: loss-ratio levels can be ~0.0x or ~1e6, so an
    # absolute eps is wrongly scaled.
    tol = 1e-12 * max(1.0, float(y @ y))
    if sse_step <= tol:
        # The step model fits to machine precision. Distinguish a perfect
        # STEP from a perfect TREND (e.g. an exact linear ramp), where the
        # trend ALSO fits perfectly and the step term adds nothing.
        if sse_trend <= tol:
            out["step_p"] = 1.0
            out["kind"] = "drift"
        else:
            out["step_p"] = 0.0
            out["kind"] = "step"
    else:
        ss_gain = max(0.0, sse_trend - sse_step)
        f_stat = (ss_gain / 1.0) / (sse_step / (n - 3))
        out["step_p"] = float(_f_dist.sf(f_stat, 1, n - 3))
        out["kind"] = "step" if out["step_p"] < _STEP_SIG else "drift"

    # Curvature guard (report-only): a step placed at a cohort extreme that
    # fits about as well as the detected change point signals absorbed curvature
    # rather than a genuine discontinuity.
    if out["kind"] == "step" and sse_step > tol:
        for c in (2, n - 2):
            if c == change_idx or not (2 <= c <= n - 2):
                continue
            sse_c = _fit_sse(y, np.column_stack([ones, idx, (idx >= c).astype(float)]))
            if sse_c <= sse_step * 1.05:
                out["curved_drift_suspect"] = True
                break

    return out


# Graft-safety / calendar-effect screen thresholds.
_SCREEN_SHAPE = 0.10      # shape_score >= -> "shape" (graft unsafe)
_SCREEN_TREND = 0.05      # |shape_trend| >= -> strong shape trend
_SCREEN_CAL = 2.0         # |calendar_score| (t-stat) >= -> "calendar" flag
_SCREEN_F_MEANINGFUL = 1.02  # only compare links where development f >= this
_SCREEN_MIN_OVERLAP = 2


def _graft_screen_group(
    loss_cum: np.ndarray,
    cohort_ranks: np.ndarray,
    seg_ids: np.ndarray,
) -> dict:
    """2-axis residual screen for graft safety + calendar effect.

    Decides whether the newest segment may safely graft late-development
    factors from an older donor, by comparing the segments' development on
    two axes.

    - Axis 1 (SHAPE): per-segment volume-weighted ATA ``f[s,k]``, then the
      relative factor difference ``|f[s+1,k]/f[s,k] - 1|`` over the adjacent
      segments' overlap. A pure level (rate) change leaves f scale-invariant
      so this is ~0 (graft safe); a shape change makes it large (unsafe).
      The overlap is restricted to links with ``f >= _SCREEN_F_MEANINGFUL``
      so the f~1 late-duration relative-difference explosion does not over-fire,
      and links are weighted by the min contributing cohort count.
    - Axis 2 (CALENDAR): residuals of cell increments vs the pooled ATA,
      regressed along the calendar diagonal after DEV-detrending (so a duration
      mis-specification cannot masquerade as a calendar trend).
      ``calendar_score`` is the t-statistic of that slope (slope / SE) --
      noise-aware, so a noisy coverage's spurious slope (large SE) deflates
      while a real diagonal trend stands out. A calendar effect inflates the
      donor's late-duration factors (measured on the shocked diagonal) that the
      young segment grafts but will not itself experience.

    ``loss_cum`` is the (n_cohorts, n_duration) CUMULATIVE loss matrix (NaN where
    undeveloped, column k = duration index k); ``cohort_ranks`` the calendar
    order; ``seg_ids`` the per-cohort segment id (0=oldest). Returns native
    scalars: ``shape_score``, ``shape_trend``, ``calendar_score`` (t-stat),
    ``n_overlap``, ``verdict`` in {insufficient, shape, calendar, level}.

    Caveat: the calendar t-stat treats cells as independent, but triangle
    residuals are autocorrelated along cohorts / diagonals, so it overstates
    significance somewhat -- hence the conservative threshold (|t| >= 2).
    """
    loss_cum = np.asarray(loss_cum, dtype=float)
    cohort_ranks = np.asarray(cohort_ranks)
    seg_ids = np.asarray(seg_ids)
    n_cohorts, n_duration = loss_cum.shape
    n_links = n_duration - 1
    nan = float("nan")

    seg_vals = np.sort(np.unique(seg_ids))
    n_seg = seg_vals.size

    # Axis 1 -- per-segment volume-weighted ATA + count + volume.
    f = np.full((n_seg, max(n_links, 0)), np.nan)
    cnt = np.zeros((n_seg, max(n_links, 0)))
    if n_links >= 1:
        for si, s in enumerate(seg_vals):
            rows = loss_cum[seg_ids == s]
            if rows.shape[0] == 0:
                continue
            base = rows[:, :-1]
            lead = rows[:, 1:]
            both = np.isfinite(base) & np.isfinite(lead)
            base_sum = np.where(both, base, 0.0).sum(axis=0)
            lead_sum = np.where(both, lead, 0.0).sum(axis=0)
            with np.errstate(divide="ignore", invalid="ignore"):
                f[si] = np.where(base_sum > 0, lead_sum / base_sum, np.nan)
            cnt[si] = both.sum(axis=0)

    shape_score = nan
    shape_trend = nan
    n_overlap_total = 0
    if n_seg >= 2 and n_links >= 1:
        pair_scores: list[float] = []
        ks_all: list[np.ndarray] = []
        rd_all: list[np.ndarray] = []
        w_all: list[np.ndarray] = []
        for si in range(n_seg - 1):
            fa, fb = f[si], f[si + 1]
            ok = (
                np.isfinite(fa) & np.isfinite(fb)
                & (fa > 0)
                & (fa >= _SCREEN_F_MEANINGFUL)
            )
            kidx = np.nonzero(ok)[0]
            if kidx.size == 0:
                continue
            reldiff = np.abs(fb[kidx] / fa[kidx] - 1.0)
            w = np.minimum(cnt[si, kidx], cnt[si + 1, kidx]).astype(float)
            if w.sum() <= 0:
                continue
            pair_scores.append(float(np.average(reldiff, weights=w)))
            n_overlap_total += int(kidx.size)
            ks_all.append(kidx.astype(float))
            rd_all.append(reldiff)
            w_all.append(w)
        if pair_scores:
            shape_score = float(np.max(pair_scores))
        if ks_all:
            ks = np.concatenate(ks_all)
            rd = np.concatenate(rd_all)
            ww = np.concatenate(w_all)
            if ks.size >= 2 and np.ptp(ks) > 0 and ww.sum() > 0:
                wm_k = np.average(ks, weights=ww)
                wm_r = np.average(rd, weights=ww)
                varx = np.sum(ww * (ks - wm_k) ** 2)
                if varx > 0:
                    shape_trend = float(
                        np.sum(ww * (ks - wm_k) * (rd - wm_r)) / varx
                    )

    # Axis 2 -- calendar-diagonal residual trend, duration-detrended.
    calendar_score = nan
    if n_links >= 1:
        base = loss_cum[:, :-1]
        lead = loss_cum[:, 1:]
        both = np.isfinite(base) & np.isfinite(lead)
        base_sum = np.where(both, base, 0.0).sum(axis=0)
        lead_sum = np.where(both, lead, 0.0).sum(axis=0)
        with np.errstate(divide="ignore", invalid="ignore"):
            f_pool = np.where(base_sum > 0, lead_sum / base_sum, np.nan)
        rows_idx, cols_idx = np.nonzero(both)
        if rows_idx.size:
            ci = loss_cum[rows_idx, cols_idx]
            ci1 = loss_cum[rows_idx, cols_idx + 1]
            fp = f_pool[cols_idx]
            exp_incr = (fp - 1.0) * ci
            # Restrict to MEANINGFUL development (f >= threshold): at f~1 the
            # expected increment ~ 0, so actual/expected - 1 explodes into
            # noise -- the same late-duration trap guarded on the shape axis.
            good = (
                np.isfinite(exp_incr)
                & np.isfinite(fp)
                & (np.abs(exp_incr) > 0)
                & (fp >= _SCREEN_F_MEANINGFUL)
            )
            if good.sum() >= 3:
                r = (ci1[good] - ci[good]) / exp_incr[good] - 1.0
                k_cell = cols_idx[good].astype(float)
                cal = cohort_ranks[rows_idx[good]].astype(float) + (k_cell + 1.0)
                # Frisch-Waugh-Lovell: partial duration (k) out of both r and cal.
                x = np.column_stack([np.ones_like(k_cell), k_cell])
                beta_r, *_ = np.linalg.lstsq(x, r, rcond=None)
                beta_c, *_ = np.linalg.lstsq(x, cal, rcond=None)
                r_perp = r - x @ beta_r
                cal_perp = cal - x @ beta_c
                vc = float(np.sum(cal_perp ** 2))
                n_cells = r.size
                if vc > 0 and n_cells > 3:
                    slope = float(np.sum(cal_perp * r_perp) / vc)
                    # calendar_score = the t-statistic of the (duration-detrended)
                    # calendar slope: slope / SE, NOT slope x span. Using the
                    # standard error makes it noise-aware -- a noisy coverage's
                    # spurious slope has a large SE and deflates, while a real
                    # diagonal trend stands out. (Same SE-vs-raw-magnitude fix
                    # as the change kernel's t-test; slope x span over-fired
                    # and even ranked noisy coverages above real ones.)
                    resid = r_perp - slope * cal_perp
                    sse = float(np.sum(resid ** 2))
                    dof = n_cells - 3  # intercept + duration + calendar
                    if sse > 0 and dof > 0:
                        se = float(np.sqrt((sse / dof) / vc))
                        if se > 0:
                            calendar_score = slope / se

    # SHAPE axis (scale-invariant) gates first; then the CALENDAR axis as a
    # noise-aware t-statistic (slope / SE) -- a calendar effect contaminates
    # the donor's late-duration factors (measured on the shocked diagonal) that
    # the young segment grafts but will not itself experience, a risk the
    # shape axis structurally cannot see (the young segment has no late-duration
    # to compare).
    if n_seg < 2 or not np.isfinite(shape_score) or n_overlap_total < _SCREEN_MIN_OVERLAP:
        verdict = "insufficient"
    elif shape_score >= _SCREEN_SHAPE or (
        np.isfinite(shape_trend) and abs(shape_trend) >= _SCREEN_TREND
    ):
        verdict = "shape"
    elif np.isfinite(calendar_score) and abs(calendar_score) >= _SCREEN_CAL:
        verdict = "calendar"
    else:
        verdict = "level"

    return {
        "shape_score": float(shape_score),
        "shape_trend": float(shape_trend),
        "calendar_score": float(calendar_score),
        "n_overlap": int(n_overlap_total),
        "verdict": verdict,
    }


def _e_divisive_change_points(
    mat: np.ndarray,
    sig_level: float,
    n_permutations: int,
    min_size: int,
    seed: int | None,
) -> tuple[list[int], list[float]]:
    """E-Divisive change_points (right-side starts) and their permutation p-values."""
    res = e_divisive(
        mat,
        sig_level=sig_level,
        n_permutations=n_permutations,
        min_size=min_size,
        alpha=1.0,
        seed=seed,
    )
    return res.change_points, res.p_values


def _bh_adjust(p_values: np.ndarray, n_tests: int | None = None) -> np.ndarray:
    """Benjamini-Hochberg adjusted p-values (FDR).

    Standard step-up procedure: sort ascending, scale the k-th smallest by
    ``m / rank``, then enforce monotonicity from the largest down. Returns
    adjusted p-values aligned with the *input* order. ``NaN`` inputs (e.g.
    edge-scan change points, which carry no permutation p-value) are passed
    through untouched and excluded.

    ``n_tests`` overrides the multiplicity denominator ``m``. The relevant
    multiplicity is the number of *coverages tested* (one first-change test
    each), not the number of change points that happened to fire -- a coverage
    that detected nothing still consumed a test. Pass the combo count so a lone
    borderline change point across many coverages is correctly deflated. Defaults
    to the number of finite p-values when not given.
    """
    p = np.asarray(p_values, dtype=np.float64)
    finite = np.isfinite(p)
    out = p.copy()
    idx = np.where(finite)[0]
    if idx.size == 0:
        return out
    m = idx.size if n_tests is None else max(int(n_tests), idx.size)
    order = idx[np.argsort(p[idx])]
    ranks = np.arange(1, order.size + 1)
    scaled = p[order] * m / ranks
    # enforce monotone non-decreasing from the top, then clip to 1
    adj = np.minimum.accumulate(scaled[::-1])[::-1]
    out[order] = np.minimum(adj, 1.0)
    return out


def _edge_scan_change_points(
    mat: np.ndarray,
    threshold: float,
    min_size: int,
) -> list[int]:
    """Detect a boundary regime that E-Divisive structurally cannot.

    E-Divisive's energy statistic needs >= 2 cohorts on each side of a split
    (and the regime detector forbids splits within ``min_size`` of an edge),
    so a regime occupying only the first / last 1..``min_size``-1 cohorts is
    unseparable -- a real failure when those cohorts are clearly different
    (e.g. a coverage whose oldest single cohort sits at a different loss
    level). This is a *1-vs-rest outlier* problem, not a two-sample
    distributional one: a length-1 block is well-defined against the
    *distribution* of the remaining cohorts.

    For each edge, grow a contiguous block ``k = 1, 2, ...`` (capped at the
    e-divisive blind zone ``< min_size``) while the block's mean is far from
    the robust centre of the *remaining* cohorts, measured in robust-z
    (RMS over duration features, MAD scale). A block survives only while every
    extension stays above ``threshold`` (contiguity); the scan stops at the
    first cohort that is not an outlier, so an isolated interior spike is not
    flagged. ``threshold`` is an effect size in noise units -- a clearly
    different edge cohort scores far above it, while a noisy coverage (large
    within-rest scatter inflates the denominator) keeps every block below it,
    so the test is robust to volatility by construction.

    Returns edge change indices (right-side starts): ``[k]`` for a left-edge
    regime of size ``k``, ``[n - k]`` for a right-edge regime of size ``k``.
    """
    n = mat.shape[0]
    max_edge = max(0, min(min_size - 1, n - 2))
    if max_edge < 1:
        return []

    # Per-cohort robust-z: each cohort's RMS standardized distance to the
    # robust centre (median / MAD over cohorts) per duration feature. Median /
    # MAD are robust to the few edge outliers, so a clearly different edge
    # cohort gets a large z while a noisy coverage (large MAD) keeps every
    # cohort modest. Constant duration features (MAD = 0) are ignored.
    med = np.median(mat, axis=0)
    mad = np.median(np.abs(mat - med), axis=0) * 1.4826
    scale = np.where(mad > 0, mad, np.nan)
    with np.errstate(invalid="ignore"):
        zmat = (mat - med) / scale
        z = np.sqrt(np.nanmean(zmat**2, axis=1))

    change_idxs: list[int] = []
    # left edge: leading run of individually-outlier cohorts (contiguity --
    # an isolated interior spike is not an edge regime), capped at the
    # e-divisive blind zone (< min_size).
    k = 0
    while k < max_edge and np.isfinite(z[k]) and z[k] >= threshold:
        k += 1
    if k >= 1:
        change_idxs.append(k)
    # right edge: trailing run
    j = 0
    while j < max_edge and np.isfinite(z[n - 1 - j]) and z[n - 1 - j] >= threshold:
        j += 1
    if j >= 1:
        change_idxs.append(n - j)
    return sorted(set(change_idxs))


def _hclust_change_points(
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


def _regime_ids_from_changes(n: int, change_idxs: list[int]) -> np.ndarray:
    """Assign 1-based regime ids in cohort order.

    Cohort ``i`` gets ``1 + (number of change indices <= i)`` -- a
    right-side searchsorted over the sorted unique change indices.
    """
    sorted_changes = np.array(sorted(set(change_idxs)), dtype=np.int64)
    return (1 + np.searchsorted(sorted_changes, np.arange(n), side="right")).astype(
        np.int64
    )


class Regime:
    """Detected cohort regime structure.

    Use :meth:`RegimeDetector.detect` (auto) or ``Regime(change=...)`` (manual)
    to construct.

    Attributes
    ----------
    method : str
        ``"e_divisive"`` or ``"hclust"``.
    target : str
        Trajectory variable name used.
    window : int
        Duration window length (number of duration cells per cohort
        used as the feature vector).
    cohort : str
        Original cohort variable name (e.g. ``"uy_m"``).
    change_points : list
        Cohort values at which a new regime starts (excluding the first
        cohort).
    n_regimes : int
        Number of regimes detected.
    dropped : list
        Cohorts excluded because they had fewer than ``window`` observed
        durations.

    How a fit CONSUMES a regime (latest_only / segment_wise / covariate) is the
    ``treatment`` knob on the ESTIMATOR (e.g. ``CredibleLoss(regime=reg,
    treatment="segment_wise")``), not a property of this structure.
    """

    # Instance attributes are set in `_from_triangle` (detection) / `__init__`
    # (manual, via `_set_manual_attrs`); declared here so the type is visible.
    _labels_df: pl.DataFrame
    _changes_df: pl.DataFrame
    _candidates_df: pl.DataFrame
    _output_type: str
    method: str
    target: str
    window: int | list[int]
    cohort: str
    duration: str
    groups: str | list[str] | None
    change_points: list[Any]
    n_regimes: int
    dropped: list[Any] | dict[Any, list[Any]]

    def __init__(
        self,
        change: Any,
        *,
        groups: Mapping[str, Sequence[Any]] | None = None,
    ) -> None:
        """Build a Regime by hand from explicit, user-supplied change points.

        Use this when you already know where the cohort regime shifts (e.g. a
        policy revision date). Contrast with ``RegimeDetector(...).detect(tri)``,
        which infers the change points from data. How the regime is CONSUMED
        (latest_only / segment_wise / covariate) is the estimator's
        ``treatment`` knob, not an argument here.

        Parameters
        ----------
        change
            Cohort date(s) where a new regime starts: a single value
            (``"YYYY-MM-DD"`` / ``date`` / ``datetime``) or a sequence.
        groups
            Optional ``{column_name: [values]}`` mapping aligned 1:1 with
            ``change``, when different groups carry different change points.

        Examples
        --------
        >>> Regime(change="2024-07-01")
        >>> Regime(
        ...     change=["2024-07-01", "2024-10-01"],
        ...     groups={"coverage": ["SURGERY", "CI"]},
        ... )
        """
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

        gmap = dict(groups) if groups else {}
        for col, vals in gmap.items():
            if not isinstance(vals, Sequence) or isinstance(vals, str):
                vals = [vals]
                gmap[col] = vals
            if len(vals) != n:
                raise ValueError(
                    f"All arguments must have equal length; "
                    f"`change`={n} but `groups[{col!r}]`={len(vals)}"
                )

        columns: dict[str, Any] = dict(gmap)
        columns["change"] = parsed
        columns["regime_id"] = [2] * n
        changes_df = pl.DataFrame(
            columns,
            schema_overrides={"regime_id": pl.Int64, "change": pl.Date},
        )
        group_spec = collapse_groups(list(gmap.keys())) if gmap else None
        self._set_manual_attrs(changes_df, group_spec)

    def _set_manual_attrs(
        self, changes_df: pl.DataFrame, groups: str | list[str] | None
    ) -> None:
        """Populate the instance attributes of a hand-built (manual) regime.

        Shared by ``__init__`` (user construction) and ``_manual`` (the
        internal derived-cut builder). ``changes_df`` carries one row per change
        point with at least a ``change`` (Date) column plus the group column if
        any. A manual regime carries no detection candidates.
        """
        self._changes_df = changes_df
        self._labels_df = pl.DataFrame(
            {"cohort": [], "regime_id": []},
            schema={"cohort": pl.Date, "regime_id": pl.Int64},
        )
        self._candidates_df = pl.DataFrame()
        self._output_type = "polars"
        self.method = "manual"
        self.target = ""
        self.window = 0
        self.cohort = ""
        self.duration = ""
        self.groups = groups
        self.change_points = changes_df["change"].to_list()
        self.n_regimes = 0
        self.dropped = []

    @classmethod
    def _from_triangle(
        cls,
        triangle: Triangle,
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
        window_floor: int | None = None,
        fdr: bool = False,
        edge_scan: bool = False,
        edge_threshold: float = 10.0,
        window_sweep: Sequence[int] | None = None,
        grain_sweep: Sequence[str] | None = None,
    ) -> Regime:
        if method not in _VALID_METHODS:
            raise ValueError(
                f"method must be one of {_VALID_METHODS}, got {method!r}"
            )

        # `premium_intensity` is an alias of `premium_ata` (constant offset of 1
        # absorbed by PCA standardisation -- detection produces identical
        # changes). Resolve before validation.
        if target == "premium_intensity":
            target = "premium_ata"

        # Preserve the user-facing target name (before the derive step
        # rewrites `target` to a derived column) so a grain sweep can re-run
        # detection on each coarsened triangle with the same target.
        user_target = target

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
        # unchanged. Keys from `iter_group_frames` match the `combos` entries
        # (scalar for a str group, tuple for multi-column).
        if grp is None:
            sub_by_combo: dict[Any, pl.DataFrame] = {None: tri_df}
        else:
            sub_by_combo = dict(iter_group_frames(tri_df, grp))

        # Resolve trajectory window per combo. ``window="auto"`` first
        # tries the ATA factor-stability point (the duration where the
        # age-to-age factors become CV/RSE-stable) for each combo. When
        # that is unavailable (pooled detection, degenerate input, or no
        # sustained stable run) it falls back to the Kneedle elbow on the
        # change-count sweep; if the elbow is also undefined, falls
        # back to ``_WINDOW_AUTO_FALLBACK``.
        if window_is_auto:
            from .._kernels.recursion import build_value_matrix
            from ..core.ata import _detect_stability_point

            # Map the regime target -> a valid cumulative metric for the
            # ATA factor diagnostic (cumulative metrics only). Derived
            # targets fall back to "ratio".
            _STABILITY_METRIC_MAP = {
                "ratio": "ratio",
                "loss": "loss",
                "premium": "premium",
                "incr_ratio": "ratio",
                "incr_loss": "loss",
                "incr_premium": "premium",
            }
            stability_metric = _STABILITY_METRIC_MAP.get(target, "ratio")

            # Per-combo factor-stability point. Pooled detection (grp is
            # None) cannot tie a stability point back to combos, so skip.
            stability_by_combo: dict[Any, int | None] = {}
            if grp is not None:
                for combo in combos:
                    try:
                        stab_obs, _, _ = build_value_matrix(
                            sub_by_combo[combo], value_col=stability_metric
                        )
                        stability_by_combo[combo] = _detect_stability_point(
                            stab_obs
                        )
                    except ValueError:
                        # Degenerate input (no valid links, single-cohort, ...):
                        # the elbow fallback handles it. A KeyError (missing
                        # stability_metric column) or RuntimeError is a genuine
                        # bug and is left to propagate.
                        stability_by_combo[combo] = None

            window_per_combo: list[int] = []
            for combo in combos:
                # Factor-stability path (per-combo when grp is set).
                stability_k = stability_by_combo.get(combo) if grp is not None else None
                if stability_k is not None and stability_k >= 2:
                    window_per_combo.append(stability_k)
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

        # Window floor (auto path only): the Kneedle elbow can resolve to a
        # degenerate window=2 on the pooled / stability-unavailable path,
        # where a length-2 cohort feature vector gives E-Divisive no power.
        # Clamping the auto-resolved window up to a floor restores detection
        # without touching the stability-anchored windows. Explicit `window`
        # is the user's choice and is left untouched.
        if window_is_auto and window_floor is not None:
            window_per_combo = [max(w, int(window_floor)) for w in window_per_combo]

        # Run detection per combo. Failures are skipped (per-combo
        # robustness: a single short coverage shouldn't kill the whole
        # multi-group call). Skipped combos contribute zero rows to
        # `changes`/`labels`.
        per_combo_results: list[tuple[Any, dict[str, Any]]] = []
        for combo, k in zip(combos, window_per_combo, strict=False):
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
                    edge_scan=edge_scan,
                    edge_threshold=edge_threshold,
                    with_assess=True,
                )
            except ValueError:
                continue
            per_combo_results.append((combo, res))

        if not per_combo_results:
            raise ValueError(
                "No group / combo produced a usable detection result. "
                "Check `window`, `min_size`, and input coverage."
            )

        # Candidate table (all detected change points + assess metrics) -- built
        # BEFORE the FDR filter, so it is a superset of the accepted
        # `changes`. The evaluation layer scores this transparent table.
        # With `window_sweep`, candidates come from a window sweep carrying
        # `window_stability` (a change that recurs across windows is robust);
        # otherwise from the single resolved window.
        if window_sweep is not None:
            candidates_df = _build_sweep_candidates_df(
                sub_by_combo, combos, grp,
                target=target,
                windows=window_sweep,
                method=method,
                n_regimes=n_regimes,
                sig_level=sig_level,
                n_permutations=n_permutations,
                min_size=min_size,
                seed=seed,
                edge_scan=edge_scan,
                edge_threshold=edge_threshold,
            )
        else:
            candidates_df = _build_candidates_df(per_combo_results, grp)

        # With `grain_sweep`, candidates instead come from a cohort-grain
        # sweep (coarsen + re-detect per grain, merge by change), adding
        # `grain` / `grain_stability` -- a noisy coverage can surface at a
        # coarser grain it is invisible at finer.
        if grain_sweep is not None:
            candidates_df = _grain_sweep_candidates(
                triangle, grain_sweep,
                target=user_target,
                by=by,
                method=method,
                n_regimes=n_regimes,
                sig_level=sig_level,
                n_permutations=n_permutations,
                min_size=min_size,
                seed=seed,
                window=window,
                window_floor=window_floor,
                fdr=fdr,
                edge_scan=edge_scan,
                edge_threshold=edge_threshold,
                window_sweep=window_sweep,
            )

        # FDR: Benjamini-Hochberg across EVERY permutation change point in the
        # whole multi-combo call. With ~60 coverages each tested at sig_level,
        # ~3 spurious first change points are expected by chance; FDR controls
        # that family-wise. Edge-scan change points carry NaN (effect-size gated, not
        # permutation tested) and are exempt. E-Divisive only.
        if fdr and method == "e_divisive":
            flat_p: list[float] = []
            refs: list[tuple[int, int]] = []
            for ci, (_, res) in enumerate(per_combo_results):
                for bi, p in enumerate(res["p_values"]):
                    flat_p.append(p)
                    refs.append((ci, bi))
            adj = _bh_adjust(
                np.asarray(flat_p, dtype=np.float64),
                n_tests=len(per_combo_results),
            )
            keep: dict[int, set[int]] = {
                ci: set() for ci in range(len(per_combo_results))
            }
            for (ci, bi), a, p0 in zip(refs, adj, flat_p, strict=False):
                if not np.isfinite(p0) or a <= sig_level:
                    keep[ci].add(bi)
            rebuilt: list[tuple[Any, dict[str, Any]]] = []
            for ci, (combo, res) in enumerate(per_combo_results):
                kept = sorted(keep[ci])
                if len(kept) == len(res["change_points"]):
                    rebuilt.append((combo, res))
                    continue
                cohorts = res["cohorts"]
                n = len(cohorts)
                change_points = [res["change_points"][bi] for bi in kept]
                pvs = [res["p_values"][bi] for bi in kept]
                idx = [cohorts.index(v) for v in change_points]
                regime_ids = _regime_ids_from_changes(n, idx)
                rebuilt.append((combo, {
                    **res,
                    "change_points": change_points,
                    "p_values": pvs,
                    "regime_ids": regime_ids,
                    "n_regimes": int(regime_ids.max()) if n > 0 else 0,
                }))
            per_combo_results = rebuilt

        labels_df, changes_df, change_points, n_regimes_total, dropped = (
            _combine_combo_results(per_combo_results, grp)
        )

        self = cls.__new__(cls)
        self._labels_df = labels_df
        self._changes_df = changes_df
        self._candidates_df = candidates_df
        self._output_type = triangle._output_type
        self.method = method
        self.target = target
        # ``window`` is scalar for single-combo, list[int] for multi-combo
        # (a single resolved window is unwrapped from the list).
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
        self.duration = triangle.duration
        self.groups = grp
        self.change_points = change_points
        self.n_regimes = n_regimes_total
        self.dropped = dropped
        return self

    @classmethod
    def _manual(
        cls,
        *,
        changes_df: pl.DataFrame,
        groups: str | list[str] | None,
    ) -> Regime:
        """Construct a manual Regime directly from a prepared ``changes_df``.

        ``__init__`` is the public manual constructor (it parses user
        ``change`` input); this classmethod is the internal builder used where a
        ``changes_df`` is already assembled (e.g. materialising a resolved cut),
        bypassing the parse step.
        """
        self = cls.__new__(cls)
        self._set_manual_attrs(changes_df, groups)
        return self

    @property
    def changes(self):
        """Detected (or manually specified) change points as a frame."""
        return mirror_output(self._changes_df, self._output_type)

    def changes_at(self, grain: str):
        """Change points with each ``change`` date snapped to ``grain``.

        For drawing the regime line on a chart aggregated to a coarser
        grain. A change that falls mid-period (e.g. ``2024-09-01`` viewed at
        quarterly grain) is snapped UP to the start of the next period
        (``2024-10-01``) -- the first period lying entirely after the
        change, which is exactly where the new regime segment begins once
        cohorts are floored to that grain. A change already on a period
        boundary is unchanged. The snap is ``ceil`` (next period start), not
        nearest, so the line always matches the grain-floored segmentation;
        a "nearest" snap would mis-place a change in the first half of a
        period.

        ``grain`` is one of ``"M"`` / ``"Q"`` / ``"H"`` / ``"Y"``. The frame
        is otherwise identical to :attr:`changes` (same columns, group keys,
        ``regime_id``); only ``change`` moves. Input mirroring is preserved.
        """
        from .._kernels.period import floor_to_period

        if grain not in _GRAIN_MONTHS:
            raise ValueError(
                f"`grain` must be one of {tuple(_GRAIN_MONTHS)}; got {grain!r}."
            )
        floored = floor_to_period(pl.col("change"), grain)
        snapped = (
            pl.when(floored < pl.col("change"))
            .then(floored.dt.offset_by(f"{_GRAIN_MONTHS[grain]}mo"))
            .otherwise(floored)
            .alias("change")
        )
        out = self._changes_df.with_columns(snapped)
        return mirror_output(out, self._output_type)

    @property
    def candidates(self):
        """All detected candidate changes with their assessment metrics.

        One row per detected change point (before the FDR / evaluation filter --
        a *superset* of :attr:`changes`), carrying the change cohort value,
        any group column(s), and the :func:`_assess_change` columns:
        ``level_shift`` (magnitude), ``t_stat`` / ``p_value`` (Welch,
        report-only), ``delta_r2``, ``step_p`` (the step-vs-drift F-test
        gate), ``curved_drift_suspect``, ``kind`` (``edge`` / ``step`` /
        ``drift``), ``n_pre`` / ``n_post``. Empty for a hand-built
        (:meth:`at`) regime. This is the transparent table an evaluation
        rule scores to decide which candidates become accepted regimes.
        """
        return mirror_output(self._candidates_df, self._output_type)

    @property
    def df(self):
        """Per-cohort regime labels in the original input format."""
        return mirror_output(self._labels_df, self._output_type)

    def evaluate(
        self,
        *,
        material: float = 0.05,
        sig: float = 0.05,
        min_stability: float = 0.5,
        rule: Callable[[dict], tuple[bool, float]] | None = None,
    ):
        """Score :attr:`candidates` into an ``action`` + ``confidence``.

        Returns the candidate table annotated with an ``action``, a
        ``still_moving`` flag, a ``confidence`` in ``[0, 1]``, and a
        convenience ``accept`` boolean (``action == "regime"``), sorted by
        confidence.

        - ``"regime"`` -- a material (``abs(level_shift) >= material``),
          significant (``step_p < sig`` OR Welch ``p_value < sig``) change.
          Fit with a regime cut at the change. A discrete step AND a gradual
          drift both cut here: cutting a decline at the detected change and
          projecting the recent segment is safer than extrapolating the
          trend, and avoids the ill-posed "how many recent" question.
        - ``"none"`` -- immaterial / edge / unsupported; leave alone.

        ``still_moving`` is set for a drift regime (``change_type == "drift"``,
        or ``kind`` when no grain profile): the recent segment is itself
        still trending, so the projection uses the recent level but its
        FORWARD path is a scenario, not a settled level.

        Confidence blends significance (0.45 -- the step F-test OR the Welch
        mean-diff, whichever is stronger), stability (0.30) and magnitude
        (0.25); it is 0 for ``"none"``. Stability does NOT gate the action
        (a real material decline seen at only one config must still be
        acted on) -- it only scales confidence.

        Parameters
        ----------
        material
            Minimum absolute relative ``level_shift`` to be material.
        sig
            Significance level (step F-test ``step_p`` or Welch ``p_value``).
        min_stability
            Reserved for stability-based confidence shaping; currently
            unused in the gate (stability feeds confidence, not the action).
        rule
            Optional ``rule(row_dict) -> (accept, confidence)`` callable to
            override the default scoring per candidate row.
        """
        cand = self._candidates_df
        if cand.is_empty():
            return mirror_output(cand, self._output_type)

        if rule is not None:
            accepts, confs = [], []
            for row in cand.iter_rows(named=True):
                a, c = rule(row)
                accepts.append(bool(a))
                confs.append(float(c))
            scored = cand.with_columns(
                pl.Series("accept", accepts),
                pl.Series("confidence", confs),
            ).with_columns(
                pl.when(pl.col("accept"))
                .then(pl.lit("regime"))
                .otherwise(pl.lit("none"))
                .alias("action"),
                pl.lit(False).alias("still_moving"),
            )
            return mirror_output(
                scored.sort("confidence", descending=True, nulls_last=True),
                self._output_type,
            )

        has_ws = "window_stability" in cand.columns
        has_gs = "grain_stability" in cand.columns
        absshift = pl.col("level_shift").abs()
        # significance from the step F-test (discrete break) OR the Welch
        # mean-diff (a gradual but real level change). max so a material
        # drift, which the step test calls insignificant, still scores on
        # its mean-difference evidence.
        sig_step = (1.0 - pl.col("step_p") / sig).clip(0.0, 1.0).fill_nan(0.0)
        sig_diff = (1.0 - pl.col("p_value") / sig).clip(0.0, 1.0).fill_nan(0.0)
        sig_score = pl.max_horizontal(sig_step, sig_diff)
        mag_score = (absshift / (2.0 * material)).clip(0.0, 1.0)

        # grain_stability in [1, n] -> [0, 1] via (gs - 1)/2 (3-grain full).
        gs_score = ((pl.col("grain_stability") - 1) / 2.0).clip(0.0, 1.0)
        if has_ws and has_gs:
            stab = 0.5 * pl.col("window_stability").fill_null(0.0) + 0.5 * gs_score
        elif has_ws:
            stab = pl.col("window_stability").fill_null(0.0)
        elif has_gs:
            stab = gs_score
        else:
            stab = pl.lit(0.5)  # single-config detect: no stability evidence

        material_move = absshift >= material
        diff_sig = (pl.col("p_value") < sig).fill_null(False) | (
            pl.col("step_p") < sig
        ).fill_null(False)
        # A material, significant change is cut as a regime -- step or drift
        # alike. Cutting a gradual decline at the detected change and
        # projecting the recent segment is safer than extrapolating the
        # trend, and sidesteps the ill-posed "how many recent" question.
        regime = material_move & diff_sig
        type_col = "change_type" if "change_type" in cand.columns else "kind"
        # `still_moving` flags a drift regime whose recent segment is itself
        # still trending, so its FORWARD path is a scenario (project the
        # recent level, but do not assume it has settled).
        still_moving = regime & (pl.col(type_col) == "drift")

        # Stability no longer gates the action (a real material decline
        # detected at one config -- e.g. a noisy coverage seen only at a
        # coarse grain -- must still be acted on); it feeds confidence.
        conf = (
            pl.when(regime)
            .then(0.45 * sig_score + 0.30 * stab + 0.25 * mag_score)
            .otherwise(0.0)
        )

        scored = cand.with_columns(
            pl.when(regime).then(pl.lit("regime")).otherwise(pl.lit("none")).alias("action"),
            still_moving.alias("still_moving"),
            conf.alias("confidence"),
            regime.alias("accept"),
        )
        return mirror_output(
            scored.sort("confidence", descending=True, nulls_last=True),
            self._output_type,
        )

    def accepted(self, **evaluate_kwargs) -> Regime:
        """Return a Regime of the accepted changes, ready to drive a fit.

        Runs :meth:`evaluate`, keeps the ``action == "regime"`` candidates,
        and wraps them as a manual Regime -- so the change points the
        evidence supports can be passed straight to an estimator's ``regime``
        slot. This closes the loop: detection and the candidate table are
        inspected, then the SAME accepted cuts drive the actual loss-ratio
        projection (the fit cuts the original triangle at each change and
        projects the recent segment), rather than stopping at a table.
        ``**evaluate_kwargs`` forward to :meth:`evaluate`
        (``material``, ``sig``, ``rule``).

        Examples
        --------
        >>> reg = lr.RegimeDetector(
        ...     grain_sweep=["M", "Q", "H"], window_sweep=range(4, 10)
        ... ).detect(tri)
        >>> fit = lr.CredibleLoss(regime=reg.accepted()).fit(tri)
        """
        ev = self.evaluate(**evaluate_kwargs)
        ev_pl = ev if isinstance(ev, pl.DataFrame) else pl.from_pandas(ev)
        group_cols = normalize_groups(self.groups)
        if "action" in ev_pl.columns:
            acc = ev_pl.filter(pl.col("action") == "regime").select(
                [*group_cols, "change"]
            )
        else:
            # No candidates surfaced (empty evaluate frame) -> no accepted
            # changes. Return a properly-typed empty `changes_df` so the
            # resulting Regime simply applies no filter, rather than crashing
            # on the missing "action" column.
            acc = pl.DataFrame(
                schema={**{c: pl.Utf8 for c in group_cols}, "change": pl.Date}
            )
        return Regime._manual(
            changes_df=acc,
            groups=self.groups,
        )

    def graft_screen(self, triangle, target: str = "loss"):
        """Screen each group's graft safety + calendar effect (2-axis).

        For every group, splits the cumulative ``target`` triangle into
        segments at this regime's change points and runs
        :func:`_graft_screen_group`, returning one row per group with:

        - ``verdict``: ``"level"`` (development shape unchanged AND no
          calendar trend -> grafting the donor's late-duration tail is SAFE),
          ``"shape"`` (the development pattern changed -> grafting UNSAFE,
          widen the band), ``"calendar"`` (a calendar-year effect
          contaminates the donor's late-duration factors -> grafting risky), or
          ``"insufficient"`` (too few segments / overlap to judge).
        - ``shape_score`` / ``shape_trend`` (Axis 1, segment-to-segment ATA
          discrepancy) and ``calendar_score`` (Axis 2, the t-statistic of the
          duration-detrended calendar-diagonal residual slope).

        A transparent guard for the graft path: graft where the
        verdict is ``"level"``; treat ``"shape"`` / ``"calendar"`` with
        caution.
        """
        tri_df = triangle.to_polars()
        if target not in tri_df.columns:
            raise ValueError(f"target {target!r} not in triangle columns")
        group_cols = normalize_groups(self.groups)
        changes = self._changes_df

        if group_cols:
            group_items = list(
                tri_df.partition_by(group_cols, as_dict=True).items()
            )
        else:
            group_items = [((), tri_df)]

        rows: list[dict] = []
        for key, gdf in group_items:
            keyvals = key if isinstance(key, tuple) else (key,)
            if group_cols:
                chg = changes
                for c, v in zip(group_cols, keyvals, strict=False):
                    chg = chg.filter(pl.col(c) == v)
            else:
                chg = changes
            change_dates = sorted(chg["change"].to_list()) if chg.height else []

            piv = (
                gdf.pivot(values=target, index="cohort", on="duration",
                          aggregate_function="first")
                .sort("cohort")
            )
            duration_cols = sorted(
                (c for c in piv.columns if c != "cohort"), key=lambda x: int(x)
            )
            loss_cum = piv.select(duration_cols).to_numpy().astype(float)
            cohorts = piv["cohort"].to_list()
            seg_ids = np.array(
                [sum(1 for c in change_dates if c <= ch) for ch in cohorts]
            )
            res = _graft_screen_group(
                loss_cum, np.arange(len(cohorts)), seg_ids
            )
            row = {c: v for c, v in zip(group_cols, keyvals, strict=False)}
            row.update(res)
            rows.append(row)

        return mirror_output(pl.DataFrame(rows), self._output_type)

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

        The ``Regime`` object carries only the per-cohort labels and
        change points, so the timeline view answers the "what regimes /
        where do they switch" question directly without retaining the
        per-cohort trajectory or PCA state a scatter view would need.

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
        from .._plot.regime import plot_regime
        return plot_regime(
            self, nrow=nrow, ncol=ncol, figsize=figsize, palette=palette
        )

    def to_polars(self) -> pl.DataFrame:
        return self._labels_df

    def to_pandas(self):
        return self._labels_df.to_pandas()

    def __repr__(self) -> str:
        # A manual regime is unbound (not yet applied to a triangle), so its
        # cohort / regime counts are meaningless -- show the change points.
        if self.method == "manual":
            changes = ", ".join(str(c) for c in self.change_points)
            return f"<Regime: manual, changes=[{changes}]>"
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
# Internal helper for manual Regime construction
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
# Detector config object (the deferred detection recipe)
# ---------------------------------------------------------------------------


@dataclass(kw_only=True)
class RegimeDetector:
    """Regime-detection config: parameters now, detection later.

    A config object (parameters only) whose :meth:`detect` runs E-Divisive (or
    Ward hierarchical) change-point detection on a triangle and returns a
    :class:`Regime`. It plays two roles, like every other estimator in the
    package:

    * **eager** -- ``RegimeDetector(...).detect(tri)`` detects now; this is how
      you get a :class:`Regime` from data (``Regime(change=...)`` is the manual
      alternative), parallel to ``PooledLoss().fit(tri)``.
    * **deferred** -- pass the detector itself as a ``regime=`` value
      (``CredibleLoss(regime=RegimeDetector(...))``). Fit / backtest call
      ``.detect`` on their own internal triangle -- inside backtest that is the
      **masked** training triangle of each fold, so change points never peek at
      held-out cells. This deferral is why the parameters live on a passable
      config object rather than on a triangle method.

    The default ``window="auto"`` resolves each group's trajectory window via
    the ATA factor-stability point (where the age-to-age factors become
    CV/RSE-stable), falling back to the elbow heuristic and finally to a fixed
    default (``6``).

    Parameters
    ----------
    target
        Metric driving change-point detection. Native cumulative (``"loss"``,
        ``"premium"``, ``"ratio"``) or derived (``"loss_ata"``,
        ``"premium_ata"``, ``"loss_intensity"``, ``"premium_intensity"``).
        Default ``"ratio"``.
    window
        Trajectory window: an integer (e.g. ``12``) for a fixed window, or
        ``"auto"`` (default) for the factor-stability-first, elbow-fallback
        resolver.
    by
        Per-group dispatch override. ``None`` (default) uses the Triangle's
        stored ``groups``; ``""`` / an empty sequence forces pooled detection;
        a string names one group column; a sequence runs detection per group
        COMBINATION.
    method, n_regimes, sig_level, n_permutations, min_size, seed
        Change-point algorithm controls (see :class:`Regime`).
    window_floor
        Minimum for ``window="auto"`` resolution -- the Kneedle elbow can
        resolve to a degenerate ``window=2`` on the pooled path, starving
        E-Divisive; a floor (e.g. ``4``) restores detection. ``None`` (default)
        leaves auto resolution unchanged; ignored for an explicit integer
        window.
    fdr
        Benjamini-Hochberg FDR correction across every permutation break in the
        call (multiplicity = number of coverages tested), controlling spurious
        breaks when many coverages are screened at once. Edge-scan breaks are
        exempt. E-Divisive only. Default ``False``.
    edge_scan
        Augment E-Divisive with a 1-vs-rest effect-size scan for a boundary
        regime occupying only the first / last 1..``min_size``-1 cohorts, which
        E-Divisive structurally cannot separate. Default ``False``.
    edge_threshold
        Effect-size (robust-z, noise units) an edge cohort must exceed for
        ``edge_scan`` to flag it. Default ``10.0``.
    window_sweep
        Optional sequence of trajectory windows (e.g. ``range(4, 13)``). When
        set, :attr:`Regime.candidates` is built from a sweep, adding
        ``window_stability`` and ``n_windows``; ``changes`` still comes from
        the single resolved ``window``. Default ``None``.
    grain_sweep
        Optional sequence of cohort grains (e.g. ``["M", "Q", "H"]``). When
        set, :attr:`Regime.candidates` is built by coarsening the triangle to
        each grain and re-detecting, then merging by change date -- adding
        ``grain`` and ``grain_stability``. Grains finer than the triangle's own
        are skipped. Combines with ``window_sweep``. Default ``None``.

    How a detected regime is CONSUMED (latest_only / segment_wise / covariate)
    is the ``treatment`` knob on the ESTIMATOR, not on the detector.
    """

    target: str = "ratio"
    window: int | str = "auto"
    by: str | Sequence[str] | None = None
    method: str = "e_divisive"
    n_regimes: int | None = None
    sig_level: float = 0.05
    n_permutations: int = 999
    min_size: int = 3
    seed: int | None = None
    window_floor: int | None = None
    fdr: bool = False
    edge_scan: bool = False
    edge_threshold: float = 10.0
    window_sweep: Sequence[int] | None = None
    grain_sweep: Sequence[str] | None = None

    def detect(self, triangle: Triangle) -> Regime:
        """Run detection on ``triangle`` and return a :class:`Regime`."""
        return Regime._from_triangle(
            triangle,
            target=self.target,
            window=self.window,
            by=self.by,
            method=self.method,
            n_regimes=self.n_regimes,
            sig_level=self.sig_level,
            n_permutations=self.n_permutations,
            min_size=self.min_size,
            seed=self.seed,
            window_floor=self.window_floor,
            fdr=self.fdr,
            edge_scan=self.edge_scan,
            edge_threshold=self.edge_threshold,
            window_sweep=self.window_sweep,
            grain_sweep=self.grain_sweep,
        )


# ---------------------------------------------------------------------------
# Cohort filter + regime resolver (used by fit / backtest / usage)
# ---------------------------------------------------------------------------


def _resolve_to_regime(regime, triangle):
    """Resolve a user ``regime=`` argument's DETECTION, leaving cuts intact.

    Runs detection only when needed and returns a value the downstream paths
    consume directly:

    * ``None`` -> ``None``;
    * a :class:`RegimeDetector` -> ``.detect(triangle)`` (a concrete Regime);
    * a :class:`Regime` -> unchanged;
    * a ``date`` / ``dict[segment -> date]`` -> unchanged (a direct manual cut).

    This is the single point where a deferred detector becomes a concrete
    Regime. Called at each public ``regime=`` entry before any cut is computed.
    The estimator's ``treatment`` knob (read separately from the estimator)
    then decides how the resolved regime is consumed.
    """
    if regime is None or isinstance(regime, (Regime, date, dict)):
        return regime
    if isinstance(regime, RegimeDetector):
        return regime.detect(triangle)
    raise TypeError(
        f"regime must be None / a date / a dict / Regime / RegimeDetector, "
        f"got {type(regime).__name__}"
    )


def _regime_cutoff_map(regime: Regime) -> pl.DataFrame | None:
    """Per-group ``(group_value, cutoff)`` table.

    The cutoff is the **latest** change point per group -- cohorts
    strictly before it are dropped (the drop-before-latest-change cohort
    filter used by the usage heatmap). Returns ``None`` when the regime
    has no change points to apply.
    """
    if not regime.change_points:
        return None

    changes = regime._changes_df
    group_cols = normalize_groups(regime.groups)
    if not group_cols or not all(g in changes.columns for g in group_cols):
        cutoff = max(regime.change_points)
        return pl.DataFrame({"_cutoff": [cutoff]}, schema={"_cutoff": pl.Date})

    return (
        changes.group_by(group_cols)
        .agg(pl.col("change").max().alias("_cutoff"))
    )


def _resolve_regime(regime, triangle):
    """Resolve a regime argument to a fit-ready cut.

    Returns ``None`` / a ``date`` / a ``dict[segment -> date]`` -- the only
    forms the engine (and the usage view) consume. Accepts those cut forms
    unchanged, a :class:`Regime` (the latest change per segment), or a
    :class:`RegimeDetector` (detected on ``triangle`` first via
    :func:`_resolve_to_regime`). Centralised here so every fit path (loss /
    premium / ratio / backtest) and the usage heatmap map the same inputs to a
    cut.
    """
    from datetime import date as _date
    if regime is None or isinstance(regime, (_date, dict)):
        return regime
    regime = _resolve_to_regime(regime, triangle)   # Regime / RegimeDetector -> Regime
    if regime is None:
        return None
    cutoff = _regime_cutoff_map(regime)        # DataFrame [group_cols?, _cutoff] | None
    if cutoff is None:
        return None
    if cutoff.columns == ["_cutoff"]:          # ungrouped -> a single date
        return cutoff["_cutoff"][0]
    group_cols = [c for c in cutoff.columns if c != "_cutoff"]
    out = {}
    for row in cutoff.iter_rows(named=True):
        key = row[group_cols[0]] if len(group_cols) == 1 else tuple(row[g] for g in group_cols)
        out[key] = row["_cutoff"]
    return out
