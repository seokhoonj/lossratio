"""Triangle visualisation -- matplotlib backend.

Implements ``Triangle.plot_triangle(kind="value" | "usage")``. Mirrors
R's ``plot_triangle.Triangle`` (``R/triangle-vis.R``).
"""

from __future__ import annotations

import bisect
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import (
    _iter_group_frames,
    format_group_value,
    group_eq,
    normalize_groups,
)
from ._plot import (
    _AMOUNT_METRICS,
    _PROP_METRICS,
    _RATIO_METRICS,
    _VALID_METRICS,
    _auto_divisor,
    _cohort_label,
    _format_period_series,
    _get_period_type,
    _hide_unused,
    _pretty_var_label,
    _resolve_grid,
    _resolve_plot_meta,
)

if TYPE_CHECKING:
    from .triangle import Triangle


# Value-view palette mirrors R `.cell_grid` defaults:
# threshold-flagged cells use "mistyrose", others "white".
_HIGH_COLOR = "mistyrose"
_LOW_COLOR = "white"
_NA_COLOR = "white"
_BORDER_COLOR = "black"
_BORDER_WIDTH = 0.3
# ggplot2 theme_grey parity for the cell-grid chrome (R `.switch_theme`):
# thin interior cell gridlines + a heavier black panel border, grey85
# facet strips, left-aligned plain title, grey30 right caption.
_GRID_WIDTH = 0.5            # interior cell separators (thin)
_PANEL_BORDER_WIDTH = 1.0    # outer panel frame (heavier)
_STRIP_FILL = "#d9d9d9"      # grey85 facet-strip background
_STRIP_EDGE = "black"        # strip outline
_STRIP_TEXT = "#1a1a1a"      # grey10 strip label
_CAPTION_COLOR = "#4d4d4d"   # grey30 caption
_STRIP_HEIGHT_IN = 0.20      # facet-strip height (inches, constant per panel)

# Usage-view categorical palette (R `.plot_triangle_usage:1395`).
_USAGE_COLORS: dict[str, str] = {
    "unused":  "#dcdcdc",
    "used":    "#1f77b4",
    "holdout": "#d62728",
    "future":  "#ffffff",
}
_USAGE_STATES: tuple[str, ...] = ("unused", "used", "holdout", "future")


def plot_triangle(
    triangle: Triangle,
    kind: str = "value",
    metric: str = "ratio",
    label_style: str = "value",
    label_size: float | None = None,
    amount_divisor: float | str = "auto",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
    *,
    x_axis: str = "dev",
    recent: int | None = None,
    regime: Any = None,
    holdout: int | None = None,
    maturity: Any = None,
) -> Any:
    """Triangle heatmap dispatcher. See
    :meth:`lossratio.Triangle.plot_triangle` for the public docs.
    """
    import matplotlib.pyplot as plt

    if kind not in ("value", "usage"):
        raise ValueError(f"`kind` must be 'value' or 'usage', got {kind!r}.")
    if x_axis not in ("dev", "calendar"):
        raise ValueError(
            f"`x_axis` must be 'dev' or 'calendar', got {x_axis!r}."
        )

    if kind == "usage":
        return _plot_triangle_usage(
            triangle,
            recent=recent,
            regime=regime,
            holdout=holdout,
            maturity=maturity,
            nrow=nrow,
            ncol=ncol,
            figsize=figsize,
            x_axis=x_axis,
        )

    if label_style not in ("value", "detail"):
        raise ValueError(
            f"`label_style` must be 'value' or 'detail', got {label_style!r}."
        )
    if metric not in _VALID_METRICS:
        raise ValueError(
            f"`metric` must be one of {_VALID_METRICS!r}; got {metric!r}."
        )

    if label_size is None:
        label_size = 7.0 if label_style == "detail" else 8.0

    df = triangle.df  # polars
    grp = triangle.groups
    coh = triangle.cohort
    dev = triangle.dev
    grain = triangle.grain

    # Resolve divisor against the values the labels will display.
    if metric in _AMOUNT_METRICS:
        div_vals = df[metric].to_numpy()
    elif metric in _RATIO_METRICS and label_style == "detail":
        denom = "premium" if metric == "ratio" else "incr_premium"
        div_vals = df[denom].to_numpy()
    else:
        div_vals = np.array([], dtype=float)

    if isinstance(amount_divisor, str):
        if amount_divisor != "auto":
            raise ValueError(
                f"`amount_divisor` must be numeric or 'auto', got "
                f"{amount_divisor!r}."
            )
        amount_divisor = _auto_divisor(div_vals)
    amount_divisor = float(amount_divisor)

    meta = _resolve_plot_meta(metric, amount_divisor)

    # Determine cell labels (cohort -> string, x -> string) once,
    # using consistent ordering across facets so the axes are stable.
    # The x-axis is either the development index (default, aligned
    # right-triangle layout) or the calendar period of each cell
    # (staircase layout: each cohort shifted to its own diagonal).
    coh_type = _get_period_type(coh, grain=grain)
    cohort_labels = _format_axis(df["cohort"], coh_type)
    coh_pairs = sorted(
        set(zip(df["cohort"].to_list(), cohort_labels)),
        key=lambda p: p[0],
    )
    y_levels = [lbl for _, lbl in coh_pairs]   # cohort, oldest -> newest

    if x_axis == "calendar":
        # calendar period of each cell = cohort + (dev - 1) at the grain.
        step = {"M": 1, "Q": 3, "H": 6, "Y": 12}[grain]
        cal_series = df.select(
            pl.col("cohort")
            .dt.offset_by(((pl.col("dev") - 1) * step).cast(pl.Utf8) + "mo")
            .alias("_cal")
        )["_cal"]
        x_values = cal_series.to_list()
        x_labels = _format_axis(cal_series, coh_type)
        x_axis_label = (
            _pretty_var_label(triangle.calendar)
            if triangle.calendar is not None
            else "calendar"
        )
    else:
        dev_type = _get_period_type(dev)  # dev_m / dev_q / ... aren't dates
        x_values = df["dev"].to_list()
        x_labels = _format_axis(df["dev"], dev_type)
        x_axis_label = _pretty_var_label(dev)

    # Ordered unique levels.
    x_pairs = sorted(set(zip(x_values, x_labels)), key=lambda p: p[0])
    x_levels = [lbl for _, lbl in x_pairs]   # smallest -> largest

    df = df.with_columns(
        pl.Series(name="_y_lbl", values=cohort_labels),
        pl.Series(name="_x_lbl", values=x_labels),
        pl.Series(name="_label", values=_cell_labels(df, metric, label_style, amount_divisor)),
    )

    # Faceting setup.
    facets = list(_iter_group_frames(df, grp))

    n_facets = len(facets)
    nrow, ncol = _resolve_grid(n_facets, nrow, ncol)

    cell_w = 0.45 + (0.18 if label_style == "detail" else 0.0)
    cell_h = 0.30 + (0.18 if label_style == "detail" else 0.0)
    if figsize is None:
        fig_w = max(4.0, cell_w * len(x_levels) * ncol + 1.2)
        fig_h = max(3.0, cell_h * len(y_levels) * nrow + 1.5)
        figsize = (fig_w, fig_h)

    from matplotlib.patches import Rectangle

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )

    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        _draw_cell_grid(
            ax,
            sub=sub,
            x_levels=x_levels,
            y_levels=y_levels,
            metric=metric,
            threshold=meta.threshold,
            when=meta.when,
            label_size=label_size,
        )
        title = format_group_value(group_value)
        if title:
            # ggplot2 facet strip: grey85 rectangle with a black outline and
            # a centered label, sitting in the reserved gap above the panel.
            # Panels share fixed scales (equal size), so a constant-height
            # strip in axes fraction renders uniformly across facets.
            panel_h_in = max(cell_h * len(y_levels), 0.5)
            strip_h = _STRIP_HEIGHT_IN / panel_h_in
            ax.set_title(" ", pad=_STRIP_HEIGHT_IN * 72.0)
            ax.add_patch(
                Rectangle(
                    (0.0, 1.0), 1.0, strip_h, transform=ax.transAxes,
                    facecolor=_STRIP_FILL, edgecolor=_STRIP_EDGE,
                    linewidth=0.5, clip_on=False, zorder=3,
                )
            )
            ax.text(
                0.5, 1.0 + strip_h / 2.0, title, transform=ax.transAxes,
                ha="center", va="center", fontsize=8.5, color=_STRIP_TEXT,
                clip_on=False, zorder=4,
            )

    # Hide unused axes.
    _hide_unused(axes, n_facets, nrow, ncol)

    # ggplot2 `plot.title`: left-aligned, plain weight, ~1.2x base size.
    fig.suptitle(meta.title, fontsize=13, fontweight="normal", x=0.01,
                 ha="left")
    fig.supxlabel(x_axis_label, fontsize=11)
    fig.supylabel(_cohort_label(coh, grain=grain), fontsize=11)
    if meta.caption:
        # ggplot2 `plot.caption`: right-aligned, grey30.
        fig.text(0.99, 0.005, meta.caption, ha="right", va="bottom",
                 fontsize=8.5, color=_CAPTION_COLOR)

    return fig


def _format_axis(values: pl.Series, period_type: str | None) -> list[str]:
    if period_type is None:
        return [str(v) for v in values.to_list()]
    return _format_period_series(values, period_type)


def _cell_labels(
    df: pl.DataFrame, metric: str, label_style: str, divisor: float
) -> list[str]:
    vals = df[metric].to_numpy()
    if metric in _RATIO_METRICS:
        scaled = vals * 100.0
        if label_style == "value":
            return [_fmt_or_blank(v, "%.0f") for v in scaled]
        loss_col = "loss" if metric == "ratio" else "incr_loss"
        prem_col = "premium" if metric == "ratio" else "incr_premium"
        loss_vals = df[loss_col].to_numpy() / divisor
        prem_vals = df[prem_col].to_numpy() / divisor
        out = []
        for r, lo, pr in zip(scaled, loss_vals, prem_vals):
            if not np.isfinite(r):
                out.append("")
            else:
                out.append(f"{r:.0f}\n({lo:.1f}/{pr:.1f})")
        return out
    if metric in _AMOUNT_METRICS:
        scaled = vals / divisor
        return [_fmt_or_blank(v, "%.1f") for v in scaled]
    # prop
    scaled = vals * 100.0
    return [_fmt_or_blank(v, "%.1f") for v in scaled]


def _fmt_or_blank(v: float, fmt: str) -> str:
    if not np.isfinite(v):
        return ""
    return fmt % v


def _draw_cell_grid(
    ax,
    *,
    sub: pl.DataFrame,
    x_levels: list[str],
    y_levels: list[str],
    metric: str,
    threshold: float,
    when: str,
    label_size: float,
) -> None:
    """Draw one facet -- tiles, labels, panel border."""
    from matplotlib.patches import Rectangle

    x_idx = {lbl: i for i, lbl in enumerate(x_levels)}
    y_idx = {lbl: i for i, lbl in enumerate(y_levels)}

    metric_vals = sub[metric].to_numpy()
    x_pos = [x_idx[lbl] for lbl in sub["_x_lbl"].to_list()]
    y_pos = [y_idx[lbl] for lbl in sub["_y_lbl"].to_list()]
    labels = sub["_label"].to_list()

    for xi, yi, v, lab in zip(x_pos, y_pos, metric_vals, labels):
        color = _threshold_color(v, threshold, when)
        ax.add_patch(
            Rectangle(
                (xi - 0.5, yi - 0.5), 1.0, 1.0,
                facecolor=color, edgecolor="none",
            )
        )
        if lab:
            ax.text(
                xi, yi, lab,
                ha="center", va="center",
                fontsize=label_size, color="black",
            )

    nx = len(x_levels)
    ny = len(y_levels)
    ax.set_xlim(-0.5, nx - 0.5)
    # Cohort axis: oldest at top. `y_levels` runs oldest -> newest
    # (index 0 = oldest), so invert the y-limits to place index 0 at
    # the top -- the run-off triangle's fully-developed (oldest) row
    # sits on top. Mirrors R's `.cell_grid` `scale_y_reverse()`.
    ax.set_ylim(ny - 0.5, -0.5)

    ax.set_xticks(range(nx))
    ax.set_xticklabels(x_levels, rotation=0, fontsize=8)
    ax.set_yticks(range(ny))
    ax.set_yticklabels(y_levels, fontsize=8)

    # Interior cell separators (thin) + a heavier outer panel border, so
    # the frame reads distinctly from the inner gridlines (ggplot2's
    # `panel.border` on top of the half-integer cell grid).
    for k in range(1, nx):
        ax.axvline(k - 0.5, color=_BORDER_COLOR, linewidth=_GRID_WIDTH,
                   zorder=2)
    for k in range(1, ny):
        ax.axhline(k - 0.5, color=_BORDER_COLOR, linewidth=_GRID_WIDTH,
                   zorder=2)
    ax.add_patch(
        Rectangle(
            (-0.5, -0.5), nx, ny, fill=False, edgecolor=_BORDER_COLOR,
            linewidth=_PANEL_BORDER_WIDTH, zorder=4,
        )
    )

    ax.set_xlabel("")
    ax.set_ylabel("")
    ax.set_aspect("auto")
    ax.tick_params(axis="both", which="both", length=0)
    for spine in ax.spines.values():
        spine.set_visible(False)


def _threshold_color(v: float, threshold: float, when: str) -> str:
    if not np.isfinite(v):
        return _NA_COLOR
    if when == ">":
        flag = v > threshold
    elif when == ">=":
        flag = v >= threshold
    elif when == "<":
        flag = v < threshold
    elif when == "<=":
        flag = v <= threshold
    else:
        raise ValueError(f"Unknown `when`: {when!r}.")
    return _HIGH_COLOR if flag else _LOW_COLOR


# ---------------------------------------------------------------------------
# Usage view -- categorical status heatmap with filter overlays
# ---------------------------------------------------------------------------


def _resolve_maturity_k(
    maturity: Any,
    triangle: Triangle | None = None,
) -> int | None:
    """Resolve the ``maturity`` arg to an integer ``k*`` or ``None``.

    ``None``: no maturity, no overlay, no hybrid threshold.
    ``int``: scalar ``k*``, used directly.
    :class:`Maturity` instance: read its ``change`` column (R parity:
    the maturity link's to-index).
    ``"auto"``: when ``triangle`` is supplied, runs
    :meth:`Triangle.detect_maturity` and collapses the per-group result
    to a single scalar via ``max(k*)`` (matches R's per-group fallback
    used by the SA fitter); when ``triangle`` is ``None``, returns
    ``None`` silently so the caller can detect upstream.
    Callable: invoked with ``triangle`` (when supplied) and the result
    is re-resolved.
    """
    from .maturity import Maturity

    if maturity is None:
        return None
    if isinstance(maturity, str):
        if maturity == "auto":
            if triangle is None:
                return None
            try:
                mat = triangle.detect_maturity()
            except (ValueError, KeyError, RuntimeError):
                # Failed auto-detect -> render the usage view without the
                # maturity overlay rather than crash the plot.
                return None
            return _resolve_maturity_k(mat, triangle=triangle)
        raise ValueError(
            f"`maturity` must be None, an int, 'auto', a Maturity "
            f"instance, or a callable; got {maturity!r}."
        )
    if isinstance(maturity, Maturity):
        k = maturity.point
        if k is None:
            return None
        if isinstance(k, dict):
            vals = [v for v in k.values() if v is not None]
            return int(max(vals)) if vals else None
        return int(k)
    if isinstance(maturity, (int, np.integer)) and not isinstance(maturity, bool):
        if int(maturity) < 1:
            raise ValueError(
                f"`maturity` must be a positive integer; got {maturity}."
            )
        return int(maturity)
    if callable(maturity):
        if triangle is None:
            return None
        return _resolve_maturity_k(maturity(triangle), triangle=triangle)
    raise ValueError(
        f"`maturity` must be None, an int, 'auto', a Maturity instance, "
        f"or a callable; got {maturity!r}."
    )


def _resolve_regime_for_usage(triangle: Triangle, regime: Any) -> Any:
    """Resolve a regime input to a Regime instance, or None.

    ``None``: no regime overlay.
    :class:`Regime` instance: used directly.
    ``"auto"``: runs :meth:`Triangle.detect_regime` (default
    ``window="auto"``, ``method="e_divisive"``); returns ``None`` if
    detection raises (e.g. degenerate triangle), so the usage view
    still renders without a regime overlay.
    Callable: invoked with ``triangle``.
    """
    from .regime import Regime

    if regime is None:
        return None
    if isinstance(regime, Regime):
        return regime
    if isinstance(regime, str):
        if regime == "auto":
            try:
                return triangle.detect_regime()
            except (ValueError, KeyError, RuntimeError):
                # Failed auto-detect -> render without the regime overlay.
                return None
        raise ValueError(
            f"`regime` must be None, a Regime instance, 'auto', or a "
            f"callable; got {regime!r}."
        )
    if callable(regime):
        return regime(triangle)
    raise ValueError(
        f"`regime` must be None, a Regime instance, 'auto', or a callable; "
        f"got {regime!r}."
    )


def _seg_dev_min(
    grp_rows: pl.DataFrame,
    cd_vec: list,
    group_cols: list[str],
    bridge: bool = False,
) -> pl.DataFrame:
    """Compute per-cell ``dev_min`` for the segment_wise mini-triangle.

    For each cell in ``grp_rows``, classify its cohort into a segment
    via ``np.searchsorted(cd_vec, cohort, side='right')`` (i.e. the
    R ``findInterval`` semantic + 1) and delegate the bounds math to
    :func:`lossratio.regime._compute_segment_mini_tri_bounds`. When
    ``bridge`` is ``True`` the older segments' walls are widened by
    the calendar-diagonal bridge (segment_wise_bridged treatment);
    when ``False`` only the natural wall applies.

    The returned frame has columns ``[*group_cols, cohort, dev,
    _seg_dev_min]`` that the caller joins back onto the expanded grid
    to override ``is_fit_data``.
    """
    from .regime import _compute_segment_mini_tri_bounds

    cohorts = grp_rows["cohort"].to_numpy()
    cd_arr = np.array(cd_vec, dtype="datetime64[D]")
    coh_arr = np.array(cohorts, dtype="datetime64[D]")
    # `findInterval(coh, cd) + 1` in R == np.searchsorted(cd, coh,
    # side='right') + 1, mapping each cohort to a 1-indexed segment.
    seg_id = np.searchsorted(cd_arr, coh_arr, side="right") + 1

    coh_ranks = grp_rows["_coh_rank"].to_numpy()
    max_cal = int(grp_rows["_max_cal"][0])

    dev_min_arr = _compute_segment_mini_tri_bounds(
        coh_ranks=coh_ranks,
        seg_ids=seg_id,
        max_cal=max_cal,
        bridge=bridge,
    )

    work = grp_rows.with_columns(pl.Series("_seg_dev_min", dev_min_arr))
    keep_cols = list(group_cols) + ["cohort", "dev", "_seg_dev_min"]
    return work.select(keep_cols)


def _compute_triangle_usage(
    triangle: Triangle,
    recent: int | None = None,
    regime: Any = None,
    holdout: int | None = None,
    m_k: int | None = None,
) -> pl.DataFrame:
    """Build the per-cell status grid driving the usage heatmap.

    Mirrors R's ``.compute_triangle_usage`` (``R/triangle-vis.R:964``).
    Returns a polars DataFrame with columns ``groups`` (when present),
    ``cohort``, ``dev``, ``status`` (one of ``"unused" | "used" |
    "holdout" | "future"``).

    ``segment_wise`` regimes carve out a mini-triangle per segment
    anchored on the latest calendar diagonal -- cells in an affected
    group but outside their segment's mini-triangle drop from
    ``used`` to ``unused``. Maturity (``m_k``) does not shrink the
    mini-triangle (R parity: it's a separate dashed-vline reference
    only).
    """
    if recent is not None and (recent < 1 or not isinstance(recent, (int, np.integer))):
        raise ValueError(f"`recent` must be a positive integer; got {recent!r}.")
    if holdout is not None and (
        holdout < 1 or not isinstance(holdout, (int, np.integer))
    ):
        raise ValueError(f"`holdout` must be a positive integer; got {holdout!r}.")

    obs = triangle.df  # polars, standardized columns

    grp = triangle.groups
    grp_cols: list[str] = normalize_groups(grp)

    # 1. Build the full (group x cohort x dev) grid.
    if grp_cols:
        full = obs.select(grp_cols + ["cohort", "dev"])
        parts: list[pl.DataFrame] = []
        for g_val, sub in full.group_by(grp_cols):
            cohorts = sub["cohort"].unique().sort()
            devs = sub["dev"].unique().sort()
            grid = cohorts.to_frame().join(devs.to_frame(), how="cross")
            for col, v in zip(grp_cols, g_val):
                grid = grid.with_columns(pl.lit(v).alias(col))
            parts.append(grid.select(grp_cols + ["cohort", "dev"]))
        expanded = pl.concat(parts, how="vertical_relaxed")
    else:
        cohorts = obs["cohort"].unique().sort()
        devs = obs["dev"].unique().sort()
        expanded = cohorts.to_frame().join(devs.to_frame(), how="cross")

    # 2. Tag rows actually present in input (vs filled-in by the grid).
    obs_marker = obs.select(grp_cols + ["cohort", "dev"]).with_columns(
        pl.lit(True).alias("_data_present")
    )
    expanded = expanded.join(
        obs_marker, on=grp_cols + ["cohort", "dev"], how="left"
    ).with_columns(pl.col("_data_present").fill_null(False))

    # 3. Cohort rank + calendar index, optionally per group.
    if grp_cols:
        expanded = expanded.with_columns(
            pl.col("cohort").rank(method="dense").over(grp_cols).cast(pl.Int64).alias("_coh_rank")
        )
    else:
        expanded = expanded.with_columns(
            pl.col("cohort").rank(method="dense").cast(pl.Int64).alias("_coh_rank")
        )
    expanded = expanded.with_columns(
        (pl.col("_coh_rank") + pl.col("dev") - 1).alias("_cal_idx")
    )

    # max_cal among data-present cells (not the full grid).
    if grp_cols:
        expanded = expanded.with_columns(
            pl.when(pl.col("_data_present"))
            .then(pl.col("_cal_idx"))
            .otherwise(None)
            .max()
            .over(grp_cols)
            .alias("_max_cal")
        )
    else:
        max_cal_val = (
            expanded.filter(pl.col("_data_present"))["_cal_idx"].max()
        )
        expanded = expanded.with_columns(
            pl.lit(max_cal_val).alias("_max_cal")
        )

    # 4. is_observed -- cell would be in the triangle (.cal_idx <= max_cal).
    expanded = expanded.with_columns(
        (pl.col("_cal_idx") <= pl.col("_max_cal")).alias("is_observed")
    )

    # 5. Holdout flag + adjusted max_cal_fit.
    if holdout is not None:
        expanded = expanded.with_columns(
            (
                pl.col("is_observed")
                & (pl.col("_cal_idx") > (pl.col("_max_cal") - holdout))
            ).alias("is_held_out"),
            (pl.col("_max_cal") - holdout).alias("_max_cal_fit"),
        )
    else:
        expanded = expanded.with_columns(
            pl.lit(False).alias("is_held_out"),
            pl.col("_max_cal").alias("_max_cal_fit"),
        )

    # 6. Resolve regime change date(s) under latest_only.
    cd_scalar: Any = None
    cd_df: pl.DataFrame | None = None
    is_segment_wise = False
    is_bridged = False

    if regime is not None:
        treatment = getattr(regime, "treatment", None)
        is_segment_wise = treatment in (
            "segment_bridged", "segment_bridged_borrowed"
        )
        is_bridged = is_segment_wise  # both treatments mask the bridged band
        if not is_segment_wise:
            from .regime import _regime_cutoff_map
            cutoff_map = _regime_cutoff_map(regime)
            if cutoff_map is not None:
                reg_grp_cols = normalize_groups(regime.groups)
                if "_cutoff" in cutoff_map.columns and cutoff_map.height == 1 and (
                    not reg_grp_cols
                    or not all(g in cutoff_map.columns for g in reg_grp_cols)
                ):
                    cd_scalar = cutoff_map["_cutoff"][0]
                else:
                    cd_df = cutoff_map.rename({"_cutoff": "_cd_join"})

    # 7. Build the pass_filter boolean.
    has_recent = recent is not None
    has_change = cd_scalar is not None or cd_df is not None

    if cd_df is not None:
        # broadcast per-group change date onto expanded rows
        expanded = expanded.join(
            cd_df, on=normalize_groups(regime.groups), how="left"
        )
        change_pass_expr = pl.col("_cd_join").is_null() | (
            pl.col("cohort") >= pl.col("_cd_join")
        )
    elif cd_scalar is not None:
        change_pass_expr = pl.col("cohort") >= cd_scalar
    else:
        change_pass_expr = pl.lit(True)

    if m_k is not None:
        m_k_geq = pl.col("dev") >= m_k
        m_k_lt = pl.col("dev") < m_k
    else:
        m_k_geq = pl.lit(False)
        m_k_lt = pl.lit(True)
    has_m_k = m_k is not None

    if has_recent and has_change:
        if has_m_k:
            pass_filter = (m_k_lt & change_pass_expr) | (
                m_k_geq & (pl.col("_cal_idx") > (pl.col("_max_cal_fit") - recent))
            )
        else:
            pass_filter = change_pass_expr & (
                pl.col("_cal_idx") > (pl.col("_max_cal_fit") - recent)
            )
    elif has_recent:
        pass_filter = pl.col("_cal_idx") > (pl.col("_max_cal_fit") - recent)
    elif has_change:
        if has_m_k:
            pass_filter = m_k_geq | change_pass_expr
        else:
            pass_filter = change_pass_expr
    else:
        pass_filter = pl.lit(True)

    expanded = expanded.with_columns(pass_filter.alias("_pass_filter"))

    if cd_df is not None:
        expanded = expanded.drop("_cd_join")

    # 8. is_fit_data, is_excluded, status.
    expanded = expanded.with_columns(
        (
            pl.col("is_observed")
            & ~pl.col("is_held_out")
            & pl.col("_pass_filter")
        ).alias("is_fit_data")
    )

    # 8b. segment_wise mini-triangle override. Each regime segment carves
    # out its own mini-triangle anchored on the latest cal diagonal:
    #
    #   dev_min(segment) = max_cal - seg_last_cohort_rank + 1
    #
    # Cells in an affected group but outside their segment's
    # mini-triangle drop from `used` to `unused`. Maturity (`m_k`) does
    # not shrink the mini-triangle here (R parity: it's a separate
    # vline only).
    if is_segment_wise and regime is not None:
        bp = (
            regime._changes_df
            if hasattr(regime, "_changes_df")
            else regime.changes
        )
        if bp is not None and bp.height > 0 and "change" in bp.columns:
            reg_groups = getattr(regime, "groups", None)
            reg_grp_cols = normalize_groups(reg_groups)
            if reg_grp_cols and all(
                g in expanded.columns for g in reg_grp_cols
            ):
                # per-group segments
                dev_min_parts: list[pl.DataFrame] = []
                for g_val, grp_changes in _iter_group_frames(
                    bp.sort([*reg_grp_cols, "change"]), reg_groups
                ):
                    cd_vec = grp_changes.sort("change")["change"].to_list()
                    grp_rows = expanded.filter(group_eq(reg_groups, g_val))
                    if grp_rows.height == 0 or not cd_vec:
                        continue
                    dev_min_parts.append(
                        _seg_dev_min(
                            grp_rows, cd_vec, reg_grp_cols,
                            bridge=is_bridged,
                        )
                    )
                if dev_min_parts:
                    dev_min_df = pl.concat(dev_min_parts)
                    expanded = expanded.join(
                        dev_min_df,
                        on=[*reg_grp_cols, "cohort", "dev"],
                        how="left",
                    ).with_columns(
                        pl.when(pl.col("_seg_dev_min").is_not_null())
                        .then(pl.col("is_fit_data") & (pl.col("dev") >= pl.col("_seg_dev_min")))
                        .otherwise(pl.col("is_fit_data"))
                        .alias("is_fit_data")
                    ).drop("_seg_dev_min")
            else:
                # pooled segments
                cd_vec = bp.sort("change")["change"].to_list()
                if cd_vec:
                    dev_min_df = _seg_dev_min(
                        expanded, cd_vec, [], bridge=is_bridged,
                    )
                    expanded = expanded.join(
                        dev_min_df, on=["cohort", "dev"], how="left",
                    ).with_columns(
                        pl.when(pl.col("_seg_dev_min").is_not_null())
                        .then(pl.col("is_fit_data") & (pl.col("dev") >= pl.col("_seg_dev_min")))
                        .otherwise(pl.col("is_fit_data"))
                        .alias("is_fit_data")
                    ).drop("_seg_dev_min")

    expanded = expanded.with_columns(
        (
            pl.col("is_observed")
            & ~pl.col("is_held_out")
            & ~pl.col("is_fit_data")
        ).alias("is_excluded")
    )

    expanded = expanded.with_columns(
        pl.when(pl.col("is_held_out")).then(pl.lit("holdout"))
        .when(pl.col("is_fit_data")).then(pl.lit("used"))
        .when(pl.col("is_excluded")).then(pl.lit("unused"))
        .otherwise(pl.lit("future"))
        .alias("status")
    )

    keep = grp_cols + ["cohort", "dev", "status"]
    return expanded.select(keep)


def _first_post_change_idx(
    cohort_values_desc: list[Any],
    change_value: Any,
) -> int | None:
    """Position (0-based) on the descending y-axis where the *first*
    cohort >= ``change_value`` sits. ``cohort_values_desc`` is the
    underlying cohort dates / ints in the same order as the y-tick
    labels (newest at top). Returns ``None`` if no cohort is at or after
    the change.

    The hline is drawn at ``returned_idx + 0.5`` (just above the row
    toward older cohorts), mirroring R's ``geom_hline(yintercept = idx
    + 0.5)``.
    """
    ascending = list(reversed(cohort_values_desc))
    for i, c in enumerate(ascending):
        if c >= change_value:
            return len(cohort_values_desc) - 1 - i
    return None


def _plot_triangle_usage(
    triangle: Triangle,
    *,
    recent: int | None,
    regime: Any,
    holdout: int | None,
    maturity: Any,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
    x_axis: str = "dev",
) -> Any:
    """Categorical status heatmap; see ``plot_triangle.Triangle(kind="usage")``."""
    import matplotlib.pyplot as plt
    from matplotlib.patches import Patch, Rectangle

    regime_obj = _resolve_regime_for_usage(triangle, regime)
    m_k = _resolve_maturity_k(maturity, triangle=triangle)

    usage_df = _compute_triangle_usage(
        triangle,
        recent=recent,
        regime=regime_obj,
        holdout=holdout,
        m_k=m_k,
    )

    grp = triangle.groups
    coh = triangle.cohort
    dev = triangle.dev
    grain = triangle.grain
    coh_type = _get_period_type(coh, grain=grain)

    cohort_labels = _format_axis(usage_df["cohort"], coh_type)
    usage_df = usage_df.with_columns(
        pl.Series(name="_y_lbl", values=cohort_labels)
    )

    # Ordered cohort levels: newest at top (descending).
    coh_pairs = sorted(
        set(zip(usage_df["cohort"].to_list(), cohort_labels)),
        key=lambda p: p[0],
        reverse=True,
    )
    cohort_values_desc = [c for c, _ in coh_pairs]
    y_levels = [lbl for _, lbl in coh_pairs]

    # x-axis: development index (default, aligned right-triangle) or the
    # calendar period of each cell (staircase: each cohort on its own
    # diagonal). recent / holdout masks are calendar-diagonal, so on the
    # calendar axis they read as clean vertical bands.
    step = {"M": 1, "Q": 3, "H": 6, "Y": 12}[grain]
    if x_axis == "calendar":
        usage_df = usage_df.with_columns(
            pl.col("cohort")
            .dt.offset_by(((pl.col("dev") - 1) * step).cast(pl.Utf8) + "mo")
            .alias("_xcal")
        )
        xkey = "_xcal"
        x_levels = sorted(set(usage_df["_xcal"].to_list()))
        x_labels = _format_axis(
            pl.Series(name="_x", values=x_levels), coh_type
        )
        x_axis_label = (
            _pretty_var_label(triangle.calendar)
            if triangle.calendar is not None
            else "calendar"
        )
    else:
        xkey = "dev"
        x_levels = sorted(set(usage_df["dev"].to_list()))
        x_labels = [str(d) for d in x_levels]
        x_axis_label = _pretty_var_label(dev)

    facets = list(_iter_group_frames(usage_df, grp))

    n_facets = len(facets)
    nrow, ncol = _resolve_grid(n_facets, nrow, ncol)

    if figsize is None:
        fig_w = max(4.0, 0.4 * len(x_levels) * ncol + 1.5)
        fig_h = max(3.0, 0.30 * len(y_levels) * nrow + 1.8)
        figsize = (fig_w, fig_h)

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )

    # Per-group regime change dates for hline routing (latest_only,
    # per-group case). Scalar case uses cd_scalar.
    per_group_cd: dict | None = None
    cd_scalar: Any = None
    is_segment_wise = (
        regime_obj is not None
        and getattr(regime_obj, "treatment", None)
        in ("segment_bridged", "segment_bridged_borrowed")
    )
    if regime_obj is not None and not is_segment_wise:
        from .regime import _regime_cutoff_map
        cm = _regime_cutoff_map(regime_obj)
        if cm is not None:
            gcols = normalize_groups(regime_obj.groups)
            if gcols and all(g in cm.columns for g in gcols):
                is_single = isinstance(regime_obj.groups, str)
                per_group_cd = {
                    (
                        row[gcols[0]]
                        if is_single
                        else tuple(row[c] for c in gcols)
                    ): row["_cutoff"]
                    for row in cm.iter_rows(named=True)
                }
            else:
                cd_scalar = cm["_cutoff"][0]

    # Segment_wise hlines come from raw changes (one per change point).
    seg_changes: list = []
    if is_segment_wise:
        ch = regime_obj.changes
        if ch is not None and getattr(ch, "height", 0) > 0:
            seg_changes = sorted(set(ch["change"].to_list()))

    x_idx = {d: i for i, d in enumerate(x_levels)}
    y_idx = {lbl: i for i, lbl in enumerate(y_levels)}

    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]

        for row in sub.iter_rows(named=True):
            xi = x_idx[row[xkey]]
            yi = y_idx[row["_y_lbl"]]
            color = _USAGE_COLORS[row["status"]]
            ax.add_patch(
                Rectangle(
                    (xi - 0.5, yi - 0.5), 1.0, 1.0,
                    facecolor=color,
                    edgecolor="white",
                    linewidth=0.4,
                )
            )

        nx = len(x_levels)
        ny = len(y_levels)
        ax.set_xlim(-0.5, nx - 0.5)
        ax.set_ylim(-0.5, ny - 0.5)
        ax.set_xticks(range(nx))
        ax.set_xticklabels(x_labels, fontsize=8)
        ax.set_yticks(range(ny))
        ax.set_yticklabels(y_levels, fontsize=8)

        # Maturity boundary at dev = m_k. On the dev axis this is a single
        # vertical line; on the calendar axis it is a stepped diagonal,
        # since each cohort reaches dev = m_k at its own calendar period.
        if m_k is not None:
            if x_axis == "calendar":
                _draw_maturity_staircase(
                    ax, m_k, step, cohort_values_desc, x_levels
                )
            else:
                ax.axvline(
                    x_idx_get(x_idx, m_k) - 0.5
                    if m_k in x_idx
                    else m_k - 1.5,
                    linestyle="--", color="black", linewidth=0.6,
                )

        # Regime hline.
        if per_group_cd is not None and group_value is not None:
            cd_g = per_group_cd.get(group_value)
            if cd_g is not None:
                hidx = _first_post_change_idx(cohort_values_desc, cd_g)
                if hidx is not None:
                    ax.axhline(
                        hidx + 0.5,
                        linestyle="--", color="black", linewidth=0.6,
                    )
        elif cd_scalar is not None:
            hidx = _first_post_change_idx(cohort_values_desc, cd_scalar)
            if hidx is not None:
                ax.axhline(
                    hidx + 0.5,
                    linestyle="--", color="black", linewidth=0.6,
                )
        elif is_segment_wise:
            for ch_val in seg_changes:
                hidx = _first_post_change_idx(cohort_values_desc, ch_val)
                if hidx is not None:
                    ax.axhline(
                        hidx + 0.5,
                        linestyle="--", color="black", linewidth=0.6,
                    )

        ax.set_xlabel("")
        ax.set_ylabel("")
        ax.tick_params(axis="both", which="both", length=0)
        for spine in ax.spines.values():
            spine.set_visible(False)
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=10)

    _hide_unused(axes, n_facets, nrow, ncol)

    # Title with active filters.
    parts: list[str] = []
    if recent is not None:
        parts.append(f"recent={int(recent)}")
    if is_segment_wise and seg_changes:
        parts.append(
            "regime=" + ",".join(str(c) for c in seg_changes)
        )
    elif cd_scalar is not None:
        parts.append(f"regime={cd_scalar}")
    elif per_group_cd is not None:
        unique_cd = sorted({str(v) for v in per_group_cd.values() if v is not None})
        if unique_cd:
            parts.append("regime=" + ",".join(unique_cd))
    if holdout is not None:
        parts.append(f"holdout={int(holdout)}")
    title_txt = (
        f"Data usage ({', '.join(parts)})" if parts else "Data usage (full)"
    )
    fig.suptitle(title_txt, fontsize=12, fontweight="bold")

    if m_k is not None:
        fig.text(
            0.5, 0.925,
            f"hybrid mode: maturity k* = {m_k}",
            ha="center", va="top", fontsize=9, style="italic",
        )

    fig.supxlabel(x_axis_label, fontsize=10)
    fig.supylabel(_cohort_label(coh, grain=grain), fontsize=10)

    # Legend on the figure (categorical key).
    legend_handles = [
        Patch(facecolor=_USAGE_COLORS[s], edgecolor="black", linewidth=0.3, label=s)
        for s in _USAGE_STATES
    ]
    fig.legend(
        handles=legend_handles,
        loc="lower center",
        ncol=4,
        frameon=False,
        fontsize=8,
        bbox_to_anchor=(0.5, -0.02),
    )

    return fig


def _draw_maturity_staircase(
    ax: Any,
    m_k: int,
    step: int,
    cohort_values_desc: list,
    x_levels: list,
) -> None:
    """Draw the dev = ``m_k`` boundary on a calendar x-axis.

    On the calendar axis each cohort reaches ``dev = m_k`` at its own
    calendar period (``cohort + (m_k - 1)`` at the grain), so the
    boundary is a stepped diagonal rather than a single vertical line.
    Rows run newest-cohort-first (``yi = 0`` at the top), so the step
    descends to the left as cohorts get older.
    """
    months = (m_k - 1) * step
    boundaries = (
        pl.DataFrame({"c": cohort_values_desc})
        .with_columns(pl.col("c").dt.offset_by(f"{months}mo").alias("b"))["b"]
        .to_list()
    )
    prev_bx: float | None = None
    for yi, b in enumerate(boundaries):
        bx = bisect.bisect_left(x_levels, b) - 0.5
        ax.plot(
            [bx, bx], [yi - 0.5, yi + 0.5],
            linestyle="--", color="black", linewidth=0.6,
        )
        if prev_bx is not None:
            ax.plot(
                [prev_bx, bx], [yi - 0.5, yi - 0.5],
                linestyle="--", color="black", linewidth=0.6,
            )
        prev_bx = bx


def x_idx_get(x_idx: dict, key: int) -> int:
    """Lookup with fall-back for off-grid maturity values (k* past max
    dev or before min dev). The fallback positions the vline just past
    the boundary so it still reads as an overlay."""
    if key in x_idx:
        return x_idx[key]
    idxs = list(x_idx.values())
    if not idxs:
        return key
    if key > max(x_idx.keys()):
        return max(idxs) + 1
    if key < min(x_idx.keys()):
        return min(idxs) - 1
    return key


def _draw_cohort_lines(ax, sub, metric, coh_color, summary, summary_min_n,
                       hline):
    """Per-cohort trajectories (+ optional summary overlay) on one facet."""
    for g in sub.partition_by("cohort", maintain_order=True):
        gg = g.sort("dev")
        x = gg["dev"].to_list()
        y = gg[metric].to_list()
        if summary:
            ax.plot(x, y, color="0.7", alpha=0.5, linewidth=0.6, zorder=1)
        else:
            ax.plot(x, y, color=coh_color(gg["cohort"][0]), linewidth=1.1,
                    zorder=2)

    if hline is not None:
        ax.axhline(hline, linestyle="--", color="red", linewidth=0.8, zorder=1)

    if not summary:
        return

    # Mean / Median / Weighted summary lines, masked where too few cohorts.
    lcol, pcol = (("loss", "premium") if metric == "ratio"
                  else ("incr_loss", "incr_premium"))
    agg = (sub.group_by("dev")
              .agg(mean=pl.col(metric).mean(),
                   median=pl.col(metric).median(),
                   wl=pl.col(lcol).sum(),
                   wp=pl.col(pcol).sum(),
                   n=pl.len())
              .sort("dev")
              .with_columns(weighted=pl.col("wl") / pl.col("wp")))
    xd = np.asarray(agg["dev"].to_list(), dtype=float)
    n = np.asarray(agg["n"].to_list())
    masked = summary_min_n is not None and np.isfinite(summary_min_n)
    mask = n < summary_min_n if masked else np.zeros(len(n), dtype=bool)
    for col, color, lbl in (("mean", "black", "Mean"),
                            ("median", "#1f77b4", "Median"),
                            ("weighted", "#d62728", "Weighted")):
        yv = np.asarray(agg[col].to_list(), dtype=float).copy()
        yv[mask] = np.nan
        ax.plot(xd, yv, color=color, linewidth=1.7, label=lbl, zorder=3)
    if masked:
        le = n <= summary_min_n
        if le.any():
            ax.axvline(xd[int(np.argmax(le))], linestyle=":", color="0.4",
                       linewidth=1.0, zorder=1)


def plot(
    triangle: Triangle,
    metric: str = "ratio",
    summary: bool = False,
    summary_min_n: int = 5,
    amount_divisor: float | str = "auto",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """Cohort-trajectory line plot -- mirrors R's ``plot.Triangle``.

    One line per cohort (x = development index, y = ``metric``), faceted by
    ``groups``. ``summary=True`` (ratio metrics only) fades cohort lines to
    grey and overlays Mean / Median / Weighted lines, masked where fewer
    than ``summary_min_n`` cohorts contribute.
    """
    import warnings

    import matplotlib as mpl
    import matplotlib.pyplot as plt
    from matplotlib.cm import ScalarMappable
    from matplotlib.colors import ListedColormap, Normalize
    from matplotlib.patches import Rectangle
    from matplotlib.ticker import FuncFormatter

    if metric not in _VALID_METRICS:
        raise ValueError(
            f"`metric` must be one of {_VALID_METRICS!r}; got {metric!r}."
        )

    df = triangle.to_polars()
    grp = triangle.groups
    coh = triangle.cohort
    dev = triangle.dev
    grain = triangle.grain

    if metric in _AMOUNT_METRICS:
        div_vals = df[metric].to_numpy()
    else:
        div_vals = np.array([], dtype=float)
    if isinstance(amount_divisor, str):
        if amount_divisor != "auto":
            raise ValueError(
                f"`amount_divisor` must be numeric or 'auto', got "
                f"{amount_divisor!r}."
            )
        amount_divisor = _auto_divisor(div_vals)
    amount_divisor = float(amount_divisor)
    meta = _resolve_plot_meta(metric, amount_divisor)

    is_ratio = metric in _RATIO_METRICS
    is_prop = metric in _PROP_METRICS
    if summary and not is_ratio:
        warnings.warn(
            "Summary overlay is only supported for `ratio` and `incr_ratio`."
        )
        summary = False

    facets: list[tuple[Any, pl.DataFrame]] = list(
        _iter_group_frames(df, grp)
    )
    n_facets = len(facets)

    nrow, ncol = _resolve_grid(n_facets, nrow, ncol)

    if figsize is None:
        figsize = (max(4.0, 2.6 * ncol + 0.8), max(3.0, 2.2 * nrow + 1.0))
    panel_h_in = max((figsize[1] - 1.0) / max(nrow, 1), 0.5)

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )

    # Cohort -> colour: YlGnBu gradient over the global cohort ordering, so
    # the same cohort keeps its colour across facets (R uses a date gradient).
    cohorts = sorted({c for c in df["cohort"].to_list()})
    n_coh = len(cohorts)
    cmap = mpl.colormaps["YlGnBu"]
    lo, hi = 0.15, 0.92
    cpos = {c: lo + (hi - lo) * (i / max(n_coh - 1, 1))
            for i, c in enumerate(cohorts)}

    def _coh_color(c):
        return cmap(cpos[c])

    if is_ratio:
        hline: float | None = 1.0
    elif metric in _AMOUNT_METRICS:
        hline = 0.0
    else:
        hline = None

    scale = 100.0 if (is_ratio or is_prop) else (1.0 / amount_divisor)
    fmt = FuncFormatter(lambda v, _p, s=scale: f"{v * s:,.0f}")

    for idx, (group_value, sub) in enumerate(facets):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        _draw_cohort_lines(ax, sub, metric, _coh_color, summary,
                           summary_min_n, hline)
        ax.yaxis.set_major_formatter(fmt)
        if group_value is not None:
            strip_h = _STRIP_HEIGHT_IN / panel_h_in
            ax.set_title(" ", pad=_STRIP_HEIGHT_IN * 72.0)
            ax.add_patch(Rectangle(
                (0.0, 1.0), 1.0, strip_h, transform=ax.transAxes,
                facecolor=_STRIP_FILL, edgecolor=_STRIP_EDGE, linewidth=0.5,
                clip_on=False, zorder=3))
            ax.text(0.5, 1.0 + strip_h / 2.0, format_group_value(group_value),
                    transform=ax.transAxes, ha="center", va="center",
                    fontsize=8.5, color=_STRIP_TEXT, clip_on=False, zorder=4)

    _hide_unused(axes, n_facets, nrow, ncol)

    fig.suptitle(meta.title, fontsize=13, fontweight="normal", x=0.01,
                 ha="left")
    fig.supxlabel(_pretty_var_label(dev), fontsize=11)
    fig.supylabel(metric, fontsize=11)
    if meta.caption:
        fig.text(0.99, 0.005, meta.caption, ha="right", va="bottom",
                 fontsize=8.5, color=_CAPTION_COLOR)

    vis_axes = [axes[divmod(i, ncol)[0]][divmod(i, ncol)[1]]
                for i in range(n_facets)]
    if summary:
        handles = [
            plt.Line2D([], [], color="black", label="Mean"),
            plt.Line2D([], [], color="#1f77b4", label="Median"),
            plt.Line2D([], [], color="#d62728", label="Weighted"),
        ]
        fig.legend(handles=handles, loc="upper right", fontsize=8,
                   frameon=False)
    elif n_coh > 1:
        # Raw-mode cohort colour bar.
        lc = ListedColormap([_coh_color(c) for c in cohorts])
        sm = ScalarMappable(norm=Normalize(vmin=0, vmax=n_coh), cmap=lc)
        cbar = fig.colorbar(sm, ax=vis_axes, fraction=0.025, pad=0.01)
        coh_type = _get_period_type(coh, grain=grain)
        ticks = list(range(0, n_coh, max(1, n_coh // 6)))
        cbar.set_ticks([t + 0.5 for t in ticks])
        if coh_type:
            lbls = _format_period_series(
                pl.Series([cohorts[t] for t in ticks]), coh_type)
        else:
            lbls = [str(cohorts[t]) for t in ticks]
        cbar.set_ticklabels(lbls)
        cbar.ax.tick_params(labelsize=7)
        cbar.ax.set_title("cohort", fontsize=8)

    return fig


__all__ = ["plot_triangle", "plot"]
