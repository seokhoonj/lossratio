"""Triangle visualisation -- matplotlib backend.

Implements ``Triangle.plot_triangle(view="value" | "usage")``. Mirrors
the R sibling's ``plot_triangle.Triangle`` (``R/triangle-vis.R``).
"""

from __future__ import annotations

import math
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._plot import (
    _AMOUNT_METRICS,
    _PROP_METRICS,
    _RATIO_METRICS,
    _VALID_METRICS,
    _auto_divisor,
    _cohort_label,
    _format_period_series,
    _get_period_type,
    _pretty_var_label,
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
    view: str = "value",
    metric: str = "ratio",
    label_style: str = "value",
    label_size: float | None = None,
    amount_divisor: float | str = "auto",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
    *,
    recent: int | None = None,
    regime: Any = None,
    holdout: int | None = None,
    maturity: Any = None,
) -> Any:
    """Triangle heatmap dispatcher. See
    :meth:`lossratio.Triangle.plot_triangle` for the public docs.
    """
    import matplotlib.pyplot as plt

    if view not in ("value", "usage"):
        raise ValueError(f"`view` must be 'value' or 'usage', got {view!r}.")

    if view == "usage":
        return _plot_triangle_usage(
            triangle,
            recent=recent,
            regime=regime,
            holdout=holdout,
            maturity=maturity,
            nrow=nrow,
            ncol=ncol,
            figsize=figsize,
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

    # Determine cell labels (cohort -> string, dev -> string) once,
    # using consistent ordering across facets so the axes are stable.
    coh_type = _get_period_type(coh, grain=grain)
    dev_type = _get_period_type(dev)  # dev_m / dev_q / ... aren't dates

    cohort_labels = _format_axis(df["cohort"], coh_type)
    dev_labels = _format_axis(df["dev"], dev_type)

    # Ordered unique levels.
    coh_pairs = sorted(
        set(zip(df["cohort"].to_list(), cohort_labels)),
        key=lambda p: p[0],
    )
    dev_pairs = sorted(
        set(zip(df["dev"].to_list(), dev_labels)),
        key=lambda p: p[0],
    )
    y_levels = [lbl for _, lbl in coh_pairs]   # cohort, oldest -> newest
    x_levels = [lbl for _, lbl in dev_pairs]   # dev, smallest -> largest

    df = df.with_columns(
        pl.Series(name="_y_lbl", values=cohort_labels),
        pl.Series(name="_x_lbl", values=dev_labels),
        pl.Series(name="_label", values=_cell_labels(df, metric, label_style, amount_divisor)),
    )

    # Faceting setup.
    if grp is None:
        facets = [(None, df)]
    else:
        groups_in_order: list = []
        seen = set()
        for g in df[grp].to_list():
            if g not in seen:
                seen.add(g)
                groups_in_order.append(g)
        facets = [(g, df.filter(pl.col(grp) == g)) for g in groups_in_order]

    n_facets = len(facets)
    if nrow is None and ncol is None:
        ncol = min(n_facets, 3)
        nrow = math.ceil(n_facets / ncol)
    elif ncol is None:
        ncol = math.ceil(n_facets / max(nrow, 1))
    elif nrow is None:
        nrow = math.ceil(n_facets / max(ncol, 1))

    if figsize is None:
        cell_w = 0.45 + (0.18 if label_style == "detail" else 0.0)
        cell_h = 0.30 + (0.18 if label_style == "detail" else 0.0)
        fig_w = max(4.0, cell_w * len(x_levels) * ncol + 1.2)
        fig_h = max(3.0, cell_h * len(y_levels) * nrow + 1.5)
        figsize = (fig_w, fig_h)

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
        title = str(group_value) if group_value is not None else ""
        if title:
            ax.set_title(title, fontsize=10)

    # Hide unused axes.
    for idx in range(n_facets, nrow * ncol):
        r, c = divmod(idx, ncol)
        axes[r][c].set_visible(False)

    fig.suptitle(meta.title, fontsize=12, fontweight="bold")
    fig.supxlabel(_pretty_var_label(dev), fontsize=10)
    fig.supylabel(_cohort_label(coh, grain=grain), fontsize=10)
    if meta.caption:
        fig.text(0.99, 0.005, meta.caption, ha="right", va="bottom", fontsize=8)

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
    # Cohort axis: oldest at top, so invert (newer rows at the bottom
    # but plotting tradition for triangles puts the newest cohort at
    # the bottom of the y-axis when y grows upward). Mirror R's
    # `geom_tile` default with oldest at the bottom.
    ax.set_ylim(-0.5, ny - 0.5)

    ax.set_xticks(range(nx))
    ax.set_xticklabels(x_levels, rotation=0, fontsize=8)
    ax.set_yticks(range(ny))
    ax.set_yticklabels(y_levels, fontsize=8)

    # Panel border: vertical + horizontal grid lines on cell edges.
    for k in range(nx + 1):
        ax.axvline(k - 0.5, color=_BORDER_COLOR, linewidth=_BORDER_WIDTH)
    for k in range(ny + 1):
        ax.axhline(k - 0.5, color=_BORDER_COLOR, linewidth=_BORDER_WIDTH)

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
            except Exception:
                return None
            return _resolve_maturity_k(mat, triangle=triangle)
        raise ValueError(
            f"`maturity` must be None, an int, 'auto', a Maturity "
            f"instance, or a callable; got {maturity!r}."
        )
    if isinstance(maturity, Maturity):
        k = maturity.mat_k
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
            except Exception:
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
    group_col: str | None,
    group_value: Any,
) -> pl.DataFrame:
    """Compute per-cell ``dev_min`` for the segment_wise mini-triangle.

    For each cell in ``grp_rows``, classify its cohort into a segment
    via ``np.searchsorted(cd_vec, cohort, side='right')`` (i.e. the
    R ``findInterval`` semantic + 1). For each segment, the last
    cohort rank inside the segment sets ``dev_min = _max_cal -
    seg_last_rank + 1``. The returned frame has columns ``[group_col?,
    cohort, dev, _seg_dev_min]`` that the caller joins back onto the
    expanded grid to override ``is_fit_data``.
    """
    cohorts = grp_rows["cohort"].to_numpy()
    cd_arr = np.array(cd_vec, dtype="datetime64[D]")
    coh_arr = np.array(cohorts, dtype="datetime64[D]")
    # `findInterval(coh, cd) + 1` in R == np.searchsorted(cd, coh,
    # side='right') + 1, mapping each cohort to a 1-indexed segment.
    seg_id = np.searchsorted(cd_arr, coh_arr, side="right") + 1

    work = grp_rows.with_columns(pl.Series("_seg_id", seg_id))
    # seg_last_rank per segment = max(_coh_rank within that segment)
    seg_last = (
        work.group_by("_seg_id")
        .agg(pl.col("_coh_rank").max().alias("_seg_last_rank"))
    )
    work = work.join(seg_last, on="_seg_id", how="left").with_columns(
        (pl.col("_max_cal") - pl.col("_seg_last_rank") + 1).alias(
            "_seg_dev_min"
        )
    )
    keep_cols = (
        [group_col] if group_col is not None else []
    ) + ["cohort", "dev", "_seg_dev_min"]
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
    grp_cols: list[str] = [grp] if grp is not None else []

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

    if regime is not None:
        is_segment_wise = getattr(regime, "treatment", None) == "segment_wise"
        if not is_segment_wise:
            from .regime import _regime_cutoff_map
            cutoff_map = _regime_cutoff_map(regime)
            if cutoff_map is not None:
                if "_cutoff" in cutoff_map.columns and cutoff_map.height == 1 and (
                    regime.groups is None or regime.groups not in cutoff_map.columns
                ):
                    cd_scalar = cutoff_map["_cutoff"][0]
                else:
                    cd_df = cutoff_map.rename({"_cutoff": "_cd_join"})

    # 7. Build the pass_filter boolean.
    has_recent = recent is not None
    has_change = cd_scalar is not None or cd_df is not None

    if cd_df is not None:
        # broadcast per-group change date onto expanded rows
        expanded = expanded.join(cd_df, on=regime.groups, how="left")
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
            if reg_groups is not None and reg_groups in expanded.columns:
                # per-group segments
                gb_iter = bp.sort([reg_groups, "change"])
                affected_groups = (
                    gb_iter.select(reg_groups).unique(maintain_order=True)
                )
                dev_min_parts: list[pl.DataFrame] = []
                for g_row in affected_groups.iter_rows(named=True):
                    g_val = g_row[reg_groups]
                    cd_vec = (
                        bp.filter(pl.col(reg_groups) == g_val)
                        .sort("change")["change"]
                        .to_list()
                    )
                    grp_rows = expanded.filter(pl.col(reg_groups) == g_val)
                    if grp_rows.height == 0 or not cd_vec:
                        continue
                    dev_min_parts.append(
                        _seg_dev_min(grp_rows, cd_vec, reg_groups, g_val)
                    )
                if dev_min_parts:
                    dev_min_df = pl.concat(dev_min_parts)
                    expanded = expanded.join(
                        dev_min_df,
                        on=[reg_groups, "cohort", "dev"],
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
                    dev_min_df = _seg_dev_min(expanded, cd_vec, None, None)
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
) -> Any:
    """Categorical status heatmap; see ``plot_triangle.Triangle(view="usage")``."""
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
    x_levels = sorted(set(usage_df["dev"].to_list()))
    x_labels = [str(d) for d in x_levels]

    if grp is None:
        facets = [(None, usage_df)]
    else:
        seen: set = set()
        groups_in_order: list = []
        for g in usage_df[grp].to_list():
            if g not in seen:
                seen.add(g)
                groups_in_order.append(g)
        facets = [
            (g, usage_df.filter(pl.col(grp) == g)) for g in groups_in_order
        ]

    n_facets = len(facets)
    if nrow is None and ncol is None:
        ncol = min(n_facets, 3)
        nrow = math.ceil(n_facets / ncol)
    elif ncol is None:
        ncol = math.ceil(n_facets / max(nrow, 1))
    elif nrow is None:
        nrow = math.ceil(n_facets / max(ncol, 1))

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
        and getattr(regime_obj, "treatment", None) == "segment_wise"
    )
    if regime_obj is not None and not is_segment_wise:
        from .regime import _regime_cutoff_map
        cm = _regime_cutoff_map(regime_obj)
        if cm is not None:
            if regime_obj.groups is not None and regime_obj.groups in cm.columns:
                per_group_cd = {
                    row[regime_obj.groups]: row["_cutoff"]
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
            xi = x_idx[row["dev"]]
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

        # Maturity vline (dev = m_k - 0.5).
        if m_k is not None:
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
            ax.set_title(str(group_value), fontsize=10)

    for idx in range(n_facets, nrow * ncol):
        r, c = divmod(idx, ncol)
        axes[r][c].set_visible(False)

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

    fig.supxlabel(_pretty_var_label(dev), fontsize=10)
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


# Silence unused-import warnings for re-exports.
__all__ = ["plot_triangle"]
_unused = (_PROP_METRICS,)
