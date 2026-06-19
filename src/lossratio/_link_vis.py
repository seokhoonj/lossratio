"""Link-factor diagnostic visualisation -- matplotlib backend.

Implements ``Link.plot()`` / ``ATA.plot()`` / ``Intensity.plot()``,
dispatching to two internal branches: ``_plot_link_ata`` (5 kinds:
``cv`` / ``rse`` / ``summary`` / ``box`` / ``point``) and
``_plot_link_intensity`` (3 kinds: ``summary`` / ``box`` / ``point``).

Per-link summaries (``mean`` / ``median`` / weighted) are derived from
the Link's per-cell ``ata`` / ``intensity`` column on the fly --
``ATA`` / ``Intensity`` only store the pooled factor diagnostic.
"""

from __future__ import annotations

import math
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import (
    _iter_group_frames,
    format_group_value,
    group_eq,
    normalize_groups,
    set_group_values,
)

if TYPE_CHECKING:
    from .link import Link


_VALID_ATA_TYPES = ("cv", "rse", "summary", "box", "point")
_VALID_ED_TYPES = ("summary", "box", "point")
_SUMMARY_STATS = ("mean", "median", "weighted")
_STAT_COLORS = {
    "mean":     "#F8766D",  # ggplot hue palette (salmon / green / blue)
    "median":   "#00BA38",
    "weighted": "#619CFF",
}


def _style_ggplot(ax: Any) -> None:
    """Give an Axes a clean ggplot-like (theme_minimal) look.

    Drops the top/right box, softens the remaining spines, lays a light
    grey grid behind the data, and removes the heavy tick marks -- so the
    matplotlib default reads closer to the R/ggplot diagnostics.
    """
    ax.set_facecolor("white")
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    for side in ("left", "bottom"):
        ax.spines[side].set_color("#c9c9c9")
        ax.spines[side].set_linewidth(0.8)
    ax.grid(True, color="#ebebeb", linewidth=0.7)
    ax.set_axisbelow(True)
    ax.tick_params(length=0, labelsize=8, colors="#555555")


def plot_link(
    link: Link,
    model: str | None = None,
    **kwargs: Any,
) -> Any:
    """``Link.plot(model=...)`` dispatcher.

    ``model = "ata"`` -> :func:`_plot_link_ata`,
    ``model = "intensity"`` -> :func:`_plot_link_intensity`.
    Default: ``"intensity"`` if the Link was built with ``exposure``,
    otherwise ``"ata"``.
    """
    if model is None:
        model = "intensity" if link._premium is not None else "ata"
    if model not in ("ata", "intensity"):
        raise ValueError(
            f"`model` must be 'ata' or 'intensity'; got {model!r}."
        )
    if model == "intensity" and link._premium is None:
        raise ValueError(
            "`model='intensity'` requires a Link built with `exposure`."
        )
    if model == "ata":
        return _plot_link_ata(link, **kwargs)
    return _plot_link_intensity(link, **kwargs)


def _plot_link_ata(
    link: Link,
    kind: str = "cv",
    alpha: float = 1.0,
    show_factor_stability: bool = True,
    max_cv: float = 0.05,
    max_rse: float = 0.05,
    min_run: int = 1,
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """ATA-mode link diagnostic plot -- 5 kind variants."""
    if kind not in _VALID_ATA_TYPES:
        raise ValueError(
            f"`kind` must be one of {_VALID_ATA_TYPES!r}; got {kind!r}."
        )

    groups = link._groups
    cells = link._df
    summary = _ata_summary(cells, groups)

    # Factor-stability overlay (per-group duration_from where CV / RSE drop
    # below thresholds and stay there for `min_run` consecutive links).
    factor_stability = _detect_factor_stability_overlay(
        summary, groups, max_cv=max_cv, max_rse=max_rse, min_run=min_run
    ) if show_factor_stability else None

    if kind == "cv":
        return _plot_per_link_scalar(
            summary,
            groups=groups,
            y_col="cv",
            y_label="CV",
            title="Coefficient of Variation of ATA Factors",
            hline=max_cv,
            factor_stability=factor_stability,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )
    if kind == "rse":
        return _plot_per_link_scalar(
            summary,
            groups=groups,
            y_col="rse",
            y_label="RSE",
            title="Relative Standard Error of ATA Factors",
            hline=max_rse,
            factor_stability=factor_stability,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )
    if kind == "summary":
        return _plot_summary_lines(
            summary,
            groups=groups,
            value_cols=_SUMMARY_STATS,
            y_label="factor",
            title="Summary of ATA Factors",
            hline=1.0,
            factor_stability=factor_stability,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )
    if kind == "box":
        return _plot_per_link_distribution(
            cells,
            groups=groups,
            y_col="ata",
            kind="box",
            y_label="factor",
            title="Box Plot of ATA Factors",
            hline=1.0,
            factor_stability=factor_stability,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )
    # point
    return _plot_per_link_distribution(
        cells,
        groups=groups,
        y_col="ata",
        kind="point",
        y_label="factor",
        title="Distribution of ATA Factors",
        hline=1.0,
        factor_stability=factor_stability,
        nrow=nrow, ncol=ncol, figsize=figsize,
    )


def _plot_link_intensity(
    link: Link,
    kind: str = "summary",
    alpha: float = 1.0,
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """Intensity-mode link diagnostic plot -- 3 kind variants."""
    if kind not in _VALID_ED_TYPES:
        raise ValueError(
            f"`kind` must be one of {_VALID_ED_TYPES!r}; got {kind!r}."
        )
    if link._premium is None:
        raise ValueError(
            "Intensity-mode plot requires a Link built with `exposure`."
        )

    groups = link._groups
    cells = link._df
    summary = _intensity_summary(cells, groups)

    if kind == "summary":
        return _plot_summary_lines(
            summary,
            groups=groups,
            value_cols=_SUMMARY_STATS,
            y_label="intensity",
            title="Summary of Incremental Loss Intensity (g)",
            hline=0.0,
            factor_stability=None,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )
    if kind == "box":
        return _plot_per_link_distribution(
            cells,
            groups=groups,
            y_col="intensity",
            kind="box",
            y_label="intensity",
            title="Box Plot of Incremental Loss Intensity (g)",
            hline=0.0,
            factor_stability=None,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )
    return _plot_per_link_distribution(
        cells,
        groups=groups,
        y_col="intensity",
        kind="point",
        y_label="intensity",
        title="Distribution of Incremental Loss Intensity (g)",
        hline=0.0,
        factor_stability=None,
        nrow=nrow, ncol=ncol, figsize=figsize,
    )


# ---------------------------------------------------------------------------
# Per-link summary computation (mean / median / weighted f / rse)
# ---------------------------------------------------------------------------


def _ata_summary(
    cells: pl.DataFrame, groups: str | list[str] | None
) -> pl.DataFrame:
    """Per-link ATA summary -- mean, median, weighted f, CV, RSE.

    Pooled weighted factor is the volume-weighted (alpha=1) estimator
    (``sum(loss_to) / sum(loss_from)``). CV is the cross-cohort
    coefficient of variation of the per-cell ``ata`` values; RSE is
    ``SE(f) / f`` derived from the cross-cohort variance.
    """
    by = [*normalize_groups(groups), "duration_from", "duration_to"]
    valid = cells.filter(
        pl.col("ata").is_finite() & pl.col("loss_from").is_finite()
    )
    agg = (
        valid.group_by(by, maintain_order=True)
        .agg(
            pl.col("ata").mean().alias("mean"),
            pl.col("ata").median().alias("median"),
            (pl.col("loss_to").sum() / pl.col("loss_from").sum())
            .alias("weighted"),
            pl.col("loss_from").sum().alias("_loss_from_sum"),
            pl.col("ata").std(ddof=1).alias("_sd"),
            pl.col("ata").count().alias("n"),
        )
    )
    # f = weighted (volume-weighted alpha=1).
    # CV = SE(ata cell-level) / mean; here we use the unweighted
    # cross-cohort SD divided by the unweighted mean. RSE = SE(f) / f
    # with SE(f) = sd / sqrt(n).
    agg = agg.with_columns(
        pl.col("weighted").alias("f"),
        pl.when(pl.col("mean").is_not_null() & (pl.col("mean") != 0.0))
        .then(pl.col("_sd") / pl.col("mean").abs())
        .otherwise(None)
        .alias("cv"),
        pl.when(
            (pl.col("n") > 0)
            & pl.col("weighted").is_not_null()
            & (pl.col("weighted") != 0.0)
            & pl.col("_sd").is_not_null()
        )
        .then(
            (pl.col("_sd") / pl.col("n").sqrt())
            / pl.col("weighted").abs()
        )
        .otherwise(None)
        .alias("rse"),
    ).drop(["_loss_from_sum", "_sd", "n"])
    return agg.sort(by)


def _intensity_summary(
    cells: pl.DataFrame, groups: str | list[str] | None
) -> pl.DataFrame:
    """Per-link intensity summary -- mean, median, weighted g."""
    by = [*normalize_groups(groups), "duration_from", "duration_to"]
    valid = cells.filter(
        pl.col("intensity").is_finite() & pl.col("premium_from").is_finite()
    )
    agg = (
        valid.group_by(by, maintain_order=True)
        .agg(
            pl.col("intensity").mean().alias("mean"),
            pl.col("intensity").median().alias("median"),
            (pl.col("loss_delta").sum() / pl.col("premium_from").sum())
            .alias("weighted"),
        )
    )
    return agg.sort(by)


def _detect_factor_stability_overlay(
    summary: pl.DataFrame,
    groups: str | list[str] | None,
    *,
    max_cv: float,
    max_rse: float,
    min_run: int,
) -> pl.DataFrame:
    """Locate the first duration_from where CV<max_cv AND RSE<max_rse for
    ``min_run`` consecutive links (per group). Returns one row per
    group, with columns ``[groups?, duration_from, duration_to, cv, rse]``.
    """
    out_rows: list[dict[str, Any]] = []
    for value, sub in _iter_group_frames(summary, groups):
        sub = sub.sort("duration_from")
        cv = sub["cv"].to_numpy()
        rse = sub["rse"].to_numpy()
        duration_from = sub["duration_from"].to_numpy()
        duration_to = sub["duration_to"].to_numpy()
        stable = (
            np.isfinite(cv) & np.isfinite(rse)
            & (cv < max_cv) & (rse < max_rse)
        )
        run = 0
        hit_idx: int | None = None
        for i, ok in enumerate(stable):
            run = run + 1 if ok else 0
            if run >= min_run:
                hit_idx = i - run + 1
                break
        if hit_idx is None:
            continue
        row: dict[str, Any] = {}
        set_group_values(row, groups, value)
        row["duration_from"] = int(duration_from[hit_idx])
        row["duration_to"] = int(duration_to[hit_idx])
        row["cv"] = float(cv[hit_idx])
        row["rse"] = float(rse[hit_idx])
        out_rows.append(row)
    return pl.DataFrame(out_rows) if out_rows else pl.DataFrame()


# ---------------------------------------------------------------------------
# Per-kind renderers
# ---------------------------------------------------------------------------


def _plot_per_link_scalar(
    summary: pl.DataFrame,
    *,
    groups: str | list[str] | None,
    y_col: str,
    y_label: str,
    title: str,
    hline: float | None,
    factor_stability: pl.DataFrame | None,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
) -> Any:
    """Per-link line+point plot of a single scalar column (cv / rse)."""
    facets = _resolve_facets(summary, groups)
    fig, axes_grid = _create_facet_grid(
        facets, nrow=nrow, ncol=ncol, figsize=figsize
    )
    for idx, (group_value, sub) in enumerate(facets):
        ax = axes_grid[idx]
        sub_sorted = sub.sort("duration_from")
        x = sub_sorted["duration_from"].to_numpy()
        y = sub_sorted[y_col].to_numpy()
        link_labels = _link_label_lookup(sub_sorted)
        m = np.isfinite(y)
        if m.any():
            ax.plot(
                x[m], y[m], color="C0", linewidth=1.0,
                marker="o", markersize=3.2, markeredgewidth=0,
            )
        if hline is not None:
            ax.axhline(hline, color="red", linestyle="--", linewidth=0.8)
        # Set xticks (which finalises xlim) BEFORE the overlay so its shaded
        # band tracks the panel's true right edge, not a pre-xtick xlim.
        _set_link_xticks(ax, x, link_labels)
        _apply_factor_stability_overlay(ax, factor_stability, group_value, groups, y_max=hline)
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=9)
        _style_ggplot(ax)
    _finalize_facet_grid(
        fig, axes_grid, n_used=len(facets),
        title=title, x_label="duration link", y_label=y_label,
    )
    return fig


def _plot_summary_lines(
    summary: pl.DataFrame,
    *,
    groups: str | list[str] | None,
    value_cols: tuple[str, ...],
    y_label: str,
    title: str,
    hline: float | None,
    factor_stability: pl.DataFrame | None,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
) -> Any:
    """Per-link summary: mean / median / weighted lines."""
    facets = _resolve_facets(summary, groups)
    fig, axes_grid = _create_facet_grid(
        facets, nrow=nrow, ncol=ncol, figsize=figsize
    )
    for idx, (group_value, sub) in enumerate(facets):
        ax = axes_grid[idx]
        sub_sorted = sub.sort("duration_from")
        x = sub_sorted["duration_from"].to_numpy()
        link_labels = _link_label_lookup(sub_sorted)
        for stat in value_cols:
            y = sub_sorted[stat].to_numpy()
            m = np.isfinite(y)
            if m.any():
                ax.plot(
                    x[m], y[m],
                    color=_STAT_COLORS.get(stat, "C0"),
                    linewidth=1.0,
                    marker="o", markersize=3.2, markeredgewidth=0,
                    label=stat,
                )
        if hline is not None:
            ax.axhline(hline, color="red", linestyle="--", linewidth=0.8)
        _apply_factor_stability_overlay(ax, factor_stability, group_value, groups, y_max=None)
        _set_link_xticks(ax, x, link_labels)
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=9)
        ax.legend(loc="best", fontsize=8, frameon=False)
        _style_ggplot(ax)
    _finalize_facet_grid(
        fig, axes_grid, n_used=len(facets),
        title=title, x_label="duration link", y_label=y_label,
    )
    return fig


def _plot_per_link_distribution(
    cells: pl.DataFrame,
    *,
    groups: str | list[str] | None,
    y_col: str,
    kind: str,
    y_label: str,
    title: str,
    hline: float | None,
    factor_stability: pl.DataFrame | None,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
) -> Any:
    """Per-link box-plot or scatter of a cell-level column (ata / intensity)."""
    facets = _resolve_facets(cells, groups)
    fig, axes_grid = _create_facet_grid(
        facets, nrow=nrow, ncol=ncol, figsize=figsize
    )
    for idx, (group_value, sub) in enumerate(facets):
        ax = axes_grid[idx]
        sub_valid = sub.filter(pl.col(y_col).is_finite())
        if sub_valid.height == 0:
            ax.text(0.5, 0.5, "(no data)", ha="center", va="center",
                    transform=ax.transAxes)
            continue
        link_keys = (
            sub_valid.select(["duration_from", "duration_to"])
            .unique(maintain_order=True)
            .sort("duration_from")
        )
        duration_from_vals = link_keys["duration_from"].to_numpy()
        link_labels = _link_label_lookup(link_keys)

        if kind == "box":
            datasets = []
            for k in duration_from_vals:
                arr = (
                    sub_valid.filter(pl.col("duration_from") == k)[y_col]
                    .to_numpy()
                )
                datasets.append(arr[np.isfinite(arr)])
            ax.boxplot(
                datasets,
                positions=duration_from_vals,
                widths=0.6,
                showfliers=True,
            )
        else:  # point
            xs = sub_valid["duration_from"].to_numpy()
            ys = sub_valid[y_col].to_numpy()
            ax.scatter(xs, ys, s=14, color="C0", alpha=0.7)
            # mean-line overlay
            means = (
                sub_valid.group_by("duration_from", maintain_order=True)
                .agg(pl.col(y_col).mean().alias("_mean"))
                .sort("duration_from")
            )
            ax.plot(
                means["duration_from"].to_numpy(),
                means["_mean"].to_numpy(),
                color="C0", linewidth=1.0,
            )

        if hline is not None:
            ax.axhline(hline, color="red", linestyle="--", linewidth=0.8)
        _apply_factor_stability_overlay(ax, factor_stability, group_value, groups, y_max=None)
        _set_link_xticks(ax, duration_from_vals, link_labels)
        if group_value is not None:
            ax.set_title(format_group_value(group_value), fontsize=9)
        _style_ggplot(ax)
    _finalize_facet_grid(
        fig, axes_grid, n_used=len(facets),
        title=title, x_label="duration link", y_label=y_label,
    )
    return fig


# ---------------------------------------------------------------------------
# Faceting + axis helpers
# ---------------------------------------------------------------------------


def _resolve_facets(
    df: pl.DataFrame, groups: str | list[str] | None
) -> list[tuple[Any, pl.DataFrame]]:
    return list(_iter_group_frames(df, groups))


def _create_facet_grid(
    facets: list[tuple[Any, pl.DataFrame]],
    *,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
):
    import matplotlib.pyplot as plt

    n = len(facets)
    if n == 0:
        fig, ax = plt.subplots(1, 1, figsize=figsize or (5.0, 3.5))
        return fig, [ax]
    if nrow is None and ncol is None:
        ncol = min(n, 3)
        nrow = math.ceil(n / ncol)
    elif ncol is None:
        ncol = math.ceil(n / max(nrow, 1))
    elif nrow is None:
        nrow = math.ceil(n / max(ncol, 1))
    if figsize is None:
        figsize = (max(4.5, 3.2 * ncol), max(3.0, 2.6 * nrow))
    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )
    flat = [axes[r][c] for r in range(nrow) for c in range(ncol)]
    return fig, flat


def _finalize_facet_grid(
    fig, axes_grid: list, *, n_used: int, title: str,
    x_label: str, y_label: str,
) -> None:
    for ax in axes_grid[n_used:]:
        ax.set_visible(False)
    fig.suptitle(title, fontsize=12, fontweight="normal")
    fig.supxlabel(x_label, fontsize=10)
    fig.supylabel(y_label, fontsize=10)


def _link_label_lookup(df: pl.DataFrame) -> dict[int, str]:
    """Build ``{duration_from: 'k-(k+1)'}`` lookup from a sorted slice."""
    lookup: dict[int, str] = {}
    for duration_from, duration_to in df.select(["duration_from", "duration_to"]).iter_rows():
        lookup[int(duration_from)] = f"{int(duration_from)}-{int(duration_to)}"
    return lookup


def _set_link_xticks(
    ax, xs: np.ndarray, link_labels: dict[int, str]
) -> None:
    keys = sorted(set(int(v) for v in xs if np.isfinite(v)))
    ax.set_xticks(keys)
    ax.set_xticklabels([link_labels.get(k, str(k)) for k in keys],
                       rotation=90, fontsize=8)


def _apply_factor_stability_overlay(
    ax,
    factor_stability: pl.DataFrame | None,
    group_value: Any,
    groups: str | list[str] | None,
    y_max: float | None,
) -> None:
    if factor_stability is None or factor_stability.height == 0:
        return
    if groups is not None:
        sub = factor_stability.filter(group_eq(groups, group_value))
    else:
        sub = factor_stability
    if sub.height == 0:
        return
    duration_from = int(sub["duration_from"][0])
    ax.axvline(duration_from, color="grey", linestyle=(0, (5, 4)), linewidth=0.8)
    # Shaded band from the stability point onwards. On the cv / rse axes
    # (``y_max`` is the threshold line) shade only the sub-threshold region
    # [0, y_max]; the factor-value axes (summary / box / point) pass
    # ``y_max=None`` and keep the vline + annotation only -- no full-height flood.
    if y_max is not None and y_max > 0:
        # Span to the current right edge. Callers set the xticks first, so xlim
        # already reaches the last duration (not just the last non-null link).
        xlim = ax.get_xlim()
        ax.fill_between(
            [duration_from, xlim[1]], 0.0, y_max,
            facecolor="#AED6F1", alpha=0.25, zorder=0,
        )
    # annotation
    cv = sub["cv"][0] if "cv" in sub.columns else None
    rse = sub["rse"][0] if "rse" in sub.columns else None
    duration_to = int(sub["duration_to"][0])
    parts = [f"factor stable: {duration_from}-{duration_to}"]
    if cv is not None:
        parts.append(f"cv: {cv:.3f}")
    if rse is not None:
        parts.append(f"rse: {rse:.3f}")
    ax.text(
        0.98, 0.98, "\n".join(parts),
        ha="right", va="top",
        transform=ax.transAxes, fontsize=7,
        bbox=dict(boxstyle="round,pad=0.3", facecolor="white",
                  alpha=0.8, edgecolor="grey"),
    )
