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

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from .._kernels.io import (
    _iter_group_frames,
    group_eq,
    normalize_groups,
    set_group_values,
)
from .base import open_facets
from .theme import STAT_COLORS_HUE, finalize_figure

if TYPE_CHECKING:
    from ..core.link import Link


_VALID_ATA_TYPES = ("cv", "rse", "summary", "box", "point")
_VALID_ED_TYPES = ("summary", "box", "point")
_SUMMARY_STATS = ("mean", "median", "weighted")


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
    recent: int | None = None,
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
        return _plot_link_ata(link, recent=recent, **kwargs)
    return _plot_link_intensity(link, recent=recent, **kwargs)


def _filter_cells_recent(
    cells: pl.DataFrame, groups: "str | list[str] | None", recent: int | None
) -> pl.DataFrame:
    """Restrict link cells to the recent calendar-diagonal wedge, per group.

    Mirrors :func:`lossratio._kernels.recent.recent_link_mask` at the cell level so a
    diagnostic built with ``recent=N`` plots the same recent window it
    summarised: ``cal_idx = (0-based per-group cohort rank) + duration_from``,
    keep ``cal_idx > max(cal_idx) - recent`` within each group.
    """
    if recent is None:
        return cells
    group_cols = normalize_groups(groups)
    rank = pl.col("cohort").rank("dense")
    rank = rank.over(group_cols) if group_cols else rank
    cal = rank.cast(pl.Int64) - 1 + pl.col("duration_from")
    c = cells.with_columns(cal.alias("_cal"))
    mx = pl.col("_cal").max()
    mx = mx.over(group_cols) if group_cols else mx
    return c.filter(pl.col("_cal") > mx - recent).drop("_cal")


def _plot_shared_kind(
    kind: str,
    *,
    cells: pl.DataFrame,
    summary: pl.DataFrame,
    groups: "str | list[str] | None",
    y_col: str,
    y_label: str,
    title_noun: str,
    hline: float,
    factor_stability: "pl.DataFrame | None",
    nrow: int | None,
    ncol: int | None,
    figsize: "tuple[float, float] | None",
) -> Any:
    """Render the ``summary`` / ``box`` / ``point`` kinds shared by the ATA
    and intensity branches. Only the factor column, label, title noun, and
    reference line differ between the two -- everything else is identical, so
    the model-specific values arrive as arguments.
    """
    if kind == "summary":
        return _plot_summary_lines(
            summary, groups=groups, value_cols=_SUMMARY_STATS,
            y_label=y_label, title=f"Summary of {title_noun}",
            hline=hline, factor_stability=factor_stability,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )
    box = kind == "box"
    return _plot_per_link_distribution(
        cells, groups=groups, y_col=y_col, kind=kind, y_label=y_label,
        title=f"{'Box Plot' if box else 'Distribution'} of {title_noun}",
        hline=hline, factor_stability=factor_stability,
        nrow=nrow, ncol=ncol, figsize=figsize,
    )


def _plot_link_ata(
    link: Link,
    kind: str = "cv",
    show_factor_stability: bool = True,
    max_cv: float = 0.05,
    max_rse: float = 0.05,
    min_run: int = 1,
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
    recent: int | None = None,
) -> Any:
    """ATA-mode link diagnostic plot -- 5 kind variants."""
    if kind not in _VALID_ATA_TYPES:
        raise ValueError(
            f"`kind` must be one of {_VALID_ATA_TYPES!r}; got {kind!r}."
        )

    groups = link._groups
    cells = _filter_cells_recent(link._df, groups, recent)
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
    # summary / box / point share one renderer with the intensity branch.
    return _plot_shared_kind(
        kind, cells=cells, summary=summary, groups=groups,
        y_col="ata", y_label="factor", title_noun="ATA Factors",
        hline=1.0, factor_stability=factor_stability,
        nrow=nrow, ncol=ncol, figsize=figsize,
    )


def _plot_link_intensity(
    link: Link,
    kind: str = "summary",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
    recent: int | None = None,
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
    cells = _filter_cells_recent(link._df, groups, recent)
    summary = _intensity_summary(cells, groups)

    return _plot_shared_kind(
        kind, cells=cells, summary=summary, groups=groups,
        y_col="intensity", y_label="intensity",
        title_noun="Incremental Loss Intensity (g)",
        hline=0.0, factor_stability=None,
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
    grid = open_facets(
        _iter_group_frames(summary, groups),
        nrow=nrow, ncol=ncol, figsize=figsize,
        figsize_fn=lambda nr, nc: (max(4.5, 3.2 * nc), max(3.0, 2.6 * nr)),
    )
    for idx, group_value, sub, ax in grid:
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
        grid.title(ax, group_value)
        _style_ggplot(ax)
    grid.hide_unused()
    finalize_figure(grid.fig, title=title, xlabel="duration link",
                    ylabel=y_label)
    return grid.fig


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
    grid = open_facets(
        _iter_group_frames(summary, groups),
        nrow=nrow, ncol=ncol, figsize=figsize,
        figsize_fn=lambda nr, nc: (max(4.5, 3.2 * nc), max(3.0, 2.6 * nr)),
    )
    for idx, group_value, sub, ax in grid:
        sub_sorted = sub.sort("duration_from")
        x = sub_sorted["duration_from"].to_numpy()
        link_labels = _link_label_lookup(sub_sorted)
        for stat in value_cols:
            y = sub_sorted[stat].to_numpy()
            m = np.isfinite(y)
            if m.any():
                ax.plot(
                    x[m], y[m],
                    color=STAT_COLORS_HUE.get(stat, "C0"),
                    linewidth=1.0,
                    marker="o", markersize=3.2, markeredgewidth=0,
                    label=stat,
                )
        if hline is not None:
            ax.axhline(hline, color="red", linestyle="--", linewidth=0.8)
        _apply_factor_stability_overlay(ax, factor_stability, group_value, groups, y_max=None)
        _set_link_xticks(ax, x, link_labels)
        grid.title(ax, group_value)
        ax.legend(loc="best", fontsize=8, frameon=False)
        _style_ggplot(ax)
    grid.hide_unused()
    finalize_figure(grid.fig, title=title, xlabel="duration link",
                    ylabel=y_label)
    return grid.fig


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
    grid = open_facets(
        _iter_group_frames(cells, groups),
        nrow=nrow, ncol=ncol, figsize=figsize,
        figsize_fn=lambda nr, nc: (max(4.5, 3.2 * nc), max(3.0, 2.6 * nr)),
    )
    for idx, group_value, sub, ax in grid:
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
        grid.title(ax, group_value)
        _style_ggplot(ax)
    grid.hide_unused()
    finalize_figure(grid.fig, title=title, xlabel="duration link",
                    ylabel=y_label)
    return grid.fig


# ---------------------------------------------------------------------------
# Faceting + axis helpers
# ---------------------------------------------------------------------------


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
