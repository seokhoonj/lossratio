"""Projection-curve visualisation -- matplotlib backend.

Implements ``RatioFit.plot()`` / ``LossFit.plot()`` / ``PremiumFit.plot()``.
Mirrors the R sibling's ``plot.RatioFit`` (``R/ratio-vis.R``) and the
shared ``.plot_projection_fit`` helper (``R/cl-vis.R``).

Per-cohort projection curves: solid observed line + bridge segment +
dashed projected line + optional analytical / bootstrap CI ribbon, one
panel per ``(group, cohort)`` pair (or one figure per group when
``per_group=True``).
"""

from __future__ import annotations

import math
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from ._plot import (
    _auto_divisor,
    _format_period_series,
    _get_period_type,
    _pretty_var_label,
)

if TYPE_CHECKING:
    from .loss import LossFit
    from .premium import PremiumFit
    from .ratio import RatioFit


_VALID_METRICS = ("ratio", "loss", "premium")
_VALID_CELL_TYPES = ("cumulative", "incremental")


def plot_ratio_fit(
    fit: RatioFit,
    metric: str = "ratio",
    cell_type: str = "cumulative",
    per_group: bool | None = None,
    conf_level: float | None = None,
    show_interval: bool = True,
    amount_divisor: float | str = "auto",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
) -> Any:
    """RatioFit projection-curve plot.

    See :meth:`lossratio.RatioFit.plot` for the public docs.
    """
    if metric not in _VALID_METRICS:
        raise ValueError(
            f"`metric` must be one of {_VALID_METRICS!r}; got {metric!r}."
        )
    if cell_type not in _VALID_CELL_TYPES:
        raise ValueError(
            f"`cell_type` must be one of {_VALID_CELL_TYPES!r}; "
            f"got {cell_type!r}."
        )

    df = fit._df
    groups = fit._groups
    coh = fit._cohort
    dev = fit._dev
    conf = float(conf_level) if conf_level is not None else float(fit.conf_level)
    ci_type = getattr(fit, "ci_type", "analytical")

    is_incr = cell_type == "incremental"
    is_ratio = metric == "ratio"
    col_key = f"incr_{metric}" if is_incr else metric
    val_col = f"{col_key}_proj"

    # CI columns exist only for cumulative metrics. Premium-side CI cols
    # are present only when the Ratio was fit with se_method="delta"; if
    # absent, the ribbon is silently skipped (R parity).
    ci_lo_col: str | None
    ci_hi_col: str | None
    if not is_incr:
        ci_lo_col = f"{metric}_ci_lo"
        ci_hi_col = f"{metric}_ci_hi"
        if ci_lo_col not in df.columns or ci_hi_col not in df.columns:
            ci_lo_col = ci_hi_col = None
    else:
        ci_lo_col = ci_hi_col = None

    # Resolve amount_divisor against the values that will be plotted.
    if isinstance(amount_divisor, str):
        if amount_divisor != "auto":
            raise ValueError(
                f"`amount_divisor` must be numeric or 'auto', got "
                f"{amount_divisor!r}."
            )
        div_vals = (
            np.array([], dtype=float)
            if is_ratio
            else df[val_col].to_numpy()
        )
        amount_divisor = _auto_divisor(div_vals)
    amount_divisor = float(amount_divisor)

    # Per-row plotting value `.y`:
    #   observed cells: loss_obs/premium_obs (ratio derived); incremental
    #     metrics reuse the `_proj` column (equals the observed incrs on
    #     observed cells by construction).
    #   projected cells: the `_proj` column.
    is_obs = df["loss_obs"].is_not_null()
    if is_incr:
        y_obs = df[val_col].to_numpy()
    elif metric == "loss":
        y_obs = df["loss_obs"].to_numpy()
    elif metric == "premium":
        y_obs = df["premium_obs"].to_numpy()
    else:  # cumulative ratio
        lo = df["loss_obs"].to_numpy()
        pr = df["premium_obs"].to_numpy()
        with np.errstate(divide="ignore", invalid="ignore"):
            y_obs = np.where(
                np.isfinite(pr) & (pr != 0.0), lo / pr, np.nan
            )

    y_full = np.where(
        is_obs.to_numpy(), y_obs, df[val_col].to_numpy()
    )

    work = df.with_columns(
        pl.Series(name="_y", values=y_full),
        is_obs.alias("_is_obs"),
    )

    # Per-row CI bounds (only for cumulative metrics with CI cols).
    if show_interval and ci_lo_col is not None:
        work = work.with_columns(
            pl.col(ci_lo_col).alias("_lo"),
            pl.col(ci_hi_col).alias("_hi"),
        )

    cum_word = "Per-Period" if is_incr else "Cumulative"
    base_lab = {"ratio": "Loss Ratio", "loss": "Loss", "premium": "Premium"}[metric]
    title_base = f"Projected {cum_word} {base_lab} (method: {fit.method})"
    hline = 1.0 if (is_ratio and not is_incr) else (0.0 if not is_ratio else None)
    y_axis_kind = "ratio" if is_ratio else "amount"

    # Pre-compute pretty cohort labels (same ordering across panels).
    grain = _resolve_grain(fit)
    coh_type = _get_period_type(coh, grain=grain)
    coh_labels = _cohort_label_series(work["cohort"], coh_type)
    work = work.with_columns(pl.Series(name="_coh_lbl", values=coh_labels))

    # Default per_group: True iff multi-group fit. Single-group returns
    # one Figure; multi-group returns list[Figure] (R parity).
    if per_group is None:
        per_group = bool(
            groups is not None
            and work[groups].n_unique() > 1
        )

    caption = (
        f"Interval: {round(conf * 100)}% ({ci_type})"
        if (show_interval and ci_lo_col is not None)
        else None
    )

    x_label = _pretty_var_label(dev)
    y_label = "ratio" if is_ratio else _get_role_label(fit, metric)

    if per_group and groups is not None:
        out_figs = []
        for gv in work[groups].unique(maintain_order=True).to_list():
            sub = work.filter(pl.col(groups) == gv)
            fig = _render_facets(
                sub,
                facet_keys=["_coh_lbl"],
                show_interval=show_interval and ci_lo_col is not None,
                amount_divisor=amount_divisor,
                y_axis_kind=y_axis_kind,
                hline=hline,
                title=f"{title_base} [{groups} = {gv}]",
                x_label=x_label,
                y_label=y_label,
                caption=caption,
                nrow=nrow,
                ncol=ncol,
                figsize=figsize,
            )
            out_figs.append(fig)
        return out_figs

    facet_keys = ([groups] if groups is not None else []) + ["_coh_lbl"]
    return _render_facets(
        work,
        facet_keys=facet_keys,
        show_interval=show_interval and ci_lo_col is not None,
        amount_divisor=amount_divisor,
        y_axis_kind=y_axis_kind,
        hline=hline,
        title=title_base,
        x_label=x_label,
        y_label=y_label,
        caption=caption,
        nrow=nrow,
        ncol=ncol,
        figsize=figsize,
    )


def plot_projection_fit(
    fit: LossFit | PremiumFit,
    role: str,
    conf_level: float | None = None,
    show_interval: bool = True,
    amount_divisor: float | str = "auto",
    nrow: int | None = None,
    ncol: int | None = None,
    figsize: tuple[float, float] | None = None,
    method_label: str | None = None,
) -> Any:
    """Shared projection-curve plot for LossFit / PremiumFit / CLFit / EDFit.

    Cumulative ``<role>_obs`` (solid) + bridge + ``<role>_proj`` (dashed)
    by cohort. CI ribbon derived from ``<role>_total_se`` when present.
    ``conf_level`` and method label fall back to
    ``getattr(fit, ..., default)`` so worker-level fits without those
    attributes still render correctly.
    """
    if role not in ("loss", "premium", "ratio"):
        raise ValueError(
            f"`role` must be 'loss', 'premium', or 'ratio'; got {role!r}."
        )

    df = fit._df
    groups = fit._groups
    coh = fit._cohort
    dev = fit._dev
    conf = float(
        conf_level if conf_level is not None
        else getattr(fit, "conf_level", 0.95)
    )
    ci_type = getattr(fit, "ci_type", "analytical")
    method = method_label if method_label is not None else getattr(fit, "method", "")

    obs_col = f"{role}_obs"
    proj_col = f"{role}_proj"
    se_col = f"{role}_total_se"

    has_se = se_col in df.columns and df[se_col].is_finite().any()
    show_band = show_interval and has_se

    if isinstance(amount_divisor, str):
        if amount_divisor != "auto":
            raise ValueError(
                f"`amount_divisor` must be numeric or 'auto', got "
                f"{amount_divisor!r}."
            )
        amount_divisor = _auto_divisor(df[proj_col].to_numpy())
    amount_divisor = float(amount_divisor)

    is_obs = df[obs_col].is_not_null()
    y_full = np.where(
        is_obs.to_numpy(), df[obs_col].to_numpy(), df[proj_col].to_numpy()
    )
    work = df.with_columns(
        pl.Series(name="_y", values=y_full),
        is_obs.alias("_is_obs"),
    )

    if show_band:
        z = float(norm.ppf((1 + conf) / 2))
        lo = np.clip(
            df[proj_col].to_numpy() - z * df[se_col].to_numpy(),
            a_min=0.0,
            a_max=None,
        )
        hi = df[proj_col].to_numpy() + z * df[se_col].to_numpy()
        work = work.with_columns(
            pl.Series(name="_lo", values=lo),
            pl.Series(name="_hi", values=hi),
        )

    grain = _resolve_grain(fit)
    coh_type = _get_period_type(coh, grain=grain)
    coh_labels = _cohort_label_series(work["cohort"], coh_type)
    work = work.with_columns(pl.Series(name="_coh_lbl", values=coh_labels))

    base_lab = {"loss": "Loss", "premium": "Premium", "ratio": "Loss Ratio"}[role]
    title = (
        f"Projected Cumulative {base_lab}"
        + (f" (method: {method})" if method else "")
    )
    caption = (
        f"Interval: {round(conf * 100)}% ({ci_type})"
        if show_band
        else None
    )

    facet_keys = ([groups] if groups is not None else []) + ["_coh_lbl"]
    return _render_facets(
        work,
        facet_keys=facet_keys,
        show_interval=show_band,
        amount_divisor=amount_divisor,
        y_axis_kind="amount",
        hline=0.0,
        title=title,
        x_label=_pretty_var_label(dev),
        y_label=role,
        caption=caption,
        nrow=nrow,
        ncol=ncol,
        figsize=figsize,
    )


# ---------------------------------------------------------------------------
# internal helpers
# ---------------------------------------------------------------------------


def _resolve_grain(fit: Any) -> str | None:
    """Pull the Triangle grain when reachable on the fit object."""
    for slot in ("data", "_data", "triangle", "_triangle"):
        obj = getattr(fit, slot, None)
        grain = getattr(obj, "grain", None) if obj is not None else None
        if grain is not None:
            return grain
    return None


def _get_role_label(fit: Any, role: str) -> str:
    """Resolve a y-axis role label, preferring the raw column name on
    the underlying Triangle if reachable.
    """
    for slot in ("data", "_data", "triangle", "_triangle"):
        obj = getattr(fit, slot, None)
        raw = getattr(obj, role, None) if obj is not None else None
        if isinstance(raw, str):
            return raw
    return role


def _cohort_label_series(values: pl.Series, period_type: str | None) -> list[str]:
    if period_type is None:
        return [str(v) for v in values.to_list()]
    return _format_period_series(values, period_type)


def _render_facets(
    work: pl.DataFrame,
    *,
    facet_keys: list[str],
    show_interval: bool,
    amount_divisor: float,
    y_axis_kind: str,
    hline: float | None,
    title: str,
    x_label: str,
    y_label: str,
    caption: str | None,
    nrow: int | None,
    ncol: int | None,
    figsize: tuple[float, float] | None,
) -> Any:
    """Render one Figure with one panel per facet key combination."""
    import matplotlib.pyplot as plt

    # Stable cohort-label ordering: sort facets by the raw `cohort` value
    # so callers see oldest -> newest left-to-right / top-to-bottom.
    sort_cols = [c for c in facet_keys if c != "_coh_lbl"] + ["cohort"]
    work_sorted = work.sort(sort_cols)
    facet_seen: list[tuple] = []
    seen_set: set[tuple] = set()
    for row in work_sorted.select(facet_keys).iter_rows():
        if row not in seen_set:
            seen_set.add(row)
            facet_seen.append(row)

    n_facets = len(facet_seen)
    if n_facets == 0:
        fig, ax = plt.subplots(1, 1, figsize=figsize or (5.0, 3.5))
        ax.text(0.5, 0.5, "(no data)", ha="center", va="center")
        fig.suptitle(title, fontsize=12, fontweight="bold")
        return fig

    if nrow is None and ncol is None:
        ncol = min(n_facets, 3)
        nrow = math.ceil(n_facets / ncol)
    elif ncol is None:
        ncol = math.ceil(n_facets / max(nrow, 1))
    elif nrow is None:
        nrow = math.ceil(n_facets / max(ncol, 1))

    if figsize is None:
        fig_w = max(5.0, 3.2 * ncol)
        fig_h = max(3.5, 2.4 * nrow)
        figsize = (fig_w, fig_h)

    fig, axes = plt.subplots(
        nrow, ncol, figsize=figsize, squeeze=False, constrained_layout=True
    )

    for idx, key in enumerate(facet_seen):
        r, c = divmod(idx, ncol)
        ax = axes[r][c]
        mask = pl.lit(True)
        for col, val in zip(facet_keys, key):
            mask = mask & (pl.col(col) == val)
        sub = work_sorted.filter(mask)
        _draw_panel(
            ax,
            sub=sub,
            show_interval=show_interval,
            amount_divisor=amount_divisor,
            y_axis_kind=y_axis_kind,
            hline=hline,
        )
        ax.set_title(" | ".join(str(v) for v in key), fontsize=9)

    for idx in range(n_facets, nrow * ncol):
        r, c = divmod(idx, ncol)
        axes[r][c].set_visible(False)

    fig.suptitle(title, fontsize=12, fontweight="bold")
    fig.supxlabel(x_label, fontsize=10)
    fig.supylabel(y_label, fontsize=10)
    if caption:
        fig.text(0.99, 0.005, caption, ha="right", va="bottom", fontsize=8)

    return fig


def _draw_panel(
    ax,
    *,
    sub: pl.DataFrame,
    show_interval: bool,
    amount_divisor: float,
    y_axis_kind: str,
    hline: float | None,
) -> None:
    """One panel: obs line + bridge + proj dashed + (optional) CI ribbon."""
    sub = sub.sort("dev")
    dev_vals = sub["dev"].to_numpy()
    y_vals = sub["_y"].to_numpy()
    is_obs = sub["_is_obs"].to_numpy()

    # Scale amounts for display (ratios are scaled to percent at axis time).
    if y_axis_kind == "amount" and amount_divisor != 1.0:
        y_vals = y_vals / amount_divisor

    obs_mask = is_obs & np.isfinite(y_vals)
    proj_mask = (~is_obs) & np.isfinite(y_vals)

    # CI ribbon -- on projected cells only.
    if show_interval and "_lo" in sub.columns:
        lo = sub["_lo"].to_numpy()
        hi = sub["_hi"].to_numpy()
        if y_axis_kind == "amount" and amount_divisor != 1.0:
            lo = lo / amount_divisor
            hi = hi / amount_divisor
        rib_mask = proj_mask & np.isfinite(lo) & np.isfinite(hi)
        if rib_mask.any():
            ax.fill_between(
                dev_vals[rib_mask], lo[rib_mask], hi[rib_mask],
                alpha=0.15, color="C0", linewidth=0,
            )

    # Bridge segment: last observed cell -> first projected cell.
    if obs_mask.any() and proj_mask.any():
        last_obs_idx = np.flatnonzero(obs_mask)[-1]
        first_proj_idx = np.flatnonzero(proj_mask)[0]
        ax.plot(
            [dev_vals[last_obs_idx], dev_vals[first_proj_idx]],
            [y_vals[last_obs_idx], y_vals[first_proj_idx]],
            color="C0", linewidth=1.2, solid_capstyle="round",
        )

    # Observed: solid line (>=2 points) or point (single).
    if obs_mask.sum() >= 2:
        ax.plot(
            dev_vals[obs_mask], y_vals[obs_mask],
            color="C0", linewidth=1.2,
        )
    elif obs_mask.sum() == 1:
        ax.scatter(
            dev_vals[obs_mask], y_vals[obs_mask],
            color="C0", s=22,
        )

    # Projected: dashed line (>=2 points) or hollow point (single).
    if proj_mask.sum() >= 2:
        ax.plot(
            dev_vals[proj_mask], y_vals[proj_mask],
            color="C0", linewidth=1.2, linestyle="--",
        )
    elif proj_mask.sum() == 1:
        ax.scatter(
            dev_vals[proj_mask], y_vals[proj_mask],
            facecolors="none", edgecolors="C0", s=22,
        )

    if hline is not None:
        h = hline / (amount_divisor if y_axis_kind == "amount" else 1.0)
        ax.axhline(h, color="red", linewidth=0.8, linestyle="--")

    if y_axis_kind == "ratio":
        ax.yaxis.set_major_formatter(
            _percent_formatter()
        )

    ax.grid(True, linewidth=0.3, alpha=0.5)


def _percent_formatter():
    from matplotlib.ticker import FuncFormatter
    return FuncFormatter(lambda v, _pos: f"{round(v * 100)}%")
