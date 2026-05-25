"""Plotting utilities shared across visualisation methods.

Mirrors the helper layer that R `R/utils.R` + `R/utils-plot.R` expose
to `plot_triangle.*` and `plot.*`. The matplotlib backend is intentional
(R/Python idiom divergence -- ggplot is not available in Python).
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import polars as pl

# Valid metric names that can be passed to plot_triangle().
_VALID_METRICS: tuple[str, ...] = (
    "ratio", "incr_ratio",
    "loss", "incr_loss",
    "premium", "incr_premium",
    "margin", "incr_margin",
    "loss_share", "incr_loss_share",
    "premium_share", "incr_premium_share",
)

_RATIO_METRICS = {"ratio", "incr_ratio"}
_AMOUNT_METRICS = {
    "loss", "incr_loss",
    "premium", "incr_premium",
    "margin", "incr_margin",
}
_PROP_METRICS = {
    "loss_share", "incr_loss_share",
    "premium_share", "incr_premium_share",
}

_TITLES: dict[str, str] = {
    "ratio":              "Cumulative Loss Ratio",
    "incr_ratio":         "Per-Period Loss Ratio",
    "loss":               "Cumulative Loss",
    "incr_loss":          "Per-Period Loss",
    "premium":            "Cumulative Premium",
    "incr_premium":       "Per-Period Premium",
    "margin":             "Cumulative Margin",
    "incr_margin":        "Per-Period Margin",
    "loss_share":         "Cumulative Loss Proportion",
    "incr_loss_share":    "Per-Period Loss Proportion",
    "premium_share":      "Cumulative Premium Proportion",
    "incr_premium_share": "Per-Period Premium Proportion",
}

_PRETTY_VAR_LABEL: dict[str, str] = {
    "dev_m": "development months",
    "dev_q": "development quarters",
    "dev_h": "development halves",
    "dev_y": "development years",
    "uy_m":  "underwriting months",
    "uy_q":  "underwriting quarters",
    "uy_h":  "underwriting halves",
    "uy":    "underwriting years",
    "cy_m":  "calendar months",
    "cy_q":  "calendar quarters",
    "cy_h":  "calendar halves",
    "cy":    "calendar years",
}

_GRAIN_TO_TYPE: dict[str, str] = {
    "M": "month", "Q": "quarter", "H": "half", "Y": "year",
}

_VAR_TO_TYPE: dict[str, str] = {
    "uy_m": "month", "cy_m": "month",
    "uy_q": "quarter", "cy_q": "quarter",
    "uy_h": "half", "cy_h": "half",
    "uy": "year", "cy": "year",
}


def _get_period_type(var: str | None, grain: str | None = None) -> str | None:
    """R `.get_period_type`. Resolve the period type of a column from
    its variable name (`uy_m`, `cy_h`, ...), falling back to grain.
    """
    if var is None:
        return None
    t = _VAR_TO_TYPE.get(var)
    if t is not None:
        return t
    if grain is None:
        return None
    return _GRAIN_TO_TYPE.get(grain)


def _pretty_var_label(var: str | None) -> str:
    if var is None:
        return ""
    return _PRETTY_VAR_LABEL.get(var, var)


def _cohort_label(var: str | None, grain: str | None = None) -> str:
    """R `.cohort_label` -- e.g. `"cohort (month)"`."""
    if var is None:
        return "cohort"
    t = _get_period_type(var, grain=grain)
    if t is None:
        return "cohort"
    qualifier = {
        "month": "month",
        "quarter": "quarter",
        "half": "half-yearly",
        "year": "yearly",
    }.get(t)
    if qualifier is None:
        return "cohort"
    return f"cohort ({qualifier})"


def _format_period_series(
    values: pl.Series,
    period_type: str,
    sep: str = ".",
    abb: bool = True,
) -> list[str]:
    """R `.format_period` -- format a Date series as period labels
    like `"23.01"`, `"23.2Q"`, `"23.1H"`, `"23"`.
    """
    if period_type == "year":
        years = values.dt.year().to_list()
        if abb:
            return [f"{y % 100:02d}" for y in years]
        return [str(y) for y in years]

    months = values.dt.month().to_list()
    years = values.dt.year().to_list()
    yr = [f"{y % 100:02d}" if abb else str(y) for y in years]

    if period_type == "month":
        return [f"{y}{sep}{m:02d}" for y, m in zip(yr, months)]
    if period_type == "quarter":
        return [f"{y}{sep}{(m - 1) // 3 + 1}Q" for y, m in zip(yr, months)]
    if period_type == "half":
        return [f"{y}{sep}{2 if m > 6 else 1}H" for y, m in zip(yr, months)]
    raise ValueError(f"Invalid period_type: {period_type!r}")


def _auto_divisor(values: np.ndarray | pl.Series | list[float]) -> float:
    """R `.auto_divisor` -- pick the largest divisor in
    {1, 1e3, 1e6, 1e9, 1e12} such that the median formats as a
    non-zero label at `%.1f`.
    """
    arr = np.asarray(values, dtype=float)
    arr = arr[np.isfinite(arr) & (arr > 0)]
    if arr.size == 0:
        return 1.0
    candidates = np.array([1.0, 1e3, 1e6, 1e9, 1e12])
    m = float(np.median(arr))
    ok = (m / candidates) >= 0.05 - 1e-10
    if not ok.any():
        return 1.0
    return float(candidates[ok].max())


def _get_amount_unit(divisor: float) -> str:
    if divisor == 1:
        return ""
    if divisor == 1e3:
        return "thousand"
    if divisor == 1e6:
        return "million"
    if divisor == 1e9:
        return "billion"
    if divisor == 1e12:
        return "trillion"
    return f"scaled (/{divisor:.0e})"


@dataclass
class PlotMeta:
    metric: str
    kind: str            # "ratio" | "amount" | "prop"
    title: str
    caption: str
    threshold: float
    when: str            # ">" | "<"


def _resolve_plot_meta(metric: str, amount_divisor: float) -> PlotMeta:
    """Combine metric metadata (title / threshold / when) with the
    resolved divisor to build the caption string.
    """
    if metric not in _VALID_METRICS:
        raise ValueError(
            f"`metric` must be one of {_VALID_METRICS!r}; got {metric!r}."
        )

    title = _TITLES[metric]

    if metric in _RATIO_METRICS:
        return PlotMeta(metric, "ratio", title, "Unit: %", 1.0, ">")
    if metric in _AMOUNT_METRICS:
        unit = _get_amount_unit(amount_divisor)
        caption = f"Unit: {unit}" if unit else "Unit:"
        return PlotMeta(metric, "amount", title, caption, 0.0, "<")
    # prop
    return PlotMeta(metric, "prop", title, "Unit: %", 0.05, ">")
