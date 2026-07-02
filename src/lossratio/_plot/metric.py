"""Metric metadata for the triangle value/line plots -- matplotlib backend.

Which metrics ``plot_triangle`` / ``Triangle.plot`` accept and, per metric, the
title / unit caption / cell-colouring rule, plus the amount-divisor helpers.
Shared by the value heatmap and the cohort-line plot; not used by the usage
heatmap (categorical) or the other plot modules, so it sits here rather than in
the shared ``base`` layer.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import polars as pl

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
_SHARE_METRICS = {
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


def _auto_divisor(values: np.ndarray | pl.Series | list[float]) -> float:
    """Pick the largest divisor in {1, 1e3, 1e6, 1e9, 1e12} such that
    the median formats as a non-zero label at `%.1f`.
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
class MetricStyle:
    metric: str
    kind: str            # "ratio" | "amount" | "share"
    title: str
    caption: str
    threshold: float
    when: str            # ">" | "<"


def _metric_style(metric: str, amount_divisor: float) -> MetricStyle:
    """Combine metric metadata (title / threshold / when) with the
    resolved divisor to build the caption string.
    """
    if metric not in _VALID_METRICS:
        raise ValueError(
            f"`metric` must be one of {_VALID_METRICS!r}; got {metric!r}."
        )

    title = _TITLES[metric]

    if metric in _RATIO_METRICS:
        return MetricStyle(metric, "ratio", title, "Unit: %", 1.0, ">")
    if metric in _AMOUNT_METRICS:
        unit = _get_amount_unit(amount_divisor)
        caption = f"Unit: {unit}" if unit else "Unit:"
        return MetricStyle(metric, "amount", title, caption, 0.0, "<")
    # prop
    return MetricStyle(metric, "share", title, "Unit: %", 0.05, ">")
