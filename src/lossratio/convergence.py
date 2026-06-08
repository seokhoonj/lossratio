"""Convergence: detect the development period at which the projected
loss ratio stabilises.

A multi-criterion drift / slope / dispersion test on the Ratio
backtest path.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import mirror_output, normalize_groups

if TYPE_CHECKING:
    from ._io import FrameLike
    from .triangle import Triangle


_VALID_METHODS = ("tail", "window", "slope", "all")
_NEAR_ZERO_FLOOR = 1e-8


def _compute_dispersion(
    triangle: "Triangle",
    min_n_cohorts: int = 5,
) -> pl.DataFrame:
    """Robust cross-cohort dispersion of incremental Ratio per (group, duration).

    Returns a polars DataFrame with columns ``[groups?, duration,
    n_cohorts, ratio_median, ratio_mad, dispersion, flag]``.
    """
    tri_df = triangle.to_polars().filter(pl.col("ratio").is_not_null())
    grp = triangle.groups
    by_cols = [*normalize_groups(grp), "duration"]

    out = (
        tri_df.group_by(by_cols)
        .agg(
            pl.len().alias("n_cohorts"),
            pl.col("ratio").median().alias("ratio_median"),
            # MAD with constant 1.4826 = consistent estimator of sigma
            # for normal data: median(|x - median(x)|) * 1.4826.
            (
                (pl.col("ratio") - pl.col("ratio").median()).abs().median()
                * 1.4826
            ).alias("ratio_mad"),
        )
        .sort(by_cols)
    )

    out = out.with_columns(
        pl.when(pl.col("n_cohorts") < min_n_cohorts)
          .then(pl.lit("sparse"))
          .when(pl.col("ratio_median").abs() < _NEAR_ZERO_FLOOR)
          .then(pl.lit("near_zero_median"))
          .otherwise(pl.lit("ok"))
          .alias("flag")
    )

    denom = pl.max_horizontal(
        pl.col("ratio_median").abs(), pl.lit(_NEAR_ZERO_FLOOR)
    )
    out = out.with_columns(
        pl.when(pl.col("flag") == "sparse")
          .then(None)
          .otherwise(pl.col("ratio_mad") / denom)
          .alias("dispersion")
    )

    return out.select(by_cols + ["n_cohorts", "ratio_median", "ratio_mad",
                                 "dispersion", "flag"])


def _extract_portfolio_ratio(bt_fit: Any) -> float:
    """Portfolio-level projected Ratio from a BacktestFit.

    Computes ``sum(loss_proj) / sum(premium_proj)`` where ``loss_proj`` /
    ``premium_proj`` are the *latest projected* per-cohort cumulative
    values (last non-null ``loss_proj`` / ``premium_proj`` sorted by
    duration). Using last-non-null is robust when the masked refit's
    projection halts before ``duration_max`` because late ATA factors are
    unestimable (no cohort-pair under the mask).

    Returns ``nan`` when no cohorts have a projectable cell or total
    premium is non-positive.
    """
    refit = getattr(bt_fit, "_refit", None)
    if refit is None:
        return float("nan")
    df = getattr(refit, "_df", None)
    if df is None:
        return float("nan")
    if not isinstance(df, pl.DataFrame):
        return float("nan")
    if "loss_proj" not in df.columns or "premium_proj" not in df.columns:
        return float("nan")

    keys = [*normalize_groups(getattr(refit, "_groups", None)), "cohort"]

    # Last non-null projection per cohort (sorted by duration).
    ult = (
        df.filter(pl.col("loss_proj").is_not_null()
                  & pl.col("premium_proj").is_not_null())
        .sort(keys + ["duration"])
        .group_by(keys, maintain_order=True)
        .agg(
            pl.col("loss_proj").last().alias("loss_proj"),
            pl.col("premium_proj").last().alias("premium_proj"),
        )
    )
    if ult.height == 0:
        return float("nan")

    total_loss = ult["loss_proj"].sum()
    total_premium = ult["premium_proj"].sum()
    if total_loss is None or not np.isfinite(total_loss):
        return float("nan")
    if total_premium is None or not np.isfinite(total_premium) or total_premium <= 0:
        return float("nan")
    return float(total_loss) / float(total_premium)


def _ols_slope(x: np.ndarray, y: np.ndarray) -> float:
    """OLS slope of y ~ x. Returns nan when fewer than 2 finite pairs or
    x has zero variance."""
    mask = np.isfinite(x) & np.isfinite(y)
    if mask.sum() < 2:
        return float("nan")
    xo = x[mask]
    yo = y[mask]
    vx = xo.var(ddof=1)
    if not np.isfinite(vx) or vx <= 0:
        return float("nan")
    return float(np.cov(xo, yo, ddof=1)[0, 1] / vx)


def detect_convergence(
    triangle: "Triangle",
    method: str = "tail",
    max_drift: float = 0.01,
    max_slope: float = 1e-3,
    max_dispersion: float = 0.15,
    window: int = 5,
    start: int = 2,
    holdout_max: int | None = None,
    min_n_cohorts: int = 5,
    **backtest_kwargs: Any,
) -> "Convergence":
    """Detect the development period at which the projected Ratio stabilises.

    Parameters
    ----------
    triangle
        A :class:`Triangle`.
    method
        Which stability criterion drives the detected convergence ``point``. One of:

        - ``"tail"`` (default, reserving-safe): tail drift over
          ``[k, duration_max]`` falls below ``max_drift``.
        - ``"window"``: local drift over ``[k, k + window - 1]``.
        - ``"slope"``: ``|OLS slope of Ratio ~ k|`` on ``[k, duration_max]``
          below ``max_slope``.
        - ``"all"``: all three above pass simultaneously.

        A cross-cohort dispersion clause ``dispersion < max_dispersion``
        is required regardless of method.
    max_drift
        Upper bound on the drift metric (window / tail). Default ``0.01``.
    max_slope
        Upper bound on ``|slope|``. Default ``1e-3``.
    max_dispersion
        Upper bound on the cross-cohort dispersion. Default ``0.15``.
    window
        Drift window length (in duration steps). Default ``5``.
    start
        First development period to scan for convergence (the candidate
        floor). Default ``2``. Convergence runs its own holdout backtests,
        so it needs no external factor-stability anchor.
    holdout_max
        Cap on holdout depth. When ``None``, set to
        ``max(window, (duration_max - start) // 2)``.
    min_n_cohorts
        Minimum cohorts required to compute dispersion. Default ``5``.
    **backtest_kwargs
        Forwarded to ``Backtest`` (e.g. ``estimator`` choice). Defaults
        to ``lr.Ratio(method="sa")``.

    Returns
    -------
    Convergence
        Result object exposing ``point`` (the convergence duration),
        ``start``, ``duration_max``, the candidate duration sequence and
        per-criterion diagnostics.
    """
    import warnings
    from .ratio import Ratio
    from .backtest import Backtest

    if method not in _VALID_METHODS:
        raise ValueError(
            f"method must be one of {_VALID_METHODS}, got {method!r}"
        )
    if not (np.isfinite(max_drift) and max_drift > 0):
        raise ValueError("max_drift must be positive and finite")
    if not (np.isfinite(max_slope) and max_slope > 0):
        raise ValueError("max_slope must be positive and finite")
    if not (np.isfinite(max_dispersion) and max_dispersion > 0):
        raise ValueError("max_dispersion must be positive and finite")
    if window < 2:
        raise ValueError("window must be >= 2")
    window = int(window)

    # 1. Resolve the candidate-scan floor. Convergence no longer anchors on
    # the ATA factor-stability point (it runs its own holdout backtests).
    start = int(start)
    if start < 2:
        raise ValueError(f"start must be >= 2, got {start}")

    # 2. duration_max + candidate sequence.
    duration_max = int(triangle.to_polars()["duration"].max())
    if holdout_max is None:
        holdout_max = max(window, (duration_max - start) // 2)
    holdout_max = int(holdout_max)

    if duration_max - 2 >= start:
        duration_cand = list(range(start, duration_max - 1))
    else:
        duration_cand = []
        warnings.warn(
            f"No candidate duration points: start ({start}) + 2 > duration_max "
            f"({duration_max}). Returning point = None.",
            stacklevel=2,
        )

    # 3. Ratio per candidate via cached backtest.
    estimator = backtest_kwargs.pop("estimator", None) or Ratio(method="sa")
    ratio_arr = np.full(len(duration_cand), np.nan)
    cache: dict[int, float] = {}

    def _get_ratio(h: int) -> float:
        if h in cache:
            return cache[h]
        try:
            bt_fit = Backtest(
                estimator=estimator, holdout=h, target="ratio"
            ).fit(triangle)
            val = _extract_portfolio_ratio(bt_fit)
        except (
            ValueError, KeyError, RuntimeError,
            pl.exceptions.ColumnNotFoundError, pl.exceptions.ComputeError,
        ):
            # A legitimately-unprojectable fold already returns nan from
            # _extract_portfolio_ratio; this only fires on a degenerate
            # refit. Genuine structural bugs propagate.
            val = float("nan")
        cache[h] = val
        return val

    for i, k in enumerate(duration_cand):
        h = duration_max - k
        if h < 1 or h > holdout_max:
            continue
        ratio_arr[i] = _get_ratio(h)

    # 4. revision (diagnostic only).
    revision = np.full(len(duration_cand), np.nan)
    if len(duration_cand) >= 2:
        revision[1:] = np.abs(np.diff(ratio_arr))

    # 5. drift_window: max - min over each length-`window` slice
    # [i, i+window-1]. NaN-propagating max/min naturally leave NaN where a
    # window has any gap (matching the original all-finite guard); slices
    # that would run past the end stay NaN.
    drift_window = np.full(len(duration_cand), np.nan)
    if len(duration_cand) >= window:
        from numpy.lib.stride_tricks import sliding_window_view
        w = sliding_window_view(ratio_arr, window)
        drift_window[: w.shape[0]] = np.max(w, axis=1) - np.min(w, axis=1)

    # 6. drift_tail over [i, end].
    drift_tail = np.full(len(duration_cand), np.nan)
    for i in range(len(duration_cand)):
        w = ratio_arr[i:]
        w = w[np.isfinite(w)]
        if len(w) >= 2:
            drift_tail[i] = float(np.max(w) - np.min(w))

    # 7. slope -- OLS slope of Ratio ~ duration on [i, end].
    slope_arr = np.full(len(duration_cand), np.nan)
    duration_cand_arr = np.array(duration_cand, dtype=float)
    for i in range(len(duration_cand)):
        slope_arr[i] = _ols_slope(duration_cand_arr[i:], ratio_arr[i:])

    # 8. dispersion at each candidate duration (group-collapsed via median).
    dispersion = np.full(len(duration_cand), np.nan)
    if duration_cand:
        disp_tbl = _compute_dispersion(triangle, min_n_cohorts=min_n_cohorts)
        if triangle.groups is not None:
            disp_tbl = (
                disp_tbl.group_by("duration")
                .agg(pl.col("dispersion").median().alias("dispersion"))
            )
        disp_map = dict(
            zip(disp_tbl["duration"].to_list(), disp_tbl["dispersion"].to_list())
        )
        for i, k in enumerate(duration_cand):
            v = disp_map.get(k)
            if v is not None:
                dispersion[i] = float(v)

    # 9. per-method pass tests. A candidate whose own Ratio could not be
    # computed (NaN -- e.g. an early duration below the holdout cap) must never be
    # selected, even if the tail/slope skipped over it: guard on the
    # candidate's own finite Ratio. With `start=2` this matters where the old
    # factor-stability floor used to keep early candidates out.
    finite_ratio = np.isfinite(ratio_arr)
    pass_d = np.isfinite(dispersion) & (dispersion < max_dispersion)
    pass_window = (
        np.isfinite(drift_window) & (drift_window < max_drift) & pass_d & finite_ratio
    )
    pass_tail = (
        np.isfinite(drift_tail) & (drift_tail < max_drift) & pass_d & finite_ratio
    )
    pass_slope = (
        np.isfinite(slope_arr) & (np.abs(slope_arr) < max_slope) & pass_d & finite_ratio
    )
    pass_all = pass_window & pass_tail & pass_slope

    pass_arr = {
        "window": pass_window,
        "tail": pass_tail,
        "slope": pass_slope,
        "all": pass_all,
    }[method]

    # 10. first passing duration.
    if pass_arr.any():
        first_idx = int(np.argmax(pass_arr))
        conv_k: int | None = int(duration_cand[first_idx])
    else:
        conv_k = None

    return Convergence._from_arrays(
        triangle=triangle,
        method=method,
        conv_k=conv_k,
        start=start,
        duration_max=duration_max,
        duration_cand=duration_cand,
        ratio=ratio_arr,
        revision=revision,
        drift_window=drift_window,
        drift_tail=drift_tail,
        slope=slope_arr,
        dispersion=dispersion,
        pass_window=pass_window,
        pass_tail=pass_tail,
        pass_slope=pass_slope,
        pass_chosen=pass_arr,
        max_drift=max_drift,
        max_slope=max_slope,
        max_dispersion=max_dispersion,
        window=window,
        holdout_max=holdout_max,
        min_n_cohorts=min_n_cohorts,
    )


def _portfolio_factor_tail_ratio(fit: Any) -> float:
    """Portfolio ultimate loss ratio under the *factor* (CL/ED) tail.

    The runoff leg of the convergence-anchored band: the loss / premium
    development factors extrapolated to ultimate, composed into a ratio.
    Refits the fit's estimator with ``tail=True`` (forcing the
    convergence-gated factor tail) unless the fit already carries a tail,
    then sums the tailed ultimate loss / premium per cohort. Falls back to
    the untailed projection on any cohort whose tail was refused
    (``*_tail`` null). Returns ``nan`` on non-positive premium.
    """
    import dataclasses

    est = fit._estimator
    tfit = fit if getattr(est, "tail", False) else dataclasses.replace(
        est, tail=True
    ).fit(fit._triangle)
    full = tfit.to_polars()
    full = full if isinstance(full, pl.DataFrame) else pl.from_pandas(full)

    keys = [*normalize_groups(fit._groups), "cohort"]
    has_tail = "loss_tail" in full.columns and "premium_tail" in full.columns
    agg = [pl.col("loss_proj").last(), pl.col("premium_proj").last()]
    if has_tail:
        agg += [pl.col("loss_tail").last(), pl.col("premium_tail").last()]
    last = (
        full.filter(
            pl.col("loss_proj").is_not_null()
            & pl.col("premium_proj").is_not_null()
        )
        .sort(keys + ["duration"])
        .group_by(keys, maintain_order=True)
        .agg(*agg)
    )
    if last.height == 0:
        return float("nan")
    if has_tail:
        # Match RatioFit's own tail composition (ratio.py): tail BOTH sides
        # together or NEITHER -- never one tailed against the other frozen.
        # loss_tail / premium_tail are emitted per group, and a cohort can
        # carry one without the other; using each independently would mix a
        # tailed numerator with an untailed denominator (inconsistent
        # horizon). Where they do not co-occur, fall both back to the
        # untailed projection (exactly what RatioFit leaves ratio_tail null).
        both = (
            pl.col("loss_tail").is_not_null()
            & pl.col("premium_tail").is_not_null()
            & (pl.col("premium_tail") != 0.0)
        )
        last = last.with_columns(
            pl.when(both).then(pl.col("loss_tail"))
            .otherwise(pl.col("loss_proj")).alias("_lu"),
            pl.when(both).then(pl.col("premium_tail"))
            .otherwise(pl.col("premium_proj")).alias("_pu"),
        )
    else:
        last = last.with_columns(
            pl.col("loss_proj").alias("_lu"),
            pl.col("premium_proj").alias("_pu"),
        )
    total_loss = last["_lu"].sum()
    total_premium = last["_pu"].sum()
    if total_premium is None or not np.isfinite(total_premium) or total_premium <= 0:
        return float("nan")
    return float(total_loss) / float(total_premium)


_CONV_TAIL_SCHEMA: dict[str, pl.DataType] = {
    "status":            pl.Utf8,
    "k_conv":            pl.Int64,
    "ratio_latest":      pl.Float64,
    "ratio_factor_tail": pl.Float64,
    "ratio_headline":    pl.Float64,
    "band_lo":           pl.Float64,
    "band_hi":           pl.Float64,
    "band_width":        pl.Float64,
}


def convergence_tail_frame(fit: Any, **conv_kwargs: Any) -> "FrameLike":
    """Convergence(k**)-anchored tail for the portfolio loss ratio.

    See :meth:`~lossratio.RatioFit.convergence_tail` for the contract.
    """
    conv = fit.convergence(**conv_kwargs)
    df = conv.summary()
    df = df if isinstance(df, pl.DataFrame) else pl.from_pandas(df)

    # The converged / stable level is the latest finite candidate ratio --
    # the one built from the smallest holdout (the most data used), NOT
    # `ratio[k**]` which is often nan (early candidate durations need a holdout
    # past `holdout_max` and are skipped).
    fin = df.filter(pl.col("ratio").is_finite()).sort("duration")
    ratio_latest = (
        float(fin["ratio"].to_list()[-1]) if fin.height else float("nan")
    )
    converged = conv.point is not None

    ratio_factor_tail = _portfolio_factor_tail_ratio(fit)

    # ANCHOR: when the LR converged in-window the headline is the observed
    # plateau (the stable level), NOT the factor-runoff extrapolation; the
    # factor tail rides along as the other band leg so any disagreement is
    # disclosed. When immature (no k**) the factor tail is the only forward
    # estimate -- it becomes the headline but is flagged with a wide band.
    headline = ratio_latest if converged else ratio_factor_tail
    status = "converged" if converged else "immature"

    legs = [
        r for r in (ratio_latest, ratio_factor_tail)
        if r is not None and np.isfinite(r)
    ]
    band_lo = min(legs) if legs else None
    band_hi = max(legs) if legs else None
    band_width = (band_hi - band_lo) if band_lo is not None else None

    row = {
        "status": status,
        "k_conv": conv.point,
        "ratio_latest": ratio_latest,
        "ratio_factor_tail": ratio_factor_tail,
        "ratio_headline": headline,
        "band_lo": band_lo,
        "band_hi": band_hi,
        "band_width": band_width,
    }
    out = pl.DataFrame([row], schema=_CONV_TAIL_SCHEMA)
    return mirror_output(out, fit._output_type)


class Convergence:
    """Result of :func:`detect_convergence`.

    Attributes
    ----------
    point : int | None
        First duration at which the chosen ``method``'s pass test fires.
    method : str
        Which criterion selected ``point``.
    start : int
        Floor used as the lower bound of the candidate window.
    duration_max : int
        Maximum observable duration.
    duration_cand : list[int]
        Candidate duration sequence ``[start, duration_max - 2]``.
    ratio, revision, drift_window, drift_tail, slope, dispersion : ndarray
        Diagnostic series, one entry per ``duration_cand``.
    pass_window, pass_tail, pass_slope, pass_ : ndarray[bool]
        Per-criterion pass vectors plus the chosen criterion.
    """

    def __init__(self) -> None:
        raise TypeError(
            "Convergence is the result of `ratio_fit.convergence()`, not a direct constructor."
        )

    @classmethod
    def _from_arrays(
        cls,
        triangle: "Triangle",
        method: str,
        conv_k: int | None,
        start: int,
        duration_max: int,
        duration_cand: list[int],
        ratio: np.ndarray,
        revision: np.ndarray,
        drift_window: np.ndarray,
        drift_tail: np.ndarray,
        slope: np.ndarray,
        dispersion: np.ndarray,
        pass_window: np.ndarray,
        pass_tail: np.ndarray,
        pass_slope: np.ndarray,
        pass_chosen: np.ndarray,
        max_drift: float,
        max_slope: float,
        max_dispersion: float,
        window: int,
        holdout_max: int,
        min_n_cohorts: int,
    ) -> "Convergence":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self.point = conv_k
        self.method = method
        self.start = start
        self.duration_max = duration_max
        self.duration_cand = list(duration_cand)
        self.ratio = ratio
        self.revision = revision
        self.drift_window = drift_window
        self.drift_tail = drift_tail
        self.slope = slope
        self.dispersion = dispersion
        self.pass_window = pass_window
        self.pass_tail = pass_tail
        self.pass_slope = pass_slope
        # `pass` is a reserved word in Python; expose as `pass_`. The
        # per-method vectors above are also retained.
        self.pass_ = pass_chosen
        self.max_drift = max_drift
        self.max_slope = max_slope
        self.max_dispersion = max_dispersion
        self.window = window
        self.holdout_max = holdout_max
        self.min_n_cohorts = min_n_cohorts
        return self

    def summary(self) -> "FrameLike":
        """One-row-per-candidate diagnostic table."""
        df = pl.DataFrame({
            "duration": self.duration_cand,
            "ratio": self.ratio,
            "revision": self.revision,
            "drift_window": self.drift_window,
            "drift_tail": self.drift_tail,
            "slope": self.slope,
            "dispersion": self.dispersion,
            "pass_window": self.pass_window,
            "pass_tail": self.pass_tail,
            "pass_slope": self.pass_slope,
            "pass": self.pass_,
        })
        return mirror_output(df, self._output_type)

    @property
    def df(self) -> "FrameLike":
        return self.summary()

    def plot(
        self,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """5-panel convergence diagnostic plot (matplotlib).

        Stacked column of ``ratio``, ``drift_window``, ``drift_tail``,
        ``|slope|``, ``dispersion`` series across candidate duration
        cutoffs. Threshold hlines, the candidate-floor (``start``) dotted vline,
        and detected convergence ``point`` solid green vline are
        overlaid on every panel.

        Parameters
        ----------
        figsize
            Passed to ``plt.subplots``. Defaults to a vertical layout
            sized for 5 panels.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from ._convergence_vis import plot_convergence
        return plot_convergence(self, figsize=figsize)

    def __repr__(self) -> str:
        n = len(self.duration_cand)
        n_win = int(np.nansum(self.pass_window))
        n_tail = int(np.nansum(self.pass_tail))
        n_slope = int(np.nansum(self.pass_slope))
        bits = [
            f"method={self.method}",
            f"point={self.point}",
            f"start={self.start}",
            f"duration_max={self.duration_max}",
            f"candidates={n}",
            f"passes(window/tail/slope)={n_win}/{n_tail}/{n_slope}",
        ]
        return f"<Convergence: {', '.join(bits)}>"
