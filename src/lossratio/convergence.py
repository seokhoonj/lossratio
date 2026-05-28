"""Convergence: detect the development period at which the projected
loss ratio stabilises.

Mirror of R's ``detect_convergence()`` --- multi-criterion drift /
slope / dispersion test on the Ratio backtest path.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import mirror_output

if TYPE_CHECKING:
    from .triangle import Triangle


_VALID_METHODS = ("tail", "window", "slope", "all")
_NEAR_ZERO_FLOOR = 1e-8


def _compute_dispersion(
    triangle: "Triangle",
    min_n_cohorts: int = 5,
) -> pl.DataFrame:
    """Robust cross-cohort dispersion of incremental Ratio per (group, dev).

    Returns a polars DataFrame with columns ``[groups?, dev,
    n_cohorts, ratio_median, ratio_mad, dispersion, flag]``.
    """
    tri_df = triangle.to_polars().filter(pl.col("ratio").is_not_null())
    grp = triangle.groups
    by_cols = ([grp] if grp is not None else []) + ["dev"]

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

    Computes ``sum(loss_ult) / sum(premium_ult)`` where ``loss_ult`` /
    ``premium_ult`` are the *latest projected* per-cohort cumulative
    values (last non-null ``loss_proj`` / ``premium_proj`` sorted by
    dev). Using last-non-null is robust when the masked refit's
    projection halts before ``dev_max`` because late ATA factors are
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

    keys: list[str] = []
    gv = getattr(refit, "_groups", None)
    if gv is not None:
        keys.append(gv)
    keys.append("cohort")

    # Last non-null projection per cohort (sorted by dev).
    ult = (
        df.filter(pl.col("loss_proj").is_not_null()
                  & pl.col("premium_proj").is_not_null())
        .sort(keys + ["dev"])
        .group_by(keys, maintain_order=True)
        .agg(
            pl.col("loss_proj").last().alias("loss_ult"),
            pl.col("premium_proj").last().alias("premium_ult"),
        )
    )
    if ult.height == 0:
        return float("nan")

    total_loss = ult["loss_ult"].sum()
    total_exp = ult["premium_ult"].sum()
    if total_exp is None or not np.isfinite(total_exp) or total_exp <= 0:
        return float("nan")
    return float(total_loss) / float(total_exp)


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
    maturity_point: int | None = None,
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
        Which stability criterion drives the detected ``convergence_point``. One of:

        - ``"tail"`` (default, reserving-safe): tail drift over
          ``[k, dev_max]`` falls below ``max_drift``.
        - ``"window"``: local drift over ``[k, k + window - 1]``.
        - ``"slope"``: ``|OLS slope of Ratio ~ k|`` on ``[k, dev_max]``
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
        Drift window length (in dev steps). Default ``5``.
    maturity_point
        Pre-computed maturity point. When ``None``, auto-detected from
        the Ratio-link ATA.
    holdout_max
        Cap on holdout depth. When ``None``, set to
        ``max(window, (dev_max - maturity_point) // 2)``.
    min_n_cohorts
        Minimum cohorts required to compute dispersion. Default ``5``.
    **backtest_kwargs
        Forwarded to ``Backtest`` (e.g. ``estimator`` choice). Defaults
        to ``lr.Ratio(method="sa")``.

    Returns
    -------
    Convergence
        Result object exposing ``convergence_point``, ``maturity_point``, ``dev_max``, the
        candidate dev sequence and per-criterion diagnostics.
    """
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

    # 1. Resolve maturity_point from Ratio-link ATA if not given.
    if maturity_point is None:
        mat = (
            triangle.link(target="ratio", exposure=None, weight="premium")
            .ata()
            .maturity()
        )
        mat_k_raw = mat.maturity_point
        if isinstance(mat_k_raw, dict):
            vals = [v for v in mat_k_raw.values() if v is not None]
            if not vals:
                raise ValueError(
                    "Auto maturity_point failed: no mature Ratio link in any group. "
                    "Pass maturity_point=... explicitly (e.g. detect_convergence("
                    "triangle, maturity_point=4)) to bypass."
                )
            maturity_point = int(min(vals))
        else:
            if mat_k_raw is None:
                raise ValueError(
                    "Auto maturity_point failed: no mature Ratio link detected. "
                    "Pass maturity_point=... explicitly."
                )
            maturity_point = int(mat_k_raw)
    maturity_point = int(maturity_point)

    # 2. dev_max + candidate sequence.
    dev_max = int(triangle.to_polars()["dev"].max())
    if holdout_max is None:
        holdout_max = max(window, (dev_max - maturity_point) // 2)
    holdout_max = int(holdout_max)

    if dev_max - 2 >= maturity_point:
        dev_cand = list(range(maturity_point, dev_max - 1))
    else:
        dev_cand = []
        import warnings
        warnings.warn(
            f"No candidate dev points: maturity_point ({maturity_point}) + 2 > dev_max "
            f"({dev_max}). Returning convergence_point = None.",
            stacklevel=2,
        )

    # 3. Ratio per candidate via cached backtest.
    estimator = backtest_kwargs.pop("estimator", None) or Ratio(method="sa")
    ratio_arr = np.full(len(dev_cand), np.nan)
    cache: dict[int, float] = {}

    def _get_ratio(h: int) -> float:
        if h in cache:
            return cache[h]
        try:
            bt_fit = Backtest(
                estimator=estimator, holdout=h, metric="ratio"
            ).fit(triangle)
            val = _extract_portfolio_ratio(bt_fit)
        except Exception:
            val = float("nan")
        cache[h] = val
        return val

    for i, k in enumerate(dev_cand):
        h = dev_max - k
        if h < 1 or h > holdout_max:
            continue
        ratio_arr[i] = _get_ratio(h)

    # 4. revision (diagnostic only).
    revision = np.full(len(dev_cand), np.nan)
    if len(dev_cand) >= 2:
        revision[1:] = np.abs(np.diff(ratio_arr))

    # 5. drift_window: max - min over each length-`window` slice
    # [i, i+window-1]. NaN-propagating max/min naturally leave NaN where a
    # window has any gap (matching the original all-finite guard); slices
    # that would run past the end stay NaN.
    drift_window = np.full(len(dev_cand), np.nan)
    if len(dev_cand) >= window:
        from numpy.lib.stride_tricks import sliding_window_view
        w = sliding_window_view(ratio_arr, window)
        drift_window[: w.shape[0]] = np.max(w, axis=1) - np.min(w, axis=1)

    # 6. drift_tail over [i, end].
    drift_tail = np.full(len(dev_cand), np.nan)
    for i in range(len(dev_cand)):
        w = ratio_arr[i:]
        w = w[np.isfinite(w)]
        if len(w) >= 2:
            drift_tail[i] = float(np.max(w) - np.min(w))

    # 7. slope -- OLS slope of Ratio ~ dev on [i, end].
    slope_arr = np.full(len(dev_cand), np.nan)
    dev_cand_arr = np.array(dev_cand, dtype=float)
    for i in range(len(dev_cand)):
        slope_arr[i] = _ols_slope(dev_cand_arr[i:], ratio_arr[i:])

    # 8. dispersion at each candidate dev (group-collapsed via median).
    dispersion = np.full(len(dev_cand), np.nan)
    if dev_cand:
        disp_tbl = _compute_dispersion(triangle, min_n_cohorts=min_n_cohorts)
        if triangle.groups is not None:
            disp_tbl = (
                disp_tbl.group_by("dev")
                .agg(pl.col("dispersion").median().alias("dispersion"))
            )
        disp_map = dict(
            zip(disp_tbl["dev"].to_list(), disp_tbl["dispersion"].to_list())
        )
        for i, k in enumerate(dev_cand):
            v = disp_map.get(k)
            if v is not None:
                dispersion[i] = float(v)

    # 9. per-method pass tests.
    pass_d = np.isfinite(dispersion) & (dispersion < max_dispersion)
    pass_window = (
        np.isfinite(drift_window) & (drift_window < max_drift) & pass_d
    )
    pass_tail = (
        np.isfinite(drift_tail) & (drift_tail < max_drift) & pass_d
    )
    pass_slope = (
        np.isfinite(slope_arr) & (np.abs(slope_arr) < max_slope) & pass_d
    )
    pass_all = pass_window & pass_tail & pass_slope

    pass_arr = {
        "window": pass_window,
        "tail": pass_tail,
        "slope": pass_slope,
        "all": pass_all,
    }[method]

    # 10. first passing dev.
    if pass_arr.any():
        first_idx = int(np.argmax(pass_arr))
        conv_k: int | None = int(dev_cand[first_idx])
    else:
        conv_k = None

    return Convergence._from_arrays(
        triangle=triangle,
        method=method,
        conv_k=conv_k,
        mat_k=maturity_point,
        dev_max=dev_max,
        dev_cand=dev_cand,
        lr=ratio_arr,
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


class Convergence:
    """Result of :func:`detect_convergence`.

    Attributes
    ----------
    convergence_point : int | None
        First dev at which the chosen ``method``'s pass test fires.
    method : str
        Which criterion selected ``convergence_point``.
    maturity_point : int
        Maturity point used as the lower bound of the candidate window.
    dev_max : int
        Maximum observable dev.
    dev_cand : list[int]
        Candidate dev sequence ``[maturity_point, dev_max - 2]``.
    lr, revision, drift_window, drift_tail, slope, dispersion : ndarray
        Diagnostic series, one entry per ``dev_cand``.
    pass_window, pass_tail, pass_slope, pass : ndarray[bool]
        Per-criterion pass vectors plus the chosen criterion.
    """

    def __init__(self) -> None:
        self._output_type: str
        self.convergence_point: int | None
        self.method: str
        self.maturity_point: int
        self.dev_max: int
        self.dev_cand: list[int]
        self.lr: np.ndarray
        self.revision: np.ndarray
        self.drift_window: np.ndarray
        self.drift_tail: np.ndarray
        self.slope: np.ndarray
        self.dispersion: np.ndarray
        self.pass_window: np.ndarray
        self.pass_tail: np.ndarray
        self.pass_slope: np.ndarray
        self.pass_: np.ndarray
        self.max_drift: float
        self.max_slope: float
        self.max_dispersion: float
        self.window: int
        self.holdout_max: int
        self.min_n_cohorts: int

    @classmethod
    def _from_arrays(
        cls,
        triangle: "Triangle",
        method: str,
        conv_k: int | None,
        mat_k: int,
        dev_max: int,
        dev_cand: list[int],
        lr: np.ndarray,
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
        self.convergence_point = conv_k
        self.method = method
        self.maturity_point = mat_k
        self.dev_max = dev_max
        self.dev_cand = list(dev_cand)
        self.lr = lr
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

    def summary(self) -> Any:
        """One-row-per-candidate diagnostic table."""
        df = pl.DataFrame({
            "dev": self.dev_cand,
            "ratio": self.lr,
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
    def df(self) -> Any:
        return self.summary()

    def plot(
        self,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """5-panel convergence diagnostic plot (matplotlib).

        Stacked column of ``ratio``, ``drift_window``, ``drift_tail``,
        ``|slope|``, ``dispersion`` series across candidate dev
        cutoffs. Threshold hlines, maturity (``maturity_point``) dotted vline,
        and detected convergence (``convergence_point``) solid green vline are
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
        n = len(self.dev_cand)
        n_win = int(np.nansum(self.pass_window))
        n_tail = int(np.nansum(self.pass_tail))
        n_slope = int(np.nansum(self.pass_slope))
        bits = [
            f"method={self.method}",
            f"convergence_point={self.convergence_point}",
            f"maturity_point={self.maturity_point}",
            f"dev_max={self.dev_max}",
            f"candidates={n}",
            f"passes(window/tail/slope)={n_win}/{n_tail}/{n_slope}",
        ]
        return f"<Convergence: {', '.join(bits)}>"
