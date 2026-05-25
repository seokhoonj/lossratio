"""Calendar-diagonal hold-out backtest."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import mirror_output

if TYPE_CHECKING:
    from .triangle import Triangle


# ---------------------------------------------------------------------------
# Helpers — calendar index, masked Triangle, projected-loss column resolver
# ---------------------------------------------------------------------------


def _add_cal_idx(tri_df: pl.DataFrame, groups: str | None) -> pl.DataFrame:
    """Add a 1-based calendar index per cell (R parity).

    Calendar index counts the antidiagonal:
    ``cohort_idx + dev - 1`` with ``cohort_idx`` the 1-based dense
    rank of cohort within its group (R's ``frank(..., ties.method =
    "dense")`` convention). So the oldest cohort at dev = 1 lands on
    cal_idx = 1, and the maximum cal_idx equals the number
    of cohorts (or, for square-ish triangles, the number of devs).
    """
    sort_keys: list[str] = []
    if groups is not None:
        sort_keys.append(groups)
    sort_keys.append("cohort")

    over_keys: list[str] | None
    if groups is not None:
        over_keys = [groups]
        cohort_idx_expr = (
            pl.col("cohort").rank(method="dense").over(over_keys).cast(pl.Int64)
        )
    else:
        cohort_idx_expr = pl.col("cohort").rank(method="dense").cast(pl.Int64)

    return tri_df.with_columns(
        cohort_idx_expr.alias("__cohort_idx"),
    ).with_columns(
        (pl.col("__cohort_idx") + pl.col("dev") - 1).alias("cal_idx"),
    )


def _build_masked_df(
    tri_df: pl.DataFrame,
    holdout: int,
    groups: str | None,
) -> tuple[pl.DataFrame, pl.DataFrame]:
    """Mask the most recent ``holdout`` calendar diagonals.

    Returns ``(masked_df, mask_df)``:

    * ``masked_df`` has ``loss``, ``incr_loss``, ``premium``,
      ``incr_premium``, ``ratio``, ``incr_ratio`` set to ``None`` for
      cells whose calendar diagonal is among the top ``holdout``.
    * ``mask_df`` has the same shape with the original cell values
      preserved and a ``masked`` boolean column.
    """
    df = _add_cal_idx(tri_df, groups)

    # Determine the calendar-diagonal cutoff per group
    if groups is not None:
        max_per_group = df.group_by(groups).agg(
            pl.col("cal_idx").max().alias("__max_cal")
        )
        df = df.join(max_per_group, on=groups, how="left")
    else:
        max_cal = int(df["cal_idx"].max())
        df = df.with_columns(pl.lit(max_cal).alias("__max_cal"))

    df = df.with_columns(
        (pl.col("cal_idx") > pl.col("__max_cal") - holdout).alias("masked")
    )

    masked_df = df.with_columns(
        [
            pl.when(pl.col("masked")).then(None).otherwise(pl.col(c)).alias(c)
            for c in ("loss", "incr_loss", "premium", "incr_premium", "ratio", "incr_ratio")
            if c in df.columns
        ]
    ).drop("__cohort_idx", "__max_cal", "cal_idx", "masked")

    annotated_df = df.drop("__cohort_idx", "__max_cal")  # keeps cal_idx + masked
    return masked_df, annotated_df


_VALID_METRICS = ("ratio", "loss", "premium")


def _assert_leakage_safe_bootstrap(estimator: Any) -> None:
    """Reject an estimator carrying a *pre-built* ``BootstrapTriangle``.

    A backtest masks the most recent calendar diagonals and refits the
    estimator on the masked triangle per fold. The leakage-safe forms of
    the estimator's ``bootstrap`` config -- ``"auto"`` / ``True`` / a
    :class:`~lossratio.bootstrap.Bootstrap` config / a callable
    ``f(tri) -> BootstrapTriangle`` -- all *rebuild* the bootstrap from
    whatever triangle the estimator is fitted on, which inside a backtest
    is the **masked** triangle. No held-out cell ever enters the residual
    pool.

    A *pre-built* :class:`~lossratio.bootstrap.BootstrapTriangle`, by
    contrast, was fitted once on the unmasked triangle. Passing it
    straight through every fold would seed the residual pool with the
    held-out cells -- look-ahead leakage. There is no way to rebuild it
    per fold, so the only safe move is to reject it and direct the user
    to the callable form.

    Mirrors the guidance in R's ``backtest()`` documentation: prefer a
    ``function(tri) -> BootstrapTriangle`` over a pre-built object.
    """
    bootstrap = getattr(estimator, "bootstrap", None)
    if bootstrap is None:
        return
    # Late import to avoid a circular dependency at module load time.
    from .bootstrap import BootstrapTriangle

    if isinstance(bootstrap, BootstrapTriangle):
        raise ValueError(
            "estimator carries a pre-built BootstrapTriangle, which was "
            "fitted on the full (unmasked) triangle and would leak the "
            "held-out cells into every backtest fold's residual pool. "
            "Use a leakage-safe form instead: bootstrap='auto', a "
            "Bootstrap config (e.g. lr.Bootstrap(B=999)), or a callable "
            "function(tri) -> BootstrapTriangle -- each rebuilds the "
            "bootstrap on the masked triangle per fold."
        )


def _is_ratio_fit_estimator(estimator: Any) -> bool:
    """Ratio / ED jointly project loss / premium / lr; CL projects a single
    column. Distinguished by whether the estimator class is a ratio-fit.
    """
    # Late import to avoid circular dependency at module load time.
    from .ed import ED
    from .ratio import Ratio

    return isinstance(estimator, (Ratio, ED))


def _resolve_expected_column(
    metric: str, fit_df_columns: list[str], refit: Any
) -> str:
    """Map ``metric`` to the projection column on the refit output frame.

    Post-Phase-4b workers emit generic ``loss_proj`` columns (CL: loss
    side; ED: target = loss, plus ``ratio_proj`` as a downstream quantity).
    Ratio keeps legacy ``loss_proj`` / ``premium_proj`` / ``ratio_proj``.
    """
    if metric not in _VALID_METRICS:
        raise ValueError(
            f"metric must be one of {_VALID_METRICS}, got {metric!r}"
        )

    # Ratio estimator: legacy column names still emitted.
    legacy = {"ratio": "ratio_proj", "loss": "loss_proj", "premium": "premium_proj"}
    if legacy[metric] in fit_df_columns:
        return legacy[metric]

    # New worker schema (CL / ED): loss_proj corresponds to the
    # estimator's `target` role. Use ratio_proj for ratio metric on ED.
    if metric == "ratio" and "ratio_proj" in fit_df_columns:
        return "ratio_proj"
    target = getattr(refit, "target", None)
    if metric == target and "loss_proj" in fit_df_columns:
        return "loss_proj"
    exposure = getattr(refit, "exposure", None)
    if metric == exposure and "premium_proj" in fit_df_columns:
        return "premium_proj"

    raise ValueError(
        f"Refitted estimator output has no column for metric={metric!r}. "
        f"Available: {fit_df_columns}"
    )


def _resolve_incr_expected_column(
    metric: str, fit_df_columns: list[str], refit: Any
) -> str | None:
    """Map ``metric`` to the incremental projection column on the refit
    output frame, mirroring :func:`_resolve_expected_column` for the
    cumulative form.

    Returns ``None`` when the refit emits no incremental projection for
    the chosen metric (some estimator paths only expose cumulative).
    """
    legacy = {
        "ratio":   "incr_ratio_proj",
        "loss":    "incr_loss_proj",
        "premium": "incr_premium_proj",
    }
    if legacy[metric] in fit_df_columns:
        return legacy[metric]

    if metric == "ratio" and "incr_ratio_proj" in fit_df_columns:
        return "incr_ratio_proj"
    target = getattr(refit, "target", None)
    if metric == target and "incr_loss_proj" in fit_df_columns:
        return "incr_loss_proj"
    exposure = getattr(refit, "exposure", None)
    if metric == exposure and "incr_premium_proj" in fit_df_columns:
        return "incr_premium_proj"
    return None


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class Backtest:
    """Calendar-diagonal hold-out backtest of a fit estimator.

    Holds out the ``holdout`` most recent calendar diagonals from the
    triangle, refits the supplied estimator on the masked data, and
    compares the projection to the original observed values on the
    held-out cells. The cell-level metric (``ae_err``, "A/E Error")
    follows the standard actuarial A/E convention,
    ``ae_err = actual / expected - 1``, where positive values mark
    cells where the model under-projected (actual exceeded the
    projection) and negative values mark over-projection.

    Parameters
    ----------
    estimator
        An ``lr.CL``, ``lr.ED``, or ``lr.Ratio`` instance (or any
        estimator whose ``fit(triangle)`` returns a result class with
        a ``loss_proj`` column in ``.df``).

        If the estimator carries a ``bootstrap`` config, only the
        *rebuild-per-fit* forms are leakage-safe: ``bootstrap='auto'``,
        a :class:`~lossratio.bootstrap.Bootstrap` config, or a callable
        ``f(tri) -> BootstrapTriangle``. Each rebuilds the bootstrap on
        the masked triangle every fold, so no held-out cell enters the
        residual pool. A *pre-built*
        :class:`~lossratio.bootstrap.BootstrapTriangle` is rejected with
        a :class:`ValueError`: it was fitted on the unmasked triangle and
        would leak the hold-out cells. The bootstrap only ever touches
        the SE / CI columns, never the point projection, so a
        bootstrap-configured estimator produces the *same* ``ae_err`` as
        an analytical one -- the only effect is extra compute per fold.
    holdout
        Number of most recent calendar diagonals to mask.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> bt = lr.Backtest(estimator=lr.Ratio(method="sa"), holdout=6).fit(tri)
    >>> bt.ae_err
    >>> bt.col_summary
    >>> bt.diag_summary
    """

    def __init__(
        self,
        estimator: Any,
        holdout: int = 6,
        metric: str = "ratio",
    ) -> None:
        if holdout < 1:
            raise ValueError(f"holdout must be >= 1, got {holdout}")
        if not hasattr(estimator, "fit"):
            raise TypeError(
                f"estimator must implement .fit(triangle); got {type(estimator).__name__}"
            )
        # Reject a pre-built BootstrapTriangle on the estimator -- it would
        # leak held-out cells into every fold (see helper docstring).
        _assert_leakage_safe_bootstrap(estimator)
        if metric not in _VALID_METRICS:
            raise ValueError(
                f"metric must be one of {_VALID_METRICS}, got {metric!r}"
            )
        # Ratio / ED are ratio-fits — only `lr` is a meaningful scoring lane;
        # use CL directly to backtest the loss or premium projection.
        if _is_ratio_fit_estimator(estimator) and metric != "ratio":
            raise ValueError(
                f"estimator is a ratio-fit ({type(estimator).__name__}); "
                f"only metric='ratio' is supported. Use lr.CL() instead to "
                f"backtest the loss or premium projection directly."
            )
        self.estimator = estimator
        self.holdout = holdout
        self.metric = metric

    def fit(self, triangle: "Triangle") -> "BacktestFit":
        return BacktestFit._from_triangle(triangle, self)


class BacktestFit:
    """Result of a calendar-diagonal hold-out backtest.

    Properties
    ----------
    ae_err : DataFrame
        Per-cell hold-out comparison
        ``[groups?, cohort, dev, cal_idx, actual, expected, ae_err]``.
        ``ae_err = actual / expected - 1`` (signed relative error;
        positive = under-projection, negative = over-projection).
    col_summary : DataFrame
        Aggregated by dev:
        ``[groups?, dev, n, ae_err_mean, ae_err_med, ae_err_wt]``.
        ``ae_err_wt = sum(actual - expected) / sum(expected)`` is the
        exposure-weighted pooled A/E - 1.
    diag_summary : DataFrame
        Aggregated by cal_idx with the same statistics as
        ``col_summary``.
    fit :
        The refitted estimator's result (e.g. CLFit / EDFit / RatioFit).
    """

    def __init__(self) -> None:
        self._ae_err: pl.DataFrame
        self._col_summary: pl.DataFrame
        self._diag_summary: pl.DataFrame
        self._refit: Any
        self._output_type: str
        self._groups: str | None
        self._cohort: str
        self._dev: str
        self._triangle: "Triangle"
        self.holdout: int
        self.estimator: Any

    @classmethod
    def _from_triangle(cls, triangle: "Triangle", bt: "Backtest") -> "BacktestFit":
        from .triangle import Triangle

        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self._cohort = triangle._cohort
        self._dev = triangle._dev
        self._triangle = triangle
        self.holdout = bt.holdout
        self.estimator = bt.estimator

        # 1. Mask the most recent calendar diagonals
        masked_df, annotated_df = _build_masked_df(
            triangle._df, bt.holdout, triangle._groups
        )

        # 2. Build a masked Triangle (same metadata, masked cells)
        masked_tri = Triangle._from_masked(triangle, masked_df)

        # 3. Refit estimator on masked Triangle
        refit = bt.estimator.fit(masked_tri)
        self._refit = refit

        refit_df = refit.to_polars()
        exp_col = _resolve_expected_column(bt.metric, refit_df.columns, refit)
        incr_exp_col = _resolve_incr_expected_column(
            bt.metric, refit_df.columns, refit
        )
        self.metric = bt.metric

        # 4. Build per-cell A/E Error by joining masked cells with refit
        keys: list[str] = []
        if triangle._groups is not None:
            keys.append(triangle._groups)
        keys.extend(["cohort", "dev"])

        # `actual` is the cumulative column on the original Triangle that
        # corresponds to the chosen scoring lane; `incr_actual` is its
        # incremental sibling (always present on a Triangle).
        actual_col = bt.metric                  # "ratio" / "loss" / "premium"
        incr_actual_col = f"incr_{bt.metric}"   # always present per Triangle schema

        sel_actual = keys + ["cal_idx", actual_col]
        if incr_actual_col in annotated_df.columns:
            sel_actual.append(incr_actual_col)
        held_out = annotated_df.filter(pl.col("masked")).select(sel_actual)
        renames: dict[str, str] = {actual_col: "actual"}
        if incr_actual_col in held_out.columns:
            renames[incr_actual_col] = "incr_actual"
        held_out = held_out.rename(renames)

        # Pull cumulative + (optional) incremental projection columns.
        sel_exp = keys + [exp_col]
        if incr_exp_col is not None:
            sel_exp.append(incr_exp_col)
        refit_exp = refit_df.select(sel_exp).rename({exp_col: "expected"})
        if incr_exp_col is not None:
            refit_exp = refit_exp.rename({incr_exp_col: "incr_expected"})

        # Drop unreachable cells: cohorts whose observations are wholly
        # within the held-out diagonals have no anchor for projection,
        # so the refit returns NaN at those cells.
        ae_err = (
            held_out.join(refit_exp, on=keys, how="inner")
            .filter(
                pl.col("expected").is_not_null()
                & pl.col("expected").is_finite()
            )
            .with_columns(
                (pl.col("actual") - pl.col("expected")).alias("aeg"),
                pl.when(
                    pl.col("expected").is_finite() & (pl.col("expected") != 0)
                )
                .then(pl.col("actual") / pl.col("expected") - 1)
                .otherwise(None)
                .alias("ae_err"),
            )
        )

        # Incremental view — emitted only when both `incr_actual` and
        # `incr_expected` survived the join. R parity: NA on edge cells
        # is acceptable on the incremental view (`incr_aeg` /
        # `incr_ae_err` may be NA when the projection has no upstream
        # cumulative anchor at dev = 1).
        has_incr = ("incr_actual" in ae_err.columns) and (
            "incr_expected" in ae_err.columns
        )
        if has_incr:
            ae_err = ae_err.with_columns(
                (pl.col("incr_actual") - pl.col("incr_expected")).alias("incr_aeg"),
                pl.when(
                    pl.col("incr_expected").is_finite()
                    & (pl.col("incr_expected") != 0)
                )
                .then(pl.col("incr_actual") / pl.col("incr_expected") - 1)
                .otherwise(None)
                .alias("incr_ae_err"),
            )

        # Final column order mirrors R's `setcolorder()` in `backtest()`.
        col_order = keys + ["actual", "expected", "aeg", "ae_err"]
        if has_incr:
            col_order += [
                "incr_actual", "incr_expected", "incr_aeg", "incr_ae_err",
            ]
        col_order.append("cal_idx")
        ae_err = ae_err.select(col_order)
        self._ae_err = ae_err.sort(keys + ["cal_idx"])

        # 5. Summaries -- mean / median / weighted A/E - 1 per dev or per
        #    calendar diagonal, with `incr_*` companions when the
        #    incremental projection is available.
        col_keys: list[str] = []
        if triangle._groups is not None:
            col_keys.append(triangle._groups)
        col_keys.append("dev")

        self._col_summary = self._aggregate_ae_err(
            ae_err, col_keys, has_incr
        ).sort(col_keys)

        diag_keys: list[str] = []
        if triangle._groups is not None:
            diag_keys.append(triangle._groups)
        diag_keys.append("cal_idx")

        self._diag_summary = self._aggregate_ae_err(
            ae_err, diag_keys, has_incr
        ).sort(diag_keys)

        return self

    @staticmethod
    def _aggregate_ae_err(
        ae_err: pl.DataFrame, by_cols: list[str], has_incr: bool
    ) -> pl.DataFrame:
        """Aggregate cell-level A/E Error to a per-key summary.

        Mirrors R's :func:`.backtest_aggregate`. Always emits the
        cumulative ``(n, aeg_*, ae_err_*)`` block; emits the
        ``incr_*`` block iff the refit exposed an incremental
        projection column.
        """
        aggs: list[pl.Expr] = [
            pl.len().alias("n"),
            pl.col("aeg").mean().alias("aeg_mean"),
            pl.col("aeg").median().alias("aeg_med"),
            pl.col("ae_err").mean().alias("ae_err_mean"),
            pl.col("ae_err").median().alias("ae_err_med"),
            (
                (pl.col("actual") - pl.col("expected")).sum()
                / pl.col("expected").sum()
            ).alias("ae_err_wt"),
        ]
        if has_incr:
            aggs += [
                pl.col("incr_aeg").mean().alias("incr_aeg_mean"),
                pl.col("incr_aeg").median().alias("incr_aeg_med"),
                pl.col("incr_ae_err").mean().alias("incr_ae_err_mean"),
                pl.col("incr_ae_err").median().alias("incr_ae_err_med"),
                (
                    (pl.col("incr_actual") - pl.col("incr_expected")).sum()
                    / pl.col("incr_expected").sum()
                ).alias("incr_ae_err_wt"),
            ]
        return ae_err.group_by(by_cols).agg(aggs)

    @property
    def ae_err(self):
        return mirror_output(self._ae_err, self._output_type)

    @property
    def col_summary(self):
        return mirror_output(self._col_summary, self._output_type)

    @property
    def diag_summary(self):
        return mirror_output(self._diag_summary, self._output_type)

    @property
    def fit(self):
        return self._refit

    def plot(
        self,
        type: str = "col",
        cell_type: str = "cumulative",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Backtest A/E error plot, backed by matplotlib.

        Parameters
        ----------
        type
            ``"col"`` (default; aggregated by development period),
            ``"diag"`` (aggregated by calendar diagonal), or
            ``"cell"`` (per-cell scatter / line, one line per cohort).
        cell_type
            ``"cumulative"`` (default; uses ``ae_err_*`` columns) or
            ``"incremental"`` (uses ``incr_ae_err_*`` columns).
        nrow, ncol
            Facet layout when ``groups`` is set.
        figsize
            Passed to ``plt.subplots``.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from ._backtest_vis import plot_backtest
        return plot_backtest(
            self, type=type, cell_type=cell_type,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )

    def plot_triangle(
        self,
        cell_type: str = "cumulative",
        label_size: float = 7.0,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """A/E error heatmap on the held-out wedge, backed by matplotlib.

        Diverging palette: red marks under-projection (actual >
        expected), blue marks over-projection. Faceted by group.

        Parameters
        ----------
        cell_type
            ``"cumulative"`` (default) or ``"incremental"``.
        label_size
            Matplotlib font size for the per-cell percent labels.
        nrow, ncol
            Facet layout.
        figsize
            Passed to ``plt.subplots``.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from ._backtest_vis import plot_triangle_backtest
        return plot_triangle_backtest(
            self,
            cell_type=cell_type,
            label_size=label_size,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )

    def __repr__(self) -> str:
        n_cells = self._ae_err.height
        est_name = type(self.estimator).__name__
        return f"<BacktestFit: estimator={est_name}, holdout={self.holdout}, n_held_out_cells={n_cells}>"
