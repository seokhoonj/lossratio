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
        cohort_idx_expr.alias("_cohort_idx"),
    ).with_columns(
        (pl.col("_cohort_idx") + pl.col("dev") - 1).alias("cal_idx"),
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
            pl.col("cal_idx").max().alias("_max_cal")
        )
        df = df.join(max_per_group, on=groups, how="left")
    else:
        max_cal = int(df["cal_idx"].max())
        df = df.with_columns(pl.lit(max_cal).alias("_max_cal"))

    df = df.with_columns(
        (pl.col("cal_idx") > pl.col("_max_cal") - holdout).alias("masked")
    )

    masked_df = df.with_columns(
        [
            pl.when(pl.col("masked")).then(None).otherwise(pl.col(c)).alias(c)
            for c in ("loss", "incr_loss", "premium", "incr_premium", "ratio", "incr_ratio")
            if c in df.columns
        ]
    ).drop("_cohort_idx", "_max_cal", "cal_idx", "masked")

    annotated_df = df.drop("_cohort_idx", "_max_cal")  # keeps cal_idx + masked
    return masked_df, annotated_df


_VALID_TARGETS = ("ratio", "loss", "premium")


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
    """Only ``Ratio`` is a ratio-fit -- it composes loss + premium into a
    ``ratio_proj`` column. The loss models (ChainLadder / ExposureDriven /
    StageAdaptive) project loss (and premium) but carry no ``ratio_proj``,
    so they are scored on ``target="loss"`` / ``"premium"`` directly.
    """
    # Late import to avoid circular dependency at module load time.
    from .loss_ratio import LossRatio

    return isinstance(estimator, LossRatio)


def _resolve_expected_column(target: str, fit_df_columns: list[str]) -> str:
    """Map ``target`` to the projection column on the refit output frame.

    The loss models (ChainLadder / ExposureDriven / StageAdaptive) emit a
    ``LossFit`` carrying ``loss_proj`` + ``premium_proj``; ``LossRatio``
    emits a ``RatioFit`` that additionally carries ``ratio_proj``. So a
    role-named direct lookup suffices; ``target="ratio"`` is only
    reachable for a LossRatio backtest (a loss model has no ``ratio_proj``
    and raises here, which the estimator guard anticipates).
    """
    if target not in _VALID_TARGETS:
        raise ValueError(
            f"target must be one of {_VALID_TARGETS}, got {target!r}"
        )
    direct = {"ratio": "ratio_proj", "loss": "loss_proj", "premium": "premium_proj"}
    if direct[target] in fit_df_columns:
        return direct[target]
    raise ValueError(
        f"Refitted estimator output has no column for target={target!r}. "
        f"Available: {fit_df_columns}"
    )


def _resolve_incr_expected_column(
    target: str, fit_df_columns: list[str]
) -> str | None:
    """Map ``target`` to the incremental projection column, mirroring
    :func:`_resolve_expected_column`. Returns ``None`` when the refit
    emits no incremental projection for the chosen target.
    """
    direct = {
        "ratio":   "incr_ratio_proj",
        "loss":    "incr_loss_proj",
        "premium": "incr_premium_proj",
    }
    col = direct[target]
    return col if col in fit_df_columns else None


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
        An ``lr.ChainLadder`` / ``lr.ExposureDriven`` / ``lr.StageAdaptive``
        / ``lr.LossRatio`` instance (or any estimator whose
        ``fit(triangle)`` returns a result class with a ``loss_proj``
        column in ``.df``).

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
    target
        Which projection to score: ``"ratio"`` (default), ``"loss"``,
        or ``"premium"``. A ratio-fit estimator (``lr.LossRatio``) only
        supports ``target="ratio"``; use a loss model (``lr.ChainLadder``
        / ``lr.ExposureDriven`` / ``lr.StageAdaptive``) to score the loss
        or premium projection directly.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> bt = lr.Backtest(estimator=lr.LossRatio(method="sa"), holdout=6).fit(tri)
    >>> bt.ae_err
    >>> bt.col_summary
    >>> bt.diag_summary
    """

    def __init__(
        self,
        estimator: Any,
        holdout: int = 6,
        target: str = "ratio",
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
        if target not in _VALID_TARGETS:
            raise ValueError(
                f"target must be one of {_VALID_TARGETS}, got {target!r}"
            )
        # Only LossRatio is a ratio-fit -- the ratio lane is its meaningful
        # target; use a loss model to backtest the loss / premium projection.
        if _is_ratio_fit_estimator(estimator) and target != "ratio":
            raise ValueError(
                f"estimator is a ratio-fit ({type(estimator).__name__}); "
                f"only target='ratio' is supported. Use a loss model "
                f"(lr.ChainLadder() / lr.ExposureDriven() / "
                f"lr.StageAdaptive()) instead to backtest the loss or "
                f"premium projection directly."
            )
        self.estimator = estimator
        self.holdout = holdout
        self.target = target

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
        exp_col = _resolve_expected_column(bt.target, refit_df.columns)
        incr_exp_col = _resolve_incr_expected_column(bt.target, refit_df.columns)
        self.target = bt.target

        # 4. Build per-cell A/E Error by joining masked cells with refit
        keys: list[str] = []
        if triangle._groups is not None:
            keys.append(triangle._groups)
        keys.extend(["cohort", "dev"])

        # `actual` is the cumulative column on the original Triangle that
        # corresponds to the chosen scoring lane; `incr_actual` is its
        # incremental sibling (always present on a Triangle).
        actual_col = bt.target                  # "ratio" / "loss" / "premium"
        incr_actual_col = f"incr_{bt.target}"   # always present per Triangle schema

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
        view: str = "value",
        cell_type: str = "cumulative",
        label_size: float = 7.0,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
        *,
        recent: int | None = None,
        regime: Any = None,
        maturity: Any = None,
    ) -> Any:
        """A/E error heatmap (``view='value'``) or cell-status
        heatmap (``view='usage'``), backed by matplotlib.

        Parameters
        ----------
        view
            ``"value"`` (default; diverging A/E-error heatmap on the
            held-out wedge) or ``"usage"`` (categorical status
            heatmap of training / held-out / regime-excluded /
            future cells, driven by the masking + filter metadata
            inherited from this Backtest's estimator).
        cell_type
            (``view='value'`` only) ``"cumulative"`` (default; uses
            ``ae_err``) or ``"incremental"`` (uses ``incr_ae_err``).
        label_size
            (``view='value'`` only) matplotlib font size for the
            per-cell percent labels.
        nrow, ncol
            Facet layout.
        figsize
            Passed to ``plt.subplots``.
        recent, regime, maturity
            (``view='usage'`` only) override values for the filter
            overlays. By default the usage view reads ``recent`` and
            ``regime`` from the estimator that drove the backtest
            (``recent`` from the loss model / ``LossRatio``; ``regime``
            from the loss-side of ``LossRatio``, or ``regime`` of the
            loss model); ``maturity`` defaults to ``None`` -- callers
            who want a maturity hline overlay must pass an explicit
            :class:`Maturity` instance or scalar (R parity:
            R's ``backtest()`` runs a 2-pass ATA fit to detect
            maturity automatically; Python defers that to the
            caller).

        Returns
        -------
        matplotlib.figure.Figure
        """
        if view not in ("value", "usage"):
            raise ValueError(
                f"`view` must be 'value' or 'usage'; got {view!r}."
            )
        if view == "value":
            from ._backtest_vis import plot_triangle_backtest
            return plot_triangle_backtest(
                self,
                cell_type=cell_type,
                label_size=label_size,
                nrow=nrow, ncol=ncol, figsize=figsize,
            )
        # view == "usage": forward to the Triangle-side usage
        # renderer with `holdout=self.holdout` and filter args
        # inherited from `self.estimator` (overridable via kwargs).
        # The Triangle renderer resolves `"auto"` for both regime and
        # maturity via inline `detect_*` calls.
        from ._triangle_vis import _plot_triangle_usage
        eff_recent = recent if recent is not None else self._infer_recent()
        eff_regime = regime if regime is not None else self._infer_regime()
        eff_maturity = maturity if maturity is not None else self._infer_maturity()
        return _plot_triangle_usage(
            self._triangle,
            recent=eff_recent,
            regime=eff_regime,
            holdout=self.holdout,
            maturity=eff_maturity,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )

    def _infer_recent(self) -> int | None:
        """Extract `recent` from `self.estimator`, if present."""
        return getattr(self.estimator, "recent", None)

    def _infer_regime(self) -> Any:
        """Extract the loss-side regime from `self.estimator`, if any.

        For ``lr.LossRatio``, prefer ``loss_regime`` (its loss-side);
        for the loss models (ChainLadder / ExposureDriven /
        StageAdaptive), ``regime``. The
        Triangle renderer accepts ``"auto"`` directly and runs
        :meth:`Triangle.detect_regime` inline, so a literal
        ``"auto"`` is forwarded as-is.
        """
        est = self.estimator
        if hasattr(est, "loss_regime"):
            return getattr(est, "loss_regime")
        return getattr(est, "regime", None)

    def _infer_maturity(self) -> Any:
        """Extract maturity from `self.estimator`, if any.

        ``lr.StageAdaptive`` / ``lr.LossRatio`` carry a ``maturity`` slot.
        The Triangle renderer accepts ``"auto"`` directly and runs
        :meth:`Triangle.detect_maturity` inline, so a literal
        ``"auto"`` is forwarded as-is. ChainLadder / ExposureDriven have
        no maturity concept and return ``None``.
        """
        return getattr(self.estimator, "maturity", None)

    def __repr__(self) -> str:
        n_cells = self._ae_err.height
        est_name = type(self.estimator).__name__
        return f"<BacktestFit: estimator={est_name}, holdout={self.holdout}, n_held_out_cells={n_cells}>"
