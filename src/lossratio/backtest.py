"""Calendar-diagonal hold-out backtest -- single-fold (_FoldBacktest/_FoldFit) and
rolling-origin (Backtest/BacktestFit) public API."""

from __future__ import annotations

import math
from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import collapse_groups, mirror_output, normalize_groups

if TYPE_CHECKING:
    from ._types import RegimeArg
    from .triangle import Triangle


# ---------------------------------------------------------------------------
# Helpers — calendar index, masked Triangle, projected-loss column resolver
# ---------------------------------------------------------------------------


def _add_cal_idx(tri_df: pl.DataFrame, groups: str | list[str] | None) -> pl.DataFrame:
    """Add a 1-based calendar index per cell.

    Calendar index counts the antidiagonal:
    ``cohort_idx + duration - 1`` with ``cohort_idx`` the 1-based dense
    rank of cohort within its group (ties share the same rank). So the
    oldest cohort at duration = 1 lands on cal_idx = 1, and the maximum
    cal_idx equals the number of cohorts (or, for square-ish triangles,
    the number of durations).
    """
    group_cols = normalize_groups(groups)
    if group_cols:
        cohort_idx_expr = (
            pl.col("cohort").rank(method="dense").over(group_cols).cast(pl.Int64)
        )
    else:
        cohort_idx_expr = pl.col("cohort").rank(method="dense").cast(pl.Int64)

    return tri_df.with_columns(
        cohort_idx_expr.alias("_cohort_idx"),
    ).with_columns(
        (pl.col("_cohort_idx") + pl.col("duration") - 1).alias("cal_idx"),
    )


def _build_masked_df(
    tri_df: pl.DataFrame,
    holdout: int,
    groups: str | list[str] | None,
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
    group_cols = normalize_groups(groups)
    if group_cols:
        max_per_group = df.group_by(group_cols).agg(
            pl.col("cal_idx").max().alias("_max_cal")
        )
        df = df.join(max_per_group, on=group_cols, how="left")
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
    to the callable form. The guidance is to prefer a
    ``function(tri) -> BootstrapTriangle`` over a pre-built object.
    """
    bootstrap = getattr(estimator, "bootstrap", None)
    if bootstrap is None:
        return
    # The redesign estimators carry uncertainty via `uncertainty=` (rebuilt per
    # fold on the masked triangle, leakage-safe); a pre-built `.bootstrap` object
    # fitted on the full triangle would leak the held-out cells, so reject it.
    raise ValueError(
        "estimator carries a pre-built bootstrap object fitted on the full "
        "(unmasked) triangle, which would leak the held-out cells into every "
        "backtest fold. Use an `uncertainty=ResidualBootstrap(...)` strategy "
        "instead -- it rebuilds the bootstrap on the masked triangle per fold."
    )


def _resolve_expected_column(target: str, fit_df_columns: list[str]) -> str:
    """Map ``target`` to the projection column on the refit output frame.

    Every estimator emits a ``LossFit`` carrying ``loss_proj`` /
    ``premium_proj`` / ``ratio_proj``, so a role-named direct lookup
    resolves all three targets.
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


def _resolve_se_column(target: str, fit_df_columns: list[str]) -> str | None:
    """Map ``target`` to the refit's projection standard-error column.

    The loss / premium projections carry ``{role}_total_se`` (process +
    parameter); the ratio composition carries ``ratio_se``. Returns ``None``
    when the refit emitted no SE for the target -- a point-only fit (charter
    Sec.5.1: no uncertainty columns), which leaves the coverage lane absent
    rather than fabricated.
    """
    direct = {
        "ratio":   "ratio_se",
        "loss":    "loss_total_se",
        "premium": "premium_total_se",
    }
    col = direct[target]
    return col if col in fit_df_columns else None


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class _FoldBacktest:
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
        A ``lr.PooledLoss`` / ``lr.CredibleLoss`` / ``lr.SmoothLoss`` /
        ``lr.ChainLadder`` instance (or any estimator whose ``fit(triangle)``
        returns a ``LossFit`` carrying ``loss_proj`` / ``premium_proj`` /
        ``ratio_proj``).

        Attach uncertainty with ``uncertainty=lr.ResidualBootstrap(...)``:
        it is rebuilt on the masked triangle every fold, so no held-out
        cell enters the residual pool, and it only touches the SE / CI
        columns -- the point projection (and hence ``ae_err``) is the same
        as an analytical fit, at the cost of extra compute per fold.
    holdout
        Number of most recent calendar diagonals to mask.
    target
        Which projection to score: ``"ratio"`` (default), ``"loss"``, or
        ``"premium"`` -- every estimator carries all three.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> bt = lr._FoldBacktest(estimator=lr.CredibleLoss(), holdout=6, target="loss").fit(tri)
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
        # Every redesign estimator carries loss_proj / premium_proj / ratio_proj,
        # so all three targets are resolvable from the refit frame -- no
        # ratio-only estimator to special-case.
        self.estimator = estimator
        self.holdout = holdout
        self.target = target

    def fit(self, triangle: "Triangle") -> "_FoldFit":
        return _FoldFit._from_triangle(triangle, self)


class _FoldFit:
    """Result of a calendar-diagonal hold-out backtest.

    Properties
    ----------
    ae_err : DataFrame
        Per-cell hold-out comparison
        ``[groups?, cohort, duration, actual, expected, aeg, ae_err,
        incr_actual, incr_expected, incr_aeg, incr_ae_err, cal_idx]``.
        ``aeg = actual - expected`` (Actual-minus-Expected Gap; signed,
        target units) and
        ``ae_err = actual / expected - 1`` (signed relative error;
        positive = under-projection, negative = over-projection). The
        ``incr_`` columns are the per-period (incremental) counterparts.
    col_summary : DataFrame
        Aggregated by duration:
        ``[groups?, duration, n, ae_err_mean, ae_err_med, ae_err_wt]``.
        ``ae_err_wt = sum(actual - expected) / sum(expected)`` is the
        exposure-weighted pooled A/E - 1.
    diag_summary : DataFrame
        Aggregated by cal_idx with the same statistics as
        ``col_summary``.
    fit :
        The refitted estimator's result (a ``LossFit``).
    """

    def __init__(self) -> None:
        raise TypeError(
            "_FoldFit is an internal per-fold building block. Use Backtest(...).fit(triangle)."
        )

    @classmethod
    def _from_triangle(cls, triangle: "Triangle", bt: "_FoldBacktest") -> "_FoldFit":
        from .triangle import Triangle

        self = cls.__new__(cls)
        # A covariate estimator projects at the REPORTING grain
        # (triangle.groups - covariates): it pools the duration shape across the
        # covariate columns and reports them as level effects. The actual cells,
        # masking and scoring therefore live at the reporting grain, while the
        # REFIT keeps the finer triangle (its sub-cells carry the covariate
        # effects). Without covariates the reporting grain IS the triangle grain.
        covs = getattr(bt.estimator, "covariates", None)
        if covs:
            report_cols = [
                g for g in normalize_groups(triangle._groups)
                if g not in set(covs)
            ]
            report_tri = triangle.collapse(
                collapse_groups(report_cols) if report_cols else None
            )
        else:
            report_tri = triangle

        self._output_type = report_tri._output_type
        self._groups = report_tri._groups
        self._cohort = report_tri._cohort
        self._duration = report_tri._duration
        self._triangle = report_tri
        self.holdout = bt.holdout
        self.estimator = bt.estimator

        # 1. Mask the most recent calendar diagonals (reporting-grain actuals).
        masked_df, annotated_df = _build_masked_df(
            report_tri._df, bt.holdout, report_tri._groups
        )

        # 2./3. Refit on the masked triangle. The covariate fit needs the FINER
        # triangle, but the hold-out is defined ONCE at the reporting grain (the
        # scoring grain): null exactly the sub-cells of the held-out report cells.
        # Masking the finer triangle on its OWN cal_idx would diverge from the
        # report grain when a coverage's covariate levels have unequal cohort
        # spans (the per-level dense rank differs), leaving a report cell
        # half-observed -- which collapse would silently under-count. A plain fit
        # uses the reporting-grain masked triangle directly.
        if covs:
            report_group_cols = normalize_groups(report_tri._groups)
            held = (
                annotated_df.filter(pl.col("masked"))
                .select([*report_group_cols, "cohort", "duration"])
                .unique()
                .with_columns(pl.lit(True).alias("_held"))
            )
            null_cols = [
                c for c in ("loss", "incr_loss", "premium", "incr_premium",
                            "ratio", "incr_ratio")
                if c in triangle._df.columns
            ]
            fine_masked_df = (
                triangle._df
                .join(held, on=[*report_group_cols, "cohort", "duration"], how="left")
                .with_columns(
                    [pl.when(pl.col("_held")).then(None).otherwise(pl.col(c)).alias(c)
                     for c in null_cols]
                )
                .drop("_held")
            )
            refit = bt.estimator.fit(Triangle._from_masked(triangle, fine_masked_df))
        else:
            refit = bt.estimator.fit(Triangle._from_masked(triangle, masked_df))
        self._refit = refit

        refit_df = refit.to_polars()
        exp_col = _resolve_expected_column(bt.target, refit_df.columns)
        incr_exp_col = _resolve_incr_expected_column(bt.target, refit_df.columns)
        se_col = _resolve_se_column(bt.target, refit_df.columns)
        self.target = bt.target

        # 4. Build per-cell A/E Error by joining masked cells with refit
        keys: list[str] = [*normalize_groups(self._groups), "cohort", "duration"]

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

        # Pull cumulative + (optional) incremental projection columns, plus the
        # projection SE (when the refit carried uncertainty) for the coverage
        # lane.
        sel_exp = keys + [exp_col]
        if incr_exp_col is not None:
            sel_exp.append(incr_exp_col)
        if se_col is not None:
            sel_exp.append(se_col)
        refit_exp = refit_df.select(sel_exp).rename({exp_col: "expected"})
        if incr_exp_col is not None:
            refit_exp = refit_exp.rename({incr_exp_col: "incr_expected"})
        if se_col is not None:
            refit_exp = refit_exp.rename({se_col: "expected_se"})

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
        # `incr_expected` survived the join. Null on edge cells is
        # acceptable on the incremental view (`incr_aeg` /
        # `incr_ae_err` may be null when the projection has no upstream
        # cumulative anchor at duration = 1).
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

        # Anchored-lane support (charter Sec.6.3): the cohort's observed
        # cumulative target at the as-of boundary (its last non-masked
        # duration) -- the origin baseline the anchored metric lane rebases
        # against (`anchored_actual = actual - anchor_value`). Constant per
        # cohort per origin; a cohort wholly inside the held-out diagonals has
        # no non-masked cell and gets a null anchor (already dropped above as
        # unreachable).
        group_cols = normalize_groups(self._groups)
        anchor = (
            annotated_df.filter(~pl.col("masked"))
            .group_by([*group_cols, "cohort"])
            .agg(
                pl.col(actual_col).sort_by("duration").last().alias("anchor_value")
            )
        )
        ae_err = ae_err.join(anchor, on=[*group_cols, "cohort"], how="left")

        # Fixed final column order for the per-cell table.
        col_order = keys + ["actual", "expected", "aeg", "ae_err"]
        if has_incr:
            col_order += [
                "incr_actual", "incr_expected", "incr_aeg", "incr_ae_err",
            ]
        if se_col is not None:
            col_order.append("expected_se")
        col_order += ["anchor_value", "cal_idx"]
        ae_err = ae_err.select(col_order)
        self._ae_err = ae_err.sort(keys + ["cal_idx"])

        # 5. Summaries -- mean / median / weighted A/E - 1 per duration or per
        #    calendar diagonal, with `incr_*` companions when the
        #    incremental projection is available.
        col_keys: list[str] = [*normalize_groups(self._groups), "duration"]

        self._col_summary = self._aggregate_ae_err(
            ae_err, col_keys, has_incr
        ).sort(col_keys)

        diag_keys: list[str] = [*normalize_groups(self._groups), "cal_idx"]

        self._diag_summary = self._aggregate_ae_err(
            ae_err, diag_keys, has_incr
        ).sort(diag_keys)

        return self

    @staticmethod
    def _aggregate_ae_err(
        ae_err: pl.DataFrame, by_cols: list[str], has_incr: bool
    ) -> pl.DataFrame:
        """Aggregate cell-level A/E Error to a per-key summary.

        Always emits the cumulative ``(n, aeg_*, ae_err_*)`` block;
        emits the ``incr_*`` block iff the refit exposed an incremental
        projection column.
        """
        aggs: list[pl.Expr] = [
            pl.len().alias("n"),
            pl.col("aeg").mean().alias("aeg_mean"),
            pl.col("aeg").median().alias("aeg_med"),
            pl.col("ae_err").mean().alias("ae_err_mean"),
            pl.col("ae_err").median().alias("ae_err_med"),
            (
                pl.when(pl.col("expected").sum() != 0)
                .then(
                    (pl.col("actual") - pl.col("expected")).sum()
                    / pl.col("expected").sum()
                )
                .otherwise(None)
            ).alias("ae_err_wt"),
        ]
        if has_incr:
            aggs += [
                pl.col("incr_aeg").mean().alias("incr_aeg_mean"),
                pl.col("incr_aeg").median().alias("incr_aeg_med"),
                pl.col("incr_ae_err").mean().alias("incr_ae_err_mean"),
                pl.col("incr_ae_err").median().alias("incr_ae_err_med"),
                (
                    pl.when(pl.col("incr_expected").sum() != 0)
                    .then(
                        (pl.col("incr_actual") - pl.col("incr_expected")).sum()
                        / pl.col("incr_expected").sum()
                    )
                    .otherwise(None)
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
        kind: str = "col",
        cell_type: str = "cumulative",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Backtest A/E error plot, backed by matplotlib.

        Parameters
        ----------
        kind
            The aggregation the error is viewed over: ``"col"`` (default;
            by duration), ``"diag"`` (by calendar diagonal), or
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
            self, kind=kind, cell_type=cell_type,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )

    def plot_triangle(
        self,
        kind: str = "value",
        cell_type: str = "cumulative",
        label_size: float = 7.0,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
        *,
        recent: int | None = None,
        regime: "RegimeArg" = None,
        x: str = "duration",
    ) -> Any:
        """A/E error heatmap (``kind='value'``) or cell-status
        heatmap (``kind='usage'``), backed by matplotlib.

        Parameters
        ----------
        kind
            ``"value"`` (default; diverging A/E-error heatmap on the
            held-out wedge) or ``"usage"`` (categorical status
            heatmap of training / held-out / regime-excluded /
            future cells, driven by the masking + filter metadata
            inherited from this Backtest's estimator).
        cell_type
            (``kind='value'`` only) ``"cumulative"`` (default; uses
            ``ae_err``) or ``"incremental"`` (uses ``incr_ae_err``).
        label_size
            (``kind='value'`` only) matplotlib font size for the
            per-cell percent labels.
        nrow, ncol
            Facet layout.
        figsize
            Passed to ``plt.subplots``.
        recent, regime
            (``kind='usage'`` only) override values for the filter
            overlays. By default the usage view reads ``recent`` and
            ``regime`` from the estimator that drove the backtest; pass an
            explicit value to override.
        x
            (``kind='value'`` only) horizontal axis: ``"duration"`` (default;
            cohort x duration) or ``"calendar"`` (cohort x calendar
            period -- each cell at its actual calendar date, so the held-out
            diagonal reads as a block of recent calendar columns).

        Returns
        -------
        matplotlib.figure.Figure
        """
        if kind not in ("value", "usage"):
            raise ValueError(
                f"`kind` must be 'value' or 'usage'; got {kind!r}."
            )
        if kind == "value":
            from ._backtest_vis import plot_triangle_backtest
            return plot_triangle_backtest(
                self,
                cell_type=cell_type,
                label_size=label_size,
                nrow=nrow, ncol=ncol, figsize=figsize,
                x=x,
            )
        # kind == "usage": forward to the Triangle-side usage
        # renderer with `holdout=self.holdout` and filter args
        # inherited from `self.estimator` (overridable via kwargs).
        # The Triangle renderer resolves `"auto"` for regime via an
        # inline `detect_regime` call.
        from ._triangle_vis import _plot_triangle_usage
        from .regime import _resolve_regime
        eff_recent = recent if recent is not None else self._infer_recent()
        eff_regime = regime if regime is not None else self._infer_regime()
        # Resolve a callable / "auto" regime spec on the MASKED fold triangle --
        # the same data the fold fit on -- so the overlay shows the cut the fold
        # actually used, never one a full-data detect could derive from held-out
        # cells. An already-eager Regime / date / dict resolves the same either
        # way (its cut is intrinsic, not re-detected).
        eff_regime = _resolve_regime(eff_regime, self._triangle.mask(self.holdout))
        return _plot_triangle_usage(
            self._triangle,
            recent=eff_recent,
            regime=eff_regime,
            holdout=self.holdout,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )

    def _infer_recent(self) -> int | None:
        """Extract `recent` from `self.estimator`, if present."""
        return getattr(self.estimator, "recent", None)

    def _infer_regime(self) -> Any:
        """Extract the regime from `self.estimator`, if any.

        Reads the estimator's ``regime`` (a resolved cohort cut). A legacy
        ``loss_regime`` slot is honoured first if present. The Triangle
        renderer accepts ``"auto"`` directly and runs
        :meth:`Triangle.detect_regime` inline, so a literal ``"auto"`` is
        forwarded as-is.
        """
        est = self.estimator
        if hasattr(est, "loss_regime"):
            return getattr(est, "loss_regime")
        return getattr(est, "regime", None)

    def __repr__(self) -> str:
        n_cells = self._ae_err.height
        est_name = type(self.estimator).__name__
        return f"<_FoldFit: estimator={est_name}, holdout={self.holdout}, n_held_out_cells={n_cells}>"

def _normalize_holdouts_seq(holdouts):
    """Validate, de-duplicate, and sort a sequence of hold-out depths."""
    items = list(holdouts)
    if not items:
        raise ValueError("holdouts must contain at least one hold-out depth")
    clean = set()
    for h in items:
        if isinstance(h, bool) or not isinstance(h, int):
            raise TypeError(
                f"each hold-out depth must be a positive int; got {h!r} "
                f"({type(h).__name__})"
            )
        if h < 1:
            raise ValueError(f"each hold-out depth must be >= 1, got {h}")
        clean.add(h)
    return tuple(sorted(clean))



# ---------------------------------------------------------------------------
# Public API: unified Backtest (single- and multi-origin)
# ---------------------------------------------------------------------------

class Backtest:
    """Calendar-diagonal hold-out backtest of a fit estimator.

    When ``holdouts`` is a single integer (or a 1-tuple), this is a
    single-origin backtest equivalent to the former ``Backtest(holdout=H)``
    interface. When ``holdouts`` has multiple depths, this is a rolling-origin
    backtest that evaluates all depths in one call.

    Parameters
    ----------
    estimator
        A fit estimator (e.g. ``lr.ChainLadder()``, ``lr.PooledLoss()``,
        ``lr.CredibleLoss()``, ``lr.SmoothLoss()``). Must implement
        ``.fit(triangle)``.
    holdouts
        One or more hold-out depths. An ``int`` is normalised to a
        1-tuple. A sequence is de-duplicated, sorted ascending, and
        validated (each >= 1, no booleans). Default ``(6, 12, 18, 24)``.
    target
        Which projection to score: ``"ratio"`` (default), ``"loss"``, or
        ``"premium"``.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> # Single-origin (equivalent to old Backtest(holdout=6)):
    >>> bt = lr.Backtest(lr.CredibleLoss(), holdouts=6).fit(tri)
    >>> bt.col_summary      # single-origin convenience property
    >>> bt.plot()           # A/E heatmap
    >>> # Multi-origin (rolling):
    >>> bt = lr.Backtest(lr.ChainLadder(), holdouts=(6, 12, 18, 24)).fit(tri)
    >>> bt.horizon_summary  # reliability curve
    >>> bt.fits[6].col_summary
    """

    def __init__(
        self,
        estimator,
        holdouts=(6, 12, 18, 24),
        *,
        target="ratio",
    ):
        if not hasattr(estimator, "fit"):
            raise TypeError(
                f"estimator must implement .fit(triangle); got "
                f"{type(estimator).__name__}"
            )
        if target not in _VALID_TARGETS:
            raise ValueError(
                f"target must be one of {_VALID_TARGETS}, got {target!r}"
            )
        # Normalise: int -> 1-tuple, else any sequence of ints (range / generator
        # / tuple / list) -> deduplicate+sort+validate.
        if isinstance(holdouts, bool):
            raise TypeError("holdouts must be an int or a sequence of ints")
        if isinstance(holdouts, int):
            if holdouts < 1:
                raise ValueError(f"holdout must be >= 1, got {holdouts}")
            norm = (holdouts,)
        else:
            try:
                items = tuple(holdouts)
            except TypeError:
                raise TypeError(
                    f"holdouts must be an int or a sequence of ints; got "
                    f"{type(holdouts).__name__}"
                )
            norm = _normalize_holdouts_seq(items)

        # Probe the first depth for early estimator / target / bootstrap checks
        _FoldBacktest(estimator=estimator, holdout=norm[0], target=target)

        self.estimator = estimator
        self.holdouts = norm
        self.target = target

    def fit(self, triangle):
        return BacktestFit._from_triangle(triangle, self)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class BacktestFit:
    """Result of a rolling-origin hold-out backtest.

    Properties
    ----------
    ae_err : DataFrame
        Combined per-cell hold-out comparison across all hold-out depths
        ``[groups?, holdout, horizon, anchor_duration, cohort, duration,
        actual, expected, aeg, ae_err, incr_actual, incr_expected, incr_aeg,
        incr_ae_err]``. ``holdout`` is the depth the cell was scored under;
        ``horizon`` (>= 1) is the number of calendar periods the cell is
        projected past that depth's as-of date; ``anchor_duration``
        (= ``duration - horizon``) is the duration the cohort was observed to
        at that as-of date. A physical ``(cohort, duration)`` cell recurs once
        per hold-out depth that holds it out (scored at a different horizon
        each time) -- so pooling over all rows without grouping by ``holdout``
        double-counts cells; ``holdout`` is carried so a caller can
        de-duplicate. The ``incr_*`` columns are carried through when the refit
        emits an incremental projection (the same condition as
        :class:`_FoldFit`); the internal ``cal_idx`` is
        dropped (``horizon`` supersedes it).
    horizon_summary : DataFrame
        The reliability curve -- error aggregated by ``horizon`` (within
        group): ``[groups?, horizon, n, abs_err_mean, ae_err_mean, ae_err_med,
        ae_err_wt]`` plus the incremental block ``[incr_abs_err_mean,
        incr_ae_err_mean, incr_ae_err_med, incr_ae_err_wt]`` when available.
        ``abs_err_mean = mean(|actual - expected|)`` is the cumulative
        target-unit absolute error; ``ae_err_*`` are the relative ``actual /
        expected - 1`` statistics; ``ae_err_wt = sum(actual - expected) /
        sum(expected)`` is the exposure-weighted pooled A/E - 1 (as in
        :class:`_FoldFit`). The ``incr_*`` companions
        are the per-period counterparts. Because the per-horizon cell
        population drifts toward higher durations as horizon grows (see the
        module docstring), prefer the **incremental** lane to read the curve
        free of the cumulative-magnitude confound; for ``target="ratio"`` the
        relative ``ae_err_*`` / ``incr_ae_err_*`` are more interpretable than
        the unit-bearing ``abs_err_mean`` (a ratio gap, not currency).
    anchor_summary : DataFrame
        Error aggregated by ``anchor_duration`` (within group), same
        statistics as ``horizon_summary``. Shows how projection error depends
        on how much history a cohort had when it was projected: a cohort with a
        single observation (``anchor_duration = 1``) is projected far less
        reliably than a mature one. Complements the horizon curve -- error is
        driven as much by how little history anchored the projection as by how
        far ahead it reaches.
    holdout_summary : DataFrame
        Error aggregated by hold-out depth ``holdout`` (within group), same
        statistics as ``horizon_summary``.
    skipped_holdouts : list[int]
        Hold-out depths that produced no reachable held-out cells (e.g. a
        depth at or beyond the triangle's calendar span) and were dropped.
    fits : dict[int, _FoldFit]
        The inner per-fold ``_FoldFit`` for each depth that produced
        cells, keyed by hold-out depth -- for drill-down / inspection.
    """

    # The incremental companion columns the inner Backtest may carry through.
    _INCR_CELL_COLS = ("incr_actual", "incr_expected", "incr_aeg", "incr_ae_err")

    def __init__(self) -> None:
        raise TypeError(
            "BacktestFit is the result of Backtest(...).fit(triangle), not a direct constructor."
        )

    @classmethod
    def _from_triangle(
        cls, triangle: "Triangle", rbt: "Backtest"
    ) -> "BacktestFit":
        self = cls.__new__(cls)
        # Scoring happens at the estimator's REPORTING grain (triangle.groups -
        # covariates); the per-fold refit keeps the finer triangle. Mirror the
        # _FoldFit collapse so the cross-fold aggregation keys match the folds'
        # reporting-grain A/E frames.
        covs = getattr(rbt.estimator, "covariates", None)
        if covs:
            report_cols = [
                g for g in normalize_groups(triangle._groups)
                if g not in set(covs)
            ]
            report_tri = triangle.collapse(
                collapse_groups(report_cols) if report_cols else None
            )
        else:
            report_tri = triangle
        self._output_type = report_tri._output_type
        self._groups = report_tri._groups
        self.estimator = rbt.estimator
        self.target = rbt.target
        self.holdouts = rbt.holdouts

        group_cols = normalize_groups(report_tri._groups)

        # Per-group max calendar index over the FULL (unmasked) triangle.
        # A depth H masks cells with cal_idx > max_cal - H, so a held-out
        # cell's horizon is `cal_idx - (max_cal - H)`, ranging 1..H. Computing
        # max_cal from the full triangle (not from a fold's surviving cells)
        # keeps the horizon anchored to the true as-of date even when the
        # oldest cohort does not reach the latest diagonal.
        full = _add_cal_idx(report_tri._df, report_tri._groups)
        max_cal_scalar = 0
        max_cal: pl.DataFrame | None = None
        if group_cols:
            max_cal = full.group_by(group_cols).agg(
                pl.col("cal_idx").max().alias("_max_cal")
            )
        else:
            max_cal_scalar = int(full["cal_idx"].max())

        per_holdout: list[pl.DataFrame] = []
        fits: dict[int, Any] = {}
        skipped: list[int] = []

        for h in rbt.holdouts:
            bt_fit = cls._run_holdout(rbt, triangle, h)
            if bt_fit is None:
                skipped.append(h)
                continue

            ae = bt_fit._ae_err  # internal polars frame: keeps cal_idx
            if ae.height == 0:
                skipped.append(h)
                continue

            # horizon = cal_idx - (max_cal - h); 1..h on held cells.
            if group_cols:
                assert max_cal is not None
                ae = ae.join(max_cal, on=group_cols, how="left")
                ae = ae.with_columns(
                    (pl.col("cal_idx") - (pl.col("_max_cal") - h)).alias(
                        "horizon"
                    )
                ).drop("_max_cal")
            else:
                ae = ae.with_columns(
                    (pl.col("cal_idx") - (max_cal_scalar - h)).alias("horizon")
                )

            ae = ae.with_columns(
                pl.lit(h, dtype=pl.Int64).alias("holdout"),
                # how much history the cohort had at this as-of date.
                (pl.col("duration") - pl.col("horizon")).alias(
                    "anchor_duration"
                ),
            )
            # `cal_idx` has done its job (horizon is derived) -- drop it so the
            # public per-cell frame is keyed by the rolling annotations
            # (holdout, horizon, anchor_duration) not the internal index.
            per_holdout.append(ae.drop("cal_idx"))
            fits[h] = bt_fit

        self._skipped = skipped
        self._fits = fits

        # The incremental lane is available only if every surviving fold
        # carried it (a heterogeneous mix would null-fill -- avoid that).
        has_incr = bool(per_holdout) and all(
            all(c in df.columns for c in cls._INCR_CELL_COLS)
            for df in per_holdout
        )

        if per_holdout:
            # vertical concat is intentional: a single estimator refit across
            # folds emits a stable column set, so a column mismatch here is a
            # real bug we want surfaced, not silently null-unioned.
            combined = pl.concat(per_holdout, how="vertical")
            lead = [*group_cols, "holdout", "horizon", "anchor_duration"]
            rest = [c for c in combined.columns if c not in lead]
            combined = combined.select(lead + rest).sort(
                [*group_cols, "holdout", "cohort", "duration"]
            )
        else:
            # No depth produced cells -- emit an empty, well-typed frame so
            # downstream code (and input-mirroring) still works.
            combined = cls._empty_ae_err(group_cols, triangle)

        self._ae_err = combined
        self._has_incr = has_incr

        self._horizon_summary = self._aggregate(
            combined, [*group_cols, "horizon"], has_incr
        )
        self._anchor_summary = self._aggregate(
            combined, [*group_cols, "anchor_duration"], has_incr
        )
        self._holdout_summary = self._aggregate(
            combined, [*group_cols, "holdout"], has_incr
        )

        return self


    # ---- single-origin convenience API ------------------------------------

    @property
    def is_single_origin(self):
        """True when this result comes from a single hold-out depth."""
        return len(self.holdouts) == 1

    @property
    def ae_err(self):
        """Per-cell A/E error.

        For single-origin, returns the fold-level ae_err (which includes
        ``cal_idx`` and ``anchor_value``) -- or the empty rolling frame if
        the single fold was skipped. For multi-origin, returns the rolling
        combined frame (with ``holdout``, ``horizon``, ``anchor_duration``).
        """
        if self.is_single_origin:
            h = self.holdouts[0]
            fold = self._fits.get(h)
            if fold is not None:
                return fold.ae_err
        return mirror_output(self._ae_err, self._output_type)


    def _single_only(self, name):
        """Raise a teaching ValueError for multi-origin access."""
        sorted_hs = sorted(self._fits)
        raise ValueError(
            f"{name!r} is a per-forecast view undefined across multiple "
            f"hold-out depths (it would double-count cells). "
            f"Use .fits[h].{name} where h is one of {sorted_hs}."
        )

    def _get_single_fold(self):
        """Return the single fold's _FoldFit, or None if it was skipped."""
        if not self.is_single_origin:
            return None
        return self._fits.get(self.holdouts[0])

    @property
    def col_summary(self):
        if self.is_single_origin:
            fold = self._get_single_fold()
            if fold is not None:
                return fold.col_summary
            return None
        self._single_only("col_summary")

    @property
    def diag_summary(self):
        if self.is_single_origin:
            fold = self._get_single_fold()
            if fold is not None:
                return fold.diag_summary
            return None
        self._single_only("diag_summary")

    @property
    def fit(self):
        if self.is_single_origin:
            fold = self._get_single_fold()
            if fold is not None:
                return fold.fit
            return None
        self._single_only("fit")

    def _single_fold_or_raise(self, name):
        """The single fold for a plot delegation -- raise if multi-origin
        (teaching error) or if the single hold-out was skipped (no forecast)."""
        if not self.is_single_origin:
            self._single_only(name)               # raises (multi-origin)
        fold = self._fits.get(self.holdouts[0])
        if fold is None:
            raise ValueError(
                f"the single hold-out (depth {self.holdouts[0]}) produced no "
                f"reachable held-out cells, so {name} has nothing to show."
            )
        return fold

    def plot_triangle(self, **kwargs):
        """Delegate to the fold's plot_triangle (single-origin only)."""
        return self._single_fold_or_raise("plot_triangle").plot_triangle(**kwargs)

    def plot(self, kind="auto", **kwargs):
        """A/E error plot (single-origin) or reliability curve (multi-origin).

        Parameters
        ----------
        kind
            ``"auto"`` (default): draws the single-fold A/E plot when
            ``is_single_origin``, otherwise draws the reliability curve.
            ``"reliability"``: always the rolling reliability line plot.
            ``"col"``, ``"diag"``, ``"cell"``, ``"triangle"``: single-origin
            fold views (raise when multi-origin).
        **kwargs
            Forwarded to the underlying plot function.
        """
        FOLD_KINDS = {"triangle", "cell", "col", "diag"}
        if kind == "auto":
            if self.is_single_origin:
                # the single-origin primary deliverable is the A/E heatmap
                return self._single_fold_or_raise("plot").plot_triangle(**kwargs)
            else:
                from ._rolling_backtest_vis import plot_rolling_backtest
                return plot_rolling_backtest(self, **kwargs)
        elif kind == "reliability":
            from ._rolling_backtest_vis import plot_rolling_backtest
            return plot_rolling_backtest(self, **kwargs)
        elif kind in FOLD_KINDS:
            if self.is_single_origin:
                fold = self._single_fold_or_raise("plot")
                if kind == "triangle":
                    return fold.plot_triangle(**kwargs)
                return fold.plot(kind=kind, **kwargs)
            sorted_hs = sorted(self._fits)
            raise ValueError(
                f"kind={kind!r} is a per-fold view undefined across multiple "
                f"hold-out depths. Use .fits[h].plot(kind={kind!r}) "
                f"where h is one of {sorted_hs}."
            )
        else:
            raise ValueError(
                f"kind must be \'auto\', \'reliability\', or one of "
                f"{{\'cell\', \'col\', \'diag\', \'triangle\'}}; got {kind!r}"
            )

    @staticmethod
    def _run_holdout(
        rbt: "Backtest", triangle: "Triangle", holdout: int
    ) -> Any | None:
        """Run one depth's inner Backtest, returning ``None`` if unrunnable.

        A depth that meets or exceeds the calendar span produces a 0-height
        ``ae_err`` rather than raising (the caller treats a 0-height frame as a
        skip), so the common skip path needs no exception handling. We narrowly
        guard only :class:`ValueError` (the package's own "no anchor" /
        degenerate-fold signal) so a genuinely unrunnable fold is skipped
        without masking unrelated failures (an estimator bug, a polars schema
        error, an OOM, a keyboard interrupt) -- those propagate.

        Caveat: a misconfigured-estimator error that itself surfaces as a
        fit-time ``ValueError`` would be absorbed here as a silent skip. The
        ``__init__`` probe ``Backtest`` only *constructs* on ``holdouts[0]`` (it
        does not ``.fit``), so a config error that raises only at fit time is
        not caught up front. In practice an over-deep depth yields a 0-height
        frame rather than raising, so ``ValueError`` is rarely the actual skip
        trigger; the narrow catch is the deliberate trade.
        """
        try:
            return _FoldBacktest(
                estimator=rbt.estimator,
                holdout=holdout,
                target=rbt.target,
            ).fit(triangle)
        except ValueError:
            return None

    # -- aggregation ---------------------------------------------------------

    @classmethod
    def _aggregate(
        cls, ae_err: pl.DataFrame, by_cols: list[str], has_incr: bool
    ) -> pl.DataFrame:
        """Aggregate the combined per-cell frame to a per-key summary.

        Always emits the cumulative block ``(n, abs_err_mean, ae_err_mean,
        ae_err_med, ae_err_wt)``; emits the incremental companion block
        (``incr_abs_err_mean`` / ``incr_ae_err_*``) when ``has_incr``. The
        cumulative ``abs_err_mean = mean(|actual - expected|)`` is target-unit
        absolute error (currency for ``"loss"`` / ``"premium"``, a ratio gap
        for ``"ratio"``); the relative ``ae_err_*`` are dimensionless. Because
        the per-horizon cell population drifts toward higher durations as
        horizon grows, the incremental lane is the confound-free reading of the
        reliability curve.
        """
        agg_exprs = cls._agg_exprs(has_incr)
        if ae_err.height == 0:
            schema = {c: ae_err.schema[c] for c in by_cols}
            schema["n"] = pl.UInt32
            schema.update(
                {name: pl.Float64 for name in cls._summary_stat_names(has_incr)}
            )
            return pl.DataFrame(schema=schema)
        return ae_err.group_by(by_cols).agg(agg_exprs).sort(by_cols)

    @staticmethod
    def _summary_stat_names(has_incr: bool) -> list[str]:
        """Ordered names of the float summary columns (single source of truth
        for both the populated and the empty-frame schema)."""
        names = ["abs_err_mean", "ae_err_mean", "ae_err_med", "ae_err_wt"]
        if has_incr:
            names += [
                "incr_abs_err_mean",
                "incr_ae_err_mean",
                "incr_ae_err_med",
                "incr_ae_err_wt",
            ]
        return names

    @staticmethod
    def _agg_exprs(has_incr: bool) -> list[pl.Expr]:
        """The aggregation expressions matching :meth:`_summary_stat_names`."""
        exprs: list[pl.Expr] = [
            pl.len().alias("n"),
            (pl.col("actual") - pl.col("expected")).abs().mean().alias(
                "abs_err_mean"
            ),
            pl.col("ae_err").mean().alias("ae_err_mean"),
            pl.col("ae_err").median().alias("ae_err_med"),
            (
                pl.when(pl.col("expected").sum() != 0)
                .then(
                    (pl.col("actual") - pl.col("expected")).sum()
                    / pl.col("expected").sum()
                )
                .otherwise(None)
            ).alias("ae_err_wt"),
        ]
        if has_incr:
            exprs += [
                (pl.col("incr_actual") - pl.col("incr_expected"))
                .abs()
                .mean()
                .alias("incr_abs_err_mean"),
                pl.col("incr_ae_err").mean().alias("incr_ae_err_mean"),
                pl.col("incr_ae_err").median().alias("incr_ae_err_med"),
                (
                    pl.when(pl.col("incr_expected").sum() != 0)
                    .then(
                        (pl.col("incr_actual") - pl.col("incr_expected")).sum()
                        / pl.col("incr_expected").sum()
                    )
                    .otherwise(None)
                ).alias("incr_ae_err_wt"),
            ]
        return exprs

    @staticmethod
    def _empty_ae_err(group_cols: list[str], triangle: "Triangle") -> pl.DataFrame:
        """A 0-row combined frame with the expected schema.

        ``cohort`` / ``duration`` dtypes are read from the source triangle
        (cohort may be a ``Date`` underwriting period or an integer
        underwriting year -- never hardcoded), so an all-skipped result labels
        its empty frame consistently with a populated one.
        """
        tri_schema = triangle._df.schema
        cohort_dt = tri_schema.get("cohort", pl.Date)
        duration_dt = tri_schema.get("duration", pl.Int64)
        schema: dict[str, Any] = {c: tri_schema.get(c, pl.Utf8) for c in group_cols}
        schema.update(
            {
                "holdout": pl.Int64,
                "horizon": pl.Int64,
                "anchor_duration": pl.Int64,
                "cohort": cohort_dt,
                "duration": duration_dt,
                "actual": pl.Float64,
                "expected": pl.Float64,
                "aeg": pl.Float64,
                "ae_err": pl.Float64,
            }
        )
        return pl.DataFrame(schema=schema)

    # -- accessors -----------------------------------------------------------

    @property
    def horizon_summary(self):
        return mirror_output(self._horizon_summary, self._output_type)

    @property
    def anchor_summary(self):
        return mirror_output(self._anchor_summary, self._output_type)

    @property
    def holdout_summary(self):
        return mirror_output(self._holdout_summary, self._output_type)

    @property
    def skipped_holdouts(self) -> list[int]:
        return list(self._skipped)

    @property
    def fits(self) -> dict[int, Any]:
        return dict(self._fits)

    # -- evidence readers ----------------------------------------------------

    def _resolve_bias_col(self, tol: float, lane: str) -> str:
        """Validate the shared ``tol`` / ``lane`` arguments of the evidence
        readers and return the pooled signed-bias column to walk."""
        if (
            isinstance(tol, bool)
            or not isinstance(tol, (int, float))
            or not math.isfinite(tol)
            or tol <= 0
        ):
            raise ValueError(
                f"tol must be a positive finite number, got {tol!r}"
            )
        if lane not in ("cumulative", "incremental"):
            raise ValueError(
                f'lane must be "cumulative" or "incremental", got {lane!r}'
            )
        if lane == "incremental" and not self._has_incr:
            raise ValueError(
                'lane="incremental" is unavailable for this fit: not every '
                "surviving hold-out depth carried an incremental projection, "
                "so the summaries have no incr_* lane (see the module "
                "docstring)"
            )
        return "ae_err_wt" if lane == "cumulative" else "incr_ae_err_wt"

    @staticmethod
    def _threshold_walk(
        summ: pl.DataFrame,
        group_cols: list[str],
        axis: str,
        bias: str,
        tol: float,
        mode: str,
        value_col: str,
        max_col: str,
        min_run: int = 1,
    ) -> pl.DataFrame:
        """Per-group tolerance walk over a summary axis.

        ``mode="suffix"`` finds the smallest axis value from which EVERY
        observed entry at or beyond it keeps ``|bias| <= tol`` (null when the
        walk never starts -- including a violation at the very last entry),
        and additionally requires the in-band suffix to hold at least
        ``min_run`` observed entries (null otherwise; ``min_run`` is ignored
        in prefix mode). ``mode="prefix"`` finds the largest axis value
        reached while every entry from the front stays within tolerance (0
        when the first entry already violates). A null bias counts as a
        violation in both modes. The summaries are tiny, so a clear per-group
        Python walk is preferred over a window-expression formulation.
        """
        axis_dt = summ.schema[axis]
        schema: dict[str, Any] = {c: summ.schema[c] for c in group_cols}
        schema[value_col] = axis_dt
        schema[max_col] = axis_dt
        if summ.height == 0:
            return pl.DataFrame(schema=schema)
        parts = (
            summ.partition_by(group_cols, maintain_order=True) if group_cols else [summ]
        )
        rows: list[dict[str, Any]] = []
        for part in parts:
            part = part.sort(axis)
            values = part[axis].to_list()
            within = [
                b is not None and abs(b) <= tol for b in part[bias].to_list()
            ]
            result: Any
            if mode == "suffix":
                result = None
                run = 0
                for v, ok in zip(reversed(values), reversed(within)):
                    if not ok:
                        break
                    result = v
                    run += 1
                if run < min_run:
                    result = None
            else:  # prefix
                result = 0
                for v, ok in zip(values, within):
                    if not ok:
                        break
                    result = v
            row: dict[str, Any] = {c: part[c][0] for c in group_cols}
            row[value_col] = result
            row[max_col] = values[-1]
            rows.append(row)
        out = pl.DataFrame(rows, schema=schema)
        return out.sort(group_cols) if group_cols else out

    def convergence(
        self, tol: float = 0.03, lane: str = "cumulative", min_run: int = 6
    ):
        """Smallest anchor duration from which the pooled bias stays in band.

        Reads the ANCHOR axis of the rolling backtest: how much observed
        history a cohort needs before its out-of-sample projections settle.
        Per group, the ``anchor_summary`` pooled signed bias (``ae_err_wt``
        for ``lane="cumulative"``, ``incr_ae_err_wt`` for
        ``lane="incremental"``) is walked over ``anchor_duration``;
        ``converged_at`` is the smallest observed anchor duration such that
        EVERY observed anchor duration at or beyond it keeps
        ``|bias| <= tol`` (a null bias counts as a violation) AND the
        in-band suffix holds at least ``min_run`` observed anchor durations.
        When no such anchor exists -- including when the largest observed
        anchor still violates -- ``converged_at`` is null. That null is
        honest, not a failure: the bias never settles within the observed
        range, and ``max_anchor`` (the largest observed anchor duration) is
        reported so the null can be judged against how far that range
        actually reaches.

        Why a SIGNED pooled bias rather than absolute error: on a lumpy
        low-frequency book the per-period absolute error never shrinks (it
        is irreducible noise), so an absolute-error criterion would never
        trigger. Systematic over/under-projection, by contrast, averages out
        across the many pooled out-of-sample cells, so the exposure-weighted
        signed bias is the statistic that can actually settle. And why the
        tolerance defaults TIGHT: the cumulative lane's denominator keeps
        growing with duration, so an apparent flattening of the cumulative
        loss ratio is an inertia illusion -- eyeballing that curve is not
        evidence; the out-of-sample bias is. ``lane`` selects which bias to
        walk: the cumulative lane is the smoother primary read, while the
        incremental lane strips the cumulative-magnitude confound (see the
        module docstring's lane discussion).

        Why ``min_run`` guards the walk: the suffix-all test alone fails
        OPEN at the data edge. Two mechanisms make the deepest observed
        anchors unreliable witnesses on the cumulative lane. First, few
        rolling-origin cells reach a deep anchor, so the pooled bias there
        is computed from a handful of cells -- noise, not evidence. Second,
        a cohort observed to a deep anchor carries a large accumulated
        denominator, so projecting a few periods ahead moves its cumulative
        ratio mechanically little: denominator inertia damps the relative
        bias toward zero regardless of model quality (the same inertia that
        makes the cumulative loss-ratio curve LOOK settled). A thin, damped
        tail of a few in-band anchors can therefore fire the unguarded read
        even when every anchor before it violates badly. Raising a
        minimum-cell filter does not fix this: on a book whose bias never
        settles, the spurious point simply chases the truncation edge as
        the filter rises, whereas a genuine convergence point is
        edge-stable under any such filter. ``min_run`` instead demands a
        sustained in-band run, so a thin damped tail cannot fire the call
        on its own. A young triangle whose whole observed anchor range is
        shorter than ``min_run`` honestly reads null -- not enough evidence
        yet -- and ``anchor_summary``'s ``n`` column is the drill-down for
        judging how much pooled evidence each anchor carries.

        This is an anchor-axis question (how much history until the
        projection settles), not a horizon-axis question (how far ahead is
        trustworthy) -- the two must not be conflated; see
        :meth:`reliable_horizon` for the horizon axis.

        Parameters
        ----------
        tol
            Tolerance band on the pooled signed bias, ``|bias| <= tol``.
            Must be a positive finite number. Default ``0.03``.
        lane
            ``"cumulative"`` (default) or ``"incremental"``. The incremental
            lane is available only when every surviving hold-out depth
            carried an incremental projection.
        min_run
            Minimum number of observed anchor durations the in-band suffix
            must hold before ``converged_at`` fires. Must be an int >= 1;
            ``1`` reproduces the unguarded suffix-all read. Default ``6``.

        Returns
        -------
        DataFrame
            Input-mirrored, one row per group, sorted by the group columns:
            ``[groups?, converged_at, max_anchor]``. ``converged_at`` keeps
            the ``anchor_duration`` dtype and is null when the bias never
            settles within the observed range (or settles over fewer than
            ``min_run`` observed anchors); ``max_anchor`` is non-null.
        """
        if (
            isinstance(min_run, bool)
            or not isinstance(min_run, int)
            or min_run < 1
        ):
            raise ValueError(
                f"min_run must be an int >= 1, got {min_run!r}"
            )
        bias = self._resolve_bias_col(tol, lane)
        group_cols = normalize_groups(self._groups)
        out = self._threshold_walk(
            self._anchor_summary,
            group_cols,
            axis="anchor_duration",
            bias=bias,
            tol=tol,
            mode="suffix",
            value_col="converged_at",
            max_col="max_anchor",
            min_run=min_run,
        )
        return mirror_output(out, self._output_type)

    def reliable_horizon(self, tol: float = 0.03, lane: str = "cumulative"):
        """Largest horizon the projections stay in band for, from the front.

        Reads the HORIZON axis of the rolling backtest: how far past the
        as-of date a projection can be pushed before the pooled signed bias
        leaves the tolerance band. Per group, the ``horizon_summary`` bias
        (``ae_err_wt`` for ``lane="cumulative"``, ``incr_ae_err_wt`` for
        ``lane="incremental"``) is walked over ``horizon`` from the smallest
        observed horizon upward; ``reliable_horizon`` is the largest observed
        horizon ``H`` such that EVERY observed horizon up to and including
        ``H`` keeps ``|bias| <= tol`` -- a contiguous-from-the-front
        definition that stops at the first violation (a null bias counts as
        a violation). Reliability must be contiguous: a horizon that drifts
        back into band beyond an out-of-band stretch is not trustworthy,
        since reaching it means trusting the stretch that was not. When the
        very first observed horizon already violates, ``reliable_horizon``
        is ``0`` -- an honest "not reliable even one step ahead". (The ``0``
        here versus :meth:`convergence`'s null is semantic, not cosmetic:
        this method reports the largest contiguous reach, which can
        legitimately be none; ``convergence`` reports the smallest point
        FROM which the bias settles, which may simply not exist within the
        observed range.)

        The signed-pooled-bias rationale of :meth:`convergence` applies
        unchanged -- on a lumpy book the per-period absolute error never
        shrinks, so the bias band, not the absolute error, is what carries
        the evidence. ``lane`` selects which bias to walk: the cumulative
        lane is the smoother primary read, while the incremental lane
        strips the cumulative-magnitude confound (the per-horizon population
        drifts toward higher durations as horizon grows -- see the module
        docstring's lane discussion). Unlike :meth:`convergence` there is no
        ``min_run`` guard here: the prefix walk starts where the pooled data
        is densest (the shallowest horizons) and stops at the first
        violation, so it fails CLOSED at the thin data edge rather than open.

        This is a horizon-axis question (how far ahead is trustworthy), not
        an anchor-axis question (how much history until the projection
        settles) -- the two must not be conflated; see :meth:`convergence`
        for the anchor axis.

        Parameters
        ----------
        tol
            Tolerance band on the pooled signed bias, ``|bias| <= tol``.
            Must be a positive finite number. Default ``0.03``.
        lane
            ``"cumulative"`` (default) or ``"incremental"``. The incremental
            lane is available only when every surviving hold-out depth
            carried an incremental projection.

        Returns
        -------
        DataFrame
            Input-mirrored, one row per group, sorted by the group columns:
            ``[groups?, reliable_horizon, max_horizon]``. ``reliable_horizon``
            keeps the ``horizon`` dtype and is ``0`` (non-null) when the
            first observed horizon already violates; ``max_horizon`` is the
            largest observed horizon.
        """
        bias = self._resolve_bias_col(tol, lane)
        group_cols = normalize_groups(self._groups)
        out = self._threshold_walk(
            self._horizon_summary,
            group_cols,
            axis="horizon",
            bias=bias,
            tol=tol,
            mode="prefix",
            value_col="reliable_horizon",
            max_col="max_horizon",
        )
        return mirror_output(out, self._output_type)



    def _infer_recent(self):
        """Extract `recent` from `self.estimator`, if present."""
        return getattr(self.estimator, "recent", None)

    def _infer_regime(self):
        """Extract the regime from `self.estimator`, if any."""
        est = self.estimator
        if hasattr(est, "loss_regime"):
            return getattr(est, "loss_regime")
        return getattr(est, "regime", None)

    def __repr__(self) -> str:
        est_name = type(self.estimator).__name__
        used = sorted(self._fits)
        n_cells = self._ae_err.height
        skipped = f", skipped={self._skipped}" if self._skipped else ""
        return (
            f"<BacktestFit: estimator={est_name}, "
            f"holdouts={used}, target={self.target!r}, "
            f"n_cells={n_cells}{skipped}>"
        )
