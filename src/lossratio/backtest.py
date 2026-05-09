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


def _add_calendar_idx(tri_df: pl.DataFrame, group_var: str | None) -> pl.DataFrame:
    """Add a 0-based calendar index per cell.

    Calendar index counts the antidiagonal: ``cohort_idx + (dev - 1)``,
    where ``cohort_idx`` is the 0-based position of the cohort within
    its group when sorted by cohort value.
    """
    sort_keys: list[str] = []
    if group_var is not None:
        sort_keys.append(group_var)
    sort_keys.append("cohort")

    over_keys: list[str] | None
    if group_var is not None:
        over_keys = [group_var]
        cohort_idx_expr = (
            pl.col("cohort").rank(method="dense").over(over_keys).cast(pl.Int64) - 1
        )
    else:
        cohort_idx_expr = pl.col("cohort").rank(method="dense").cast(pl.Int64) - 1

    return tri_df.with_columns(
        cohort_idx_expr.alias("__cohort_idx"),
    ).with_columns(
        (pl.col("__cohort_idx") + pl.col("dev") - 1).alias("calendar_idx"),
    )


def _build_masked_df(
    tri_df: pl.DataFrame,
    holdout: int,
    group_var: str | None,
) -> tuple[pl.DataFrame, pl.DataFrame]:
    """Mask the most recent ``holdout`` calendar diagonals.

    Returns ``(masked_df, mask_df)``:

    * ``masked_df`` has ``loss``, ``loss_incr``, ``premium``,
      ``premium_incr``, ``lr``, ``lr_incr`` set to ``None`` for cells
      whose calendar diagonal is among the top ``holdout``.
    * ``mask_df`` has the same shape with the original cell values
      preserved and a ``masked`` boolean column.
    """
    df = _add_calendar_idx(tri_df, group_var)

    # Determine the calendar-diagonal cutoff per group
    if group_var is not None:
        max_per_group = df.group_by(group_var).agg(
            pl.col("calendar_idx").max().alias("__max_cal")
        )
        df = df.join(max_per_group, on=group_var, how="left")
    else:
        max_cal = int(df["calendar_idx"].max())
        df = df.with_columns(pl.lit(max_cal).alias("__max_cal"))

    df = df.with_columns(
        (pl.col("calendar_idx") > pl.col("__max_cal") - holdout).alias("masked")
    )

    masked_df = df.with_columns(
        [
            pl.when(pl.col("masked")).then(None).otherwise(pl.col(c)).alias(c)
            for c in ("loss", "loss_incr", "premium", "premium_incr", "lr", "lr_incr")
            if c in df.columns
        ]
    ).drop("__cohort_idx", "__max_cal", "calendar_idx", "masked")

    annotated_df = df.drop("__cohort_idx", "__max_cal")  # keeps calendar_idx + masked
    return masked_df, annotated_df


def _resolve_predicted_column(fit_df_columns: list[str]) -> str:
    """Return the projected-loss column name for an LRFit / CLFit / EDFit."""
    if "loss_proj" in fit_df_columns:
        return "loss_proj"
    raise ValueError(
        "Refitted estimator output has no 'loss_proj' column. "
        "Backtest currently supports CL / ED / LR."
    )


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class Backtest:
    """Calendar-diagonal hold-out backtest of a fit estimator.

    Holds out the ``holdout`` most recent calendar diagonals from the
    triangle, refits the supplied estimator on the masked data, and
    compares the projection to the original observed values on the
    held-out cells.

    Parameters
    ----------
    estimator
        An ``lr.CL``, ``lr.ED``, or ``lr.LR`` instance (or any
        estimator whose ``fit(triangle)`` returns a result class with
        a ``loss_proj`` column in ``.df``).
    holdout
        Number of most recent calendar diagonals to mask.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Experience(df).triangle(group_var="cv_nm")
    >>> bt = lr.Backtest(estimator=lr.LR(method="sa"), holdout=6).fit(tri)
    >>> bt.aeg
    >>> bt.col_summary
    >>> bt.diag_summary
    """

    def __init__(self, estimator: Any, holdout: int = 6) -> None:
        if holdout < 1:
            raise ValueError(f"holdout must be >= 1, got {holdout}")
        if not hasattr(estimator, "fit"):
            raise TypeError(
                f"estimator must implement .fit(triangle); got {type(estimator).__name__}"
            )
        self.estimator = estimator
        self.holdout = holdout

    def fit(self, triangle: "Triangle") -> "BacktestFit":
        return BacktestFit._from_triangle(triangle, self)


class BacktestFit:
    """Result of a calendar-diagonal hold-out backtest.

    Properties
    ----------
    aeg : DataFrame
        Per-cell hold-out comparison
        ``[group_var?, cohort, dev, calendar_idx, actual, predicted, aeg]``.
    col_summary : DataFrame
        Aggregated by dev: total actual, total predicted, total aeg, n.
    diag_summary : DataFrame
        Aggregated by calendar_idx: total actual, total predicted,
        total aeg, n.
    fit :
        The refitted estimator's result (e.g. CLFit / EDFit / LRFit).
    """

    def __init__(self) -> None:
        self._aeg: pl.DataFrame
        self._col_summary: pl.DataFrame
        self._diag_summary: pl.DataFrame
        self._refit: Any
        self._output_type: str
        self._group_var: str | None
        self.holdout: int
        self.estimator: Any

    @classmethod
    def _from_triangle(cls, triangle: "Triangle", bt: "Backtest") -> "BacktestFit":
        from .triangle import Triangle

        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._group_var = triangle._group_var
        self.holdout = bt.holdout
        self.estimator = bt.estimator

        # 1. Mask the most recent calendar diagonals
        masked_df, annotated_df = _build_masked_df(
            triangle._df, bt.holdout, triangle._group_var
        )

        # 2. Build a masked Triangle (same metadata, masked cells)
        masked_tri = Triangle._from_masked(triangle, masked_df)

        # 3. Refit estimator on masked Triangle
        refit = bt.estimator.fit(masked_tri)
        self._refit = refit

        refit_df = refit.to_polars()
        pred_col = _resolve_predicted_column(refit_df.columns)

        # 4. Build per-cell AEG by joining masked cells with refit
        keys: list[str] = []
        if triangle._group_var is not None:
            keys.append(triangle._group_var)
        keys.extend(["cohort", "dev"])

        held_out = annotated_df.filter(pl.col("masked")).select(
            keys + ["calendar_idx", "loss"]
        ).rename({"loss": "actual"})

        refit_pred = refit_df.select(keys + [pred_col]).rename({pred_col: "predicted"})

        # Drop unreachable cells: cohorts whose observations are wholly
        # within the held-out diagonals have no anchor for projection,
        # so the refit returns NaN at those cells.
        aeg = (
            held_out.join(refit_pred, on=keys, how="inner")
            .filter(pl.col("predicted").is_not_null())
            .with_columns((pl.col("actual") - pl.col("predicted")).alias("aeg"))
        )
        self._aeg = aeg.sort(keys + ["calendar_idx"])

        # 5. Summaries
        col_keys: list[str] = []
        if triangle._group_var is not None:
            col_keys.append(triangle._group_var)
        col_keys.append("dev")

        self._col_summary = (
            aeg.group_by(col_keys)
            .agg(
                pl.col("actual").sum().alias("sum_actual"),
                pl.col("predicted").sum().alias("sum_predicted"),
                pl.col("aeg").sum().alias("sum_aeg"),
                pl.len().alias("n"),
            )
            .sort(col_keys)
        )

        diag_keys: list[str] = []
        if triangle._group_var is not None:
            diag_keys.append(triangle._group_var)
        diag_keys.append("calendar_idx")

        self._diag_summary = (
            aeg.group_by(diag_keys)
            .agg(
                pl.col("actual").sum().alias("sum_actual"),
                pl.col("predicted").sum().alias("sum_predicted"),
                pl.col("aeg").sum().alias("sum_aeg"),
                pl.len().alias("n"),
            )
            .sort(diag_keys)
        )

        return self

    @property
    def aeg(self):
        return mirror_output(self._aeg, self._output_type)

    @property
    def col_summary(self):
        return mirror_output(self._col_summary, self._output_type)

    @property
    def diag_summary(self):
        return mirror_output(self._diag_summary, self._output_type)

    @property
    def fit(self):
        return self._refit

    def __repr__(self) -> str:
        n_cells = self._aeg.height
        est_name = type(self.estimator).__name__
        return f"<BacktestFit: estimator={est_name}, holdout={self.holdout}, n_held_out_cells={n_cells}>"
