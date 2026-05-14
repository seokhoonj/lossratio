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


def _add_calendar_idx(tri_df: pl.DataFrame, groups: str | None) -> pl.DataFrame:
    """Add a 1-based calendar index per cell (R parity).

    Calendar index counts the antidiagonal:
    ``cohort_idx + dev - 1`` with ``cohort_idx`` the 1-based dense
    rank of cohort within its group (R's ``frank(..., ties.method =
    "dense")`` convention). So the oldest cohort at dev = 1 lands on
    calendar_idx = 1, and the maximum calendar_idx equals the number
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
        (pl.col("__cohort_idx") + pl.col("dev") - 1).alias("calendar_idx"),
    )


def _build_masked_df(
    tri_df: pl.DataFrame,
    holdout: int,
    groups: str | None,
) -> tuple[pl.DataFrame, pl.DataFrame]:
    """Mask the most recent ``holdout`` calendar diagonals.

    Returns ``(masked_df, mask_df)``:

    * ``masked_df`` has ``loss``, ``loss_incr``, ``premium``,
      ``premium_incr``, ``lr``, ``lr_incr`` set to ``None`` for cells
      whose calendar diagonal is among the top ``holdout``.
    * ``mask_df`` has the same shape with the original cell values
      preserved and a ``masked`` boolean column.
    """
    df = _add_calendar_idx(tri_df, groups)

    # Determine the calendar-diagonal cutoff per group
    if groups is not None:
        max_per_group = df.group_by(groups).agg(
            pl.col("calendar_idx").max().alias("__max_cal")
        )
        df = df.join(max_per_group, on=groups, how="left")
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


_VALID_METRICS = ("lr", "loss", "premium")


def _is_ratio_fit_estimator(estimator: Any) -> bool:
    """LR / ED jointly project loss / premium / lr; CL projects a single
    column. Distinguished by whether the estimator class is a ratio-fit.
    """
    # Late import to avoid circular dependency at module load time.
    from .ed import ED
    from .lr import LR

    return isinstance(estimator, (LR, ED))


def _resolve_expected_column(
    metric: str, fit_df_columns: list[str], refit: Any
) -> str:
    """Map ``metric`` to the projection column on the refit output frame.

    Post-Phase-4b workers emit generic ``target_proj`` columns (CL: loss
    side; ED: target = loss, plus ``lr_proj`` as a downstream quantity).
    LR keeps legacy ``loss_proj`` / ``premium_proj`` / ``lr_proj``.
    """
    if metric not in _VALID_METRICS:
        raise ValueError(
            f"metric must be one of {_VALID_METRICS}, got {metric!r}"
        )

    # LR estimator: legacy column names still emitted.
    legacy = {"lr": "lr_proj", "loss": "loss_proj", "premium": "premium_proj"}
    if legacy[metric] in fit_df_columns:
        return legacy[metric]

    # New worker schema (CL / ED): target_proj corresponds to the
    # estimator's `target` role. Use lr_proj for ratio metric on ED.
    if metric == "lr" and "lr_proj" in fit_df_columns:
        return "lr_proj"
    target = getattr(refit, "target", None)
    if metric == target and "target_proj" in fit_df_columns:
        return "target_proj"
    exposure = getattr(refit, "exposure", None)
    if metric == exposure and "exposure_proj" in fit_df_columns:
        return "exposure_proj"

    raise ValueError(
        f"Refitted estimator output has no column for metric={metric!r}. "
        f"Available: {fit_df_columns}"
    )


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
        An ``lr.CL``, ``lr.ED``, or ``lr.LR`` instance (or any
        estimator whose ``fit(triangle)`` returns a result class with
        a ``loss_proj`` column in ``.df``).
    holdout
        Number of most recent calendar diagonals to mask.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> bt = lr.Backtest(estimator=lr.LR(method="sa"), holdout=6).fit(tri)
    >>> bt.ae_err
    >>> bt.col_summary
    >>> bt.diag_summary
    """

    def __init__(
        self,
        estimator: Any,
        holdout: int = 6,
        metric: str = "lr",
    ) -> None:
        if holdout < 1:
            raise ValueError(f"holdout must be >= 1, got {holdout}")
        if not hasattr(estimator, "fit"):
            raise TypeError(
                f"estimator must implement .fit(triangle); got {type(estimator).__name__}"
            )
        if metric not in _VALID_METRICS:
            raise ValueError(
                f"metric must be one of {_VALID_METRICS}, got {metric!r}"
            )
        # LR / ED are ratio-fits — only `lr` is a meaningful scoring lane;
        # use CL directly to backtest the loss or premium projection.
        if _is_ratio_fit_estimator(estimator) and metric != "lr":
            raise ValueError(
                f"estimator is a ratio-fit ({type(estimator).__name__}); "
                f"only metric='lr' is supported. Use lr.CL() instead to "
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
        ``[groups?, cohort, dev, calendar_idx, actual, expected, ae_err]``.
        ``ae_err = actual / expected - 1`` (signed relative error;
        positive = under-projection, negative = over-projection).
    col_summary : DataFrame
        Aggregated by dev:
        ``[groups?, dev, n, ae_err_mean, ae_err_med, ae_err_wt]``.
        ``ae_err_wt = sum(actual - expected) / sum(expected)`` is the
        exposure-weighted pooled A/E - 1.
    diag_summary : DataFrame
        Aggregated by calendar_idx with the same statistics as
        ``col_summary``.
    fit :
        The refitted estimator's result (e.g. CLFit / EDFit / LRFit).
    """

    def __init__(self) -> None:
        self._ae_err: pl.DataFrame
        self._col_summary: pl.DataFrame
        self._diag_summary: pl.DataFrame
        self._refit: Any
        self._output_type: str
        self._groups: str | None
        self.holdout: int
        self.estimator: Any

    @classmethod
    def _from_triangle(cls, triangle: "Triangle", bt: "Backtest") -> "BacktestFit":
        from .triangle import Triangle

        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
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
        self.metric = bt.metric

        # 4. Build per-cell A/E Error by joining masked cells with refit
        keys: list[str] = []
        if triangle._groups is not None:
            keys.append(triangle._groups)
        keys.extend(["cohort", "dev"])

        # `actual` is the cumulative column on the original Triangle that
        # corresponds to the chosen scoring lane.
        actual_col = bt.metric  # "lr" / "loss" / "premium"
        held_out = annotated_df.filter(pl.col("masked")).select(
            keys + ["calendar_idx", actual_col]
        ).rename({actual_col: "actual"})

        refit_exp = refit_df.select(keys + [exp_col]).rename({exp_col: "expected"})

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
        self._ae_err = ae_err.sort(keys + ["calendar_idx"])

        # 5. Summaries — mean / median / weighted A/E - 1 per dev or
        #    per calendar diagonal.
        col_keys: list[str] = []
        if triangle._groups is not None:
            col_keys.append(triangle._groups)
        col_keys.append("dev")

        self._col_summary = (
            ae_err.group_by(col_keys)
            .agg(
                pl.len().alias("n"),
                pl.col("aeg").mean().alias("aeg_mean"),
                pl.col("aeg").median().alias("aeg_med"),
                pl.col("ae_err").mean().alias("ae_err_mean"),
                pl.col("ae_err").median().alias("ae_err_med"),
                (
                    (pl.col("actual") - pl.col("expected")).sum()
                    / pl.col("expected").sum()
                ).alias("ae_err_wt"),
            )
            .sort(col_keys)
        )

        diag_keys: list[str] = []
        if triangle._groups is not None:
            diag_keys.append(triangle._groups)
        diag_keys.append("calendar_idx")

        self._diag_summary = (
            ae_err.group_by(diag_keys)
            .agg(
                pl.len().alias("n"),
                pl.col("aeg").mean().alias("aeg_mean"),
                pl.col("aeg").median().alias("aeg_med"),
                pl.col("ae_err").mean().alias("ae_err_mean"),
                pl.col("ae_err").median().alias("ae_err_med"),
                (
                    (pl.col("actual") - pl.col("expected")).sum()
                    / pl.col("expected").sum()
                ).alias("ae_err_wt"),
            )
            .sort(diag_keys)
        )

        return self

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

    def __repr__(self) -> str:
        n_cells = self._ae_err.height
        est_name = type(self.estimator).__name__
        return f"<BacktestFit: estimator={est_name}, holdout={self.holdout}, n_held_out_cells={n_cells}>"
