"""Triangle: cohort x duration aggregated experience data."""

from __future__ import annotations

from collections.abc import Sequence
from typing import TYPE_CHECKING, Any, Literal

import polars as pl

from .._kernels.io import (
    collapse_groups,
    detect_input_type,
    mirror_output,
    normalize_groups,
    to_polars,
)
from .._kernels.period import (
    Grain,
    coerce_cols_to_date,
    count_periods,
    floor_cols_to_period,
    infer_grain,
    resolve_grain,
)

if TYPE_CHECKING:
    import pandas as pd

    from .._kernels.io import FrameLike
    from .._types import LabelStyle, RegimeArg, XAxis
    from .calendar import Calendar
    from .link import Link
    from .total import Total


# Triangle-specific: standard duration column name per grain code.
_GRAIN_TO_DURATION_COL = {
    "M": "duration_m",
    "Q": "duration_q",
    "H": "duration_h",
    "Y": "duration_y",
}


class Triangle:
    """Cohort x duration aggregated experience data.

    A Triangle is built by aggregating a raw experience DataFrame
    (polars or pandas) over ``groups`` (optional), cohort, and
    duration (the elapsed-period axis since cohort inception).

    Input flexibility:

    - ``cohort`` / ``calendar`` columns can be ``Date``,
      ``Datetime``, integer (yyyy / yyyymm / yyyymmdd auto-detected),
      or ISO string ("YYYY-MM-DD", "YYYY/MM/DD", "YYYYMMDD",
      with optional " HH:MM:SS" time suffix).
    - Granularity ("month" / "quarter" / "half" / "year") is
      auto-detected from cohort date spacing. Override via ``grain``
      to view at a coarser granularity (e.g., monthly data viewed
      quarterly).
    - Requested grain must be at least as coarse as input (no
      decomposition: yearly input cannot be viewed monthly).

    Three-mode dispatch on ``calendar`` / ``duration``:

    - mode 1 (cohort + calendar): ``duration`` is derived from the two.
    - mode 2 (cohort + duration, no calendar): the supplied ``duration`` column
      is taken directly; the ``calendar`` attribute is ``None``.
    - mode 3 (cohort + calendar + duration): the supplied ``duration`` is
      cross-checked against the derived value, then used as given.

    The resulting frame has columns:

    * ``groups`` -- present only if supplied
    * ``n_cohorts`` -- distinct cohorts observed per (group, duration)
    * ``cohort`` -- the underwriting period
    * ``duration`` -- the elapsed-period index (1, 2, ...) within each cohort
    * ``loss``, ``premium`` -- cumulative sums within each (group, cohort)
    * ``incr_loss``, ``incr_premium`` -- per-period sums per cell
    * ``ratio``, ``incr_ratio`` -- cumulative / per-period loss ratio
    * ``margin``, ``incr_margin`` -- ``premium - loss``
    * ``profit``, ``incr_profit`` -- ``"pos"`` / ``"neg"`` indicator
    * ``loss_share``, ``incr_loss_share`` -- within-(cohort, duration) shares
    * ``premium_share``, ``incr_premium_share`` -- ditto for premium

    Cumulative is the unmarked default; per-period values carry an
    ``incr_`` (incremental) prefix.

    Original column names (e.g. ``"uy_m"`` for cohort) are kept as
    instance attributes for downstream plotting and Calendar.
    """

    def __init__(
        self,
        df: FrameLike,
        groups: str | Sequence[str] | None = None,
        cohort: str = "uy_m",
        calendar: str | None = "cy_m",
        duration: str | None = None,
        loss: str = "incr_loss",
        premium: str = "incr_premium",
        grain: Grain | Literal["auto"] = "auto",
        basis: Literal["incremental", "cumulative"] = "incremental",
        fill_gaps: bool = False,
    ) -> None:
        self._output_type = detect_input_type(df)
        df_pl = to_polars(df)

        if basis not in ("incremental", "cumulative"):
            raise ValueError(
                f"basis must be 'incremental' or 'cumulative', "
                f"got {basis!r}"
            )
        if calendar is None and duration is None:
            raise ValueError(
                "Must supply at least one of `calendar` or `duration`."
            )

        # Required columns: cohort, loss, premium, groups (if set), and
        # whichever of calendar / duration were supplied.
        required = {cohort, loss, premium}
        required.update(normalize_groups(groups))
        if calendar is not None:
            required.add(calendar)
        if duration is not None:
            required.add(duration)
        missing = required - set(df_pl.columns)
        if missing:
            raise ValueError(
                f"Missing required columns: {sorted(missing)}. "
                f"Required: {sorted(required)}"
            )

        # A null level in a group column cannot be aggregated into a segment:
        # it survives grouping but then fails to match on the segment join,
        # crashing the per-segment fit downstream. Reject it up front with a
        # clear message rather than an opaque error at fit time.
        group_cols = normalize_groups(groups)
        if group_cols:
            null_counts = df_pl.select(group_cols).null_count().row(0)
            offenders = {
                c: n for c, n in zip(group_cols, null_counts, strict=True) if n
            }
            if offenders:
                raise ValueError(
                    f"Group column(s) contain null values: {offenders}. "
                    f"A null group level cannot form a segment; drop those rows "
                    f"or relabel the nulls to an explicit level (e.g. 'unknown') "
                    f"before building the Triangle."
                )

        # Coerce cohort (and calendar if present) to Date.
        date_cols = [cohort] + ([calendar] if calendar is not None else [])
        df_pl = coerce_cols_to_date(df_pl, date_cols)
        df_pl = df_pl.with_columns(
            pl.col(loss).cast(pl.Float64),
            pl.col(premium).cast(pl.Float64),
        )

        # Cumulative input: difference per-cohort to recover increments
        # at INPUT grain (before binning). Sort axis prefers calendar
        # when present, else duration -- both monotone within a cohort.
        if basis == "cumulative":
            sort_axis = calendar if calendar is not None else duration
            cell_keys: list[str] = normalize_groups(groups)
            cell_keys.extend([cohort, sort_axis])  # type: ignore[list-item]
            # Collapse any sub-tracks sharing a (group, cohort, axis) cell into one
            # cumulative value BEFORE differencing. Otherwise the per-cohort shift
            # diffs one cumulative track against an unrelated one and the later
            # per-cell sum silently drops a track's increments. Sum-of-one is a
            # no-op for the usual one-row-per-cell input.
            df_pl = df_pl.group_by(cell_keys).agg(
                pl.col(loss).sum(),
                pl.col(premium).sum(),
                pl.exclude([*cell_keys, loss, premium]).first(),
            )
            diff_over: list[str] = normalize_groups(groups)
            diff_over.append(cohort)
            df_pl = df_pl.sort(cell_keys).with_columns(
                (pl.col(loss) - pl.col(loss).shift(1, fill_value=0.0)
                 .over(diff_over)).alias(loss),
                (pl.col(premium) - pl.col(premium).shift(1, fill_value=0.0)
                 .over(diff_over)).alias(premium),
            )

        # Auto-detect input grain; resolve "auto" or validate explicit value.
        input_grain = infer_grain(df_pl[cohort])
        grain = resolve_grain(input_grain, grain)

        # Bin cohort (and calendar if present) to requested grain.
        # Always floor: even when grain == input_grain, the input dates may
        # not sit on period starts (e.g. two January dates 2024-01-15 and
        # 2024-01-25), and flooring collapses them into one cohort. Skipping
        # this would silently split a single period into multiple cohorts.
        # floor_to_period is idempotent on already-aligned dates.
        df_pl = floor_cols_to_period(df_pl, date_cols, grain)

        # Three-mode dispatch: derive / validate the integer duration axis.
        if calendar is not None and duration is not None:
            # mode 3: cross-check supplied duration against derived.
            df_pl = df_pl.with_columns(
                count_periods(pl.col(cohort), pl.col(calendar), grain)
                .alias("_duration_derived"),
                pl.col(duration).cast(pl.Int64).alias("_duration_temp"),
            )
            mismatch = df_pl.filter(
                pl.col("_duration_derived").is_not_null()
                & pl.col("_duration_temp").is_not_null()
                & (pl.col("_duration_derived") != pl.col("_duration_temp"))
            )
            if mismatch.height:
                raise ValueError(
                    f"`duration` is inconsistent with `cohort` + `calendar` "
                    f"(grain {grain!r}) in {mismatch.height} row(s)."
                )
            df_pl = df_pl.drop("_duration_derived")
        elif calendar is not None:
            # mode 1: derive duration from cohort + calendar.
            df_pl = df_pl.with_columns(
                count_periods(pl.col(cohort), pl.col(calendar), grain)
                .alias("_duration_temp")
            )
        else:
            # mode 2: take supplied duration directly (calendar is None here,
            # so duration is guaranteed present by the earlier validation).
            assert duration is not None
            df_pl = df_pl.with_columns(
                pl.col(duration).cast(pl.Int64).alias("_duration_temp")
            )

        # Duration is 1-indexed: a cell at duration < 1 means the calendar period
        # precedes the cohort's inception (a dirty export) or a non-positive
        # duration was supplied. Reject it at the boundary rather than folding a
        # pre-inception amount into the duration-1 cumulative.
        n_pre_inception = df_pl.filter(pl.col("_duration_temp") < 1).height
        if n_pre_inception:
            raise ValueError(
                f"{n_pre_inception} row(s) have duration < 1 (calendar precedes "
                f"cohort, or a non-positive duration was supplied); every cell "
                f"must be at duration >= 1."
            )

        # Aggregate per-period values by (groups, cohort, duration).
        agg_keys: list[str] = normalize_groups(groups)
        agg_keys.extend([cohort, "_duration_temp"])

        agg = (
            df_pl.group_by(agg_keys)
            .agg(
                pl.col(loss).sum().alias("incr_loss"),
                pl.col(premium).sum().alias("incr_premium"),
            )
            .sort(agg_keys)
        )

        # n_cohorts: distinct cohorts observed per (group, duration). Computed
        # on the pre-fill aggregate so zero-filled gap cells do not
        # inflate the count (the count is built before gap-filling).
        ncoh_keys: list[str] = normalize_groups(groups)
        ncoh_keys.append("_duration_temp")
        ncoh = (
            agg.group_by(ncoh_keys)
            .agg(pl.col(cohort).n_unique().alias("n_cohorts"))
        )

        # Check for non-consecutive duration sequences within each cohort.
        # fill_gaps=False raises on a gap; fill_gaps=True zero-fills.
        gap_keys = agg_keys[:-1]  # groups + cohort (or cohort alone)
        gap_summary = (
            agg.group_by(gap_keys)
            .agg(
                pl.col("_duration_temp").min().alias("duration_min"),
                pl.col("_duration_temp").max().alias("duration_max"),
                pl.col("_duration_temp").n_unique().alias("n_duration"),
            )
            .with_columns(
                (pl.col("duration_max") - pl.col("duration_min") + 1).alias("n_expected"),
            )
        )
        gaps = gap_summary.filter(
            pl.col("n_duration") != pl.col("n_expected")
        )
        if gaps.height:
            if fill_gaps:
                # Build full (group, cohort, duration) grid spanning each cohort's
                # observed duration range, left-join the aggregate, zero-fill.
                full_grid = (
                    gap_summary
                    .with_columns(
                        pl.int_ranges(
                            pl.col("duration_min"), pl.col("duration_max") + 1
                        ).alias("_duration_temp")
                    )
                    .explode("_duration_temp")
                    .select([*gap_keys, "_duration_temp"])
                )
                agg = (
                    full_grid.join(agg, on=agg_keys, how="left")
                    .with_columns(
                        pl.col("incr_loss").fill_null(0.0),
                        pl.col("incr_premium").fill_null(0.0),
                    )
                    .sort(agg_keys)
                )
            else:
                raise ValueError(
                    f"Found {gaps.height} cohort(s) with non-consecutive "
                    f"duration sequences. Pass fill_gaps=True to zero-fill, or "
                    f"inspect with TriangleValidation(...) (same kwargs)."
                )

        # Join n_cohorts, then rename cohort / duration to standard names so
        # subsequent `over` / `sum().over` expressions read the canonical
        # columns.
        agg = agg.join(ncoh, on=ncoh_keys, how="left")
        agg = agg.rename({cohort: "cohort", "_duration_temp": "duration"})

        cum_keys: list[str] = normalize_groups(groups)
        cum_keys.append("cohort")

        coh_duration = ["cohort", "duration"]

        # Cumulative sums within (group, cohort) -- cumulative is the
        # unmarked default name; per-period carry the incr_ prefix.
        agg = agg.with_columns(
            pl.col("incr_loss").cum_sum().over(cum_keys).alias("loss"),
            pl.col("incr_premium").cum_sum().over(cum_keys).alias("premium"),
        ).with_columns(
            # ratio (cumulative + per-period)
            (pl.col("loss") / pl.col("premium")).alias("ratio"),
            (pl.col("incr_loss") / pl.col("incr_premium")).alias("incr_ratio"),
            # margin = premium - loss (cumulative + per-period)
            (pl.col("premium") - pl.col("loss")).alias("margin"),
            (pl.col("incr_premium") - pl.col("incr_loss")).alias("incr_margin"),
        ).with_columns(
            # profit indicator: "pos" when margin >= 0 else "neg"
            pl.when(pl.col("margin") >= 0).then(pl.lit("pos"))
              .otherwise(pl.lit("neg")).alias("profit"),
            pl.when(pl.col("incr_margin") >= 0).then(pl.lit("pos"))
              .otherwise(pl.lit("neg")).alias("incr_profit"),
            # within-(cohort, duration) shares -- proportions sum to 1 across groups
            (pl.col("loss") / pl.col("loss").sum().over(coh_duration))
                .alias("loss_share"),
            (pl.col("incr_loss") / pl.col("incr_loss").sum().over(coh_duration))
                .alias("incr_loss_share"),
            (pl.col("premium") / pl.col("premium").sum().over(coh_duration))
                .alias("premium_share"),
            (pl.col("incr_premium") / pl.col("incr_premium").sum().over(coh_duration))
                .alias("incr_premium_share"),
        )

        # Reorder columns: cum-first paired.
        ordered = normalize_groups(groups)
        ordered.extend([
            "n_cohorts", "cohort", "duration",
            "loss", "incr_loss",
            "premium", "incr_premium",
            "ratio", "incr_ratio",
            "margin", "incr_margin",
            "profit", "incr_profit",
            "loss_share", "incr_loss_share",
            "premium_share", "incr_premium_share",
        ])
        agg = agg.select(ordered)

        self._df = agg
        self._groups = collapse_groups(groups)
        self._cohort = cohort
        self._calendar = calendar
        self._loss = loss
        self._premium = premium
        self._grain = grain
        self._duration = _GRAIN_TO_DURATION_COL[grain]

    @property
    def df(self) -> FrameLike:
        """Return the triangle data in the original input format."""
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        """Return the triangle data as a polars DataFrame."""
        return self._df

    def to_pandas(self) -> pd.DataFrame:
        """Return the triangle data as a pandas DataFrame."""
        return self._df.to_pandas()

    @property
    def n_rows(self) -> int:
        return self._df.height

    @property
    def columns(self) -> list[str]:
        return self._df.columns

    @property
    def groups(self) -> str | list[str] | None:
        """Group column name(s): a ``str`` for a single group column, a
        ``list[str]`` for multiple, or ``None`` if ungrouped.

        Set via the ``groups`` constructor argument, which accepts either a
        single name or a sequence::

            lr.Triangle(df, groups="coverage")               # single
            lr.Triangle(df, groups=["coverage", "channel"])  # per combination

        With multiple columns every grouped surface (fits, diagnostics,
        backtest, plots) keys per group COMBINATION, and per-group values are
        TUPLES (e.g. ``("CI", "TM")``) rather than scalars."""
        return self._groups

    @property
    def cohort(self) -> str:
        """Original cohort variable name (e.g. 'uy_m')."""
        return self._cohort

    @property
    def calendar(self) -> str | None:
        """Original calendar column name (e.g. 'cy_m').

        ``None`` for a mode-2 (duration-only) triangle: no calendar column
        was supplied, so there is no raw name to retain. (The calendar
        aggregation view is :meth:`calendar_agg`, so this bare name
        stays the raw-column accessor.)
        """
        return self._calendar

    @property
    def loss(self) -> str:
        """Original per-period loss column name (e.g. 'incr_loss')."""
        return self._loss

    @property
    def premium(self) -> str:
        """Original per-period premium column name (e.g. 'incr_premium')."""
        return self._premium

    @property
    def duration(self) -> str:
        """Standard duration column name for the grain (e.g. 'duration_m')."""
        return self._duration

    @property
    def grain(self) -> Grain:
        """Triangle grain code: ``"M"`` / ``"Q"`` / ``"H"`` / ``"Y"``.

        Matches the standard column suffix (``"M"`` <-> ``duration_m``, etc.).
        """
        return self._grain

    @classmethod
    def _from_masked(cls, original: Triangle, masked_df: pl.DataFrame) -> Triangle:
        """Build a Triangle from a pre-built (masked) DataFrame.

        Used internally by Backtest. ``masked_df`` must already have
        all the standard Triangle columns; this constructor just wraps
        it and copies metadata from ``original``.
        """
        tri = cls.__new__(cls)
        tri._df = masked_df
        tri._output_type = original._output_type
        tri._groups = original._groups
        tri._cohort = original._cohort
        tri._calendar = original._calendar
        tri._loss = original._loss
        tri._premium = original._premium
        tri._grain = original._grain
        tri._duration = original._duration
        return tri

    def collapse(self, groups: str | Sequence[str] | None) -> Triangle:
        """Re-aggregate to a SUBSET of the current group columns.

        Sums the incremental loss / premium over the dropped group columns
        and re-derives every cumulative / ratio / share column, returning a
        new Triangle keyed by ``groups`` (which must be a subset of
        :attr:`groups`; ``None`` / ``[]`` collapses to a single ungrouped
        triangle). Because summation is associative, the result is identical
        cell-for-cell to building the Triangle from the raw frame at the
        coarser grouping -- the standardized ``cohort`` (Date) and
        ``duration`` (Int) cells already carry the binning, so this re-build
        re-floors nothing.

        Used by the covariate fit to project at the reporting grain
        (``groups - covariates``) while keeping the finer cells for the
        covariate level effects, and available as the general "operate at a
        coarser grain" lever (e.g. ``tri.collapse('coverage')``).

        Null preservation: a backtest masks held-out diagonals to ``null``. A
        ``(group, cohort, duration)`` cell with ANY null finer sub-cell stays
        null in the result (a sum with a missing addend is missing, not a
        partial ``0`` -- which would silently under-count the cell and corrupt
        the cumulative chain of the observed cells after it), so the report fit
        still PROJECTS it rather than reading a spurious value. (``absent``
        sub-cells -- a ragged grid -- are summed normally; only explicit nulls
        propagate.)
        """
        cur = normalize_groups(self._groups)
        sub = normalize_groups(groups)
        extra = [g for g in sub if g not in cur]
        if extra:
            raise ValueError(
                f"collapse groups {extra} are not a subset of the triangle's "
                f"groups {cur}."
            )
        keys = [*sub, "cohort", "duration"]
        cells = self._df.select([*keys, "incr_loss", "incr_premium"])
        # cells with no observed sub-cell (all-null increments) -- the held-out
        # diagonals a backtest masks. polars sums these to 0; track them so they
        # can be restored to null after the (summing) re-build.
        held = None
        if cells["incr_premium"].null_count():
            held = (
                cells.group_by(keys)
                .agg((pl.col("incr_premium").null_count() > 0).alias("_hold"))
                .filter(pl.col("_hold"))
                .select(keys)
            )
        out = Triangle(
            cells,
            groups=(collapse_groups(sub) if sub else None),
            cohort="cohort",
            calendar=None,
            duration="duration",
            loss="incr_loss",
            premium="incr_premium",
            grain=self._grain,
        )
        if held is not None and held.height:
            # mirror the backtest mask: null the cumulative + incremental value
            # lanes on the fully-held-out cells (the share / margin lanes do not
            # feed the fit). The held cells are trailing diagonals, so the 0-fill
            # never corrupts an observed cell's cumulative upstream of them.
            null_cols = [
                c for c in ("loss", "incr_loss", "premium", "incr_premium",
                            "ratio", "incr_ratio")
                if c in out._df.columns
            ]
            odf = out._df.join(
                held.with_columns(pl.lit(True).alias("_hold")), on=keys, how="left"
            )
            out._df = odf.with_columns(
                [pl.when(pl.col("_hold")).then(None).otherwise(pl.col(c)).alias(c)
                 for c in null_cols]
            ).drop("_hold")
        # preserve the raw-name metadata for labels / formatting (the rebuild
        # only knows the standardized "cohort" / "incr_loss" names).
        out._output_type = self._output_type
        out._cohort = self._cohort
        out._calendar = self._calendar
        out._loss = self._loss
        out._premium = self._premium
        return out

    def calendar_agg(self) -> Calendar:
        """Aggregate this triangle to its calendar-period diagonals.

        Each row of the result is one ``(group, calendar)`` cell: the sum
        of all triangle cells on the same calendar diagonal, with
        ``loss`` / ``premium`` / ``ratio`` cumulative columns plus
        ``incr_`` per-period siblings and within-calendar shares. The
        ``cal_idx`` column is a sequential 1-based index per group (rank
        of the calendar date within its group), not a ``duration`` period.
        """
        from .calendar import Calendar

        return Calendar._from_triangle(self)

    def total_agg(self) -> Total:
        """Aggregate this triangle to per-group portfolio totals.

        One row per group with cohort count, sales window, total loss /
        premium, loss ratio, and within-portfolio shares -- the time
        dimension collapsed. Use for portfolio-level comparisons across
        groups.
        """
        from .total import Total

        return Total._from_triangle(self)

    def link(
        self,
        target: str = "loss",
        exposure: str | None = "premium",
        weight: str | None = None,
        min_denom: float = 0.0,
        drop_invalid: bool = False,
    ) -> Link:
        """Build the long-format link table.

        Returns a :class:`Link` data class — one row per (cohort,
        adjacent duration pair) — that exposes the per-cell ATA factor
        and (when ``exposure`` is supplied) the per-cell additive intensity.
        Diagnostic methods :meth:`Link.ata` and :meth:`Link.intensity`
        aggregate across cohorts to per-link summaries.

        ``tri.link()`` is the single canonical entry point for
        factor-level diagnostics. The chains:

        - ``tri.link().ata()``                    → :class:`ATA`
        - ``tri.link().intensity()``              → :class:`Intensity`

        Parameters
        ----------
        target
            Cumulative metric used as the link numerator. One of
            ``"loss"``, ``"premium"``, ``"ratio"``. Default ``"loss"``.
        exposure
            Optional cumulative metric for the additive exposure anchor.
            Default ``"premium"``. Pass ``None`` for ATA-only mode.
        weight
            Optional WLS weight column (cannot combine with exposure).
        min_denom
            Minimum denominator required to compute ``ata`` /
            ``intensity``. Default ``0``.
        drop_invalid
            If ``True``, rows with non-finite ``ata`` (single-var) or
            ``intensity`` (dual-var) are dropped. Default ``False``.

        Examples
        --------
        >>> tri = lr.Triangle(df, groups="coverage")
        >>> link = tri.link()                          # target='loss', exposure='premium'
        >>> link = tri.link(target='loss')             # ATA-only
        >>> link.ata().summary()
        """
        from .link import Link

        return Link._from_triangle(
            self,
            target=target,
            exposure=exposure,
            weight=weight,
            min_denom=min_denom,
            drop_invalid=drop_invalid,
        )

    def mask(self, holdout: int = 0) -> Triangle:
        """Drop the most-recent ``holdout`` calendar diagonals.

        Returns a new :class:`Triangle` with the most-recent ``holdout``
        diagonals removed -- the standard hold-out pattern used by
        :class:`Backtest`. ``holdout=0`` returns a shallow copy with the
        same underlying frame (no rows removed).

        Parameters
        ----------
        holdout
            Non-negative integer. Number of trailing calendar diagonals
            to remove per group.

        Returns
        -------
        Triangle
            A masked Triangle with the same metadata (``groups``,
            ``cohort``, ``calendar``, ``grain``, ``duration``).

        Raises
        ------
        ValueError
            If ``holdout`` is negative, or if masking removes every
            observation.
        """
        if not isinstance(holdout, int) or isinstance(holdout, bool):
            raise ValueError(
                f"`holdout` must be a non-negative integer, got "
                f"{type(holdout).__name__}."
            )
        if holdout < 0:
            raise ValueError(
                f"`holdout` must be a non-negative integer, got {holdout}."
            )
        if holdout == 0:
            return Triangle._from_masked(self, self._df)

        # Compute per-group calendar index = cohort_rank + duration - 1, where
        # cohort_rank is the dense rank of distinct cohort values (oldest
        # = 1).
        partition = normalize_groups(self._groups)
        coh_rank_expr = (
            pl.col("cohort").rank(method="dense").over(partition)
            if partition
            else pl.col("cohort").rank(method="dense")
        ).cast(pl.Int64).alias("_coh_rank")

        df = self._df.with_columns(coh_rank_expr).with_columns(
            (pl.col("_coh_rank") + pl.col("duration") - 1).alias("_cal_idx")
        )
        max_cal_expr = (
            pl.col("_cal_idx").max().over(partition)
            if partition
            else pl.col("_cal_idx").max()
        )
        df = df.with_columns(max_cal_expr.alias("_max_cal")).filter(
            pl.col("_cal_idx") <= (pl.col("_max_cal") - holdout)
        ).drop(["_coh_rank", "_cal_idx", "_max_cal"])

        if df.height == 0:
            raise ValueError(
                f"After masking with `holdout={holdout}`, no observations "
                f"remain. Reduce `holdout`."
            )

        return Triangle._from_masked(self, df)

    def usage(
        self,
        *,
        recent: int | None = None,
        regime: RegimeArg = None,
        treatment: str = "latest_only",
        holdout: int | None = None,
    ) -> FrameLike:
        """Per-cell fit-usage status grid (the data behind :meth:`plot_usage`).

        Returns one row per ``group x cohort x duration`` cell with a ``status``
        of ``"used"`` / ``"unused"`` / ``"holdout"`` / ``"future"`` / ``"donor"``
        under the given filters -- so a caller can render the usage view itself
        instead of the bundled matplotlib plot. Only OBSERVED cells are ever
        flagged used / donor; projection cells stay ``"future"``.
        :meth:`plot_usage` is exactly the chart over this frame, with the same
        input resolution; this is its data source of truth.

        Parameters
        ----------
        recent
            Number of trailing calendar diagonals kept as ``"used"`` (the
            recent calendar-diagonal window); cells outside drop to
            ``"unused"``. ``None`` applies no recent filter.
        regime
            ``None``, a :class:`Regime`, or a :class:`RegimeDetector` -- the
            cohort regime whose cut / segmentation the usage view reflects.
        treatment
            How the regime is consumed (the estimator's knob, mirror it here):
            ``latest_only`` (default) drops the pre-change cohorts to
            ``"unused"``; ``segment_wise`` / ``covariate`` keep every regime, and
            under ``segment_wise`` the older regimes' observed cells past the
            newest regime's depth are flagged ``"donor"`` (the graft donor).
        holdout
            Number of trailing calendar diagonals flagged ``"holdout"`` (the
            :class:`Backtest` hold-out pattern).

        Returns
        -------
        DataFrame
            Columns: the group key(s) when set, ``cohort``, ``duration``,
            ``status``. Input mirroring is preserved (pandas in -> pandas
            out).
        """
        from ..diagnostics.regime import Regime, _resolve_regime, _resolve_to_regime
        from .usage import _compute_triangle_usage

        # resolve a RegimeDetector to a concrete Regime first, so the cohort cut
        # is computed here. `treatment` is the estimator's consumption knob,
        # supplied by the caller; it only bites when a concrete Regime is present.
        regime = _resolve_to_regime(regime, self)
        regime_cut = _resolve_regime(regime, self)
        eff_treatment = treatment if isinstance(regime, Regime) else "latest_only"
        usage_df = _compute_triangle_usage(
            self, recent=recent, regime_cut=regime_cut, holdout=holdout,
            treatment=eff_treatment,
        )
        return mirror_output(usage_df, self._output_type)

    def plot_triangle(
        self,
        metric: str = "ratio",
        label_style: LabelStyle = "plain",
        label_size: float | None = None,
        amount_divisor: float | str = "auto",
        *,
        x_axis: XAxis = "duration",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Cell-value heatmap of the aligned cohort x duration triangle.

        For the categorical fit-usage status heatmap, use
        :meth:`plot_usage`.

        Parameters
        ----------
        metric
            One of: ``"ratio"``, ``"incr_ratio"``, ``"loss"``,
            ``"incr_loss"``, ``"premium"``, ``"incr_premium"``,
            ``"margin"``, ``"incr_margin"``, ``"loss_share"``,
            ``"incr_loss_share"``, ``"premium_share"``,
            ``"incr_premium_share"``.
        label_style
            ``"plain"`` (default; one number per cell) or ``"detail"``
            (ratio metrics get a second-line ``"(loss/premium)"``
            breakdown).
        label_size
            matplotlib font size for cell labels. Defaults to ``8`` for
            ``"plain"`` and ``7`` for ``"detail"``.
        amount_divisor
            Numeric divisor for amount metrics (and for the ratio
            ``"detail"`` breakdown). ``"auto"`` selects the largest of
            ``{1, 1e3, 1e6, 1e9, 1e12}`` that keeps the median formatting
            non-zero at ``%.1f``.
        x_axis
            ``"duration"`` (default; columns are the duration index, the
            aligned right-triangle layout) or ``"calendar"`` (columns
            are the calendar period of each cell, so cohorts sit on
            their own diagonal -- the staircase layout the raw data sits
            in before alignment). Each cell's calendar period is
            ``cohort + (duration - 1)`` at the triangle grain.
        nrow, ncol
            Facet wrap layout when ``groups`` is set. Defaults to a
            near-square grid.
        figsize
            Passed to ``plt.subplots``. Defaults to a size scaled by
            the cell count.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from .._plot.triangle import plot_triangle as _impl
        return _impl(
            self,
            metric=metric,
            label_style=label_style,
            label_size=label_size,
            amount_divisor=amount_divisor,
            x_axis=x_axis,
            nrow=nrow,
            ncol=ncol,
            figsize=figsize,
        )

    def plot_usage(
        self,
        recent: int | None = None,
        regime: RegimeArg = None,
        treatment: str = "latest_only",
        holdout: int | None = None,
        *,
        x_axis: XAxis = "duration",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Categorical fit-usage status heatmap, backed by matplotlib.

        Each ``(cohort, duration)`` cell is coloured by its status
        (``"used"`` / ``"unused"`` / ``"holdout"`` / ``"future"`` /
        ``"donor"``) under the given ``recent`` / ``regime`` / ``holdout``
        masks -- the chart over the same grid :meth:`usage` returns. For
        the cell-value metric heatmap, use :meth:`plot_triangle`.

        Parameters
        ----------
        recent
            Number of trailing calendar diagonals to keep as "used".
        regime
            ``None``, a :class:`Regime` instance, or a
            :class:`RegimeDetector` -- resolved to the same cohort cut the
            fit uses; cohorts before the change show as "unused", with a
            dashed hline at the cut.
        treatment
            How the regime is consumed (the estimator's knob, mirrored
            here): ``"latest_only"`` (default) drops the pre-change
            cohorts to "unused"; ``"segment_wise"`` / ``"covariate"`` keep
            every regime, and under ``"segment_wise"`` the older regimes'
            observed cells past the newest regime's depth are flagged
            "donor".
        holdout
            Number of trailing calendar diagonals to flag as "holdout"
            (the hold-out pattern used by :class:`Backtest`).
        x_axis
            ``"duration"`` (default; the aligned right-triangle layout) or
            ``"calendar"`` (each cohort on its own diagonal -- the
            staircase layout). On the calendar axis the ``recent`` /
            ``holdout`` calendar-diagonal masks become clean vertical
            bands.
        nrow, ncol
            Facet wrap layout when ``groups`` is set. Defaults to a
            near-square grid.
        figsize
            Passed to ``plt.subplots``. Defaults to a size scaled by
            the cell count.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from .._plot.triangle_usage import plot_triangle_usage as _impl
        return _impl(
            self,
            recent=recent,
            regime=regime,
            treatment=treatment,
            holdout=holdout,
            x_axis=x_axis,
            nrow=nrow,
            ncol=ncol,
            figsize=figsize,
        )

    def plot(
        self,
        metric: str = "ratio",
        summary: bool = False,
        summary_min_n: int = 5,
        regime: RegimeArg = None,
        amount_divisor: float | str = "auto",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Cohort-trajectory line plot.

        One line per cohort -- x is the duration index, y the selected
        ``metric`` (default cumulative loss ``"ratio"``) -- faceted by
        ``groups``. With ``summary=True`` (ratio metrics only) the
        per-cohort lines fade to grey and Mean / Median / Weighted summary
        lines are overlaid, masked at durations where fewer than
        ``summary_min_n`` cohorts contribute (a dotted vline marks the
        first such period).

        ``regime`` (a :class:`~lossratio.Regime` / :class:`~lossratio.RegimeDetector`,
        used only with ``summary=True``) splits the summary by regime: each
        facet's cohorts are tinted per regime and get their own Mean / Median /
        Weighted trio, so a regime-shifted book shows one summary per level
        instead of a single line threading between the bands.

        For the cell-value heatmap (the aligned cohort x duration triangle),
        use :meth:`plot_triangle` instead.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from .._plot.triangle import plot as _impl
        return _impl(
            self,
            metric=metric,
            summary=summary,
            summary_min_n=summary_min_n,
            regime=regime,
            amount_divisor=amount_divisor,
            nrow=nrow,
            ncol=ncol,
            figsize=figsize,
        )

    def __repr__(self) -> str:
        bits = [f"{self._df.height:,} rows"]
        if self._groups is not None:
            n_groups = self._df.select(normalize_groups(self._groups)).unique().height
            bits.append(f"{n_groups} groups")
        n_cohorts = self._df["cohort"].n_unique()
        n_durations = self._df["duration"].n_unique()
        bits.append(f"{n_cohorts} cohorts x {n_durations} durations ({self._grain})")
        return f"<Triangle: {', '.join(bits)}>"

    def __len__(self) -> int:
        return self._df.height


class TriangleValidation:
    """Validation report for a raw experience-style DataFrame.

    Direct-instantiate result class. Runs two checks on the supplied
    raw frame -- *before* aggregation by :class:`Triangle` -- and
    surfaces the findings:

    1. **Row-level calendar consistency**: rows where
       ``calendar < cohort`` (an event recorded before the cohort
       starts, which is logically impossible). Available only when
       ``calendar`` is supplied.
    2. **Cohort duration-sequence gaps**: per ``(groups?, cohort)``, the
       observed integer duration sequence is checked for non-consecutive
       values in ``[duration_min, duration_max]``. When ``duration`` is supplied it is
       used directly; otherwise it is derived from
       ``cohort + calendar + grain`` (the same
       :class:`Triangle` 3-mode dispatch).

    Parameters
    ----------
    df
        Raw experience DataFrame (polars or pandas).
    groups, cohort, calendar, duration
        Column names. ``cohort`` is required; at least one of
        ``calendar`` / ``duration`` is required.
    loss, premium
        Optional column names. Not used by validation itself but
        accepted for signature symmetry with :class:`Triangle` (so the
        same kwargs work in both calls).
    grain
        Grain for cohort/calendar binning. Default ``"auto"``.

    The findings are surfaced by the ``gaps`` / ``invalid_rows`` properties
    and the ``summary()`` method (documented individually below).
    """

    def __init__(
        self,
        df: FrameLike,
        groups: str | Sequence[str] | None = None,
        cohort: str = "uy_m",
        calendar: str | None = "cy_m",
        duration: str | None = None,
        loss: str | None = None,
        premium: str | None = None,
        grain: Grain | Literal["auto"] = "auto",
    ) -> None:
        self._output_type = detect_input_type(df)
        df_pl = to_polars(df)

        if calendar is None and duration is None:
            raise ValueError(
                "Must supply at least one of `calendar` or `duration`."
            )

        # `loss` / `premium` are accepted for kwarg-symmetry with
        # Triangle but not consumed by validation. Reference them in a
        # no-op so type checkers don't flag unused params.
        del loss, premium

        required = {cohort}
        required.update(normalize_groups(groups))
        if calendar is not None:
            required.add(calendar)
        if duration is not None:
            required.add(duration)
        missing = required - set(df_pl.columns)
        if missing:
            raise ValueError(
                f"Missing required columns: {sorted(missing)}. "
                f"Required: {sorted(required)}"
            )

        self._groups = collapse_groups(groups)
        self._cohort = cohort
        self._calendar = calendar
        self._duration_col = duration
        self._grain_arg = grain

        # --- 1. Row-level calendar < cohort consistency ------------------
        if calendar is not None:
            # Coerce both date columns to Date before comparison so int /
            # str inputs don't slip through silently as text compare.
            check_df = coerce_cols_to_date(df_pl, [cohort, calendar])
            bad_mask = (
                pl.col(cohort).is_not_null()
                & pl.col(calendar).is_not_null()
                & (pl.col(calendar) < pl.col(cohort))
            )
            invalid = check_df.filter(bad_mask)
            if invalid.height:
                keep_cols = [c for c in (
                    normalize_groups(groups)
                ) + [cohort, calendar] + (
                    [duration] if duration is not None else []
                ) if c in invalid.columns]
                invalid = invalid.select(keep_cols).with_columns(
                    pl.lit(f"{calendar} < {cohort}").alias("reason")
                )
            df_for_gaps = check_df.filter(~bad_mask)
        else:
            invalid = pl.DataFrame()
            df_for_gaps = df_pl

        self._invalid_rows = invalid

        # --- 2. duration-sequence gaps on the cleaned data --------------------
        # Derive duration when only calendar is supplied (the same Triangle
        # 3-mode dispatch).
        if duration is None:
            # duration absent -> calendar is present (validated on entry).
            assert calendar is not None
            input_grain = infer_grain(df_for_gaps[cohort])
            g = resolve_grain(input_grain, grain)
            df_for_gaps = df_for_gaps.with_columns(
                count_periods(pl.col(cohort), pl.col(calendar), g)
                .alias("_duration_derived")
            )
            duration_col_name = "_duration_derived"
        else:
            df_for_gaps = df_for_gaps.with_columns(
                pl.col(duration).cast(pl.Int64).alias("_duration_explicit")
            )
            duration_col_name = "_duration_explicit"

        gap_keys: list[str] = normalize_groups(groups)
        gap_keys.append(cohort)

        # Per (groups?, cohort): duration_min / duration_max / n_duration (distinct) /
        # n_expected (range length) / missing (list of integers).
        gap_summary = (
            df_for_gaps.filter(pl.col(duration_col_name).is_not_null())
            .group_by(gap_keys)
            .agg(
                pl.col(duration_col_name).min().cast(pl.Int64).alias("duration_min"),
                pl.col(duration_col_name).max().cast(pl.Int64).alias("duration_max"),
                pl.col(duration_col_name).n_unique().cast(pl.Int64).alias("n_duration"),
            )
            .with_columns(
                (pl.col("duration_max") - pl.col("duration_min") + 1)
                .cast(pl.Int64)
                .alias("n_expected"),
            )
        )

        gaps = gap_summary.filter(pl.col("n_duration") != pl.col("n_expected"))

        # Per-cohort missing duration values (set difference inside polars
        # is awkward; do it in Python for the gap rows only).
        if gaps.height:
            observed_durations = (
                df_for_gaps.filter(pl.col(duration_col_name).is_not_null())
                .group_by(gap_keys)
                .agg(pl.col(duration_col_name).unique().alias("_obs"))
            )
            gaps = gaps.join(observed_durations, on=gap_keys, how="left")
            missing_lists: list[list[int]] = []
            for row in gaps.iter_rows(named=True):
                obs = set(int(v) for v in row["_obs"])
                rng = range(int(row["duration_min"]), int(row["duration_max"]) + 1)
                missing_lists.append(sorted(int(v) for v in rng if v not in obs))
            gaps = gaps.drop("_obs").with_columns(
                pl.Series("missing", missing_lists)
            )
        else:
            # Add the empty `missing` column to keep schema stable across
            # the "no gaps" and "has gaps" cases.
            gaps = gaps.with_columns(
                pl.lit([]).cast(pl.List(pl.Int64)).alias("missing")
            )

        self._gaps = gaps

        # --- 3. Summary count table --------------------------------------
        self._summary = pl.DataFrame(
            {
                "check": [
                    "calendar < cohort (rows)",
                    "non-consecutive duration (cohorts)",
                ],
                "n": [self._invalid_rows.height, self._gaps.height],
            }
        )

    # ------------------------------------------------------------------
    # Public accessors -- same input-type machinery as Triangle
    # ------------------------------------------------------------------

    @property
    def gaps(self):
        """DataFrame of cohorts with non-consecutive duration sequences."""
        return mirror_output(self._gaps, self._output_type)

    @property
    def invalid_rows(self):
        """DataFrame of rows where ``calendar < cohort``."""
        return mirror_output(self._invalid_rows, self._output_type)

    def summary(self):
        """Two-row count summary of findings.

        A zero-arg method (not a property) to match every other result
        class's ``summary()`` -- call ``validation.summary()``.
        """
        return mirror_output(self._summary, self._output_type)

    @property
    def is_clean(self) -> bool:
        """``True`` iff both checks found zero violations."""
        return self._gaps.height == 0 and self._invalid_rows.height == 0

    @property
    def groups(self) -> str | list[str] | None:
        """Group column name(s): a ``str`` for a single group column, a
        ``list[str]`` for multiple, or ``None`` if ungrouped."""
        return self._groups

    @property
    def cohort(self) -> str:
        return self._cohort

    @property
    def calendar(self) -> str | None:
        return self._calendar

    @property
    def duration(self) -> str | None:
        return self._duration_col

    def to_polars(self) -> pl.DataFrame:
        """The gaps table as a polars DataFrame."""
        return self._gaps

    def to_pandas(self) -> pd.DataFrame:
        """The gaps table as a pandas DataFrame."""
        return self._gaps.to_pandas()

    def plot(
        self,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Bar chart of observed vs expected duration counts per cohort.

        One panel per group, with two bars per cohort (``n_duration`` blue,
        ``n_expected`` grey). When the validation found no gaps, returns a
        figure containing only a "nothing to visualise" placeholder.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from .._plot.validation import plot_validation
        return plot_validation(self, nrow=nrow, ncol=ncol, figsize=figsize)

    def plot_triangle(
        self,
        x_axis: XAxis = "duration",
        show_label: bool = False,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Cohort x duration / cohort x calendar heatmap of observed /
        missing cells.

        For each cohort with gaps, the cell grid is coloured blue
        (observed) or red (missing); clean cohorts are omitted (only the
        gaps frame is carried, so non-gappy cohorts don't appear).

        Parameters
        ----------
        x_axis
            ``"duration"`` (default) or ``"calendar"`` -- the same layout
            selector as :meth:`Triangle.plot_triangle`. The calendar
            layout synthesises per-cell calendar values via
            ``cohort + (duration - 1) * grain_step`` and requires the
            cohort series to have an inferable grain. Raises if no
            ``calendar`` column was supplied to
            :class:`TriangleValidation`.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from .._plot.validation import plot_triangle_validation
        return plot_triangle_validation(
            self,
            x_axis=x_axis,
            show_label=show_label,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )

    def __repr__(self) -> str:
        if self.is_clean:
            tail = "clean"
        else:
            bits = []
            if self._invalid_rows.height:
                bits.append(
                    f"{self._invalid_rows.height} invalid row(s)"
                )
            if self._gaps.height:
                bits.append(
                    f"{self._gaps.height} cohort(s) with gaps"
                )
            tail = ", ".join(bits)
        return f"<TriangleValidation: {tail}>"

    def __len__(self) -> int:
        """Number of cohorts with duration-sequence gaps."""
        return self._gaps.height
