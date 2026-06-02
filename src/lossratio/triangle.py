"""Triangle: cohort x dev aggregated experience data."""

from __future__ import annotations

from collections.abc import Sequence
from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import (
    collapse_groups,
    detect_input_type,
    mirror_output,
    normalize_groups,
    to_polars,
)
from ._period import (
    coerce_cols_to_date,
    count_periods,
    floor_cols_to_period,
    infer_grain,
    resolve_grain,
)

if TYPE_CHECKING:
    from .calendar import Calendar
    from .link import Link
    from .maturity import Maturity
    from .regime import Regime
    from .total import Total


# Triangle-specific: standard dev column name per grain code.
_GRAIN_TO_DEV_VAR = {
    "M": "dev_m",
    "Q": "dev_q",
    "H": "dev_h",
    "Y": "dev_y",
}


class Triangle:
    """Cohort x development period aggregated experience data.

    A Triangle is built by aggregating a raw experience DataFrame
    (polars or pandas) over ``groups`` (optional), cohort, and
    development period.

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

    Three-mode dispatch on ``calendar`` / ``dev`` (mirrors the R
    sibling's ``as_triangle``):

    - mode 1 (cohort + calendar): ``dev`` is derived from the two.
    - mode 2 (cohort + dev, no calendar): the supplied ``dev`` column
      is taken directly; the ``calendar`` attribute is ``None``.
    - mode 3 (cohort + calendar + dev): the supplied ``dev`` is
      cross-checked against the derived value, then used as given.

    The resulting frame has columns:

    * ``groups`` -- present only if supplied
    * ``n_cohorts`` -- distinct cohorts observed per (group, dev)
    * ``cohort`` -- the underwriting period
    * ``dev`` -- the development index (1, 2, ...) within each cohort
    * ``loss``, ``premium`` -- cumulative sums within each (group, cohort)
    * ``incr_loss``, ``incr_premium`` -- per-period sums per cell
    * ``ratio``, ``incr_ratio`` -- cumulative / per-period loss ratio
    * ``margin``, ``incr_margin`` -- ``premium - loss``
    * ``profit``, ``incr_profit`` -- ``"pos"`` / ``"neg"`` indicator
    * ``loss_share``, ``incr_loss_share`` -- within-(cohort, dev) shares
    * ``premium_share``, ``incr_premium_share`` -- ditto for premium

    Cumulative is the unmarked default; per-period values carry an
    ``incr_`` (incremental) prefix.

    Original column names (e.g. ``"uy_m"`` for cohort) are kept as
    instance attributes for downstream plotting and Calendar.
    """

    def __init__(
        self,
        df: pl.DataFrame | Any,
        groups: str | Sequence[str] | None = None,
        cohort: str = "uy_m",
        calendar: str | None = "cy_m",
        dev: str | None = None,
        loss: str = "incr_loss",
        premium: str = "incr_premium",
        grain: str = "auto",
        cell_type: str = "incremental",
        fill_gaps: bool = False,
    ) -> None:
        self._output_type = detect_input_type(df)
        df_pl = to_polars(df)

        if cell_type not in ("incremental", "cumulative"):
            raise ValueError(
                f"cell_type must be 'incremental' or 'cumulative', "
                f"got {cell_type!r}"
            )
        if calendar is None and dev is None:
            raise ValueError(
                "Must supply at least one of `calendar` or `dev`."
            )

        # Required columns: cohort, loss, premium, groups (if set), and
        # whichever of calendar / dev were supplied.
        required = {cohort, loss, premium}
        required.update(normalize_groups(groups))
        if calendar is not None:
            required.add(calendar)
        if dev is not None:
            required.add(dev)
        missing = required - set(df_pl.columns)
        if missing:
            raise ValueError(
                f"Missing required columns: {sorted(missing)}. "
                f"Required: {sorted(required)}"
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
        # when present, else dev -- both monotone within a cohort.
        if cell_type == "cumulative":
            sort_axis = calendar if calendar is not None else dev
            sort_keys: list[str] = normalize_groups(groups)
            sort_keys.extend([cohort, sort_axis])  # type: ignore[list-item]
            diff_over: list[str] = normalize_groups(groups)
            diff_over.append(cohort)
            df_pl = df_pl.sort(sort_keys).with_columns(
                (pl.col(loss) - pl.col(loss).shift(1, fill_value=0.0)
                 .over(diff_over)).alias(loss),
                (pl.col(premium) - pl.col(premium).shift(1, fill_value=0.0)
                 .over(diff_over)).alias(premium),
            )

        # Auto-detect input grain; resolve "auto" or validate explicit value.
        input_grain = infer_grain(df_pl[cohort])
        grain = resolve_grain(input_grain, grain)

        # Bin cohort (and calendar if present) to requested grain.
        if grain != input_grain:
            df_pl = floor_cols_to_period(df_pl, date_cols, grain)

        # Three-mode dispatch: derive / validate the integer dev axis.
        if calendar is not None and dev is not None:
            # mode 3: cross-check supplied dev against derived.
            df_pl = df_pl.with_columns(
                count_periods(pl.col(cohort), pl.col(calendar), grain)
                .alias("_dev_derived"),
                pl.col(dev).cast(pl.Int64).alias("_dev_temp"),
            )
            mismatch = df_pl.filter(
                pl.col("_dev_derived").is_not_null()
                & pl.col("_dev_temp").is_not_null()
                & (pl.col("_dev_derived") != pl.col("_dev_temp"))
            )
            if mismatch.height:
                raise ValueError(
                    f"`dev` is inconsistent with `cohort` + `calendar` "
                    f"(grain {grain!r}) in {mismatch.height} row(s)."
                )
            df_pl = df_pl.drop("_dev_derived")
        elif calendar is not None:
            # mode 1: derive dev from cohort + calendar.
            df_pl = df_pl.with_columns(
                count_periods(pl.col(cohort), pl.col(calendar), grain)
                .alias("_dev_temp")
            )
        else:
            # mode 2: take supplied dev directly.
            df_pl = df_pl.with_columns(
                pl.col(dev).cast(pl.Int64).alias("_dev_temp")
            )

        # Aggregate per-period values by (groups, cohort, dev).
        agg_keys: list[str] = normalize_groups(groups)
        agg_keys.extend([cohort, "_dev_temp"])

        agg = (
            df_pl.group_by(agg_keys)
            .agg(
                pl.col(loss).sum().alias("incr_loss"),
                pl.col(premium).sum().alias("incr_premium"),
            )
            .sort(agg_keys)
        )

        # n_cohorts: distinct cohorts observed per (group, dev). Computed
        # on the pre-fill aggregate so zero-filled gap cells do not
        # inflate the count (R parity: `dn` is built before gap-filling).
        ndev_keys: list[str] = normalize_groups(groups)
        ndev_keys.append("_dev_temp")
        ncoh = (
            agg.group_by(ndev_keys)
            .agg(pl.col(cohort).n_unique().alias("n_cohorts"))
        )

        # Check for non-consecutive dev sequences within each cohort.
        # R parity: as_triangle(fill_gaps = FALSE) raises; TRUE zero-fills.
        gap_keys = agg_keys[:-1]  # groups + cohort (or cohort alone)
        gap_summary = (
            agg.group_by(gap_keys)
            .agg(
                pl.col("_dev_temp").min().alias("dev_min"),
                pl.col("_dev_temp").max().alias("dev_max"),
                pl.col("_dev_temp").n_unique().alias("dev_observed"),
            )
            .with_columns(
                (pl.col("dev_max") - pl.col("dev_min") + 1).alias("dev_expected"),
            )
        )
        gaps = gap_summary.filter(
            pl.col("dev_observed") != pl.col("dev_expected")
        )
        if gaps.height:
            if fill_gaps:
                # Build full (group, cohort, dev) grid spanning each cohort's
                # observed dev range, left-join the aggregate, zero-fill.
                full_grid = (
                    gap_summary
                    .with_columns(
                        pl.int_ranges(
                            pl.col("dev_min"), pl.col("dev_max") + 1
                        ).alias("_dev_temp")
                    )
                    .explode("_dev_temp")
                    .select([*gap_keys, "_dev_temp"])
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
                    f"dev sequences. Pass fill_gaps=True to zero-fill, or "
                    f"inspect with TriangleValidation(...) (same kwargs)."
                )

        # Join n_cohorts, then rename cohort / dev to standard names so
        # subsequent `over` / `sum().over` expressions read the canonical
        # columns.
        agg = agg.join(ncoh, on=ndev_keys, how="left")
        agg = agg.rename({cohort: "cohort", "_dev_temp": "dev"})

        cum_keys: list[str] = normalize_groups(groups)
        cum_keys.append("cohort")

        coh_dev = ["cohort", "dev"]

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
            # within-(cohort, dev) shares -- proportions sum to 1 across groups
            (pl.col("loss") / pl.col("loss").sum().over(coh_dev))
                .alias("loss_share"),
            (pl.col("incr_loss") / pl.col("incr_loss").sum().over(coh_dev))
                .alias("incr_loss_share"),
            (pl.col("premium") / pl.col("premium").sum().over(coh_dev))
                .alias("premium_share"),
            (pl.col("incr_premium") / pl.col("incr_premium").sum().over(coh_dev))
                .alias("incr_premium_share"),
        )

        # Reorder columns: cum-first paired (matches R as_triangle).
        ordered = normalize_groups(groups)
        ordered.extend([
            "n_cohorts", "cohort", "dev",
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
        self._dev = _GRAIN_TO_DEV_VAR[grain]

    @property
    def df(self):
        """Return the triangle data in the original input format."""
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        """Return the triangle data as a polars DataFrame."""
        return self._df

    def to_pandas(self):
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
        ``list[str]`` for multiple, or ``None`` if ungrouped."""
        return self._groups

    @property
    def cohort(self) -> str:
        """Original cohort variable name (e.g. 'uy_m')."""
        return self._cohort

    @property
    def calendar(self) -> str | None:
        """Original calendar column name (e.g. 'cy_m').

        ``None`` for a mode-2 (dev-only) triangle: no calendar column
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
    def dev(self) -> str:
        """Standard dev column name for the grain (e.g. 'dev_m')."""
        return self._dev

    @property
    def grain(self) -> str:
        """Triangle grain code: ``"M"`` / ``"Q"`` / ``"H"`` / ``"Y"``.

        Mirrors the standard column suffix (``"M"`` ↔ ``dev_m``, etc.).
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
        tri._dev = original._dev
        return tri

    def calendar_agg(self) -> "Calendar":
        """Aggregate this triangle to its calendar-period diagonals.

        Each row of the result is one ``(group, calendar)`` cell: the sum
        of all triangle cells on the same calendar diagonal, with
        ``loss`` / ``premium`` / ``ratio`` cumulative columns plus
        ``incr_`` per-period siblings and within-calendar shares. The
        ``cal_idx`` column is a sequential 1-based index per group (rank
        of the calendar date within its group), not a ``dev`` period.
        """
        from .calendar import Calendar

        return Calendar._from_triangle(self)

    def total_agg(self) -> "Total":
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
        adjacent dev pair) — that exposes the per-cell ATA factor
        and (when ``exposure`` is supplied) the per-cell ED intensity.
        Diagnostic methods :meth:`Link.ata` and :meth:`Link.intensity`
        aggregate across cohorts to per-link summaries.

        ``tri.link()`` is the single canonical entry point for
        factor-level diagnostics. The chains:

        - ``tri.link().ata()``                    → :class:`ATA`
        - ``tri.link().intensity()``              → :class:`Intensity`
        - ``tri.link().ata().maturity(...)``      → :class:`Maturity`

        Parameters
        ----------
        target
            Cumulative metric used as the link numerator. One of
            ``"loss"``, ``"premium"``, ``"ratio"``. Default ``"loss"``.
        exposure
            Optional cumulative metric for the ED exposure anchor.
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
        >>> link.ata().maturity(max_cv=0.15)
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

    def detect_regime(
        self,
        target: str = "ratio",
        window: int | str = "auto",
        by: str | Sequence[str] | None = None,
        method: str = "e_divisive",
        n_regimes: int | None = None,
        sig_level: float = 0.05,
        R: int = 999,
        min_size: int = 3,
        seed: int | None = None,
        treatment: str = "segment_bridged",
    ) -> Regime:
        """Detect structural regime shifts across underwriting cohorts.

        Mirrors the R sibling's ``detect_regime(triangle, ...)``. The
        default ``window="auto"`` resolves each group's trajectory
        window via :meth:`detect_maturity`, falling back to the elbow
        heuristic and finally to a fixed default (``6``) when neither
        signal is available.

        Parameters
        ----------
        target
            Metric to drive change-point detection. Native cumulative
            (``"loss"``, ``"premium"``, ``"ratio"``) or derived
            (``"loss_ata"``, ``"premium_ata"``, ``"loss_ed"``,
            ``"premium_ed"``). Default ``"ratio"``.
        window
            Trajectory window. Integer (e.g. ``12``) for a fixed
            window, or ``"auto"`` (default) for the maturity-first
            elbow-fallback resolver.
        by
            Optional override for the per-group dispatch. ``None``
            (default) uses the Triangle's stored ``groups``; an empty
            string forces pooled detection; a string names a single
            group column.
        method, n_regimes, sig_level, R, min_size, seed, treatment
            See :class:`Regime` for full description.
        """
        from .regime import Regime

        return Regime._from_triangle(
            self,
            target=target,
            window=window,
            by=by,
            method=method,
            n_regimes=n_regimes,
            sig_level=sig_level,
            R=R,
            min_size=min_size,
            seed=seed,
            treatment=treatment,
        )

    def detect_maturity(
        self,
        loss: str = "loss",
        weight: str | None = None,
        max_cv: float = 0.15,
        max_rse: float = 0.05,
        min_run: int = 2,
    ) -> Maturity:
        """Detect the age-to-age maturity point ``k*``.

        Convenience entry point for the canonical chain
        ``triangle.link(target=loss, weight=weight).ata().maturity(...)``.
        Mirrors the R sibling's ``detect_maturity(triangle, ...)``.

        Maturity is determined jointly by:

        * cross-cohort ``CV(f_k) < max_cv``
        * pooled-factor ``RSE(f_k) < max_rse``
        * sustained for ``min_run`` consecutive stable links

        Parameters
        ----------
        loss
            Cumulative metric used as the link numerator. Default
            ``"loss"`` (chain-ladder convention). One of ``"loss"``,
            ``"premium"``, ``"ratio"``. Forwarded to
            :meth:`Triangle.link` as ``target``.
        weight
            Optional WLS weight column. Forwarded to :meth:`Triangle.link`.
        max_cv
            Maximum cross-cohort coefficient of variation.
            Default ``0.15``.
        max_rse
            Maximum pooled-factor relative standard error.
            Default ``0.05``.
        min_run
            Minimum consecutive stable links. Default ``2``.

        Returns
        -------
        Maturity
            One :class:`Maturity` per Triangle (per group when
            ``groups`` is set on the Triangle).

        Notes
        -----
        The R sibling additionally exposes ``alpha`` (WLS variance
        structure), ``groups`` (rebucket to a coarser partition),
        ``min_valid_ratio`` and ``min_n_valid`` thresholds. Those are
        not yet plumbed through the Python ATA pipeline; the Python
        default thresholds (CV / RSE only) are the active subset of the
        R defaults.
        """
        return (
            self.link(target=loss, weight=weight)
            .ata()
            .maturity(max_cv=max_cv, max_rse=max_rse, min_run=min_run)
        )

    def mask(self, holdout: int = 0) -> Triangle:
        """Drop the most-recent ``holdout`` calendar diagonals.

        Returns a new :class:`Triangle` with the most-recent ``holdout``
        diagonals removed -- the standard hold-out pattern used by
        :class:`Backtest`. ``holdout=0`` returns a shallow copy with the
        same underlying frame (no rows removed).

        Mirrors the R sibling's ``mask_triangle(x, holdout)``.

        Parameters
        ----------
        holdout
            Non-negative integer. Number of trailing calendar diagonals
            to remove per group.

        Returns
        -------
        Triangle
            A masked Triangle with the same metadata (``groups``,
            ``cohort``, ``calendar``, ``grain``, ``dev``).

        Raises
        ------
        ValueError
            If ``holdout`` is negative, or if masking removes every
            observation.
        """
        if not isinstance(holdout, (int,)) or isinstance(holdout, bool):
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

        # Compute per-group calendar index = cohort_rank + dev - 1, where
        # cohort_rank is the dense rank of distinct cohort values (oldest
        # = 1). Mirrors R: data.table::frank(cohort, ties.method = "dense").
        partition = normalize_groups(self._groups)
        coh_rank_expr = (
            pl.col("cohort").rank(method="dense").over(partition)
            if partition
            else pl.col("cohort").rank(method="dense")
        ).cast(pl.Int64).alias("_coh_rank")

        df = self._df.with_columns(coh_rank_expr).with_columns(
            (pl.col("_coh_rank") + pl.col("dev") - 1).alias("_cal_idx")
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

    def plot_triangle(
        self,
        view: str = "value",
        metric: str = "ratio",
        label_style: str = "value",
        label_size: float | None = None,
        amount_divisor: float | str = "auto",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
        *,
        x_axis: str = "dev",
        recent: int | None = None,
        regime: Any = None,
        holdout: int | None = None,
        maturity: Any = None,
    ) -> Any:
        """Triangle heatmap (cell-value or status), backed by matplotlib.

        Parameters
        ----------
        view
            ``"value"`` (default; cell-value heatmap of one metric) or
            ``"usage"`` (status heatmap showing which cells the fit
            would use vs. drop under the given ``recent`` / ``regime`` /
            ``holdout`` / ``maturity`` masks).
        x_axis
            ``"dev"`` (default; columns are the development index, the
            aligned right-triangle layout) or ``"calendar"`` (columns
            are the calendar period of each cell, so cohorts sit on
            their own diagonal -- the staircase layout the raw data sits
            in before alignment). Each cell's calendar period is
            ``cohort + (dev - 1)`` at the triangle grain. Applies to
            both views; in the usage view the ``recent`` / ``holdout``
            calendar-diagonal masks become clean vertical bands, and the
            ``maturity`` boundary becomes a stepped diagonal.
        metric
            (value view) One of: ``"ratio"``, ``"incr_ratio"``,
            ``"loss"``, ``"incr_loss"``, ``"premium"``,
            ``"incr_premium"``, ``"margin"``, ``"incr_margin"``,
            ``"loss_share"``, ``"incr_loss_share"``, ``"premium_share"``,
            ``"incr_premium_share"``.
        label_style
            (value view) ``"value"`` (default; one number per cell) or
            ``"detail"`` (ratio metrics get a second-line
            ``"(loss/premium)"`` breakdown).
        label_size
            (value view) matplotlib font size for cell labels. Defaults
            to ``8`` for ``"value"`` and ``7`` for ``"detail"``.
        amount_divisor
            (value view) Numeric divisor for amount metrics (and for the
            ratio ``"detail"`` breakdown). ``"auto"`` selects the
            largest of ``{1, 1e3, 1e6, 1e9, 1e12}`` that keeps the
            median formatting non-zero at ``%.1f``.
        recent
            (usage view) Number of trailing calendar diagonals to keep
            as "used".
        regime
            (usage view) ``None``, a :class:`Regime` instance, or a
            callable ``triangle -> Regime``. A segment treatment masks
            the triangle to the bridged development band; cells outside
            the band show as "unused".
        holdout
            (usage view) Number of trailing calendar diagonals to flag
            as "holdout" (the hold-out pattern used by
            :class:`Backtest`).
        maturity
            (usage view) ``None``, an integer ``k*``, or a
            :class:`Maturity` instance. When supplied alongside
            ``regime``, switches to hybrid filtering -- cohort cut on
            ``dev < k*`` (ED region), calendar cut on ``dev >= k*``
            (CL region). Also draws a dashed vertical line at
            ``dev = k*``. ``"auto"`` is reserved for a later pass.
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
        from ._triangle_vis import plot_triangle as _impl
        return _impl(
            self,
            view=view,
            metric=metric,
            label_style=label_style,
            label_size=label_size,
            amount_divisor=amount_divisor,
            nrow=nrow,
            ncol=ncol,
            figsize=figsize,
            x_axis=x_axis,
            recent=recent,
            regime=regime,
            holdout=holdout,
            maturity=maturity,
        )

    def plot(
        self,
        metric: str = "ratio",
        summary: bool = False,
        summary_min_n: int = 5,
        amount_divisor: float | str = "auto",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Cohort-trajectory line plot (mirrors R's ``plot.Triangle``).

        One line per cohort -- x is the development index, y the selected
        ``metric`` (default cumulative loss ``"ratio"``) -- faceted by
        ``groups``. With ``summary=True`` (ratio metrics only) the
        per-cohort lines fade to grey and Mean / Median / Weighted summary
        lines are overlaid, masked at development periods where fewer than
        ``summary_min_n`` cohorts contribute (a dotted vline marks the
        first such period).

        For the cell-value heatmap (the aligned run-off triangle), use
        :meth:`plot_triangle` instead.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from ._triangle_vis import plot as _impl
        return _impl(
            self,
            metric=metric,
            summary=summary,
            summary_min_n=summary_min_n,
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
        n_devs = self._df["dev"].n_unique()
        bits.append(f"{n_cohorts} cohorts x {n_devs} devs ({self._grain})")
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
    2. **Cohort dev-sequence gaps**: per ``(groups?, cohort)``, the
       observed integer dev sequence is checked for non-consecutive
       values in ``[dev_min, dev_max]``. When ``dev`` is supplied it is
       used directly; otherwise it is derived from
       ``cohort + calendar + grain`` (mirrors the
       :class:`Triangle` 3-mode dispatch).

    Mirrors the R sibling's ``validate_triangle(...)`` which returns a
    ``TriangleValidation`` object.

    Parameters
    ----------
    df
        Raw experience DataFrame (polars or pandas).
    groups, cohort, calendar, dev
        Column names. ``cohort`` is required; at least one of
        ``calendar`` / ``dev`` is required.
    loss, premium
        Optional column names. Not used by validation itself but
        accepted for signature symmetry with :class:`Triangle` (so the
        same kwargs work in both calls).
    grain
        Grain for cohort/calendar binning. Default ``"auto"``.

    Attributes
    ----------
    gaps : DataFrame
        One row per ``(groups?, cohort)`` with a non-consecutive dev
        sequence. Columns: ``[groups?, cohort, dev_min, dev_max,
        n_dev, n_expected, missing]``. Empty when no gaps were found.
    invalid_rows : DataFrame
        One row per input row that fails ``calendar >= cohort``.
        Columns: original ``(groups?, cohort, calendar, dev?)`` plus
        ``reason``. Empty when no row-level violations were found
        (or when no ``calendar`` was supplied).
    summary : DataFrame
        Two-row count summary: how many rows / cohorts failed each
        check.
    """

    def __init__(
        self,
        df: pl.DataFrame | Any,
        groups: str | Sequence[str] | None = None,
        cohort: str = "uy_m",
        calendar: str | None = "cy_m",
        dev: str | None = None,
        loss: str | None = None,
        premium: str | None = None,
        grain: str = "auto",
    ) -> None:
        self._output_type = detect_input_type(df)
        df_pl = to_polars(df)

        if calendar is None and dev is None:
            raise ValueError(
                "Must supply at least one of `calendar` or `dev`."
            )

        # `loss` / `premium` are accepted for kwarg-symmetry with
        # Triangle but not consumed by validation. Reference them in a
        # no-op so type checkers don't flag unused params.
        del loss, premium

        required = {cohort}
        required.update(normalize_groups(groups))
        if calendar is not None:
            required.add(calendar)
        if dev is not None:
            required.add(dev)
        missing = required - set(df_pl.columns)
        if missing:
            raise ValueError(
                f"Missing required columns: {sorted(missing)}. "
                f"Required: {sorted(required)}"
            )

        self._groups = collapse_groups(groups)
        self._cohort = cohort
        self._calendar = calendar
        self._dev_col = dev
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
                    [dev] if dev is not None else []
                ) if c in invalid.columns]
                invalid = invalid.select(keep_cols).with_columns(
                    pl.lit(f"{calendar} < {cohort}").alias("reason")
                )
            df_for_gaps = check_df.filter(~bad_mask)
        else:
            invalid = pl.DataFrame()
            df_for_gaps = df_pl

        self._invalid_rows = invalid

        # --- 2. dev-sequence gaps on the cleaned data --------------------
        # Derive dev when only calendar is supplied (mirrors Triangle's
        # 3-mode dispatch).
        if dev is None:
            input_grain = infer_grain(df_for_gaps[cohort])
            g = resolve_grain(input_grain, grain)
            df_for_gaps = df_for_gaps.with_columns(
                count_periods(pl.col(cohort), pl.col(calendar), g)
                .alias("_dev_derived")
            )
            dev_col_name = "_dev_derived"
        else:
            df_for_gaps = df_for_gaps.with_columns(
                pl.col(dev).cast(pl.Int64).alias("_dev_explicit")
            )
            dev_col_name = "_dev_explicit"

        gap_keys: list[str] = normalize_groups(groups)
        gap_keys.append(cohort)

        # Per (groups?, cohort): dev_min / dev_max / n_dev (distinct) /
        # n_expected (range length) / missing (list of integers).
        gap_summary = (
            df_for_gaps.filter(pl.col(dev_col_name).is_not_null())
            .group_by(gap_keys)
            .agg(
                pl.col(dev_col_name).min().cast(pl.Int64).alias("dev_min"),
                pl.col(dev_col_name).max().cast(pl.Int64).alias("dev_max"),
                pl.col(dev_col_name).n_unique().cast(pl.Int64).alias("n_dev"),
            )
            .with_columns(
                (pl.col("dev_max") - pl.col("dev_min") + 1)
                .cast(pl.Int64)
                .alias("n_expected"),
            )
        )

        gaps = gap_summary.filter(pl.col("n_dev") != pl.col("n_expected"))

        # Per-cohort missing dev values (set difference inside polars
        # is awkward; do it in Python for the gap rows only).
        if gaps.height:
            observed_devs = (
                df_for_gaps.filter(pl.col(dev_col_name).is_not_null())
                .group_by(gap_keys)
                .agg(pl.col(dev_col_name).unique().alias("_obs"))
            )
            gaps = gaps.join(observed_devs, on=gap_keys, how="left")
            missing_lists: list[list[int]] = []
            for row in gaps.iter_rows(named=True):
                obs = set(int(v) for v in row["_obs"])
                rng = range(int(row["dev_min"]), int(row["dev_max"]) + 1)
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
                    "non-consecutive dev (cohorts)",
                ],
                "n": [self._invalid_rows.height, self._gaps.height],
            }
        )

    # ------------------------------------------------------------------
    # Public accessors -- mirror Triangle's input-type machinery
    # ------------------------------------------------------------------

    @property
    def gaps(self):
        """DataFrame of cohorts with non-consecutive dev sequences."""
        return mirror_output(self._gaps, self._output_type)

    @property
    def invalid_rows(self):
        """DataFrame of rows where ``calendar < cohort``."""
        return mirror_output(self._invalid_rows, self._output_type)

    @property
    def summary(self):
        """Two-row count summary of findings."""
        return mirror_output(self._summary, self._output_type)

    @property
    def is_clean(self) -> bool:
        """``True`` iff both checks found zero violations."""
        return self._gaps.height == 0 and self._invalid_rows.height == 0

    @property
    def groups(self) -> str | list[str] | None:
        return self._groups

    @property
    def cohort(self) -> str:
        return self._cohort

    @property
    def calendar(self) -> str | None:
        return self._calendar

    @property
    def dev(self) -> str | None:
        return self._dev_col

    def to_polars(self) -> pl.DataFrame:
        """The gaps table as a polars DataFrame."""
        return self._gaps

    def to_pandas(self):
        """The gaps table as a pandas DataFrame."""
        return self._gaps.to_pandas()

    def plot(
        self,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Bar chart of observed vs expected dev counts per cohort.

        Mirrors R's ``plot.TriangleValidation`` -- one panel per
        group, with two bars per cohort (``n_dev`` blue, ``n_expected``
        grey). When the validation found no gaps, returns a
        figure containing only a "nothing to visualise" placeholder.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from ._validation_vis import plot_validation
        return plot_validation(self, nrow=nrow, ncol=ncol, figsize=figsize)

    def plot_triangle(
        self,
        x_axis: str = "dev",
        show_label: bool = False,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Cohort x dev / cohort x calendar heatmap of observed /
        missing cells.

        Mirrors R's ``plot_triangle.TriangleValidation``. For each
        cohort with gaps, the cell grid is coloured blue (observed)
        or red (missing); clean cohorts are omitted (R divergence:
        R uses a stored ``observed_pairs`` slot to paint every
        cohort -- Python carries only the gaps frame, so non-gappy
        cohorts don't appear).

        Parameters
        ----------
        x_axis
            ``"dev"`` (default) or ``"calendar"`` -- the same layout
            selector as :meth:`Triangle.plot_triangle`. The calendar
            layout synthesises per-cell calendar values via
            ``cohort + (dev - 1) * grain_step`` and requires the
            cohort series to have an inferable grain. Raises if no
            ``calendar`` column was supplied to
            :class:`TriangleValidation`.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from ._validation_vis import plot_triangle_validation
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
        """Number of cohorts with dev-sequence gaps."""
        return self._gaps.height
