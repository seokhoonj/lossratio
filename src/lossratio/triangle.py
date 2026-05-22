"""Triangle: cohort x dev aggregated experience data."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl

from ._period import (
    coerce_cols_to_date,
    count_periods,
    floor_cols_to_period,
    infer_grain,
    resolve_grain,
)
from ._io import detect_input_type, mirror_output, to_polars

if TYPE_CHECKING:
    from .link import Link
    from .regime import Regime


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
        df: "pl.DataFrame | Any",
        groups: str | None = None,
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
        if groups is not None:
            required.add(groups)
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
            sort_keys: list[str] = []
            if groups is not None:
                sort_keys.append(groups)
            sort_keys.extend([cohort, sort_axis])  # type: ignore[list-item]
            diff_over: list[str] = []
            if groups is not None:
                diff_over.append(groups)
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
        agg_keys: list[str] = []
        if groups is not None:
            agg_keys.append(groups)
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
        ndev_keys: list[str] = []
        if groups is not None:
            ndev_keys.append(groups)
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
                    f"inspect with validate_experience()."
                )

        # Join n_cohorts, then rename cohort / dev to standard names so
        # subsequent `over` / `sum().over` expressions read the canonical
        # columns.
        agg = agg.join(ncoh, on=ndev_keys, how="left")
        agg = agg.rename({cohort: "cohort", "_dev_temp": "dev"})

        cum_keys: list[str] = []
        if groups is not None:
            cum_keys.append(groups)
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
        ordered = []
        if groups is not None:
            ordered.append(groups)
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
        self._groups = groups
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
    def groups(self) -> str | None:
        """Original group variable name (or None if no grouping)."""
        return self._groups

    @property
    def cohort(self) -> str:
        """Original cohort variable name (e.g. 'uy_m')."""
        return self._cohort

    @property
    def calendar(self) -> str | None:
        """Original calendar variable name (e.g. 'cy_m').

        ``None`` for a mode-2 (dev-only) triangle: no calendar column
        was supplied, so there is no raw name to retain.
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
    def _from_masked(cls, original: "Triangle", masked_df: pl.DataFrame) -> "Triangle":
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

    def link(
        self,
        target: str = "loss",
        exposure: str | None = "premium",
        weight: str | None = None,
        min_denom: float = 0.0,
        drop_invalid: bool = False,
    ) -> "Link":
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
        window: int = 12,
        method: str = "e_divisive",
        n_regimes: int | None = None,
        sig_level: float = 0.05,
        R: int = 999,
        min_size: int = 3,
        seed: int | None = None,
    ) -> "Regime":
        """Detect structural regime shifts across underwriting cohorts.

        See :class:`Regime` for full description.
        """
        from .regime import Regime

        return Regime._from_triangle(
            self,
            target=target,
            window=window,
            method=method,
            n_regimes=n_regimes,
            sig_level=sig_level,
            R=R,
            min_size=min_size,
            seed=seed,
        )

    def __repr__(self) -> str:
        bits = [f"{self._df.height:,} rows"]
        if self._groups is not None:
            n_groups = self._df[self._groups].n_unique()
            bits.append(f"{n_groups} groups")
        n_cohorts = self._df["cohort"].n_unique()
        n_devs = self._df["dev"].n_unique()
        bits.append(f"{n_cohorts} cohorts x {n_devs} devs ({self._grain})")
        return f"<Triangle: {', '.join(bits)}>"

    def __len__(self) -> int:
        return self._df.height
