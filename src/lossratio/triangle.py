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


# Required value columns (cohort/cym are checked separately by name).
_REQUIRED_VALUE_COLS = ("loss_incr", "premium_incr")

# Triangle-specific: standard dev column name per grain code.
_GRAIN_TO_DEV_VAR = {
    "M": "dev_m",
    "Q": "dev_q",
    "S": "dev_s",
    "A": "dev_a",
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
      auto-detected from cohort date spacing. Override via
      ``granularity`` arg to view at a coarser granularity (e.g.,
      monthly data viewed quarterly).
    - Requested granularity must be at least as coarse as input
      (no decomposition: yearly input cannot be viewed monthly).

    The resulting frame has columns:

    * ``groups`` -- present only if supplied
    * ``cohort`` -- the underwriting period (renamed from cohort)
    * ``dev`` -- the development index (1, 2, ...) within each cohort
    * ``loss``, ``premium`` -- cumulative sums within each (group, cohort)
    * ``loss_incr``, ``premium_incr`` -- per-period sums per cell
    * ``lr`` -- cumulative loss ratio (``loss / premium``)
    * ``lr_incr`` -- per-period loss ratio (``loss_incr / premium_incr``)

    Cumulative is the unmarked default; per-period values carry an
    ``_incr`` (incremental) suffix.

    Original column names (e.g. ``"uy_m"`` for cohort) are kept
    as instance attributes for downstream plotting.
    """

    def __init__(
        self,
        df: "pl.DataFrame | Any",
        groups: str | None = None,
        cohort: str = "uy_m",
        calendar: str = "cy_m",
        grain: str = "auto",
        fill_gaps: bool = False,
    ) -> None:
        self._output_type = detect_input_type(df)
        df_pl = to_polars(df)

        # Required: cohort, calendar, value cols, and groups if set.
        required = {cohort, calendar, *_REQUIRED_VALUE_COLS}
        if groups is not None:
            required.add(groups)
        missing = required - set(df_pl.columns)
        if missing:
            raise ValueError(
                f"Missing required columns: {sorted(missing)}. "
                f"Required: {sorted(required)}"
            )

        # Coerce cohort, calendar to Date (Date/Datetime/Int/String).
        df_pl = coerce_cols_to_date(df_pl, [cohort, calendar])
        df_pl = df_pl.with_columns(
            pl.col("loss_incr").cast(pl.Float64),
            pl.col("premium_incr").cast(pl.Float64),
        )

        # Auto-detect input grain; resolve "auto" or validate explicit value.
        input_grain = infer_grain(df_pl[cohort])
        grain = resolve_grain(input_grain, grain)

        # Bin to requested grain (no-op if grain == input).
        if grain != input_grain:
            df_pl = floor_cols_to_period(
                df_pl, [cohort, calendar], grain
            )

        # Compute integer dev (1, 2, ...) at requested grain.
        df_pl = df_pl.with_columns(
            count_periods(pl.col(cohort), pl.col(calendar), grain)
            .alias("_dev_temp")
        )

        # Aggregate per-period values by (groups, cohort, dev).
        agg_keys: list[str] = []
        if groups is not None:
            agg_keys.append(groups)
        agg_keys.extend([cohort, "_dev_temp"])

        agg = (
            df_pl.group_by(agg_keys)
            .agg(
                pl.col("loss_incr").sum(),
                pl.col("premium_incr").sum(),
            )
            .sort(agg_keys)
        )

        # Check for non-consecutive dev sequences within each cohort.
        # R parity: build_triangle(fill_gaps = FALSE) raises; TRUE zero-fills.
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
                        pl.col("loss_incr").fill_null(0.0),
                        pl.col("premium_incr").fill_null(0.0),
                    )
                    .sort(agg_keys)
                )
            else:
                raise ValueError(
                    f"Found {gaps.height} cohort(s) with non-consecutive "
                    f"dev sequences. Pass fill_gaps=True to zero-fill, or "
                    f"inspect with validate_experience()."
                )

        # Cumulative sums within (group, cohort) — cumulative is default name.
        cum_keys: list[str] = []
        if groups is not None:
            cum_keys.append(groups)
        cum_keys.append(cohort)

        agg = agg.with_columns(
            pl.col("loss_incr").cum_sum().over(cum_keys).alias("loss"),
            pl.col("premium_incr").cum_sum().over(cum_keys).alias("premium"),
        ).with_columns(
            (pl.col("loss") / pl.col("premium")).alias("lr"),
            (pl.col("loss_incr") / pl.col("premium_incr")).alias("lr_incr"),
        )

        # Rename to standard column names: cohort -> cohort, _dev_temp -> dev.
        agg = agg.rename({cohort: "cohort", "_dev_temp": "dev"})

        # Reorder columns: cum-first paired.
        ordered = []
        if groups is not None:
            ordered.append(groups)
        ordered.extend([
            "cohort", "dev",
            "loss", "loss_incr",
            "premium", "premium_incr",
            "lr", "lr_incr",
        ])
        agg = agg.select(ordered)

        self._df = agg
        self._groups = groups
        self._cohort = cohort
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
    def dev(self) -> str:
        """Standard dev column name for the grain (e.g. 'dev_m')."""
        return self._dev

    @property
    def grain(self) -> str:
        """Triangle grain code: ``"M"`` / ``"Q"`` / ``"S"`` / ``"A"``.

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
            ``"loss"``, ``"premium"``, ``"lr"``. Default ``"loss"``.
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
        target: str = "lr",
        K: int = 12,
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
            K=K,
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
