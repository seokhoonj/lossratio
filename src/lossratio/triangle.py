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
    (polars or pandas) over ``group_var`` (optional), cohort, and
    development period.

    Input flexibility:

    - ``cohort_var`` / ``calendar_var`` columns can be ``Date``,
      ``Datetime``, integer (yyyy / yyyymm / yyyymmdd auto-detected),
      or ISO string ("YYYY-MM-DD", "YYYY/MM/DD", "YYYYMMDD",
      with optional " HH:MM:SS" time suffix).
    - Granularity ("month" / "quarter" / "half" / "year") is
      auto-detected from cohort_var date spacing. Override via
      ``granularity`` arg to view at a coarser granularity (e.g.,
      monthly data viewed quarterly).
    - Requested granularity must be at least as coarse as input
      (no decomposition: yearly input cannot be viewed monthly).

    The resulting frame has columns:

    * ``group_var`` -- present only if supplied
    * ``cohort`` -- the underwriting period (renamed from cohort_var)
    * ``dev`` -- the development index (1, 2, ...) within each cohort
    * ``loss``, ``premium`` -- cumulative sums within each (group, cohort)
    * ``loss_incr``, ``premium_incr`` -- per-period sums per cell
    * ``lr`` -- cumulative loss ratio (``loss / premium``)
    * ``lr_incr`` -- per-period loss ratio (``loss_incr / premium_incr``)

    Cumulative is the unmarked default; per-period values carry an
    ``_incr`` (incremental) suffix.

    Original column names (e.g. ``"uy_m"`` for cohort_var) are kept
    as instance attributes for downstream plotting.
    """

    def __init__(
        self,
        df: "pl.DataFrame | Any",
        group_var: str | None = None,
        cohort_var: str = "uy_m",
        calendar_var: str = "cy_m",
        grain: str = "auto",
    ) -> None:
        self._output_type = detect_input_type(df)
        df_pl = to_polars(df)

        # Required: cohort_var, calendar_var, value cols, and group_var if set.
        required = {cohort_var, calendar_var, *_REQUIRED_VALUE_COLS}
        if group_var is not None:
            required.add(group_var)
        missing = required - set(df_pl.columns)
        if missing:
            raise ValueError(
                f"Missing required columns: {sorted(missing)}. "
                f"Required: {sorted(required)}"
            )

        # Coerce cohort_var, calendar_var to Date (Date/Datetime/Int/String).
        df_pl = coerce_cols_to_date(df_pl, [cohort_var, calendar_var])
        df_pl = df_pl.with_columns(
            pl.col("loss_incr").cast(pl.Float64),
            pl.col("premium_incr").cast(pl.Float64),
        )

        # Auto-detect input grain; resolve "auto" or validate explicit value.
        input_grain = infer_grain(df_pl[cohort_var])
        grain = resolve_grain(input_grain, grain)

        # Bin to requested grain (no-op if grain == input).
        if grain != input_grain:
            df_pl = floor_cols_to_period(
                df_pl, [cohort_var, calendar_var], grain
            )

        # Compute integer dev (1, 2, ...) at requested grain.
        df_pl = df_pl.with_columns(
            count_periods(pl.col(cohort_var), pl.col(calendar_var), grain)
            .alias("_dev_temp")
        )

        # Aggregate per-period values by (group_var, cohort, dev).
        agg_keys: list[str] = []
        if group_var is not None:
            agg_keys.append(group_var)
        agg_keys.extend([cohort_var, "_dev_temp"])

        agg = (
            df_pl.group_by(agg_keys)
            .agg(
                pl.col("loss_incr").sum(),
                pl.col("premium_incr").sum(),
            )
            .sort(agg_keys)
        )

        # Cumulative sums within (group, cohort) — cumulative is default name.
        cum_keys: list[str] = []
        if group_var is not None:
            cum_keys.append(group_var)
        cum_keys.append(cohort_var)

        agg = agg.with_columns(
            pl.col("loss_incr").cum_sum().over(cum_keys).alias("loss"),
            pl.col("premium_incr").cum_sum().over(cum_keys).alias("premium"),
        ).with_columns(
            (pl.col("loss") / pl.col("premium")).alias("lr"),
            (pl.col("loss_incr") / pl.col("premium_incr")).alias("lr_incr"),
        )

        # Rename to standard column names: cohort_var -> cohort, _dev_temp -> dev.
        agg = agg.rename({cohort_var: "cohort", "_dev_temp": "dev"})

        # Reorder columns: cum-first paired.
        ordered = []
        if group_var is not None:
            ordered.append(group_var)
        ordered.extend([
            "cohort", "dev",
            "loss", "loss_incr",
            "premium", "premium_incr",
            "lr", "lr_incr",
        ])
        agg = agg.select(ordered)

        self._df = agg
        self._group_var = group_var
        self._cohort_var = cohort_var
        self._grain = grain
        self._dev_var = _GRAIN_TO_DEV_VAR[grain]

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
    def group_var(self) -> str | None:
        """Original group variable name (or None if no grouping)."""
        return self._group_var

    @property
    def cohort_var(self) -> str:
        """Original cohort variable name (e.g. 'uy_m')."""
        return self._cohort_var

    @property
    def dev_var(self) -> str:
        """Standard dev column name for the grain (e.g. 'dev_m')."""
        return self._dev_var

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
        tri._group_var = original._group_var
        tri._cohort_var = original._cohort_var
        tri._grain = original._grain
        tri._dev_var = original._dev_var
        return tri

    def link(self) -> "Link":
        """Build the long-format link table.

        Returns a :class:`Link` data class — one row per (cohort,
        adjacent dev pair) — that exposes the per-cell ATA factor
        and (when the source Triangle has a premium column) the
        per-cell ED intensity. Diagnostic methods :meth:`Link.ata`
        and :meth:`Link.intensity` aggregate across cohorts to
        per-link summaries.

        ``tri.link()`` is the single canonical entry point for
        factor-level diagnostics. The chains:

        - ``tri.link().ata()``                    → :class:`ATA`
        - ``tri.link().intensity()``              → :class:`Intensity`
        - ``tri.link().ata().maturity(...)``      → :class:`Maturity`

        Examples
        --------
        >>> tri = lr.Triangle(df, group_var="coverage")
        >>> link = tri.link()
        >>> link.ata()                            # multiplicative
        >>> link.intensity()                      # additive
        >>> link.ata().maturity(max_cv=0.15)      # stability detection
        """
        from .link import Link

        return Link._from_triangle(self)

    def detect_regime(
        self,
        loss_var: str = "lr",
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
            loss_var=loss_var,
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
        if self._group_var is not None:
            n_groups = self._df[self._group_var].n_unique()
            bits.append(f"{n_groups} groups")
        n_cohorts = self._df["cohort"].n_unique()
        n_devs = self._df["dev"].n_unique()
        bits.append(f"{n_cohorts} cohorts x {n_devs} devs ({self._grain})")
        return f"<Triangle: {', '.join(bits)}>"

    def __len__(self) -> int:
        return self._df.height
