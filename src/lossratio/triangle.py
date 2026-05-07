"""Triangle: cohort x dev aggregated experience data."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import detect_input_type, mirror_output, to_polars

if TYPE_CHECKING:
    from .experience import Experience
    from .maturity import Maturity


_DEV_UNITS = {"month", "quarter", "half", "year"}


def _compute_dev(
    df: pl.DataFrame,
    cohort_var: str,
    cym_var: str,
    dev_unit: str,
) -> pl.DataFrame:
    """Add a numeric dev column derived from cohort_var and cym_var."""
    if dev_unit == "month":
        elap = (
            (pl.col(cym_var).dt.year() - pl.col(cohort_var).dt.year()) * 12
            + (pl.col(cym_var).dt.month() - pl.col(cohort_var).dt.month())
            + 1
        )
    elif dev_unit == "quarter":
        elap = (
            (pl.col(cym_var).dt.year() - pl.col(cohort_var).dt.year()) * 4
            + ((pl.col(cym_var).dt.month() - 1) // 3
               - (pl.col(cohort_var).dt.month() - 1) // 3)
            + 1
        )
    elif dev_unit == "half":
        elap = (
            (pl.col(cym_var).dt.year() - pl.col(cohort_var).dt.year()) * 2
            + ((pl.col(cym_var).dt.month() - 1) // 6
               - (pl.col(cohort_var).dt.month() - 1) // 6)
            + 1
        )
    elif dev_unit == "year":
        elap = pl.col(cym_var).dt.year() - pl.col(cohort_var).dt.year() + 1
    else:
        raise ValueError(
            f"dev_unit must be one of {sorted(_DEV_UNITS)}, got {dev_unit!r}"
        )

    return df.with_columns(elap.cast(pl.Int64).alias("_dev_temp"))


class Triangle:
    """Cohort x development period aggregated experience data.

    A Triangle is built by aggregating an :class:`Experience` (or a
    raw DataFrame with the same schema) over ``group_var`` (optional),
    cohort, and development period. The resulting frame has columns:

    * ``group_var`` -- present only if supplied
    * ``cohort`` -- the underwriting period (renamed from cohort_var)
    * ``dev`` -- the development index (1, 2, ...) within each cohort
    * ``loss``, ``rp`` -- incremental sums per cell
    * ``closs``, ``crp`` -- cumulative sums within each (group, cohort)
    * ``lr`` -- incremental loss ratio (loss / rp)
    * ``clr`` -- cumulative loss ratio (closs / crp)

    Original column names (e.g. ``"uym"`` for cohort_var) are kept
    as instance attributes for downstream plotting.
    """

    def __init__(
        self,
        source: "Experience | pl.DataFrame | Any",
        group_var: str | None = None,
        cohort_var: str = "uym",
        dev_unit: str = "month",
        cym_var: str = "cym",
    ) -> None:
        if dev_unit not in _DEV_UNITS:
            raise ValueError(
                f"dev_unit must be one of {sorted(_DEV_UNITS)}, got {dev_unit!r}"
            )

        # Resolve source: Experience instance or DataFrame
        # (avoid circular import by checking class name and module path)
        src_cls = type(source)
        is_experience = (
            src_cls.__module__.endswith("lossratio.experience")
            and src_cls.__name__ == "Experience"
        )

        if is_experience:
            self._output_type = source._output_type
            df_pl = source._df
        else:
            self._output_type = detect_input_type(source)
            df_pl = to_polars(source)

        # Validate required columns
        required = {cohort_var, cym_var, "loss", "rp"}
        if group_var is not None:
            required.add(group_var)
        missing = required - set(df_pl.columns)
        if missing:
            raise ValueError(
                f"Missing required columns: {sorted(missing)}. "
                f"Required: {sorted(required)}"
            )

        # Coerce types when source was raw DataFrame
        # (Experience already coerced these in its own __init__)
        if not is_experience:
            df_pl = df_pl.with_columns(
                pl.col(cohort_var).cast(pl.Date),
                pl.col(cym_var).cast(pl.Date),
                pl.col("loss").cast(pl.Float64),
                pl.col("rp").cast(pl.Float64),
            )

        # Compute dev index (1, 2, ...) per cohort
        df_pl = _compute_dev(df_pl, cohort_var, cym_var, dev_unit)

        # Aggregate: sum(loss), sum(rp) by (group_var, cohort, dev)
        agg_keys: list[str] = []
        if group_var is not None:
            agg_keys.append(group_var)
        agg_keys.extend([cohort_var, "_dev_temp"])

        agg = (
            df_pl.group_by(agg_keys)
            .agg(
                pl.col("loss").sum(),
                pl.col("rp").sum(),
            )
            .sort(agg_keys)
        )

        # Cumulative sums within (group, cohort)
        cum_keys: list[str] = []
        if group_var is not None:
            cum_keys.append(group_var)
        cum_keys.append(cohort_var)

        agg = agg.with_columns(
            pl.col("loss").cum_sum().over(cum_keys).alias("closs"),
            pl.col("rp").cum_sum().over(cum_keys).alias("crp"),
        ).with_columns(
            (pl.col("loss") / pl.col("rp")).alias("lr"),
            (pl.col("closs") / pl.col("crp")).alias("clr"),
        )

        # Rename to standard column names: cohort_var -> cohort, _dev_temp -> dev
        agg = agg.rename({cohort_var: "cohort", "_dev_temp": "dev"})

        # Reorder columns for readability
        ordered = []
        if group_var is not None:
            ordered.append(group_var)
        ordered.extend(["cohort", "dev", "loss", "rp", "closs", "crp", "lr", "clr"])
        agg = agg.select(ordered)

        self._df = agg
        self._group_var = group_var
        self._cohort_var = cohort_var
        self._dev_var = "elap_m" if dev_unit == "month" else f"elap_{dev_unit[0]}"
        self._dev_unit = dev_unit

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
        """Original cohort variable name (e.g. 'uym')."""
        return self._cohort_var

    @property
    def dev_var(self) -> str:
        """Development variable name (e.g. 'elap_m')."""
        return self._dev_var

    @property
    def dev_unit(self) -> str:
        """Development unit ('month', 'quarter', 'half', 'year')."""
        return self._dev_unit

    def maturity(
        self,
        theta_cv: float = 0.1,
        theta_rse: float = 0.05,
        m: int = 2,
    ) -> "Maturity":
        """Detect the ATA maturity point ``k*``.

        Returns a ``Maturity`` result with per-link diagnostics
        (CV, RSE, stable flag) and the detected k_star (the first dev
        at which factors are stable for ``m`` consecutive links).

        Parameters
        ----------
        theta_cv
            Threshold on the cross-cohort coefficient of variation of
            individual link factors.
        theta_rse
            Threshold on the relative standard error of the pooled f_k.
        m
            Required number of consecutive stable links.
        """
        from .maturity import Maturity

        return Maturity._from_triangle(
            self, theta_cv=theta_cv, theta_rse=theta_rse, m=m
        )

    def __repr__(self) -> str:
        bits = [f"{self._df.height:,} rows"]
        if self._group_var is not None:
            n_groups = self._df[self._group_var].n_unique()
            bits.append(f"{n_groups} groups")
        n_cohorts = self._df["cohort"].n_unique()
        n_devs = self._df["dev"].n_unique()
        bits.append(f"{n_cohorts} cohorts x {n_devs} devs")
        return f"<Triangle: {', '.join(bits)}>"

    def __len__(self) -> int:
        return self._df.height
