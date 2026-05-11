"""Link table — per (cohort, adjacent dev pair) intermediate.

Built via :meth:`Triangle.link`. Carries per-cell age-to-age
factors and (when the source Triangle has a premium column) the
per-cell exposure-driven intensity. Mirrors R's ``Link`` data
class.

Diagnostic methods :meth:`ata` and :meth:`intensity` aggregate
across cohorts to per-link summaries that pair with each other:

* :meth:`Link.ata` — multiplicative ATA factor diagnostic
* :meth:`Link.intensity` — additive ED intensity diagnostic
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import mirror_output

if TYPE_CHECKING:
    from .ata import ATA
    from .intensity import Intensity
    from .triangle import Triangle


def _build_link_df(
    tri_df: pl.DataFrame,
    group_var: str | None,
    has_premium: bool,
) -> pl.DataFrame:
    """Build the long-format link table from a Triangle DataFrame."""
    sort_keys: list[str] = []
    if group_var is not None:
        sort_keys.append(group_var)
    sort_keys.extend(["cohort", "dev"])
    df = tri_df.sort(sort_keys)

    over_keys: list[str] = []
    if group_var is not None:
        over_keys.append(group_var)
    over_keys.append("cohort")

    base_cols: list[pl.Expr] = []
    if group_var is not None:
        base_cols.append(pl.col(group_var))
    base_cols.extend(
        [
            pl.col("cohort"),
            pl.col("dev").alias("ata_from"),
            pl.col("dev").shift(-1).over(over_keys).alias("ata_to"),
            pl.col("loss").alias("loss_from"),
            pl.col("loss").shift(-1).over(over_keys).alias("loss_to"),
        ]
    )
    if has_premium:
        base_cols.extend(
            [
                pl.col("premium").alias("premium_from"),
                pl.col("premium").shift(-1).over(over_keys).alias("premium_to"),
            ]
        )

    out = df.select(base_cols).filter(pl.col("ata_to").is_not_null())

    out = out.with_columns(
        [
            pl.format(
                "{}-{}", pl.col("ata_from"), pl.col("ata_to")
            ).alias("ata_link"),
            (pl.col("loss_to") - pl.col("loss_from")).alias("loss_delta"),
            pl.when(pl.col("loss_from") > 0)
            .then(pl.col("loss_to") / pl.col("loss_from"))
            .otherwise(None)
            .alias("ata"),
        ]
    )

    if has_premium:
        out = out.with_columns(
            [
                (pl.col("premium_to") - pl.col("premium_from")).alias(
                    "premium_delta"
                ),
                pl.when(pl.col("premium_from") > 0)
                .then(
                    (pl.col("loss_to") - pl.col("loss_from"))
                    / pl.col("premium_from")
                )
                .otherwise(None)
                .alias("intensity"),
            ]
        )

    col_order: list[str] = []
    if group_var is not None:
        col_order.append(group_var)
    col_order.extend(
        [
            "cohort",
            "ata_from",
            "ata_to",
            "ata_link",
            "loss_from",
            "loss_to",
            "loss_delta",
            "ata",
        ]
    )
    if has_premium:
        col_order.extend(
            ["premium_from", "premium_to", "premium_delta", "intensity"]
        )

    return out.select(col_order)


class Link:
    """Long-format link table — one row per (cohort, adjacent dev pair).

    Built via :meth:`Triangle.link`. Stores the per-cell ATA factor
    and, in dual mode, the per-cell ED intensity. Methods
    :meth:`ata` and :meth:`intensity` aggregate to per-link
    summaries.

    Properties
    ----------
    df : DataFrame
        Long-format link table:

        - ATA columns (always):
          ``[group_var?, cohort, ata_from, ata_to, ata_link,
          loss_from, loss_to, loss_delta, ata]``.
        - ED columns (added when the source Triangle has a
          ``premium`` column):
          ``[premium_from, premium_to, premium_delta, intensity]``.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, group_var="coverage")
    >>> link = tri.link()
    >>> link.ata()         # ATA factor diagnostic
    >>> link.intensity()   # ED intensity diagnostic
    >>> link.df            # raw long-format link table
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._tri_df: pl.DataFrame
        self._output_type: str
        self._group_var: str | None
        self._cohort_var: str
        self._dev_var: str
        self._has_premium: bool

    @classmethod
    def _from_triangle(cls, triangle: "Triangle") -> "Link":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._group_var = triangle._group_var
        self._cohort_var = triangle._cohort_var
        self._dev_var = triangle._dev_var

        tri_df = triangle._df
        self._tri_df = tri_df
        self._has_premium = "premium" in tri_df.columns

        self._df = _build_link_df(tri_df, self._group_var, self._has_premium)
        return self

    @property
    def df(self):
        """Long-format link table in the original input format."""
        return mirror_output(self._df, self._output_type)

    def ata(self, sigma_method: str = "locf") -> "ATA":
        """ATA factor diagnostic on this link.

        Aggregates the per-cell ``ata`` column across cohorts via
        Mack pooling, returning per-link f, sigma2, cv, rse, n_obs.

        Parameters
        ----------
        sigma_method
            Tail-sigma extrapolation method when the last link has only
            one contributing cohort. One of ``"locf"`` (default, most
            conservative), ``"min_last2"``, ``"loglinear"``.
        """
        from .ata import ATA

        return ATA._from_link(self, sigma_method=sigma_method)

    def intensity(self, sigma_method: str = "locf") -> "Intensity":
        """ED intensity diagnostic on this link.

        Requires the Link to be in dual mode (source Triangle has a
        ``premium`` column). Aggregates the per-cell ``intensity``
        column via WLS, returning per-link g, g_se, sigma2, n_obs.

        Parameters
        ----------
        sigma_method
            Tail-sigma extrapolation method when the last link has
            only one contributing cohort. One of ``"locf"`` (default,
            most conservative), ``"min_last2"``, ``"loglinear"``.
        """
        if not self._has_premium:
            raise ValueError(
                "Link.intensity() requires the source Triangle to "
                "carry a `premium` column. The current Link is "
                "ATA-only — rebuild the Triangle from a DataFrame "
                "that includes `premium_incr`."
            )
        from .intensity import Intensity

        return Intensity._from_link(self, sigma_method=sigma_method)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def __repr__(self) -> str:
        mode = "dual-mode" if self._has_premium else "ATA-only"
        n_links = self._df.height
        if self._group_var is None:
            return f"<Link: {n_links} links, {mode}>"
        n_groups = self._df[self._group_var].n_unique()
        return (
            f"<Link: {n_groups} groups, {n_links} total links, {mode}>"
        )
