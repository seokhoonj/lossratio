"""Link table — per (cohort, adjacent dev pair) intermediate.

Built via :meth:`Triangle.link`. Carries per-cell age-to-age
factors and (when an ``exposure`` column is supplied) the
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


_VALID_TARGETS = ("loss", "prem", "lr")


def _build_link_df(
    tri_df: pl.DataFrame,
    groups: str | None,
    target: str,
    exposure: str | None,
    weight: str | None,
    min_denom: float = 0.0,
    drop_invalid: bool = False,
) -> pl.DataFrame:
    """Build the long-format link table from a Triangle DataFrame.

    Generic worker: ``target`` / ``exposure`` / ``weight`` are *role*
    selectors that pull the cumulative column of the same name from
    the source triangle and emit role-prefixed columns
    ``target_from`` / ``exposure_from`` / ``weight``.
    """
    sort_keys: list[str] = []
    if groups is not None:
        sort_keys.append(groups)
    sort_keys.extend(["cohort", "dev"])
    df = tri_df.sort(sort_keys)

    over_keys: list[str] = []
    if groups is not None:
        over_keys.append(groups)
    over_keys.append("cohort")

    base_cols: list[pl.Expr] = []
    if groups is not None:
        base_cols.append(pl.col(groups))
    base_cols.extend(
        [
            pl.col("cohort"),
            pl.col("dev").alias("ata_from"),
            pl.col("dev").shift(-1).over(over_keys).alias("ata_to"),
            pl.col(target).alias("target_from"),
            pl.col(target).shift(-1).over(over_keys).alias("target_to"),
        ]
    )
    if exposure is not None:
        base_cols.extend(
            [
                pl.col(exposure).alias("exposure_from"),
                pl.col(exposure).shift(-1).over(over_keys).alias("exposure_to"),
            ]
        )
    if weight is not None:
        base_cols.append(pl.col(weight).alias("weight"))

    out = df.select(base_cols).filter(pl.col("ata_to").is_not_null())

    out = out.with_columns(
        [
            pl.format(
                "{}-{}", pl.col("ata_from"), pl.col("ata_to")
            ).alias("ata_link"),
            (pl.col("target_to") - pl.col("target_from")).alias("target_delta"),
            pl.when(pl.col("target_from") > min_denom)
            .then(pl.col("target_to") / pl.col("target_from"))
            .otherwise(None)
            .alias("ata"),
        ]
    )

    if exposure is not None:
        out = out.with_columns(
            [
                (pl.col("exposure_to") - pl.col("exposure_from")).alias(
                    "exposure_delta"
                ),
                pl.when(pl.col("exposure_from") > min_denom)
                .then(
                    (pl.col("target_to") - pl.col("target_from"))
                    / pl.col("exposure_from")
                )
                .otherwise(None)
                .alias("intensity"),
            ]
        )

    if drop_invalid:
        if exposure is not None:
            out = out.filter(pl.col("intensity").is_finite())
        else:
            out = out.filter(pl.col("ata").is_finite())

    col_order: list[str] = []
    if groups is not None:
        col_order.append(groups)
    col_order.extend(
        [
            "cohort",
            "ata_from",
            "ata_to",
            "ata_link",
            "target_from",
            "target_to",
            "target_delta",
            "ata",
        ]
    )
    if exposure is not None:
        col_order.extend(
            ["exposure_from", "exposure_to", "exposure_delta", "intensity"]
        )
    if weight is not None:
        col_order.append("weight")

    return out.select(col_order)


def _validate_target_arg(name: str, value: str) -> str:
    if value not in _VALID_TARGETS:
        raise ValueError(
            f"`{name}` must be one of {_VALID_TARGETS}, got {value!r}"
        )
    return value


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

        - Always:
          ``[groups?, cohort, ata_from, ata_to, ata_link,
          target_from, target_to, target_delta, ata]``.
        - When ``exposure`` is set:
          ``[exposure_from, exposure_to, exposure_delta, intensity]``.
        - When ``weight`` is set: ``weight``.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> link = tri.link()                              # target='loss', exposure='prem'
    >>> link = tri.link(target="loss")                 # ATA-only
    >>> link = tri.link(target="loss", exposure="prem")
    >>> link.ata()         # ATA factor diagnostic
    >>> link.intensity()   # ED intensity diagnostic
    >>> link.df            # raw long-format link table
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._tri_df: pl.DataFrame
        self._output_type: str
        self._groups: str | None
        self._cohort: str
        self._dev: str
        self._target: str
        self._exposure: str | None
        self._weight: str | None

    @classmethod
    def _from_triangle(
        cls,
        triangle: "Triangle",
        target: str = "loss",
        exposure: str | None = "prem",
        weight: str | None = None,
        min_denom: float = 0.0,
        drop_invalid: bool = False,
    ) -> "Link":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self._cohort = triangle._cohort
        self._dev = triangle._dev

        tri_df = triangle._df

        target = _validate_target_arg("target", target)
        if target not in tri_df.columns:
            raise ValueError(
                f"`target={target!r}` column missing from Triangle. "
                f"Available columns: {tri_df.columns}"
            )

        if exposure is not None:
            exposure = _validate_target_arg("exposure", exposure)
            if exposure not in tri_df.columns:
                # Auto-disable exposure when it's not present (e.g. a
                # Triangle without premium). This keeps ATA-only path working.
                exposure = None

        if weight is not None:
            weight = _validate_target_arg("weight", weight)
            if exposure is not None:
                raise ValueError(
                    "`weight` cannot be combined with `exposure`. "
                    "Dual mode uses `exposure_from` as its anchor."
                )
            if weight == target:
                raise ValueError("`weight` must differ from `target`.")
            if weight not in tri_df.columns:
                raise ValueError(
                    f"`weight={weight!r}` column missing from Triangle."
                )

        self._target = target
        self._exposure = exposure
        self._weight = weight
        self._tri_df = tri_df

        self._df = _build_link_df(
            tri_df,
            self._groups,
            target=target,
            exposure=exposure,
            weight=weight,
            min_denom=min_denom,
            drop_invalid=drop_invalid,
        )
        return self

    @property
    def df(self):
        """Long-format link table in the original input format."""
        return mirror_output(self._df, self._output_type)

    @property
    def target(self) -> str:
        """Triangle column used as the link numerator (e.g. ``"loss"``)."""
        return self._target

    @property
    def exposure(self) -> str | None:
        """Triangle column used as the ED exposure anchor (or ``None``)."""
        return self._exposure

    @property
    def weight(self) -> str | None:
        """Optional WLS weight column name (or ``None``)."""
        return self._weight

    @property
    def _has_prem(self) -> bool:
        """Backward-compat alias for dual-mode detection."""
        return self._exposure is not None

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

        Requires the Link to be in dual mode (``exposure`` set).
        Aggregates the per-cell ``intensity`` column via WLS,
        returning per-link g, g_se, sigma2, n_obs.

        Parameters
        ----------
        sigma_method
            Tail-sigma extrapolation method when the last link has
            only one contributing cohort. One of ``"locf"`` (default,
            most conservative), ``"min_last2"``, ``"loglinear"``.
        """
        if self._exposure is None:
            raise ValueError(
                "Link.intensity() requires the Link to be built with "
                "`exposure` set (e.g. exposure='prem')."
            )
        from .intensity import Intensity

        return Intensity._from_link(self, sigma_method=sigma_method)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def __repr__(self) -> str:
        mode = "dual-mode" if self._exposure is not None else "ATA-only"
        n_links = self._df.height
        if self._groups is None:
            return f"<Link: {n_links} links, {mode}>"
        n_groups = self._df[self._groups].n_unique()
        return (
            f"<Link: {n_groups} groups, {n_links} total links, {mode}>"
        )
