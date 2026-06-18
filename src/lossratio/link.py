"""Link table — per (cohort, adjacent duration pair) intermediate.

Built via :meth:`Triangle.link`. Carries per-cell age-to-age
factors and (when an ``exposure`` column is supplied) the
per-cell additive intensity.

Diagnostic methods :meth:`ata` and :meth:`intensity` aggregate
across cohorts to per-link summaries that pair with each other:

* :meth:`Link.ata` — multiplicative ATA factor diagnostic
* :meth:`Link.intensity` — additive intensity diagnostic
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import mirror_output, normalize_groups

if TYPE_CHECKING:
    from .ata import ATA
    from .intensity import Intensity
    from .triangle import Triangle


_VALID_TARGETS = ("loss", "premium", "ratio")


def _build_link_df(
    tri_df: pl.DataFrame,
    groups: str | list[str] | None,
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
    ``loss_from`` / ``premium_from`` / ``weight``.
    """
    sort_keys: list[str] = []
    sort_keys.extend(normalize_groups(groups))
    sort_keys.extend(["cohort", "duration"])
    df = tri_df.sort(sort_keys)

    over_keys: list[str] = []
    over_keys.extend(normalize_groups(groups))
    over_keys.append("cohort")

    base_cols: list[pl.Expr] = [
        pl.col(c) for c in normalize_groups(groups)
    ]
    base_cols.extend(
        [
            pl.col("cohort"),
            pl.col("duration").alias("duration_from"),
            pl.col("duration").shift(-1).over(over_keys).alias("duration_to"),
            pl.col(target).alias("loss_from"),
            pl.col(target).shift(-1).over(over_keys).alias("loss_to"),
        ]
    )
    if exposure is not None:
        base_cols.extend(
            [
                pl.col(exposure).alias("premium_from"),
                pl.col(exposure).shift(-1).over(over_keys).alias("premium_to"),
            ]
        )
    if weight is not None:
        base_cols.append(pl.col(weight).alias("weight"))

    out = df.select(base_cols).filter(pl.col("duration_to").is_not_null())

    out = out.with_columns(
        [
            pl.format(
                "{}-{}", pl.col("duration_from"), pl.col("duration_to")
            ).alias("duration_link"),
            (pl.col("loss_to") - pl.col("loss_from")).alias("loss_delta"),
            pl.when(pl.col("loss_from") > min_denom)
            .then(pl.col("loss_to") / pl.col("loss_from"))
            .otherwise(None)
            .alias("ata"),
        ]
    )

    if exposure is not None:
        out = out.with_columns(
            [
                (pl.col("premium_to") - pl.col("premium_from")).alias(
                    "premium_delta"
                ),
                pl.when(pl.col("premium_from") > min_denom)
                .then(
                    (pl.col("loss_to") - pl.col("loss_from"))
                    / pl.col("premium_from")
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
    col_order.extend(normalize_groups(groups))
    col_order.extend(
        [
            "cohort",
            "duration_from",
            "duration_to",
            "duration_link",
            "loss_from",
            "loss_to",
            "loss_delta",
            "ata",
        ]
    )
    if exposure is not None:
        col_order.extend(
            ["premium_from", "premium_to", "premium_delta", "intensity"]
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
    """Long-format link table — one row per (cohort, adjacent duration pair).

    Built via :meth:`Triangle.link`. Stores the per-cell ATA factor
    and, in dual mode, the per-cell additive intensity. Methods
    :meth:`ata` and :meth:`intensity` aggregate to per-link
    summaries.

    Properties
    ----------
    df : DataFrame
        Long-format link table:

        - Always:
          ``[groups?, cohort, duration_from, duration_to, duration_link,
          loss_from, loss_to, loss_delta, ata]``.
        - When ``exposure`` is set:
          ``[premium_from, premium_to, premium_delta, intensity]``.
        - When ``weight`` is set: ``weight``.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> link = tri.link()                              # target='loss', exposure='premium'
    >>> link = tri.link(target="loss")                 # ATA-only
    >>> link = tri.link(target="loss", exposure="premium")
    >>> link.ata()         # ATA factor diagnostic
    >>> link.intensity()   # additive intensity diagnostic
    >>> link.df            # raw long-format link table
    """

    def __init__(self) -> None:
        raise TypeError(
            "Link is produced by `triangle.link(...)`, not a direct constructor."
        )

    @classmethod
    def _from_triangle(
        cls,
        triangle: "Triangle",
        target: str = "loss",
        exposure: str | None = "premium",
        weight: str | None = None,
        min_denom: float = 0.0,
        drop_invalid: bool = False,
    ) -> "Link":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self._cohort = triangle._cohort
        self._duration = triangle._duration

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
                if exposure != "premium":
                    raise ValueError(
                        f"`exposure={exposure!r}` column missing from "
                        f"Triangle. Available columns: {tri_df.columns}"
                    )
                # The default premium denominator is absent (e.g. an
                # ATA-only Triangle) -> fall back to the ATA-only path.
                exposure = None

        if weight is not None:
            weight = _validate_target_arg("weight", weight)
            if exposure is not None:
                raise ValueError(
                    "`weight` cannot be combined with `exposure`. "
                    "Dual mode uses `premium_from` as its anchor."
                )
            if weight == target:
                raise ValueError("`weight` must differ from `target`.")
            if weight not in tri_df.columns:
                raise ValueError(
                    f"`weight={weight!r}` column missing from Triangle."
                )

        self._target = target
        self._premium = exposure
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
    def premium(self) -> str | None:
        """Triangle column used as the exposure anchor (or ``None``)."""
        return self._premium

    @property
    def weight(self) -> str | None:
        """Optional WLS weight column name (or ``None``)."""
        return self._weight

    def ata(
        self,
        sigma_method: str = "locf",
        recent: int | None = None,
    ) -> "ATA":
        """ATA factor diagnostic on this link.

        Aggregates the per-cell ``ata`` column across cohorts via
        volume-weighted pooling, returning per-link f, sigma2, cv, rse,
        n_obs.

        Parameters
        ----------
        sigma_method
            Tail-sigma extrapolation method when the last link has only
            one contributing cohort. One of ``"locf"`` (default, most
            conservative), ``"min_last2"``, ``"loglinear"``.
        recent
            Optional positive integer. When supplied, only the
            most-recent ``recent`` calendar diagonals feed the per-link
            factor diagnostic (a calendar-diagonal wedge). ``None``
            (default) leaves the diagnostic byte-unchanged.
        """
        from .ata import ATA

        return ATA._from_link(self, sigma_method=sigma_method, recent=recent)

    def intensity(
        self,
        sigma_method: str = "locf",
        recent: int | None = None,
    ) -> "Intensity":
        """Additive intensity diagnostic on this link.

        Requires the Link to be in dual mode (``exposure`` set).
        Aggregates the per-cell ``intensity`` column via WLS,
        returning per-link g, g_se, sigma2, n_obs.

        Parameters
        ----------
        sigma_method
            Tail-sigma extrapolation method when the last link has
            only one contributing cohort. One of ``"locf"`` (default,
            most conservative), ``"min_last2"``, ``"loglinear"``.
        recent
            Optional positive integer. When supplied, only the
            most-recent ``recent`` calendar diagonals feed the per-link
            intensity diagnostic (a calendar-diagonal wedge). ``None``
            (default) leaves the diagnostic byte-unchanged.
        """
        if self._premium is None:
            raise ValueError(
                "Link.intensity() requires the Link to be built with "
                "`exposure` set (e.g. exposure='premium')."
            )
        from .intensity import Intensity

        return Intensity._from_link(
            self, sigma_method=sigma_method, recent=recent
        )

    def plot(
        self,
        model: str | None = None,
        **kwargs: Any,
    ) -> Any:
        """Link-factor diagnostic plot, backed by matplotlib.

        Dispatches on ``model``: ``"ata"`` (multiplicative ATA factor
        diagnostic, 5 kind variants) or ``"intensity"`` (additive intensity
        diagnostic, 3 kind variants). Default ``model`` is ``"intensity"`` when
        the Link was built with ``exposure``, otherwise ``"ata"``.

        For ``model="ata"`` accepts:
        ``kind in {"cv","rse","summary","box","point"}``,
        ``alpha``, ``show_factor_stability``, ``max_cv``, ``max_rse``,
        ``min_run``, ``nrow``, ``ncol``, ``figsize``.

        For ``model="intensity"`` accepts:
        ``kind in {"summary","box","point"}``, ``alpha``, ``nrow``,
        ``ncol``, ``figsize``.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from ._link_vis import plot_link
        return plot_link(self, model=model, **kwargs)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def __repr__(self) -> str:
        mode = "dual-mode" if self._premium is not None else "ATA-only"
        n_links = self._df.height
        if self._groups is None:
            return f"<Link: {n_links} links, {mode}>"
        n_groups = self._df.select(normalize_groups(self._groups)).unique().height
        return (
            f"<Link: {n_groups} groups, {n_links} total links, {mode}>"
        )
