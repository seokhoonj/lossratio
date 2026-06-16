"""Loss-ratio composition (charter Sec.5.3 -- numerator / denominator).

``Ratio`` pairs a loss-side estimator (``PooledLoss`` / ``CredibleLoss`` /
``SmoothLoss`` / ``LinkRatio``) with a premium-side estimator
(``PooledPremium``) and composes the projected loss ratio
``ratio_proj = loss_proj / premium_proj`` cell by cell, propagating the two
projections' uncertainty into a ratio band.

Two ``se_method`` settings control how premium uncertainty enters:

* ``"fixed"`` -- premium is treated as known (the projected denominator is
  deterministic): ``ratio_se = loss_total_se / premium_proj``. This is the
  band the loss fit already carries internally; ``Ratio(se_method="fixed")``
  reproduces it while letting the denominator model be chosen and inspected.
* ``"delta"`` -- the full first-order delta method on ``R = L / P``, adding the
  premium variance and an optional loss-premium correlation ``rho``::

      ratio_se = (1 / |P|) * sqrt(seL^2 + R^2 seP^2 - 2 R rho seL seP)

  ``rho = 0`` (default) treats the loss and premium projections as
  independent; a positive ``rho`` (claims and premium moving together within a
  cohort) narrows the band.

The result is a :class:`RatioFit` -- a long-format frame with ``ratio_proj`` +
the ratio band, carrying the contributing ``loss_proj`` / ``premium_proj`` and
their SEs for transparency.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

import polars as pl
from scipy.stats import norm

from ._io import mirror_output, normalize_groups
from .loss_fit import LossFit, _EstimatorBase
from .premium_fit import PremiumFit, _PremiumEstimatorBase

if TYPE_CHECKING:
    from ._io import FrameLike
    from .triangle import Triangle


_RATIO_COLUMNS = [
    "cohort", "duration",
    "loss_proj", "premium_proj", "ratio_proj",
    "loss_total_se", "premium_total_se",
    "ratio_se", "ratio_ci_lo", "ratio_ci_hi",
    "source",
]


@dataclass(kw_only=True)
class Ratio:
    """Loss-ratio composition of a loss estimator and a premium estimator.

    Parameters
    ----------
    loss
        A loss-side estimator (``PooledLoss`` / ``CredibleLoss`` /
        ``SmoothLoss`` / ``LinkRatio``).
    premium
        A premium-side estimator (``PooledPremium``). Defaults to a plain
        ``PooledPremium()``.
    se_method
        ``"fixed"`` (premium deterministic; default) or ``"delta"`` (full
        delta method with premium variance + ``rho``).
    rho
        Loss-premium correlation used by ``se_method="delta"`` (default 0.0,
        independent). Ignored when ``se_method="fixed"``.
    conf_level
        Two-sided confidence level for the ratio CI columns.
    """

    loss: "_EstimatorBase"
    premium: "_PremiumEstimatorBase" = field(default_factory=lambda: _default_premium())
    se_method: str = "fixed"
    rho: float = 0.0
    conf_level: float = 0.95

    def __post_init__(self) -> None:
        if not isinstance(self.loss, _EstimatorBase):
            raise TypeError(
                "loss must be a loss-side estimator (PooledLoss / CredibleLoss "
                f"/ SmoothLoss / LinkRatio), got {type(self.loss).__name__}"
            )
        if not isinstance(self.premium, _PremiumEstimatorBase):
            raise TypeError(
                "premium must be a premium-side estimator (PooledPremium), got "
                f"{type(self.premium).__name__}"
            )
        if self.se_method not in ("fixed", "delta"):
            raise ValueError(
                f"se_method must be 'fixed' or 'delta', got {self.se_method!r}"
            )
        if not (-1.0 <= self.rho <= 1.0):
            raise ValueError(f"rho must be in [-1, 1], got {self.rho!r}")
        if not (0.0 < self.conf_level < 1.0):
            raise ValueError(f"conf_level must be in (0, 1), got {self.conf_level!r}")

    def fit(self, triangle: "Triangle") -> "RatioFit":
        """Fit both sides on ``triangle`` and compose the loss ratio."""
        loss_fit = self.loss.fit(triangle)
        premium_fit = self.premium.fit(triangle)
        groups = normalize_groups(triangle.groups)
        keys = (groups or []) + ["cohort", "duration"]

        left = loss_fit._df.select(
            [*keys, "loss_proj", "loss_total_se", "source"]
        )
        right = premium_fit._df.select([*keys, "premium_proj", "premium_total_se"])
        joined = left.join(right, on=keys, how="left")

        z = float(norm.ppf((1 + self.conf_level) / 2))
        # deterministic denominator unless `delta`; a zero/null premium yields a
        # null ratio rather than an inf.
        safe_pp = (
            pl.when(pl.col("premium_proj").is_null() | (pl.col("premium_proj") == 0.0))
            .then(None)
            .otherwise(pl.col("premium_proj"))
        )
        ratio_proj = pl.col("loss_proj") / safe_pp
        if self.se_method == "fixed":
            ratio_se = pl.col("loss_total_se") / safe_pp.abs()
        else:
            seL = pl.col("loss_total_se")
            seP = pl.col("premium_total_se")
            radicand = (
                seL**2
                + ratio_proj**2 * seP**2
                - 2.0 * self.rho * ratio_proj * seL * seP
            )
            ratio_se = pl.max_horizontal(radicand, pl.lit(0.0)).sqrt() / safe_pp.abs()

        composed = joined.with_columns(
            ratio_proj.alias("ratio_proj"),
        ).with_columns(
            ratio_se.alias("ratio_se"),
        ).with_columns(
            ratio_ci_lo=pl.max_horizontal(
                pl.col("ratio_proj") - z * pl.col("ratio_se"), pl.lit(0.0)
            ),
            ratio_ci_hi=pl.col("ratio_proj") + z * pl.col("ratio_se"),
        )

        order = (groups or []) + _RATIO_COLUMNS
        df = composed.select(order)

        return RatioFit(
            df,
            groups=triangle.groups,
            loss_model=loss_fit.model,
            premium_model=premium_fit.model,
            se_method=self.se_method,
            rho=self.rho,
            conf_level=self.conf_level,
            output_type=triangle._output_type,
            loss_fit=loss_fit,
            premium_fit=premium_fit,
        )


def _default_premium() -> "_PremiumEstimatorBase":
    # local import avoids a module-load cycle (pooled_premium -> premium_fit;
    # ratio -> pooled_premium would otherwise import at module scope).
    from .pooled_premium import PooledPremium

    return PooledPremium()


# ---------------------------------------------------------------------------
# Result object
# ---------------------------------------------------------------------------


class RatioFit:
    """Composed loss-ratio projection result.

    The long-format frame (one row per cohort x duration cell) carries
    ``ratio_proj`` + the ratio band, plus the contributing ``loss_proj`` /
    ``premium_proj`` and their SEs. The underlying ``loss_fit`` / ``premium_fit``
    are retained for drill-down.
    """

    def __init__(
        self,
        df: pl.DataFrame,
        *,
        groups: "str | list[str] | None",
        loss_model: str,
        premium_model: str,
        se_method: str,
        rho: float,
        conf_level: float,
        output_type: str,
        loss_fit: LossFit,
        premium_fit: PremiumFit,
    ) -> None:
        self._df = df
        self._output_type = output_type
        self.groups = groups
        self.loss_model = loss_model
        self.premium_model = premium_model
        self.se_method = se_method
        self.rho = rho
        self.conf_level = conf_level
        self.loss_fit = loss_fit
        self.premium_fit = premium_fit

    @property
    def df(self) -> "FrameLike":
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def summary(self) -> "FrameLike":
        """Per-cohort headline: the latest within-triangle projected loss ratio
        and its SE."""
        keys = (normalize_groups(self.groups) or []) + ["cohort"]
        agg = self._df.group_by(keys, maintain_order=True).agg(
            loss_proj=pl.col("loss_proj").drop_nulls().last(),
            premium_proj=pl.col("premium_proj").drop_nulls().last(),
            ratio_proj=pl.col("ratio_proj").drop_nulls().last(),
            ratio_se=pl.col("ratio_se").drop_nulls().last(),
        )
        return mirror_output(agg, self._output_type)

    def __repr__(self) -> str:
        return (
            f"RatioFit(loss_model={self.loss_model!r}, "
            f"premium_model={self.premium_model!r}, "
            f"se_method={self.se_method!r}, rows={self._df.height})"
        )
