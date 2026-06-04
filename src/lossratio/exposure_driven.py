"""Exposure-driven loss model (additive, premium-anchored).

``ExposureDriven`` is a loss-side estimator: ``.fit(triangle)`` develops the
cumulative ``loss`` series via the additive exposure-driven rule
``E[dloss | F] = g_k * premium`` (intensity ``g_k`` pooled across cohorts,
projected onto the developing cumulative premium) and returns a role-based
:class:`~lossratio.loss.LossFit` (``.model == "exposure_driven"``).

ExposureDriven is the unconditional safe baseline -- no maturity dependency,
robust to early-development age-to-age volatility. It is the all-ED
configuration of the shared loss-projection engine. The optional ``tail``
extends the projection beyond the observed window via the additive
intensity ``g_k -> 0`` (tail increment = ``Sum future g_k * premium``).
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ._types import RegimeArg, TailArg, UncertaintyArg
    from .loss import LossFit
    from .triangle import Triangle


@dataclass(kw_only=True)
class ExposureDriven:
    """Exposure-driven (additive) loss projection.

    Parameters
    ----------
    alpha
        Variance-structure exponent. Default ``1`` (only value supported).
    sigma_method
        Tail-sigma extrapolation: ``"locf"`` (default), ``"min_last2"``,
        or ``"loglinear"``.
    recent
        Optional positive integer. When supplied, only the most-recent
        ``recent`` calendar diagonals feed factor estimation; the point
        projection still covers the full grid. ``None`` (default) uses all
        diagonals.
    regime
        Loss-side regime filter (cohort-axis cut). See
        :class:`~lossratio.Regime`. ``None`` (default) applies no filter.
    tail
        Tail extension beyond the observed window. ``False`` (default)
        applies no tail; a positive number is used directly as a
        multiplicative tail factor; ``True`` requests the default
        convergence-gated additive tail (``Sum future g_k * premium``);
        a :class:`~lossratio.Tail` spec configures the curve family,
        divergence policy, and horizon.
    conf_level
        Confidence level for the analytical CI on ``loss_proj``. Default
        ``0.95``.
    uncertainty
        Uncertainty strategy. ``None`` (default) keeps the closed-form
        analytical SE. Pass :class:`~lossratio.Analytical`,
        :class:`~lossratio.ResidualBootstrap`, or
        :class:`~lossratio.ParametricBootstrap` to override.
    """

    alpha:        float          = 1.0
    sigma_method: str            = "locf"
    recent:       int | None     = None
    regime:       RegimeArg      = None
    tail:         TailArg        = False
    conf_level:   float          = 0.95
    uncertainty:  UncertaintyArg = None

    def fit(self, triangle: "Triangle") -> "LossFit":
        """Fit the exposure-driven loss projection on a Triangle."""
        from .loss import Loss
        from .uncertainty import resolve_uncertainty

        # The loss SE overlay always uses the analytical-CL (Mack) bootstrap
        # paradigm regardless of model; resolve against method="cl".
        boots = resolve_uncertainty(
            self.uncertainty, triangle, target="loss", method="cl",
        )
        return Loss(
            method      = "ed",
            alpha       = self.alpha,
            sigma_method = self.sigma_method,
            recent      = self.recent,
            regime      = self.regime,
            tail        = self.tail,
            conf_level  = self.conf_level,
            bootstrap   = boots,
        ).fit(triangle)
