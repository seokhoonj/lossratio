"""Chain ladder loss model (Mack multiplicative recursion).

``ChainLadder`` is a loss-side estimator: ``.fit(triangle)`` develops the
cumulative ``loss`` series via Mack's multiplicative age-to-age recursion
and returns a role-based :class:`~lossratio.loss.LossFit` (``.model ==
"chain_ladder"``). It is the classical reference projection.

The fit runs on the loss-projection engine shared by all three loss models
(chain ladder / exposure-driven / stage-adaptive); ``ChainLadder`` is the
all-CL configuration of that engine.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from .loss import LossFit
    from .triangle import Triangle


class ChainLadder:
    """Mack chain ladder loss projection.

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
        Tail-factor extension beyond the observed development window.
        ``False`` (default) applies no tail; a positive number / ``True``
        requests the log-linear tail factor.
    conf_level
        Confidence level for the analytical CI on ``loss_proj``. Default
        ``0.95``.
    uncertainty
        Uncertainty strategy. ``None`` (default) keeps the closed-form
        Mack analytical SE. Pass :class:`~lossratio.Analytical`,
        :class:`~lossratio.ResidualBootstrap`, or
        :class:`~lossratio.MonteCarlo` to override.
    """

    def __init__(
        self,
        *,
        alpha:       float       = 1.0,
        sigma_method: str        = "locf",
        recent:      int | None  = None,
        regime:      Any         = None,
        tail:        bool | float = False,
        conf_level:  float       = 0.95,
        uncertainty: Any         = None,
    ) -> None:
        self.alpha       = alpha
        self.sigma_method = sigma_method
        self.recent      = recent
        self.regime      = regime
        self.tail        = tail
        self.conf_level  = conf_level
        self.uncertainty = uncertainty

    def fit(self, triangle: "Triangle") -> "LossFit":
        """Fit the chain ladder loss projection on a Triangle."""
        from .loss import Loss
        from .uncertainty import resolve_uncertainty

        # The loss SE overlay always uses the analytical-CL (Mack) bootstrap
        # paradigm regardless of model; resolve against method="cl".
        boots = resolve_uncertainty(
            self.uncertainty, triangle, target="loss", method="cl",
        )
        return Loss(
            method      = "cl",
            alpha       = self.alpha,
            sigma_method = self.sigma_method,
            recent      = self.recent,
            regime      = self.regime,
            tail        = self.tail,
            conf_level  = self.conf_level,
            bootstrap   = boots,
        ).fit(triangle)
