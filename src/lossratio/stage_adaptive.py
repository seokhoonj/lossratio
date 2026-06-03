"""Stage-adaptive loss model (exposure-driven before maturity, CL after).

``StageAdaptive`` is a loss-side estimator: ``.fit(triangle)`` develops the
cumulative ``loss`` series with a maturity switch -- exposure-driven (ED)
for early development (where age-to-age factors are volatile) and Mack chain
ladder (CL) once the development has matured -- and returns a role-based
:class:`~lossratio.loss.LossFit` (``.model == "stage_adaptive"``).

It is the ED+CL composition configuration of the shared loss-projection
engine. The maturity switch ``k*`` is resolved from the ``maturity``
argument (auto-detected per group by default).
"""

from __future__ import annotations

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ._types import MaturityArg, RegimeArg, TailArg, UncertaintyArg
    from .loss import LossFit
    from .triangle import Triangle


class StageAdaptive:
    """Stage-adaptive (ED-before-maturity, CL-after) loss projection.

    Parameters
    ----------
    alpha
        Variance-structure exponent. Default ``1`` (only value supported).
    sigma_method
        Tail-sigma extrapolation: ``"locf"`` (default), ``"min_last2"``,
        or ``"loglinear"``.
    maturity
        Maturity specification for the ED -> CL switch. Four-type
        dispatch: ``"auto"`` (default, auto-detect per group), a
        :class:`~lossratio.Maturity` object, a callable
        ``f(triangle) -> Maturity``, or ``None`` (no switch -- falls back
        to ED throughout).
    max_cv, max_rse, min_run
        Stability thresholds for ``maturity="auto"`` detection. Ignored
        when ``maturity`` is a concrete object or callable.
    recent
        Optional positive integer; restricts factor estimation to the
        most-recent ``recent`` calendar diagonals. ``None`` (default)
        uses all diagonals.
    regime
        Loss-side regime filter (cohort-axis cut). See
        :class:`~lossratio.Regime`. ``None`` (default) applies no filter.
    tail
        Tail extension beyond the observed window. ``False`` (default)
        applies no tail; a positive number is an explicit multiplicative
        factor; ``True`` / a :class:`~lossratio.Tail` spec computes the
        tail of whichever stage is active at the last development column --
        the post-maturity CL (multiplicative ``f -> 1``, fit on the
        post-maturity factors) when a maturity switch is found, otherwise
        the ED additive ``g -> 0`` tail.
    conf_level
        Confidence level for the analytical CI on ``loss_proj``. Default
        ``0.95``.
    uncertainty
        Uncertainty strategy. ``None`` (default) keeps the closed-form
        analytical SA SE. :class:`~lossratio.ResidualBootstrap` is
        rejected: the ED(additive)+CL(multiplicative) two-phase fit with
        an estimated switch point has no coherent single residual pool, so
        stage-adaptive uncertainty must be distribution-based
        (:class:`~lossratio.MonteCarlo`) or the analytical SA SE.
    """

    def __init__(
        self,
        *,
        alpha:        float          = 1.0,
        sigma_method: str            = "locf",
        maturity:     MaturityArg    = "auto",
        max_cv:       float          = 0.15,
        max_rse:      float          = 0.05,
        min_run:      int            = 2,
        recent:       int | None     = None,
        regime:       RegimeArg      = None,
        tail:         TailArg        = False,
        conf_level:   float          = 0.95,
        uncertainty:  UncertaintyArg = None,
    ) -> None:
        from .uncertainty import ResidualBootstrap

        if isinstance(uncertainty, ResidualBootstrap):
            raise ValueError(
                "StageAdaptive does not support ResidualBootstrap: the "
                "ED+CL two-phase fit has no coherent single residual pool. "
                "Use MonteCarlo() or the default analytical SE."
            )
        self.alpha       = alpha
        self.sigma_method = sigma_method
        self.maturity    = maturity
        self.max_cv      = max_cv
        self.max_rse     = max_rse
        self.min_run     = min_run
        self.recent      = recent
        self.regime      = regime
        self.tail        = tail
        self.conf_level  = conf_level
        self.uncertainty = uncertainty

    def fit(self, triangle: "Triangle") -> "LossFit":
        """Fit the stage-adaptive loss projection on a Triangle."""
        from .loss import Loss
        from .uncertainty import resolve_uncertainty

        # The loss SE overlay always uses the analytical-CL (Mack) bootstrap
        # paradigm regardless of model; resolve against method="cl".
        boots = resolve_uncertainty(
            self.uncertainty, triangle, target="loss", method="cl",
        )
        return Loss(
            method      = "sa",
            alpha       = self.alpha,
            sigma_method = self.sigma_method,
            maturity    = self.maturity,
            max_cv      = self.max_cv,
            max_rse     = self.max_rse,
            min_run     = self.min_run,
            recent      = self.recent,
            regime      = self.regime,
            tail        = self.tail,
            conf_level  = self.conf_level,
            bootstrap   = boots,
        ).fit(triangle)
