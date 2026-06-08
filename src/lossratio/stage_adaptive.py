"""Stage-adaptive loss model (exposure-driven before the switch, CL after).

``StageAdaptive`` is a loss-side estimator: ``.fit(triangle)`` develops the
cumulative ``loss`` series with an ED -> CL switch -- exposure-driven (ED)
for early development (where age-to-age factors are volatile) and Mack chain
ladder (CL) past the switch duration -- and returns a role-based
:class:`~lossratio.loss.LossFit` (``.model == "stage_adaptive"``).

It is the ED+CL composition configuration of the shared loss-projection
engine. The switch duration is resolved from the ``switch`` argument (a
:class:`~lossratio.SwitchPoint`, an int, or ``None`` for pure ED).
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ._types import RegimeArg, SwitchArg, TailArg, UncertaintyArg
    from .loss import LossFit
    from .triangle import Triangle


@dataclass(kw_only=True)
class StageAdaptive:
    """Stage-adaptive (ED-before-switch, CL-after) loss projection.

    Parameters
    ----------
    alpha
        Variance-structure exponent. Default ``1`` (only value supported).
    sigma_method
        Tail-sigma extrapolation: ``"locf"`` (default), ``"min_last2"``,
        or ``"loglinear"``.
    switch
        The ED -> CL switch (authoritative input). ``None`` (default) takes
        no discretionary switch: the general path is pure ED, while a regime
        segment-borrow path still develops the borrowed late-duration region with
        the donor's level-invariant ``f_k``. Otherwise an ``int`` (fixed
        switch duration), a :class:`~lossratio.SwitchPoint`, or a
        ``SwitchPoint.detect()`` spec (backtest-selected, leakage-safe inside
        a backtest fold). ``StageAdaptive()`` with no switch and no regime is
        therefore equivalent to ``ExposureDriven()``.
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
        the post-switch CL (multiplicative ``f -> 1``, fit on the post-switch
        factors) when a switch is found, otherwise the ED additive
        ``g -> 0`` tail.
    conf_level
        Confidence level for the analytical CI on ``loss_proj``. Default
        ``0.95``.
    uncertainty
        Uncertainty strategy. ``None`` (default) keeps the closed-form
        analytical SA SE. :class:`~lossratio.ResidualBootstrap` is
        rejected: the ED(additive)+CL(multiplicative) two-phase fit with
        an estimated switch point has no coherent single residual pool, so
        stage-adaptive uncertainty must be distribution-based
        (:class:`~lossratio.ParametricBootstrap`) or the analytical SA SE.
    """

    alpha:        float          = 1.0
    sigma_method: str            = "locf"
    switch:       SwitchArg      = None
    recent:       int | None     = None
    regime:       RegimeArg      = None
    tail:         TailArg        = False
    conf_level:   float          = 0.95
    uncertainty:  UncertaintyArg = None

    def __post_init__(self) -> None:
        from ._recent import validate_recent as _validate_recent
        from ._sigma import VALID_SIGMA_METHODS
        from .tail import validate_tail
        from .uncertainty import ResidualBootstrap

        if self.sigma_method not in VALID_SIGMA_METHODS:
            raise ValueError(
                f"sigma_method must be one of {VALID_SIGMA_METHODS}, "
                f"got {self.sigma_method!r}"
            )
        if not (0.0 < self.conf_level < 1.0):
            raise ValueError(
                f"conf_level must be in (0, 1), got {self.conf_level!r}"
            )
        if self.alpha != 1.0:
            raise NotImplementedError(
                f"alpha={self.alpha} not yet implemented; only alpha=1 is supported"
            )
        _validate_recent(self.recent)
        validate_tail(self.tail)

        if isinstance(self.uncertainty, ResidualBootstrap):
            raise ValueError(
                "StageAdaptive does not support ResidualBootstrap: the "
                "ED+CL two-phase fit has no coherent single residual pool. "
                "Use ParametricBootstrap() or the default analytical SE."
            )

    def fit(self, triangle: "Triangle") -> "LossFit":
        """Fit the stage-adaptive loss projection on a Triangle."""
        from .loss import Loss
        from .uncertainty import resolve_uncertainty

        # The bootstrap SE follows the SA model and its switch so the
        # dispersion is centred on the same projection the point estimate
        # uses (Analytical(simulate) stays CL-only by its own nature). A
        # distribution-based ResidualBootstrap is rejected upstream; only
        # ParametricBootstrap / the analytical SE reach here.
        boots = resolve_uncertainty(
            self.uncertainty, triangle, target="loss", method="sa",
            switch=self.switch,
        )
        return Loss(
            method       = "sa",
            alpha        = self.alpha,
            sigma_method = self.sigma_method,
            switch       = self.switch,
            recent       = self.recent,
            regime       = self.regime,
            tail         = self.tail,
            conf_level   = self.conf_level,
            bootstrap    = boots,
        ).fit(triangle)
