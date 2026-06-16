"""Smooth-premium estimator (denominator-side, top ladder rung).

``SmoothPremium`` is the top rung of the premium ladder
(``PooledPremium`` -> ``CrediblePremium`` -> ``SmoothPremium``), the
denominator mirror of :class:`~lossratio.smooth_loss.SmoothLoss`. It is the
credible premium rung with the saturated self-exposure intensity
``h_k = f^P_k - 1`` replaced by a smooth P-spline shape ``h_k = exp(s(k))``, fit
by the shared backfitting core (smooth shape + GCV ``lambda`` + conjugate level)
on premium as its own exposure. The projection is the self-exposure
multiplicative recursion ``P_{k+1} = P_k * (1 + u_i * h_k)``. It returns the
engine-backed :class:`~lossratio.premium_fit.PremiumFit`.

Point-only in v1 (SE / CI null, like the loss smooth rung); ``recent`` is
rejected.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from .premium_fit import PremiumFit, _PremiumEstimatorBase, _fit_premium

if TYPE_CHECKING:
    from .triangle import Triangle


@dataclass(kw_only=True)
class SmoothPremium(_PremiumEstimatorBase):
    """Smooth-shape partial-pooling premium estimator (top denominator rung).

    Parameters
    ----------
    psi
        Between-cohort variance: ``"auto"`` (default) estimates it by the
        Buhlmann-Straub moment, or a fixed non-negative float. ``psi = 0``
        keeps ``u = 1`` (a single smooth pass).
    lam
        Smoothing parameter for the 2nd-difference P-spline penalty: ``"auto"``
        (default) selects it by GCV, or a fixed non-negative float.
    n_basis
        B-spline basis size: ``None`` (default) uses a generous fixed count
        bounded by the segment's distinct durations.
    sigma_method
        Tail-sigma extrapolation for edf-deficient links: ``"locf"`` (default).
    regime
        Resolved cohort cut: ``None``, a ``date``, or a ``dict[segment -> date]``.
    conf_level
        Two-sided confidence level (unused in v1 -- SE/CI are null point-only).
    """

    psi: "float | str" = "auto"
    lam: "float | str" = "auto"
    n_basis: "int | None" = None

    def __post_init__(self) -> None:
        super().__post_init__()
        for name, val in (("psi", self.psi), ("lam", self.lam)):
            if val != "auto":
                if (
                    isinstance(val, bool)
                    or not isinstance(val, (int, float))
                    or val < 0
                ):
                    raise ValueError(
                        f'{name} must be "auto" or a non-negative float, got {val!r}'
                    )
        if self.n_basis is not None and (
            not isinstance(self.n_basis, int)
            or isinstance(self.n_basis, bool)
            or self.n_basis < 4
        ):
            raise ValueError(
                f"n_basis must be None or an int >= 4, got {self.n_basis!r}"
            )
        if self.recent is not None:
            raise NotImplementedError("SmoothPremium does not support recent yet")

    def fit(self, triangle: "Triangle") -> PremiumFit:
        """Fit the smooth premium projection on a :class:`Triangle`."""
        return _fit_premium(
            triangle,
            mechanism="smooth",
            sigma_method=self.sigma_method,
            regime=self.regime,
            conf_level=self.conf_level,
            psi=self.psi,
            n_basis=self.n_basis,
            lam=self.lam,
        )
