"""Smooth-loss estimator (charter Sec.3.1 / Sec.4, redesigned naming v2).

``SmoothLoss`` is the top rung of the structure ladder
(``PooledLoss`` -> ``CredibleLoss`` -> ``SmoothLoss``): the credible rung with
the free saturated per-duration intensity replaced by a smooth penalized
B-spline shape ``g_k = exp(s(k))``. The smooth shape and the per-cohort
credibility level are fit jointly by backfitting (charter Sec.4.5): the s-step
refits the shape on the ``u``-adjusted exposure to decontaminate the
late-duration wedge, the u-step is the dispersion-scaled conjugate the credible
rung already uses. It returns the engine-backed
:class:`~lossratio.loss_fit.LossFit`.

Exact ladder relation: with ``psi = 0`` the credibility level is ``u = 1``, so
``SmoothLoss`` is a single smooth pass (no contamination to remove); a one-hot
shape with no penalty would in turn reduce the smooth shape to the saturated
``g_k`` -- the chain back to the golden anchor.

The smooth-shape + credibility estimation variance breaks the Mack analytical
recursion, so SE / CI are null UNLESS a :class:`~lossratio._resample.ResidualBootstrap`
is attached -- the bootstrap re-runs the whole smooth pipeline (shape +
``lambda`` selection + level) per replicate, so the interval and the coverage
lane are available for ``SmoothLoss`` like the credible rung. ``recent`` and
``borrow`` are not supported.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from .loss_fit import LossFit, _EstimatorBase, _fit_loss

if TYPE_CHECKING:
    from .triangle import Triangle


@dataclass(kw_only=True)
class SmoothLoss(_EstimatorBase):
    """Smooth-shape partial-pooling loss estimator (top ladder rung).

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
        Two-sided confidence level for the bootstrap band (used when an
        ``uncertainty=ResidualBootstrap(...)`` is attached; SE / CI are null
        otherwise).
    uncertainty
        ``None`` (point-only) or a :class:`~lossratio._resample.ResidualBootstrap`
        -- a full smooth-pipeline refit per replicate. Each replicate re-runs the
        shape + ``lambda`` selection + backfitting, so the selection uncertainty
        is captured (charter Sec.5.2). NOTE: this is materially heavier than the
        pooled / credible bootstrap (a backfitting per replicate).
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
            raise NotImplementedError("SmoothLoss does not support recent yet")
        if self.borrow is not False:
            raise NotImplementedError("SmoothLoss does not support borrow yet")

    def fit(self, triangle: "Triangle") -> LossFit:
        """Fit the smooth (GLMM) loss projection on a :class:`Triangle`."""
        return _fit_loss(
            triangle,
            mechanism="smooth",
            sigma_method=self.sigma_method,
            regime=self.regime,
            recent=None,
            conf_level=self.conf_level,
            borrow=False,
            psi=self.psi,
            n_basis=self.n_basis,
            lam=self.lam,
            uncertainty=self.uncertainty,
        )
