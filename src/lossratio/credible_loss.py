"""Credible-loss estimator (charter Sec.3.1 / Sec.4.3-4.4, redesigned naming v2).

``CredibleLoss`` is the partial-pooling rung of the structure ladder
(``PooledLoss`` -> ``CredibleLoss`` -> ``SmoothLoss``): the pooled intensity
``g_k`` plus a per-cohort credibility LEVEL ``u_i`` (the dispersion-scaled
Buhlmann-Straub conjugate), so a cohort's projected increment is
``u_i * g_k * P_k`` -- its own loss-ratio level shrunk toward the pooled level
by its credibility. It returns the engine-backed
:class:`~lossratio.loss_fit.LossFit`.

Exact ladder nesting: ``psi = 0`` (no between-cohort variance) degenerates to
``u = 1``, i.e. ``CredibleLoss(psi=0)`` reproduces ``PooledLoss`` cell-for-cell
-- the ladder's automatic collapse when credibility earns nothing.

v1 is point-only: the credibility level's estimation variance makes the Mack
analytical recursion invalid (charter Sec.5.1/5.2), so the SE / CI columns are
null -- interval coverage rides the ResidualBootstrap, wired in a later step.
``recent`` and ``borrow`` are not supported yet and are rejected at config time.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from .loss_fit import LossFit, _EstimatorBase, _fit_loss

if TYPE_CHECKING:
    from .triangle import Triangle


@dataclass(kw_only=True)
class CredibleLoss(_EstimatorBase):
    """Partial-pooling credibility loss estimator.

    Parameters
    ----------
    psi
        Between-cohort variance component: ``"auto"`` (default) estimates it by
        the Buhlmann-Straub moment, or a fixed non-negative float. ``psi = 0``
        degenerates to :class:`~lossratio.pooled_loss.PooledLoss`.
    sigma_method
        Tail-sigma extrapolation for edf-deficient links: ``"locf"`` (default).
    regime
        Resolved cohort cut: ``None``, a ``date``, or a ``dict[segment -> date]``.
    conf_level
        Two-sided confidence level (unused in v1 -- SE/CI are null point-only).
    """

    psi: "float | str" = "auto"

    def __post_init__(self) -> None:
        super().__post_init__()
        if self.psi != "auto":
            if (
                isinstance(self.psi, bool)
                or not isinstance(self.psi, (int, float))
                or self.psi < 0
            ):
                raise ValueError(
                    f'psi must be "auto" or a non-negative float, got {self.psi!r}'
                )
        if self.recent is not None:
            raise NotImplementedError(
                "CredibleLoss does not support recent yet"
            )
        if self.borrow is not False:
            raise NotImplementedError(
                "CredibleLoss does not support borrow yet"
            )

    def fit(self, triangle: "Triangle") -> LossFit:
        """Fit the credibility loss projection on a :class:`Triangle`."""
        return _fit_loss(
            triangle,
            mechanism="credible",
            sigma_method=self.sigma_method,
            regime=self.regime,
            recent=None,
            conf_level=self.conf_level,
            borrow=False,
            psi=self.psi,
        )
