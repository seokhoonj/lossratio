"""Uncertainty strategies for projection fits.

Three strategy classes -- passed on a model's ``uncertainty=`` constructor
argument -- name the uncertainty paradigm by WHAT IT IS:

* :class:`Analytical` -- Mack / ED closed-form propagation (NO resampling).
  The default: the fit reports its built-in closed-form analytical SE and
  no bootstrap overlay is applied.
* :class:`ResidualBootstrap` -- nonparametric residual resampling
  (England-Verrall). Coherent for single-model chain ladder.
* :class:`MonteCarlo` -- parametric simulation: draw either the per-cell
  increments from the process distribution (``draw="process"``, the
  default) or the development parameters from their asymptotic normal
  (``draw="parameter"`` -- the Mack closed-form propagation simulated).

A strategy is MODEL-AGNOSTIC: the model supplies its own fit paradigm
(``method`` in ``{"cl", "ed", "sa"}``); the strategy supplies the
uncertainty paradigm. ``Analytical`` resolves to ``None`` (keep the fit's
closed-form SE); the other two resolve to a :class:`.bootstrap.BootstrapTriangle`
via the engine in :mod:`lossratio.bootstrap`.

This module is the PUBLIC uncertainty surface; the resampling/simulation
engine itself stays in :mod:`lossratio.bootstrap` behind these classes.
Model-vs-strategy coherence guards (e.g. stage-adaptive must reject
:class:`ResidualBootstrap`) live with the model wiring, not here.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from .bootstrap import BootstrapTriangle
    from .triangle import Triangle


@dataclass
class Analytical:
    """Closed-form Mack / ED analytical SE (no resampling). The default.

    Carries no engine configuration: ``_resolve`` returns ``None``, which
    signals the fit to keep its built-in closed-form analytical SE (the
    proc / param / total decomposition computed directly in the fit). This
    is the cheapest and most stable option and the default for CL / ED.

    Parameters
    ----------
    alpha
        Mack variance exponent ``Var(C_{k+1}|C_k) = sigma^2 C_k^alpha``.
        Default ``1`` (the only value currently supported by the workers).
    """

    alpha: float = 1.0

    def _resolve(
        self, triangle: "Triangle", *, target: str, method: str
    ) -> "BootstrapTriangle | None":
        # No overlay: the fit's built-in closed-form SE is the answer.
        return None


@dataclass
class ResidualBootstrap:
    """Nonparametric residual bootstrap (England-Verrall resampling).

    Resamples fitted residuals to build pseudo-triangles, refits, and
    propagates. Coherent for single-model chain ladder. Maps to the engine
    ``type="nonparametric"`` path.

    Parameters
    ----------
    residual
        Residual scope: ``"cell"`` (ODP Pearson, England-Verrall) or
        ``"link"`` (Mack standardized link residuals).
    process
        Stage-2 process distribution: ``"gamma"`` (default), ``"od_pois"``,
        or ``"normal"``. The ``"cell"`` (ODP) path requires a
        positivity-preserving process (``gamma`` / ``od_pois``).
    pooling
        Residual pooling: ``"pooled"`` (default), ``"separated"``, or
        ``"tail_pooled"``.
    tail, min_pool
        Tail-pooling strategy and minimum pool size for thin links.
    hat_adj, demean
        Hat-matrix bias correction and residual demeaning.
    B, seed
        Replicate count and reproducibility seed.
    quantile_ci
        Emit empirical percentile CI (``ci_lo`` / ``ci_hi``).
    """

    residual:    str        = "cell"
    process:     str        = "gamma"
    pooling:     str        = "pooled"
    tail:        str        = "auto"
    min_pool:    int        = 5
    hat_adj:     bool       = True
    demean:      bool       = True
    B:           int        = 499
    seed:        int | None = None
    quantile_ci: bool       = False

    def _resolve(
        self, triangle: "Triangle", *, target: str, method: str
    ) -> "BootstrapTriangle":
        from .bootstrap import Bootstrap

        return Bootstrap(
            type        = "nonparametric",
            method      = method,
            residual    = self.residual,
            process     = self.process,
            pooling     = self.pooling,
            tail        = self.tail,
            min_pool    = self.min_pool,
            hat_adj     = self.hat_adj,
            demean      = self.demean,
            B           = self.B,
            seed        = self.seed,
            quantile_ci = self.quantile_ci,
        ).fit(triangle, target=target)


@dataclass
class MonteCarlo:
    """Parametric simulation bootstrap.

    Two flavours, selected by ``draw``:

    * ``"process"`` (default) -- draw the per-cell incremental losses from
      the process distribution (parametric / textbook cell simulation;
      engine ``type="parametric"``).
    * ``"parameter"`` -- draw the development factors from their asymptotic
      normal and propagate (the Mack closed-form, simulated; engine
      ``type="analytical"``, chain-ladder + normal process only). This is
      the simulated counterpart of :class:`Analytical`.

    The default and only coherent uncertainty for stage-adaptive (SA),
    whose two-phase ED+CL structure has no single residual pool.

    Parameters
    ----------
    process
        Stage-2 process distribution: ``"gamma"`` (default), ``"od_pois"``,
        or ``"normal"``. Forced to ``"normal"`` is required when
        ``draw="parameter"``.
    draw
        ``"process"`` (default) or ``"parameter"`` -- see above.
    hat_adj
        Hat-matrix bias correction (cell path).
    B, seed
        Replicate count and reproducibility seed.
    quantile_ci
        Emit empirical percentile CI (``ci_lo`` / ``ci_hi``).
    """

    process:     str        = "gamma"
    draw:        str        = "process"
    hat_adj:     bool       = True
    B:           int        = 499
    seed:        int | None = None
    quantile_ci: bool       = False

    def __post_init__(self) -> None:
        if self.draw not in ("process", "parameter"):
            raise ValueError(
                f"draw must be 'process' or 'parameter', got {self.draw!r}"
            )
        if self.draw == "parameter" and self.process != "normal":
            raise ValueError(
                "draw='parameter' (Mack closed-form simulated -- draws the "
                "development factors from their asymptotic normal) requires "
                f"process='normal', got {self.process!r}."
            )

    def _resolve(
        self, triangle: "Triangle", *, target: str, method: str
    ) -> "BootstrapTriangle":
        from .bootstrap import Bootstrap

        engine_type = "analytical" if self.draw == "parameter" else "parametric"
        return Bootstrap(
            type        = engine_type,
            method      = method,
            process     = self.process,
            hat_adj     = self.hat_adj,
            B           = self.B,
            seed        = self.seed,
            quantile_ci = self.quantile_ci,
        ).fit(triangle, target=target)


def resolve_uncertainty(
    uncertainty: Any,
    triangle: "Triangle",
    *,
    target: str,
    method: str,
) -> "BootstrapTriangle | None":
    """Resolve an ``uncertainty=`` argument to a result or ``None``.

    Accepted forms:

    * ``None`` -> ``None`` (the fit keeps its closed-form analytical SE;
      equivalent to :class:`Analytical`).
    * an :class:`Analytical` / :class:`ResidualBootstrap` /
      :class:`MonteCarlo` strategy instance -> delegated to its
      ``_resolve``.
    * a callable ``f(triangle) -> strategy`` -> invoked, then re-resolved.

    ``method`` is the model's own fit paradigm (``"cl"`` / ``"ed"`` /
    ``"sa"``); the strategy is model-agnostic and receives it here.
    """
    if uncertainty is None:
        return None
    if isinstance(uncertainty, (Analytical, ResidualBootstrap, MonteCarlo)):
        return uncertainty._resolve(triangle, target=target, method=method)
    if callable(uncertainty):
        return resolve_uncertainty(
            uncertainty(triangle), triangle, target=target, method=method
        )
    raise TypeError(
        "`uncertainty` must be None, an Analytical / ResidualBootstrap / "
        "MonteCarlo strategy, or a callable returning one; got "
        f"{type(uncertainty).__name__}."
    )
