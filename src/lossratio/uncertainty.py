"""Uncertainty strategies for projection fits.

Three strategy classes -- passed on a model's ``uncertainty=`` constructor
argument -- name the uncertainty paradigm by WHAT IT IS:

* :class:`Analytical` -- Mack / ED closed-form propagation (NO resampling).
  The default: the fit reports its built-in closed-form analytical SE.
  ``Analytical(simulate=True)`` returns the simulated Mack factor draw
  (CL only) for a predictive histogram.
* :class:`ResidualBootstrap` -- nonparametric residual resampling
  (England-Verrall). Coherent for single-model chain ladder.
* :class:`ParametricBootstrap` -- parametric bootstrap: per-cell process
  simulation (gamma / ODP) with refit. The parametric sibling of
  :class:`ResidualBootstrap`; the default uncertainty for stage-adaptive.

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


# Self-contained option enums, validated eagerly at construction (mirrors
# the Tail / Curve __post_init__ convention). The cross-field coherence
# checks (type/method-dependent, e.g. ED forbids link residuals) stay
# authoritative in Bootstrap.__post_init__, since `method` is only known
# at `_resolve` time.
_RESIDUALS = ("cell", "link")
_PROCESSES = ("gamma", "od_pois", "normal")
_POOLINGS = ("pooled", "separated", "tail_pooled")
_TAILS = ("auto",)


@dataclass
class Analytical:
    """Closed-form Mack / ED analytical SE (no resampling). The default.

    ``simulate=False`` (default) keeps the fit's built-in closed-form
    analytical SE (the proc / param / total decomposition computed
    directly in the fit) -- the cheapest, most stable option, the default
    for CL / ED, and the only one that needs no engine run (``_resolve``
    returns ``None``).

    ``simulate=True`` returns the *simulated* counterpart -- draws the
    development factors from their asymptotic normal and propagates (the
    Mack closed-form, Monte Carlo'd). Same mean and variance as the closed
    form; the only thing it adds is the (skewed) predictive *shape*,
    exposed as per-replicate samples for a histogram. Chain-ladder only --
    there is no factor recursion for the additive ED / composite SA, and
    an ED fit's closed form is already exactly Gaussian (a simulation
    would add nothing) -- so ``simulate=True`` always produces a CL band
    regardless of the headline method.

    Parameters
    ----------
    alpha
        Mack variance exponent ``Var(C_{k+1}|C_k) = sigma^2 C_k^alpha``.
        Default ``1`` (the only value currently supported by the workers).
    simulate
        ``False`` (default) -- closed-form SE. ``True`` -- the simulated
        Mack factor draw (CL only); yields per-replicate samples.
    n_replicates, seed, quantile_ci
        Consulted only when ``simulate=True``.
    """

    alpha:        float      = 1.0
    simulate:     bool       = False
    n_replicates: int        = 499
    seed:         int | None = None
    quantile_ci:  bool       = False

    def __post_init__(self) -> None:
        if self.n_replicates < 1:
            raise ValueError(
                f"n_replicates must be >= 1, got {self.n_replicates!r}"
            )

    def _resolve(
        self, triangle: "Triangle", *, target: str, method: str,
        switch: Any = None,
    ) -> "BootstrapTriangle | None":
        if not self.simulate:
            # Closed-form: the fit's built-in analytical SE is the answer.
            return None
        from .bootstrap import Bootstrap

        # Simulated Mack factor draw. The analytical paradigm is CL-only
        # (no factor recursion for additive ED / composite SA), so it is
        # always a CL band regardless of the headline `method` / `switch`.
        return Bootstrap(
            type         = "analytical",
            method       = "cl",
            process      = "normal",
            n_replicates = self.n_replicates,
            seed         = self.seed,
            alpha        = self.alpha,
            quantile_ci  = self.quantile_ci,
        ).fit(triangle, target=target)


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
    n_replicates, seed
        Replicate count and reproducibility seed.
    quantile_ci
        Emit empirical percentile CI (``ci_lo`` / ``ci_hi``).
    """

    residual:     str        = "cell"
    process:      str        = "gamma"
    pooling:      str        = "pooled"
    tail:         str        = "auto"
    min_pool:     int        = 5
    hat_adj:      bool       = True
    demean:       bool       = True
    n_replicates: int        = 499
    seed:         int | None = None
    quantile_ci:  bool       = False

    def __post_init__(self) -> None:
        if self.residual not in _RESIDUALS:
            raise ValueError(
                f"residual must be one of {_RESIDUALS}, got {self.residual!r}"
            )
        if self.process not in _PROCESSES:
            raise ValueError(
                f"process must be one of {_PROCESSES}, got {self.process!r}"
            )
        if self.pooling not in _POOLINGS:
            raise ValueError(
                f"pooling must be one of {_POOLINGS}, got {self.pooling!r}"
            )
        if self.tail not in _TAILS:
            raise ValueError(f"tail must be one of {_TAILS}, got {self.tail!r}")
        if not isinstance(self.n_replicates, int) or self.n_replicates < 1:
            raise ValueError("`n_replicates` must be a positive integer.")
        if not isinstance(self.min_pool, int) or self.min_pool < 1:
            raise ValueError("`min_pool` must be a positive integer.")

    def _resolve(
        self, triangle: "Triangle", *, target: str, method: str,
        switch: Any = None,
    ) -> "BootstrapTriangle":
        from .bootstrap import Bootstrap

        return Bootstrap(
            type         = "nonparametric",
            method       = method,
            switch       = switch,
            residual     = self.residual,
            process      = self.process,
            pooling      = self.pooling,
            tail         = self.tail,
            min_pool     = self.min_pool,
            hat_adj      = self.hat_adj,
            demean       = self.demean,
            n_replicates = self.n_replicates,
            seed         = self.seed,
            quantile_ci  = self.quantile_ci,
        ).fit(triangle, target=target)


@dataclass
class ParametricBootstrap:
    """Parametric bootstrap -- per-cell process simulation.

    Regenerates the triangle by drawing the per-cell incremental losses
    from a parametric process distribution (gamma / ODP), refits on the
    pseudo-triangle (parameter uncertainty), and adds process noise on the
    future cells (process uncertainty). The parametric sibling of
    :class:`ResidualBootstrap` -- the same refit-on-pseudo-triangle
    machinery, with the past perturbation drawn from an assumed
    distribution rather than resampled from empirical residuals.

    The default and only coherent uncertainty for stage-adaptive (SA),
    whose two-phase ED+CL structure has no single residual pool.

    Parameters
    ----------
    process
        Process distribution: ``"gamma"`` (default), ``"od_pois"``, or
        ``"normal"``.
    hat_adj
        Hat-matrix bias correction.
    n_replicates, seed
        Replicate count and reproducibility seed.
    quantile_ci
        Emit empirical percentile CI (``ci_lo`` / ``ci_hi``).
    """

    process:      str        = "gamma"
    hat_adj:      bool       = True
    n_replicates: int        = 499
    seed:         int | None = None
    quantile_ci:  bool       = False

    def __post_init__(self) -> None:
        if self.process not in _PROCESSES:
            raise ValueError(
                f"process must be one of {_PROCESSES}, got {self.process!r}"
            )
        if not isinstance(self.n_replicates, int) or self.n_replicates < 1:
            raise ValueError("`n_replicates` must be a positive integer.")

    def _resolve(
        self, triangle: "Triangle", *, target: str, method: str,
        switch: Any = None,
    ) -> "BootstrapTriangle":
        from .bootstrap import Bootstrap

        # Follows the headline `method` (ED -> ED, CL -> CL, SA -> SA) and
        # the SA `switch`: the predictive dispersion is centred on the same
        # projection that drives the point estimate.
        return Bootstrap(
            type         = "parametric",
            method       = method,
            switch       = switch,
            process      = self.process,
            hat_adj      = self.hat_adj,
            n_replicates = self.n_replicates,
            seed         = self.seed,
            quantile_ci  = self.quantile_ci,
        ).fit(triangle, target=target)


def resolve_uncertainty(
    uncertainty: Any,
    triangle: "Triangle",
    *,
    target: str,
    method: str,
    switch: Any = None,
) -> "BootstrapTriangle | None":
    """Resolve an ``uncertainty=`` argument to a result or ``None``.

    Accepted forms:

    * ``None`` -> ``None`` (the fit keeps its closed-form analytical SE;
      equivalent to :class:`Analytical`).
    * an :class:`Analytical` / :class:`ResidualBootstrap` /
      :class:`ParametricBootstrap` strategy instance -> delegated to its
      ``_resolve``.
    * a callable ``f(triangle) -> strategy`` -> invoked, then re-resolved.

    ``method`` is the model's own fit paradigm (``"cl"`` / ``"ed"`` /
    ``"sa"``); the strategy is model-agnostic and receives it here.
    """
    if uncertainty is None:
        return None
    if isinstance(uncertainty, (Analytical, ResidualBootstrap, ParametricBootstrap)):
        return uncertainty._resolve(
            triangle, target=target, method=method, switch=switch
        )
    if callable(uncertainty):
        return resolve_uncertainty(
            uncertainty(triangle), triangle, target=target, method=method,
            switch=switch,
        )
    raise TypeError(
        "`uncertainty` must be None, an Analytical / ResidualBootstrap / "
        "ParametricBootstrap strategy, or a callable returning one; got "
        f"{type(uncertainty).__name__}."
    )
