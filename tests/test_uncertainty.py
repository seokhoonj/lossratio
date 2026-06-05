"""Tests for the public uncertainty strategy surface (Phase 4 P4.1).

The three strategy classes (Analytical / ResidualBootstrap /
ParametricBootstrap) are the public uncertainty config; the engine stays
in ``bootstrap.py`` behind them. These tests pin that each strategy
delegates to the equivalent engine configuration byte-for-byte (same seed
-> identical summary), that Analytical means "no overlay" (closed-form),
and that ``resolve_uncertainty`` dispatches None / strategy / callable.
"""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr
from lossratio.bootstrap import Bootstrap
from lossratio.uncertainty import (
    Analytical,
    ParametricBootstrap,
    ResidualBootstrap,
    resolve_uncertainty,
)

B = 50
SEED = 20260602


def _tri() -> lr.Triangle:
    return lr.Triangle(lr.load_experience(), groups="coverage")


# ---------------------------------------------------------------------------
# Analytical = closed-form, no overlay (resolves to None)
# ---------------------------------------------------------------------------


def test_analytical_resolves_to_none():
    tri = _tri()
    assert Analytical()._resolve(tri, target="loss", method="cl") is None


def test_resolve_none_is_none():
    tri = _tri()
    assert resolve_uncertainty(None, tri, target="loss", method="cl") is None
    assert (
        resolve_uncertainty(Analytical(), tri, target="loss", method="cl")
        is None
    )


# ---------------------------------------------------------------------------
# ResidualBootstrap == engine type="nonparametric"
# ---------------------------------------------------------------------------


def test_residual_bootstrap_matches_engine():
    tri = _tri()
    strat = ResidualBootstrap(
        residual="cell", process="gamma", n_replicates=B, seed=SEED
    )._resolve(tri, target="loss", method="cl")
    engine = Bootstrap(
        type="nonparametric",
        method="cl",
        residual="cell",
        process="gamma",
        n_replicates=B,
        seed=SEED,
    ).fit(tri, target="loss")
    assert strat.summary.equals(engine.summary)


# ---------------------------------------------------------------------------
# ParametricBootstrap == engine type="parametric"
# ---------------------------------------------------------------------------


def test_parametric_bootstrap_matches_engine():
    tri = _tri()
    strat = ParametricBootstrap(process="gamma", n_replicates=B, seed=SEED)._resolve(
        tri, target="loss", method="cl"
    )
    engine = Bootstrap(
        type="parametric", method="cl", process="gamma", n_replicates=B, seed=SEED
    ).fit(tri, target="loss")
    assert strat.summary.equals(engine.summary)


# ---------------------------------------------------------------------------
# Analytical(simulate=True) == engine type="analytical" (Mack factor draw)
# ---------------------------------------------------------------------------


def test_analytical_simulate_matches_analytical_engine():
    tri = _tri()
    strat = Analytical(
        simulate=True, n_replicates=B, seed=SEED
    )._resolve(tri, target="loss", method="cl")
    engine = Bootstrap(
        type="analytical", method="cl", process="normal", n_replicates=B, seed=SEED
    ).fit(tri, target="loss")
    assert strat.summary.equals(engine.summary)


# ---------------------------------------------------------------------------
# resolve_uncertainty dispatch
# ---------------------------------------------------------------------------


def test_resolve_callable_form():
    tri = _tri()
    direct = ParametricBootstrap(process="gamma", n_replicates=B, seed=SEED)._resolve(
        tri, target="loss", method="cl"
    )
    via_callable = resolve_uncertainty(
        lambda t: ParametricBootstrap(process="gamma", n_replicates=B, seed=SEED),
        tri,
        target="loss",
        method="cl",
    )
    assert via_callable.summary.equals(direct.summary)


def test_resolve_bad_type_raises():
    tri = _tri()
    with pytest.raises(TypeError, match="must be None"):
        resolve_uncertainty("auto", tri, target="loss", method="cl")
