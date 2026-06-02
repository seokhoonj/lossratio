"""Tests for the ChainLadder loss model (Phase 4 P4.2b).

ChainLadder is the all-CL configuration of the shared loss engine; it must
reproduce ``Loss(method="cl")`` byte-for-byte while carrying ``.model ==
"chain_ladder"`` and accepting the ``uncertainty=`` strategy surface.
"""

from pathlib import Path

import polars as pl
from polars.testing import assert_frame_equal

import lossratio as lr


_FIXTURES = Path(__file__).parent / "fixtures"


def _triangle() -> lr.Triangle:
    exp = pl.read_csv(
        _FIXTURES / "experience.csv",
        try_parse_dates=True,
        infer_schema_length=10000,
    )
    return lr.Triangle(exp, groups="coverage")


def test_model_name() -> None:
    fit = lr.ChainLadder().fit(_triangle())
    assert fit.model == "chain_ladder"
    assert fit.method == "cl"


def test_matches_loss_engine() -> None:
    tri = _triangle()
    new = lr.ChainLadder().fit(tri).to_polars()
    old = lr.Loss(method="cl").fit(tri).to_polars()
    assert_frame_equal(new, old)


def test_passthrough_args_match_loss() -> None:
    tri = _triangle()
    new = lr.ChainLadder(recent=12, sigma_method="min_last2").fit(tri).to_polars()
    old = lr.Loss(method="cl", recent=12, sigma_method="min_last2").fit(tri).to_polars()
    assert_frame_equal(new, old)


def test_analytical_default_equivalent() -> None:
    tri = _triangle()
    default = lr.ChainLadder().fit(tri).to_polars()
    explicit = lr.ChainLadder(uncertainty=lr.Analytical()).fit(tri).to_polars()
    assert_frame_equal(default, explicit)


def test_monte_carlo_overlays_bootstrap() -> None:
    tri = _triangle()
    fit = lr.ChainLadder(uncertainty=lr.MonteCarlo(seed=42, B=50)).fit(tri)
    assert fit.ci_type == "bootstrap"
    # The point projection is unchanged by the SE overlay.
    base = lr.ChainLadder().fit(tri).to_polars()
    assert_frame_equal(
        fit.to_polars().select(["coverage", "cohort", "dev", "loss_proj"]),
        base.select(["coverage", "cohort", "dev", "loss_proj"]),
    )
