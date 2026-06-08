"""Tests for the StageAdaptive loss model (Phase 4 P4.2b).

StageAdaptive is the ED+CL composition configuration of the shared loss
engine; it must reproduce ``Loss(method="sa")`` byte-for-byte while carrying
``.model == "stage_adaptive"``, accepting the ``uncertainty=`` surface, and
rejecting ResidualBootstrap (no coherent single residual pool).
"""

from pathlib import Path

import polars as pl
import pytest
from polars.testing import assert_frame_equal

import lossratio as lr
from lossratio.loss import Loss


_FIXTURES = Path(__file__).parent / "fixtures"


def _triangle() -> lr.Triangle:
    exp = pl.read_csv(
        _FIXTURES / "experience.csv",
        try_parse_dates=True,
        infer_schema_length=10000,
    )
    return lr.Triangle(exp, groups="coverage")


def test_model_name() -> None:
    fit = lr.StageAdaptive().fit(_triangle())
    assert fit.model == "stage_adaptive"
    assert fit.method == "sa"


def test_matches_loss_engine() -> None:
    tri = _triangle()
    new = lr.StageAdaptive().fit(tri).to_polars()
    old = Loss(method="sa").fit(tri).to_polars()
    assert_frame_equal(new, old)


def test_switch_passthrough_matches_loss() -> None:
    tri = _triangle()
    new = lr.StageAdaptive(switch=None).fit(tri).to_polars()
    old = Loss(method="sa", switch=None).fit(tri).to_polars()
    assert_frame_equal(new, old)


def test_analytical_default_equivalent() -> None:
    tri = _triangle()
    default = lr.StageAdaptive().fit(tri).to_polars()
    explicit = lr.StageAdaptive(uncertainty=lr.Analytical()).fit(tri).to_polars()
    assert_frame_equal(default, explicit)


def test_rejects_residual_bootstrap() -> None:
    with pytest.raises(ValueError, match="ResidualBootstrap"):
        lr.StageAdaptive(uncertainty=lr.ResidualBootstrap())


def test_monte_carlo_allowed() -> None:
    tri = _triangle()
    fit = lr.StageAdaptive(uncertainty=lr.ParametricBootstrap(seed=42, n_replicates=50)).fit(tri)
    assert fit.ci_type == "bootstrap"


# ---------------------------------------------------------------------------
# Argument validation -- mirror the ChainLadder / ExposureDriven siblings
# (fail-fast in __post_init__).
# ---------------------------------------------------------------------------


def test_sa_invalid_sigma_method_raises() -> None:
    with pytest.raises(ValueError, match="sigma_method"):
        lr.StageAdaptive(sigma_method="nope")


def test_sa_invalid_conf_level_raises() -> None:
    with pytest.raises(ValueError, match="conf_level"):
        lr.StageAdaptive(conf_level=1.5)


def test_sa_alpha_other_raises() -> None:
    with pytest.raises(NotImplementedError, match="alpha"):
        lr.StageAdaptive(alpha=2.0)


@pytest.mark.parametrize("bad", [0, -1, 2.5, "x"])
def test_sa_recent_invalid_raises(bad) -> None:
    with pytest.raises(ValueError, match="recent"):
        lr.StageAdaptive(recent=bad)
