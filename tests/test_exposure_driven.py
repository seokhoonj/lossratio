"""Tests for the ExposureDriven loss model (Phase 4 P4.2b).

ExposureDriven is the all-ED configuration of the shared loss engine; it must
reproduce ``Loss(method="ed")`` byte-for-byte while carrying ``.model ==
"exposure_driven"`` and accepting the ``uncertainty=`` strategy surface.
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
    fit = lr.ExposureDriven().fit(_triangle())
    assert fit.model == "exposure_driven"
    assert fit.method == "ed"


def test_matches_loss_engine() -> None:
    tri = _triangle()
    new = lr.ExposureDriven().fit(tri).to_polars()
    old = lr.Loss(method="ed").fit(tri).to_polars()
    assert_frame_equal(new, old)


def test_passthrough_args_match_loss() -> None:
    tri = _triangle()
    new = lr.ExposureDriven(recent=12).fit(tri).to_polars()
    old = lr.Loss(method="ed", recent=12).fit(tri).to_polars()
    assert_frame_equal(new, old)


def test_analytical_default_equivalent() -> None:
    tri = _triangle()
    default = lr.ExposureDriven().fit(tri).to_polars()
    explicit = lr.ExposureDriven(uncertainty=lr.Analytical()).fit(tri).to_polars()
    assert_frame_equal(default, explicit)
