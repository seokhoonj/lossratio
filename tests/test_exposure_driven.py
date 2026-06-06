"""Tests for the ExposureDriven loss model (Phase 4 P4.2b).

ExposureDriven is the all-ED configuration of the shared loss engine; it must
reproduce ``Loss(method="ed")`` byte-for-byte while carrying ``.model ==
"exposure_driven"`` and accepting the ``uncertainty=`` strategy surface.
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


def _sur_triangle() -> lr.Triangle:
    """Surgery slice of the synthetic experience dataset (36 x 36).

    Read from the same fixture the R `recent` parity fixtures were
    dumped from, so behavioural and parity tests share input rows.
    """
    exp = (
        pl.read_csv(
            _FIXTURES / "experience.csv",
            try_parse_dates=True,
            infer_schema_length=10000,
        )
        .filter(pl.col("coverage") == "surgery")
    )
    return lr.Triangle(exp, groups="coverage")


def _toy_triangle_input() -> pl.DataFrame:
    """A 5x5 toy triangle for hand-verified ED checks.

    Resulting (cohort, dev) cumulative loss:
        2024-01: 100, 200, 320, 420, 500
        2024-02: 150, 280, 440, 570
        2024-03: 120, 250, 380
        2024-04: 180, 370
        2024-05: 200

    Risk premium incr_premium is constant 100 per cell, so cumulative
    premium is 100, 200, 300, ... per cohort regardless of dev attained.
    """
    return pl.DataFrame(
        {
            "cy_m": [
                "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-04-01", "2024-05-01",
                "2024-05-01",
            ],
            "uy_m": [
                "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01",
                "2024-02-01", "2024-02-01", "2024-02-01", "2024-02-01",
                "2024-03-01", "2024-03-01", "2024-03-01",
                "2024-04-01", "2024-04-01",
                "2024-05-01",
            ],
            "incr_loss": [
                100.0, 100.0, 120.0, 100.0, 80.0,
                150.0, 130.0, 160.0, 130.0,
                120.0, 130.0, 130.0,
                180.0, 190.0,
                200.0,
            ],
            "incr_premium": [100.0] * 15,
        }
    )


def _date(s: str) -> pl.Expr:
    return pl.lit(s).cast(pl.Date)


def test_model_name() -> None:
    fit = lr.ExposureDriven().fit(_triangle())
    assert fit.model == "exposure_driven"
    assert fit.method == "ed"


def test_matches_loss_engine() -> None:
    tri = _triangle()
    new = lr.ExposureDriven().fit(tri).to_polars()
    old = Loss(method="ed").fit(tri).to_polars()
    assert_frame_equal(new, old)


def test_passthrough_args_match_loss() -> None:
    tri = _triangle()
    new = lr.ExposureDriven(recent=12).fit(tri).to_polars()
    old = Loss(method="ed", recent=12).fit(tri).to_polars()
    assert_frame_equal(new, old)


def test_analytical_default_equivalent() -> None:
    tri = _triangle()
    default = lr.ExposureDriven().fit(tri).to_polars()
    explicit = lr.ExposureDriven(uncertainty=lr.Analytical()).fit(tri).to_polars()
    assert_frame_equal(default, explicit)


# ---------------------------------------------------------------------------
# Basic structure (ported from the retired test_ed.py ED suite)
# ---------------------------------------------------------------------------


def test_output_shape() -> None:
    fit = lr.ExposureDriven().fit(lr.Triangle(_toy_triangle_input()))
    assert fit.n_rows == 25  # 5 cohorts x 5 devs


def test_repr() -> None:
    fit = lr.ExposureDriven().fit(lr.Triangle(_toy_triangle_input()))
    text = repr(fit)
    assert "LossFit" in text
    assert "ed" in text


def test_alpha_other_raises() -> None:
    # ExposureDriven validates alpha at fit time, not construction.
    with pytest.raises(NotImplementedError, match="alpha"):
        lr.ExposureDriven(alpha=2.0).fit(lr.Triangle(_toy_triangle_input()))


# ---------------------------------------------------------------------------
# Projection of cumulative loss via the additive ED rule
# ---------------------------------------------------------------------------


def test_projection_observed_cells_unchanged() -> None:
    fit = lr.ExposureDriven().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    observed = df.filter(pl.col("loss_obs").is_not_null())
    diffs = (observed["loss_proj"] - observed["loss_obs"]).abs()
    assert diffs.max() == pytest.approx(0.0)


def test_projection_uses_additive_rule() -> None:
    """Cohort 2024-05 has only dev 1 observed (loss = 200, premium = 100).

        loss_proj[1, 2] = loss[1, 1] + g_1 * premium_proj[1, 1]
                          = 200 + 1.375 * 100
                          = 337.5

    The intensity g_1 = 1.375 comes from the dev 1 -> 2 link pooling the
    four cohorts 2024-01..04 (sum Δloss 550 / sum premium 400).
    """
    fit = lr.ExposureDriven().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars().sort(["cohort", "dev"])
    cohort_5 = df.filter(pl.col("cohort") == _date("2024-05-01"))
    loss_proj = cohort_5["loss_proj"].to_list()
    assert loss_proj[0] == 200.0
    assert loss_proj[1] == pytest.approx(200.0 + 1.375 * 100.0)


# ---------------------------------------------------------------------------
# Premium projection appears alongside loss projection
# ---------------------------------------------------------------------------


def test_premium_proj_present_for_all_cells() -> None:
    fit = lr.ExposureDriven().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    # Every cell has a projected premium (observed or chain-ladder forecast)
    assert df["premium_proj"].null_count() == 0


# ---------------------------------------------------------------------------
# Mack-style SE on projected cumulative loss
# ---------------------------------------------------------------------------


def test_se_observed_cells_null() -> None:
    fit = lr.ExposureDriven().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    observed = df.filter(pl.col("loss_obs").is_not_null())
    assert observed["loss_total_se"].null_count() == observed.height


def test_se_proj_positive_for_projected_cells() -> None:
    fit = lr.ExposureDriven().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    projected = df.filter(pl.col("loss_obs").is_null())
    se_values = projected["loss_total_se"].to_list()
    assert all(v is not None and v > 0 for v in se_values)


def test_se_grows_with_distance_from_observed() -> None:
    fit = lr.ExposureDriven().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars().sort(["cohort", "dev"])
    cohort_5 = df.filter(pl.col("cohort") == _date("2024-05-01"))
    se = cohort_5["loss_total_se"].to_list()
    se_proj_only = [v for v in se if v is not None]
    for i in range(1, len(se_proj_only)):
        assert se_proj_only[i] >= se_proj_only[i - 1]


# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------


def test_summary_columns_and_size() -> None:
    """LossFit summary carries role-prefixed loss columns and one row per
    cohort."""
    fit = lr.ExposureDriven().fit(lr.Triangle(_toy_triangle_input()))
    summary = fit.summary()
    assert set(summary.columns) >= {
        "cohort",
        "loss_proj",
        "loss_total_se",
        "loss_total_cv",
    }
    assert summary.height == 5


def test_summary_fully_observed_cohort() -> None:
    fit = lr.ExposureDriven().fit(lr.Triangle(_toy_triangle_input()))
    summary = fit.summary().filter(pl.col("cohort") == _date("2024-01-01"))
    assert summary.height == 1
    assert summary["loss_proj"].to_list()[0] == pytest.approx(500.0)
    se = summary["loss_total_se"].to_list()[0]
    assert se is None or se == pytest.approx(0.0)


# ---------------------------------------------------------------------------
# Grouping
# ---------------------------------------------------------------------------


def test_with_group_var() -> None:
    df = _toy_triangle_input().with_columns(pl.lit("SUR").alias("coverage"))
    fit = lr.ExposureDriven().fit(lr.Triangle(df, groups="coverage"))
    assert "coverage" in fit.to_polars().columns


def test_groups_fitted_independently() -> None:
    base = _toy_triangle_input()
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    fit = lr.ExposureDriven().fit(lr.Triangle(df_grouped, groups="coverage"))
    out = fit.to_polars()
    cols = ["cohort", "dev", "loss_proj", "premium_proj"]
    a = out.filter(pl.col("coverage") == "A").sort(["cohort", "dev"]).select(cols)
    b = out.filter(pl.col("coverage") == "B").sort(["cohort", "dev"]).select(cols)
    assert_frame_equal(a, b)


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_pandas_input_mirror() -> None:
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(_toy_triangle_input().to_pandas())
    fit = lr.ExposureDriven().fit(lr.Triangle(df))
    assert isinstance(fit.df, pd.DataFrame)


# ---------------------------------------------------------------------------
# Contrast with CL
# ---------------------------------------------------------------------------


def test_ed_and_cl_differ_on_same_triangle() -> None:
    """ED's additive projection differs from CL's multiplicative projection
    when individual cohort loss histories differ from the pooled mean."""
    tri = lr.Triangle(_toy_triangle_input())
    cl_fit = lr.ChainLadder().fit(tri)
    ed_fit = lr.ExposureDriven().fit(tri)

    cl_ult = cl_fit.summary().sort("cohort")["loss_proj"].to_list()
    ed_ult = ed_fit.summary().sort("cohort")["loss_proj"].to_list()
    # Numerically distinct (the two models would only coincide in
    # degenerate cases -- here they diverge because cohorts have
    # different observed loss histories at the same exposure).
    assert any(
        abs(c - e) > 1e-6
        for c, e in zip(cl_ult, ed_ult)
        if c is not None and e is not None
    )


# ---------------------------------------------------------------------------
# recent -- calendar-diagonal wedge factor filter
# ---------------------------------------------------------------------------


def test_recent_changes_projection() -> None:
    """recent=12 restricts the ED intensity (and inner premium chain
    ladder) to the recent diagonal wedge, so the projected output differs
    from the unfiltered fit."""
    tri = _sur_triangle()
    full = lr.ExposureDriven().fit(tri).to_polars()
    r12 = lr.ExposureDriven(recent=12).fit(tri).to_polars()
    with pytest.raises(AssertionError):
        assert_frame_equal(full, r12)


def test_recent_none_is_byte_identical_to_no_arg() -> None:
    """recent=None (the default) reproduces the no-arg ED fit exactly."""
    tri = _sur_triangle()
    no_arg = lr.ExposureDriven().fit(tri).to_polars()
    explicit_none = lr.ExposureDriven(recent=None).fit(tri).to_polars()
    assert_frame_equal(no_arg, explicit_none)


def test_recent_larger_than_span_equals_unfiltered() -> None:
    """recent wider than the diagonal span is byte-identical to the
    unfiltered ED fit."""
    tri = _sur_triangle()
    unfiltered = lr.ExposureDriven().fit(tri).to_polars()
    wide = lr.ExposureDriven(recent=10_000).fit(tri).to_polars()
    assert_frame_equal(unfiltered, wide)


@pytest.mark.parametrize("bad", [0, -1, 2.5, "x"])
def test_recent_invalid_raises(bad) -> None:
    """Non-positive-integer `recent` is rejected at fit time."""
    tri = lr.Triangle(_toy_triangle_input())
    with pytest.raises(ValueError, match="recent"):
        lr.ExposureDriven(recent=bad).fit(tri)
