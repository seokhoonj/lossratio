"""Tests for the ChainLadder loss model (Phase 4 P4.2b).

ChainLadder is the all-CL configuration of the shared loss engine; it must
reproduce ``Loss(method="cl")`` byte-for-byte while carrying ``.model ==
"chain_ladder"`` and accepting the ``uncertainty=`` strategy surface.
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
    """Surgery slice of the synthetic experience dataset.

    36 cohorts x 36 durations -- large enough that the `recent` calendar
    wedge meaningfully restricts the contributing cohort set (early
    links lose all but the most-recent diagonals). Read from the same
    fixture the R `recent` parity fixtures were dumped from, so the
    behavioural and parity tests exercise identical input rows.
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
    """Hand-verifiable 5-cohort, 5-duration experience data.

    Resulting loss (cumulative loss) per (cohort, duration):

        cohort       duration_1  duration_2  duration_3  duration_4  duration_5
        2024-01      100    200    320    420    500
        2024-02      150    280    440    570
        2024-03      120    250    380
        2024-04      180    370
        2024-05      200

    Hand-computed Mack quantities (alpha = 1):

        f_1 = (200 + 280 + 250 + 370) / (100 + 150 + 120 + 180)
            = 1100 / 550 = 2.0                                (exact)
        f_2 = (320 + 440 + 380) / (200 + 280 + 250)
            = 1140 / 730 ~ 1.561644
        f_3 = (420 + 570) / (320 + 440)
            = 990 / 760  ~ 1.302632
        f_4 = 500 / 420
            ~ 1.190476                         (n_k = 1, single link sample)

    With n_links = 4, Mack's tail recommendation triggers for
    sigma^2_4 since the last link has only one observation.
    """
    return pl.DataFrame(
        {
            "cy_m": [
                # cohort 2024-01: duration 1..5
                "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                # cohort 2024-02: duration 1..4
                "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                # cohort 2024-03: duration 1..3
                "2024-03-01", "2024-04-01", "2024-05-01",
                # cohort 2024-04: duration 1..2
                "2024-04-01", "2024-05-01",
                # cohort 2024-05: duration 1
                "2024-05-01",
            ],
            "uy_m": [
                "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01",
                "2024-02-01", "2024-02-01", "2024-02-01", "2024-02-01",
                "2024-03-01", "2024-03-01", "2024-03-01",
                "2024-04-01", "2024-04-01",
                "2024-05-01",
            ],
            # Incremental losses chosen so cumulative matches the docstring
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
    fit = lr.ChainLadder().fit(_triangle())
    assert fit.model == "chain_ladder"
    assert fit.method == "cl"


def test_matches_loss_engine() -> None:
    tri = _triangle()
    new = lr.ChainLadder().fit(tri).to_polars()
    old = Loss(method="cl").fit(tri).to_polars()
    assert_frame_equal(new, old)


def test_passthrough_args_match_loss() -> None:
    tri = _triangle()
    new = lr.ChainLadder(recent=12, sigma_method="min_last2").fit(tri).to_polars()
    old = Loss(method="cl", recent=12, sigma_method="min_last2").fit(tri).to_polars()
    assert_frame_equal(new, old)


def test_analytical_default_equivalent() -> None:
    tri = _triangle()
    default = lr.ChainLadder().fit(tri).to_polars()
    explicit = lr.ChainLadder(uncertainty=lr.Analytical()).fit(tri).to_polars()
    assert_frame_equal(default, explicit)


def test_monte_carlo_overlays_bootstrap() -> None:
    tri = _triangle()
    fit = lr.ChainLadder(uncertainty=lr.ParametricBootstrap(seed=42, n_replicates=50)).fit(tri)
    assert fit.ci_type == "bootstrap"
    # The point projection is unchanged by the SE overlay.
    base = lr.ChainLadder().fit(tri).to_polars()
    assert_frame_equal(
        fit.to_polars().select(["coverage", "cohort", "duration", "loss_proj"]),
        base.select(["coverage", "cohort", "duration", "loss_proj"]),
    )


# ---------------------------------------------------------------------------
# Basic structure (ported from test_cl.py, adapted to LossFit)
# ---------------------------------------------------------------------------


def test_cl_output_shape() -> None:
    tri = lr.Triangle(_toy_triangle_input())
    fit = lr.ChainLadder().fit(tri)
    # 5 cohorts x 5 durations = 25 cells (observed + projected)
    assert fit.n_rows == 25


def test_cl_repr() -> None:
    fit = lr.ChainLadder().fit(lr.Triangle(_toy_triangle_input()))
    text = repr(fit)
    assert "LossFit" in text
    assert "cl" in text


def test_cl_alpha_other_raises() -> None:
    # ChainLadder validates alpha at fit time.
    with pytest.raises(NotImplementedError, match="alpha"):
        lr.ChainLadder(alpha=2.0).fit(lr.Triangle(_toy_triangle_input()))


# ---------------------------------------------------------------------------
# Projection of unobserved cells
# ---------------------------------------------------------------------------


def test_cl_projection_observed_cells_unchanged() -> None:
    """loss_proj equals loss_obs in observed cells."""
    fit = lr.ChainLadder().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    observed = df.filter(pl.col("loss_obs").is_not_null())
    diffs = (observed["loss_proj"] - observed["loss_obs"]).abs()
    assert diffs.max() == pytest.approx(0.0)


def test_cl_projection_propagates_via_f_k() -> None:
    """Cohort 2024-05 only has duration 1 observed; subsequent durations are projected
    by successive multiplication of f_1, f_2, f_3, f_4.

    The per-link ATA factors now live on the ATA diagnostic
    (``tri.link().ata()``), not on the loss fit.
    """
    tri = lr.Triangle(_toy_triangle_input())
    fk = tri.link().ata().df.sort("duration")["f"].to_list()
    fit = lr.ChainLadder().fit(tri)
    df = fit.to_polars().sort(["cohort", "duration"])
    cohort_5 = df.filter(pl.col("cohort") == _date("2024-05-01"))
    loss_proj = cohort_5["loss_proj"].to_list()
    assert loss_proj[0] == 200.0
    assert loss_proj[1] == pytest.approx(200.0 * fk[0])
    assert loss_proj[2] == pytest.approx(200.0 * fk[0] * fk[1])
    assert loss_proj[3] == pytest.approx(200.0 * fk[0] * fk[1] * fk[2])
    assert loss_proj[4] == pytest.approx(
        200.0 * fk[0] * fk[1] * fk[2] * fk[3]
    )


# ---------------------------------------------------------------------------
# Mack standard error
# ---------------------------------------------------------------------------


def test_cl_se_observed_cells_null() -> None:
    """Observed cells carry a null projection SE.

    The old CLFit seeded the Mack process / parameter accumulators at 0
    on observed cells; the role-based LossFit leaves the projection SE
    null on observed cells (only projected cells get a numeric SE).
    """
    fit = lr.ChainLadder().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    observed = df.filter(pl.col("loss_obs").is_not_null())
    for col in ("loss_proc_se", "loss_param_se", "loss_total_se"):
        vals = observed[col].to_list()
        assert all(v is None for v in vals), (
            f"column {col!r} should be null on observed cells, got {vals}"
        )


def test_cl_se_proj_positive_for_projected_cells() -> None:
    """All projected cells should carry a positive Mack SE now that
    every link has a non-zero sigma^2 (links 1-3 by sample, link 4
    via Mack's tail rule)."""
    fit = lr.ChainLadder().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    projected = df.filter(pl.col("loss_obs").is_null())
    # Every projected cell carries a numeric SE
    se_values = projected["loss_total_se"].to_list()
    assert all(v is not None and v > 0 for v in se_values)


def test_cl_se_grows_with_distance_from_observed() -> None:
    """For a single cohort, SE on the ultimate should be >= SE on an
    intermediate projected cell (variance accumulates monotonically)."""
    fit = lr.ChainLadder().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars().sort(["cohort", "duration"])
    cohort_5 = df.filter(pl.col("cohort") == _date("2024-05-01"))
    se = cohort_5["loss_total_se"].to_list()
    # se[0] is None (observed). se[1..4] are projected, monotonically
    # non-decreasing in absolute terms because variance accumulates.
    se_proj_only = [v for v in se if v is not None]
    for i in range(1, len(se_proj_only)):
        assert se_proj_only[i] >= se_proj_only[i - 1]


# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------


def test_cl_summary_columns_and_size() -> None:
    """LossFit summary returns role-prefixed ultimate / SE columns."""
    fit = lr.ChainLadder().fit(lr.Triangle(_toy_triangle_input()))
    summary = fit.summary()
    assert set(summary.columns) >= {
        "cohort",
        "loss_proj",
        "loss_total_se",
        "loss_total_cv",
    }
    assert summary.height == 5


def test_cl_summary_ultimate_for_fully_observed_cohort() -> None:
    """Cohort 2024-01 has all 5 durations observed; loss_proj must equal the
    observed loss at duration 5 = 500.0 with a null SE (no projection)."""
    fit = lr.ChainLadder().fit(lr.Triangle(_toy_triangle_input()))
    summary = fit.summary().filter(pl.col("cohort") == _date("2024-01-01"))
    assert summary.height == 1
    assert summary["loss_proj"].to_list()[0] == pytest.approx(500.0)
    # Fully observed cohort: no projection, so SE on ultimate is null.
    assert summary["loss_total_se"].to_list()[0] is None


# ---------------------------------------------------------------------------
# Grouping
# ---------------------------------------------------------------------------


def test_cl_with_group_var() -> None:
    df = _toy_triangle_input().with_columns(pl.lit("SURGERY").alias("coverage"))
    tri = lr.Triangle(df, groups="coverage")
    fit = lr.ChainLadder().fit(tri)
    assert "coverage" in fit.to_polars().columns


def test_cl_groups_fitted_independently() -> None:
    """Two independent groups should each yield identical ATA factors to
    the ungrouped fit on the same data."""
    base = _toy_triangle_input()
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    ata = lr.Triangle(df_grouped, groups="coverage").link().ata().df
    fk_A = ata.filter(pl.col("coverage") == "A").sort("duration")["f"].to_list()
    fk_B = ata.filter(pl.col("coverage") == "B").sort("duration")["f"].to_list()
    assert fk_A == fk_B


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_cl_pandas_input_mirror() -> None:
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(_toy_triangle_input().to_pandas())
    fit = lr.ChainLadder().fit(lr.Triangle(df))
    assert isinstance(fit.df, pd.DataFrame)


def test_cl_chain_with_triangle_constructor() -> None:
    """sklearn-style estimator + Triangle constructor."""
    fit = lr.ChainLadder().fit(lr.Triangle(_toy_triangle_input()))
    assert fit.n_rows == 25


# ---------------------------------------------------------------------------
# recent -- calendar-diagonal wedge factor filter
# ---------------------------------------------------------------------------


def test_cl_recent_changes_proj() -> None:
    """recent=12 restricts factor estimation to the recent diagonal
    wedge: at least one projected loss cell moves, and the projection
    grid (row count) is unchanged."""
    tri = _sur_triangle()
    full = lr.ChainLadder().fit(tri).to_polars().sort(["cohort", "duration"])
    r12 = lr.ChainLadder(recent=12).fit(tri).to_polars().sort(["cohort", "duration"])
    assert full.height == r12.height
    u = full["loss_proj"].to_list()
    f = r12["loss_proj"].to_list()
    assert any(
        a is not None and b is not None and abs(a - b) > 1.0
        for a, b in zip(u, f)
    ), "recent filter did not change any loss_proj cell"


def test_cl_recent_none_is_byte_identical_to_no_arg() -> None:
    """recent=None (the default) reproduces the no-arg fit exactly."""
    tri = _sur_triangle()
    no_arg = lr.ChainLadder().fit(tri).to_polars()
    explicit_none = lr.ChainLadder(recent=None).fit(tri).to_polars()
    assert_frame_equal(no_arg, explicit_none)


def test_cl_recent_larger_than_span_equals_unfiltered() -> None:
    """recent wider than the triangle's diagonal span keeps every link
    -- byte-identical to the unfiltered fit."""
    tri = _sur_triangle()
    # 36 cohorts x 36 durations -> max source calendar index is 35; any
    # `recent` >= that keeps every existing link.
    unfiltered = lr.ChainLadder().fit(tri).to_polars()
    wide = lr.ChainLadder(recent=10_000).fit(tri).to_polars()
    assert_frame_equal(unfiltered, wide)


@pytest.mark.parametrize("bad", [0, -1, 2.5, "x"])
def test_cl_recent_invalid_raises(bad) -> None:
    """Non-positive-integer `recent` is rejected at fit time."""
    tri = lr.Triangle(_toy_triangle_input())
    with pytest.raises(ValueError, match="recent"):
        lr.ChainLadder(recent=bad).fit(tri)
