"""Tests for the CL (Mack chain ladder) estimator."""

import math
from pathlib import Path

import polars as pl
import pytest
from polars.testing import assert_frame_equal

import lossratio as lr


_FIXTURES = Path(__file__).parent / "fixtures"


def _sur_triangle() -> lr.Triangle:
    """Surgery slice of the synthetic experience dataset.

    36 cohorts x 36 devs -- large enough that the `recent` calendar
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
    """Hand-verifiable 5-cohort, 5-dev experience data.

    Resulting loss (cumulative loss) per (cohort, dev):

        cohort       dev_1  dev_2  dev_3  dev_4  dev_5
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
                # cohort 2024-01: dev 1..5
                "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                # cohort 2024-02: dev 1..4
                "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                # cohort 2024-03: dev 1..3
                "2024-03-01", "2024-04-01", "2024-05-01",
                # cohort 2024-04: dev 1..2
                "2024-04-01", "2024-05-01",
                # cohort 2024-05: dev 1
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


# ---------------------------------------------------------------------------
# Basic structure
# ---------------------------------------------------------------------------


def test_cl_output_shape():
    tri = lr.Triangle(_toy_triangle_input())
    fit = lr.CL().fit(tri)
    # 5 cohorts x 5 devs = 25 cells (observed + projected)
    assert fit.n_rows == 25


def test_cl_repr():
    fit = lr.CL().fit(lr.Triangle(_toy_triangle_input()))
    text = repr(fit)
    assert "CLFit" in text
    assert "alpha=1" in text


def test_cl_alpha_other_raises():
    with pytest.raises(NotImplementedError, match="alpha"):
        lr.CL(alpha=2.0)


# ---------------------------------------------------------------------------
# ATA factors
# ---------------------------------------------------------------------------


def test_cl_ata_factor_first_link_exact():
    """f_1 was constructed to come out as 2.0 exactly."""
    fit = lr.CL().fit(lr.Triangle(_toy_triangle_input()))
    fk = fit._fk_df.sort("dev")
    assert fk["f"].to_list()[0] == pytest.approx(2.0)


def test_cl_ata_factors_subsequent_links():
    fit = lr.CL().fit(lr.Triangle(_toy_triangle_input()))
    fk = fit._fk_df.sort("dev")
    factors = fk["f"].to_list()
    assert factors[1] == pytest.approx(1140 / 730)
    assert factors[2] == pytest.approx(990 / 760)
    assert factors[3] == pytest.approx(500 / 420)


# ---------------------------------------------------------------------------
# sigma^2 estimation across multiple links
# ---------------------------------------------------------------------------


def test_cl_sigma2_first_three_links_positive():
    """Links 1-3 each have n_k >= 2, so sigma^2_k > 0."""
    fit = lr.CL().fit(lr.Triangle(_toy_triangle_input()))
    fk = fit._fk_df.sort("dev")
    sigma2 = fk["sigma2"].to_list()
    assert sigma2[0] > 0
    assert sigma2[1] > 0
    assert sigma2[2] > 0


def test_cl_sigma2_last_link_uses_locf_tail():
    """Link 4 has n_k = 1, so sigma^2_4 is filled by the tail
    extrapolation. With the default sigma_method = "locf" the last
    finite positive sigma2 is carried forward."""
    fit = lr.CL().fit(lr.Triangle(_toy_triangle_input()))
    fk = fit._fk_df.sort("dev")
    sigma2 = fk["sigma2"].to_list()
    s2 = sigma2[3]
    assert s2 is not None
    assert s2 > 0
    # locf: most recent finite positive sigma2 — sigma2[2]
    expected = sigma2[2]
    assert s2 == pytest.approx(expected, rel=1e-6)


def test_cl_sigma2_last_link_min_last2():
    """sigma_method = 'min_last2' falls back to min(s_{k-1}, s_{k-2})."""
    fit = lr.CL(sigma_method="min_last2").fit(lr.Triangle(_toy_triangle_input()))
    fk = fit._fk_df.sort("dev")
    sigma2 = fk["sigma2"].to_list()
    expected = min(sigma2[2], sigma2[1])
    assert sigma2[3] == pytest.approx(expected, rel=1e-6)


# ---------------------------------------------------------------------------
# Projection of unobserved cells
# ---------------------------------------------------------------------------


def test_cl_projection_observed_cells_unchanged():
    """loss_proj equals loss_obs in observed cells."""
    fit = lr.CL().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    observed = df.filter(pl.col("loss_obs").is_not_null())
    diffs = (observed["loss_proj"] - observed["loss_obs"]).abs()
    assert diffs.max() == pytest.approx(0.0)


def test_cl_projection_propagates_via_f_k():
    """Cohort 2024-05 only has dev 1 observed; subsequent devs are projected
    by successive multiplication of f_1, f_2, f_3, f_4."""
    fit = lr.CL().fit(lr.Triangle(_toy_triangle_input()))
    fk = fit._fk_df.sort("dev")["f"].to_list()
    df = fit.to_polars().sort(["cohort", "dev"])
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


def test_cl_se_observed_cells_zero():
    """Observed cells carry SE = 0 (Mack additive recursion seeds the
    process / parameter accumulators at 0 at the last observed dev).

    R parity: ``fit_cl()$full`` reports ``loss_proc_se = loss_param_se =
    loss_total_se = 0`` on observed cells, not ``NA``. The Python
    decomposition mirrors that.
    """
    fit = lr.CL().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    observed = df.filter(pl.col("loss_obs").is_not_null())
    for col in ("loss_proc_se", "loss_param_se", "loss_total_se"):
        vals = observed[col].to_list()
        assert all(v == 0.0 for v in vals), (
            f"column {col!r} should be 0 on observed cells, got {vals}"
        )


def test_cl_se_proj_positive_for_projected_cells():
    """All projected cells should carry a positive Mack SE now that
    every link has a non-zero sigma^2 (links 1-3 by sample, link 4
    via Mack's tail rule)."""
    fit = lr.CL().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars()
    projected = df.filter(pl.col("loss_obs").is_null())
    # Every projected cell carries a numeric SE
    se_values = projected["loss_total_se"].to_list()
    assert all(v is not None and v > 0 for v in se_values)


def test_cl_se_grows_with_distance_from_observed():
    """For a single cohort, SE on the ultimate should be >= SE on an
    intermediate projected cell (variance accumulates monotonically)."""
    fit = lr.CL().fit(lr.Triangle(_toy_triangle_input()))
    df = fit.to_polars().sort(["cohort", "dev"])
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


def test_cl_summary_columns_and_size():
    """R parity: `summary.CLFit` returns role-prefixed columns
    (`loss_ult`, `loss_total_se`, `loss_total_cv`) and a `latest` /
    `reserve` pair, not generic `ultimate*` columns."""
    fit = lr.CL().fit(lr.Triangle(_toy_triangle_input()))
    summary = fit.summary()
    assert set(summary.columns) >= {
        "cohort",
        "latest",
        "loss_ult",
        "reserve",
        "loss_proc_se",
        "loss_param_se",
        "loss_total_se",
        "loss_total_cv",
    }
    assert summary.height == 5


def test_cl_summary_ultimate_for_fully_observed_cohort():
    """Cohort 2024-01 has all 5 devs observed; loss_ult must equal the
    observed loss at dev 5 = 500.0 with SE = 0 (no projection)."""
    fit = lr.CL().fit(lr.Triangle(_toy_triangle_input()))
    summary = fit.summary().filter(pl.col("cohort") == _date("2024-01-01"))
    assert summary.height == 1
    assert summary["loss_ult"].to_list()[0] == pytest.approx(500.0)
    se = summary["loss_total_se"].to_list()[0]
    # Fully observed cohort: SE on ultimate is 0 (Mack recursion seeded
    # at last observed dev; R parity).
    assert se == pytest.approx(0.0)


# ---------------------------------------------------------------------------
# Grouping
# ---------------------------------------------------------------------------


def test_cl_with_group_var():
    df = _toy_triangle_input().with_columns(pl.lit("SUR").alias("coverage"))
    tri = lr.Triangle(df, groups="coverage")
    fit = lr.CL().fit(tri)
    assert "coverage" in fit.to_polars().columns
    assert "coverage" in fit._fk_df.columns


def test_cl_groups_fitted_independently():
    """Two independent groups should each yield identical f_k to the
    ungrouped fit on the same data."""
    base = _toy_triangle_input()
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    fit = lr.CL().fit(lr.Triangle(df_grouped, groups="coverage"))
    fk_A = fit._fk_df.filter(pl.col("coverage") == "A").sort("dev")["f"].to_list()
    fk_B = fit._fk_df.filter(pl.col("coverage") == "B").sort("dev")["f"].to_list()
    assert fk_A == fk_B


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_cl_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(_toy_triangle_input().to_pandas())
    fit = lr.CL().fit(lr.Triangle(df))
    assert isinstance(fit.df, pd.DataFrame)
    assert isinstance(fit.f_k, pd.DataFrame)


def test_cl_chain_with_triangle_constructor():
    """sklearn-style estimator + Triangle constructor."""
    fit = lr.CL().fit(lr.Triangle(_toy_triangle_input()))
    assert fit.n_rows == 25


# ---------------------------------------------------------------------------
# recent — calendar-diagonal wedge factor filter
# ---------------------------------------------------------------------------


def test_cl_recent_changes_factors():
    """recent=12 restricts factor estimation to the recent diagonal
    wedge: early-link factors differ from the unfiltered fit and the
    per-link n_cohorts drops to the recent window."""
    tri = _sur_triangle()
    full = lr.CL().fit(tri)._fk_df.sort("dev")
    r12 = lr.CL(recent=12).fit(tri)._fk_df.sort("dev")

    # Early links (lots of cohorts dropped) must have different f.
    f_full = full["f"].to_list()
    f_r12 = r12["f"].to_list()
    assert f_full[0] != pytest.approx(f_r12[0]), (
        "first-link factor unchanged by recent filter"
    )
    assert f_full[1] != pytest.approx(f_r12[1])
    # The two fits have the same link set (projection grid unchanged).
    assert full.height == r12.height


def test_cl_recent_drops_n_cohorts_per_link():
    """The per-link contributing cohort count drops to the recent
    window for early links (the ATA diagnostic exposes n_cohorts)."""
    tri = _sur_triangle()
    full = tri.link().ata().df.sort("dev")
    r12 = tri.link().ata(recent=12).df.sort("dev")

    # Unfiltered: early links pool many cohorts; recent=12 caps it.
    assert full["n_cohorts"][0] > 12
    assert r12["n_cohorts"][0] == 12
    # Every early link inside the wedge collapses to exactly 12.
    early = r12.filter(pl.col("dev") <= 24)
    assert all(n == 12 for n in early["n_cohorts"].to_list())


def test_cl_recent_none_is_byte_identical_to_no_arg():
    """recent=None (the default) reproduces the no-arg fit exactly."""
    tri = _sur_triangle()
    no_arg = lr.CL().fit(tri).to_polars()
    explicit_none = lr.CL(recent=None).fit(tri).to_polars()
    assert_frame_equal(no_arg, explicit_none)
    # f_k table identical too.
    assert_frame_equal(
        lr.CL().fit(tri)._fk_df, lr.CL(recent=None).fit(tri)._fk_df
    )


def test_cl_recent_larger_than_span_equals_unfiltered():
    """recent wider than the triangle's diagonal span keeps every link
    -- byte-identical to the unfiltered fit."""
    tri = _sur_triangle()
    # 36 cohorts x 36 devs -> max source calendar index is 35; any
    # `recent` >= that keeps every existing link.
    unfiltered = lr.CL().fit(tri).to_polars()
    wide = lr.CL(recent=10_000).fit(tri).to_polars()
    assert_frame_equal(unfiltered, wide)


@pytest.mark.parametrize("bad", [0, -1, 2.5, "x"])
def test_cl_recent_invalid_raises(bad):
    """Non-positive-integer `recent` is rejected at construction."""
    with pytest.raises(ValueError, match="recent"):
        lr.CL(recent=bad)


def test_cl_recent_invalid_raises_on_link_ata():
    """The Link.ata() diagnostic validates `recent` the same way."""
    tri = _sur_triangle()
    with pytest.raises(ValueError, match="recent"):
        tri.link().ata(recent=0)


@pytest.mark.parametrize(
    "estimator_factory",
    [
        lambda r: lr.Loss(recent=r),
        lambda r: lr.Ratio(recent=r),
        lambda r: lr.BF(prior=1.5, recent=r),
        lambda r: lr.CC(recent=r),
    ],
    ids=["Loss", "Ratio", "BF", "CC"],
)
def test_cl_recent_threads_through_dispatchers(estimator_factory):
    """`recent` runs and shifts the fit on every loss-side estimator
    that exposes it (Loss / Ratio / BF / CC)."""
    tri = _sur_triangle()
    unfiltered = (
        estimator_factory(None).fit(tri).to_polars().sort(["cohort", "dev"])
    )
    filtered = (
        estimator_factory(12).fit(tri).to_polars().sort(["cohort", "dev"])
    )
    assert unfiltered.height == filtered.height
    u = unfiltered["loss_proj"].to_list()
    f = filtered["loss_proj"].to_list()
    # At least one projected cell must move once the wedge is applied.
    assert any(
        a is not None and b is not None and abs(a - b) > 1.0
        for a, b in zip(u, f)
    ), "recent filter did not change any loss_proj cell"


def test_cl_recent_on_link_diagnostics():
    """`recent` works on both Link.ata() and Link.intensity()."""
    tri = _sur_triangle()
    link = tri.link()
    ata_full = link.ata().df.sort("dev")
    ata_r12 = link.ata(recent=12).df.sort("dev")
    inten_full = link.intensity().df.sort("dev")
    inten_r12 = link.intensity(recent=12).df.sort("dev")

    # Same link set, but first-link factors / intensities shift.
    assert ata_full.height == ata_r12.height
    assert inten_full.height == inten_r12.height
    assert ata_full["f"][0] != pytest.approx(ata_r12["f"][0])
    assert inten_full["g"][0] != pytest.approx(inten_r12["g"][0])
    assert inten_r12["n_cohorts"][0] == 12
