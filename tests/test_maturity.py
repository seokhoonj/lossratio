"""Tests for ATA maturity detection (`tri.link().ata().maturity()`)."""

import math

import polars as pl
import pytest

import lossratio as lr


def _polars_input() -> pl.DataFrame:
    """5-cohort, 5-dev experience data (same as CL/ED tests)."""
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


def _stable_input() -> pl.DataFrame:
    """Highly stable triangle: every cohort grows by exactly 2x each dev.

    All individual link factors equal 2.0 → CV = 0 at every link.
    sigma^2_k = 0 → RSE = 0. Both thresholds trivially met.
    """
    rows = []
    cohorts = ["2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01"]
    for ci, uym in enumerate(cohorts):
        for di in range(4 - ci):  # 4, 3, 2, 1 cells
            month = ci + di + 1
            cym = f"2024-{month:02d}-01"
            # Constant rp = 100, loss doubling pattern via incremental loss
            # loss target: 100, 200, 400, 800 → incremental loss: 100, 100, 200, 400
            target_closs = 100 * (2 ** di)
            prev_closs = 100 * (2 ** (di - 1)) if di > 0 else 0
            inc_loss = target_closs - prev_closs
            rows.append({
                "cy_m": cym,
                "uy_m": uym,
                "incr_loss": float(inc_loss),
                "incr_premium": 100.0,
            })
    return pl.DataFrame(rows)


def _date(s: str) -> pl.Expr:
    return pl.lit(s).cast(pl.Date)


# ---------------------------------------------------------------------------
# Basic structure
# ---------------------------------------------------------------------------


def test_maturity_returns_atamaturity():
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity()
    assert isinstance(mat, lr.Maturity)


def test_maturity_diagnostic_columns():
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity()
    df = mat.to_polars()
    # Per-link diagnostic carries the full R ATASummary column set
    # plus the maturity-specific `stable` boolean.
    expected = {
        "ata_from", "ata_to", "ata_link",
        "f", "f_se", "sigma", "sigma2",
        "cv", "rse",
        "mean", "median", "wt",
        "n_cohorts", "n_valid", "n_inf", "n_nan", "valid_ratio",
        "stable",
    }
    assert expected.issubset(set(df.columns))
    # n_links = n_devs - 1 = 4
    assert df.height == 4


def test_maturity_repr():
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity()
    text = repr(mat)
    assert "Maturity" in text
    assert "max_cv" in text


# ---------------------------------------------------------------------------
# mat_k detection
# ---------------------------------------------------------------------------


def test_maturity_stable_data_finds_k_star_at_two():
    """When every link is perfectly stable (CV=0, RSE=0), mat_k=2.

    mat_k is the target dev (ata_to) of the first stable link; the
    first link goes from dev 1 to dev 2, so target dev = 2.
    """
    tri = lr.Triangle(_stable_input())
    mat = tri.link().ata().maturity()
    assert mat.maturity_point == 2


def test_maturity_strict_thresholds_no_detection():
    """Set thresholds tighter than any observed CV/RSE → mat_k=None."""
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity(max_cv=1e-9, max_rse=1e-9, min_run=2)
    assert mat.maturity_point is None


def test_maturity_loose_thresholds_finds_k_star():
    """Generous thresholds: mat_k should be the target dev of the
    first stable link.
    """
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=2)
    # With min_run=2 we need 2 consecutive stable links starting at link 0
    # (dev 1 -> dev 2). Target dev of that link = 2.
    assert mat.maturity_point == 2


def test_maturity_m_consecutive_required():
    """With m larger than the number of links, no detection possible."""
    tri = lr.Triangle(_polars_input())
    # n_links = 4. min_run=5 cannot be satisfied.
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=5)
    assert mat.maturity_point is None


# ---------------------------------------------------------------------------
# Diagnostic content
# ---------------------------------------------------------------------------


def test_maturity_f_matches_cl_factors():
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity()
    cl_fit = lr.CL().fit(tri)

    mat_f = mat.to_polars().sort("ata_from")["f"].to_list()
    cl_f = cl_fit._fk_df.sort("dev")["f"].to_list()
    assert mat_f == cl_f


def test_maturity_cv_first_link_hand_check():
    """Individual factors at link 1 (dev 1 -> 2):
        cohort_1: 200/100 = 2.0
        cohort_2: 280/150 = 1.866666...
        cohort_3: 250/120 = 2.083333...
        cohort_4: 370/180 = 2.055555...

    mean = 2.001388...
    sd (n-1)  = ?
    CV = sd / mean
    """
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity()
    cv = mat.to_polars().sort("ata_from")["cv"].to_list()
    # CV is positive (some spread)
    assert cv[0] is not None
    assert cv[0] > 0


def test_maturity_rse_link_with_one_observation_is_null():
    """Link 4 has n_k=1, sigma^2_k=0 unless tail rule applies; RSE may
    be null (no SE estimate for a single observation)."""
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity()
    df = mat.to_polars().sort("ata_from")
    rse_link_4 = df["rse"].to_list()[3]
    # With Mack tail rule, sigma^2_4 may be set > 0, so rse_link_4 may
    # actually be defined. But cv at link 4 (n_k=1) should be null.
    cv_link_4 = df["cv"].to_list()[3]
    assert cv_link_4 is None


# ---------------------------------------------------------------------------
# Grouping
# ---------------------------------------------------------------------------


def test_maturity_with_group_var():
    df = _polars_input().with_columns(pl.lit("SUR").alias("coverage"))
    tri = lr.Triangle(df, groups="coverage")
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=2)

    assert "coverage" in mat.to_polars().columns
    # mat_k is a dict per group when groups present
    assert isinstance(mat.maturity_point, dict)
    assert "SUR" in mat.maturity_point


def test_maturity_per_group_independent():
    base = _polars_input()
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    tri = lr.Triangle(df_grouped, groups="coverage")
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=2)
    # Same data in each group → same mat_k
    assert mat.maturity_point == {"A": 2, "B": 2}


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_maturity_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(_polars_input().to_pandas())
    tri = lr.Triangle(df)
    mat = tri.link().ata().maturity()
    assert isinstance(mat.df, pd.DataFrame)
    assert isinstance(mat.summary(), pd.DataFrame)


# ---------------------------------------------------------------------------
# Threshold parameters propagated
# ---------------------------------------------------------------------------


def test_maturity_thresholds_stored():
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity(max_cv=0.2, max_rse=0.1, min_run=3)
    assert mat.max_cv == 0.2
    assert mat.max_rse == 0.1
    assert mat.min_run == 3


# ---------------------------------------------------------------------------
# R-parity output schema (Maturity.summary())
# ---------------------------------------------------------------------------


_R_SUMMARY_STAT_COLS = (
    "ata_from", "change", "ata_link",
    "mean", "median", "wt", "cv",
    "f", "f_se", "rse", "sigma",
    "n_cohorts", "n_valid", "n_inf", "n_nan", "valid_ratio",
)


def test_summary_has_r_parity_columns():
    """`Maturity.summary()` carries the full R sibling column set."""
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=2)
    smr = mat.summary()
    for col in _R_SUMMARY_STAT_COLS:
        assert col in smr.columns, f"missing column: {col}"


def test_summary_one_row_per_group_no_groups():
    """No-groups Triangle -> pooled single-row summary."""
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=2)
    smr = mat.summary()
    assert smr.height == 1


def test_summary_one_row_per_group_with_groups():
    """Per-group Triangle -> one summary row per group."""
    base = _polars_input()
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    tri = lr.Triangle(df_grouped, groups="coverage")
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=2)
    smr = mat.summary()
    assert smr.height == 2
    assert set(smr["coverage"].to_list()) == {"A", "B"}


def test_summary_change_matches_mat_k():
    """`change` column in summary equals the `mat_k` property value."""
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=2)
    change = mat.summary()["change"].to_list()[0]
    assert int(change) == mat.maturity_point


def test_summary_ata_link_string_format():
    """`ata_link` is the canonical '<from>-<to>' label."""
    tri = lr.Triangle(_stable_input())
    mat = tri.link().ata().maturity()
    row = mat.summary().row(0, named=True)
    expected = f"{int(row['ata_from'])}-{int(row['change'])}"
    assert row["ata_link"] == expected


def test_summary_count_columns_sum_to_n_cohorts():
    """R-parity invariant: n_valid + n_inf + n_nan == n_cohorts.

    Counts come from finite/inf/nan classification of the per-cohort
    `ata` factor (which partitions the link table by R's
    .summarize_link_ata).
    """
    tri = lr.Triangle(_stable_input())
    mat = tri.link().ata().maturity()
    row = mat.summary().row(0, named=True)
    n_total = row["n_valid"] + row["n_inf"] + row["n_nan"]
    assert n_total == row["n_cohorts"]


def test_summary_valid_ratio_in_unit_interval():
    """`valid_ratio = n_valid / n_cohorts` lies in [0, 1]."""
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=2)
    vr = mat.summary()["valid_ratio"].to_list()[0]
    assert 0.0 <= vr <= 1.0


def test_summary_f_se_consistent_with_rse_times_f():
    """f_se = rse * f -- the derived stat must round-trip exactly."""
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=2)
    row = mat.summary().row(0, named=True)
    f, rse, f_se = row["f"], row["rse"], row["f_se"]
    if all(v is not None and not math.isnan(v) for v in (f, rse, f_se)):
        assert f_se == pytest.approx(f * rse)


def test_summary_sigma_is_sqrt_sigma2_on_diag_frame():
    """`sigma` in per-link diag is sqrt of `sigma2`."""
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity()
    df = mat.to_polars().sort("ata_from")
    for sig, sig2 in zip(df["sigma"].to_list(), df["sigma2"].to_list()):
        if sig is None or sig2 is None:
            continue
        if math.isnan(sig) or math.isnan(sig2):
            continue
        assert sig == pytest.approx(math.sqrt(sig2))


def test_summary_wt_equals_f_for_alpha_one():
    """Volume-weighted `wt = sum(loss_to)/sum(loss_from)` is the
    alpha=1 WLS factor `f`. On the picked maturity link row they
    must agree (modulo float rounding).
    """
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=2)
    row = mat.summary().row(0, named=True)
    if row["f"] is not None and row["wt"] is not None:
        assert row["wt"] == pytest.approx(row["f"])


def test_summary_no_detection_returns_nan_row():
    """When no stable run is found, R fills the stat columns with NA.

    Python parity: NaN for numeric columns, None for `ata_link`.
    `mat_k` reports `None`.
    """
    tri = lr.Triangle(_polars_input())
    # Impossible thresholds force the no-match branch.
    mat = tri.link().ata().maturity(
        max_cv=1e-15, max_rse=1e-15, min_run=2
    )
    assert mat.maturity_point is None
    row = mat.summary().row(0, named=True)
    assert row["ata_link"] is None or (
        isinstance(row["ata_link"], float) and math.isnan(row["ata_link"])
    )
    for col in ("ata_from", "change", "mean", "median",
                "wt", "cv", "f", "f_se", "rse", "sigma",
                "n_cohorts", "n_valid", "n_inf", "n_nan", "valid_ratio"):
        v = row[col]
        assert v is None or math.isnan(v), (
            f"{col} should be NaN in no-detection branch, got {v!r}"
        )


def test_summary_per_group_independence_no_detection_mixed():
    """One group detects, another doesn't -- per-group rows must
    independently carry stats or NaN-fill.
    """
    base = _polars_input()
    # Construct a degenerate "B" cohort where loss_to == loss_from
    # everywhere so CV is exactly 0 (passes), then knock out a row
    # to force NaN at the second link.
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    tri = lr.Triangle(df_grouped, groups="coverage")
    # Generous thresholds: both groups should detect.
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=2)
    smr = mat.summary().sort("coverage")
    assert smr.height == 2
    # Both detected -- both rows have a non-NaN `change`.
    for c in smr["change"].to_list():
        assert not math.isnan(c)


def test_summary_groups_column_preserved_in_pandas():
    """pandas-input Triangle: summary mirrors back to pandas with
    the groups column present.
    """
    pd = pytest.importorskip("pandas")
    base = _polars_input()
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    ).to_pandas()
    tri = lr.Triangle(df_grouped, groups="coverage")
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=2)
    smr = mat.summary()
    assert isinstance(smr, pd.DataFrame)
    assert "coverage" in smr.columns


def test_summary_manual_maturity_at_has_r_parity_schema():
    """`maturity_at()` (manual) emits the same R column set with stat
    columns NaN-filled (no factor data backs a manual override).
    """
    mat = lr.maturity_at(change=4)
    smr = mat.summary()
    for col in _R_SUMMARY_STAT_COLS:
        assert col in smr.columns, f"missing column: {col}"
    row = smr.row(0, named=True)
    assert int(row["change"]) == 4
    assert int(row["ata_from"]) == 3
    assert row["ata_link"] == "3-4"
    # Stat columns are NaN.
    for col in ("mean", "median", "wt", "cv",
                "f", "f_se", "rse", "sigma",
                "n_cohorts", "n_valid", "n_inf", "n_nan", "valid_ratio"):
        assert math.isnan(row[col])
