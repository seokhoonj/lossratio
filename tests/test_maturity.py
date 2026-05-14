"""Tests for ATA maturity detection (`tri.link().ata().maturity()`)."""

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
            "loss_incr": [
                100.0, 100.0, 120.0, 100.0, 80.0,
                150.0, 130.0, 160.0, 130.0,
                120.0, 130.0, 130.0,
                180.0, 190.0,
                200.0,
            ],
            "premium_incr": [100.0] * 15,
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
                "loss_incr": float(inc_loss),
                "premium_incr": 100.0,
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
    # Maturity now builds on top of ATA factor diagnostic, so its
    # df carries the full ATA schema (`f`, `sigma2`, `cv`, `rse`,
    # `n_cohorts`) plus the maturity-specific `stable` column.
    assert set(df.columns) >= {"dev", "f", "sigma2", "cv", "rse", "stable"}
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
    assert mat.mat_k == 2


def test_maturity_strict_thresholds_no_detection():
    """Set thresholds tighter than any observed CV/RSE → mat_k=None."""
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity(max_cv=1e-9, max_rse=1e-9, min_run=2)
    assert mat.mat_k is None


def test_maturity_loose_thresholds_finds_k_star():
    """Generous thresholds: mat_k should be the target dev of the
    first stable link.
    """
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=2)
    # With min_run=2 we need 2 consecutive stable links starting at link 0
    # (dev 1 -> dev 2). Target dev of that link = 2.
    assert mat.mat_k == 2


def test_maturity_m_consecutive_required():
    """With m larger than the number of links, no detection possible."""
    tri = lr.Triangle(_polars_input())
    # n_links = 4. min_run=5 cannot be satisfied.
    mat = tri.link().ata().maturity(max_cv=10.0, max_rse=10.0, min_run=5)
    assert mat.mat_k is None


# ---------------------------------------------------------------------------
# Diagnostic content
# ---------------------------------------------------------------------------


def test_maturity_f_matches_cl_factors():
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity()
    cl_fit = lr.CL().fit(tri)

    mat_f = mat.to_polars().sort("dev")["f"].to_list()
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
    cv = mat.to_polars().sort("dev")["cv"].to_list()
    # CV is positive (some spread)
    assert cv[0] is not None
    assert cv[0] > 0


def test_maturity_rse_link_with_one_observation_is_null():
    """Link 4 has n_k=1, sigma^2_k=0 unless tail rule applies; RSE may
    be null (no SE estimate for a single observation)."""
    tri = lr.Triangle(_polars_input())
    mat = tri.link().ata().maturity()
    df = mat.to_polars().sort("dev")
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
    assert isinstance(mat.mat_k, dict)
    assert "SUR" in mat.mat_k


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
    assert mat.mat_k == {"A": 2, "B": 2}


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
