"""R parity tests.

The fixtures in ``tests/fixtures/*.csv`` are dumped from the R sibling
(`Rscript dev/parity_dump.R` in the R repo). Each test loads the
R-side fixture, runs the equivalent Python call on the same input
rows, and asserts column-by-column numerical agreement to a tight
tolerance.

These tests guard against silent algorithmic drift between the two
languages. To refresh fixtures after a deliberate algorithm change,
re-run the R dump script and copy the new CSVs here.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
import pytest

import lossratio as lr

FIXTURES = Path(__file__).parent / "fixtures"
ATOL = 1e-6
RTOL = 1e-9


def _load(name: str) -> pl.DataFrame:
    fp = FIXTURES / f"{name}.csv"
    if not fp.exists():
        pytest.skip(f"missing R fixture: {fp.name}")
    # data.table::fwrite output. Bumped schema length for mixed-magnitude
    # numeric columns and try_parse_dates to materialise cohort / change
    # Date columns rather than leaving them as strings.
    return pl.read_csv(fp, try_parse_dates=True, infer_schema_length=10000)


def _exp_sur() -> pl.DataFrame:
    """Same SUR-only slice the R fixtures were built from."""
    return _load("experience").filter(pl.col("coverage") == "SUR")


def _compare_numeric(py_df: pl.DataFrame, r_df: pl.DataFrame, cols: list[str]) -> None:
    """Pairwise compare numeric columns row-aligned (both already sorted).

    Uses combined absolute + relative tolerance: |py - r| <= ATOL + RTOL * |r|.
    """
    assert py_df.height == r_df.height, (
        f"row count mismatch: py={py_df.height}, r={r_df.height}"
    )
    for c in cols:
        assert c in py_df.columns, f"Python output missing column {c!r}"
        assert c in r_df.columns, f"R fixture missing column {c!r}"
        py_v = py_df[c].to_list()
        r_v = r_df[c].to_list()
        for i, (a, b) in enumerate(zip(py_v, r_v)):
            if a is None or b is None:
                continue
            if isinstance(a, float) and (a != a):
                continue
            if isinstance(b, float) and (b != b):
                continue
            tol = ATOL + RTOL * abs(b)
            assert abs(a - b) <= tol, (
                f"column {c!r} row {i}: py={a} r={b} diff={a - b} tol={tol}"
            )


# ---------------------------------------------------------------------------
# Triangle build
# ---------------------------------------------------------------------------


def test_triangle_build_matches_r():
    r = _load("triangle_sur").sort(["cohort", "dev"])
    py = (
        lr.Triangle(_exp_sur(), groups="coverage")
        .to_polars()
        .sort(["cohort", "dev"])
    )
    _compare_numeric(
        py, r,
        cols=["loss", "incr_loss", "prem", "incr_prem", "lr", "incr_lr"],
    )


# ---------------------------------------------------------------------------
# fit_lr / LR
# ---------------------------------------------------------------------------


def test_lr_sa_full_matches_r():
    r = _load("lr_sa_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = (
        lr.LR(method="sa").fit(tri)
        .to_polars()
        .sort(["cohort", "dev"])
    )
    _compare_numeric(py, r, cols=["loss_proj", "prem_proj", "lr_proj"])


def test_lr_ed_full_matches_r():
    r = _load("lr_ed_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = (
        lr.LR(method="ed").fit(tri)
        .to_polars()
        .sort(["cohort", "dev"])
    )
    _compare_numeric(py, r, cols=["loss_proj", "prem_proj", "lr_proj"])


def test_lr_cl_full_matches_r():
    r = _load("lr_cl_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = (
        lr.LR(method="cl").fit(tri)
        .to_polars()
        .sort(["cohort", "dev"])
    )
    _compare_numeric(py, r, cols=["loss_proj", "prem_proj", "lr_proj"])


def test_lr_sa_maturity_matches_r():
    """Maturity dev: R's $maturity table carries the link target dev in
    the `change` column (mirrors the `change` column convention used by
    Regime). Python exposes the same value via `fit.mat_k[<group>]`."""
    r = _load("lr_sa_maturity")
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    fit = lr.LR(method="sa").fit(tri)
    r_k = int(r["change"].max())
    py_k = fit.mat_k["SUR"]
    assert py_k == r_k, f"mat_k mismatch: py={py_k} r={r_k}"


# ---------------------------------------------------------------------------
# fit_cl / CL  (worker layer — generic `target_*` columns)
# ---------------------------------------------------------------------------


def test_cl_full_matches_r():
    r = _load("cl_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = (
        lr.CL().fit(tri)
        .to_polars()
        .sort(["cohort", "dev"])
    )
    _compare_numeric(py, r, cols=["target_proj"])


def test_cl_mack_se_matches_r():
    """Mack-style SE on the chain ladder projection."""
    r = _load("cl_mack_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = (
        lr.CL().fit(tri)
        .to_polars()
        .sort(["cohort", "dev"])
    )
    _compare_numeric(py, r, cols=["target_proj", "target_total_se"])


# ---------------------------------------------------------------------------
# Link-level diagnostics — ATA factors + ED intensities
# ---------------------------------------------------------------------------


def test_ata_factors_match_r():
    """Per-link ATA factor diagnostic (f, sigma2, cv, rse, n_cohorts).

    R fixture is keyed by (ata_from, ata_to, ata_link); Python uses
    `dev` which equals R's `ata_from` (link source dev). Numeric
    columns must agree exactly on the overlapping link set.
    """
    r = _load("ata_selected").sort(["ata_from"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = tri.link().ata().df.sort(["dev"])

    assert py.height == r.height, (
        f"ATA link count mismatch: py={py.height} r={r.height}"
    )
    _compare_numeric(py, r, cols=["f", "sigma2", "cv", "rse", "n_cohorts"])


def test_intensity_factors_match_r():
    """Per-link ED intensity diagnostic (g, g_se, sigma2, n_cohorts)."""
    r = _load("intensity_selected").sort(["ata_from"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = tri.link().intensity().df.sort(["dev"])

    assert py.height == r.height, (
        f"intensity link count mismatch: py={py.height} r={r.height}"
    )
    _compare_numeric(py, r, cols=["g", "g_se", "sigma2", "n_cohorts"])


# ---------------------------------------------------------------------------
# detect_regime
# ---------------------------------------------------------------------------


def test_regime_changes_match_r():
    """detect_regime change points (Date list)."""
    r = _load("regime_changes")
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = tri.detect_regime(window=12, method="e_divisive").breakpoints

    r_dates = r["change"].to_list()
    assert len(py) == len(r_dates), (
        f"change count mismatch: py={len(py)} r={len(r_dates)}"
    )
    for p, rr in zip(sorted(py), sorted(r_dates)):
        assert p == rr, f"change mismatch: py={p} r={rr}"


# ---------------------------------------------------------------------------
# Summary tables
# ---------------------------------------------------------------------------


def test_lr_sa_summary_matches_r():
    """LR(method='sa').summary() — per-cohort projected lr / SE / CV."""
    r = _load("lr_sa_summary").sort(["cohort"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    lr_fit = lr.LR(method="sa").fit(tri)
    py = lr_fit.summary().sort(["cohort"])

    common = [
        c for c in ["lr_ult", "lr_latest", "lr_se", "lr_cv"]
        if c in r.columns and c in py.columns
    ]
    if not common:
        pytest.skip("no overlapping summary columns to compare")
    _compare_numeric(py, r, cols=common)


# ---------------------------------------------------------------------------
# backtest with metric = "lr"
# ---------------------------------------------------------------------------


def test_backtest_lr_ae_err_matches_r():
    """Cell-level parity with R's backtest output.

    Both languages emit `actual`, `expected`, `aeg`, `ae_err` and use
    NaN for unfittable links. The defensive intersect-on-(cohort, dev)
    is kept as a guard against future drift.
    """
    r = _load("backtest_lr_ae_err").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    bt = lr.Backtest(estimator=lr.LR(method="sa"), holdout=6, metric="lr").fit(tri)
    py_aligned = bt.ae_err.sort(["cohort", "dev"])

    keys = ["cohort", "dev"]
    py_common = py_aligned.join(r.select(keys), on=keys, how="inner").sort(keys)
    r_common = r.join(py_aligned.select(keys), on=keys, how="inner").sort(keys)

    _compare_numeric(py_common, r_common, cols=["actual", "expected", "ae_err"])


def test_backtest_col_summary_matches_r():
    """col_summary aggregates by dev."""
    r = _load("backtest_lr_col_summary").sort(["dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    bt = lr.Backtest(estimator=lr.LR(method="sa"), holdout=6, metric="lr").fit(tri)
    py = bt.col_summary.sort(["dev"])

    keys = ["dev"]
    py_common = py.join(r.select(keys), on=keys, how="inner").sort(keys)
    r_common = r.join(py.select(keys), on=keys, how="inner").sort(keys)

    _compare_numeric(
        py_common, r_common,
        cols=["ae_err_mean", "ae_err_med", "ae_err_wt"],
    )


def test_backtest_diag_summary_matches_r():
    """diag_summary aggregates by calendar diagonal."""
    r = _load("backtest_lr_diag_summary").sort(["cal_idx"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    bt = lr.Backtest(estimator=lr.LR(method="sa"), holdout=6, metric="lr").fit(tri)
    py = bt.diag_summary.sort(["cal_idx"])

    keys = ["cal_idx"]
    py_common = py.join(r.select(keys), on=keys, how="inner").sort(keys)
    r_common = r.join(py.select(keys), on=keys, how="inner").sort(keys)

    _compare_numeric(
        py_common, r_common,
        cols=["ae_err_mean", "ae_err_med", "ae_err_wt"],
    )
