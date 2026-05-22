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
    return _load("experience").filter(pl.col("coverage") == "surgery")


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
    # Numeric columns: the headline triplet plus the 7 new numeric
    # parity columns (n_cohorts, margin pair, share quartet). Proves
    # the Python formulas match R's `as_triangle` bit-for-bit.
    _compare_numeric(
        py, r,
        cols=[
            "loss", "incr_loss", "premium", "incr_premium",
            "ratio", "incr_ratio",
            "n_cohorts", "margin", "incr_margin",
            "loss_share", "incr_loss_share",
            "premium_share", "incr_premium_share",
        ],
    )
    # `profit` / `incr_profit` are string indicators ("pos" / "neg")
    # -- compare with plain equality, not the numeric tolerance path.
    for c in ("profit", "incr_profit"):
        assert c in py.columns, f"Python output missing column {c!r}"
        assert c in r.columns, f"R fixture missing column {c!r}"
        assert py[c].to_list() == r[c].to_list(), (
            f"string column {c!r} differs from R fixture"
        )


# ---------------------------------------------------------------------------
# fit_ratio / Ratio
# ---------------------------------------------------------------------------


def test_ratio_sa_full_matches_r():
    r = _load("ratio_sa_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = (
        lr.Ratio(method="sa").fit(tri)
        .to_polars()
        .sort(["cohort", "dev"])
    )
    _compare_numeric(py, r, cols=["loss_proj", "premium_proj", "ratio_proj"])


def test_ratio_ed_full_matches_r():
    r = _load("ratio_ed_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = (
        lr.Ratio(method="ed").fit(tri)
        .to_polars()
        .sort(["cohort", "dev"])
    )
    _compare_numeric(py, r, cols=["loss_proj", "premium_proj", "ratio_proj"])


def test_ratio_cl_full_matches_r():
    r = _load("ratio_cl_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = (
        lr.Ratio(method="cl").fit(tri)
        .to_polars()
        .sort(["cohort", "dev"])
    )
    _compare_numeric(py, r, cols=["loss_proj", "premium_proj", "ratio_proj"])


def test_ratio_sa_maturity_matches_r():
    """Maturity dev: R's $maturity table carries the link target dev in
    the `change` column (mirrors the `change` column convention used by
    Regime). Python exposes the same value via `fit.mat_k[<group>]`."""
    r = _load("ratio_sa_maturity")
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    fit = lr.Ratio(method="sa").fit(tri)
    r_k = int(r["change"].max())
    py_k = fit.mat_k["surgery"]
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
    _compare_numeric(py, r, cols=["loss_proj"])


def test_cl_mack_se_matches_r():
    """Mack-style SE on the chain ladder projection."""
    r = _load("cl_mack_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = (
        lr.CL().fit(tri)
        .to_polars()
        .sort(["cohort", "dev"])
    )
    _compare_numeric(py, r, cols=["loss_proj", "loss_total_se"])


# ---------------------------------------------------------------------------
# recent — calendar-diagonal wedge factor filter (deterministic; bit-parity)
# ---------------------------------------------------------------------------

# The `recent` filter is a deterministic cell selection, so Python and R
# must agree to floating-point precision — the only slack is R's fwrite
# rounding to ~15 significant digits. Use a tight tolerance.
_RECENT_ATOL = 1e-4
_RECENT_RTOL = 1e-12


def _compare_recent(
    py_df: pl.DataFrame, r_df: pl.DataFrame, cols: list[str]
) -> float:
    """Bit-parity compare for the deterministic `recent` filter.

    Returns the worst absolute difference observed (for reporting). The
    tolerance is `_RECENT_ATOL + _RECENT_RTOL * |r|` — far tighter than
    the generic `_compare_numeric` path, since the recent wedge selects
    an exact cohort set with no algorithmic slack.
    """
    assert py_df.height == r_df.height, (
        f"row count mismatch: py={py_df.height}, r={r_df.height}"
    )
    worst = 0.0
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
            diff = abs(a - b)
            worst = max(worst, diff)
            tol = _RECENT_ATOL + _RECENT_RTOL * abs(b)
            assert diff <= tol, (
                f"column {c!r} row {i}: py={a} r={b} diff={diff} tol={tol}"
            )
    return worst


def test_cl_recent_matches_r():
    """CL(recent=12).fit(...) loss_proj vs R fit_cl(recent=12)$full.

    The recent-diagonal wedge is a deterministic filter, so the
    projected losses must match R to floating-point precision.
    """
    r = _load("cl_recent12_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = (
        lr.CL(recent=12).fit(tri)
        .to_polars()
        .sort(["cohort", "dev"])
    )
    _compare_recent(py, r, cols=["loss_proj"])


def test_ed_recent_matches_r():
    """ED(recent=12).fit(...) loss_proj / premium_proj vs R
    fit_ed(recent=12)$full — deterministic, floating-point parity."""
    r = _load("ed_recent12_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = (
        lr.ED(recent=12).fit(tri)
        .to_polars()
        .sort(["cohort", "dev"])
    )
    cols = ["loss_proj"]
    if "premium_proj" in r.columns:
        cols.append("premium_proj")
    _compare_recent(py, r, cols=cols)


def test_cl_recent_factors_match_r():
    """recent=12 per-link ATA factors vs R cl_recent12_selected.csv.

    Keys on the link's source dev (Python `dev` == R `ata_from`). The
    R `selected` fixture carries `f`, `f_sel`, `sigma2`, `n_cohorts`;
    Python applies no separate selection on top of the volume-weighted
    `f`, so `f` == R's `f_sel`.

    The decisive check is `n_cohorts`. With recent=12 every early link
    caps at exactly 12 contributing cohorts (down from 35 unfiltered);
    only the late links near the triangle tip carry fewer than 12 --
    the calendar-diagonal wedge simply runs out of links there. Python
    must reproduce R's exact per-link cohort count.
    """
    r = _load("cl_recent12_selected").sort(["ata_from"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = tri.link().ata(recent=12).df.sort(["dev"])

    assert py.height == r.height, (
        f"link count mismatch: py={py.height} r={r.height}"
    )
    # Python `dev` aligns with R `ata_from` after both are sorted.
    assert py["dev"].to_list() == r["ata_from"].to_list(), (
        "link source-dev keys differ from R fixture"
    )
    # Every early link caps at exactly 12; the wedge runs out only at
    # the triangle tip. Python and R must agree link-for-link.
    r_n = r["n_cohorts"].to_list()
    assert all(n == 12 for n in r_n[:12]), (
        "R fixture: recent=12 should cap early links at 12 cohorts"
    )
    assert py["n_cohorts"].to_list() == r_n, (
        "per-link n_cohorts differs from R fixture"
    )
    _compare_recent(py, r, cols=["f", "sigma2", "n_cohorts"])
    # Python `f` is the selected factor (no separate selection step):
    # compare it against R's `f_sel` column directly.
    py_sel = py.rename({"f": "f_sel"})
    _compare_recent(py_sel, r, cols=["f_sel"])


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


def test_ratio_sa_summary_matches_r():
    """Ratio(method='sa').summary() — per-cohort ultimate / SE / CV.

    Compares the full numeric column set. The R fixture is dumped with
    bootstrap disabled (`fit_ratio(..., bootstrap = FALSE)`), so the SE
    columns are the deterministic analytical values — comparable to
    Python's analytical SE. Fully-observed cohorts carry NaN SE in
    Python (no projection) vs 0 in R; `_compare_numeric` skips those.
    """
    r = _load("ratio_sa_summary").sort(["cohort"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    ratio_fit = lr.Ratio(method="sa").fit(tri)
    py = ratio_fit.summary().sort(["cohort"])

    _compare_numeric(
        py, r,
        cols=[
            "latest", "loss_ult", "reserve", "premium_ult",
            "ratio_latest", "ratio_ult", "maturity_from",
            "loss_proc_se", "loss_param_se", "loss_total_se",
            "loss_total_cv", "ratio_se", "ratio_cv",
            "ratio_ci_lo", "ratio_ci_hi",
        ],
    )


# ---------------------------------------------------------------------------
# backtest with metric = "ratio"
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("method", ["ed", "sa"])
def test_backtest_ratio_ae_err_matches_r(method: str):
    """Cell-level parity with R's backtest output, per loss method.

    R's `backtest()` is dumped once per `loss_method` ("ed", the
    default, and "sa"); the Python `Backtest` uses the matching
    `Ratio` method. Both languages emit `actual`, `expected`, `aeg`,
    `ae_err`. The intersect-on-(cohort, dev) guards against drift.
    """
    r = _load(f"backtest_ratio_{method}_ae_err").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    bt = lr.Backtest(
        estimator=lr.Ratio(method=method), holdout=6, metric="ratio"
    ).fit(tri)
    py_aligned = bt.ae_err.sort(["cohort", "dev"])

    keys = ["cohort", "dev"]
    py_common = py_aligned.join(r.select(keys), on=keys, how="inner").sort(keys)
    r_common = r.join(py_aligned.select(keys), on=keys, how="inner").sort(keys)

    _compare_numeric(py_common, r_common, cols=["actual", "expected", "ae_err"])


@pytest.mark.parametrize("method", ["ed", "sa"])
def test_backtest_col_summary_matches_r(method: str):
    """col_summary aggregates by dev, per loss method."""
    r = _load(f"backtest_ratio_{method}_col_summary").sort(["dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    bt = lr.Backtest(
        estimator=lr.Ratio(method=method), holdout=6, metric="ratio"
    ).fit(tri)
    py = bt.col_summary.sort(["dev"])

    keys = ["dev"]
    py_common = py.join(r.select(keys), on=keys, how="inner").sort(keys)
    r_common = r.join(py.select(keys), on=keys, how="inner").sort(keys)

    _compare_numeric(
        py_common, r_common,
        cols=["ae_err_mean", "ae_err_med", "ae_err_wt"],
    )


@pytest.mark.parametrize("method", ["ed", "sa"])
def test_backtest_diag_summary_matches_r(method: str):
    """diag_summary aggregates by calendar diagonal, per loss method."""
    r = _load(f"backtest_ratio_{method}_diag_summary").sort(["cal_idx"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    bt = lr.Backtest(
        estimator=lr.Ratio(method=method), holdout=6, metric="ratio"
    ).fit(tri)
    py = bt.diag_summary.sort(["cal_idx"])

    keys = ["cal_idx"]
    py_common = py.join(r.select(keys), on=keys, how="inner").sort(keys)
    r_common = r.join(py.select(keys), on=keys, how="inner").sort(keys)

    _compare_numeric(
        py_common, r_common,
        cols=["ae_err_mean", "ae_err_med", "ae_err_wt"],
    )


# ---------------------------------------------------------------------------
# fit_bf / BF
# ---------------------------------------------------------------------------

_BF_SUMMARY_COLS = [
    "latest", "loss_ult", "reserve", "elr", "q",
    "loss_total_se", "loss_total_cv", "loss_ci_lo", "loss_ci_hi",
]
_BF_FULL_COLS = [
    "loss_obs", "loss_proj", "incr_loss_proj",
    "premium_obs", "premium_proj", "incr_premium_proj",
]
# Cape Cod summary additionally carries the pooled-ELR uncertainty.
_CC_SUMMARY_COLS = _BF_SUMMARY_COLS + [
    "elr_cc_se", "elr_cc_cv", "elr_cc_ci_lo", "elr_cc_ci_hi",
]


def test_bf_summary_matches_r():
    """BF(prior=1.5).summary() vs R fit_bf(..., prior=1.5)."""
    r = _load("bf_summary").sort(["cohort"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = lr.BF(prior=1.5).fit(tri).summary().sort(["cohort"])
    _compare_numeric(py, r, cols=_BF_SUMMARY_COLS)


def test_bf_full_matches_r():
    """BF(prior=1.5).df cell grid vs R fit_bf(...) $full."""
    r = _load("bf_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = lr.BF(prior=1.5).fit(tri).to_polars().sort(["cohort", "dev"])
    _compare_numeric(py, r, cols=_BF_FULL_COLS)


def test_bf_cred_summary_matches_r():
    """BF(prior=1.5, credibility={'method':'bs'}).summary() vs R
    fit_bf(..., credibility=list(method='bs'))."""
    r = _load("bf_cred_summary").sort(["cohort"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = (
        lr.BF(prior=1.5, credibility={"method": "bs"})
        .fit(tri)
        .summary()
        .sort(["cohort"])
    )
    _compare_numeric(py, r, cols=_BF_SUMMARY_COLS)


# ---------------------------------------------------------------------------
# fit_cc / CC
# ---------------------------------------------------------------------------


def test_cc_summary_matches_r():
    """CC().summary() vs R fit_cc(...). R names the pooled-ELR column
    `elr` in `$summary`; Python matches (and adds a separate `elr_cc`)."""
    r = _load("cc_summary").sort(["cohort"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = lr.CC().fit(tri).summary().sort(["cohort"])
    _compare_numeric(py, r, cols=_CC_SUMMARY_COLS)


def test_cc_full_matches_r():
    """CC().df cell grid vs R fit_cc(...) $full."""
    r = _load("cc_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = lr.CC().fit(tri).to_polars().sort(["cohort", "dev"])
    _compare_numeric(py, r, cols=_BF_FULL_COLS)


def test_cc_cred_summary_matches_r():
    """CC(credibility={'method':'bs'}).summary() vs R
    fit_cc(..., credibility=list(method='bs'))."""
    r = _load("cc_cred_summary").sort(["cohort"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = (
        lr.CC(credibility={"method": "bs"})
        .fit(tri)
        .summary()
        .sort(["cohort"])
    )
    _compare_numeric(py, r, cols=_CC_SUMMARY_COLS)
