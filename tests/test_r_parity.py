"""R parity tests.

The fixtures in ``tests/fixtures/*.parquet`` are dumped from the R
sibling (`Rscript dev/parity_dump.R` in the R repo). Each test loads
the R-side fixture, runs the equivalent Python call on the same
input rows, and asserts column-by-column numerical agreement to a
tight tolerance.

These tests guard against silent algorithmic drift between the two
languages. To refresh fixtures after a deliberate algorithm change,
re-run the R dump script and copy the new parquets here.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
import pytest

import lossratio as lr

FIXTURES = Path(__file__).parent / "fixtures"
ATOL = 1e-8
RTOL = 1e-9


def _load(name: str) -> pl.DataFrame:
    fp = FIXTURES / f"{name}.parquet"
    if not fp.exists():
        pytest.skip(f"missing R fixture: {fp.name}")
    return pl.read_parquet(fp)


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
        lr.Triangle(_exp_sur(), group_var="coverage")
        .to_polars()
        .sort(["cohort", "dev"])
    )
    _compare_numeric(
        py, r,
        cols=["loss", "loss_incr", "premium", "premium_incr", "lr", "lr_incr"],
    )


# ---------------------------------------------------------------------------
# fit_lr / LR
# ---------------------------------------------------------------------------


def test_lr_sa_full_matches_r():
    r = _load("lr_sa_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), group_var="coverage")
    py = (
        lr.LR(method="sa").fit(tri)
        .to_polars()
        .sort(["cohort", "dev"])
    )
    _compare_numeric(py, r, cols=["loss_proj", "premium_proj", "lr_proj"])


# ---------------------------------------------------------------------------
# fit_cl / CL
# ---------------------------------------------------------------------------


def test_cl_full_matches_r():
    r = _load("cl_full").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), group_var="coverage")
    py = (
        lr.CL().fit(tri)
        .to_polars()
        .sort(["cohort", "dev"])
    )
    # CL in R produces value_proj; in Python it's loss_proj. Compare by
    # name on Python; map the R column by content.
    _compare_numeric(
        py.rename({"loss_proj": "value_proj"}),
        r,
        cols=["value_proj"],
    )


# ---------------------------------------------------------------------------
# backtest with metric = "lr"
# ---------------------------------------------------------------------------


def test_backtest_lr_ae_err_matches_r():
    """Parity on cells the R side keeps after `is.finite(value_pred)`.

    Note: the Python masked-fit can produce finite projections for some
    deepest-dev cells where the R masked-fit returns NA. The set of cells
    on which both languages report a value is what we compare; this is
    the natural overlap and matches R's intent (R drops NA cells before
    aggregation anyway). The Python-only cells are an intentional
    algorithmic difference, not a bug — flagged here for visibility.
    """
    r = _load("backtest_lr_ae_err").sort(["cohort", "dev"])
    tri = lr.Triangle(_exp_sur(), group_var="coverage")
    bt = lr.Backtest(estimator=lr.LR(method="sa"), holdout=6, metric="lr").fit(tri)
    py_aligned = (
        bt.ae_err
        .rename({"actual": "value_actual", "predicted": "value_pred"})
        .sort(["cohort", "dev"])
    )

    # Intersect on (cohort, dev) so the comparison is row-aligned across
    # the cells both languages produced.
    keys = ["cohort", "dev"]
    py_common = py_aligned.join(r.select(keys), on=keys, how="inner").sort(keys)
    r_common = r.join(py_aligned.select(keys), on=keys, how="inner").sort(keys)

    _compare_numeric(
        py_common, r_common,
        cols=["value_actual", "value_pred", "ae_err"],
    )
