"""R parity for the segment_wise regime treatment.

Fixtures are produced by ``dev/parity_segment_wise.R`` in the R repo
on the canonical real-data fixture (``dev/data.rds``, SUR-only). The
Python side rebuilds the same Triangle from the same input rows, runs
the equivalent LR fit, and compares projections cell-by-cell.

Tight numerical agreement is the contract: any drift signals an
implementation divergence between the two language ports.
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
    # CSV emitted by R's data.table::fwrite. infer_schema_length is bumped
    # to cope with mixed-magnitude numeric columns.
    return pl.read_csv(fp, try_parse_dates=True, infer_schema_length=10000)


def _compare(
    py: pl.DataFrame,
    r: pl.DataFrame,
    cols: list[str],
    atol: float = ATOL,
    rtol: float = RTOL,
) -> None:
    assert py.height == r.height, (
        f"row count mismatch: py={py.height}, r={r.height}"
    )
    for c in cols:
        assert c in py.columns, f"Python missing column {c!r}"
        assert c in r.columns, f"R fixture missing column {c!r}"
        for i, (a, b) in enumerate(zip(py[c].to_list(), r[c].to_list())):
            if a is None or b is None:
                continue
            if isinstance(a, float) and (a != a):  # NaN
                continue
            if isinstance(b, float) and (b != b):
                continue
            tol = atol + rtol * abs(b)
            assert abs(a - b) <= tol, (
                f"column {c!r} row {i}: py={a} r={b} diff={a - b} tol={tol}"
            )


def _build_triangle() -> lr.Triangle:
    """Reload the SUR-only input that R used and build a Triangle on
    the same column names (uym / cym / incr_loss / incr_prem)."""
    raw = _load("segment_wise_input")
    return lr.Triangle(
        raw,
        cohort="uym",
        calendar="cym",
        # group_var is None — SUR-only, single group
    )


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------


def test_segment_wise_lr_cl_full_matches_r():
    """Cell-by-cell comparison of fit_lr's $full projection under
    segment_wise treatment (both loss + prem sides)."""
    r = _load("segment_wise_lr_cl_full").sort(["cohort", "dev"])
    tri = _build_triangle()
    reg = lr.regime_at(change="2024-07-01", treatment="segment_wise")
    fit = lr.LR(
        method="cl",
        premium_method="cl",
        loss_regime=reg,
        premium_regime=reg,
    ).fit(tri)
    py = fit.to_polars().sort(["cohort", "dev"])

    # Schema parity check
    for c in ("cohort", "dev", "segment_id",
              "loss_proj", "prem_proj", "lr_proj"):
        assert c in py.columns, f"Python missing column {c!r}"

    # Segment ids should match exactly per (cohort, dev)
    _compare(py, r, cols=["segment_id"], atol=0, rtol=0)

    # Projection columns — tight tolerance
    _compare(
        py, r,
        cols=[
            "loss_obs", "loss_proj", "incr_loss_proj",
            "prem_obs", "prem_proj", "incr_prem_proj",
            "lr_proj", "incr_lr_proj",
        ],
    )


def test_segment_wise_lr_cl_summary_matches_r():
    """Per-cohort summary (lr_ult / loss_ult / prem_ult) comparison."""
    r = _load("segment_wise_lr_cl_summary").sort(["cohort"])
    tri = _build_triangle()
    reg = lr.regime_at(change="2024-07-01", treatment="segment_wise")
    fit = lr.LR(
        method="cl",
        premium_method="cl",
        loss_regime=reg,
        premium_regime=reg,
    ).fit(tri)
    py = fit.summary()
    if hasattr(py, "to_polars"):
        py = py.to_polars()
    py = py.sort(["cohort"])

    common = [
        c for c in ("loss_ult", "prem_ult", "lr_ult")
        if c in r.columns and c in py.columns
    ]
    assert common, f"no overlapping summary cols; r={r.columns}, py={py.columns}"
    _compare(py, r, cols=common)


def test_segment_wise_lr_ed_full_matches_r():
    """ED loss method + CL prem method under segment_wise."""
    r = _load("segment_wise_lr_ed_full").sort(["cohort", "dev"])
    tri = _build_triangle()
    reg = lr.regime_at(change="2024-07-01", treatment="segment_wise")
    fit = lr.LR(
        method="ed",
        premium_method="cl",
        loss_regime=reg,
        premium_regime=reg,
    ).fit(tri)
    py = fit.to_polars().sort(["cohort", "dev"])

    _compare(py, r, cols=["segment_id"], atol=0, rtol=0)
    _compare(
        py, r,
        cols=[
            "loss_obs", "loss_proj", "incr_loss_proj",
            "prem_obs", "prem_proj", "incr_prem_proj",
            "lr_proj", "incr_lr_proj",
        ],
    )


def test_segment_wise_lr_ed_summary_matches_r():
    """ED method summary comparison."""
    r = _load("segment_wise_lr_ed_summary").sort(["cohort"])
    tri = _build_triangle()
    reg = lr.regime_at(change="2024-07-01", treatment="segment_wise")
    fit = lr.LR(
        method="ed",
        premium_method="cl",
        loss_regime=reg,
        premium_regime=reg,
    ).fit(tri)
    py = fit.summary()
    if hasattr(py, "to_polars"):
        py = py.to_polars()
    py = py.sort(["cohort"])

    common = [
        c for c in ("loss_ult", "prem_ult", "lr_ult")
        if c in r.columns and c in py.columns
    ]
    assert common, f"no overlapping summary cols; r={r.columns}, py={py.columns}"
    _compare(py, r, cols=common)
