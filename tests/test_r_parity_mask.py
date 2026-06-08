"""R parity tests for ``Triangle.mask(holdout)``.

The R sibling's ``mask_triangle(x, holdout)`` drops the most recent
``holdout`` calendar diagonals per group. The mask is a deterministic
cell selection (no RNG), so Python and R must produce cell-for-cell
identical loss / premium / ratio totals.

Fixtures (from ``dev/parity_dump.R::dump_mask``):

* ``mask_holdout0.csv``  -- no rows dropped (sanity check + metadata parity)
* ``mask_holdout3.csv``
* ``mask_holdout6.csv``
* ``mask_holdout12.csv``

To refresh, run ``Rscript -e 'source("dev/parity_dump.R")'`` in the R
repo and copy the resulting CSVs to ``tests/fixtures/``.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
import pytest

import lossratio as lr

FIXTURES = Path(__file__).parent / "fixtures"
ATOL = 1e-6
RTOL = 1e-9

_HOLDOUTS = (0, 3, 6, 12)
_NUMERIC_COLS = (
    "loss", "incr_loss",
    "premium", "incr_premium",
    "ratio", "incr_ratio",
)


def _load(name: str) -> pl.DataFrame:
    fp = FIXTURES / f"{name}.csv"
    if not fp.exists():
        pytest.skip(f"missing R fixture: {fp.name}")
    return pl.read_csv(fp, try_parse_dates=True, infer_schema_length=10000)


def _exp_sur() -> pl.DataFrame:
    return _load("experience").filter(pl.col("coverage") == "surgery")


def _compare_numeric(
    py_df: pl.DataFrame, r_df: pl.DataFrame, cols: list[str]
) -> None:
    assert py_df.height == r_df.height, (
        f"row count mismatch: py={py_df.height}, r={r_df.height}"
    )
    for c in cols:
        if c not in r_df.columns:
            continue
        assert c in py_df.columns, f"Python output missing column {c!r}"
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
                f"holdout col {c!r} row {i}: py={a} r={b} "
                f"diff={a - b} tol={tol}"
            )


@pytest.mark.parametrize("holdout", _HOLDOUTS)
def test_mask_cells_match_r(holdout: int):
    """Cell-by-cell parity between R ``mask_triangle`` and Python ``Triangle.mask``.

    For each holdout value the masked Triangle's per-(cohort, duration) loss /
    premium / ratio totals must match R bit-for-bit. Holdout = 0 is the
    no-op identity case (full Triangle).
    """
    r = _load(f"mask_holdout{holdout}").sort(["cohort", "duration"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    masked = tri.mask(holdout=holdout)
    py = masked.to_polars().sort(["cohort", "duration"])

    # Keep this row-count assertion separate from numeric compare so the
    # failure message points clearly at the mask-shape bug if R and
    # Python disagree on which cells survive.
    assert py.height == r.height, (
        f"holdout={holdout}: row count differs py={py.height} r={r.height}"
    )
    _compare_numeric(py, r, cols=list(_NUMERIC_COLS))


@pytest.mark.parametrize("holdout", _HOLDOUTS)
def test_mask_preserves_cohort_set(holdout: int):
    """The masked Triangle must keep the same cohort set as R.

    R's mask drops *cells*, not entire cohorts, so the unique cohort
    values should be identical. Catches subtle off-by-one errors in
    the `cohort_rank + duration - 1` calendar-diagonal index.
    """
    r = _load(f"mask_holdout{holdout}").sort(["cohort"])
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = tri.mask(holdout=holdout).to_polars().sort(["cohort"])

    py_cohorts = sorted(set(py["cohort"].to_list()))
    r_cohorts = sorted(set(r["cohort"].to_list()))
    assert py_cohorts == r_cohorts, (
        f"holdout={holdout}: cohort set differs "
        f"py={len(py_cohorts)} r={len(r_cohorts)}"
    )
