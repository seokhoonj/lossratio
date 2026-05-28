"""R parity tests for ``TriangleValidation`` against ``validate_triangle``.

The R sibling's ``validate_triangle()`` returns a ``TriangleValidation``
data.table carrying:

* the gap table itself (rows = cohorts with a non-consecutive dev sequence)
* ``attr(., "invalid_rows")`` -- rows where ``calendar < cohort``
* a printed summary that we mirror as a two-row count table

Python's :class:`TriangleValidation` exposes the same three pieces via
``.gaps``, ``.invalid_rows``, and ``.summary``. The R dump uses a
deliberately-broken DataFrame (see ``dump_validation`` in the R repo)
so all three sections carry non-trivial content.

Fixtures (from ``dev/parity_dump.R::dump_validation``):

* ``validation_input.csv``         -- the deliberately-broken raw frame
* ``validation_gaps.csv``          -- the ``.gaps`` table
* ``validation_summary.csv``       -- two-row count summary
* ``validation_invalid_rows.csv``  -- the ``calendar < cohort`` rows

To refresh: ``Rscript -e 'source("dev/parity_dump.R")'`` in the R repo.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
import pytest

from lossratio.triangle import TriangleValidation

FIXTURES = Path(__file__).parent / "fixtures"


def _load(name: str) -> pl.DataFrame:
    fp = FIXTURES / f"{name}.csv"
    if not fp.exists():
        pytest.skip(f"missing R fixture: {fp.name}")
    return pl.read_csv(fp, try_parse_dates=True, infer_schema_length=10000)


def _build_validation() -> TriangleValidation:
    """Construct a TriangleValidation from the same broken frame R used.

    The R dump emits ``validation_input.csv``; we re-read it and pass it
    into the Python validator. Both calls operate on byte-identical
    rows, so comparison is apples-to-apples.
    """
    raw = _load("validation_input")
    # Coerce date strings if necessary (try_parse_dates handles most).
    return TriangleValidation(
        raw, groups="coverage", cohort="uy_m", calendar="cy_m"
    )


def test_validation_gaps_match_r():
    """The .gaps table -- cohorts with non-consecutive dev sequences.

    Compares the cohort identity and the gap-positions list (``missing``).
    R writes ``missing`` as a list-column collapsed to a comma-separated
    string in the dump; Python carries it as ``list[int]``. Both are
    canonicalised to a sorted comma-string for comparison.
    """
    r = _load("validation_gaps")
    v = _build_validation()
    py = v.gaps

    assert py.height == r.height, (
        f"gaps row count differs: py={py.height} r={r.height}"
    )
    if py.height == 0:
        return  # nothing else to compare

    # Align on (coverage, cohort). The gap table carries the raw cohort
    # column name (`uy_m`) on both sides, mirroring R's validate_triangle.
    keys = ["coverage", "uy_m"]
    py_sorted = py.sort(keys)
    r_sorted = r.sort(keys)

    assert py_sorted["coverage"].to_list() == r_sorted["coverage"].to_list()
    assert py_sorted["uy_m"].to_list() == r_sorted["uy_m"].to_list()

    for col in ("n_dev", "n_expected"):
        if col not in r_sorted.columns:
            continue
        assert py_sorted[col].to_list() == r_sorted[col].to_list(), (
            f"gaps col {col!r} differs"
        )

    # `missing` is a list[int] in Python, comma-string in the R CSV.
    py_missing = [",".join(str(v) for v in row) for row in py_sorted["missing"].to_list()]
    r_missing = [
        ("" if v is None else str(v))
        for v in r_sorted["missing"].to_list()
    ]
    assert py_missing == r_missing, (
        f"missing columns differ: py={py_missing} r={r_missing}"
    )


def test_validation_summary_matches_r():
    """The two-row count summary -- invalid-row count and gap-cohort count."""
    r = _load("validation_summary")
    v = _build_validation()
    py = v.summary

    # Both languages emit two rows in the same order.
    assert py.height == 2 and r.height == 2
    assert py["check"].to_list() == r["check"].to_list(), (
        f"summary 'check' labels differ: py={py['check'].to_list()} "
        f"r={r['check'].to_list()}"
    )
    assert py["n"].to_list() == r["n"].to_list(), (
        f"summary counts differ: py={py['n'].to_list()} r={r['n'].to_list()}"
    )


def test_validation_invalid_rows_match_r():
    """The .invalid_rows table -- rows where calendar < cohort.

    Compares cohort identity, calendar date, and the reason string.
    """
    r = _load("validation_invalid_rows")
    v = _build_validation()
    py = v.invalid_rows

    assert py.height == r.height, (
        f"invalid_rows row count: py={py.height} r={r.height}"
    )
    if py.height == 0:
        return

    # Stable sort key: (coverage, cohort, calendar).
    keys = [k for k in ("coverage", "uy_m", "cy_m") if k in py.columns]
    py_sorted = py.sort(keys)
    r_sorted = r.sort(keys)

    for col in keys:
        assert py_sorted[col].to_list() == r_sorted[col].to_list(), (
            f"invalid_rows col {col!r} differs"
        )
    # `reason` is a string column on both sides.
    if "reason" in py_sorted.columns and "reason" in r_sorted.columns:
        assert py_sorted["reason"].to_list() == r_sorted["reason"].to_list()
