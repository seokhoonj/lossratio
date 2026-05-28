"""R parity tests for the full ``Maturity.summary()`` 16-column schema.

The R sibling's ``detect_maturity()`` returns a one-row-per-group
``Maturity`` data.table with columns::

    ata_from, change, ata_link, mean, median, wt, cv,
    f, f_se, rse, sigma,
    n_cohorts, n_valid, n_inf, n_nan, valid_ratio

Python's :meth:`Maturity.summary` mirrors that schema. Both must agree
to floating-point precision -- the underlying ATA factor estimation is
deterministic.

Fixture: ``maturity_schema.csv`` from
``dev/parity_dump.R::dump_maturity_schema``.

To refresh: ``Rscript -e 'source("dev/parity_dump.R")'`` in the R repo.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
import pytest

import lossratio as lr

FIXTURES = Path(__file__).parent / "fixtures"
ATOL = 1e-6
RTOL = 1e-9

_NUMERIC_COLS = (
    "ata_from", "change",
    "mean", "median", "wt", "cv",
    "f", "f_se", "rse", "sigma",
    "n_cohorts", "n_valid", "n_inf", "n_nan", "valid_ratio",
)
_STRING_COLS = ("ata_link",)


def _load(name: str) -> pl.DataFrame:
    fp = FIXTURES / f"{name}.csv"
    if not fp.exists():
        pytest.skip(f"missing R fixture: {fp.name}")
    return pl.read_csv(fp, try_parse_dates=True, infer_schema_length=10000)


def _exp_sur() -> pl.DataFrame:
    return _load("experience").filter(pl.col("coverage") == "surgery")


def test_maturity_full_schema_matches_r():
    """All 16 columns of the Maturity summary table match R bit-for-bit.

    Tighter than the existing ``test_ratio_sa_maturity_matches_r`` which
    only checks the scalar ``change`` (= maturity dev). This test
    exercises the per-link stats joined onto the matched row.
    """
    r = _load("maturity_schema")
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    py = tri.detect_maturity(loss="loss").summary()

    # Single-group input -> one row each. Align trivially without sort.
    assert py.height == 1 and r.height == 1, (
        f"expected 1-row summary; py={py.height} r={r.height}"
    )

    for c in _NUMERIC_COLS:
        assert c in py.columns, f"Python summary missing column {c!r}"
        assert c in r.columns, f"R fixture missing column {c!r}"
        a = py[c][0]
        b = r[c][0]
        # NaN tolerance: R writes empty for NA_real_, polars reads None.
        if a is None and b is None:
            continue
        if isinstance(a, float) and (a != a) and isinstance(b, float) and (b != b):
            continue
        assert a is not None, f"py {c!r} is None but R has {b!r}"
        assert b is not None, f"R {c!r} is None but py has {a!r}"
        tol = ATOL + RTOL * abs(b)
        assert abs(a - b) <= tol, (
            f"maturity col {c!r}: py={a} r={b} diff={a - b} tol={tol}"
        )

    # String column: exact match
    for c in _STRING_COLS:
        assert py[c][0] == r[c][0], (
            f"maturity col {c!r}: py={py[c][0]!r} r={r[c][0]!r}"
        )


def test_maturity_change_equals_mat_k():
    """The ``change`` column on the summary table equals ``Maturity.maturity_point``.

    Cross-check of the two access paths into the same value. Catches
    drift if one of the two ever stops mirroring R.
    """
    r = _load("maturity_schema")
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    mat = tri.detect_maturity(loss="loss")
    summary_change = mat.summary()["change"][0]
    mat_k_value = mat.maturity_point["surgery"]
    r_change = r["change"][0]

    assert int(summary_change) == int(mat_k_value), (
        f"summary['change']={summary_change} != mat_k={mat_k_value}"
    )
    assert int(mat_k_value) == int(r_change), (
        f"mat_k={mat_k_value} != R fixture change={r_change}"
    )
