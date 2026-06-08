"""R parity tests for ``extrapolate_tail_sigma2``.

The fixture ``tests/fixtures/sigma_extrapolation.csv`` is dumped from
R's ``.extrapolate_sigma_ata``: 8 input cases (C1..C8) x 5
``sigma_method`` values (``locf``, ``min_last2``, ``loglinear``,
``mack``, ``none``) x 6 link rows = 240 rows. Each test rebuilds the
Python input from ``sigma2_in`` (null -> ``np.nan``; the literal ``0``
in C6 stays ``0`` -- both flagged as "needs fill"), runs
:func:`extrapolate_tail_sigma2`, and asserts row-by-row numerical
agreement plus a per-group warning-flag agreement.

A second block smoke-tests that the estimators accept the newly wired
``sigma_method="mack"`` and ``"none"`` end-to-end.
"""

from __future__ import annotations

import warnings
from pathlib import Path

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio._sigma import extrapolate_tail_sigma2

FIXTURES = Path(__file__).parent / "fixtures"
ATOL = 1e-6
RTOL = 1e-9

METHODS = ("locf", "min_last2", "loglinear", "mack", "none")


def _load(name: str) -> pl.DataFrame:
    fp = FIXTURES / f"{name}.csv"
    if not fp.exists():
        pytest.skip(f"missing R fixture: {fp.name}")
    return pl.read_csv(fp, try_parse_dates=True, infer_schema_length=10000)


def _exp_sur() -> pl.DataFrame:
    """Same SURGERY-only slice the R fixtures were built from."""
    return _load("experience").filter(pl.col("coverage") == "surgery")


def _sigma_groups() -> list[tuple[str, str]]:
    """Enumerate every (case, method) pair present in the fixture."""
    df = _load("sigma_extrapolation")
    cases = df["case"].unique(maintain_order=True).to_list()
    return [(c, m) for c in cases for m in METHODS]


# ---------------------------------------------------------------------------
# extrapolate_tail_sigma2 parity
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("case,method", _sigma_groups())
def test_extrapolate_tail_sigma2_matches_r(case: str, method: str) -> None:
    """Per-(case, method) parity with R's ``.extrapolate_sigma_ata``."""
    df = _load("sigma_extrapolation")
    grp = (
        df.filter((pl.col("case") == case) & (pl.col("method") == method))
        .sort("ata_from")
    )
    assert grp.height == 6, f"{case}/{method}: expected 6 rows"

    # Build the Python input: null -> NaN, numeric (incl. literal 0) kept
    # as-is. Both NaN and 0 are flagged as "needs fill" downstream.
    s2_in = grp["sigma2_in"].to_list()
    py_in = np.array(
        [np.nan if v is None else float(v) for v in s2_in],
        dtype=np.float64,
    )

    # Capture whether the call emits a warning.
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        py_out = extrapolate_tail_sigma2(py_in, method)
        warned_py = len(caught) > 0

    # Numeric assertion, row by row.
    s2_out = grp["sigma2_out"].to_list()
    for i, (py_v, r_v) in enumerate(zip(py_out.tolist(), s2_out)):
        if r_v is None:
            # R left this link unfilled -> Python must also be unfilled.
            assert np.isnan(py_v) or py_v <= 0.0, (
                f"{case}/{method} row {i}: R unfilled, "
                f"Python filled with {py_v}"
            )
        else:
            tol = ATOL + RTOL * abs(r_v)
            assert abs(py_v - r_v) <= tol, (
                f"{case}/{method} row {i}: py={py_v} r={r_v} "
                f"diff={py_v - r_v} tol={tol}"
            )

    # Warning-flag assertion (per-(case, method), repeated down the rows).
    warned_r = bool(grp["warned"][0])
    assert warned_py == warned_r, (
        f"{case}/{method}: warned mismatch py={warned_py} r={warned_r}"
    )


# ---------------------------------------------------------------------------
# mack / none wiring smoke test
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("sigma_method", ["mack", "none"])
def test_cl_accepts_new_sigma_methods(sigma_method: str) -> None:
    """``lr.ChainLadder`` runs with the newly wired ``mack`` / ``none`` methods."""
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    fit = lr.ChainLadder(sigma_method=sigma_method).fit(tri)
    assert fit is not None
    assert fit.sigma_method == sigma_method
    assert fit.to_polars().height > 0


@pytest.mark.parametrize("sigma_method", ["mack", "none"])
def test_ratio_sa_accepts_new_sigma_methods(sigma_method: str) -> None:
    """``lr.Ratio(method='sa')`` threads ``mack`` / ``none`` through.

    ``RatioFit`` does not surface ``sigma_method`` as a slot (only the
    ``Ratio`` estimator does), so this asserts the estimator stored it
    and the fit ran end-to-end.
    """
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    estimator = lr.Ratio(method="sa", sigma_method=sigma_method)
    assert estimator.sigma_method == sigma_method
    fit = estimator.fit(tri)
    assert fit is not None
    assert fit.to_polars().height > 0
