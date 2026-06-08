"""Tests for the Premium projection dispatcher.

Premium had no dedicated suite even though it is a core dispatcher: it
projects cumulative premium across the cohort x duration grid and feeds the
denominator of every loss-ratio fit. These tests pin its headline
contract -- the point projection is identical under both variance
recursions, only the standard errors differ.
"""

from __future__ import annotations

import numpy as np
import polars as pl
import pytest

import lossratio as lr


def _sur_triangle() -> lr.Triangle:
    return lr.Triangle(lr.load_experience().filter(pl.col("coverage") == "SUR"))


_PREMIUM_COLUMNS = {
    "cohort", "duration", "premium_obs", "premium_proj", "incr_premium_proj",
    "premium_proc_se", "premium_param_se", "premium_total_se",
    "premium_proc_cv", "premium_param_cv", "premium_total_cv",
    "premium_ci_lo", "premium_ci_hi",
}


def test_premium_output_schema():
    df = lr.Premium().fit(_sur_triangle()).to_polars()
    assert _PREMIUM_COLUMNS.issubset(set(df.columns))


def test_premium_default_method_is_ed():
    assert lr.Premium().fit(_sur_triangle()).method == "ed"


def test_premium_ed_cl_point_estimate_identical():
    """The premium point projection is the multiplicative recursion under
    both methods -- only the variance recursion differs -- so premium_proj
    must be identical between ed and cl."""
    tri = _sur_triangle()
    ed = lr.Premium(method="ed").fit(tri).to_polars().sort(["cohort", "duration"])
    cl = lr.Premium(method="cl").fit(tri).to_polars().sort(["cohort", "duration"])
    e = ed["premium_proj"].to_numpy()
    c = cl["premium_proj"].to_numpy()
    finite = ~np.isnan(e) & ~np.isnan(c)
    assert finite.sum() > 0
    assert np.allclose(e[finite], c[finite])


def test_premium_ed_cl_variance_differs():
    """The ed (additive) and cl (multiplicative f^2-compounding) recursions
    give different standard errors on the identical point projection."""
    tri = _sur_triangle()
    ed = lr.Premium(method="ed").fit(tri).to_polars().sort(["cohort", "duration"])
    cl = lr.Premium(method="cl").fit(tri).to_polars().sort(["cohort", "duration"])
    e = ed["premium_total_se"].fill_null(0.0).to_numpy()
    c = cl["premium_total_se"].fill_null(0.0).to_numpy()
    assert not np.allclose(e, c)


def test_premium_ci_brackets_projection():
    df = lr.Premium().fit(_sur_triangle()).to_polars()
    rows = df.filter(
        pl.col("premium_ci_lo").is_not_null()
        & pl.col("premium_ci_hi").is_not_null()
        & pl.col("premium_proj").is_not_null()
    )
    assert rows.height > 0
    lo = rows["premium_ci_lo"].to_numpy()
    hi = rows["premium_ci_hi"].to_numpy()
    proj = rows["premium_proj"].to_numpy()
    assert np.all(lo <= proj + 1e-9)
    assert np.all(proj <= hi + 1e-9)


def test_premium_observed_cells_have_no_projection_uncertainty():
    """Observed cells carry the actual premium with null SE; SE is only
    populated on projected cells."""
    df = lr.Premium().fit(_sur_triangle()).to_polars()
    observed = df.filter(pl.col("premium_obs").is_not_null())
    assert observed.height > 0
    assert observed["premium_total_se"].is_null().all()


def test_premium_rejects_unknown_method():
    with pytest.raises(ValueError):
        lr.Premium(method="bogus")


def test_premium_rejects_alpha_not_one():
    with pytest.raises(NotImplementedError):
        lr.Premium(alpha=2)


def test_premium_grouped_keeps_every_group():
    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    df = lr.Premium().fit(tri).to_polars()
    assert set(df["coverage"].unique().to_list()) == {"CAN", "CI", "HOS", "SUR"}


def test_premium_mirror_pandas_in_pandas_out():
    pd = pytest.importorskip("pandas")
    df_pd = lr.load_experience().filter(pl.col("coverage") == "SUR").to_pandas()
    fit = lr.Premium().fit(lr.Triangle(df_pd))
    assert isinstance(fit.df, pd.DataFrame)
    assert isinstance(fit.to_polars(), pl.DataFrame)
