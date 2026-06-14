"""PooledLoss self-anchor — the redesigned engine-backed ED fit must reproduce
the current ``ExposureDriven`` projection bit-for-bit (charter Sec.7-3).

The intensity ``g_k`` is driven by ``_engine.saturated_intensity`` and the
variance / premium machinery reuses the kept ``_mack`` kernel; this pins that
the new public surface (PooledLoss -> LossFit) is numerically identical to the
old path on every shared loss column, for single- and multi-column groups.
"""
from __future__ import annotations

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio.pooled_loss import PooledLoss

# Columns both the old EDFit/LossFit and the redesigned LossFit carry, where
# byte parity is required (charter golden anchor).
_SHARED = [
    "loss_obs", "loss_proj", "incr_loss_proj",
    "premium_obs", "premium_proj", "incr_premium_proj",
    "loss_proc_se", "loss_param_se", "loss_total_se", "loss_total_cv",
]


def _pl(obj) -> pl.DataFrame:
    d = obj.df if hasattr(obj, "df") else obj
    return d if isinstance(d, pl.DataFrame) else d.to_polars()


@pytest.fixture(scope="module")
def exp() -> pl.DataFrame:
    return lr.load_experience()


@pytest.mark.parametrize(
    "groups",
    ["coverage", ["coverage", "age_band", "channel"]],
)
def test_pooled_loss_matches_exposure_driven(exp, groups):
    tri = lr.Triangle(exp, groups=groups)
    ref = _pl(lr.ExposureDriven().fit(tri))
    got = _pl(PooledLoss().fit(tri))

    keys = (groups if isinstance(groups, list) else [groups]) + ["cohort", "duration"]
    a = ref.sort(keys)
    b = got.sort(keys)
    assert a.height == b.height

    for c in _SHARED:
        x = a[c].to_numpy().astype(float)
        y = b[c].to_numpy().astype(float)
        # observed-cell SE: old wrote 0.0, new leaves null -> normalise both
        if c.endswith("_se") or c.endswith("_cv"):
            x = np.nan_to_num(x, nan=0.0)
            y = np.nan_to_num(y, nan=0.0)
        nan_match = np.isnan(x) == np.isnan(y)
        assert nan_match.all(), f"{c}: NaN pattern differs"
        m = ~np.isnan(x)
        assert np.array_equal(x[m], y[m]), f"{c}: values differ"


def test_ratio_proj_is_loss_over_premium(exp):
    got = _pl(PooledLoss().fit(lr.Triangle(exp, groups="coverage")))
    lp = got["loss_proj"].to_numpy().astype(float)
    pp = got["premium_proj"].to_numpy().astype(float)
    rp = got["ratio_proj"].to_numpy().astype(float)
    denom = np.where(pp == 0.0, np.nan, pp)
    expected = lp / denom
    m = np.isfinite(expected) & np.isfinite(rp)
    assert np.allclose(expected[m], rp[m], rtol=0, atol=0)


def test_status_fields(exp):
    fit = PooledLoss().fit(lr.Triangle(exp, groups="coverage"))
    assert fit.status == "valid"
    assert fit.status_reasons == []
    assert fit.converged is True
    assert fit.cell_counts["observed"] == 2664
    assert fit.cell_counts["projected"] > 0
    assert fit.cell_counts["unfittable"] == 0


def test_regime_cohort_cut(exp):
    from datetime import date

    tri = lr.Triangle(exp, groups="coverage")
    fit = PooledLoss(regime=date(2024, 1, 1)).fit(tri)
    df = fit.to_polars()
    assert df["cohort"].min() == date(2024, 1, 1)
    # fewer rows than the unfiltered fit (pre-2024 cohorts dropped)
    assert df.height < _pl(PooledLoss().fit(tri)).height


def test_recent_not_implemented(exp):
    with pytest.raises(NotImplementedError):
        PooledLoss(recent=6)


def test_summary_shape(exp):
    fit = PooledLoss().fit(lr.Triangle(exp, groups="coverage"))
    s = fit.summary()
    s = s if isinstance(s, pl.DataFrame) else pl.from_pandas(s)
    assert {"coverage", "cohort", "latest", "loss_proj",
            "loss_proj_remaining"}.issubset(s.columns)
    # one row per cohort x coverage
    assert s.height == fit.to_polars().select(["coverage", "cohort"]).unique().height
