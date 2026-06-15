"""LinkRatio self-anchor — the redesigned engine-backed chain ladder must
reproduce the current ``ChainLadder`` projection bit-for-bit (charter Sec.7-3).

The link ratio ``f_k`` is driven by ``_engine.link_ratios`` and the variance /
premium machinery reuses the kept ``_mack`` kernel; this pins that the new
``LinkRatio`` -> ``LossFit`` surface is numerically identical to the old path
on every shared loss column, for single- and multi-column groups.
"""
from __future__ import annotations

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio.link_ratio import LinkRatio

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
def test_link_ratio_matches_chain_ladder(exp, groups):
    tri = lr.Triangle(exp, groups=groups)
    ref = _pl(lr.ChainLadder().fit(tri))
    got = _pl(LinkRatio().fit(tri))

    keys = (groups if isinstance(groups, list) else [groups]) + ["cohort", "duration"]
    a = ref.sort(keys)
    b = got.sort(keys)
    assert a.height == b.height

    for c in _SHARED:
        x = a[c].to_numpy().astype(float)
        y = b[c].to_numpy().astype(float)
        if c.endswith("_se") or c.endswith("_cv"):
            x = np.nan_to_num(x, nan=0.0)
            y = np.nan_to_num(y, nan=0.0)
        assert (np.isnan(x) == np.isnan(y)).all(), f"{c}: NaN pattern differs"
        m = ~np.isnan(x)
        assert np.array_equal(x[m], y[m]), f"{c}: values differ"


def test_link_ratio_model_label(exp):
    fit = LinkRatio().fit(lr.Triangle(exp, groups="coverage"))
    assert fit.model == "link_ratio"
    assert fit.method == "link_ratio"
    assert fit.status == "valid"
    assert fit.converged is True


def _assert_shared_parity(ref: pl.DataFrame, got: pl.DataFrame, keys: list[str]):
    a = ref.sort(keys)
    b = got.sort(keys)
    assert a.height == b.height
    for c in _SHARED:
        x = a[c].to_numpy().astype(float)
        y = b[c].to_numpy().astype(float)
        if c.endswith("_se") or c.endswith("_cv"):
            x = np.nan_to_num(x, nan=0.0)
            y = np.nan_to_num(y, nan=0.0)
        assert (np.isnan(x) == np.isnan(y)).all(), f"{c}: NaN pattern differs"
        m = ~np.isnan(x)
        assert np.array_equal(x[m], y[m]), f"{c}: values differ"


@pytest.mark.parametrize("groups", ["coverage", ["coverage", "age_band", "channel"]])
@pytest.mark.parametrize("recent", [6, 12])
def test_recent_matches_chain_ladder(exp, groups, recent):
    # recent gates f_k to the recent-N diagonal wedge (via the engine `include`
    # flag), projection seed from the full triangle -- must reproduce the old
    # ChainLadder(recent=N) bit-for-bit on the shared loss columns.
    tri = lr.Triangle(exp, groups=groups)
    ref = _pl(lr.ChainLadder(recent=recent).fit(tri))
    got = _pl(LinkRatio(recent=recent).fit(tri))
    keys = (groups if isinstance(groups, list) else [groups]) + ["cohort", "duration"]
    _assert_shared_parity(ref, got, keys)


def test_recent_none_matches_no_arg(exp):
    tri = lr.Triangle(exp, groups="coverage")
    a = _pl(LinkRatio().fit(tri))
    b = _pl(LinkRatio(recent=None).fit(tri))
    _assert_shared_parity(a, b, ["coverage", "cohort", "duration"])


def test_recent_validates(exp):
    with pytest.raises(ValueError):
        LinkRatio(recent=-1)
