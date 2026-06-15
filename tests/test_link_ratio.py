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


# --- ODP residual bootstrap (England-Verrall) for the chain ladder ---------


def test_odp_bootstrap_populates_se_ci(exp):
    from lossratio._resample import ResidualBootstrap
    tri = lr.Triangle(exp, groups="coverage")
    d = _pl(LinkRatio(uncertainty=ResidualBootstrap(n_replicates=80, seed=1)).fit(tri))
    proj = d.filter(pl.col("source") == "own")
    assert proj["loss_total_se"].is_not_null().all()
    assert (proj["loss_total_se"] > 0).all()
    assert (proj["loss_proc_se"] <= proj["loss_total_se"] + 1e-9).all()
    assert (proj["loss_ci_lo"] <= proj["loss_proj"] + 1e-6).all()
    assert (proj["loss_proj"] <= proj["loss_ci_hi"] + 1e-6).all()
    assert proj["ratio_se"].is_not_null().all()


def test_odp_bootstrap_reproducible(exp):
    from lossratio._resample import ResidualBootstrap
    tri = lr.Triangle(exp, groups="coverage")
    a = _pl(LinkRatio(uncertainty=ResidualBootstrap(n_replicates=60, seed=5)).fit(tri))
    b = _pl(LinkRatio(uncertainty=ResidualBootstrap(n_replicates=60, seed=5)).fit(tri))
    assert (a["loss_total_se"].fill_null(-1) - b["loss_total_se"].fill_null(-1)).abs().max() == 0.0


def test_odp_bootstrap_in_calibration_range(exp):
    # the ODP bootstrap and the analytical Mack SE are different variance models
    # (ODP Var ~ mean vs Mack Var ~ C^alpha), so they need not match -- but the
    # bootstrap SE should stay in a sane band around the analytical one
    from lossratio._resample import ResidualBootstrap
    tri = lr.Triangle(exp, groups="coverage")
    ana = _pl(LinkRatio().fit(tri))
    boot = _pl(LinkRatio(uncertainty=ResidualBootstrap(n_replicates=300, seed=1, drift=False)).fit(tri))
    j = ana.join(boot, on=["coverage", "cohort", "duration"], suffix="_b").filter(
        pl.col("source") == "own"
    )
    ratio = float((j["loss_total_se_b"] / j["loss_total_se"]).median())
    assert 0.4 < ratio < 2.0


def test_analytical_default_unchanged_by_bootstrap_wiring(exp):
    # LinkRatio() with no uncertainty must still carry its analytical Mack SE
    tri = lr.Triangle(exp, groups="coverage")
    d = _pl(LinkRatio().fit(tri))
    assert d.filter(pl.col("source") == "own")["loss_total_se"].is_not_null().all()
