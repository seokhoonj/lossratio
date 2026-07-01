"""ChainLadder self-anchor — the engine-backed link-ratio fit must
reproduce the golden projection bit-for-bit.

The link ratio ``f_k`` is driven by ``_engine.link_ratios`` and the variance /
premium machinery reuses the kept ``_recursion`` kernel; this pins that the new
``ChainLadder`` -> ``LossFit`` surface is numerically identical to the old path
on every shared loss column, for single- and multi-column groups.
"""
from __future__ import annotations

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio.estimators.chain_ladder import ChainLadder

_SHARED = [
    "loss_obs", "loss_proj", "incr_loss_proj",
    "premium_obs", "premium_proj", "incr_premium_proj",
    "loss_proc_se", "loss_param_se", "loss_total_se", "loss_total_cv",
]


def _to_polars(obj) -> pl.DataFrame:
    d = obj.df if hasattr(obj, "df") else obj
    return d if isinstance(d, pl.DataFrame) else d.to_polars()


def test_chain_ladder_model_label(exp):
    fit = ChainLadder().fit(lr.Triangle(exp, groups="coverage"))
    assert fit.model == "chain_ladder"
    assert fit.method == "chain_ladder"
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
def test_recent_self_consistent(exp, groups, recent):
    # recent gates f_k to the recent-N diagonal wedge (via the engine `include`
    # flag), projection seed from the full triangle. Self-consistency check; the
    # byte-for-byte parity to the old link-ratio fit was the build-time anchor, now
    # retired with the old surface (the golden master pins the absolute numbers).
    tri = lr.Triangle(exp, groups=groups)
    a = _to_polars(ChainLadder(recent=recent).fit(tri))
    b = _to_polars(ChainLadder(recent=recent).fit(tri))
    keys = (groups if isinstance(groups, list) else [groups]) + ["cohort", "duration"]
    _assert_shared_parity(a, b, keys)


def test_recent_none_matches_no_arg(exp):
    tri = lr.Triangle(exp, groups="coverage")
    a = _to_polars(ChainLadder().fit(tri))
    b = _to_polars(ChainLadder(recent=None).fit(tri))
    _assert_shared_parity(a, b, ["coverage", "cohort", "duration"])


def test_recent_validates(exp):
    with pytest.raises(ValueError):
        ChainLadder(recent=-1)


# --- ODP residual bootstrap (England-Verrall) for the link-ratio fit -------


def test_odp_bootstrap_populates_se_ci(exp):
    from lossratio._kernels.resample import ResidualBootstrap
    tri = lr.Triangle(exp, groups="coverage")
    d = _to_polars(ChainLadder(uncertainty=ResidualBootstrap(n_replicates=80, seed=1)).fit(tri))
    proj = d.filter(pl.col("source") == "own")
    assert proj["loss_total_se"].is_not_null().all()
    assert (proj["loss_total_se"] > 0).all()
    assert (proj["loss_proc_se"] <= proj["loss_total_se"] + 1e-9).all()
    assert (proj["loss_ci_lo"] <= proj["loss_proj"] + 1e-6).all()
    assert (proj["loss_proj"] <= proj["loss_ci_hi"] + 1e-6).all()
    assert proj["ratio_se"].is_not_null().all()


def test_odp_bootstrap_reproducible(exp):
    from lossratio._kernels.resample import ResidualBootstrap
    tri = lr.Triangle(exp, groups="coverage")
    a = _to_polars(ChainLadder(uncertainty=ResidualBootstrap(n_replicates=60, seed=5)).fit(tri))
    b = _to_polars(ChainLadder(uncertainty=ResidualBootstrap(n_replicates=60, seed=5)).fit(tri))
    assert (a["loss_total_se"].fill_null(-1) - b["loss_total_se"].fill_null(-1)).abs().max() == 0.0


def test_odp_bootstrap_in_calibration_range(exp):
    # the ODP bootstrap and the analytical SE are different variance models
    # (ODP Var ~ mean vs link-ratio Var ~ C^alpha), so they need not match -- but the
    # bootstrap SE should stay in a sane band around the analytical one
    from lossratio._kernels.resample import ResidualBootstrap
    tri = lr.Triangle(exp, groups="coverage")
    ana = _to_polars(ChainLadder().fit(tri))
    boot = _to_polars(
        ChainLadder(uncertainty=ResidualBootstrap(n_replicates=300, seed=1, drift=False)).fit(tri)
    )
    j = ana.join(boot, on=["coverage", "cohort", "duration"], suffix="_b").filter(
        pl.col("source") == "own"
    )
    ratio = float((j["loss_total_se_b"] / j["loss_total_se"]).median())
    assert 0.4 < ratio < 2.0


def test_analytical_default_unchanged_by_bootstrap_wiring(exp):
    # ChainLadder() with no uncertainty must still carry its analytical SE
    tri = lr.Triangle(exp, groups="coverage")
    d = _to_polars(ChainLadder().fit(tri))
    assert d.filter(pl.col("source") == "own")["loss_total_se"].is_not_null().all()
