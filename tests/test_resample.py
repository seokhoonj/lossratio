"""Full-refit residual bootstrap (charter Sec.5.2): CredibleLoss SE/CI, pooled
calibration vs the analytical Mack SE, reproducibility, and the coverage lane
flowing through a backtest."""

from __future__ import annotations

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio.backtest import Backtest
from lossratio.credible_loss import CredibleLoss
from lossratio.link_ratio import LinkRatio
from lossratio.metric_panel import metric_panel
from lossratio.pooled_loss import PooledLoss
from lossratio._resample import ResidualBootstrap


def _pl(x) -> pl.DataFrame:
    return x if isinstance(x, pl.DataFrame) else pl.from_pandas(x)


@pytest.fixture(scope="module")
def tri():
    return lr.Triangle(lr.load_experience(), groups="coverage")


# --- spec validation -------------------------------------------------------


@pytest.mark.parametrize("kwargs", [
    {"n_replicates": 1},
    {"n_replicates": 1.5},
    {"min_pool": 0},
    {"process": "lognormal"},
])
def test_spec_rejects_bad_args(kwargs):
    with pytest.raises((ValueError, TypeError)):
        ResidualBootstrap(**kwargs)


def test_spec_is_pure_config():
    # frozen-style dataclass: equal specs compare equal (kw-only, free eq)
    assert ResidualBootstrap(seed=1) == ResidualBootstrap(seed=1)
    assert ResidualBootstrap(seed=1) != ResidualBootstrap(seed=2)


# --- CredibleLoss: the headline (point-only -> bootstrap fills SE/CI) -------


def test_credible_point_only_is_null(tri):
    cred = CredibleLoss().fit(tri).to_polars()
    for c in ("loss_proc_se", "loss_param_se", "loss_total_se",
              "loss_total_cv", "loss_ci_lo", "loss_ci_hi"):
        assert cred[c].is_null().all()


def test_credible_bootstrap_populates_se_and_ci(tri):
    cred = CredibleLoss(
        uncertainty=ResidualBootstrap(n_replicates=80, seed=7)
    ).fit(tri).to_polars()
    proj = cred.filter(pl.col("source") == "own")
    obs = cred.filter(pl.col("source") == "observed")

    # projected cells carry a full uncertainty quad + band
    assert proj["loss_total_se"].is_not_null().all()
    assert (proj["loss_total_se"] > 0).all()
    assert proj["loss_ci_lo"].is_not_null().all()
    # observed cells carry no projection uncertainty
    assert obs["loss_total_se"].is_null().all()
    assert obs["loss_ci_lo"].is_null().all()


def test_proc_param_total_decomposition(tri):
    cred = CredibleLoss(
        uncertainty=ResidualBootstrap(n_replicates=200, seed=2)
    ).fit(tri).to_polars()
    proj = cred.filter(pl.col("source") == "own")
    # proc_se is the law-of-total-variance residual: sqrt(max(total^2-param^2,0))
    t = proj["loss_total_se"].to_numpy()
    p = proj["loss_param_se"].to_numpy()
    proc = proj["loss_proc_se"].to_numpy()
    expect = np.sqrt(np.maximum(t ** 2 - p ** 2, 0.0))
    assert np.allclose(proc, expect, rtol=1e-9, atol=1e-6 * np.nanmax(t))
    assert (proc <= t + 1e-9).all()
    assert (t > 0).all() and (p >= 0).all()


def test_ci_brackets_the_point(tri):
    # the reported band must contain its own headline projection (bias-corrected)
    cred = CredibleLoss(
        uncertainty=ResidualBootstrap(n_replicates=150, seed=5)
    ).fit(tri).to_polars()
    proj = cred.filter((pl.col("source") == "own") & pl.col("loss_ci_lo").is_not_null())
    assert (proj["loss_ci_lo"] <= proj["loss_proj"] + 1e-9).all()
    assert (proj["loss_proj"] <= proj["loss_ci_hi"] + 1e-9).all()
    assert (proj["loss_ci_lo"] >= -1e-9).all()           # loss band floored at 0


def test_bootstrap_is_reproducible(tri):
    a = CredibleLoss(uncertainty=ResidualBootstrap(n_replicates=60, seed=11)).fit(tri).to_polars()
    b = CredibleLoss(uncertainty=ResidualBootstrap(n_replicates=60, seed=11)).fit(tri).to_polars()
    diff = (a["loss_total_se"].fill_null(-1) - b["loss_total_se"].fill_null(-1)).abs().max()
    assert diff == 0.0


def test_seed_changes_the_draw(tri):
    a = CredibleLoss(uncertainty=ResidualBootstrap(n_replicates=60, seed=11)).fit(tri).to_polars()
    b = CredibleLoss(uncertainty=ResidualBootstrap(n_replicates=60, seed=22)).fit(tri).to_polars()
    diff = (a["loss_total_se"].fill_null(0) - b["loss_total_se"].fill_null(0)).abs().max()
    assert diff > 0.0


def test_process_none_is_parameter_spread_only(tri):
    # process="none" -> no process noise -> total_se == param_se (proc = 0)
    cred = CredibleLoss(
        uncertainty=ResidualBootstrap(n_replicates=80, seed=4, process="none")
    ).fit(tri).to_polars()
    proj = cred.filter(pl.col("source") == "own")
    assert (proj["loss_proc_se"].fill_null(0) <= 1e-9).all()
    d = (proj["loss_total_se"] - proj["loss_param_se"]).abs().max()
    assert d <= 1e-9


# --- PooledLoss: calibration + non-breaking default ------------------------


def test_pooled_none_preserves_analytical(tri):
    # uncertainty=None must not perturb the byte-identical analytical fit
    a = PooledLoss().fit(tri).to_polars()
    b = PooledLoss(uncertainty=None).fit(tri).to_polars()
    for c in ("loss_total_se", "loss_proj", "loss_ci_lo"):
        assert (a[c].fill_null(-1) - b[c].fill_null(-1)).abs().max() == 0.0


def test_pooled_bootstrap_tracks_analytical(tri):
    # for the pooled rung (where Mack is valid) the bootstrap SE should sit
    # close to the analytical SE -- a calibration sanity check. Compared
    # apples-to-apples: drift=False, since the analytical Mack SE has no
    # calendar-drift term.
    a = PooledLoss().fit(tri).to_polars()
    b = PooledLoss(
        uncertainty=ResidualBootstrap(n_replicates=300, seed=1, drift=False)
    ).fit(tri).to_polars()
    j = a.join(b, on=["coverage", "cohort", "duration"], suffix="_b").filter(
        pl.col("source") == "own"
    )
    ratio = (j["loss_total_se_b"] / j["loss_total_se"]).median()
    assert 0.7 < float(ratio) < 1.5


# --- calendar drift band (Sec.5.3) -----------------------------------------


def test_drift_widens_band_but_keeps_mean(tri):
    on = CredibleLoss(
        uncertainty=ResidualBootstrap(n_replicates=200, seed=7, drift=True)
    ).fit(tri).to_polars()
    off = CredibleLoss(
        uncertainty=ResidualBootstrap(n_replicates=200, seed=7, drift=False)
    ).fit(tri).to_polars()
    j = on.join(off, on=["coverage", "cohort", "duration"], suffix="_off").filter(
        pl.col("source") == "own"
    )
    # the mean path is unchanged -- drift lives in the band, not the projection
    assert (j["loss_proj"] - j["loss_proj_off"]).abs().max() == 0.0
    # in aggregate the band is wider with drift on
    assert float((j["loss_total_se"] / j["loss_total_se_off"]).median()) > 1.0


def test_drift_grows_with_calendar_horizon(tri):
    # the drift term accumulates with calendar horizon: long durations (deep,
    # far-future calendar) inflate more than short ones
    on = CredibleLoss(
        uncertainty=ResidualBootstrap(n_replicates=200, seed=7, drift=True)
    ).fit(tri).to_polars()
    off = CredibleLoss(
        uncertainty=ResidualBootstrap(n_replicates=200, seed=7, drift=False)
    ).fit(tri).to_polars()
    j = on.join(off, on=["coverage", "cohort", "duration"], suffix="_off").filter(
        pl.col("source") == "own"
    ).with_columns(ratio=pl.col("loss_total_se") / pl.col("loss_total_se_off"))
    by_dur = j.group_by("duration").agg(pl.col("ratio").median()).sort("duration")
    early = float(by_dur.head(3)["ratio"].mean())
    late = float(by_dur.tail(3)["ratio"].mean())
    assert late > early + 0.5


# --- scope guards ----------------------------------------------------------


def test_link_ratio_rejects_bootstrap(tri):
    with pytest.raises(NotImplementedError):
        LinkRatio(uncertainty=ResidualBootstrap(n_replicates=10)).fit(tri)


def test_borrow_plus_bootstrap_rejected(tri):
    with pytest.raises(NotImplementedError):
        PooledLoss(
            borrow="pooled", uncertainty=ResidualBootstrap(n_replicates=10)
        ).fit(tri)


def test_recent_plus_bootstrap_rejected(tri):
    with pytest.raises(NotImplementedError):
        PooledLoss(
            recent=12, uncertainty=ResidualBootstrap(n_replicates=10)
        ).fit(tri)


def test_estimator_rejects_non_bootstrap_uncertainty():
    with pytest.raises(TypeError):
        PooledLoss(uncertainty="bootstrap")


# --- coverage lane through a backtest --------------------------------------


def test_coverage_lane_flows_through_backtest(tri):
    est = CredibleLoss(uncertainty=ResidualBootstrap(n_replicates=50, seed=3))
    ae = _pl(Backtest(estimator=est, holdout=6, target="loss").fit(tri).ae_err)
    assert "expected_se" in ae.columns
    panel = _pl(metric_panel(ae, groups="coverage", coverage_levels=(0.80, 0.95)))
    assert {"coverage_80", "coverage_95"}.issubset(panel.columns)
    cum = panel.filter((pl.col("lane") == "cum") & (pl.col("population") == "all"))
    cov = cum["coverage_95"].drop_nulls()
    assert cov.len() > 0
    assert ((cov >= 0.0) & (cov <= 1.0)).all()


# --- ratio band (fixed-premium) + ratio coverage lane ----------------------


def test_ratio_band_is_loss_band_over_fixed_premium(tri):
    cb = CredibleLoss(
        uncertainty=ResidualBootstrap(n_replicates=60, seed=3)
    ).fit(tri).to_polars()
    proj = cb.filter(pl.col("source") == "own")
    # premium is deterministic -> ratio SE / band = loss SE / band over premium
    assert (proj["ratio_se"] - proj["loss_total_se"] / proj["premium_proj"]).abs().max() < 1e-9
    assert (proj["ratio_ci_lo"] <= proj["ratio_proj"] + 1e-12).all()
    assert (proj["ratio_proj"] <= proj["ratio_ci_hi"] + 1e-12).all()


def test_pooled_analytical_has_ratio_se(tri):
    # the ratio band rides whatever loss SE the fit has -- analytical for pooled
    pa = PooledLoss().fit(tri).to_polars()
    assert pa.filter(pl.col("source") == "own")["ratio_se"].is_not_null().all()
    # credible point-only: no loss SE -> no ratio SE
    cred = CredibleLoss().fit(tri).to_polars()
    assert cred["ratio_se"].is_null().all()


def test_ratio_coverage_lane_flows_through_backtest(tri):
    est = CredibleLoss(uncertainty=ResidualBootstrap(n_replicates=50, seed=2))
    ae = _pl(Backtest(estimator=est, holdout=6, target="ratio").fit(tri).ae_err)
    assert "expected_se" in ae.columns
    panel = _pl(metric_panel(ae, groups="coverage", coverage_levels=(0.95,)))
    cum = panel.filter((pl.col("lane") == "cum") & (pl.col("population") == "all"))
    cov = cum["coverage_95"].drop_nulls()
    assert cov.len() > 0
    assert ((cov >= 0.0) & (cov <= 1.0)).all()
