"""SmoothLoss -- top ladder rung: smooth shape + credibility.

The credible rung with the saturated per-duration intensity replaced by a
penalized smooth shape, fit by backfitting. Tests pin the result
schema, the point-only SE, the credibility diagnostics, the ladder relations
(psi=0 -> single smooth pass; smooth tracks credible modestly), and the config
guards.
"""

from __future__ import annotations

import polars as pl
import pytest

from lossratio.estimators.credible_loss import CredibleLoss
from lossratio.estimators.smooth_loss import SmoothLoss


def _to_polars(x) -> pl.DataFrame:
    return x if isinstance(x, pl.DataFrame) else pl.from_pandas(x)


def test_smooth_fit_runs_and_labels(tri):
    fit = SmoothLoss().fit(tri)
    assert fit.model == "smooth_loss"
    assert fit.status in ("valid", "degraded")
    assert fit.converged is True
    d = fit.to_polars()
    assert d.filter(pl.col("source") == "own")["loss_proj"].is_not_null().all()
    assert {"ratio_proj", "ratio_se"}.issubset(d.columns)


def test_point_only_se_is_null(tri):
    d = SmoothLoss().fit(tri).to_polars()
    for c in ("loss_proc_se", "loss_param_se", "loss_total_se",
              "loss_ci_lo", "loss_ci_hi"):
        assert d[c].is_null().all()


def test_credibility_diagnostics_exposed(tri):
    c = _to_polars(SmoothLoss().fit(tri).credibility)
    assert c.columns == ["coverage", "cohort", "u", "Z", "psi"]
    assert (c["u"] >= 0).all()
    assert ((c["Z"] >= 0) & (c["Z"] <= 1)).all()


def test_psi_zero_is_single_smooth_pass(tri):
    # psi = 0 -> no credibility -> u = 1 for every cohort (one smooth pass)
    c = _to_polars(SmoothLoss(psi=0).fit(tri).credibility)
    assert (c["u"] == 1.0).all()
    assert (c["Z"] == 0.0).all()


def test_smooth_tracks_credible_modestly(tri):
    # the smooth shape regularises the saturated per-duration g_k, so SmoothLoss
    # departs from CredibleLoss but only modestly (a smoother shape, same level)
    sm = SmoothLoss().fit(tri).to_polars()
    cr = CredibleLoss().fit(tri).to_polars()
    j = sm.join(cr, on=["coverage", "cohort", "duration"], suffix="_c").filter(
        pl.col("source") == "own"
    )
    rel = ((j["loss_proj"] - j["loss_proj_c"]) / j["loss_proj_c"]).abs()
    assert float(rel.median()) < 0.05          # close to credible
    assert float(rel.max()) < 1.0              # never wildly off (regression)


def test_fixed_lambda_and_basis(tri):
    fit = SmoothLoss(lam=1e6, n_basis=8).fit(tri)
    assert fit.converged
    assert fit.to_polars().filter(
        pl.col("source") == "own"
    )["loss_proj"].is_not_null().all()


def test_multi_group(tri):
    # already grouped by coverage; a credibility row per cohort x coverage
    c = _to_polars(SmoothLoss().fit(tri).credibility)
    n = SmoothLoss().fit(tri).to_polars().select(
        ["coverage", "cohort"]
    ).unique().height
    assert c.height == n


def test_config_guards(tri):
    # recent (re-estimates factors on the recent diagonals) is supported.
    full = SmoothLoss().fit(tri).to_polars()
    rec = SmoothLoss(recent=12).fit(tri).to_polars()
    assert not full.equals(rec)
    assert full.equals(SmoothLoss(recent=None).fit(tri).to_polars())


def test_bootstrap_populates_se_ci_and_coverage(tri):
    from lossratio._kernels.resample import ResidualBootstrap
    from lossratio.diagnostics.backtest import Backtest
    from lossratio._kernels.scorecard import score_cells
    est = SmoothLoss(uncertainty=ResidualBootstrap(n_replicates=15, seed=7))
    d = est.fit(tri).to_polars()
    proj = d.filter(pl.col("source") == "own")
    assert proj["loss_total_se"].is_not_null().all()
    assert (proj["loss_total_se"] > 0).all()
    assert (proj["loss_proc_se"] <= proj["loss_total_se"] + 1e-9).all()
    assert (proj["loss_ci_lo"] <= proj["loss_proj"] + 1e-9).all()
    assert (proj["loss_proj"] <= proj["loss_ci_hi"] + 1e-9).all()
    assert proj["ratio_se"].is_not_null().all()
    # coverage lane flows through a backtest
    ae = _to_polars(Backtest(estimator=est, holdouts=6, target="loss").fit(tri).ae_err)
    assert "expected_se" in ae.columns
    panel = _to_polars(score_cells(ae, groups="coverage", coverage_levels=(0.95,)))
    cum = panel.filter((pl.col("lane") == "cumulative") & (pl.col("population") == "all"))
    cov = cum["coverage_95"].drop_nulls()
    assert cov.len() > 0 and ((cov >= 0) & (cov <= 1)).all()


def test_bootstrap_reproducible(tri):
    from lossratio._kernels.resample import ResidualBootstrap
    a = SmoothLoss(uncertainty=ResidualBootstrap(n_replicates=12, seed=4)).fit(tri).to_polars()
    b = SmoothLoss(uncertainty=ResidualBootstrap(n_replicates=12, seed=4)).fit(tri).to_polars()
    assert (a["loss_total_se"].fill_null(-1) - b["loss_total_se"].fill_null(-1)).abs().max() == 0.0


@pytest.mark.parametrize("kwargs", [
    {"psi": -1.0}, {"lam": -1.0}, {"n_basis": 2}, {"n_basis": 3.5},
])
def test_bad_params_rejected(kwargs):
    with pytest.raises((ValueError, TypeError)):
        SmoothLoss(**kwargs)
