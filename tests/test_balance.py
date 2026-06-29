"""Balance property (Ohlsson 2008) -- the ``balance=`` opt-in on the additive
loss ladder (PooledLoss / CredibleLoss / SmoothLoss).

One ``alpha`` per segment rescales the projected increments so the in-sample
fitted-increment total matches observed. Pooled (saturated g_k) self-balances
(alpha == 1, byte-identical); the credibility / smooth re-weighting is what
balance corrects. Tests pin: the no-op cases (default off, Pooled, psi=0
collapse), the per-segment alpha exposure, the increment-scaling semantics, and
that observed cells are never touched.
"""
from __future__ import annotations

import numpy as np
import polars as pl

from lossratio.estimators.credible_loss import CredibleLoss
from lossratio.estimators.loss import _apply_balance
from lossratio.estimators.pooled_loss import PooledLoss


def _pl(obj) -> pl.DataFrame:
    return obj if isinstance(obj, pl.DataFrame) else pl.from_pandas(obj)


def test_balance_factor_none_by_default(tri):
    assert PooledLoss().fit(tri).balance_factor is None
    assert CredibleLoss().fit(tri).balance_factor is None


def test_pooled_balance_is_noop(tri):
    """Saturated g_k already balances per duration -> alpha == 1, output is
    byte-identical to the unbalanced fit."""
    base = PooledLoss().fit(tri).df
    bal = PooledLoss(balance=True).fit(tri)
    bf = _pl(bal.balance_factor)
    assert bf.columns == ["coverage", "alpha"]
    assert (bf["alpha"] == 1.0).all()
    assert _pl(base).equals(_pl(bal.df))


def test_balance_is_noop_at_psi_zero(tri):
    """psi=0 -> u=1 -> alpha=1, so balance is a no-op: the balanced fit is
    byte-identical to the unbalanced one (the ladder nesting survives balance)."""
    c0 = _pl(CredibleLoss(psi=0).fit(tri).df)
    fit_b = CredibleLoss(psi=0, balance=True).fit(tri)
    assert c0.equals(_pl(fit_b.df))
    assert (_pl(fit_b.balance_factor)["alpha"] == 1.0).all()


def test_credible_psi_zero_point_matches_pooled(tri):
    """At psi=0 the credibility point projection collapses to pooled cell-for-cell
    (the SE block differs: pooled is analytical, credible is point-only null)."""
    pt = ["loss_proj", "incr_loss_proj", "ratio_proj", "premium_proj"]
    pooled = _pl(PooledLoss().fit(tri).df).select(pt)
    cred0 = _pl(CredibleLoss(psi=0).fit(tri).df).select(pt)
    assert pooled.equals(cred0)


def test_balance_factor_shape_and_range(tri):
    bf = _pl(CredibleLoss(balance=True).fit(tri).balance_factor)
    assert bf.columns == ["coverage", "alpha"]
    # one row per segment
    n_seg = _pl(tri.df).select("coverage").unique().height
    assert bf.height == n_seg
    assert bf["alpha"].is_finite().all()
    assert (bf["alpha"] > 0).all()


def test_observed_cells_untouched_by_balance(tri):
    """Balance rescales only the projected portion -- observed cells (and the
    premium projection) are identical with and without balance."""
    base = _pl(CredibleLoss().fit(tri).df)
    bal = _pl(CredibleLoss(balance=True).fit(tri).df)
    obs = base.filter(pl.col("loss_obs").is_not_null())
    obs_b = bal.filter(pl.col("loss_obs").is_not_null())
    assert obs.select("loss_obs").equals(obs_b.select("loss_obs"))
    # premium projection is loss-independent -> unchanged everywhere
    assert base.select("premium_proj").equals(bal.select("premium_proj"))


def test_balance_scales_projected_increment_by_alpha(tri):
    """Defining semantics: on projected (future) cells the incremental loss
    scales by the segment's alpha (premium is untouched, so ratio scales too).
    Holds for any alpha, including the alpha==1 no-op."""
    base = _pl(CredibleLoss().fit(tri).df)
    fit = CredibleLoss(balance=True).fit(tri)
    bal = _pl(fit.df)
    bf = _pl(fit.balance_factor)

    key = ["coverage", "cohort", "duration"]
    j = (
        base.select(key + ["incr_loss_proj", "loss_obs"])
        .rename({"incr_loss_proj": "incr_base"})
        .join(bal.select(key + ["incr_loss_proj"]), on=key)
        .join(bf, on="coverage")
    )
    # projected increments only (observed cells carry actuals, untouched)
    fut = j.filter(
        pl.col("loss_obs").is_null()
        & pl.col("incr_base").is_not_null()
        & (pl.col("incr_base").abs() > 1e-9)
    )
    assert fut.height > 0
    ratio = (fut["incr_loss_proj"] / fut["incr_base"]).to_numpy()
    expected = fut["alpha"].to_numpy()
    assert (abs(ratio - expected) < 1e-6).all()


def test_balance_noop_when_net_increment_nonpositive():
    """A segment whose net observed increment is <= 0 (claim reversals/recoveries)
    yields a non-positive raw alpha; balance must NOT apply it (no negative SE or
    inverted CI) -- it no-ops to alpha=1.0 and leaves the fit untouched."""
    loss_obs = np.array([[0.0, 5.0, -3.0, np.nan]])      # increments +5, -8 -> net -3
    premium_obs = np.array([[100.0, 100.0, 100.0, 100.0]])
    g_k = np.array([0.05, 0.05, 0.05])                   # positive -> s_fit > 0
    se = np.array([[np.nan, np.nan, np.nan, 1.0]])
    fit = {
        "loss_obs": loss_obs,
        "premium_obs": premium_obs,
        "g_k": g_k,
        "loss_proj": np.array([[0.0, 5.0, -3.0, 2.0]]),
        "proc_se": se.copy(),
        "param_se": se.copy(),
        "total_se": se.copy(),
    }
    ci = (np.array([[np.nan, np.nan, np.nan, 0.5]]),
          np.array([[np.nan, np.nan, np.nan, 3.5]]))
    out, out_ci, alpha = _apply_balance(fit, None, ci)
    assert alpha == 1.0
    assert out["loss_proj"][0, 3] == 2.0                      # projection untouched
    assert out["total_se"][0, 3] == 1.0                       # SE not negated
    assert out_ci[0][0, 3] == 0.5 and out_ci[1][0, 3] == 3.5  # CI not inverted
