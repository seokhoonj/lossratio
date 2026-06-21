"""Borrow option — a thin (e.g. regime-thinned) segment projects out to its own
full development horizon by borrowing the level-invariant link ratio of its OWN
full-cohort history (same-segment donor, never cross-segment).

Pins: borrow extends the horizon; the borrowed tail is driven by the segment's
own link-ratio f_k (shape, not the donor cohorts' loss-ratio level); the
borrowed region is loss-only (premium not borrowed); provenance is flagged;
borrow=False is the honest no-borrow default.
"""
from __future__ import annotations

from datetime import date

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio.pooled_loss import PooledLoss
from lossratio.chain_ladder import ChainLadder
from lossratio.credible_loss import CredibleLoss
from lossratio.smooth_loss import SmoothLoss
from lossratio._resample import ResidualBootstrap
from lossratio._recursion import _fit_multiplicative, _build_value_matrices


def _pl(obj) -> pl.DataFrame:
    d = obj.df if hasattr(obj, "df") else obj
    return d if isinstance(d, pl.DataFrame) else d.to_polars()


@pytest.fixture(scope="module")
def tri() -> lr.Triangle:
    return lr.Triangle(lr.load_experience(), groups="coverage")


CUT = date(2024, 7, 1)


@pytest.mark.parametrize(
    "estimator", [PooledLoss, ChainLadder, CredibleLoss, SmoothLoss]
)
def test_borrow_extends_horizon(tri, estimator):
    """Regime cut leaves thin post-change cohorts; borrow projects them to the
    full horizon, off leaves the short one."""
    short = _pl(estimator(regime=CUT).fit(tri))
    full = _pl(estimator(regime=CUT, borrow="pooled").fit(tri))
    assert full["duration"].max() > short["duration"].max()
    # full triangle's own horizon is 36 months
    assert full["duration"].max() == _pl(estimator().fit(tri))["duration"].max()
    # borrowed cells exist and are flagged
    assert (full["source"] == "borrowed").sum() > 0
    fit = estimator(regime=CUT, borrow="pooled").fit(tri)
    assert fit.status == "valid"
    assert fit.cell_counts["borrowed"] > 0


def test_borrow_is_loss_only(tri):
    """The borrowed tail develops loss (donor f_k), not premium -> premium and
    ratio are null on borrowed cells."""
    full = _pl(PooledLoss(regime=CUT, borrow="pooled").fit(tri))
    b = full.filter(pl.col("source") == "borrowed")
    assert b.height > 0
    assert b["loss_proj"].is_not_null().all()
    assert b["premium_proj"].null_count() == b.height
    assert b["ratio_proj"].null_count() == b.height


def test_borrowed_tail_uses_own_segment_f_k(tri):
    """The donor is the segment's OWN full-cohort link-ratio f_k (same book),
    so the borrowed step ratio loss_proj[d]/loss_proj[d-1] == own f_k."""
    cov = "CANCER"
    src = lr.load_experience()
    sub = (pl.from_pandas(src.to_pandas()) if not isinstance(src, pl.DataFrame) else src)
    sub = sub.filter(pl.col("coverage") == cov)
    (loss,), _, _ = _build_value_matrices(
        lr.Triangle(sub, groups="coverage").to_polars().sort(["cohort", "duration"]),
        value_cols=("loss",),
    )
    own_f = _fit_multiplicative(loss).f_k                       # CANCER's own full f_k

    full = _pl(PooledLoss(regime=CUT, borrow="pooled").fit(tri)).filter(
        pl.col("coverage") == cov
    ).sort(["cohort", "duration"])

    checked = 0
    for coh, g in full.group_by("cohort", maintain_order=True):
        g = g.sort("duration")
        lp = g["loss_proj"].to_numpy().astype(float)
        srcs = g["source"].to_list()
        durs = g["duration"].to_list()
        for i in range(1, len(lp)):
            if srcs[i] == "borrowed" and lp[i - 1] and np.isfinite(lp[i - 1]):
                k = durs[i] - 2                        # 0-based link into duration durs[i]
                assert np.isclose(lp[i] / lp[i - 1], own_f[k], rtol=1e-9), (
                    f"{coh} dur {durs[i]}: borrowed step != own f_k"
                )
                checked += 1
    assert checked > 0


def test_borrow_validation(tri):
    with pytest.raises(ValueError):
        PooledLoss(borrow="donor")
    with pytest.raises(ValueError):
        ChainLadder(borrow=True)  # only False | "pooled"


def test_borrow_false_is_default(tri):
    assert PooledLoss().borrow is False
    # no borrowed cells / no source=="borrowed" on the plain fit
    full = _pl(PooledLoss().fit(tri))
    assert (full["source"] == "borrowed").sum() == 0


def test_credible_borrow_collapses_to_pooled_at_psi_zero(tri):
    """The ladder-collapse invariant survives borrow: CredibleLoss(psi=0) has
    u = 1, so its borrow fit is the PooledLoss borrow fit cell-for-cell (own
    body AND borrowed tail)."""
    pool = _pl(PooledLoss(regime=CUT, borrow="pooled").fit(tri)).sort(
        ["coverage", "cohort", "duration"]
    )
    cred0 = _pl(
        CredibleLoss(regime=CUT, borrow="pooled", psi=0).fit(tri)
    ).sort(["coverage", "cohort", "duration"])
    a = pool["loss_proj"].to_numpy()
    b = cred0["loss_proj"].to_numpy()
    m = np.isfinite(a) & np.isfinite(b)
    assert m.any()
    assert np.allclose(a[m], b[m], rtol=0, atol=0)         # exact collapse
    assert pool["source"].to_list() == cred0["source"].to_list()


@pytest.mark.parametrize("estimator", [CredibleLoss, SmoothLoss])
def test_credible_smooth_body_keeps_level_tail_stays_invariant(tri, estimator):
    """The own body carries the per-cohort credibility level (so a psi=auto
    borrow fit differs from the pooled borrow body), while the borrowed tail
    stays level-invariant -- its step ratio is the segment's own full f_k,
    independent of the credibility level."""
    cov = "CANCER"
    src = lr.load_experience()
    sub = pl.from_pandas(src.to_pandas()) if not isinstance(src, pl.DataFrame) else src
    sub = sub.filter(pl.col("coverage") == cov)
    (loss,), _, _ = _build_value_matrices(
        lr.Triangle(sub, groups="coverage").to_polars().sort(["cohort", "duration"]),
        value_cols=("loss",),
    )
    own_f = _fit_multiplicative(loss).f_k

    pool = _pl(PooledLoss(regime=CUT, borrow="pooled").fit(tri))
    cred = _pl(estimator(regime=CUT, borrow="pooled", psi="auto").fit(tri))
    # the credibility level moves the body away from the pooled body
    a = pool["loss_proj"].to_numpy(); b = cred["loss_proj"].to_numpy()
    m = np.isfinite(a) & np.isfinite(b)
    assert np.nanmax(np.abs(a[m] - b[m])) > 0.0

    # ... but the borrowed tail develops on the segment's own f_k (shape only)
    full = cred.filter(pl.col("coverage") == cov).sort(["cohort", "duration"])
    checked = 0
    for _coh, g in full.group_by("cohort", maintain_order=True):
        g = g.sort("duration")
        lp = g["loss_proj"].to_numpy().astype(float)
        srcs = g["source"].to_list()
        durs = g["duration"].to_list()
        for i in range(1, len(lp)):
            if srcs[i] == "borrowed" and lp[i - 1] and np.isfinite(lp[i - 1]):
                k = durs[i] - 2
                assert np.isclose(lp[i] / lp[i - 1], own_f[k], rtol=1e-9)
                checked += 1
    assert checked > 0


def test_credible_borrow_bootstrap_covers_the_borrowed_tail(tri):
    """A ResidualBootstrap on a credible borrow fit fills the SE / CI on the
    borrowed-tail cells too (the donor process + parameter draws), so the
    data-thinnest cohort the borrow targets gets a non-degenerate band."""
    fit = CredibleLoss(
        regime=CUT, borrow="pooled",
        uncertainty=ResidualBootstrap(n_replicates=40, seed=3),
    ).fit(tri)
    assert fit.status == "valid"
    d = _pl(fit)
    b = d.filter(pl.col("source") == "borrowed")
    assert b.height > 0
    assert b["loss_ci_hi"].is_not_null().all()
    assert b["loss_total_se"].is_not_null().all()
    # a genuine band (some positive width), not a degenerate zero-width interval
    width = (b["loss_ci_hi"] - b["loss_ci_lo"]).to_numpy()
    assert np.nanmax(width) > 0.0


def test_project_borrow_interior_nan_donor_does_not_truncate():
    """An interior NaN donor link is LOCF-filled, so the borrowed tail does not
    break mid-way (the boundary-indexed switch + donor carry-forward fix)."""
    from lossratio.loss import _project_borrow

    # one cohort observed at durations 1, 2 only (boundary link 0), 5 durations.
    loss_obs = np.array([[100.0, 200.0, np.nan, np.nan, np.nan]])
    prem = np.full((1, 5), np.nan)                    # unused for link body
    nan4 = np.full(4, np.nan)
    own_f = np.array([2.0, np.nan, np.nan, np.nan])   # own covers link 0 only
    donor_f = np.array([1.5, 1.4, np.nan, 1.2])       # interior NaN at link 2
    donor_sig = np.zeros(4)
    donor_var = np.zeros(4)

    loss_proj, _, _, _, borrowed = _project_borrow(
        loss_obs, prem, body="link",
        own_g=nan4, own_sig_g=nan4, own_var_g=nan4,
        own_f=own_f, own_sig_f=np.zeros(4), own_var_f=np.zeros(4),
        donor_f=donor_f, donor_sig_f=donor_sig, donor_var_f=donor_var,
    )
    lp = loss_proj[0]
    # tail fully projected (no truncation despite the interior NaN donor link)
    assert np.all(np.isfinite(lp))
    # links beyond the boundary are borrowed
    assert borrowed[0, 2] and borrowed[0, 3] and borrowed[0, 4]
    # step at the LOCF'd link (link 2) carries link-1's donor ratio forward
    assert np.isclose(lp[3] / lp[2], 1.4)
    # first borrowed link uses donor_f[1]
    assert np.isclose(lp[2] / lp[1], 1.4)
