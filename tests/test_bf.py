"""Tests for the BF (Bornhuetter-Ferguson) estimator."""

from pathlib import Path

import polars as pl
import pytest

import lossratio as lr

FIXTURES = Path(__file__).parent / "fixtures"


def _exp_sur() -> pl.DataFrame:
    """SUR-only experience slice — the same data the R fixtures use."""
    return pl.read_csv(
        FIXTURES / "experience.csv", try_parse_dates=True
    ).filter(pl.col("coverage") == "surgery")


def _tri(grouped: bool = True) -> lr.Triangle:
    exp = _exp_sur()
    if grouped:
        return lr.Triangle(exp, groups="coverage")
    return lr.Triangle(exp.drop("coverage"))


_SUMMARY_COLS = {
    "cohort",
    "latest",
    "loss_ult",
    "reserve",
    "elr",
    "q",
    "loss_total_se",
    "loss_total_cv",
    "loss_ci_lo",
    "loss_ci_hi",
}

_FULL_COLS = {
    "cohort",
    "dev",
    "loss_obs",
    "loss_proj",
    "incr_loss_proj",
    "premium_obs",
    "premium_proj",
    "incr_premium_proj",
}


# ---------------------------------------------------------------------------
# Basic fit
# ---------------------------------------------------------------------------


def test_bf_summary_schema():
    fit = lr.BF(prior=1.5).fit(_tri())
    summary = fit.summary()
    assert _SUMMARY_COLS <= set(summary.columns)
    assert summary.height > 0


def test_bf_full_schema():
    fit = lr.BF(prior=1.5).fit(_tri())
    df = fit.df
    assert _FULL_COLS <= set(df.columns)
    # n_cohorts x n_devs grid
    assert df.height > 0


def test_bf_q_in_unit_interval():
    """The emergence fraction q must lie in (0, 1]."""
    fit = lr.BF(prior=1.5).fit(_tri())
    q = [v for v in fit.summary()["q"].to_list() if v is not None]
    assert q
    assert all(0.0 < v <= 1.0 + 1e-9 for v in q)


def test_bf_loss_ult_at_least_latest():
    """BF ultimate is latest + a non-negative reserve (prior > 0)."""
    summary = lr.BF(prior=1.5).fit(_tri()).summary()
    for row in summary.iter_rows(named=True):
        if row["loss_ult"] is None or row["latest"] is None:
            continue
        assert row["loss_ult"] >= row["latest"] - 1e-6


def test_bf_reserve_is_ult_minus_latest():
    summary = lr.BF(prior=1.5).fit(_tri()).summary()
    for row in summary.iter_rows(named=True):
        if None in (row["loss_ult"], row["latest"], row["reserve"]):
            continue
        assert row["reserve"] == pytest.approx(
            row["loss_ult"] - row["latest"], rel=1e-9, abs=1e-6
        )


def test_bf_repr():
    fit = lr.BF(prior=1.5).fit(_tri())
    assert "BFFit" in repr(fit)


# ---------------------------------------------------------------------------
# Prior forms
# ---------------------------------------------------------------------------


def test_bf_scalar_prior_uniform_elr():
    """A scalar prior applies the same ELR to every cohort."""
    summary = lr.BF(prior=1.5).fit(_tri()).summary()
    elr = summary["elr"].unique().to_list()
    assert elr == [pytest.approx(1.5)]


def test_bf_per_cohort_prior_mapping():
    """A {cohort: elr} dict supplies a per-cohort prior."""
    tri = _tri()
    cohorts = tri.to_polars()["cohort"].unique().to_list()
    prior = {c: 1.0 + 0.01 * i for i, c in enumerate(sorted(cohorts))}
    fit = lr.BF(prior=prior).fit(tri)
    summary = fit.summary().sort("cohort")
    for row in summary.iter_rows(named=True):
        assert row["elr"] == pytest.approx(prior[row["cohort"]])


def test_bf_distribution_prior_pair():
    """An (elr, elr_se) pair supplies a distribution prior; the elr_se
    feeds Var(ELR) so the analytical SE strictly exceeds the
    deterministic-prior SE."""
    tri = _tri()
    cohorts = sorted(tri.to_polars()["cohort"].unique().to_list())
    det_prior = {c: 1.5 for c in cohorts}
    dist_prior = {c: (1.5, 0.3) for c in cohorts}

    det = lr.BF(prior=det_prior).fit(tri).summary().sort("cohort")
    dist = lr.BF(prior=dist_prior).fit(tri).summary().sort("cohort")

    # Point estimates identical (same ELR mean).
    for a, b in zip(det["loss_ult"].to_list(), dist["loss_ult"].to_list()):
        if a is None or b is None:
            continue
        assert a == pytest.approx(b, rel=1e-9, abs=1e-6)
    # Prior uncertainty inflates the SE on at least one projected cohort.
    det_se = [v for v in det["loss_total_se"].to_list() if v]
    dist_se = [v for v in dist["loss_total_se"].to_list() if v]
    assert any(d > s for d, s in zip(dist_se, det_se))


def test_bf_prior_missing_cohort_raises():
    tri = _tri()
    cohorts = sorted(tri.to_polars()["cohort"].unique().to_list())
    # drop one cohort key -> incomplete per-cohort mapping
    bad = {c: 1.5 for c in cohorts[1:]}
    with pytest.raises(ValueError):
        lr.BF(prior=bad).fit(tri)


# ---------------------------------------------------------------------------
# Error paths
# ---------------------------------------------------------------------------


def test_bf_no_prior_raises():
    """BF() takes `prior` as a required argument."""
    with pytest.raises(TypeError):
        lr.BF()


def test_bf_negative_prior_raises():
    with pytest.raises(ValueError):
        lr.BF(prior=-1.0).fit(_tri())


def test_bf_zero_prior_raises():
    with pytest.raises(ValueError):
        lr.BF(prior=0.0).fit(_tri())


def test_bf_bootstrap_raises_not_implemented():
    with pytest.raises(NotImplementedError):
        lr.BF(prior=1.5, bootstrap=True)


def test_bf_bootstrap_dict_raises_not_implemented():
    with pytest.raises(NotImplementedError):
        lr.BF(prior=1.5, bootstrap={"n_sim": 100})


def test_bf_non_analytical_type_raises_not_implemented():
    with pytest.raises(NotImplementedError):
        lr.BF(prior=1.5, type="bootstrap")


def test_bf_bad_credibility_spec_raises():
    # not a dict
    with pytest.raises(ValueError):
        lr.BF(prior=1.5, credibility="bs")
    # unknown method
    with pytest.raises(ValueError):
        lr.BF(prior=1.5, credibility={"method": "lfc"})
    # negative K
    with pytest.raises(ValueError):
        lr.BF(prior=1.5, credibility={"method": "bs", "K": -1.0})


# ---------------------------------------------------------------------------
# Credibility (Buehlmann-Straub) blend
# ---------------------------------------------------------------------------


def test_bf_credibility_runs():
    fit = lr.BF(prior=1.5, credibility={"method": "bs"}).fit(_tri())
    assert _SUMMARY_COLS <= set(fit.summary().columns)
    assert "credibility=bs" in repr(fit)


def test_bf_credibility_shifts_green_cohorts_toward_prior():
    """Under the BS blend the green (low-q) cohort weights the prior
    heavily, so its blended ultimate sits much closer to the
    prior-implied ultimate (`prior * premium_ult`) than the raw CL
    ultimate does."""
    tri = _tri()
    prior = 1.5
    cl = lr.CL().fit(tri).summary().sort("cohort").to_dicts()
    cred = (
        lr.BF(prior=prior, credibility={"method": "bs"})
        .fit(tri)
        .summary()
        .sort("cohort")
        .to_dicts()
    )
    full = lr.BF(prior=prior).fit(tri).df.sort(["cohort", "dev"])

    # greenest cohort = smallest emergence fraction q
    green_i = min(
        ((i, r["q"]) for i, r in enumerate(cred) if r["q"] is not None),
        key=lambda t: t[1],
    )[0]
    coh = cred[green_i]["cohort"]
    sub = full.filter(pl.col("cohort") == coh)
    premium_ult = sub["premium_proj"].drop_nulls().to_list()[-1]
    prior_ult = prior * premium_ult

    cl_ult = cl[green_i]["ultimate"]
    cred_ult = cred[green_i]["loss_ult"]
    # The credibility blend pulls the green cohort toward the prior.
    assert abs(cred_ult - prior_ult) < abs(cl_ult - prior_ult)


# ---------------------------------------------------------------------------
# Ungrouped input
# ---------------------------------------------------------------------------


def test_bf_ungrouped():
    fit = lr.BF(prior=1.5).fit(_tri(grouped=False))
    summary = fit.summary()
    assert _SUMMARY_COLS <= set(summary.columns)
    assert "coverage" not in summary.columns
    assert summary.height > 0


# ---------------------------------------------------------------------------
# Output-type mirroring
# ---------------------------------------------------------------------------


def test_bf_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    exp = _exp_sur().to_pandas()
    tri = lr.Triangle(exp, groups="coverage")
    fit = lr.BF(prior=1.5).fit(tri)
    assert isinstance(fit.df, pd.DataFrame)
    assert isinstance(fit.summary(), pd.DataFrame)


def test_bf_polars_input_mirror():
    fit = lr.BF(prior=1.5).fit(_tri())
    assert isinstance(fit.df, pl.DataFrame)
    assert isinstance(fit.summary(), pl.DataFrame)


# ---------------------------------------------------------------------------
# Loss dispatcher parity
# ---------------------------------------------------------------------------


def test_loss_bf_no_prior_raises():
    with pytest.raises(ValueError):
        lr.Loss(method="bf")


def test_loss_bf_matches_standalone_bf():
    """Loss(method='bf', prior=...) produces the same numbers as the
    standalone BF estimator."""
    tri = _tri()
    standalone = lr.BF(prior=1.5).fit(tri).summary().sort("cohort")
    dispatched = (
        lr.Loss(method="bf", prior=1.5).fit(tri).summary().sort("cohort")
    )
    assert standalone.height == dispatched.height
    for c in ("loss_ult", "reserve", "elr", "q", "loss_total_se"):
        for a, b in zip(standalone[c].to_list(), dispatched[c].to_list()):
            if a is None or b is None:
                continue
            assert a == pytest.approx(b, rel=1e-12, abs=1e-9)


def test_bf_cell_projection_holds_latest_on_flat_cl():
    """`_bf_cell_projection`: an unobserved cell where the inner chain
    ladder is flat (`cl_remainder` ~ 0) is held at `loss_latest`, not
    collapsed to `loss_ult` -- mirrors R's `fit_bf()` cell block. Guards
    the edge branch the surgery parity fixtures never exercise.
    """
    import numpy as np

    from lossratio.bf import _bf_cell_projection

    # One cohort, 4 devs. Observed devs 0-1; latest = 100.
    loss_obs     = np.array([[80.0, 100.0, np.nan, np.nan]])
    is_observed  = np.array([[True, True, False, False]])
    loss_latest  = np.array([100.0])
    loss_ult     = np.array([200.0])
    # Inner CL: flat at dev 2 (cl_remainder = 0), develops at dev 3.
    loss_proj_cl = np.array([[80.0, 100.0, 100.0, 150.0]])
    premium_proj = np.array([[50.0, 60.0, 70.0, 80.0]])

    loss_proj, _, _ = _bf_cell_projection(
        loss_obs, loss_proj_cl, premium_proj,
        is_observed, loss_latest, loss_ult,
    )

    assert loss_proj[0, 0] == 80.0    # observed
    assert loss_proj[0, 1] == 100.0   # observed
    assert loss_proj[0, 2] == 100.0   # flat CL -> held at loss_latest
    assert loss_proj[0, 3] == 200.0   # CL develops -> loss_ult
