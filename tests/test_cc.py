"""Tests for the CC (Cape Cod) estimator."""

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
    "elr_cc",
    "elr_cc_se",
    "elr_cc_cv",
    "elr_cc_ci_lo",
    "elr_cc_ci_hi",
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


def test_cc_summary_schema():
    fit = lr.CC().fit(_tri())
    summary = fit.summary()
    assert _SUMMARY_COLS <= set(summary.columns)
    assert summary.height > 0


def test_cc_full_schema():
    fit = lr.CC().fit(_tri())
    df = fit.df
    assert _FULL_COLS <= set(df.columns)
    assert df.height > 0


def test_cc_q_in_unit_interval():
    fit = lr.CC().fit(_tri())
    q = [v for v in fit.summary()["q"].to_list() if v is not None]
    assert q
    assert all(0.0 < v <= 1.0 + 1e-9 for v in q)


def test_cc_loss_ult_at_least_latest():
    summary = lr.CC().fit(_tri()).summary()
    for row in summary.iter_rows(named=True):
        if row["loss_ult"] is None or row["latest"] is None:
            continue
        assert row["loss_ult"] >= row["latest"] - 1e-6


def test_cc_reserve_is_ult_minus_latest():
    summary = lr.CC().fit(_tri()).summary()
    for row in summary.iter_rows(named=True):
        if None in (row["loss_ult"], row["latest"], row["reserve"]):
            continue
        assert row["reserve"] == pytest.approx(
            row["loss_ult"] - row["latest"], rel=1e-9, abs=1e-6
        )


def test_cc_repr():
    fit = lr.CC().fit(_tri())
    assert "CCFit" in repr(fit)


# ---------------------------------------------------------------------------
# Pooled ELR
# ---------------------------------------------------------------------------


def test_cc_pooled_elr_positive_and_constant_within_group():
    """The Cape Cod ELR is data-pooled per group — positive and the same
    value for every cohort of a group."""
    fit = lr.CC().fit(_tri())
    summary = fit.summary()
    elr_cc = summary["elr_cc"].unique().to_list()
    assert len(elr_cc) == 1
    assert elr_cc[0] > 0
    # `elr` (the per-cohort blend prior) equals the pooled ELR for CC.
    elr = summary["elr"].unique().to_list()
    assert len(elr) == 1
    assert elr[0] == pytest.approx(elr_cc[0])


def test_cc_elr_cc_property():
    """The `.elr_cc` property exposes the pooled ELR per group."""
    fit = lr.CC().fit(_tri())
    elr_df = fit.elr_cc
    assert "elr_cc" in elr_df.columns
    assert elr_df.height == 1  # one group
    assert elr_df["elr_cc"].to_list()[0] > 0


def test_cc_pooled_elr_per_group_independent():
    """Two groups each pool their own ELR; identical data -> identical
    pooled ELR."""
    exp = _exp_sur()
    grouped = pl.concat(
        [
            exp.with_columns(pl.lit("A").alias("coverage")),
            exp.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    fit = lr.CC().fit(lr.Triangle(grouped, groups="coverage"))
    summary = fit.summary()
    elr_a = (
        summary.filter(pl.col("coverage") == "A")["elr_cc"]
        .unique()
        .to_list()
    )
    elr_b = (
        summary.filter(pl.col("coverage") == "B")["elr_cc"]
        .unique()
        .to_list()
    )
    assert elr_a == pytest.approx(elr_b)


def test_cc_pooled_elr_uncertainty_columns():
    """The Cape Cod ELR is data-estimated, so its SE / CV / CI columns
    are populated and finite."""
    summary = lr.CC().fit(_tri()).summary()
    for c in ("elr_cc_se", "elr_cc_cv", "elr_cc_ci_lo", "elr_cc_ci_hi"):
        vals = [v for v in summary[c].to_list() if v is not None]
        assert vals
        assert all(v == v for v in vals)  # no NaN
    se = summary["elr_cc_se"].to_list()[0]
    assert se >= 0


# ---------------------------------------------------------------------------
# Error paths
# ---------------------------------------------------------------------------


def test_cc_bootstrap_flag_accepted():
    # bootstrap=True is resolved at fit time; construction must succeed.
    est = lr.CC(bootstrap=True)
    assert est.bootstrap is True


def test_cc_bootstrap_bad_dict_raises():
    # a dict missing the loss / premium BootstrapTriangle keys is
    # rejected at fit time.
    with pytest.raises(ValueError):
        lr.CC(bootstrap={"n_sim": 100}).fit(_tri())


def test_cc_bad_type_raises():
    with pytest.raises(ValueError):
        lr.CC(type="bootstrap")


def test_cc_bad_credibility_spec_raises():
    with pytest.raises(ValueError):
        lr.CC(credibility="bs")
    with pytest.raises(ValueError):
        lr.CC(credibility={"method": "lfc"})
    with pytest.raises(ValueError):
        lr.CC(credibility={"method": "bs", "K": -1.0})


def test_cc_alpha_other_raises():
    with pytest.raises(NotImplementedError):
        lr.CC(alpha=2.0)


# ---------------------------------------------------------------------------
# Credibility (Buehlmann-Straub) blend
# ---------------------------------------------------------------------------


def test_cc_credibility_runs():
    fit = lr.CC(credibility={"method": "bs"}).fit(_tri())
    assert _SUMMARY_COLS <= set(fit.summary().columns)
    assert "credibility=bs" in repr(fit)


def test_cc_credibility_shifts_green_cohorts_toward_pooled_elr():
    """Under the BS blend the green (low-q) cohort weights the pooled
    Cape Cod ELR heavily, so its blended ultimate sits closer to the
    pooled-ELR-implied ultimate than the raw CL ultimate does."""
    tri = _tri()
    cl = lr.CL().fit(tri).summary().sort("cohort").to_dicts()
    cred = (
        lr.CC(credibility={"method": "bs"})
        .fit(tri)
        .summary()
        .sort("cohort")
        .to_dicts()
    )
    full = lr.CC().fit(tri).df.sort(["cohort", "dev"])

    green_i = min(
        ((i, r["q"]) for i, r in enumerate(cred) if r["q"] is not None),
        key=lambda t: t[1],
    )[0]
    coh = cred[green_i]["cohort"]
    sub = full.filter(pl.col("cohort") == coh)
    premium_ult = sub["premium_proj"].drop_nulls().to_list()[-1]
    elr_cc = cred[green_i]["elr_cc"]
    pooled_ult = elr_cc * premium_ult

    cl_ult = cl[green_i]["loss_ult"]
    cred_ult = cred[green_i]["loss_ult"]
    assert abs(cred_ult - pooled_ult) < abs(cl_ult - pooled_ult)


# ---------------------------------------------------------------------------
# Ungrouped input
# ---------------------------------------------------------------------------


def test_cc_ungrouped():
    fit = lr.CC().fit(_tri(grouped=False))
    summary = fit.summary()
    assert _SUMMARY_COLS <= set(summary.columns)
    assert "coverage" not in summary.columns
    assert summary.height > 0


# ---------------------------------------------------------------------------
# Output-type mirroring
# ---------------------------------------------------------------------------


def test_cc_pandas_input_mirror():
    pd = pytest.importorskip("pandas")
    exp = _exp_sur().to_pandas()
    tri = lr.Triangle(exp, groups="coverage")
    fit = lr.CC().fit(tri)
    assert isinstance(fit.df, pd.DataFrame)
    assert isinstance(fit.summary(), pd.DataFrame)
    assert isinstance(fit.elr_cc, pd.DataFrame)


def test_cc_polars_input_mirror():
    fit = lr.CC().fit(_tri())
    assert isinstance(fit.df, pl.DataFrame)
    assert isinstance(fit.summary(), pl.DataFrame)


# ---------------------------------------------------------------------------
# Loss dispatcher parity
# ---------------------------------------------------------------------------


def test_loss_cc_matches_standalone_cc():
    """Loss(method='cc') produces the same numbers as the standalone CC
    estimator."""
    tri = _tri()
    standalone = lr.CC().fit(tri).summary().sort("cohort")
    dispatched = (
        lr.Loss(method="cc").fit(tri).summary().sort("cohort")
    )
    assert standalone.height == dispatched.height
    for c in (
        "loss_ult",
        "reserve",
        "elr",
        "q",
        "loss_total_se",
        "elr_cc",
        "elr_cc_se",
    ):
        for a, b in zip(standalone[c].to_list(), dispatched[c].to_list()):
            if a is None or b is None:
                continue
            assert a == pytest.approx(b, rel=1e-12, abs=1e-9)
