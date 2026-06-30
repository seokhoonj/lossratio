"""segment_wise regime treatment: the multi-regime cascade.

``Regime(treatment="segment_wise")`` keeps EVERY regime (vs ``"latest_only"``
which drops every cohort before the latest change), fits each regime on its own
cohorts (own level + own shape to its own observed depth), and borrows only the
unobservable deep tail from the pooled level-invariant link ratio of the older
(deeper) regimes. The default ``"latest_only"`` leaves all existing paths
unchanged.
"""

import polars as pl
import pytest

import lossratio as lr

CHANGE = "2024-07-01"            # a mid-span cohort (data spans 2023-01..2025-12)


def test_treatment_default_and_validation(tri):
    assert lr.Regime.at(change=CHANGE).treatment == "latest_only"
    assert lr.Regime.at(change=CHANGE, treatment="segment_wise").treatment == (
        "segment_wise"
    )
    with pytest.raises(ValueError):
        lr.Regime.at(change=CHANGE, treatment="bogus")
    # detect path carries it too -- and survives .accepted() (the evidence-
    # gated regime that actually drives a fit), for every treatment.
    reg = tri.detect_regime(target="ratio", treatment="segment_wise")
    assert reg.treatment == "segment_wise"
    for t in ("latest_only", "segment_wise", "covariate"):
        assert tri.detect_regime(target="ratio", treatment=t).accepted().treatment == t


def test_default_treatment_is_byte_identical_to_plain_latest_only(tri):
    # a default-treatment Regime must fit EXACTLY as the equivalent resolved cut
    # (the latest_only path is untouched by the segment_wise wiring).
    reg = lr.Regime.at(change=CHANGE)                 # default latest_only
    from lossratio.diagnostics.regime import _resolve_regime
    resolved = _resolve_regime(reg, tri)
    a = lr.PooledLoss(regime=reg).fit(tri).to_polars()
    b = lr.PooledLoss(regime=resolved).fit(tri).to_polars()
    assert a.equals(b)


@pytest.mark.parametrize("Est", [lr.PooledLoss, lr.CredibleLoss, lr.SmoothLoss])
def test_segment_wise_keeps_all_regimes_and_borrows_the_tail(Est, tri):
    reg_sw = lr.Regime.at(change=CHANGE, treatment="segment_wise")
    reg_lo = lr.Regime.at(change=CHANGE)             # latest_only

    sw = Est(regime=reg_sw).fit(tri).to_polars()
    lo = Est(regime=reg_lo).fit(tri).to_polars()

    n_sw = sw.select(pl.col("cohort").n_unique()).item()
    n_lo = lo.select(pl.col("cohort").n_unique()).item()
    # segment_wise retains the pre-change cohorts that latest_only drops
    assert n_sw > n_lo
    # the younger regime's deep tail is filled from the older regimes
    assert sw.filter(pl.col("source") == "borrowed").height > 0
    # clean splice: no projection gaps anywhere
    obs_proj = sw.filter(pl.col("source").is_in(["observed", "own", "borrowed"]))
    assert obs_proj.height == sw.height


@pytest.mark.parametrize("Est", [lr.PooledLoss, lr.CredibleLoss, lr.SmoothLoss])
def test_oldest_regime_equals_standalone_fit_on_its_own_cohorts(Est, tri):
    # the oldest regime has no older donor -> it is fit on its own cohorts ALONE,
    # so its projection must match a plain fit of just those cohorts (per-regime
    # level + shape preserved; no shrinkage toward the newest regime).
    df = lr.load_experience()
    reg_sw = lr.Regime.at(change=CHANGE, treatment="segment_wise")
    sw = Est(regime=reg_sw).fit(tri).to_polars()

    cut = pl.lit(CHANGE).str.to_date()
    old = df.filter(pl.col("uy_m") < cut)
    standalone = Est().fit(lr.Triangle(old, groups="coverage")).to_polars()

    key = ["coverage", "cohort", "duration"]
    j = sw.select([*key, "loss_proj"]).join(
        standalone.select([*key, pl.col("loss_proj").alias("ref")]),
        on=key, how="inner",
    )
    assert j.height > 0
    d = j.select(
        (pl.col("loss_proj") - pl.col("ref")).abs().max()
    ).item()
    assert d == pytest.approx(0.0, abs=1e-9)


def test_segment_wise_cohorts_are_globally_ascending(tri):
    reg_sw = lr.Regime.at(change=CHANGE, treatment="segment_wise")
    sw = lr.PooledLoss(regime=reg_sw).fit(tri).to_polars()
    for _cov, sub in sw.group_by("coverage"):
        # distinct cohorts must appear in ascending order (regimes are
        # date-disjoint with ascending cohorts, so the row-stack is globally
        # sorted -- what _segment_long_df's np.repeat(cohorts) relies on).
        seen = sub.get_column("cohort").to_list()
        first_seen = list(dict.fromkeys(seen))       # unique, order-preserving
        assert first_seen == sorted(first_seen)


def test_thin_newest_regime_borrows_from_the_start(tri):
    # a change near the last cohort leaves the newest regime only 1-2 cohorts
    # with shallow depth -> it must borrow from (almost) duration 1 with no crash
    # and no projection gap.
    reg = lr.Regime.at(change="2025-11-01", treatment="segment_wise")
    fit = lr.PooledLoss(regime=reg).fit(tri)
    df = fit.to_polars()
    assert df.filter(pl.col("source") == "borrowed").height > 0
    assert fit.cell_counts["unfittable"] == 0


def test_multi_regime_cascade_three_regimes(tri):
    reg = lr.Regime.at(
        change=["2024-01-01", "2024-07-01"], treatment="segment_wise"
    )
    sw = lr.PooledLoss(regime=reg).fit(tri).to_polars()
    lo = lr.PooledLoss(
        regime=lr.Regime.at(change=["2024-01-01", "2024-07-01"])
    ).fit(tri).to_polars()
    # all three regimes retained
    assert sw.select(pl.col("cohort").n_unique()).item() > (
        lo.select(pl.col("cohort").n_unique()).item()
    )
    assert sw.filter(pl.col("source") == "borrowed").height > 0


def test_segment_wise_rejects_unsupported_combinations(tri):
    reg = lr.Regime.at(change=CHANGE, treatment="segment_wise")
    with pytest.raises(NotImplementedError, match="balance"):
        lr.PooledLoss(regime=reg, balance=True).fit(tri)
    with pytest.raises(NotImplementedError, match="ResidualBootstrap"):
        lr.PooledLoss(
            regime=reg,
            uncertainty=lr.ResidualBootstrap(n_replicates=5, seed=1),
        ).fit(tri)
    with pytest.raises(NotImplementedError, match="segment_wise"):
        lr.ChainLadder(regime=reg).fit(tri)


def test_segment_wise_rejects_covariates():
    df = lr.load_experience()
    tri = lr.Triangle(df, groups=["coverage", "channel"])
    reg = lr.Regime.at(change=CHANGE, treatment="segment_wise")
    with pytest.raises(NotImplementedError, match="covariates"):
        lr.CredibleLoss(regime=reg, covariates=["channel"]).fit(tri)


def test_treatment_survives_the_deferred_detector_path(tri):
    # a DEFERRED segment_wise regime (a RegimeDetector, resolved at fit time)
    # must keep every regime and borrow the tail -- identical to the eager
    # concrete Regime. Regression: the resolution used to read `.treatment` off
    # a concrete Regime only, so a deferred/auto/callable regime silently fell
    # back to latest_only (dropped the older regimes, no borrow).
    eager = lr.Regime.at(change=CHANGE, treatment="segment_wise")
    deferred = lr.RegimeDetector(treatment="segment_wise")

    a = lr.PooledLoss(regime=eager).fit(tri).to_polars()
    b = lr.PooledLoss(regime=deferred).fit(tri).to_polars()

    # the detector finds the same planted change, so the deferred path keeps the
    # same cohorts and borrows the same tail as the eager one
    assert b.select(pl.col("cohort").n_unique()).item() == (
        a.select(pl.col("cohort").n_unique()).item()
    )
    assert b.filter(pl.col("source") == "borrowed").height > 0
