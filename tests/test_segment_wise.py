"""segment_wise regime treatment: the multi-regime cascade.

The estimator ``treatment="segment_wise"`` knob keeps EVERY regime (vs
``"latest_only"`` which drops every cohort before the latest change), fits each
regime on its own cohorts (own level + own shape to its own observed depth), and
borrows only the unobservable deep tail from the pooled level-invariant link
ratio of the older (deeper) regimes. The default ``"latest_only"`` leaves all
existing paths unchanged.
"""

import polars as pl
import pytest

import lossratio as lr

CHANGE = "2024-07-01"            # a mid-span cohort (data spans 2023-01..2025-12)


def test_treatment_default_and_validation(tri):
    # treatment now lives on the ESTIMATOR (default latest_only on loss), not the
    # Regime; the estimator validates it at construction.
    assert lr.PooledLoss().treatment == "latest_only"
    assert lr.PooledLoss(treatment="segment_wise").treatment == "segment_wise"
    with pytest.raises(ValueError):
        lr.PooledLoss(treatment="bogus")
    # the detect path still yields an evidence-gated regime (.accepted()) that a
    # fit can consume under every treatment -- the treatment rides on the fit.
    reg = lr.RegimeDetector(target="ratio").detect(tri)
    assert isinstance(reg, lr.Regime)
    for t in ("latest_only", "segment_wise", "covariate"):
        assert lr.CredibleLoss(treatment=t).treatment == t
        assert isinstance(
            lr.RegimeDetector(target="ratio").detect(tri).accepted(), lr.Regime
        )


def test_default_treatment_is_byte_identical_to_plain_latest_only(tri):
    # a default-treatment Regime must fit EXACTLY as the equivalent resolved cut
    # (the latest_only path is untouched by the segment_wise wiring).
    reg = lr.Regime(change=CHANGE)                    # default latest_only
    from lossratio.diagnostics.regime import _resolve_regime
    resolved = _resolve_regime(reg, tri)
    a = lr.PooledLoss(regime=reg).fit(tri).to_polars()
    b = lr.PooledLoss(regime=resolved).fit(tri).to_polars()
    assert a.equals(b)


@pytest.mark.parametrize("Est", [lr.PooledLoss, lr.CredibleLoss, lr.SmoothLoss])
def test_segment_wise_keeps_all_regimes_and_borrows_the_tail(Est, tri):
    reg = lr.Regime(change=CHANGE)

    sw = Est(regime=reg, treatment="segment_wise").fit(tri).to_polars()
    lo = Est(regime=reg).fit(tri).to_polars()        # default latest_only

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
    reg_sw = lr.Regime(change=CHANGE)
    sw = Est(regime=reg_sw, treatment="segment_wise").fit(tri).to_polars()

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
    reg_sw = lr.Regime(change=CHANGE)
    sw = lr.PooledLoss(regime=reg_sw, treatment="segment_wise").fit(tri).to_polars()
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
    reg = lr.Regime(change="2025-11-01")
    fit = lr.PooledLoss(regime=reg, treatment="segment_wise").fit(tri)
    df = fit.to_polars()
    assert df.filter(pl.col("source") == "borrowed").height > 0
    assert fit.cell_counts["unfittable"] == 0


def test_multi_regime_cascade_three_regimes(tri):
    reg = lr.Regime(change=["2024-01-01", "2024-07-01"])
    sw = lr.PooledLoss(regime=reg, treatment="segment_wise").fit(tri).to_polars()
    lo = lr.PooledLoss(
        regime=lr.Regime(change=["2024-01-01", "2024-07-01"])
    ).fit(tri).to_polars()
    # all three regimes retained
    assert sw.select(pl.col("cohort").n_unique()).item() > (
        lo.select(pl.col("cohort").n_unique()).item()
    )
    assert sw.filter(pl.col("source") == "borrowed").height > 0


def test_segment_wise_rejects_unsupported_combinations(tri):
    reg = lr.Regime(change=CHANGE)
    with pytest.raises(NotImplementedError, match="balance"):
        lr.PooledLoss(regime=reg, treatment="segment_wise", balance=True).fit(tri)
    with pytest.raises(NotImplementedError, match="ResidualBootstrap"):
        lr.PooledLoss(
            regime=reg,
            treatment="segment_wise",
            uncertainty=lr.ResidualBootstrap(n_replicates=5, seed=1),
        ).fit(tri)
    # ChainLadder segment_wise IS supported (the link-ratio f_k cascade); see
    # test_chain_ladder_segment_wise_borrows.


def test_segment_wise_rejects_covariates():
    df = lr.load_experience()
    tri = lr.Triangle(df, groups=["coverage", "channel"])
    reg = lr.Regime(change=CHANGE)
    with pytest.raises(NotImplementedError, match="covariates"):
        lr.CredibleLoss(
            regime=reg, treatment="segment_wise", covariates=["channel"]
        ).fit(tri)


def test_treatment_survives_the_deferred_detector_path(tri):
    # a DEFERRED segment_wise regime (a RegimeDetector, resolved at fit time)
    # must keep every regime and borrow the tail -- identical to the eager
    # concrete Regime. Regression: the resolution used to read `.treatment` off
    # a concrete Regime only, so a deferred/auto/callable regime silently fell
    # back to latest_only (dropped the older regimes, no borrow).
    eager = lr.Regime(change=CHANGE)
    deferred = lr.RegimeDetector()

    a = lr.PooledLoss(regime=eager, treatment="segment_wise").fit(tri).to_polars()
    b = lr.PooledLoss(regime=deferred, treatment="segment_wise").fit(tri).to_polars()

    # the detector finds the same planted change, so the deferred path keeps the
    # same cohorts and borrows the same tail as the eager one
    assert b.select(pl.col("cohort").n_unique()).item() == (
        a.select(pl.col("cohort").n_unique()).item()
    )
    assert b.filter(pl.col("source") == "borrowed").height > 0


# ---------------------------------------------------------------------------
# premium ladder segment_wise (the f^P_k cascade) + ChainLadder + the F2 close
# ---------------------------------------------------------------------------

@pytest.mark.parametrize(
    "Est", [lr.PooledPremium, lr.CrediblePremium, lr.SmoothPremium]
)
def test_premium_segment_wise_keeps_all_regimes_and_borrows(Est, tri):
    reg = lr.Regime(change=CHANGE)

    sw = Est(regime=reg, treatment="segment_wise").fit(tri).to_polars()
    lo = Est(regime=reg).fit(tri).to_polars()

    # every regime kept (vs latest_only dropping the pre-change cohorts)
    assert sw.select(pl.col("cohort").n_unique()).item() > (
        lo.select(pl.col("cohort").n_unique()).item()
    )
    # the young regime's borrow region is filled from the donor regimes
    assert sw.filter(pl.col("source") == "borrowed").height > 0
    # no projection gap -- premium is fully covered (this is what closes F2)
    assert sw.select(pl.col("premium_proj").is_null().sum()).item() == 0


@pytest.mark.parametrize(
    "Prem", [lr.PooledPremium, lr.CrediblePremium, lr.SmoothPremium]
)
def test_both_side_segment_wise_ratio_has_no_null(Prem, tri):
    # the footgun F2: a segment_wise loss keeps every cohort; if the premium
    # could only cut, the old cohorts' ratio would be null. With premium
    # segment_wise both sides keep every cohort -> zero null ratio.
    reg = lr.Regime(change=CHANGE)
    rat = lr.Ratio(
        loss=lr.CredibleLoss(regime=reg, treatment="segment_wise"),
        premium=Prem(regime=reg, treatment="segment_wise"),
    ).fit(tri).to_polars()
    assert rat.select(pl.col("ratio_proj").is_null().sum()).item() == 0


def test_premium_rejects_covariate_treatment(tri):
    reg = lr.Regime(change=CHANGE)
    with pytest.raises(ValueError, match="covariate"):
        lr.PooledPremium(regime=reg, treatment="covariate").fit(tri)


def test_chain_ladder_segment_wise_borrows(tri):
    reg = lr.Regime(change=CHANGE)
    cl = lr.ChainLadder(regime=reg, treatment="segment_wise").fit(tri).to_polars()
    assert cl.filter(pl.col("source") == "borrowed").height > 0
    assert cl.select(pl.col("loss_proj").is_null().sum()).item() == 0


_RAGGED_CHANGES = ["2023-04-01", "2023-06-01"]


def _ragged_input() -> pl.DataFrame:
    """A ragged triangle where the OLDEST regime is the SHALLOWEST.

    Consecutive monthly cohorts (so the grain infers as monthly) across three
    regimes cut at ``_RAGGED_CHANGES``: the oldest regime (2023-01..03) is
    observed only to duration 3, the middle (2023-04..05) to duration 4, the
    newest (2023-06..07) to duration 10 -- the deepest, so the segment horizon is
    10. The oldest regime stops well short of the horizon and, being oldest, has
    no older donor; the middle regime's donor (the oldest) is itself shallower
    than the horizon. This is the ragged-depth shape that used to crash the
    cascade (donor index overrun) or re-null the oldest regime's tail.
    """
    specs = [
        ("2023-01", 3), ("2023-02", 3), ("2023-03", 3),   # R0 (oldest, shallow)
        ("2023-04", 4), ("2023-05", 4),                    # R1
        ("2023-06", 10), ("2023-07", 10),                  # R2 (deepest -> horizon 10)
    ]
    rows = []
    for start, n in specs:
        y, m = (int(x) for x in start.split("-"))
        for d in range(n):
            month = (m - 1) + d
            rows.append({
                "uy_m": f"{start}-01",
                "cy_m": f"{y + month // 12:04d}-{month % 12 + 1:02d}-01",
                "incr_loss": 10.0 + d,
                "incr_premium": 100.0,
            })
    return pl.DataFrame(rows)


@pytest.mark.parametrize(
    "Est",
    [lr.PooledPremium, lr.CrediblePremium, lr.SmoothPremium,
     lr.ChainLadder, lr.PooledLoss],
)
def test_ragged_depth_oldest_shallower_no_crash_no_null(Est):
    # ragged depth: the oldest regime is the SHALLOWEST. This used to (a) crash
    # with an IndexError (a middle regime's donor is shallower than the segment
    # horizon, so donor_f indexes out of bounds) and (b) re-null the oldest
    # regime's tail (a donor-less oldest regime stops at its own depth). Every
    # cohort must now project to the horizon with no gap.
    tri = lr.Triangle(_ragged_input(), grain="M")
    reg = lr.Regime(change=_RAGGED_CHANGES)
    proj_col = "loss_proj" if Est in (lr.ChainLadder, lr.PooledLoss) else "premium_proj"
    out = Est(regime=reg, treatment="segment_wise").fit(tri).to_polars()
    assert out.select(pl.col(proj_col).is_null().sum()).item() == 0
    # the oldest cohort reaches the segment horizon (duration 10) with a value
    oldest = out.filter(pl.col("cohort") == pl.lit("2023-01-01").cast(pl.Date))
    assert oldest.select(pl.col("duration").max()).item() == 10
    assert oldest.filter(pl.col("duration") == 10).select(
        pl.col(proj_col).is_not_null().all()
    ).item()


def test_ragged_depth_both_side_ratio_has_no_null():
    # the end-to-end F2 guard on ragged depth: both sides segment_wise, oldest
    # regime shallowest -> zero null ratio.
    tri = lr.Triangle(_ragged_input(), grain="M")
    reg = lr.Regime(change=_RAGGED_CHANGES)
    rat = lr.Ratio(
        loss=lr.PooledLoss(regime=reg, treatment="segment_wise"),
        premium=lr.PooledPremium(regime=reg, treatment="segment_wise"),
    ).fit(tri).to_polars()
    assert rat.select(pl.col("ratio_proj").is_null().sum()).item() == 0


def test_project_borrow_leading_gap_and_short_donor_fill_the_tail():
    # kernel guard: a LEADING own gap (e.g. from a `recent` window that leaves the
    # early links unestimated) must fall through to the donor tail, and a donor
    # shallower than the segment horizon must LOCF-extend -- neither may leave a
    # NaN tail or index out of bounds.
    import numpy as np

    from lossratio._kernels.credible import project_borrow

    own_h = np.array([np.nan, np.nan, 0.10])        # leading gap at links 0, 1
    zero, nan3 = np.zeros(3), np.full(3, np.nan)
    loss_obs = np.array([
        [100.0, np.nan, np.nan, np.nan],            # shallow cohort (obs to dur 1)
        [100.0, 110.0, 120.0, np.nan],
    ])
    prem, u = np.full_like(loss_obs, np.nan), np.ones(2)

    def _run(donor_f):
        n = donor_f.shape[0]
        return project_borrow(
            loss_obs, prem, body="self_exposure",
            own_g=nan3, own_sig_g=nan3, own_var_g=nan3,
            own_f=nan3, own_sig_f=zero, own_var_f=zero,
            donor_f=donor_f, donor_sig_f=np.zeros(n), donor_var_f=np.zeros(n),
            own_u=u, own_h=own_h,
        )[0]

    assert not np.isnan(_run(np.array([1.2, 1.15, 1.1]))[0, 1:]).any()
    # a donor shorter than the horizon must not crash (LOCF-extends its last link)
    assert not np.isnan(_run(np.array([1.2]))[0, 1:]).any()


@pytest.mark.parametrize(
    "Est", [lr.PooledPremium, lr.CrediblePremium, lr.SmoothPremium]
)
def test_premium_no_change_segment_wise_equals_latest_only(Est, tri):
    # a segment with NO change date degenerates to one regime, no donor -> the
    # cascade must equal the plain (latest_only / no-regime) fit on those cohorts.
    far = "2099-01-01"   # later than any cohort -> no actual change
    sw = Est(regime=lr.Regime(change=far), treatment="segment_wise").fit(tri).to_polars()
    plain = Est().fit(tri).to_polars()
    key = ["coverage", "cohort", "duration"]
    j = sw.select([*key, "premium_proj"]).join(
        plain.select([*key, pl.col("premium_proj").alias("ref")]), on=key, how="inner"
    )
    assert j.height > 0
    d = j.select((pl.col("premium_proj") - pl.col("ref")).abs().max()).item()
    assert d == pytest.approx(0.0, abs=1e-9)
