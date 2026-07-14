"""LossFit.at_grain: grain-stable coarse VIEW of a finer fit.

The fit is computed once at its own grain; a coarser view is a deterministic
aggregation of the projected increments, not a re-fit. So the coarse numbers
equal this fit summed up (grain-invariant) and do NOT drift the way an
independently re-binned coarse fit does.
"""
from __future__ import annotations

from datetime import date

import polars as pl
import pytest

import lossratio as lr
from lossratio._kernels.period import sum_increments_to_grain


def _tri(grain):
    return lr.Triangle(lr.load_experience(), groups="coverage", grain=grain)


def _proj_total(frame, group_cols=("coverage",)):
    """Portfolio projected loss = sum of each cohort's deepest cumulative."""
    last = (
        frame.sort([*group_cols, "cohort", "duration"])
        .group_by([*group_cols, "cohort"])
        .agg(pl.col("loss_proj").last())
    )
    return last["loss_proj"].sum()


def test_at_grain_identity_returns_fit_projection():
    fit = lr.CredibleLoss().fit(_tri("M"))
    cols = ["coverage", "cohort", "duration", "loss_proj", "incr_loss_proj",
            "source"]
    a = fit.at_grain("M").sort(["coverage", "cohort", "duration"])
    b = fit.to_polars().select(cols).sort(["coverage", "cohort", "duration"])
    assert a.equals(b)


def test_at_grain_preserves_increment_totals():
    """Aggregation conserves mass: the summed projected increments at the coarse
    grain equal those at the fit grain, per group."""
    fit = lr.CredibleLoss().fit(_tri("M"))
    m = fit.to_polars()
    for g in ("Q", "H", "Y"):
        q = fit.at_grain(g)
        assert m["incr_loss_proj"].sum() == pytest.approx(
            q["incr_loss_proj"].sum(), rel=1e-9
        )


def test_at_grain_is_grain_invariant_and_differs_from_refit():
    """The coarse VIEW equals the fine fit summed up (invariant across target
    grains), and differs from an independently re-binned coarse fit."""
    fitM = lr.CredibleLoss().fit(_tri("M"))
    base = _proj_total(fitM.to_polars())
    for g in ("Q", "H", "Y"):
        assert _proj_total(fitM.at_grain(g)) == pytest.approx(base, rel=1e-9)
    # an independent coarse fit re-estimates on coarsened data -> it drifts,
    # so the at_grain view is genuinely different machinery (and the stable one).
    refit_Q = _proj_total(lr.CredibleLoss().fit(_tri("Q")).to_polars())
    assert refit_Q != pytest.approx(base, rel=1e-6)


def test_at_grain_matches_manual_increment_aggregation():
    """at_grain('Q') is exactly the fit's monthly increments floored to quarter
    (cohort + calendar), summed, re-cumulated -- the documented mechanism."""
    fit = lr.CredibleLoss().fit(_tri("M"))
    m = fit.to_polars()
    manual = (
        m.with_columns(
            pl.col("cohort").dt.offset_by(
                ((pl.col("duration") - 1)).cast(pl.Utf8) + "mo"
            ).alias("_cal")
        )
        .with_columns(
            pl.col("cohort").dt.truncate("3mo").alias("uc"),
            pl.col("_cal").dt.truncate("3mo").alias("cc"),
        )
        .group_by(["coverage", "uc", "cc"])
        .agg(pl.col("incr_loss_proj").sum().alias("il"))
    )
    q = fit.at_grain("Q").with_columns(
        pl.col("cohort").dt.offset_by(
            ((pl.col("duration") - 1) * 3).cast(pl.Utf8) + "mo"
        ).alias("cc")
    ).rename({"cohort": "uc"})
    j = q.join(manual, on=["coverage", "uc", "cc"], how="full", coalesce=True)
    assert j.filter(
        (pl.col("incr_loss_proj") - pl.col("il")).abs() > 1e-6
    ).height == 0


def test_at_grain_observed_only_when_all_subcells_observed():
    """A coarse cell is 'observed' only if every finer sub-cell is observed."""
    fit = lr.CredibleLoss().fit(_tri("M"))
    m = fit.to_polars()
    q = fit.at_grain("Q")
    # coarse observed-cell count must not exceed the count implied by requiring
    # all 3 monthly sub-cells observed (a strict subset of any-observed)
    n_coarse_obs = q.filter(pl.col("source") == "observed").height
    n_any_obs_cells = (
        m.with_columns(
            pl.col("cohort").dt.truncate("3mo").alias("uc"),
            pl.col("cohort").dt.offset_by(((pl.col("duration") - 1)).cast(pl.Utf8) + "mo")
              .dt.truncate("3mo").alias("cc"),
        )
        .group_by(["coverage", "uc", "cc"])
        .agg((pl.col("source") == "observed").all().alias("all_obs"))
        .filter(pl.col("all_obs")).height
    )
    assert n_coarse_obs == n_any_obs_cells


def test_at_grain_guards():
    fit = lr.CredibleLoss().fit(_tri("Q"))
    with pytest.raises(ValueError, match="grain must be one of"):
        fit.at_grain("X")
    with pytest.raises(ValueError, match="COARSER"):
        fit.at_grain("M")          # finer than the Q fit


def test_at_grain_carries_graft_provenance():
    """A segment_wise fit's coarse view keeps grafted cells (the increments are
    already graft-filled), labelled 'grafted' when any sub-cell is grafted."""
    fit = lr.CredibleLoss(
        regime=lr.Regime(date(2024, 7, 1)), treatment="segment_wise"
    ).fit(_tri("M"))
    src = set(fit.at_grain("Q")["source"].unique().to_list())
    assert "grafted" in src


def test_sum_increments_to_grain_null_source_gap_is_not_observed():
    """Regression (62cf96a): a coarse cell containing a null-source GAP sub-cell
    must NOT be labelled 'observed'. polars ``.all()`` ignores nulls, so without
    the ``fill_null(False)`` guard the middle gap below would vanish and the
    quarter would be mislabelled observed."""
    # one quarter = three monthly sub-cells; the middle one is a null-source gap.
    df = pl.DataFrame({
        "coverage": ["A", "A", "A"],
        "cohort": [date(2024, 1, 1)] * 3,
        "duration": [1, 2, 3],
        "incr_loss_proj": [10.0, 20.0, 30.0],
        "source": ["observed", None, "observed"],
    })
    out = sum_increments_to_grain(
        df, group_cols=["coverage"], from_grain="M", to_grain="Q",
        incr_col="incr_loss_proj", cum_col="loss_proj",
    )
    assert out.height == 1                      # the three months fold to one quarter
    assert out["source"][0] != "observed"       # the gap forbids 'observed'
    assert out["source"][0] == "own"            # not all observed, none grafted
    assert out["incr_loss_proj"][0] == pytest.approx(60.0)  # mass conserved


def _loss_ratio_fit(grain):
    return lr.LossRatio(loss=lr.PooledLoss()).fit(_tri(grain))


def test_loss_ratio_at_grain_recomposes_loss_over_premium():
    """LossRatioFit.at_grain sums both legs to the coarse grain and recomposes
    ratio_proj = loss_proj / premium_proj, with no infinities from the
    known-exposure denominator (null/0 premium -> null ratio)."""
    fit = _loss_ratio_fit("M")
    q = fit.at_grain("Q")
    priced = q.filter(pl.col("premium_proj") > 0.0)
    assert priced.height > 0
    for ratio, loss, prem in zip(
        priced["ratio_proj"], priced["loss_proj"], priced["premium_proj"],
        strict=True,
    ):
        assert ratio == pytest.approx(loss / prem, rel=1e-9)
    # known exposure -> the ratio never blows up to +/-inf
    assert not q["ratio_proj"].is_infinite().any()
    # where a premium cell is null, the ratio is null (never a fabricated number)
    missing_premium = q.filter(pl.col("premium_proj").is_null())
    assert missing_premium["ratio_proj"].is_null().all()


def test_loss_ratio_at_grain_conserves_mass_and_identity():
    """The composed coarse view is a deterministic aggregation of the fit: the
    identity grain returns it unchanged, and every coarser grain conserves the
    portfolio projected loss."""
    fit = _loss_ratio_fit("M")
    native = fit.to_polars()
    cols = ["coverage", "cohort", "duration", "loss_proj", "premium_proj",
            "ratio_proj", "source"]
    ident = fit.at_grain("M").sort(cols[:3])
    assert ident.equals(native.select(cols).sort(cols[:3]))
    base = _proj_total(native)
    for g in ("Q", "H", "Y"):
        assert _proj_total(fit.at_grain(g)) == pytest.approx(base, rel=1e-9)


def test_loss_ratio_at_grain_rejects_unknown_and_finer_grain():
    fit = _loss_ratio_fit("Q")
    with pytest.raises(ValueError, match="grain must be one of"):
        fit.at_grain("X")
    with pytest.raises(ValueError, match="COARSER"):
        fit.at_grain("M")          # finer than the Q fit


def test_premium_at_grain_identity_and_conserves_mass():
    """PremiumFit.at_grain is the denominator analogue of LossFit.at_grain: the
    identity grain returns the fit unchanged and every coarser grain conserves the
    projected premium increment total."""
    fit = lr.PooledPremium().fit(_tri("M"))
    native = fit.to_polars()
    cols = ["coverage", "cohort", "duration", "premium_proj",
            "incr_premium_proj", "source"]
    ident = fit.at_grain("M").sort(cols[:3])
    assert ident.equals(native.select(cols).sort(cols[:3]))
    for g in ("Q", "H", "Y"):
        coarse = fit.at_grain(g)
        assert native["incr_premium_proj"].sum() == pytest.approx(
            coarse["incr_premium_proj"].sum(), rel=1e-9
        )


def test_premium_at_grain_guards():
    fit = lr.PooledPremium().fit(_tri("Q"))
    with pytest.raises(ValueError, match="grain must be one of"):
        fit.at_grain("X")
    with pytest.raises(ValueError, match="COARSER"):
        fit.at_grain("M")
