"""LossFit.at_grain: grain-stable coarse VIEW of a finer fit.

The fit is computed once at its own grain; a coarser view is a deterministic
aggregation of the projected increments, not a re-fit. So the coarse numbers
equal this fit summed up (grain-invariant) and do NOT drift the way an
independently re-binned coarse fit does.
"""
from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


def _tri(grain):
    return lr.Triangle(lr.load_experience(), groups="coverage", grain=grain)


def _proj_total(frame, gcols=("coverage",)):
    """Portfolio projected ratio = sum of each cohort's deepest cumulative."""
    last = (
        frame.sort([*gcols, "cohort", "duration"])
        .group_by([*gcols, "cohort"])
        .agg(pl.col("loss_proj").last(), pl.col("premium_proj").last())
    )
    return last["loss_proj"].sum() / last["premium_proj"].sum()


def test_at_grain_identity_returns_fit_projection():
    fit = lr.CredibleLoss().fit(_tri("M"))
    cols = ["coverage", "cohort", "duration", "loss_proj", "incr_loss_proj",
            "premium_proj", "incr_premium_proj", "ratio_proj", "source"]
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
        for col in ("incr_loss_proj", "incr_premium_proj"):
            assert m[col].sum() == pytest.approx(q[col].sum(), rel=1e-9)


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
        .agg(pl.col("incr_loss_proj").sum().alias("il"),
             pl.col("incr_premium_proj").sum().alias("ip"))
    )
    q = fit.at_grain("Q").with_columns(
        pl.col("cohort").dt.offset_by(
            ((pl.col("duration") - 1) * 3).cast(pl.Utf8) + "mo"
        ).alias("cc")
    ).rename({"cohort": "uc"})
    j = q.join(manual, on=["coverage", "uc", "cc"], how="full", coalesce=True)
    assert j.filter(
        ((pl.col("incr_loss_proj") - pl.col("il")).abs() > 1e-6)
        | ((pl.col("incr_premium_proj") - pl.col("ip")).abs() > 1e-6)
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


def test_at_grain_carries_borrow_provenance():
    """A borrow fit's coarse view keeps borrowed cells (the increments are
    already borrow-filled), labelled 'borrowed' when any sub-cell is borrowed."""
    from datetime import date
    fit = lr.CredibleLoss(borrow="pooled", regime=lr.Regime.at(date(2024, 7, 1))).fit(_tri("M"))
    src = set(fit.at_grain("Q")["source"].unique().to_list())
    assert "borrowed" in src
