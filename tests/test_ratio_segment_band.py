"""Tests for the regime go-forward band (curve leg + spread).

The band is additive: it must never change the default segment_summary
output, the projection, or the golden master. These tests prove the
default surface is byte-identical and the new band frame is honest about
young-segment uncertainty.

Uses only the bundled ``lr.load_experience()`` data plus small synthetic
regime triangles built in-test (never reads ``~/Dropbox/private``).
"""

from __future__ import annotations

import datetime as dt

import numpy as np
import polars as pl
import pytest
from polars.testing import assert_frame_equal

import lossratio as lr


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


def _experience_triangle() -> "lr.Triangle":
    return lr.Triangle(
        lr.load_experience(),
        groups="coverage",
        cohort="uy_m",
        calendar="cy_m",
    )


def _regime_fit() -> "lr.RatioFit":
    """An SA ratio fit with an auto-detected loss regime (SUR has a change)."""
    return lr.Ratio(method="sa", loss_regime="auto").fit(_experience_triangle())


def _synthetic_young_regime():
    """A tiny synthetic triangle whose recent segment is short (young).

    Two coverages, monthly cohorts. The recent segment (cohorts on/after a
    manual change) has only a couple of developed durations, so its own
    intensity series is short -> under-determined curve fit.
    """
    rng = np.random.default_rng(7)
    rows: list[dict] = []
    # 12 monthly cohorts; change at month 10 -> recent segment = 3 cohorts.
    cohorts = [dt.date(2023, m, 1) for m in range(1, 13)]
    max_duration = 12
    for cov in ("A", "B"):
        base_lr = 0.5 if cov == "A" else 0.6
        for ci, coh in enumerate(cohorts):
            # younger cohorts observe fewer durations (right-triangle).
            n_obs = max_duration - ci
            prem_step = 1_000.0
            cum_loss = 0.0
            cum_prem = 0.0
            for d in range(1, n_obs + 1):
                cal_m = coh.month + (d - 1)
                cal_y = coh.year + (cal_m - 1) // 12
                cal_m = (cal_m - 1) % 12 + 1
                cum_prem += prem_step
                # additive intensity decaying with duration
                g = base_lr * np.exp(-0.2 * d) + 0.01
                cum_loss += g * (cum_prem - (cum_prem - prem_step))
                rows.append(
                    dict(
                        coverage=cov,
                        uy_m=coh,
                        cy_m=dt.date(cal_y, cal_m, 1),
                        incr_loss=g * prem_step + rng.normal(0, 1),
                        incr_premium=prem_step,
                    )
                )
    df = pl.DataFrame(rows)
    return lr.Triangle(df, groups="coverage", cohort="uy_m", calendar="cy_m")


# ---------------------------------------------------------------------------
# 1. Default output unchanged (additive guarantee)
# ---------------------------------------------------------------------------


def test_default_output_unchanged():
    fit = _regime_fit()

    # segment_summary snapshot before any band call.
    seg_before = fit.segment_summary()
    df_before = fit.to_polars().clone()

    # A band call must not mutate anything.
    _ = fit.segment_band()

    seg_after = fit.segment_summary()
    df_after = fit.to_polars()

    assert_frame_equal(seg_before, seg_after)
    assert_frame_equal(df_before, df_after)

    # segment_summary keeps exactly its 7 (+group) columns.
    assert seg_after.columns == [
        "coverage",
        "segment",
        "change_from",
        "n_cohorts",
        "loss_proj",
        "premium_proj",
        "ratio_proj",
    ]


# ---------------------------------------------------------------------------
# 2. Borrow leg byte-equal to segment_summary
# ---------------------------------------------------------------------------


def test_borrow_leg_byte_equal():
    fit = _regime_fit()
    band = fit.segment_band()
    seg = fit.segment_summary()

    # Recent-segment rows of segment_summary: change_from non-null and the
    # max change date per group. For SUR that is the single segment "1".
    seg_recent = seg.filter(pl.col("change_from").is_not_null())

    joined = band.join(
        seg_recent,
        on=["coverage", "change_from"],
        how="inner",
        suffix="_seg",
    )
    assert joined.height == band.height == seg_recent.height
    for col, seg_col in [
        ("loss_proj_borrow", "loss_proj"),
        ("ratio_proj_borrow", "ratio_proj"),
        ("premium_proj", "premium_proj_seg"),
    ]:
        a = joined[col].to_numpy()
        b = joined[seg_col].to_numpy()
        assert np.allclose(a, b, rtol=0, atol=0), col


# ---------------------------------------------------------------------------
# 3. Both legs land and the spread is small when the recent segment agrees
# ---------------------------------------------------------------------------


def test_band_legs_present_when_mature():
    """Both legs land on the SUR recent segment, agree, and earn "narrow".

    The two tail methods (borrow / curve) land close on the mature-ish SUR
    recent segment, and the curve fit is data-sufficient (n_points >= the
    floor), so the agreement is genuine -- the status is the spread-driven
    ``"narrow"``. (The curve's own divergence / alt-law swing are reported
    on their own columns; they qualify the curve leg but do not override a
    real two-leg agreement.)
    """
    fit = _regime_fit()
    band = fit.segment_band().filter(pl.col("coverage") == "SUR")
    assert band.height == 1
    row = band.row(0, named=True)
    # Both legs land on the mature-ish SUR recent segment.
    assert row["ratio_proj_borrow"] is not None
    assert row["ratio_proj_curve"] is not None
    assert row["curve_reason"] == "ok"
    assert row["curve_n_points"] >= 3
    # The two ultimate loss ratios are close (the two-leg spread is small).
    assert abs(row["ratio_proj_curve"] - row["ratio_proj_borrow"]) < 0.05
    # band_lo / band_hi bracket both legs.
    assert row["band_lo"] <= row["ratio_proj_borrow"] <= row["band_hi"]
    assert row["band_lo"] <= row["ratio_proj_curve"] <= row["band_hi"]
    assert row["band_width"] == pytest.approx(row["band_hi"] - row["band_lo"])
    # Data-sufficient agreement -> the honesty flag earns "narrow".
    assert not row["curve_under_determined"]
    assert row["band_status"] == "narrow"


def _synthetic_mature_regime():
    """A synthetic triangle whose recent segment is mature and well-determined.

    Two coverages, monthly cohorts. The change is placed early (month 13)
    so the recent segment carries many cohorts, the earliest observing
    ~24 durations -> a long, over-determined own intensity series. The recent
    segment decays steeply (slope safely below the ``-1`` divergence
    boundary, so the curve does not flag diverged), while the donor (old)
    segment decays more slowly. The two development shapes differ enough
    that the borrow-vs-curve spread (``band_width``) comfortably exceeds
    the alt-law model-choice swing -> a genuine ``band_status == "narrow"``.
    """
    rng = np.random.default_rng(11)
    rows: list[dict] = []
    cohorts = [
        dt.date(2021 + (m - 1) // 12, (m - 1) % 12 + 1, 1) for m in range(1, 37)
    ]
    change = dt.date(2022, 1, 1)  # month 13 -> recent segment = 24 cohorts.
    max_duration = 36
    old_decay, new_decay = 1.3, 2.0
    for cov in ("A", "B"):
        base_lr = 0.5 if cov == "A" else 0.6
        for ci, coh in enumerate(cohorts):
            decay = new_decay if coh >= change else old_decay
            n_obs = max_duration - ci
            prem_step = 1_000.0
            for d in range(1, n_obs + 1):
                cal_index = (coh.year - 2021) * 12 + (coh.month - 1) + (d - 1)
                cal_y = 2021 + cal_index // 12
                cal_m = cal_index % 12 + 1
                # Smooth inverse-power-like decaying intensity, tiny noise.
                g = base_lr * (d ** -decay) + 0.001
                rows.append(
                    dict(
                        coverage=cov,
                        uy_m=coh,
                        cy_m=dt.date(cal_y, cal_m, 1),
                        incr_loss=g * prem_step + rng.normal(0, 0.02),
                        incr_premium=prem_step,
                    )
                )
    df = pl.DataFrame(rows)
    return lr.Triangle(df, groups="coverage", cohort="uy_m", calendar="cy_m")


def test_band_status_narrow_on_well_determined_curve():
    """Pin the ``"narrow"`` verdict on a well-determined, agreeing curve.

    Covers the final ``return "narrow"`` branch of ``_band_status`` on a
    mature recent segment: the curve fit is not under-determined, not
    diverged, the alternate law does not diverge, and the alt-law swing is
    within the two-leg spread -- so the honesty flag must read ``"narrow"``.
    """
    tri = _synthetic_mature_regime()
    fit = lr.Ratio(
        method="ed", loss_regime=lr.Regime.at(change=dt.date(2022, 1, 1))
    ).fit(tri)
    band = fit.segment_band()
    assert band.height >= 1
    narrow_rows = [
        r for r in band.iter_rows(named=True) if r["band_status"] == "narrow"
    ]
    # At least one group earns the narrow verdict (the branch is exercised).
    assert narrow_rows, (
        "expected at least one narrow band_status on a mature, "
        "well-determined recent segment"
    )
    for r in narrow_rows:
        # narrow is earned by data sufficiency, not a lucky small width.
        assert r["curve_under_determined"] is False
        assert r["curve_diverged"] is False
        assert r["ratio_proj_curve"] is not None
        assert r["band_width"] is not None
        # The alt-law swing (if present) is within the two-leg spread.
        if r["curve_alt_ratio_proj"] is not None:
            assert (
                abs(r["curve_alt_ratio_proj"] - r["ratio_proj_curve"])
                <= r["band_width"]
            )


def _synthetic_divergent_regime():
    """A regime whose two tail legs DISAGREE on a data-sufficient curve.

    The recent segment is young enough to need a tail (its oldest cohort
    reaches ~12 of 36 durations) but carries enough cohorts that its own
    intensity series is over-determined (not under-determined). Its own
    intensity decays *shallowly* (slope ~ -0.7), so the curve leg
    extrapolates a heavy tail -> a high curve ultimate; the donor decays
    *steeply* (slope ~ -2.5), so the borrowed f_k adds little tail -> a low
    borrow ultimate. The two legs diverge by far more than 10% of the
    level -> the honesty flag must read ``"wide"`` even though the curve is
    fully determined.
    """
    rng = np.random.default_rng(7)
    rows: list[dict] = []
    cohorts = [
        dt.date(2021 + (m - 1) // 12, (m - 1) % 12 + 1, 1) for m in range(1, 37)
    ]
    change = dt.date(2023, 1, 1)  # month 25 -> recent segment = 12 cohorts.
    max_duration = 36
    old_decay, new_decay = 2.5, 0.7  # donor steep (small tail), recent shallow
    for cov in ("A", "B"):
        base_lr = 0.6 if cov == "A" else 0.7
        for ci, coh in enumerate(cohorts):
            decay = new_decay if coh >= change else old_decay
            n_obs = max_duration - ci
            prem_step = 1_000.0
            for d in range(1, n_obs + 1):
                cal_index = (coh.year - 2021) * 12 + (coh.month - 1) + (d - 1)
                cal_y = 2021 + cal_index // 12
                cal_m = cal_index % 12 + 1
                g = base_lr * (d ** -decay) + 0.001
                rows.append(
                    dict(
                        coverage=cov,
                        uy_m=coh,
                        cy_m=dt.date(cal_y, cal_m, 1),
                        incr_loss=g * prem_step + rng.normal(0, 0.02),
                        incr_premium=prem_step,
                    )
                )
    df = pl.DataFrame(rows)
    return lr.Triangle(df, groups="coverage", cohort="uy_m", calendar="cy_m")


def test_band_status_wide_on_divergent_determined_legs():
    """Wide verdict driven by leg DISAGREEMENT, not data insufficiency.

    The recent segment's own intensity (shallow decay -> heavy curve tail)
    disagrees with the borrowed donor shape (steep -> light tail) by far
    more than 10% of the level. The curve is fully determined (not
    under-determined, n_points well above the floor), so the only thing
    making it ``"wide"`` is the genuine two-leg spread -- the spread-driven
    branch of ``_band_status``.
    """
    tri = _synthetic_divergent_regime()
    fit = lr.Ratio(
        method="ed", loss_regime=lr.Regime.at(change=dt.date(2023, 1, 1))
    ).fit(tri)
    band = fit.segment_band()
    wide = [
        r for r in band.iter_rows(named=True)
        if r["band_status"] == "wide" and not r["curve_under_determined"]
    ]
    assert wide, "expected a wide band from determined but disagreeing legs"
    for r in wide:
        assert r["curve_n_points"] >= 3
        assert r["ratio_proj_curve"] is not None
        # The spread alone clears the narrow threshold (10% of the level).
        assert r["band_width"] / abs(r["ratio_proj_borrow"]) > 0.10


# ---------------------------------------------------------------------------
# 4. Wide band on a young / under-determined recent segment
# ---------------------------------------------------------------------------


def test_band_wide_when_under_determined():
    tri = _synthetic_young_regime()
    change = dt.date(2023, 10, 1)
    fit = lr.Ratio(
        method="ed", loss_regime=lr.Regime.at(change=change)
    ).fit(tri)
    band = fit.segment_band()
    assert band.height >= 1
    # Use min_points high enough that the short recent segment is flagged.
    band_strict = fit.segment_band(
        curve=lr.Curve(target="intensity", min_points=20)
    )
    for r in band_strict.iter_rows(named=True):
        if r["ratio_proj_curve"] is not None:
            assert r["curve_under_determined"] is True
            assert r["band_status"] == "wide"
            # The point is still emitted alongside the flag.
            assert r["band_width"] is not None


# ---------------------------------------------------------------------------
# 5. Under-determined point emitted WITH the flag
# ---------------------------------------------------------------------------


def test_under_determined_flagged():
    fit = _regime_fit()
    # Force under-determined by demanding far more points than exist.
    band = fit.segment_band(
        curve=lr.Curve(target="intensity", min_points=99)
    ).filter(pl.col("coverage") == "SUR")
    row = band.row(0, named=True)
    assert row["curve_under_determined"] is True
    assert row["band_status"] == "wide"
    # The curve point is NOT silently dropped.
    assert row["ratio_proj_curve"] is not None
    assert row["curve_n_points"] >= 2


# ---------------------------------------------------------------------------
# 6. Degenerate honesty -- null curve, borrow preserved
# ---------------------------------------------------------------------------


def test_degenerate_honesty():
    """A recent segment with no usable decaying link -> degenerate curve.

    Build a synthetic where the recent segment has all-flat (non-positive
    increment) intensity so g_k has no decaying region / no positive
    values -> CurveResult.slope is None.
    """
    rows: list[dict] = []
    cohorts = [dt.date(2023, m, 1) for m in (10, 11, 12)]
    for ci, coh in enumerate(cohorts):
        n_obs = 3 - ci + 1
        for d in range(1, max(n_obs, 1) + 1):
            cal_m = coh.month + (d - 1)
            cal_y = coh.year + (cal_m - 1) // 12
            cal_m = (cal_m - 1) % 12 + 1
            rows.append(
                dict(
                    coverage="X",
                    uy_m=coh,
                    cy_m=dt.date(cal_y, cal_m, 1),
                    incr_loss=0.0,  # zero increments -> non-positive g_k
                    incr_premium=1_000.0,
                )
            )
    df = pl.DataFrame(rows)
    tri = lr.Triangle(df, groups="coverage", cohort="uy_m", calendar="cy_m")
    fit = lr.Ratio(
        method="ed", loss_regime=lr.Regime.at(change=dt.date(2023, 11, 1))
    ).fit(tri)
    band = fit.segment_band()
    if band.height == 0:
        pytest.skip("synthetic produced no recent segment row")
    row = band.row(0, named=True)
    assert row["band_status"] == "degenerate"
    assert row["ratio_proj_curve"] is None
    assert row["loss_proj_curve"] is None
    assert row["band_lo"] is None
    assert row["band_hi"] is None
    assert row["band_width"] is None
    assert row["curve_reason"] in (
        "empty",
        "non_positive",
        "no_decaying_region",
    )
    # Borrow leg still present (NaN must not masquerade as a number).
    assert row["ratio_proj_borrow"] is not None
    # The mean leg falls back to the borrow leg (never null), so the single
    # computable headline number survives a degenerate curve.
    assert row["ratio_proj_mean"] == row["ratio_proj_borrow"]
    assert row["loss_proj_mean"] == row["loss_proj_borrow"]


def test_ratio_proj_mean_is_midpoint():
    """``ratio_proj_mean`` / ``loss_proj_mean`` are the midpoint of the borrow
    and curve legs -- a single computable headline number (the band rides
    alongside on its own columns).
    """
    fit = _regime_fit()
    band = fit.segment_band().filter(pl.col("coverage") == "SUR")
    row = band.row(0, named=True)
    assert row["ratio_proj_curve"] is not None  # SUR curve is well-defined
    assert row["ratio_proj_mean"] == pytest.approx(
        (row["ratio_proj_borrow"] + row["ratio_proj_curve"]) / 2.0
    )
    assert row["loss_proj_mean"] == pytest.approx(
        (row["loss_proj_borrow"] + row["loss_proj_curve"]) / 2.0
    )
    # The mean lies inside the band.
    assert row["band_lo"] <= row["ratio_proj_mean"] <= row["band_hi"]


# ---------------------------------------------------------------------------
# 7. No regime -> empty typed frame
# ---------------------------------------------------------------------------


def test_no_regime_graceful():
    tri = _experience_triangle()
    fit = lr.Ratio(method="sa", loss_regime=None).fit(tri)
    band = fit.segment_band()
    assert band.height == 0
    # Correct schema even when empty.
    assert "ratio_proj_borrow" in band.columns
    assert "band_status" in band.columns
    assert "coverage" in band.columns


def test_only_regime_groups_present():
    """Groups without a change are absent; SUR (with a change) is present."""
    fit = _regime_fit()
    band = fit.segment_band()
    covs = set(band["coverage"].to_list())
    assert covs == {"SUR"}


# ---------------------------------------------------------------------------
# 8. Determinism
# ---------------------------------------------------------------------------


def test_determinism():
    fit = _regime_fit()
    b1 = fit.segment_band()
    b2 = fit.segment_band()
    assert_frame_equal(b1, b2)

    # A min_points bump changes only flag columns, never the borrow leg.
    b3 = fit.segment_band(curve=lr.Curve(target="intensity", min_points=99))
    assert_frame_equal(
        b1.select(["coverage", "ratio_proj_borrow", "loss_proj_borrow"]),
        b3.select(["coverage", "ratio_proj_borrow", "loss_proj_borrow"]),
    )


# ---------------------------------------------------------------------------
# 9. target='ata' rejected
# ---------------------------------------------------------------------------


def test_target_ata_rejected():
    fit = _regime_fit()
    with pytest.raises(ValueError, match="target='intensity'"):
        fit.segment_band(curve=lr.Curve(target="ata"))


# ---------------------------------------------------------------------------
# 10. Mirror output (pandas in -> pandas out)
# ---------------------------------------------------------------------------


def test_mirror_output_pandas():
    pd = pytest.importorskip("pandas")
    df = lr.load_experience()
    df_pd = df.to_pandas() if isinstance(df, pl.DataFrame) else df
    tri = lr.Triangle(df_pd, groups="coverage", cohort="uy_m", calendar="cy_m")
    fit = lr.Ratio(method="sa", loss_regime="auto").fit(tri)
    band = fit.segment_band()
    assert isinstance(band, pd.DataFrame)


def test_mirror_output_polars():
    fit = _regime_fit()
    band = fit.segment_band()
    assert isinstance(band, pl.DataFrame)


# ---------------------------------------------------------------------------
# 11. Auto-grain tail extrapolation (opt-in; default path untouched)
# ---------------------------------------------------------------------------
#
# The fresh-regime go-forward is grain-UNSTABLE: the OBSERVED segment loss
# ratio is grain-invariant, but the projected tail diverges because the
# Curve fits the intensity decay against the development INDEX, not real
# time. On an immature fine grain (monthly) the recovered slope is above the
# convergence boundary (an unphysical, divergent tail) that over-projects;
# coarsening to a more mature grain (quarterly) recovers a convergent slope
# and a real, narrow band. ``auto_grain=True`` auto-selects the finest grain
# where BOTH the slope is physical AND the two legs agree.


def _insufficient_fresh_regime():
    """A regime so fresh that no grain converges (the insufficient state).

    A single coverage, monthly cohorts, with the change placed so the
    recent segment is only four cohorts. The recent segment decays very
    shallowly (its monthly index-fit does not agree with the borrowed donor
    shape) and the coarse grains have too few points to fit -- so no grain
    fires both selection signals and the band is ``"insufficient"``.
    """
    rng = np.random.default_rng(5)
    rows: list[dict] = []
    cohorts = [
        dt.date(2022 + (m - 1) // 12, (m - 1) % 12 + 1, 1) for m in range(1, 25)
    ]
    change = cohorts[24 - 4]  # recent segment = 4 cohorts.
    for ci, coh in enumerate(cohorts):
        base = 0.5 if coh >= change else 1.0
        decay = 0.5 if coh >= change else 2.0
        n_obs = 24 - ci
        for d in range(1, n_obs + 1):
            cal_index = (coh.year - 2022) * 12 + (coh.month - 1) + (d - 1)
            cal_y = 2022 + cal_index // 12
            cal_m = cal_index % 12 + 1
            g = base * (d ** -decay) + 0.001
            rows.append(
                dict(
                    coverage="A",
                    uy_m=coh,
                    cy_m=dt.date(cal_y, cal_m, 1),
                    incr_loss=g * 1_000.0 + rng.normal(0, 0.5),
                    incr_premium=1_000.0,
                )
            )
    df = pl.DataFrame(rows)
    tri = lr.Triangle(df, groups="coverage", cohort="uy_m", calendar="cy_m")
    return tri, change


def test_auto_grain_off_byte_identical():
    """``auto_grain=False`` is byte-identical to the bare default call."""
    fit = _regime_fit()
    default = fit.segment_band()
    off = fit.segment_band(auto_grain=False)
    assert_frame_equal(default, off)
    # The OFF columns are exactly today's set -- no ``selected_grain``.
    assert default.columns == [
        "coverage",
        "segment",
        "change_from",
        "n_cohorts",
        "premium_proj",
        "loss_proj_borrow",
        "ratio_proj_borrow",
        "loss_proj_curve",
        "ratio_proj_curve",
        "loss_proj_mean",
        "ratio_proj_mean",
        "band_lo",
        "band_hi",
        "band_width",
        "band_status",
        "curve_n_points",
        "curve_under_determined",
        "curve_reason",
        "curve_diverged",
        "curve_alt_ratio_proj",
    ]
    assert "selected_grain" not in default.columns


def test_auto_grain_selects_coarse_when_fine_diverges():
    """The fine grain's divergent curve is vetoed; a coarser grain is picked.

    On the bundled experience data the SUR recent segment is immature at the
    monthly display grain -- its curve slope is above the convergence
    boundary (``curve_diverged=True``), an unphysical tail, even though the
    monthly two-leg spread is small. Signal (a) (slope physicality) vetoes
    that jointly-wrong narrow band, and the auto-grain walk coarsens to the
    finest grain where the slope is physical AND the legs agree.
    """
    fit = _regime_fit()
    on = fit.segment_band(auto_grain=True).filter(pl.col("coverage") == "SUR")
    assert on.height == 1
    row = on.row(0, named=True)
    # A coarser grain than the monthly display grain was selected.
    assert row["selected_grain"] in {"Q", "H", "Y"}
    assert row["band_status"] in {"narrow", "wide"}
    assert row["band_status"] != "insufficient"

    # The selected grain is physical (its curve did not diverge), unlike the
    # vetoed monthly grain.
    off = fit.segment_band().filter(pl.col("coverage") == "SUR").row(0, named=True)
    assert off["curve_diverged"] is True  # the fine-grain veto fired.
    assert row["curve_diverged"] is False

    # The auto-grain ultimate equals a direct re-fit at the selected grain
    # (selected-grain values verbatim, no interpolation back to fine grain).
    from lossratio.regime import _coarsen_triangle

    tri_g = _coarsen_triangle(fit._triangle, row["selected_grain"])
    direct = fit._estimator.fit(tri_g).segment_band()
    direct = direct.filter(pl.col("coverage") == "SUR").row(0, named=True)
    assert row["ratio_proj_borrow"] == pytest.approx(direct["ratio_proj_borrow"])
    assert row["ratio_proj_curve"] == pytest.approx(direct["ratio_proj_curve"])


def test_auto_grain_observed_lr_grain_invariant():
    """The OBSERVED recent-segment loss ratio is identical at M and Q.

    The empirical invariant the whole design rests on: re-binning to a
    coarser grain changes only the unobserved tail, never the observed
    increments, so the recent segment's observed loss / premium ratio is
    grain-invariant. (All the M-vs-Q divergence is therefore tail-only.)
    """
    from lossratio.regime import _coarsen_triangle

    fit = _regime_fit()
    change = fit._regime._changes_df.filter(pl.col("coverage") == "SUR")
    change_from = max(change["change"].to_list())

    def _observed_lr(grain: str) -> float:
        # Portfolio observed loss ratio of the recent segment = sum over
        # cohorts of the LAST observed cumulative loss / premium. (A naive
        # sum of every cumulative cell would double-count the development
        # axis differently per grain; the per-cohort latest is the real,
        # grain-invariant observed aggregate.)
        f = fit if grain == fit._triangle.grain else (
            fit._estimator.fit(_coarsen_triangle(fit._triangle, grain))
        )
        df = f.to_polars()
        df = df.with_columns(pl.col("cohort").cast(pl.Date))
        sub = df.filter(
            (pl.col("coverage") == "SUR")
            & (pl.col("cohort") >= change_from)
        )
        latest = sub.sort("duration").group_by("cohort").agg(
            pl.col("loss_obs").drop_nulls().last().alias("loss"),
            pl.col("premium_obs").drop_nulls().last().alias("premium"),
        )
        return float(latest["loss"].sum()) / float(latest["premium"].sum())

    assert _observed_lr("M") == pytest.approx(_observed_lr("Q"), rel=1e-9)


def test_auto_grain_insufficient_fallback():
    """No grain converges -> ``"insufficient"`` + borrow-only headline."""
    tri, change = _insufficient_fresh_regime()
    fit = lr.Ratio(
        method="ed", loss_regime=lr.Regime.at(change=change)
    ).fit(tri)
    on = fit.segment_band(auto_grain=True)
    assert on.height == 1
    row = on.row(0, named=True)
    assert row["band_status"] == "insufficient"
    assert row["selected_grain"] is None
    # Curve / band columns are nulled -- a non-converged tail is never a point.
    assert row["loss_proj_curve"] is None
    assert row["ratio_proj_curve"] is None
    assert row["band_lo"] is None
    assert row["band_hi"] is None
    assert row["band_width"] is None
    # Headline falls back to the robust borrow leg (never null).
    assert row["ratio_proj_borrow"] is not None
    assert row["ratio_proj_mean"] == row["ratio_proj_borrow"]
    assert row["loss_proj_mean"] == row["loss_proj_borrow"]


def test_auto_grain_insufficient_distinct_from_degenerate():
    """``"insufficient"`` and ``"degenerate"`` are distinct band states.

    ``degenerate`` (default path) = no curve fit at all at the display
    grain; ``insufficient`` (auto path) = a curve existed at >= 1 grains
    but none passed the dual signal.
    """
    # degenerate fixture (from the default-path degenerate test).
    rows: list[dict] = []
    cohorts = [dt.date(2023, m, 1) for m in (10, 11, 12)]
    for ci, coh in enumerate(cohorts):
        n_obs = 3 - ci + 1
        for d in range(1, max(n_obs, 1) + 1):
            cal_m = coh.month + (d - 1)
            cal_y = coh.year + (cal_m - 1) // 12
            cal_m = (cal_m - 1) % 12 + 1
            rows.append(
                dict(
                    coverage="X",
                    uy_m=coh,
                    cy_m=dt.date(cal_y, cal_m, 1),
                    incr_loss=0.0,
                    incr_premium=1_000.0,
                )
            )
    deg_tri = lr.Triangle(
        pl.DataFrame(rows), groups="coverage", cohort="uy_m", calendar="cy_m"
    )
    deg_fit = lr.Ratio(
        method="ed", loss_regime=lr.Regime.at(change=dt.date(2023, 11, 1))
    ).fit(deg_tri)
    deg_band = deg_fit.segment_band(auto_grain=True)
    deg_status = (
        set(deg_band["band_status"].to_list()) if deg_band.height else set()
    )

    ins_tri, ins_change = _insufficient_fresh_regime()
    ins_fit = lr.Ratio(
        method="ed", loss_regime=lr.Regime.at(change=ins_change)
    ).fit(ins_tri)
    ins_status = set(
        ins_fit.segment_band(auto_grain=True)["band_status"].to_list()
    )

    assert "insufficient" in ins_status
    assert "insufficient" not in deg_status


def test_auto_grain_schema_superset():
    """ON columns == OFF columns + ``selected_grain`` (same prefix order)."""
    fit = _regime_fit()
    off = fit.segment_band()
    on = fit.segment_band(auto_grain=True)
    assert on.columns == [*off.columns, "selected_grain"]
    assert on.schema["selected_grain"] == pl.Utf8
    # Shared columns keep identical dtypes (a strict superset, not a recast).
    for col in off.columns:
        assert on.schema[col] == off.schema[col]


def test_auto_grain_empty_frame_typed():
    """No-regime fit with ``auto_grain=True`` -> 0 rows, typed superset."""
    tri = _experience_triangle()
    fit = lr.Ratio(method="sa", loss_regime=None).fit(tri)
    on = fit.segment_band(auto_grain=True)
    assert on.height == 0
    assert "selected_grain" in on.columns
    assert on.schema["selected_grain"] == pl.Utf8
    assert "band_status" in on.columns


def test_auto_grain_mirror_output_pandas():
    pd = pytest.importorskip("pandas")
    df = lr.load_experience()
    df_pd = df.to_pandas() if isinstance(df, pl.DataFrame) else df
    tri = lr.Triangle(df_pd, groups="coverage", cohort="uy_m", calendar="cy_m")
    fit = lr.Ratio(method="sa", loss_regime="auto").fit(tri)
    on = fit.segment_band(auto_grain=True)
    assert isinstance(on, pd.DataFrame)
    assert "selected_grain" in on.columns


def test_auto_grain_mirror_output_polars():
    fit = _regime_fit()
    on = fit.segment_band(auto_grain=True)
    assert isinstance(on, pl.DataFrame)


def test_auto_grain_determinism():
    """The auto-grain selection has no RNG; two calls are byte-identical."""
    fit = _regime_fit()
    b1 = fit.segment_band(auto_grain=True)
    b2 = fit.segment_band(auto_grain=True)
    assert_frame_equal(b1, b2)


# ---------------------------------------------------------------------------
# 8. Developing path (segment_path) -- the duration-by-duration companion to the band
# ---------------------------------------------------------------------------

_PATH_COLS = [
    "duration",
    "ratio_borrow",
    "ratio_curve",
    "ratio_mean",
    "band_lo",
    "band_hi",
    "observed",
]


def test_segment_path_columns_off_vs_on():
    """OFF: the base path columns. ON: a strict superset with selected_grain."""
    fit = _regime_fit()
    off = fit.segment_path()
    on = fit.segment_path(auto_grain=True)
    assert [c for c in off.columns if c != "coverage"] == _PATH_COLS
    assert [c for c in on.columns if c != "coverage"] == _PATH_COLS + [
        "selected_grain"
    ]


def test_segment_path_no_regime_is_empty_and_typed():
    """A fit with no regime yields a zero-row, correctly-typed path frame."""
    tri = _experience_triangle()
    fit = lr.Ratio(method="ed").fit(tri)  # no regime detection
    p = fit.segment_path(auto_grain=True)
    assert p.height == 0
    assert "selected_grain" in p.columns
    assert "ratio_borrow" in p.columns


def test_segment_path_only_regime_groups():
    """Only groups with a regime change produce rows (latest-only, like band)."""
    fit = _regime_fit()
    p = fit.segment_path(auto_grain=True)
    band = fit.segment_band(auto_grain=True)
    band = band if isinstance(band, pl.DataFrame) else pl.from_pandas(band)
    assert sorted(p["coverage"].unique().to_list()) == sorted(
        band["coverage"].unique().to_list()
    )


def test_segment_path_is_additive():
    """Calling segment_path never mutates df / summary / segment_summary."""
    fit = _regime_fit()
    df0 = fit.to_polars().clone()
    sm0 = fit.summary().clone()
    seg0 = fit.segment_summary().clone()
    _ = fit.segment_path()
    _ = fit.segment_path(auto_grain=True)
    assert_frame_equal(fit.to_polars(), df0)
    assert_frame_equal(fit.summary(), sm0)
    assert_frame_equal(fit.segment_summary(), seg0)


def test_segment_path_deterministic():
    fit = _regime_fit()
    p1 = fit.segment_path(auto_grain=True)
    p2 = fit.segment_path(auto_grain=True)
    assert_frame_equal(p1, p2)


@pytest.mark.parametrize("auto", [False, True])
def test_segment_path_ultimate_matches_band(auto):
    """The last path row of each leg equals segment_band's reported ultimate.

    Both reduce to ``sum loss_proj / sum premium_proj`` at the ultimate duration,
    so the developing path and the headline band can never disagree.
    """
    fit = _regime_fit()
    band = fit.segment_band(auto_grain=auto)
    band = band if isinstance(band, pl.DataFrame) else pl.from_pandas(band)
    path = fit.segment_path(auto_grain=auto)
    for cov in band["coverage"].unique().to_list():
        b = band.filter(pl.col("coverage") == cov).row(0, named=True)
        last = (
            path.filter(pl.col("coverage") == cov)
            .sort("duration")
            .tail(1)
            .row(0, named=True)
        )
        assert last["ratio_borrow"] == pytest.approx(b["ratio_proj_borrow"])
        assert last["ratio_mean"] == pytest.approx(b["ratio_proj_mean"])
        if b["ratio_proj_curve"] is not None:
            assert last["ratio_curve"] == pytest.approx(b["ratio_proj_curve"])
            assert last["band_lo"] == pytest.approx(b["band_lo"])
            assert last["band_hi"] == pytest.approx(b["band_hi"])
        else:
            assert last["ratio_curve"] is None


def test_segment_path_observed_region_grain_invariant():
    """Observed rows equal the fine-grain recent-segment aggregate exactly.

    The observed region is always at the display grain, so it must reproduce
    ``sum(loss_proj) / sum(premium_proj)`` over the recent cohorts cell for
    cell -- the auto-grain tail never re-bins the observed detail.
    """
    fit = _regime_fit()
    path = fit.segment_path(auto_grain=True)
    df = fit.to_polars().filter(pl.col("coverage") == "SUR")
    change = fit._regime._changes_df.row(0, named=True)["change"]
    recent = df.filter(pl.col("cohort") >= change)
    agg = (
        recent.group_by("duration")
        .agg(
            (pl.col("loss_proj").sum() / pl.col("premium_proj").sum()).alias(
                "r"
            )
        )
        .sort("duration")
    )
    obs = path.filter(
        (pl.col("coverage") == "SUR") & pl.col("observed")
    ).select(["duration", "ratio_borrow"])
    chk = obs.join(agg, on="duration")
    assert chk.height == obs.height
    assert (chk["ratio_borrow"] - chk["r"]).abs().max() == pytest.approx(0.0)


def test_segment_path_observed_then_tail_structure():
    """Observed rows have a collapsed band; tail rows have a bracketing band."""
    fit = _regime_fit()
    p = fit.segment_path(auto_grain=True).filter(pl.col("coverage") == "SUR")
    obs = p.filter(pl.col("observed"))
    tail = p.filter(~pl.col("observed"))
    assert obs.height > 0 and tail.height > 0
    # Observed: both legs coincide -> band collapses onto the line.
    assert (obs["band_lo"] == obs["ratio_borrow"]).all()
    assert (obs["band_hi"] == obs["ratio_borrow"]).all()
    assert (obs["ratio_curve"] == obs["ratio_borrow"]).all()
    # Tail: the band brackets the mean (lo <= mean <= hi).
    assert (tail["band_lo"] <= tail["ratio_mean"] + 1e-9).all()
    assert (tail["ratio_mean"] <= tail["band_hi"] + 1e-9).all()
    # The observed flag is a single contiguous prefix (no holes).
    flags = p.sort("duration")["observed"].to_list()
    assert flags == sorted(flags, reverse=True)


def test_segment_path_insufficient_falls_back_to_borrow():
    """When no grain converges the curve / band columns are null, mean=borrow.

    Mirrors segment_band's ``insufficient`` posture so a non-converged tail is
    never dressed up as a confident curve in the path either.
    """
    tri = _synthetic_young_regime()
    fit = lr.Ratio(
        method="sa", loss_regime=lr.Regime.at(change="2023-10-01")
    ).fit(tri)
    band = fit.segment_band(auto_grain=True)
    band = band if isinstance(band, pl.DataFrame) else pl.from_pandas(band)
    assert (band["band_status"] == "insufficient").all()  # precondition

    p = fit.segment_path(auto_grain=True)
    assert p.height > 0
    assert p["ratio_curve"].null_count() == p.height
    assert p["band_lo"].null_count() == p.height
    assert p["band_hi"].null_count() == p.height
    assert p["selected_grain"].null_count() == p.height
    assert (p["ratio_mean"] == p["ratio_borrow"]).all()


def test_segment_path_auto_selected_grain_matches_band():
    """The path's selected_grain equals the band's per group."""
    fit = _regime_fit()
    band = fit.segment_band(auto_grain=True)
    band = band if isinstance(band, pl.DataFrame) else pl.from_pandas(band)
    path = fit.segment_path(auto_grain=True)
    for cov in band["coverage"].unique().to_list():
        bg = band.filter(pl.col("coverage") == cov).row(0, named=True)[
            "selected_grain"
        ]
        pg = (
            path.filter(pl.col("coverage") == cov)["selected_grain"]
            .unique()
            .to_list()
        )
        assert pg == [bg]


def test_segment_path_curve_target_validation():
    """A non-intensity curve is rejected (the path formula is g_k-based)."""
    fit = _regime_fit()
    with pytest.raises(ValueError, match="target='intensity'"):
        fit.segment_path(curve=lr.Curve(target="ata"))


def test_segment_path_mirror_output_pandas():
    pd = pytest.importorskip("pandas")
    df = lr.load_experience()
    df_pd = df.to_pandas() if isinstance(df, pl.DataFrame) else df
    tri = lr.Triangle(df_pd, groups="coverage", cohort="uy_m", calendar="cy_m")
    fit = lr.Ratio(method="sa", loss_regime="auto").fit(tri)
    p = fit.segment_path(auto_grain=True)
    assert isinstance(p, pd.DataFrame)
    assert "selected_grain" in p.columns


def test_segment_path_dtypes_and_band_ordering():
    """``duration``/``observed`` dtypes and ``band_lo <= band_hi`` everywhere."""
    fit = _regime_fit()
    p = fit.segment_path(auto_grain=True)
    assert p.schema["duration"] == pl.Int64
    assert p.schema["observed"] == pl.Boolean
    both = p.filter(
        pl.col("band_lo").is_not_null() & pl.col("band_hi").is_not_null()
    )
    assert both.height > 0
    assert (both["band_lo"] <= both["band_hi"] + 1e-9).all()
    # Empty and populated frames share the column order + dtypes.
    empty = lr.Ratio(method="ed").fit(_experience_triangle()).segment_path(
        auto_grain=True
    )
    assert empty.columns == p.columns


def _two_col_regime_triangle() -> "lr.Triangle":
    """A 2-column-groups triangle (coverage x uy-year parity block)."""
    df = lr.load_experience().with_columns(
        pl.when(pl.col("uy_m").dt.year() % 2 == 0)
        .then(pl.lit("E"))
        .otherwise(pl.lit("O"))
        .alias("block")
    )
    return lr.Triangle(
        df, groups=["coverage", "block"], cohort="uy_m", calendar="cy_m"
    )


def test_segment_path_multi_column_groups():
    """The path works under multi-column groups (the pre-release gate).

    Group keys come through on every row, the per-combo selected_grain agrees
    with the band, and the result is deterministic.
    """
    fit = lr.Ratio(method="sa", loss_regime="auto").fit(
        _two_col_regime_triangle()
    )
    band = fit.segment_band(auto_grain=True)
    band = band if isinstance(band, pl.DataFrame) else pl.from_pandas(band)
    path = fit.segment_path(auto_grain=True)
    # Both group columns are present and fully populated.
    assert {"coverage", "block"}.issubset(set(path.columns))
    assert path["coverage"].null_count() == 0
    assert path["block"].null_count() == 0
    # Same set of regime combos as the band.
    keys = ["coverage", "block"]
    pk = path.select(keys).unique().sort(keys)
    bk = band.select(keys).unique().sort(keys)
    assert pk.equals(bk)
    # Per-combo selected_grain matches the band and is constant within a combo.
    for row in bk.iter_rows(named=True):
        flt = (pl.col("coverage") == row["coverage"]) & (
            pl.col("block") == row["block"]
        )
        bg = band.filter(flt).row(0, named=True)["selected_grain"]
        pg = path.filter(flt)["selected_grain"].unique().to_list()
        assert pg == [bg]
    # Deterministic.
    assert_frame_equal(path, fit.segment_path(auto_grain=True))
