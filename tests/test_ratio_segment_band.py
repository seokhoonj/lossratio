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
    manual change) has only a couple of developed devs, so its own
    intensity series is short -> under-determined curve fit.
    """
    rng = np.random.default_rng(7)
    rows: list[dict] = []
    # 12 monthly cohorts; change at month 10 -> recent segment = 3 cohorts.
    cohorts = [dt.date(2023, m, 1) for m in range(1, 13)]
    max_dev = 12
    for cov in ("A", "B"):
        base_lr = 0.5 if cov == "A" else 0.6
        for ci, coh in enumerate(cohorts):
            # younger cohorts observe fewer devs (right-triangle).
            n_obs = max_dev - ci
            prem_step = 1_000.0
            cum_loss = 0.0
            cum_prem = 0.0
            for d in range(1, n_obs + 1):
                cal_m = coh.month + (d - 1)
                cal_y = coh.year + (cal_m - 1) // 12
                cal_m = (cal_m - 1) % 12 + 1
                cum_prem += prem_step
                # additive intensity decaying with dev
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
        "loss_ult",
        "premium_ult",
        "ratio_ult",
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
        ("loss_ult_borrow", "loss_ult"),
        ("ratio_ult_borrow", "ratio_ult"),
        ("premium_ult", "premium_ult_seg"),
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
    assert row["ratio_ult_borrow"] is not None
    assert row["ratio_ult_curve"] is not None
    assert row["curve_reason"] == "ok"
    assert row["curve_n_points"] >= 3
    # The two ultimate loss ratios are close (the two-leg spread is small).
    assert abs(row["ratio_ult_curve"] - row["ratio_ult_borrow"]) < 0.05
    # band_lo / band_hi bracket both legs.
    assert row["band_lo"] <= row["ratio_ult_borrow"] <= row["band_hi"]
    assert row["band_lo"] <= row["ratio_ult_curve"] <= row["band_hi"]
    assert row["band_width"] == pytest.approx(row["band_hi"] - row["band_lo"])
    # Data-sufficient agreement -> the honesty flag earns "narrow".
    assert not row["curve_under_determined"]
    assert row["band_status"] == "narrow"


def _synthetic_mature_regime():
    """A synthetic triangle whose recent segment is mature and well-determined.

    Two coverages, monthly cohorts. The change is placed early (month 13)
    so the recent segment carries many cohorts, the earliest observing
    ~24 devs -> a long, over-determined own intensity series. The recent
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
    max_dev = 36
    old_decay, new_decay = 1.3, 2.0
    for cov in ("A", "B"):
        base_lr = 0.5 if cov == "A" else 0.6
        for ci, coh in enumerate(cohorts):
            decay = new_decay if coh >= change else old_decay
            n_obs = max_dev - ci
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
        assert r["ratio_ult_curve"] is not None
        assert r["band_width"] is not None
        # The alt-law swing (if present) is within the two-leg spread.
        if r["curve_alt_ratio_ult"] is not None:
            assert (
                abs(r["curve_alt_ratio_ult"] - r["ratio_ult_curve"])
                <= r["band_width"]
            )


def _synthetic_divergent_regime():
    """A regime whose two tail legs DISAGREE on a data-sufficient curve.

    The recent segment is young enough to need a tail (its oldest cohort
    reaches ~12 of 36 devs) but carries enough cohorts that its own
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
    max_dev = 36
    old_decay, new_decay = 2.5, 0.7  # donor steep (small tail), recent shallow
    for cov in ("A", "B"):
        base_lr = 0.6 if cov == "A" else 0.7
        for ci, coh in enumerate(cohorts):
            decay = new_decay if coh >= change else old_decay
            n_obs = max_dev - ci
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
        assert r["ratio_ult_curve"] is not None
        # The spread alone clears the narrow threshold (10% of the level).
        assert r["band_width"] / abs(r["ratio_ult_borrow"]) > 0.10


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
        if r["ratio_ult_curve"] is not None:
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
    assert row["ratio_ult_curve"] is not None
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
    assert row["ratio_ult_curve"] is None
    assert row["loss_ult_curve"] is None
    assert row["band_lo"] is None
    assert row["band_hi"] is None
    assert row["band_width"] is None
    assert row["curve_reason"] in (
        "empty",
        "non_positive",
        "no_decaying_region",
    )
    # Borrow leg still present (NaN must not masquerade as a number).
    assert row["ratio_ult_borrow"] is not None


# ---------------------------------------------------------------------------
# 7. No regime -> empty typed frame
# ---------------------------------------------------------------------------


def test_no_regime_graceful():
    tri = _experience_triangle()
    fit = lr.Ratio(method="sa", loss_regime=None).fit(tri)
    band = fit.segment_band()
    assert band.height == 0
    # Correct schema even when empty.
    assert "ratio_ult_borrow" in band.columns
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
        b1.select(["coverage", "ratio_ult_borrow", "loss_ult_borrow"]),
        b3.select(["coverage", "ratio_ult_borrow", "loss_ult_borrow"]),
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
