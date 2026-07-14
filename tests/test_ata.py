"""Tests for Triangle.ata() and the ATA result class."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


def _tri(toy_input):
    return lr.Triangle(toy_input)


def _tri_grouped(toy_input):
    df = toy_input.with_columns(pl.lit("SURGERY").alias("coverage"))
    return lr.Triangle(df, groups="coverage")


# ---------------------------------------------------------------------------
# Class shape
# ---------------------------------------------------------------------------


def test_ata_returns_ata_result(toy_input):
    tri = _tri(toy_input)
    ata = tri.link().ata()
    assert isinstance(ata, lr.ATA)


def test_ata_repr_no_group(toy_input):
    ata = _tri(toy_input).link().ata()
    text = repr(ata)
    assert "ATA" in text
    assert "links" in text


def test_ata_repr_grouped(toy_input):
    ata = _tri_grouped(toy_input).link().ata()
    text = repr(ata)
    assert "ATA" in text
    assert "groups" in text


# ---------------------------------------------------------------------------
# Diagnostic schema
# ---------------------------------------------------------------------------


def test_ata_df_columns_no_group(toy_input):
    ata = _tri(toy_input).link().ata()
    assert set(ata.df.columns) >= {"duration", "ata", "sigma2", "cv", "rse", "n_cohorts"}


def test_ata_df_columns_with_group(toy_input):
    ata = _tri_grouped(toy_input).link().ata()
    assert set(ata.df.columns) >= {
        "coverage", "duration", "ata", "sigma2", "cv", "rse", "n_cohorts",
    }


def test_ata_df_n_links_equals_n_durations_minus_one(toy_input):
    ata = _tri(toy_input).link().ata()
    df = ata.df
    # toy input has 5 duration periods → 4 links
    assert df.shape[0] == 4
    assert sorted(df["duration"].to_list()) == [1, 2, 3, 4]


# ---------------------------------------------------------------------------
# Numerical sanity
# ---------------------------------------------------------------------------


def test_ata_f_is_finite_for_nontrivial_links(toy_input):
    ata = _tri(toy_input).link().ata()
    df = ata.df
    # links 1..3 have at least 2 cohorts contributing → f should be finite
    for k, f in zip(df["duration"].to_list(), df["ata"].to_list(), strict=False):
        if k <= 3:
            assert f is not None


def test_ata_n_obs_decreasing_with_duration(toy_input):
    """As duration grows, fewer cohorts contribute (triangular structure)."""
    ata = _tri(toy_input).link().ata()
    df = ata.df.sort("duration")
    counts = df["n_cohorts"].to_list()
    # toy: duration=1 has 4 links, duration=2 has 3, duration=3 has 2, duration=4 has 1
    assert counts == [4, 3, 2, 1]


def test_ata_sigma2_nonneg_when_present(toy_input):
    ata = _tri(toy_input).link().ata()
    for v in ata.df["sigma2"].to_list():
        assert v is None or v >= 0.0


def test_ata_cv_nonneg_when_present(toy_input):
    ata = _tri(toy_input).link().ata()
    for v in ata.df["cv"].to_list():
        assert v is None or v >= 0.0


# ---------------------------------------------------------------------------
# Output type mirroring
# ---------------------------------------------------------------------------


def test_ata_pandas_input_mirror(toy_input):
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(toy_input.to_pandas())
    ata = lr.Triangle(df).link().ata()
    assert isinstance(ata.df, pd.DataFrame)
    assert isinstance(ata.summary(), pd.DataFrame)


# ---------------------------------------------------------------------------
# Multi-group independence
# ---------------------------------------------------------------------------


def test_ata_per_group_independent(toy_input):
    base = toy_input
    df_grouped = pl.concat(
        [
            base.with_columns(pl.lit("A").alias("coverage")),
            base.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    tri = lr.Triangle(df_grouped, groups="coverage")
    ata = tri.link().ata()
    df = ata.df
    a_f = df.filter(pl.col("coverage") == "A").sort("duration")["ata"].to_list()
    b_f = df.filter(pl.col("coverage") == "B").sort("duration")["ata"].to_list()
    assert a_f == b_f


def test_rse_requires_two_positive_denominators():
    # n_k >= 2 cohorts reach the link, but only ONE has a positive cumulative
    # loss (a positive denominator). RSE must stay NaN (insufficient sample),
    # not a misleading 0 -- matching the CV guard right above it.
    import numpy as np

    from lossratio.core.ata import _cross_cohort_link_stats

    loss_obs = np.array(
        [
            [0.0, 5.0],     # denom 0 -> excluded from the fit
            [0.0, 6.0],     # denom 0 -> excluded
            [10.0, 15.0],   # the only positive denominator
        ]
    )
    f_k = np.array([26.0 / 10.0])     # pooled sum(to)/sum(from)
    sigma2_k = np.array([0.0])        # the branch that used to yield rse = 0.0
    stats = _cross_cohort_link_stats(loss_obs, f_k, sigma2_k)
    assert np.isnan(stats.rse_k[0])
    assert np.isnan(stats.cv_k[0])


# ---------------------------------------------------------------------------
# stability() -- factor-stability point (public contract)
# ---------------------------------------------------------------------------


def test_stability_columns_and_one_row_no_group(toy_input):
    stability = _tri(toy_input).link().ata().stability()
    assert set(stability.columns) == {"duration_from", "duration_to", "cv", "rse"}
    assert stability.height == 1


def test_stability_one_row_per_group(toy_input):
    df = pl.concat(
        [
            toy_input.with_columns(pl.lit("A").alias("coverage")),
            toy_input.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    stability = lr.Triangle(df, groups="coverage").link().ata().stability()
    assert "coverage" in stability.columns
    assert sorted(stability["coverage"].to_list()) == ["A", "B"]


def test_stability_reaches_first_link_under_generous_thresholds(toy_input):
    # Thresholds nothing can fail + min_run=1 -> the first finite link is stable;
    # a real triangle's first link runs duration_from 1 -> duration_to 2.
    stability = _tri(toy_input).link().ata().stability(
        max_cv=1e9, max_rse=1e9, min_run=1
    )
    assert stability["duration_to"].to_list() == [2]


def test_stability_unreachable_thresholds_give_a_null_point(toy_input):
    # Thresholds no link can clear -> the group is still reported, with a null
    # point, so "no stable run" is distinguishable from a missing group.
    stability = _tri(toy_input).link().ata().stability(max_cv=1e-9, max_rse=1e-9)
    assert stability.height == 1
    assert stability["duration_to"].to_list() == [None]
    assert stability["cv"].to_list() == [None]


def test_stability_keeps_unstable_group_as_a_null_row(toy_input):
    # Every group appears even at the public API; a group that never stabilises
    # is a null row, not dropped.
    df = pl.concat(
        [
            toy_input.with_columns(pl.lit("A").alias("coverage")),
            toy_input.with_columns(pl.lit("B").alias("coverage")),
        ]
    )
    stability = (
        lr.Triangle(df, groups="coverage").link().ata()
        .stability(max_cv=1e-9, max_rse=1e-9)
        .sort("coverage")
    )
    assert stability["coverage"].to_list() == ["A", "B"]
    assert stability["duration_to"].to_list() == [None, None]


def test_stability_empty_grouped_triangle_returns_typed_empty_frame():
    # An empty grouped triangle yields a column-less chart frame; stability()
    # must return a typed 0-row frame, not raise an opaque polars error.
    empty = pl.DataFrame(
        schema={
            "coverage": pl.Utf8, "uy_m": pl.Int64, "cy_m": pl.Int64,
            "incr_loss": pl.Float64, "incr_premium": pl.Float64,
        }
    )
    stability = lr.Triangle(empty, groups="coverage").link().ata().stability()
    assert stability.height == 0
    assert set(stability.columns) == {
        "coverage", "duration_from", "duration_to", "cv", "rse",
    }


def test_stability_invalid_arguments_raise(toy_input):
    ata = _tri(toy_input).link().ata()
    with pytest.raises(ValueError):
        ata.stability(min_run=0)
    with pytest.raises(ValueError):
        ata.stability(max_cv=0)
    with pytest.raises(ValueError):
        ata.stability(max_rse=-1.0)


def test_stability_pandas_input_mirror(toy_input):
    pd = pytest.importorskip("pandas")
    df = pd.DataFrame(toy_input.to_pandas())
    stability = lr.Triangle(df).link().ata().stability()
    assert isinstance(stability, pd.DataFrame)
    assert list(stability.columns) == ["duration_from", "duration_to", "cv", "rse"]
    assert len(stability) == 1


# ---------------------------------------------------------------------------
# stability() detection logic -- deterministic, on hand-built cv/rse frames
# ---------------------------------------------------------------------------


def _make_chart(cv, rse, group=None):
    """A minimal ATA chart frame with controlled per-link cv / rse."""
    n = len(cv)
    cols = {
        "duration_from": list(range(1, n + 1)),
        "duration_to":   list(range(2, n + 2)),
        "cv":  cv,
        "rse": rse,
    }
    if group is not None:
        cols = {"coverage": [group] * n, **cols}
    return pl.DataFrame(cols)


def test_stability_frame_first_sustained_run():
    from lossratio.core.ata import _stability_frame

    # links 3 & 4 are the first pair both below cv/rse -> run starts at link 3.
    chart = _make_chart(cv=[0.30, 0.20, 0.05, 0.04], rse=[0.10, 0.08, 0.02, 0.01])
    stability = _stability_frame(chart, None, max_cv=0.10, max_rse=0.05, min_run=2)
    assert stability["duration_from"].to_list() == [3]
    assert stability["duration_to"].to_list() == [4]
    # the reported cv / rse belong to the run's START link (link 3), not a neighbour.
    assert stability["cv"].to_list()[0] == pytest.approx(0.05)
    assert stability["rse"].to_list()[0] == pytest.approx(0.02)


def test_stability_frame_min_run_rejects_a_transient_dip():
    from lossratio.core.ata import _stability_frame

    # link 2 dips sub-threshold but link 3 spikes back: not a run of 2. The
    # first sustained pair is links 4 & 5.
    chart = _make_chart(
        cv=[0.30, 0.05, 0.30, 0.04, 0.03],
        rse=[0.10, 0.02, 0.10, 0.01, 0.01],
    )
    run2 = _stability_frame(chart, None, max_cv=0.10, max_rse=0.05, min_run=2)
    assert run2["duration_from"].to_list() == [4]
    # min_run=1 accepts the lone dip at link 2 instead.
    run1 = _stability_frame(chart, None, max_cv=0.10, max_rse=0.05, min_run=1)
    assert run1["duration_from"].to_list() == [2]


def test_stability_frame_nan_link_breaks_a_run():
    from lossratio.core.ata import _stability_frame

    # A non-finite (NaN) cv/rse link is unstable and breaks the run; the first
    # sustained pair of finite sub-threshold links is links 4 & 5.
    chart = _make_chart(
        cv=[0.30, 0.04, float("nan"), 0.03, 0.02],
        rse=[0.10, 0.01, float("nan"), 0.01, 0.01],
    )
    stability = _stability_frame(chart, None, max_cv=0.10, max_rse=0.05, min_run=2)
    assert stability["duration_from"].to_list() == [4]


def test_stability_frame_min_run_exceeds_link_count_is_null():
    from lossratio.core.ata import _stability_frame

    # One link cannot open a run of two -> reported, but with a null point.
    chart = _make_chart(cv=[0.04], rse=[0.01])
    stability = _stability_frame(chart, None, max_cv=0.10, max_rse=0.05, min_run=2)
    assert stability.height == 1
    assert stability["duration_to"].to_list() == [None]


def test_stability_frame_requires_both_cv_and_rse():
    from lossratio.core.ata import _stability_frame

    # cv clears the threshold everywhere but rse never does -> no stable link.
    chart = _make_chart(cv=[0.05, 0.05, 0.05], rse=[0.10, 0.10, 0.10])
    stability = _stability_frame(chart, None, max_cv=0.10, max_rse=0.05, min_run=1)
    assert stability["duration_to"].to_list() == [None]


def test_stability_frame_per_group_independent_points():
    from lossratio.core.ata import _stability_frame

    # A stabilises at link 1, B never stabilises -> A carries a point, B null.
    chart = pl.concat(
        [
            _make_chart(cv=[0.04, 0.03], rse=[0.01, 0.01], group="A"),
            _make_chart(cv=[0.40, 0.40], rse=[0.20, 0.20], group="B"),
        ]
    )
    stability = _stability_frame(
        chart, "coverage", max_cv=0.10, max_rse=0.05, min_run=1
    ).sort("coverage")
    assert stability["duration_to"].to_list() == [2, None]


def test_stability_overlay_draws_only_non_null_point_groups():
    from lossratio.core.ata import _stability_frame

    # The plot overlay is _stability_frame(...) filtered to non-null points;
    # lock that only the group with a real point is drawn (dedup regression).
    chart = pl.concat(
        [
            _make_chart(cv=[0.04, 0.03], rse=[0.01, 0.01], group="A"),  # stabilises
            _make_chart(cv=[0.40, 0.40], rse=[0.20, 0.20], group="B"),  # never does
        ]
    )
    full = _stability_frame(chart, "coverage", max_cv=0.10, max_rse=0.05, min_run=1)
    overlay = full.filter(pl.col("duration_to").is_not_null())
    assert full.height == 2                        # value form keeps every group
    assert overlay["coverage"].to_list() == ["A"]  # only the reached group is drawn
