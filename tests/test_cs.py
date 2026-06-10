"""Regression tests for the cohort-scaled ("cs") loss / premium method.

Each test targets a specific bug from the cs fix plan. The header comment on
every test names the bug it guards (H1..H6 / M4 / warnings) and how it fails on
the pre-fix code, so the regression intent stays legible. The cs surface is
exercised through three entry points: the worker functions (``_fit_cs`` /
``_fit_premium_cs``) for the matrix-level gap / guard bugs, the public
dispatchers (``Loss`` / ``Premium`` / ``Ratio`` with ``method="cs"``) for the
end-to-end CI / band contracts, and the standalone ``CohortScaled`` for the
shared-knob validation.
"""

from __future__ import annotations

import warnings

import numpy as np
import polars as pl
import pytest
from polars.testing import assert_frame_equal

import lossratio as lr
from lossratio.cohort_scaled import CohortScaled
from lossratio.loss import Loss, _cs_build_cells, _fit_cs
from lossratio.premium import Premium, _fit_premium_cs


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


def _sur_triangle() -> "lr.Triangle":
    """A real-sized single-coverage triangle (SURGERY) for the public path."""
    return lr.Triangle(lr.load_experience().filter(pl.col("coverage") == "SURGERY"))


def _gap_matrices() -> tuple[np.ndarray, np.ndarray]:
    """Loss / premium matrices with an INTERIOR duration gap.

    Cohort 0 observes cumulative loss / premium at durations 1, 2, 4 (NaN at
    duration 3 = column index 2); cohort 1 is fully observed at durations 1-3.
    Feeding this to ``_cs_build_cells`` yields a cohort whose duration keys are
    ``{1, 2, 4}`` -- a non-contiguous gap, which is exactly what crashed the
    pre-fix bootstrap pseudo-increment walk with ``KeyError: 3``.
    """
    loss = np.array(
        [
            [100.0, 220.0, np.nan, 300.0, np.nan],
            [120.0, 250.0, 390.0, np.nan, np.nan],
        ]
    )
    prem = np.array(
        [
            [100.0, 200.0, np.nan, 300.0, np.nan],
            [100.0, 200.0, 300.0, np.nan, np.nan],
        ]
    )
    return loss, prem


# ---------------------------------------------------------------------------
# H1 -- gap-walk hardening (KeyError on an interior-NaN duration gap)
# ---------------------------------------------------------------------------


def test_cs_build_cells_keeps_noncontiguous_gap() -> None:
    """The build keeps the {1, 2, 4} gap (so the walk must handle it)."""
    loss, prem = _gap_matrices()
    cells, _ = _cs_build_cells(loss, prem)
    assert sorted(cells[0]) == [1, 2, 4]  # interior gap at duration 3


def test_cs_loss_bootstrap_survives_interior_gap() -> None:
    """H1 (loss): bootstrap over a non-contiguous cohort must not KeyError.

    Pre-fix the pseudo-increment loop indexed ``cdict[d - 1]`` unconditionally,
    raising ``KeyError`` at the cohort's gap. The hardened walk reseeds from the
    next observed cell instead.
    """
    loss, prem = _gap_matrices()
    rng = np.random.default_rng(1)
    res = _fit_cs(
        loss, prem, prem.copy(),
        credibility=3.0, smooth=None, n_bootstrap=25, conf_level=0.95, rng=rng,
    )
    # Same set of projected cells as the point path -> CI arrays exist.
    assert res.boot_ci_lo is not None and res.boot_ci_hi is not None


def test_cs_premium_bootstrap_survives_interior_gap() -> None:
    """H1 (premium): the premium worker bootstrap also survives the gap."""
    _, prem = _gap_matrices()
    rng = np.random.default_rng(1)
    res = _fit_premium_cs(
        prem, credibility=3.0, smooth=None, n_bootstrap=25,
        conf_level=0.95, rng=rng,
    )
    assert res.boot_ci_lo is not None and res.boot_ci_hi is not None


# ---------------------------------------------------------------------------
# H4 -- CI re-centred on the point (lo <= proj <= hi)
# ---------------------------------------------------------------------------


def test_cs_loss_ci_brackets_the_point() -> None:
    """H4 (loss): every projected cell with a CI has lo <= proj <= hi.

    Pre-fix the replicate cloud was not centred on the noise-free point, so raw
    quantiles could fall entirely above or below ``loss_proj``.
    """
    df = (
        Loss(method="cs", n_bootstrap=80, seed=1)
        .fit(_sur_triangle())
        .to_polars()
    )
    ci = df.filter(pl.col("loss_ci_lo").is_not_null())
    assert ci.height > 0
    bad = ci.filter(
        ~(
            (pl.col("loss_ci_lo") <= pl.col("loss_proj"))
            & (pl.col("loss_proj") <= pl.col("loss_ci_hi"))
        )
    )
    assert bad.height == 0


def test_cs_premium_ci_brackets_the_point() -> None:
    """H4 (premium): the premium worker CI brackets its point too."""
    df = (
        Premium(method="cs", n_bootstrap=80, seed=1)
        .fit(_sur_triangle())
        .to_polars()
    )
    ci = df.filter(pl.col("premium_ci_lo").is_not_null())
    assert ci.height > 0
    bad = ci.filter(
        ~(
            (pl.col("premium_ci_lo") <= pl.col("premium_proj"))
            & (pl.col("premium_proj") <= pl.col("premium_ci_hi"))
        )
    )
    assert bad.height == 0


# ---------------------------------------------------------------------------
# H3 -- premium positivity / internal-fallback never negative
# ---------------------------------------------------------------------------


def test_cs_premium_projection_strictly_positive() -> None:
    """H3: no projected premium_proj is <= 0 (the compounding break guard)."""
    df = Premium(method="cs", seed=1).fit(_sur_triangle()).to_polars()
    proj = df.filter(pl.col("premium_proj").is_not_null())
    assert proj.filter(pl.col("premium_proj") <= 0.0).height == 0


def test_cs_loss_internal_premium_fallback_positive() -> None:
    """H3 (loss worker): the gap-filled premium base stays > 0.

    The loss worker develops a premium fallback when the fit projection has a
    gap; the guard breaks the walk on a non-finite / non-positive base, so the
    stored ``premium_proj`` driving the ratio is always positive.
    """
    df = Loss(method="cs", seed=1).fit(_sur_triangle()).to_polars()
    proj = df.filter(pl.col("premium_proj").is_not_null())
    assert proj.filter(pl.col("premium_proj") <= 0.0).height == 0


# ---------------------------------------------------------------------------
# H2 -- zero RuntimeWarnings from the bootstrap reductions
# ---------------------------------------------------------------------------


def test_cs_loss_bootstrap_emits_no_runtime_warning() -> None:
    """H2 (loss): the nanstd / nanquantile reductions emit no RuntimeWarning."""
    tri = _sur_triangle()
    with warnings.catch_warnings():
        warnings.simplefilter("error", RuntimeWarning)
        Loss(method="cs", n_bootstrap=40, seed=1).fit(tri).to_polars()


def test_cs_premium_bootstrap_emits_no_runtime_warning() -> None:
    """H2 (premium): same for the premium worker bootstrap."""
    tri = _sur_triangle()
    with warnings.catch_warnings():
        warnings.simplefilter("error", RuntimeWarning)
        Premium(method="cs", n_bootstrap=40, seed=1).fit(tri).to_polars()


# ---------------------------------------------------------------------------
# M4 -- knob validation (credibility=NaN, even/odd smooth)
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "estimator",
    [
        lambda **kw: Loss(method="cs", **kw),
        lambda **kw: Premium(method="cs", **kw),
        lambda **kw: lr.Ratio(method="cs", **kw),
        lambda **kw: lr.Ratio(premium_method="cs", **kw),
        lambda **kw: CohortScaled(**kw),
    ],
)
def test_cs_credibility_nan_raises(estimator) -> None:
    """M4: credibility=NaN must raise (it silently all-NaN'd the forecast)."""
    with pytest.raises(ValueError):
        estimator(credibility=float("nan"))


@pytest.mark.parametrize(
    "estimator",
    [
        lambda **kw: Loss(method="cs", **kw),
        lambda **kw: Premium(method="cs", **kw),
        lambda **kw: lr.Ratio(method="cs", **kw),
        lambda **kw: CohortScaled(**kw),
    ],
)
def test_cs_even_smooth_raises_odd_ok(estimator) -> None:
    """An even smooth window is rejected; an odd one is accepted."""
    with pytest.raises(ValueError):
        estimator(smooth=2)
    estimator(smooth=3)  # odd -> no raise


# ---------------------------------------------------------------------------
# H6 -- Ratio cs + uncertainty overlay guard
# ---------------------------------------------------------------------------


def test_ratio_cs_with_uncertainty_overlay_raises() -> None:
    """H6: a cs LOSS fit + uncertainty= overlay is NotImplementedError."""
    with pytest.raises(NotImplementedError):
        lr.Ratio(method="cs", uncertainty=lr.ResidualBootstrap())


def test_ratio_cs_with_analytical_uncertainty_ok() -> None:
    """Analytical uncertainty (the closed-form default) is still allowed."""
    lr.Ratio(method="cs", uncertainty=lr.Analytical())  # no raise


def test_ratio_premium_cs_with_uncertainty_overlay_ok() -> None:
    """premium_method='cs' + uncertainty= is fine (overlay is loss-side only)."""
    lr.Ratio(method="ed", premium_method="cs", uncertainty=lr.ResidualBootstrap())


# ---------------------------------------------------------------------------
# H5 -- Ratio cs fixed-mode asymmetric ratio band
# ---------------------------------------------------------------------------


def test_ratio_cs_fixed_band_is_loss_ci_over_premium() -> None:
    """H5: in fixed mode the ratio band is the asymmetric loss_ci / premium.

    Pre-fix the ratio band was the symmetric Gaussian ``ratio_proj +/- z *
    ratio_se``, which discards the worker's asymmetric bootstrap-quantile loss
    CI. The cs + fixed gate propagates ``loss_ci_* / premium_proj`` instead.
    """
    df = lr.Ratio(method="cs", n_bootstrap=60, seed=1).fit(_sur_triangle()).to_polars()
    sub = df.filter(
        pl.col("ratio_ci_lo").is_not_null() & pl.col("loss_ci_lo").is_not_null()
    )
    assert sub.height > 0
    exp_lo = np.maximum(
        0.0, sub["loss_ci_lo"].to_numpy() / sub["premium_proj"].to_numpy()
    )
    exp_hi = sub["loss_ci_hi"].to_numpy() / sub["premium_proj"].to_numpy()
    assert np.allclose(exp_lo, sub["ratio_ci_lo"].to_numpy())
    assert np.allclose(exp_hi, sub["ratio_ci_hi"].to_numpy())
    # The band is genuinely asymmetric (so this is NOT the Gaussian fallback).
    gap_lo = sub["ratio_proj"].to_numpy() - sub["ratio_ci_lo"].to_numpy()
    gap_hi = sub["ratio_ci_hi"].to_numpy() - sub["ratio_proj"].to_numpy()
    assert np.any(~np.isclose(gap_lo, gap_hi))


# ---------------------------------------------------------------------------
# Basic correctness: additive loss / multiplicative premium + reproducibility
# ---------------------------------------------------------------------------


def test_cs_loss_point_projection_is_additive() -> None:
    """The cs loss increment is additive: loss[k] = loss[k-1] + scale*g_k*prem.

    Built so the pooled g_k is exactly known: every cohort grows loss by a
    constant fraction of the prior cumulative premium, so g_pool[k] is that
    fraction and each cohort_scale is 1. The projected increment then equals
    ``g_k * premium[k-1]`` exactly.
    """
    # 3 cohorts, premium constant 1000 each duration (so prem_prev = 1000).
    # incr_loss = 100 at every observed link -> g_k = 100 / 1000 = 0.1.
    prem = np.array(
        [
            [1000.0, 1000.0, 1000.0, 1000.0],
            [1000.0, 1000.0, 1000.0, np.nan],
            [1000.0, 1000.0, np.nan, np.nan],
        ]
    )
    loss = np.array(
        [
            [500.0, 600.0, 700.0, 800.0],
            [500.0, 600.0, 700.0, np.nan],
            [500.0, 600.0, np.nan, np.nan],
        ]
    )
    rng = np.random.default_rng(0)
    res = _fit_cs(
        loss, prem, prem.copy(),
        credibility=0.0, smooth=None, n_bootstrap=None, conf_level=0.95, rng=rng,
    )
    # Cohort 2 (last obs duration 2 = idx 1) projects duration 3 (idx 2):
    # loss[2] = loss[1] + scale * g_3 * prem[1] = 600 + 1*0.1*1000 = 700.
    assert res.loss_proj[2, 2] == pytest.approx(700.0)


def test_cs_premium_point_projection_is_multiplicative() -> None:
    """The cs premium increment is multiplicative: prem[k] = prem[k-1]*(1+gp_k)."""
    # Premium grows by a constant 10% each link -> gp_k = 0.1.
    prem = np.array(
        [
            [1000.0, 1100.0, 1210.0, 1331.0],
            [1000.0, 1100.0, 1210.0, np.nan],
            [1000.0, 1100.0, np.nan, np.nan],
        ]
    )
    rng = np.random.default_rng(0)
    res = _fit_premium_cs(
        prem, credibility=0.0, smooth=None, n_bootstrap=None,
        conf_level=0.95, rng=rng,
    )
    # Cohort 2 projects duration 3 (idx 2): 1100 * (1 + 1*0.1) = 1210.
    assert res.premium_proj[2, 2] == pytest.approx(1210.0)


def test_cs_bootstrap_reproducible_across_runs() -> None:
    """Two fits with the same seed are byte-identical (deterministic RNG)."""
    tri = _sur_triangle()
    a = Loss(method="cs", n_bootstrap=30, seed=1).fit(tri).to_polars()
    b = Loss(method="cs", n_bootstrap=30, seed=1).fit(tri).to_polars()
    assert_frame_equal(a, b)


def test_cs_standalone_handles_gap_without_error() -> None:
    """The standalone CohortScaled bootstrap survives a non-contiguous cohort.

    Uses a triangle whose cohorts have different lengths (the natural ragged
    shape) and bootstraps -- exercising the standalone gap-walk + finite-sample
    survivor gate. No KeyError, and any emitted ratio CI brackets the point.
    """
    tri = _sur_triangle()
    fit = CohortScaled(n_bootstrap=40, seed=1).fit(tri)
    df = fit.df
    ci = df.filter(pl.col("ratio_lo").is_not_null())
    assert ci.height > 0
    bad = ci.filter(
        ~(
            (pl.col("ratio_lo") <= pl.col("ratio_proj"))
            & (pl.col("ratio_proj") <= pl.col("ratio_hi"))
        )
    )
    assert bad.height == 0
