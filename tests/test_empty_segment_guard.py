"""Empty-segment guards in the covariate GLM path.

A coverage segment can be left with zero fittable cells -- every cell dropped by
the exposure>0 / finite filter (e.g. a thin coverage whose whole train window is
held out in a backtest). The penalized-IRLS convergence check reduces ``max|d
eta|`` over an empty array, which numpy cannot evaluate (no identity). The guards
return a degenerate, non-converged fit (NaN intensities -> projection gap /
degraded status) instead of raising ``ValueError: zero-size array to reduction``.
"""

import numpy as np

import lossratio as lr
from lossratio._kernels.covariate import CovariateFit, fit_covariate_intensity
from lossratio._kernels.smooth import penalized_irls


def test_penalized_irls_empty_response_is_degenerate_not_crash():
    # n == 0: no data to fit. Must not raise; returns NaN coefficients,
    # non-converged, with empty eta / mu of the right shape.
    p = 3
    r = penalized_irls(
        np.zeros(0), np.zeros(0), np.zeros((0, p)), np.zeros((p, p)), 1.0
    )
    assert r.converged is False
    assert r.beta.shape == (p,) and np.isnan(r.beta).all()
    assert r.eta.size == 0 and r.mu.size == 0
    assert r.n_iter == 0


def test_fit_covariate_intensity_all_cells_filtered_is_degenerate():
    # every cell has non-positive exposure -> keep filter empties the response.
    cf = fit_covariate_intensity(
        response=np.array([10.0, 5.0]),
        exposure=np.array([0.0, -1.0]),          # both dropped (exposure must be > 0)
        duration=np.array([1, 2]),
        covariates={"regime": np.array(["R0", "R1"])},
    )
    assert isinstance(cf, CovariateFit)
    assert cf.durations == []
    assert cf.converged is False
    # intensity is NaN for any duration (no fitted shape)
    assert np.isnan(cf.intensity(1, {"regime": "R0"}))
    assert np.isnan(cf.intensity(5))


def test_fit_covariate_intensity_empty_smooth_branch_no_bspline_crash():
    # the smooth branch (n_basis set) must short-circuit before building a
    # B-spline basis on an empty duration vector.
    cf = fit_covariate_intensity(
        response=np.array([10.0]),
        exposure=np.array([0.0]),                # dropped -> empty
        duration=np.array([1]),
        covariates={},
        n_basis=6,
    )
    assert cf.durations == [] and cf.converged is False


def test_credible_covariate_fit_with_a_fully_held_out_coverage():
    # public-API smoke: one coverage's cohorts sit entirely in the most-recent
    # diagonals; masking that holdout empties its train segment. The covariate
    # fit must complete (degraded), not crash.
    import polars as pl

    df = lr.load_experience().with_columns(
        pl.lit("R0").alias("regime")             # single regime level (valid covariate)
    )
    # a thin synthetic coverage whose cohorts are only the latest few months
    thin = (
        df.filter(pl.col("uy_m") >= pl.lit("2025-10-01").str.to_date())
        .with_columns(pl.lit("THIN").alias("coverage"))
    )
    book = pl.concat([df, thin])
    tri = lr.Triangle(book, groups=["coverage", "regime"]).mask(holdout=6)

    fit = lr.CredibleLoss(covariates=["regime"]).fit(tri)   # must not raise
    out = fit.to_polars()
    assert out.height > 0
    # the well-populated coverages still project real numbers
    assert out.filter(pl.col("ratio_proj").is_not_null()).height > 0
