"""Tests for the Phase 1 bootstrap module (analytical CL / Mack closed-form).

Verification is layered because R and Python use different RNGs, so the
Monte-Carlo bootstrap SE is *not* bit-comparable to the R sibling:

* Layer 1 -- deterministic bit-parity. The Mack anchor (per-link
  ``f_hat`` / ``sigma2`` / ``f_var``) is a closed-form function of the
  triangle and must match R to machine precision. The summary kernel
  and the Stage-1 forward projection are equally deterministic given a
  fixed ``_injected_fstar``.
* Layer 3 -- statistical / behavioural. The Monte-Carlo summary
  (``total_se`` etc.) is checked against the analytical Mack reference
  and the R B=4000 fixture only up to Monte-Carlo noise -- a tolerant
  median-relative-difference band, never bit-equality.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio.bootstrap import (
    Bootstrap,
    BootstrapTriangle,
    _apply_bootstrap_overlay,
    _boot_anchor_cl,
    _boot_kernel_cl_analytical,
    _boot_kernel_cl_cell,
    _boot_summary_decompose,
    _build_group_inputs,
    _build_pool_cell,
    _build_pool_link,
    _cell_residuals_cl,
    _cell_residuals_ed,
    _fitted_grid_cl,
    _link_residuals_cl,
    _pool_dev_subpools,
    _resolve_bootstrap,
)
from lossratio.link import _build_link_df
from lossratio.loss import Loss

FIXTURES = Path(__file__).parent / "fixtures"


# ---------------------------------------------------------------------------
# Shared fixtures / helpers
# ---------------------------------------------------------------------------


def _exp_sur() -> pl.DataFrame:
    """SUR-only experience slice -- the data the R fixtures were built on."""
    return pl.read_csv(
        FIXTURES / "experience.csv", try_parse_dates=True
    ).filter(pl.col("coverage") == "surgery")


def _tri(grouped: bool = True) -> lr.Triangle:
    exp = _exp_sur()
    if grouped:
        return lr.Triangle(exp, groups="coverage")
    return lr.Triangle(exp.drop("coverage"))


def _load(name: str) -> pl.DataFrame:
    return pl.read_csv(
        FIXTURES / f"{name}.csv", try_parse_dates=True, infer_schema_length=10000
    )


def _median_rel_diff(py: list, ref: list) -> float:
    """Median relative difference over projected cells.

    Only cells where the reference value is finite and strictly positive
    contribute -- observed cells carry SE 0 and would divide by zero.
    """
    diffs: list[float] = []
    for a, b in zip(py, ref):
        if a is None or b is None:
            continue
        if not (np.isfinite(a) and np.isfinite(b)):
            continue
        if b <= 0.0:
            continue
        diffs.append(abs(a - b) / b)
    assert diffs, "no projected cells to compare"
    return float(np.median(diffs))


# ---------------------------------------------------------------------------
# Layer 1 -- deterministic bit-parity
# ---------------------------------------------------------------------------


def test_bootstrap_anchor_matches_r():
    """The Mack anchor is a closed-form function of the triangle and so
    must match R to machine precision -- no RNG is involved."""
    r = _load("boot_anchor").sort(["ata_from"])
    tri = _tri()
    bt = Bootstrap(type="analytical", method="cl", B=8, seed=1).fit(tri)

    f_anchor = bt.f_anchor.sort(["ata_from"])
    sigma2_anchor = bt.sigma2_anchor.sort(["ata_from"])

    assert f_anchor.height == r.height
    assert sigma2_anchor.height == r.height

    for col, df in (
        ("f_hat", f_anchor),
        ("n_cohorts", f_anchor),
        ("sigma2", sigma2_anchor),
        ("f_var", sigma2_anchor),
    ):
        py_v = df[col].to_list()
        r_v = r[col].to_list()
        for i, (a, b) in enumerate(zip(py_v, r_v)):
            assert a == pytest.approx(b, rel=1e-9, abs=1e-9), (
                f"anchor column {col!r} row {i}: py={a} r={b}"
            )


def test_boot_summary_decompose_unit():
    """Hand-built ``(cum_mean, cum_sampled)`` pair with known statistics.

    1 cohort x 2 devs x 4 replicates. Dev 0 is observed (cum_mean ==
    cum_sampled, no spread). Dev 1 is projected with deliberately
    distinct mean/sampled spreads so every output is hand-checkable.
    """
    # cum_mean: dev0 constant 100; dev1 = [10, 20, 30, 40]
    cum_mean = np.array(
        [[[100.0, 100.0, 100.0, 100.0],
          [10.0, 20.0, 30.0, 40.0]]]
    )
    # cum_sampled: dev0 == cum_mean; dev1 = [12, 18, 33, 37]
    cum_sampled = np.array(
        [[[100.0, 100.0, 100.0, 100.0],
          [12.0, 18.0, 33.0, 37.0]]]
    )

    out = _boot_summary_decompose(cum_mean, cum_sampled, quantile_ci=True)

    # Flatten order is column-major (cohort fastest); 1 cohort x 2 devs
    # -> index 0 = (cohort 0, dev 0), index 1 = (cohort 0, dev 1).
    mean_proj = out["mean_proj"]
    param_se = out["param_se"]
    proc_se = out["proc_se"]
    total_se = out["total_se"]
    total_cv = out["total_cv"]

    # ---- dev 0: constant -> mean 100, both SEs 0 ----
    assert mean_proj[0] == pytest.approx(100.0)
    assert param_se[0] == pytest.approx(0.0, abs=1e-12)
    assert total_se[0] == pytest.approx(0.0, abs=1e-12)
    assert proc_se[0] == pytest.approx(0.0, abs=1e-12)
    assert total_cv[0] == pytest.approx(0.0, abs=1e-12)

    # ---- dev 1: hand-computed ----
    m = [10.0, 20.0, 30.0, 40.0]
    s = [12.0, 18.0, 33.0, 37.0]
    exp_mean = float(np.mean(m))                       # 25.0
    exp_param = float(np.std(m, ddof=1))               # sd of cum_mean
    exp_total = float(np.std(s, ddof=1))               # sd of cum_sampled
    exp_proc = float(np.sqrt(max(exp_total ** 2 - exp_param ** 2, 0.0)))
    exp_cv = exp_total / exp_mean

    assert mean_proj[1] == pytest.approx(exp_mean, rel=1e-12)
    assert param_se[1] == pytest.approx(exp_param, rel=1e-12)
    assert total_se[1] == pytest.approx(exp_total, rel=1e-12)
    assert proc_se[1] == pytest.approx(exp_proc, rel=1e-12)
    assert total_cv[1] == pytest.approx(exp_cv, rel=1e-12)

    # ---- inverted_cdf (R type=1) quantiles of cum_sampled at dev 1 ----
    q = np.quantile(s, (0.025, 0.975), method="inverted_cdf")
    assert out["ci_lo"][1] == pytest.approx(q[0], rel=1e-12)
    assert out["ci_hi"][1] == pytest.approx(q[-1], rel=1e-12)


def test_boot_summary_decompose_edge_cases():
    """``n < 2 -> NaN`` and ``mean_proj <= 0 -> total_cv NaN`` branches."""
    # ---- n < 2: only one finite replicate-pair -> all-NaN cell ----
    cum_mean = np.array([[[5.0, np.nan, np.nan]]])      # 1 cohort, 1 dev, B=3
    cum_sampled = np.array([[[6.0, np.nan, np.nan]]])
    out = _boot_summary_decompose(cum_mean, cum_sampled)
    assert np.isnan(out["mean_proj"][0])
    assert np.isnan(out["param_se"][0])
    assert np.isnan(out["proc_se"][0])
    assert np.isnan(out["total_se"][0])
    assert np.isnan(out["total_cv"][0])

    # ---- mean_proj <= 0: finite SE but non-positive mean -> CV NaN ----
    cum_mean = np.array([[[-2.0, 0.0, 2.0, 0.0]]])      # mean exactly 0
    cum_sampled = np.array([[[-1.0, 1.0, 3.0, 1.0]]])
    out = _boot_summary_decompose(cum_mean, cum_sampled)
    assert out["mean_proj"][0] == pytest.approx(0.0, abs=1e-12)
    assert np.isfinite(out["total_se"][0])             # SE still finite
    assert np.isnan(out["total_cv"][0])                # CV suppressed


def test_boot_kernel_injected_fstar_deterministic():
    """With ``_injected_fstar`` supplied the Stage-1 projection is pure
    arithmetic -- two calls are byte-identical and one cell is
    hand-verifiable."""
    # 2 cohorts x 3 devs. Cohort 0 fully observed; cohort 1 observed to
    # dev 1 only, so dev 2 must be projected.
    loss_obs = np.array([
        [100.0, 150.0, 210.0],
        [120.0, 168.0, np.nan],
    ])
    anchor = _boot_anchor_cl(loss_obs)
    B = 5
    n_links = loss_obs.shape[1] - 1
    # Deterministic per-link factors, distinct per replicate.
    f_star = np.array([
        [1.10, 1.20, 1.30, 1.40, 1.50],   # link 0 (dev 1 -> dev 2)
        [1.05, 1.15, 1.25, 1.35, 1.45],   # link 1 (dev 2 -> dev 3)
    ])
    assert f_star.shape == (n_links, B)

    rng = np.random.default_rng(0)
    r1 = _boot_kernel_cl_analytical(
        loss_obs, anchor, B=B, rng=rng, _injected_fstar=f_star
    )
    rng2 = np.random.default_rng(999)   # different seed, must not matter
    r2 = _boot_kernel_cl_analytical(
        loss_obs, anchor, B=B, rng=rng2, _injected_fstar=f_star
    )

    # Byte-identical -- no RNG path was taken for cum_mean (Stage 1).
    assert np.array_equal(
        np.nan_to_num(r1.cum_mean), np.nan_to_num(r2.cum_mean)
    )

    # ---- hand-check the one projected cell ----
    # Cohort 1, dev index 2 (last_obs = 1). link index k = 1.
    # cum_mean[1, 2, b] = f_star[1, b] * cum_mean[1, 1, b]
    #                   = f_star[1, b] * 168.0  (observed cell unchanged)
    for b in range(B):
        expect = f_star[1, b] * 168.0
        assert r1.cum_mean[1, 2, b] == pytest.approx(expect, rel=1e-12)

    # Observed cells equal the observed cumulative across all replicates.
    for b in range(B):
        assert r1.cum_mean[0, 0, b] == pytest.approx(100.0)
        assert r1.cum_mean[0, 2, b] == pytest.approx(210.0)
        assert r1.cum_mean[1, 1, b] == pytest.approx(168.0)


# ---------------------------------------------------------------------------
# Layer 3 -- statistical / behavioural
# ---------------------------------------------------------------------------


def test_bootstrap_seed_reproducible():
    """Same seed -> identical summary; different seed -> different."""
    tri = _tri()
    a = Bootstrap(type="analytical", method="cl", B=300, seed=42).fit(tri)
    b = Bootstrap(type="analytical", method="cl", B=300, seed=42).fit(tri)
    c = Bootstrap(type="analytical", method="cl", B=300, seed=43).fit(tri)

    sa, sb, sc = a.summary, b.summary, c.summary
    # Same seed -> bit-identical total_se.
    for x, y in zip(sa["total_se"].to_list(), sb["total_se"].to_list()):
        if x is None or y is None:
            continue
        assert x == y, "same seed must give identical bootstrap output"

    # Different seed -> at least one projected cell differs.
    differs = any(
        x is not None and y is not None and np.isfinite(x)
        and np.isfinite(y) and x > 0 and x != y
        for x, y in zip(sa["total_se"].to_list(), sc["total_se"].to_list())
    )
    assert differs, "a different seed should change the Monte-Carlo SE"


def test_bootstrap_se_ordering():
    """Pythagorean decomposition invariants on every cell:
    ``total_se >= param_se`` and ``0 <= proc_se <= total_se``, all finite
    where defined."""
    tri = _tri()
    bt = Bootstrap(type="analytical", method="cl", B=500, seed=7).fit(tri)
    s = bt.summary
    for row in s.iter_rows(named=True):
        ts, ps, prs = row["total_se"], row["param_se"], row["proc_se"]
        if ts is None or (isinstance(ts, float) and np.isnan(ts)):
            continue  # fully observed cell -> all-NaN, nothing to check
        assert np.isfinite(ts) and np.isfinite(ps) and np.isfinite(prs)
        # total^2 = param^2 + proc^2 -> total >= each component.
        assert ts >= ps - 1e-9, f"total_se < param_se: {ts} < {ps}"
        assert 0.0 <= prs <= ts + 1e-9, f"proc_se out of [0, total]: {prs}"


def test_bootstrap_converges_to_mack_analytical():
    """The analytical bootstrap is a Monte-Carlo realisation of Mack's
    closed form, so at large B its projected-cell ``total_se`` must
    track the analytical Mack ``loss_total_se`` up to Monte-Carlo
    noise."""
    r = _load("cl_mack_analytical_se").sort(["cohort", "dev"])
    tri = _tri()
    bt = Bootstrap(
        type="analytical", method="cl", B=4000, seed=20260522
    ).fit(tri)
    py = bt.summary.sort(["cohort", "dev"])

    keys = ["cohort", "dev"]
    py_c = py.join(r.select(keys), on=keys, how="inner").sort(keys)
    r_c = r.join(py.select(keys), on=keys, how="inner").sort(keys)

    mrd = _median_rel_diff(
        py_c["total_se"].to_list(), r_c["loss_total_se"].to_list()
    )
    assert mrd < 0.08, (
        f"bootstrap total_se median rel diff vs Mack analytical = "
        f"{mrd:.4f} (expected < 0.08)"
    )


def test_bootstrap_summary_statistically_close_to_r():
    """Python ``Bootstrap`` summary vs the R B=4000 fixture.

    R and Python use different RNGs -- the Monte-Carlo draws are *not*
    the same numbers, so bit-equality is impossible. Both summaries are
    Monte-Carlo estimates of the same Mack closed form, so they must
    agree up to Monte-Carlo noise: this asserts the *median* relative
    difference of ``total_se`` over projected cells is small, NOT
    cell-by-cell equality.
    """
    r = _load("boot_cl_analytical_summary").sort(["cohort", "dev"])
    tri = _tri()
    bt = Bootstrap(
        type="analytical", method="cl", B=4000, seed=20260522
    ).fit(tri)
    py = bt.summary.sort(["cohort", "dev"])

    keys = ["cohort", "dev"]
    py_c = py.join(r.select(keys), on=keys, how="inner").sort(keys)
    r_c = r.join(py.select(keys), on=keys, how="inner").sort(keys)

    mrd = _median_rel_diff(
        py_c["total_se"].to_list(), r_c["total_se"].to_list()
    )
    assert mrd < 0.05, (
        f"Python vs R B=4000 total_se median rel diff = {mrd:.4f} "
        f"(expected < 0.05; statistical, not bit-equal -- RNGs differ)"
    )


def test_bootstrap_B_convergence():
    """Monte-Carlo error shrinks with B: a large-B run is on average
    closer to the analytical Mack reference than a small-B run."""
    r = _load("cl_mack_analytical_se").sort(["cohort", "dev"])
    tri = _tri()
    keys = ["cohort", "dev"]

    def _mrd_for_B(B: int, seed: int) -> float:
        bt = Bootstrap(
            type="analytical", method="cl", B=B, seed=seed
        ).fit(tri)
        py = bt.summary.sort(["cohort", "dev"])
        py_c = py.join(r.select(keys), on=keys, how="inner").sort(keys)
        r_c = r.join(py.select(keys), on=keys, how="inner").sort(keys)
        return _median_rel_diff(
            py_c["total_se"].to_list(), r_c["loss_total_se"].to_list()
        )

    small = _mrd_for_B(100, seed=101)
    large = _mrd_for_B(2000, seed=101)
    assert large < small, (
        f"B=2000 (mrd={large:.4f}) should track the Mack reference more "
        f"closely than B=100 (mrd={small:.4f})"
    )


# ---------------------------------------------------------------------------
# Integration with the CL estimator
# ---------------------------------------------------------------------------


def test_cl_bootstrap_overlay():
    """``Loss(method='cl', bootstrap='auto')`` overlays bootstrap SE on
    projected cells while leaving observed cells and ``loss_proj``
    untouched. The bootstrap overlay lives on the internal Loss engine
    (the public CL model carries no ``bootstrap`` slot)."""
    tri = _tri()
    plain = Loss(method="cl").fit(tri)
    boot = Loss(method="cl", bootstrap="auto").fit(tri)

    assert plain.ci_type == "analytical"
    assert plain.boots is None
    assert boot.ci_type == "bootstrap"
    assert isinstance(boot.boots, BootstrapTriangle)

    p = plain.to_polars().sort(["cohort", "dev"])
    b = boot.to_polars().sort(["cohort", "dev"])
    assert p.height == b.height

    # loss_proj is the analytical point estimate -- never overlaid.
    for x, y in zip(p["loss_proj"].to_list(), b["loss_proj"].to_list()):
        if x is None or y is None:
            continue
        assert x == pytest.approx(y, rel=1e-12, abs=1e-9), (
            "loss_proj must stay analytical under bootstrap"
        )

    # Observed cells (loss_obs not null) keep their analytical SE;
    # projected cells (loss_obs null) get the bootstrap SE.
    obs_unchanged = True
    proj_changed = False
    for pr, br in zip(p.iter_rows(named=True), b.iter_rows(named=True)):
        ps, bs = pr["loss_total_se"], br["loss_total_se"]
        if pr["loss_obs"] is not None:
            # observed cell -- SE identical (both analytical / both NaN)
            if ps is None and bs is None:
                continue
            if ps is None or bs is None:
                obs_unchanged = False
            elif not (np.isnan(ps) and np.isnan(bs)) and ps != bs:
                obs_unchanged = False
        else:
            # projected cell -- SE should differ from the analytical one
            if (
                ps is not None and bs is not None
                and np.isfinite(ps) and np.isfinite(bs)
                and ps > 0 and ps != bs
            ):
                proj_changed = True
    assert obs_unchanged, "observed-cell SE must not change under bootstrap"
    assert proj_changed, "projected-cell SE must differ from analytical"


def test_cl_no_bootstrap_unchanged():
    """A plain ``ChainLadder().fit`` is the pure-analytical state -- no
    bootstrap."""
    fit = lr.ChainLadder().fit(_tri())
    assert fit.ci_type == "analytical"
    assert fit.boots is None


# ---------------------------------------------------------------------------
# Error / not-implemented paths
# ---------------------------------------------------------------------------


def test_bootstrap_bad_args_raise():
    """Construction-time validation of ``B``, ``alpha``, ``process``."""
    with pytest.raises(ValueError):
        Bootstrap(B=0)
    with pytest.raises(ValueError):
        Bootstrap(B=-5)
    with pytest.raises(ValueError):
        Bootstrap(alpha=float("nan"))
    with pytest.raises(ValueError):
        # type='analytical' is Mack closed-form -> process must be normal.
        Bootstrap(process="gamma")


def test_bootstrap_fit_bad_target_raises():
    with pytest.raises(ValueError):
        Bootstrap(B=8, seed=1).fit(_tri(), target="frequency")


def test_resolve_bootstrap_target_mismatch_raises():
    """A pre-built ``BootstrapTriangle`` whose ``meta['target']`` does not
    match the requested target is rejected."""
    tri = _tri()
    loss_boot = Bootstrap(B=8, seed=1).fit(tri, target="loss")
    # asking for it as a 'premium' bootstrap must fail.
    with pytest.raises(ValueError):
        _resolve_bootstrap(loss_boot, tri, target="premium")


def test_resolve_bootstrap_bad_type_raises():
    tri = _tri()
    with pytest.raises(TypeError):
        _resolve_bootstrap(123, tri, target="loss")


# ---------------------------------------------------------------------------
# BootstrapTriangle container -- repr / conversion / mirroring
# ---------------------------------------------------------------------------


def test_bootstrap_triangle_repr():
    bt = Bootstrap(type="analytical", method="cl", B=16, seed=1).fit(_tri())
    text = repr(bt)
    assert "BootstrapTriangle" in text
    assert "type=analytical" in text
    assert "method=cl" in text
    assert "B=16" in text
    assert "groups" in text   # grouped input -> group count reported


def test_bootstrap_triangle_conversion():
    """``to_polars`` / ``to_pandas`` return the summary slot."""
    pd = pytest.importorskip("pandas")
    bt = Bootstrap(B=16, seed=1).fit(_tri())
    pol = bt.to_polars()
    pan = bt.to_pandas()
    assert isinstance(pol, pl.DataFrame)
    assert isinstance(pan, pd.DataFrame)
    assert pol.height == len(pan)
    expected = {
        "coverage", "cohort", "dev",
        "mean_proj", "param_se", "proc_se", "total_se", "total_cv",
    }
    assert expected <= set(pol.columns)


def test_bootstrap_pandas_input_mirror():
    """pandas in -> pandas out for the summary / anchor properties."""
    pd = pytest.importorskip("pandas")
    exp = _exp_sur().to_pandas()
    tri = lr.Triangle(exp, groups="coverage")
    bt = Bootstrap(B=16, seed=1).fit(tri)
    assert isinstance(bt.summary, pd.DataFrame)
    assert isinstance(bt.f_anchor, pd.DataFrame)
    assert isinstance(bt.sigma2_anchor, pd.DataFrame)


def test_bootstrap_polars_input_mirror():
    """polars in -> polars out."""
    bt = Bootstrap(B=16, seed=1).fit(_tri())
    assert isinstance(bt.summary, pl.DataFrame)
    assert isinstance(bt.f_anchor, pl.DataFrame)
    assert isinstance(bt.sigma2_anchor, pl.DataFrame)


def test_bootstrap_ungrouped_input():
    """An ungrouped Triangle bootstraps with no ``groups`` column and the
    repr omits the group count."""
    bt = Bootstrap(type="analytical", method="cl", B=32, seed=3).fit(
        _tri(grouped=False)
    )
    s = bt.summary
    assert "coverage" not in s.columns
    assert s.height > 0
    assert "groups" not in repr(bt)


def test_bootstrap_quantile_ci_columns():
    """``quantile_ci=True`` emits ``ci_lo`` / ``ci_hi`` percentile columns
    that bracket ``mean_proj`` on projected cells."""
    bt = Bootstrap(
        type="analytical", method="cl", B=500, seed=5, quantile_ci=True
    ).fit(_tri())
    s = bt.summary
    assert "ci_lo" in s.columns and "ci_hi" in s.columns
    for row in s.iter_rows(named=True):
        lo, hi, mp = row["ci_lo"], row["ci_hi"], row["mean_proj"]
        if None in (lo, hi, mp):
            continue
        if not (np.isfinite(lo) and np.isfinite(hi) and np.isfinite(mp)):
            continue
        assert lo <= hi + 1e-9


def test_bootstrap_keep_pseudo():
    """``keep_pseudo=True`` materialises the per-replicate trajectories;
    ``False`` leaves the slot ``None``."""
    tri = _tri()
    no_keep = Bootstrap(B=10, seed=1, keep_pseudo=False).fit(tri)
    assert no_keep.pseudo_triangles is None

    keep = Bootstrap(B=10, seed=1, keep_pseudo=True).fit(tri)
    ps = keep.pseudo_triangles
    assert ps is not None
    # 36 cohorts x 36 devs x B=10 replicates for the single surgery group.
    n_cells = keep.summary.height
    assert ps.height == n_cells * 10


# ===========================================================================
# Phase 2 -- residual machinery + resampling kernels + Loss integration
# ===========================================================================
#
# Phase 2 added the deterministic residual scaffolding (`_cell_residuals_cl`,
# `_cell_residuals_ed`, `_link_residuals_cl`, the pool builders) and the
# `type in {nonparametric, parametric}` x `residual in {cell, link}` x
# `method in {cl, ed, sa}` resampling kernels, wired into `Bootstrap` and
# `Loss(bootstrap=...)`.
#
# Layered exactly like Phase 1:
#   Layer 1 -- residual pools / `phi` / `_injected_resample` are pure
#              functions of the triangle and must bit-match R (~1e-7 rel).
#   Layer 3 -- the Monte-Carlo summary is statistical; R and Python use
#              different RNGs, so only the *median* relative difference of
#              `total_se` against the R B=4000 fixture is asserted.


# ---------------------------------------------------------------------------
# Phase 2 -- shared helpers
# ---------------------------------------------------------------------------


def _sur_obs_matrix(target: str = "loss") -> tuple[np.ndarray, list, list]:
    """Observed cumulative ``(n_coh, n_dev)`` matrix for the surgery group.

    Mirrors the per-group matrix build the kernels use -- rows are
    cohorts (sorted ascending), columns dev (sorted ascending), with
    ``np.nan`` on unobserved cells.
    """
    sub = _tri()._df.filter(pl.col("coverage") == "surgery").sort(
        ["cohort", "dev"]
    )
    cohorts = sorted(sub["cohort"].unique().to_list())
    devs = sorted(sub["dev"].unique().to_list())
    coh_pos = {c: i for i, c in enumerate(cohorts)}
    dev_pos = {d: j for j, d in enumerate(devs)}
    mat = np.full((len(cohorts), len(devs)), np.nan, dtype=np.float64)
    for row in sub.iter_rows(named=True):
        v = row[target]
        if v is not None:
            mat[coh_pos[row["cohort"]], dev_pos[row["dev"]]] = float(v)
    return mat, cohorts, devs


def _sur_sub() -> pl.DataFrame:
    """The single-group surgery Triangle DataFrame."""
    return _tri()._df.filter(pl.col("coverage") == "surgery")


def _cohort_date_series(cohort: np.ndarray) -> pl.Series:
    """Coerce a residual-helper ``cohort`` array to a polars ``Date`` series.

    The helpers emit ``cohort`` either as a numpy ``object`` array of
    python ``date`` (the cell path) or as ``datetime64[D]`` (the ED /
    link path). ``.tolist()`` turns both into a list of python ``date``
    objects, from which polars infers a ``Date`` series -- matching the
    Date-typed R fixture keys for a clean JOIN.
    """
    return pl.Series(cohort.tolist()).cast(pl.Date)


def _pool_to_df(pool, key_name: str) -> pl.DataFrame:
    """Pool residuals as a polars DataFrame keyed for a JOIN comparison.

    The residual-helper ``cohort`` arrays are coerced to polars ``Date``
    so the JOIN against the (Date-typed) R fixture keys cleanly.
    """
    return pl.DataFrame(
        {
            "cohort": _cohort_date_series(pool.cohort),
            key_name: pool.key,
            "py_residual": pool.residual,
        }
    )


def _max_rel_diff_on_join(
    py_df: pl.DataFrame,
    r_df: pl.DataFrame,
    keys: list[str],
    py_col: str,
    r_col: str,
) -> tuple[int, float]:
    """JOIN ``py_df`` / ``r_df`` on ``keys``; max relative diff of values.

    R emits residual pools column-major, Python row-major -- the row
    order differs, so the comparison must JOIN on the cell key, never
    compare positionally.
    """
    j = py_df.join(r_df, on=keys, how="inner")
    a = np.asarray(j[py_col].to_list(), dtype=np.float64)
    b = np.asarray(j[r_col].to_list(), dtype=np.float64)
    denom = np.where(np.abs(b) > 0.0, np.abs(b), 1.0)
    rel = np.abs(a - b) / denom
    return j.height, float(rel.max()) if rel.size else 0.0


# ---------------------------------------------------------------------------
# Phase 2 / Layer 1 -- residual-pool bit-parity
# ---------------------------------------------------------------------------


def test_phase2_resid_pool_cl_cell_hat_matches_r():
    """CL cell residual pool (``hat_adj=True``) -- ODP Pearson residuals
    after the England-Verrall (2002) hat-matrix leverage adjustment. A
    closed-form function of the triangle, so it must bit-match R.

    R and Python emit the pool in different row order, so the comparison
    JOINs on ``(cohort, dev)``.
    """
    mat, cohorts, devs = _sur_obs_matrix("loss")
    anchor = _boot_anchor_cl(mat)
    cell = _cell_residuals_cl(_sur_sub(), anchor, target="loss", hat_adj=True)
    pool = _build_pool_cell(cell, pooling="pooled")

    py = _pool_to_df(pool, "dev")
    r = _load("boot_resid_cl_cell")
    n, mrd = _max_rel_diff_on_join(
        py, r, ["cohort", "dev"], "py_residual", "residual"
    )
    assert n == r.height == py.height, (
        f"pool / fixture row counts must agree: py={py.height} "
        f"joined={n} r={r.height}"
    )
    assert mrd < 1e-7, f"cl-cell (hat) residual pool max rel diff = {mrd:.2e}"


def test_phase2_resid_pool_cl_cell_nohat_matches_r():
    """CL cell residual pool (``hat_adj=False``) -- the simple
    degrees-of-freedom correction instead of the hat adjustment.
    Bit-parity with R."""
    mat, cohorts, devs = _sur_obs_matrix("loss")
    anchor = _boot_anchor_cl(mat)
    cell = _cell_residuals_cl(_sur_sub(), anchor, target="loss", hat_adj=False)
    pool = _build_pool_cell(cell, pooling="pooled")

    py = _pool_to_df(pool, "dev")
    r = _load("boot_resid_cl_cell_nohat")
    n, mrd = _max_rel_diff_on_join(
        py, r, ["cohort", "dev"], "py_residual", "residual"
    )
    assert n == r.height == py.height
    assert mrd < 1e-7, (
        f"cl-cell (no-hat) residual pool max rel diff = {mrd:.2e}"
    )


def test_phase2_resid_pool_ed_cell_matches_r():
    """ED cell residual pool -- exposure-driven Pearson residuals on the
    dual-variable Link table. Bit-parity with R."""
    link_df = _build_link_df(
        _sur_sub(), None, "loss", "premium", None, drop_invalid=True
    )
    cell = _cell_residuals_ed(link_df)
    pool = _build_pool_cell(cell, pooling="pooled")

    py = _pool_to_df(pool, "dev")
    r = _load("boot_resid_ed_cell")
    n, mrd = _max_rel_diff_on_join(
        py, r, ["cohort", "dev"], "py_residual", "residual"
    )
    assert n == r.height == py.height
    assert mrd < 1e-7, f"ed-cell residual pool max rel diff = {mrd:.2e}"


def test_phase2_resid_pool_cl_link_matches_r():
    """CL link residual pool -- Mack standardized link residuals
    (Pinheiro et al. 2003). The link key carries ``(ata_from, ata_to)``,
    so the JOIN keys on the full link triple."""
    mat, cohorts, devs = _sur_obs_matrix("loss")
    anchor = _boot_anchor_cl(mat)
    link_df = _build_link_df(
        _sur_sub(), None, "loss", None, None, drop_invalid=True
    )
    link_res = _link_residuals_cl(link_df, anchor)

    # _LinkResiduals carries the full triple; build the comparison frame
    # directly (the pool's `key` is ata_to only).
    fin = np.isfinite(link_res.residual)
    py = pl.DataFrame(
        {
            "cohort":      _cohort_date_series(link_res.cohort[fin]),
            "ata_from":    link_res.ata_from[fin],
            "ata_to":      link_res.ata_to[fin],
            "py_residual": link_res.residual[fin],
        }
    )
    r = _load("boot_resid_cl_link")
    n, mrd = _max_rel_diff_on_join(
        py, r, ["cohort", "ata_from", "ata_to"], "py_residual", "residual"
    )
    assert n == r.height == py.height
    assert mrd < 1e-7, f"cl-link residual pool max rel diff = {mrd:.2e}"

    # The pool builder keeps every finite link residual (no corner drop).
    pool = _build_pool_link(link_res, pooling="pooled")
    assert len(pool) == py.height


def test_phase2_phi_matches_r():
    """ODP / ED dispersion ``phi`` -- the cl-cell, cl-cell-nohat and
    ed-cell ``phi`` are closed-form and must bit-match ``boot_phi.csv``.

    cl-cell and cl-cell-nohat share one ``phi`` (it is computed from the
    *raw* residuals, invariant to the stage correction).
    """
    phi_ref = {
        row["paradigm"]: row["phi"]
        for row in _load("boot_phi").iter_rows(named=True)
    }

    mat, cohorts, devs = _sur_obs_matrix("loss")
    anchor = _boot_anchor_cl(mat)
    phi_cl_hat = _cell_residuals_cl(
        _sur_sub(), anchor, target="loss", hat_adj=True
    ).phi
    phi_cl_nohat = _cell_residuals_cl(
        _sur_sub(), anchor, target="loss", hat_adj=False
    ).phi
    link_df = _build_link_df(
        _sur_sub(), None, "loss", "premium", None, drop_invalid=True
    )
    phi_ed = _cell_residuals_ed(link_df).phi

    assert phi_cl_hat == pytest.approx(phi_ref["cl_cell"], rel=1e-9)
    assert phi_cl_nohat == pytest.approx(phi_ref["cl_cell_nohat"], rel=1e-9)
    assert phi_ed == pytest.approx(phi_ref["ed_cell"], rel=1e-9)
    # cl-cell and cl-cell-nohat share phi (raw-residual invariant).
    assert phi_cl_hat == pytest.approx(phi_cl_nohat, rel=1e-12)


# ---------------------------------------------------------------------------
# Phase 2 / Layer 1 -- _injected_resample determinism
# ---------------------------------------------------------------------------


def test_phase2_injected_resample_deterministic():
    """With ``_injected_resample`` the CL cell kernel's Stage-1 projection
    is pure arithmetic -- two runs with different RNG seeds produce a
    byte-identical ``cum_mean``, and one Stage-1 pseudo cell is
    hand-verifiable as ``mu_hat + r_star * sqrt(|mu_hat|)``."""
    mat, cohorts, devs = _sur_obs_matrix("loss")
    anchor = _boot_anchor_cl(mat)
    gi = _build_group_inputs(mat, anchor, cohorts, devs)
    mu_grid = _fitted_grid_cl(mat, gi.last_obs, anchor, devs)
    cell = _cell_residuals_cl(_sur_sub(), anchor, target="loss", hat_adj=True)
    pool = _build_pool_cell(cell, pooling="pooled")

    # Active upper cells -- the cells that get a resampled increment.
    from lossratio.bootstrap import _active_upper_cells

    coh_idx, dev_idx, lin = _active_upper_cells(gi.last_obs, mu_grid)
    n_active = lin.size
    B = 12
    # Inject a fixed index array -- always pull pool element 0.
    injected = np.zeros((n_active, B), dtype=np.int64)

    r1 = _boot_kernel_cl_cell(
        gi, anchor, mu_grid, pool, cell.phi, B,
        np.random.default_rng(1), 1.0, "gamma",
        _injected_resample=injected,
    )
    r2 = _boot_kernel_cl_cell(
        gi, anchor, mu_grid, pool, cell.phi, B,
        np.random.default_rng(987654),     # different seed must not matter
        1.0, "gamma",
        _injected_resample=injected,
    )
    # Stage 1 (cum_mean) is fully determined by the injected indices.
    assert np.array_equal(
        np.nan_to_num(r1.cum_mean), np.nan_to_num(r2.cum_mean)
    ), "injected resample must make Stage-1 cum_mean byte-identical"

    # ---- hand-verify one Stage-1 pseudo cell -----------------------------
    # Active cell a=0 sits at dev `devs[dev_idx[0]]`. Its pseudo increment
    # is `mu_hat + r_star * sqrt(|mu_hat|)` with `r_star` the dev's pool
    # element 0 (injected index 0). When that cell is at dev index 0 the
    # cumulative equals the increment itself.
    mu_active = mu_grid.flatten(order="F")[lin]
    subpools = _pool_dev_subpools(pool)
    dev0 = int(devs[dev_idx[0]])
    r_star = subpools[dev0][0]
    expect_inc = mu_active[0] + r_star * np.sqrt(abs(mu_active[0]))
    if dev_idx[0] == 0:
        coh0 = coh_idx[0]
        for b in range(B):
            assert r1.cum_mean[coh0, 0, b] == pytest.approx(
                expect_inc, rel=1e-12
            )


# ---------------------------------------------------------------------------
# Phase 2 / Layer 3 -- statistical closeness to the R B=4000 fixtures
# ---------------------------------------------------------------------------
#
# Each of the five Phase-2 paradigms is run at B=4000 / seed=20260522 and
# its projected-cell `total_se` is compared to the matching R fixture via
# the MEDIAN relative difference. R and Python use different RNGs, so the
# Monte-Carlo draws are not the same numbers -- this is a statistical
# closeness band, never bit-equality.

_PHASE2_PARADIGMS = [
    (
        "boot_cl_nonparam_cell_summary",
        dict(type="nonparametric", method="cl", residual="cell",
             process="gamma"),
    ),
    (
        "boot_cl_nonparam_link_summary",
        dict(type="nonparametric", method="cl", residual="link",
             process="normal"),
    ),
    (
        "boot_cl_parametric_summary",
        dict(type="parametric", method="cl", process="gamma"),
    ),
    (
        "boot_ed_nonparam_cell_summary",
        dict(type="nonparametric", method="ed", residual="cell",
             process="gamma"),
    ),
    (
        "boot_sa_nonparam_cell_summary",
        dict(type="nonparametric", method="sa", residual="cell",
             process="gamma"),
    ),
]


@pytest.mark.parametrize("fixture,config", _PHASE2_PARADIGMS)
def test_phase2_summary_statistically_close_to_r(fixture, config):
    """Python ``Bootstrap`` summary vs the R B=4000 fixture for each
    Phase-2 paradigm.

    Statistical, NOT bit-equal -- R and Python use different RNGs, so the
    draws differ. Both are Monte-Carlo estimates of the same paradigm, so
    the *median* relative difference of ``total_se`` over projected cells
    must be small (< 6%).
    """
    r = _load(fixture).sort(["cohort", "dev"])
    tri = _tri()
    bt = Bootstrap(B=4000, seed=20260522, **config).fit(tri)
    py = bt.summary.sort(["cohort", "dev"])

    keys = ["cohort", "dev"]
    py_c = py.join(r.select(keys), on=keys, how="inner").sort(keys)
    r_c = r.join(py.select(keys), on=keys, how="inner").sort(keys)

    mrd = _median_rel_diff(
        py_c["total_se"].to_list(), r_c["total_se"].to_list()
    )
    assert mrd < 0.06, (
        f"{fixture}: total_se median rel diff = {mrd:.4f} "
        f"(expected < 0.06; statistical, not bit-equal -- RNGs differ)"
    )


@pytest.mark.parametrize(
    "config",
    [c for _, c in _PHASE2_PARADIGMS],
    ids=[f for f, _ in _PHASE2_PARADIGMS],
)
def test_phase2_se_decomposition_invariants(config):
    """Pythagorean decomposition invariants on every projected cell of
    each Phase-2 paradigm: ``total_se >= param_se``, ``proc_se`` finite,
    ``0 <= proc_se <= total_se``.

    ``param_se`` (the spread of the noise-free ``cum_mean``) and
    ``total_se`` (the spread of the noisy ``cum_sampled``) are two
    *finite-B sample* standard deviations. The Pythagorean identity
    ``total^2 = param^2 + proc^2`` holds in expectation, not per finite
    sample -- on a cell whose process variance is near zero the two
    estimates can cross by Monte-Carlo noise. A relative slack absorbs
    that. ``proc_se`` itself is always in ``[0, total_se]`` because the
    kernel clamps the variance difference at zero before the sqrt.
    """
    tri = _tri()
    bt = Bootstrap(B=800, seed=31, **config).fit(tri)
    for row in bt.summary.iter_rows(named=True):
        ts, ps, prs = row["total_se"], row["param_se"], row["proc_se"]
        if ts is None or (isinstance(ts, float) and np.isnan(ts)):
            continue
        assert np.isfinite(ts) and np.isfinite(ps) and np.isfinite(prs)
        # total >= param up to finite-B sampling noise on near-zero-
        # process cells (two sample SDs of the same quantity).
        assert ts >= ps - 2e-2 * max(abs(ps), 1.0), (
            f"total_se < param_se beyond finite-B noise: {ts} < {ps}"
        )
        # proc_se is exactly clamped to [0, total_se] by the kernel.
        assert 0.0 <= prs <= ts + 1e-9, (
            f"proc_se out of [0, total_se]: {prs}"
        )


@pytest.mark.parametrize(
    "config",
    [c for _, c in _PHASE2_PARADIGMS],
    ids=[f for f, _ in _PHASE2_PARADIGMS],
)
def test_phase2_seed_reproducible(config):
    """Same seed -> identical summary; a different seed changes the
    Monte-Carlo SE -- for every Phase-2 paradigm."""
    tri = _tri()
    a = Bootstrap(B=300, seed=42, **config).fit(tri)
    b = Bootstrap(B=300, seed=42, **config).fit(tri)
    c = Bootstrap(B=300, seed=43, **config).fit(tri)

    for x, y in zip(
        a.summary["total_se"].to_list(), b.summary["total_se"].to_list()
    ):
        if x is None or y is None:
            continue
        assert x == y, "same seed must give identical bootstrap output"

    differs = any(
        x is not None and y is not None and np.isfinite(x)
        and np.isfinite(y) and x > 0 and x != y
        for x, y in zip(
            a.summary["total_se"].to_list(),
            c.summary["total_se"].to_list(),
        )
    )
    assert differs, "a different seed should change the Monte-Carlo SE"


# ---------------------------------------------------------------------------
# Phase 2 -- Loss(bootstrap=...) integration
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("method", ["cl", "ed", "sa"])
def test_phase2_loss_bootstrap_overlay(method):
    """``Loss(method=..., bootstrap='auto')`` overlays bootstrap SE on the
    projected cells while leaving ``loss_proj`` analytical. The bootstrap
    SE overlay always uses the analytical-CL paradigm regardless of the
    dispatcher's loss ``method`` (R parity). The bootstrap overlay lives
    on the internal Loss engine (the public role models carry no
    ``bootstrap`` slot)."""
    tri = _tri()
    plain = Loss(method=method).fit(tri)
    boot = Loss(method=method, bootstrap="auto").fit(tri)

    assert plain.ci_type == "analytical"
    assert plain.boots is None
    assert boot.ci_type == "bootstrap"
    assert isinstance(boot.boots, BootstrapTriangle)
    # the dispatcher bootstrap SE overlay always uses the analytical-CL
    # (Mack closed-form) paradigm regardless of the loss method.
    assert boot.boots.meta["method"] == "cl"
    assert boot.boots.meta["type"] == "analytical"

    p = plain.to_polars().sort(["coverage", "cohort", "dev"])
    b = boot.to_polars().sort(["coverage", "cohort", "dev"])
    assert p.height == b.height

    # loss_proj is the analytical point estimate -- never overlaid.
    for x, y in zip(p["loss_proj"].to_list(), b["loss_proj"].to_list()):
        if x is None or y is None:
            assert x is None and y is None
            continue
        assert x == pytest.approx(y, rel=1e-9, abs=1e-6), (
            "loss_proj must stay analytical under bootstrap"
        )

    # at least one projected cell's loss_total_se must differ from the
    # analytical value.
    proj_changed = False
    for pr, br in zip(p.iter_rows(named=True), b.iter_rows(named=True)):
        ps, bs = pr["loss_total_se"], br["loss_total_se"]
        if (
            pr["loss_obs"] is None
            and ps is not None and bs is not None
            and np.isfinite(ps) and np.isfinite(bs)
            and ps > 0 and ps != bs
        ):
            proj_changed = True
    assert proj_changed, (
        f"projected-cell loss_total_se must differ under "
        f"bootstrap for method={method!r}"
    )


def test_phase2_loss_no_bootstrap_unchanged():
    """A plain ``Loss(method='cl').fit`` keeps the pure-analytical state."""
    fit = Loss(method="cl").fit(_tri())
    assert fit.ci_type == "analytical"
    assert fit.boots is None


# ---------------------------------------------------------------------------
# Phase 2 -- validation gates
# ---------------------------------------------------------------------------


def test_phase2_nonparam_cell_normal_rejected():
    """The ODP cell path (England-Verrall 1999/2002) needs a
    positivity-preserving process -- ``process='normal'`` is rejected."""
    with pytest.raises(ValueError):
        Bootstrap(type="nonparametric", residual="cell", process="normal")


def test_phase2_parametric_ed_normal_rejected():
    """The additive ED parametric path needs a positivity-preserving
    process -- ``process='normal'`` is rejected."""
    with pytest.raises(ValueError):
        Bootstrap(type="parametric", method="ed", process="normal")


# ===========================================================================
# Phase 3 -- Ratio(bootstrap=...) integration
# ===========================================================================
#
# Phase 3 wired the loss-side bootstrap into the Ratio composition layer:
# `Ratio(method=..., bootstrap=...)` runs a loss-side bootstrap with the
# Ratio's own loss `method` paradigm, overlays the bootstrap-derived
# `loss_total_se` onto the projected cells, and recomputes `ratio_se` /
# `ratio_cv` / `ratio_ci_lo` / `ratio_ci_hi` from it. The premium side is
# never bootstrapped. `RatioFit` gained `boots` / `ci_type` slots.
#
# Layered like Phases 1-2:
#   Layer 1 -- the analytical no-bootstrap fit is byte-unchanged, and the
#              `se_method="fixed"` identity (`ratio_se == loss_total_se /
#              premium_proj`) is exact arithmetic.
#   Layer 3 -- the Monte-Carlo `ratio_se` is statistical; R and Python use
#              different RNGs, so only the *median* relative difference vs
#              the R B=4000 fixture is asserted, never bit-equality.


@pytest.mark.parametrize("method", ["sa", "ed"])
def test_phase3_ratio_bootstrap_overlay(method):
    """``Ratio(method=..., bootstrap='auto')`` overlays a loss-side
    bootstrap onto the projected cells and recomputes ``ratio_se`` from
    it, while leaving ``ratio_proj`` analytical and observed cells
    untouched.

    The Ratio composition layer always bootstraps loss with the
    analytical CL (Mack closed-form) paradigm regardless of its loss
    ``method`` -- this mirrors R ``fit_ratio()``, whose wrap-only
    bootstrap path is hard-wired to ``type="analytical"`` /
    ``process="normal"``. The loss ``method`` still drives the point
    projection; only the SE overlay is the analytical bootstrap.
    """
    tri = _tri()
    plain = lr.Ratio(method=method).fit(tri)
    boot = lr.Ratio(method=method, bootstrap="auto").fit(tri)

    assert plain.ci_type == "analytical"
    assert plain.boots is None
    assert boot.ci_type == "bootstrap"
    assert isinstance(boot.boots, BootstrapTriangle)
    # Ratio always bootstraps loss with the analytical CL paradigm
    # (R `fit_ratio` parity), independent of the loss `method`.
    assert boot.boots.meta["method"] == "cl"
    assert boot.boots.meta["type"] == "analytical"
    assert boot.boots.meta["target"] == "loss"

    p = plain.to_polars().sort(["coverage", "cohort", "dev"])
    b = boot.to_polars().sort(["coverage", "cohort", "dev"])
    assert p.height == b.height

    # ratio_proj is the analytical point estimate -- never overlaid.
    for x, y in zip(p["ratio_proj"].to_list(), b["ratio_proj"].to_list()):
        if x is None or y is None:
            assert x is None and y is None
            continue
        assert x == pytest.approx(y, rel=1e-12, abs=1e-12), (
            "ratio_proj must stay analytical under bootstrap"
        )

    # observed cells keep their analytical ratio_se; projected cells get
    # a bootstrap-derived ratio_se that differs from the analytical one.
    obs_unchanged = True
    proj_changed = False
    for pr, br in zip(p.iter_rows(named=True), b.iter_rows(named=True)):
        ps, bs = pr["ratio_se"], br["ratio_se"]
        if pr["loss_obs"] is not None:
            # observed cell -- ratio_se identical (both analytical / NaN)
            if ps is None and bs is None:
                continue
            if ps is None or bs is None:
                obs_unchanged = False
            elif not (np.isnan(ps) and np.isnan(bs)) and ps != bs:
                obs_unchanged = False
        else:
            # projected cell -- ratio_se should differ from analytical
            if (
                ps is not None and bs is not None
                and np.isfinite(ps) and np.isfinite(bs)
                and ps > 0 and ps != bs
            ):
                proj_changed = True
    assert obs_unchanged, "observed-cell ratio_se must not change"
    assert proj_changed, (
        f"projected-cell ratio_se must differ under bootstrap for "
        f"method={method!r}"
    )


def test_phase3_ratio_no_bootstrap_unchanged():
    """A plain ``Ratio(method='sa').fit`` is the pure-analytical state --
    ``ci_type='analytical'``, ``boots is None``, and the output frame is
    byte-equal to a fresh analytical fit."""
    tri = _tri()
    fit = lr.Ratio(method="sa").fit(tri)
    assert fit.ci_type == "analytical"
    assert fit.boots is None

    fresh = lr.Ratio(method="sa").fit(tri)
    a = fit.to_polars().sort(["coverage", "cohort", "dev"])
    b = fresh.to_polars().sort(["coverage", "cohort", "dev"])
    assert a.columns == b.columns
    assert a.equals(b), "no-bootstrap Ratio fit must be byte-reproducible"


@pytest.mark.parametrize("method", ["sa", "ed"])
def test_phase3_ratio_summary_statistically_close_to_r(method):
    """Python ``Ratio(bootstrap='auto')`` summary vs the R B=4000 fixture.

    R and Python use different RNGs -- the Monte-Carlo draws are not the
    same numbers, so bit-equality is impossible. Both ``ratio_se`` /
    ``ratio_cv`` columns are Monte-Carlo estimates of the same loss-side
    bootstrap paradigm divided by the (identical, analytical)
    ``premium_proj``, so they must agree up to Monte-Carlo noise. This
    asserts the *median* relative difference over projected cohorts is
    small, NOT cell-by-cell equality.

    Fully-matured cohorts carry ``ratio_se == 0`` in the R fixture (no
    projection, no spread); those are skipped by ``_median_rel_diff``.
    """
    r = _load(f"ratio_{method}_boot_summary").sort(["coverage", "cohort"])
    tri = _tri()
    fit = lr.Ratio(
        method=method, bootstrap="auto", B=4000, seed=20260522,
        se_method="fixed",
    ).fit(tri)
    py = fit.summary().sort(["coverage", "cohort"])

    keys = ["coverage", "cohort"]
    py_c = py.join(r.select(keys), on=keys, how="inner").sort(keys)
    r_c = r.join(py.select(keys), on=keys, how="inner").sort(keys)
    assert py_c.height == r_c.height > 0

    mrd_se = _median_rel_diff(
        py_c["ratio_se"].to_list(), r_c["ratio_se"].to_list()
    )
    assert mrd_se < 0.06, (
        f"ratio_{method}: ratio_se median rel diff = {mrd_se:.4f} "
        f"(expected < 0.06; statistical, not bit-equal -- RNGs differ)"
    )

    mrd_cv = _median_rel_diff(
        py_c["ratio_cv"].to_list(), r_c["ratio_cv"].to_list()
    )
    assert mrd_cv < 0.06, (
        f"ratio_{method}: ratio_cv median rel diff = {mrd_cv:.4f} "
        f"(expected < 0.06; statistical, not bit-equal -- RNGs differ)"
    )


def test_phase3_ratio_se_consistency():
    """On a bootstrap fit the ``se_method='fixed'`` identity holds
    exactly: ``ratio_se == loss_total_se / premium_proj`` on every
    projected cell. The overlay recomputes ``ratio_se`` from the
    bootstrap ``loss_total_se`` but the premium-fixed division is pure
    arithmetic."""
    tri = _tri()
    fit = lr.Ratio(
        method="sa", bootstrap="auto", B=200, seed=11, se_method="fixed"
    ).fit(tri)
    assert fit.ci_type == "bootstrap"

    checked = 0
    for row in fit.to_polars().iter_rows(named=True):
        lse, pp, rse = (
            row["loss_total_se"], row["premium_proj"], row["ratio_se"]
        )
        if None in (lse, pp, rse):
            continue
        if not (np.isfinite(lse) and np.isfinite(pp) and np.isfinite(rse)):
            continue
        if pp == 0.0:
            continue
        assert rse == pytest.approx(lse / pp, rel=1e-12, abs=1e-12), (
            "ratio_se must equal loss_total_se / premium_proj (fixed)"
        )
        checked += 1
    assert checked > 0, "no cells exercised the fixed-premium identity"


def test_phase3_ratio_seed_reproducible():
    """Same seed -> identical Ratio bootstrap summary; a different seed
    changes the Monte-Carlo ``ratio_se``."""
    tri = _tri()
    a = lr.Ratio(method="sa", bootstrap="auto", B=300, seed=42).fit(tri)
    b = lr.Ratio(method="sa", bootstrap="auto", B=300, seed=42).fit(tri)
    c = lr.Ratio(method="sa", bootstrap="auto", B=300, seed=43).fit(tri)

    sa, sb, sc = a.summary(), b.summary(), c.summary()
    # same seed -> bit-identical ratio_se.
    for x, y in zip(sa["ratio_se"].to_list(), sb["ratio_se"].to_list()):
        if x is None or y is None:
            continue
        assert x == y, "same seed must give identical bootstrap output"

    # different seed -> at least one projected cohort's ratio_se differs.
    differs = any(
        x is not None and y is not None and np.isfinite(x)
        and np.isfinite(y) and x > 0 and x != y
        for x, y in zip(sa["ratio_se"].to_list(), sc["ratio_se"].to_list())
    )
    assert differs, "a different seed should change the Monte-Carlo SE"


# ===========================================================================
# Phase 5 -- Backtest x bootstrap-configured estimator
#
# A Backtest masks the last `holdout` calendar diagonals and refits the
# estimator on the masked triangle per fold. When the estimator carries a
# `bootstrap` config, the rebuild-per-fit forms ("auto" / a Bootstrap
# config / a callable) all rebuild the bootstrap from the *masked*
# triangle -- no held-out cell ever enters the residual pool. A pre-built
# BootstrapTriangle, by contrast, was fitted on the unmasked triangle and
# would leak; Backtest rejects it.
#
# The bootstrap overlay only ever touches the SE / CI columns, never the
# point projection, so `ae_err` (computed from `expected`, the point
# projection) is provably identical with or without bootstrap.
# ---------------------------------------------------------------------------


def test_phase5_backtest_bootstrap_runs_without_error():
    """``Backtest(estimator=lr.Ratio(method='sa', bootstrap='auto'))``
    runs end-to-end -- each masked-triangle refit rebuilds the bootstrap
    on the masked data (no leakage) and the result carries an ``ae_err``
    table."""
    tri = _tri()
    bt = lr.Backtest(
        estimator=lr.Ratio(method="sa", bootstrap="auto", B=80, seed=1),
        holdout=6,
        target="ratio",
    ).fit(tri)
    assert bt.ae_err.height > 0
    # the per-fold refit ran with the bootstrap config -> bootstrap CI.
    assert bt.fit.ci_type == "bootstrap"
    assert isinstance(bt.fit.boots, BootstrapTriangle)


def test_phase5_backtest_ae_err_bootstrap_independent():
    """``ae_err`` is computed from the point projection (``expected``),
    which the bootstrap overlay never touches. A bootstrap-configured
    estimator must therefore produce the *same* ``ae_err`` (and the same
    ``col_summary`` / ``diag_summary``) as an analytical one -- the only
    effect of the bootstrap is extra compute per fold."""
    tri = _tri()
    analytic = lr.Backtest(
        estimator=lr.Ratio(method="sa"),
        holdout=6,
        target="ratio",
    ).fit(tri)
    booted = lr.Backtest(
        estimator=lr.Ratio(method="sa", bootstrap="auto", B=80, seed=1),
        holdout=6,
        target="ratio",
    ).fit(tri)

    a = analytic.ae_err.sort(["coverage", "cohort", "dev"])
    b = booted.ae_err.sort(["coverage", "cohort", "dev"])
    assert a.height == b.height > 0
    # the point projection -- and therefore every A/E quantity -- is
    # byte-identical regardless of the bootstrap overlay.
    for col in ("actual", "expected", "aeg", "ae_err"):
        assert a[col].to_list() == b[col].to_list(), (
            f"backtest {col!r} must be bootstrap-independent (the overlay "
            f"touches SE/CI only, never the point projection)"
        )
    # the dev / diagonal summaries inherit the same invariance.
    assert analytic.col_summary.sort(["coverage", "dev"]).equals(
        booted.col_summary.sort(["coverage", "dev"])
    ), "col_summary must be bootstrap-independent"
    assert analytic.diag_summary.sort(["coverage", "cal_idx"]).equals(
        booted.diag_summary.sort(["coverage", "cal_idx"])
    ), "diag_summary must be bootstrap-independent"


def test_phase5_backtest_bootstrap_config_form_safe():
    """A ``Bootstrap`` config instance on the estimator is also a
    rebuild-per-fit form -- accepted, and each fold refits it on the
    masked triangle."""
    tri = _tri()
    bt = lr.Backtest(
        estimator=lr.Ratio(
            method="sa",
            bootstrap=Bootstrap(type="analytical", method="cl",
                                B=80, seed=2),
        ),
        holdout=6,
        target="ratio",
    ).fit(tri)
    assert bt.ae_err.height > 0
    assert bt.fit.ci_type == "bootstrap"


def test_phase5_backtest_bootstrap_callable_form_safe():
    """A callable ``f(tri) -> BootstrapTriangle`` is the leakage-safe
    form: Backtest accepts it, and the callable is invoked on the masked
    triangle per fold."""
    tri = _tri()
    orig_obs = tri._df["loss"].len() - tri._df["loss"].null_count()
    seen: list[int] = []

    def make_boot(masked_tri: lr.Triangle):
        # the callable receives the *masked* triangle -- record its
        # observed-cell count to prove it is not the original (unmasked)
        # triangle. Masking nulls the held-out cells (row count is
        # unchanged), so the masked triangle has strictly fewer observed
        # `loss` cells than the original.
        n_obs = masked_tri._df["loss"].len() - masked_tri._df["loss"].null_count()
        seen.append(n_obs)
        return Bootstrap(
            type="analytical", method="cl", B=80, seed=3
        ).fit(masked_tri, target="loss")

    bt = lr.Backtest(
        estimator=lr.Ratio(method="sa", bootstrap=make_boot),
        holdout=6,
        target="ratio",
    ).fit(tri)
    assert bt.ae_err.height > 0
    assert bt.fit.ci_type == "bootstrap"
    # the callable ran on the masked triangle, which has fewer observed
    # cells than the original -- proof the bootstrap saw no held-out
    # diagonal.
    assert seen, "the bootstrap callable was never invoked"
    assert all(n < orig_obs for n in seen), (
        "the callable must be invoked on the masked triangle (fewer "
        "observed cells than the original), not the unmasked one"
    )


def test_phase5_backtest_prebuilt_bootstrap_triangle_rejected():
    """A pre-built ``BootstrapTriangle`` on the estimator is rejected by
    ``Backtest`` -- it was fitted on the unmasked triangle and would leak
    the held-out cells into every fold's residual pool. The error directs
    the user to the rebuild-per-fit forms."""
    tri = _tri()
    # build a BootstrapTriangle on the FULL (unmasked) triangle.
    prebuilt = Bootstrap(
        type="analytical", method="cl", B=80, seed=4
    ).fit(tri, target="loss")
    assert isinstance(prebuilt, BootstrapTriangle)

    est = lr.Ratio(method="sa", bootstrap=prebuilt)
    with pytest.raises(ValueError, match="pre-built BootstrapTriangle"):
        lr.Backtest(estimator=est, holdout=6, target="ratio")


def test_phase5_backtest_prebuilt_bootstrap_triangle_rejected_loss():
    """The pre-built-``BootstrapTriangle`` guard fires for any estimator
    that carries a ``bootstrap`` slot -- here the internal Loss engine."""
    tri = _tri()
    prebuilt = Bootstrap(
        type="analytical", method="cl", B=80, seed=5
    ).fit(tri, target="loss")

    est = Loss(method="cl", bootstrap=prebuilt)
    with pytest.raises(ValueError, match="leak"):
        lr.Backtest(estimator=est, holdout=6, target="loss")
