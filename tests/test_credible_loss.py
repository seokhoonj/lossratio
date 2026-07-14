"""CredibleLoss -- partial-pooling credibility estimator.

Pooled intensity g_k + per-cohort credibility level u_i (Bühlmann-Straub
conjugate), projecting u_i * g_k * P. Tests pin the exact ladder collapse to
PooledLoss at psi=0, the per-cohort level scaling, point-only SE, and the
config guards.
"""
from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr
from lossratio.estimators.credible_loss import CredibleLoss
from lossratio.estimators.pooled_loss import PooledLoss

KEY = ["coverage", "cohort", "duration"]
PROJ_COLS = ["loss_proj"]


def _to_polars(obj) -> pl.DataFrame:
    return obj if isinstance(obj, pl.DataFrame) else pl.from_pandas(obj)


def test_credibility_diagnostics_exposed(tri):
    cred = CredibleLoss().fit(tri)
    c = _to_polars(cred.credibility)
    assert c.columns == ["coverage", "cohort", "u", "Z", "psi"]
    # one row per cohort x segment
    n = cred.to_polars().select(["coverage", "cohort"]).unique().height
    assert c.height == n
    # sane ranges: u >= 0 (recovery floor), 0 <= Z <= 1, psi >= 0
    assert (c["u"] >= 0).all()
    assert ((c["Z"] >= 0) & (c["Z"] <= 1)).all()
    assert (c["psi"] >= 0).all()
    # psi is constant within a segment
    per_seg = c.group_by("coverage").agg(pl.col("psi").n_unique())
    assert (per_seg["psi"] == 1).all()


def test_credibility_psi_zero_is_pooled_collapse(tri):
    c = _to_polars(CredibleLoss(psi=0).fit(tri).credibility)
    assert (c["u"] == 1.0).all()
    assert (c["Z"] == 0.0).all()
    assert (c["psi"] == 0.0).all()


def test_pooled_and_link_have_no_credibility(tri):
    from lossratio.estimators.chain_ladder import ChainLadder
    assert PooledLoss().fit(tri).credibility is None
    assert ChainLadder().fit(tri).credibility is None


def test_psi_zero_collapses_to_pooled(tri):
    # psi = 0 -> no between-cohort variance -> u = 1 -> exactly PooledLoss
    # (the ladder's automatic collapse). Byte-identical on every projection col.
    pooled = PooledLoss().fit(tri).to_polars()
    cred0 = CredibleLoss(psi=0).fit(tri).to_polars()
    j = pooled.select(KEY + PROJ_COLS).join(
        cred0.select(KEY + PROJ_COLS), on=KEY, suffix="_c0"
    )
    for c in PROJ_COLS:
        diff = (j[c].fill_null(0.0) - j[f"{c}_c0"].fill_null(0.0)).abs().max()
        assert diff == 0.0


def test_psi_auto_is_credibility_active(tri):
    # On the real book psi_hat > 0, so the credible projection departs from the
    # pooled one.
    pooled = PooledLoss().fit(tri).to_polars()
    cred = CredibleLoss().fit(tri).to_polars()
    j = pooled.select(KEY + ["loss_proj"]).join(
        cred.select(KEY + ["loss_proj"]), on=KEY, suffix="_c"
    )
    assert (j["loss_proj"].fill_null(0.0) - j["loss_proj_c"].fill_null(0.0)).abs().max() > 0.0


def test_credible_scales_pooled_increment_by_cohort_level(tri):
    # The credible increment is u_i * g_k * P_k, the pooled increment is
    # g_k * P_k (same g_k, same premium projection), so their ratio on the
    # projected cells is the cohort level u_i -- CONSTANT within a cohort.
    pooled = PooledLoss().fit(tri).to_polars()
    cred = CredibleLoss().fit(tri).to_polars()
    j = (
        pooled.select(KEY + ["incr_loss_proj", "source"])
        .join(cred.select(KEY + ["incr_loss_proj"]), on=KEY, suffix="_c")
        .filter((pl.col("source") == "own") & (pl.col("incr_loss_proj").abs() > 1e-9))
        .with_columns((pl.col("incr_loss_proj_c") / pl.col("incr_loss_proj")).alias("u"))
    )
    assert j.height > 0
    spread = j.group_by(["coverage", "cohort"]).agg(
        (pl.col("u").max() - pl.col("u").min()).alias("spread")
    )
    assert spread["spread"].max() == pytest.approx(0.0, abs=1e-9)   # u_i constant per cohort
    assert (j["u"] - 1.0).abs().max() > 1e-6  # and not all 1 (credibility active)


def test_point_only_se_is_null(tri):
    # The credibility level's estimation variance breaks the analytical
    # recursion, so v1 leaves SE / CI null (bootstrap comes later).
    cred = _to_polars(CredibleLoss().fit(tri).df)
    for c in ("loss_proc_se", "loss_param_se", "loss_total_se",
              "loss_ci_lo", "loss_ci_hi"):
        assert cred[c].is_null().all()


def test_model_label(tri):
    fit = CredibleLoss().fit(tri)
    assert fit.model == "credible_loss"
    assert fit.method == "credible"


def test_rejects_unsupported_and_bad_psi():
    # recent (calendar-diagonal fit window) is supported; only a bad psi is
    # rejected.
    assert CredibleLoss(recent=6).recent == 6
    for bad in (-1.0, "nope"):
        with pytest.raises(ValueError):
            CredibleLoss(psi=bad)


def test_recent_window_changes_the_fit():
    import lossratio as lr

    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    full = CredibleLoss().fit(tri).to_polars()
    rec = CredibleLoss(recent=12).fit(tri).to_polars()
    j = (full.select(["coverage", "cohort", "duration", "loss_proj"])
         .rename({"loss_proj": "f"})
         .join(rec.select(["coverage", "cohort", "duration", "loss_proj"])
               .rename({"loss_proj": "r"}),
               on=["coverage", "cohort", "duration"]).drop_nulls())
    assert ((j["f"] - j["r"]).abs() > 1e-6).sum() > 0      # recent re-estimates factors
    # recent=None is the unchanged full-triangle fit
    assert CredibleLoss().fit(tri).to_polars().equals(
        CredibleLoss(recent=None).fit(tri).to_polars())


def _single_cohort_input() -> pl.DataFrame:
    # one underwriting cohort observed over four months -> every from-duration
    # has a single cell (df-deficient dispersion) and there is only one cohort:
    # the degenerate cases that must collapse to pooled, not crash.
    return pl.DataFrame({
        "uy_m": ["2024-01-01"] * 4,
        "cy_m": ["2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01"],
        "incr_loss": [100.0, 80.0, 60.0, 40.0],
        "incr_premium": [100.0, 100.0, 100.0, 100.0],
    })


def test_degenerate_single_cohort_collapses_to_pooled():
    tri1 = lr.Triangle(_single_cohort_input())
    cred = CredibleLoss().fit(tri1).to_polars()      # must not raise
    pooled = PooledLoss().fit(tri1).to_polars()
    key = ["cohort", "duration"]
    j = pooled.select(key + ["loss_proj"]).join(
        cred.select(key + ["loss_proj"]), on=key, suffix="_c"
    )
    diff = (j["loss_proj"].fill_null(0.0) - j["loss_proj_c"].fill_null(0.0)).abs().max()
    assert diff == 0.0


def test_zero_increment_duration_does_not_nan_the_level():
    # finding: a zero-increment duration makes m0 = g_k * P = 0, which is finite
    # but cannot carry a Pearson residual (0/0). With an explicit psi > 0 the
    # conjugate level used to come back NaN; the m0 > 0 filter must drop those
    # cells so the level (and loss_proj) stay finite.
    from datetime import date

    import polars as pl

    rows = []
    for i in range(8):
        for d in range(1, 9 - i):
            loss = 0.0 if d == 1 else 5.0   # duration 1 has zero incremental loss
            rows.append({
                "uy_m": date(2020 + i // 12, 1 + i % 12, 1),
                "cy_m": date(2020 + (i + d - 1) // 12, 1 + (i + d - 1) % 12, 1),
                "incr_loss": loss, "incr_premium": 100.0,
            })
    fit = lr.CredibleLoss(psi=0.5).fit(lr.Triangle(pl.DataFrame(rows)))
    proj = fit.to_polars()["loss_proj"]
    assert proj.is_nan().sum() == 0
