"""Structural invariants the projection must satisfy.

These lock behaviour the engine must PRESERVE -- regression
guards on the current golden implementation, written against the public
API only.

Covered here:
* **unit invariance** (raison d'etre): scaling loss AND premium by
  a common currency factor scales every loss/premium projection by that
  factor while leaving the dimensionless loss ratio and CV unchanged.
* **loss equivariance** (link-ratio): scaling loss alone scales the loss
  projection by the same factor and leaves the CV unchanged (f_k is a
  ratio of losses, scale-free).

Not yet covered (need fit/backtest-internal contracts): the balance property on
the real fit (per-duration
Sum fitted == Sum observed) and the leakage sentinel (mask a held-out cell
with an extreme value, refit, assert byte-identical).
"""

from __future__ import annotations

import numpy as np
import polars as pl

import lossratio as lr

C = 1000.0
KEYS = ["coverage", "cohort", "duration"]


def _experience() -> pl.DataFrame:
    df = lr.load_experience()
    return df if isinstance(df, pl.DataFrame) else pl.from_pandas(df)


def _triangle(df: pl.DataFrame) -> lr.Triangle:
    return lr.Triangle(df, groups="coverage", cohort="uy_m", calendar="cy_m",
                       loss="incr_loss", premium="incr_premium", grain="M")


def _fit_df(estimator, df: pl.DataFrame) -> pl.DataFrame:
    out = estimator.fit(_triangle(df))
    return out.to_polars().sort(KEYS)


def _ratio_df(loss_estimator, df: pl.DataFrame) -> pl.DataFrame:
    """RatioFit frame -- the loss ratio lives only on the composed Ratio, so the
    ratio/premium invariance is checked here (the bare LossFit is loss-only)."""
    out = lr.Ratio(loss=loss_estimator, premium=lr.PooledPremium()).fit(_triangle(df))
    return out.to_polars().sort(KEYS)


def _close(a: np.ndarray, b: np.ndarray, rtol: float = 1e-9) -> None:
    m = np.isfinite(a) & np.isfinite(b)
    assert m.any()
    np.testing.assert_allclose(a[m], b[m], rtol=rtol)


def test_pooled_loss_unit_invariance():
    """PooledLoss: scale loss AND premium by C -> projections scale by C, the
    loss ratio and CV are invariant."""
    df = _experience()
    base = _fit_df(lr.PooledLoss(), df)
    scaled = df.with_columns(
        incr_loss=pl.col("incr_loss") * C,
        incr_premium=pl.col("incr_premium") * C,
    )
    sc = _fit_df(lr.PooledLoss(), scaled)
    assert base.height == sc.height

    _close(sc["loss_proj"].to_numpy(), C * base["loss_proj"].to_numpy())
    # dimensionless quantity unchanged by the currency unit
    _close(sc["loss_total_cv"].to_numpy(), base["loss_total_cv"].to_numpy())
    # the loss ratio is a composed quantity -> read it off the Ratio: premium
    # scales by C while the dimensionless ratio is unit-invariant.
    ratio_base = _ratio_df(lr.PooledLoss(), df)
    ratio_sc = _ratio_df(lr.PooledLoss(), scaled)
    _close(ratio_sc["premium_proj"].to_numpy(), C * ratio_base["premium_proj"].to_numpy())
    _close(ratio_sc["ratio_proj"].to_numpy(), ratio_base["ratio_proj"].to_numpy())


def test_chain_ladder_loss_equivariance():
    """ChainLadder: scale loss alone by C -> loss projection scales by C, CV
    invariant (f_k is a ratio of losses, scale-free)."""
    df = _experience()
    base = _fit_df(lr.ChainLadder(), df)
    scaled = df.with_columns(incr_loss=pl.col("incr_loss") * C)
    sc = _fit_df(lr.ChainLadder(), scaled)
    assert base.height == sc.height

    _close(sc["loss_proj"].to_numpy(), C * base["loss_proj"].to_numpy())
    _close(sc["loss_total_cv"].to_numpy(), base["loss_total_cv"].to_numpy())


def test_row_order_invariance():
    """Shuffling input rows does not change the fitted projection (a fit is a
    function of the cells, not their order)."""
    df = _experience()
    base = _fit_df(lr.PooledLoss(), df)
    shuffled = df.sample(fraction=1.0, shuffle=True, seed=20260614)
    sc = _fit_df(lr.PooledLoss(), shuffled)
    _close(sc["loss_proj"].to_numpy(), base["loss_proj"].to_numpy())
