"""Tests for ``RatioFit.at_grain`` (compute-once, display-aggregate)."""

import polars as pl
import pytest

import lossratio as lr


def _m_fit():
    """An M-grain Ratio fit on a single small segment of the experience data."""
    exp = lr.load_experience()
    exp = pl.from_pandas(exp) if not isinstance(exp, pl.DataFrame) else exp
    sub = exp.filter(
        (pl.col("coverage") == "CAN")
        & (pl.col("age_band") == "40s")
        & (pl.col("channel") == "TM")
    )
    tri = lr.Triangle(
        sub,
        cohort="uy_m",
        calendar="cy_m",
        loss="incr_loss",
        premium="incr_premium",
        grain="M",
    )
    return lr.Ratio(method="ed").fit(tri)


def _portfolio_ultimate(df) -> tuple[float, float, float]:
    """Sum each cohort's last-dev (ultimate) loss / premium across the book."""
    df = pl.from_pandas(df) if not isinstance(df, pl.DataFrame) else df
    last = df.sort("dev").group_by("cohort").last()
    loss = last["loss_proj"].sum()
    premium = last["premium_proj"].sum()
    return loss, premium, loss / premium


def test_at_grain_portfolio_ultimate_is_grain_invariant():
    """Coarsening to Q must not change the portfolio ultimate ratio.

    M-grain and Q-grain DISPLAYS are the same forecast binned differently:
    the total Sigma(loss ultimate) / Sigma(premium ultimate) is preserved.
    """
    fit = _m_fit()
    m_loss, m_prem, m_ratio = _portfolio_ultimate(fit.df)
    q_loss, q_prem, q_ratio = _portfolio_ultimate(fit.at_grain("Q"))

    assert abs(m_ratio - q_ratio) < 1e-9
    assert abs(m_loss - q_loss) < 1e-3
    assert abs(m_prem - q_prem) < 1e-3


def test_at_grain_same_grain_returns_unchanged():
    """``at_grain`` with the source grain returns the fit frame unchanged."""
    fit = _m_fit()
    same = fit.at_grain("M")
    same = pl.from_pandas(same) if not isinstance(same, pl.DataFrame) else same
    assert same.shape == fit.df.shape
    assert same["loss_proj"].sum() == pytest.approx(fit.df["loss_proj"].sum())


def test_at_grain_refine_raises():
    """A coarser display cannot be un-aggregated to a finer grain."""
    with pytest.raises(ValueError, match="coarsen only"):
        _refine()


def _refine():
    # Build a Q-grain fit, then ask for the finer M grain -> must raise.
    exp = lr.load_experience()
    exp = pl.from_pandas(exp) if not isinstance(exp, pl.DataFrame) else exp
    sub = exp.filter(
        (pl.col("coverage") == "CAN")
        & (pl.col("age_band") == "40s")
        & (pl.col("channel") == "TM")
    )
    tri_q = lr.Triangle(
        sub,
        cohort="uy_m",
        calendar="cy_m",
        loss="incr_loss",
        premium="incr_premium",
        grain="Q",
    )
    fit_q = lr.Ratio(method="ed").fit(tri_q)
    return fit_q.at_grain("M")


def test_at_grain_unknown_grain_raises():
    fit = _m_fit()
    with pytest.raises(ValueError, match="unknown grain"):
        fit.at_grain("Z")
