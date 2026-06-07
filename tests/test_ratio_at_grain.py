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


def _regime_m_fit():
    """An M-grain REGIME (segment_borrowed) Ratio fit on the SUR book.

    On the regime path the stored ``incr_loss_proj`` column is NULL at
    dev 1 for every cohort, so summing that column would drop the dev-1
    mass. ``at_grain`` derives increments from the (complete) cumulative
    ``loss_proj`` / ``premium_proj`` instead.
    """
    exp = lr.load_experience()
    exp = pl.from_pandas(exp) if not isinstance(exp, pl.DataFrame) else exp
    sub = exp.filter(pl.col("coverage") == "SUR")
    tri = lr.Triangle(
        sub,
        cohort="uy_m",
        calendar="cy_m",
        loss="incr_loss",
        premium="incr_premium",
        grain="M",
    )
    return lr.Ratio(
        method="ed", loss_regime=lr.Regime.at(change="2024-07-01")
    ).fit(tri)


def test_at_grain_regime_portfolio_ultimate_is_grain_invariant():
    """Regime fits: coarsening to Q must preserve the portfolio ultimate.

    Regression for the dev-1 mass drop: the regime path leaves
    ``incr_loss_proj`` NULL at dev 1, so re-binning from that column was
    short by the first increment (~0.56% on this book). Deriving the
    increments from the complete cumulative projection makes Q EXACT.
    """
    fit = _regime_m_fit()
    summ = fit.summary()
    summ = pl.from_pandas(summ) if not isinstance(summ, pl.DataFrame) else summ
    m_loss = summ["loss_proj"].sum()
    m_prem = summ["premium_proj"].sum()
    q_loss, q_prem, _ = _portfolio_ultimate(fit.at_grain("Q"))

    assert abs(m_loss - q_loss) < 1e-9 * abs(m_loss)
    assert abs(m_prem - q_prem) < 1e-9 * abs(m_prem)
    assert abs(m_loss / m_prem - q_loss / q_prem) < 1e-9


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


def _m_fit_with_tail():
    """An M-grain Ratio fit on a single segment WITH an exponential tail."""
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
    return lr.Ratio(method="ed", tail=lr.Tail(family="exponential")).fit(tri)


def _fine_tail_inclusive_ultimate(df) -> tuple[float, float, float]:
    """Sum each cohort's tail-inclusive ultimate (loss_tail or loss_proj)."""
    df = pl.from_pandas(df) if not isinstance(df, pl.DataFrame) else df
    last = df.sort("dev").group_by("cohort").last()
    last = last.with_columns(
        pl.coalesce("loss_tail", "loss_proj").alias("_u_loss"),
        pl.coalesce("premium_tail", "premium_proj").alias("_u_prem"),
    )
    loss = last["_u_loss"].sum()
    prem = last["_u_prem"].sum()
    return loss, prem, loss / prem


def test_at_grain_with_tail_portfolio_ultimate_is_grain_invariant():
    """With a tail active, the coarse ultimate must FOLD the tail mass.

    The M-grain tail-inclusive ultimate (loss_tail-or-loss_proj at the
    last dev) must equal the Q-grain ``at_grain`` last-dev ``loss_proj``
    (which now folds the tail). This is the M=Q-with-tail guarantee --
    without the fold, ``at_grain`` silently dropped the tail mass.
    """
    fit = _m_fit_with_tail()
    assert "loss_tail" in (
        fit.df if isinstance(fit.df, pl.DataFrame) else pl.from_pandas(fit.df)
    ).columns

    m_loss, m_prem, m_ratio = _fine_tail_inclusive_ultimate(fit.df)
    # the tail genuinely adds mass (guards against a no-op false pass)
    assert m_loss > 0.0

    q = fit.at_grain("Q")
    q_loss, q_prem, q_ratio = _portfolio_ultimate(q)

    assert abs(m_loss - q_loss) < 1e-9 * max(1.0, abs(m_loss))
    assert abs(m_prem - q_prem) < 1e-9 * max(1.0, abs(m_prem))
    assert abs(m_ratio - q_ratio) < 1e-9


def test_at_grain_tail_off_unchanged():
    """Tail-off ``at_grain`` is unaffected by the tail-fold code path."""
    fit = _m_fit()  # no tail
    df = fit.df if isinstance(fit.df, pl.DataFrame) else pl.from_pandas(fit.df)
    assert "loss_tail" not in df.columns
    q = fit.at_grain("Q")
    q_loss, q_prem, _ = _portfolio_ultimate(q)
    # tail-blind sum of within-triangle projections still matches the
    # fine within-triangle ultimate (no tail mass to add either side)
    m_loss, m_prem, _ = _portfolio_ultimate(fit.df)
    assert abs(m_loss - q_loss) < 1e-3
    assert abs(m_prem - q_prem) < 1e-3


def _summary_portfolio_ratio(summary_df) -> tuple[float, float, float]:
    """Sum ``summary()``'s per-cohort ultimate loss / premium across the book."""
    s = (
        summary_df
        if isinstance(summary_df, pl.DataFrame)
        else pl.from_pandas(summary_df)
    )
    loss = float(s["loss_proj"].sum())
    prem = float(s["premium_proj"].sum())
    return loss, prem, loss / prem


def test_summary_tail_inclusive_ultimate():
    """``summary()``'s per-cohort ultimate must FOLD the tail when active.

    Each cohort's reported ``loss_proj`` / ``premium_proj`` becomes the
    tail-inclusive ultimate (``*_tail`` on the last-dev row), so the
    headline grows when a tail is turned on. The tail-off summary stays
    the within-triangle ultimate.
    """
    off = _m_fit().summary()
    on = _m_fit_with_tail().summary()
    off = off if isinstance(off, pl.DataFrame) else pl.from_pandas(off)
    on = on if isinstance(on, pl.DataFrame) else pl.from_pandas(on)

    # No `*_tail` columns leak into the summary schema either way.
    assert not [c for c in off.columns if c.endswith("_tail")]
    assert not [c for c in on.columns if c.endswith("_tail")]

    j = off.select(["cohort", "loss_proj"]).rename(
        {"loss_proj": "loss_off"}
    ).join(
        on.select(["cohort", "loss_proj"]).rename({"loss_proj": "loss_on"}),
        on="cohort",
    )
    # tail-inclusive ultimate is never below the within-triangle one, and
    # strictly above for at least one cohort (the tail adds real mass).
    assert (j["loss_on"] >= j["loss_off"] - 1e-6).all()
    assert (j["loss_on"] > j["loss_off"] + 1e-3).any()


def test_summary_tail_matches_at_grain_q():
    """The cascade is M=Q-with-tail: ``summary()``'s portfolio ultimate
    ratio (M) equals ``at_grain('Q')``'s portfolio ultimate ratio (Q).

    ``summary()`` now folds the tail; ``at_grain('Q')`` already does. Both
    must land on the same tail-inclusive headline.
    """
    fit = _m_fit_with_tail()
    m_loss, m_prem, m_ratio = _summary_portfolio_ratio(fit.summary())
    q_loss, q_prem, q_ratio = _portfolio_ultimate(fit.at_grain("Q"))

    assert abs(m_loss - q_loss) < 1e-9 * max(1.0, abs(m_loss))
    assert abs(m_prem - q_prem) < 1e-9 * max(1.0, abs(m_prem))
    assert abs(m_ratio - q_ratio) < 1e-9


def test_summary_tail_off_byte_identical():
    """Tail-off ``summary()`` is unchanged by the tail-fold code path.

    Equals the within-triangle ultimate folded straight from ``fit.df``
    (the pre-fix behaviour).
    """
    fit = _m_fit()  # no tail
    s_loss, s_prem, s_ratio = _summary_portfolio_ratio(fit.summary())
    d_loss, d_prem, d_ratio = _portfolio_ultimate(fit.df)
    assert s_loss == pytest.approx(d_loss)
    assert s_prem == pytest.approx(d_prem)
    assert s_ratio == pytest.approx(d_ratio)
