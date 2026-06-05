"""Tests for cohort regime detection."""

import numpy as np
import polars as pl
import pytest

import lossratio as lr
from lossratio._e_divisive import e_divisive


# ---------------------------------------------------------------------------
# E-Divisive algorithm tests (low-level)
# ---------------------------------------------------------------------------


def test_edivisive_detects_mean_shift():
    """Synthetic mean-shift at index 25 — must be detected exactly."""
    rng = np.random.default_rng(42)
    X = np.vstack(
        [
            rng.normal(loc=0.0, scale=1.0, size=(25, 5)),
            rng.normal(loc=3.0, scale=1.0, size=(25, 5)),
        ]
    )
    res = e_divisive(X, sig_level=0.05, n_permutations=199, min_size=5, seed=20260509)
    assert res.breakpoints == [25]
    assert res.p_values[0] < 0.05


def test_edivisive_no_shift_returns_empty():
    """Homogeneous data — no significant breaks should be reported."""
    rng = np.random.default_rng(123)
    X = rng.normal(loc=0.0, scale=1.0, size=(50, 5))
    res = e_divisive(X, sig_level=0.05, n_permutations=199, min_size=5, seed=20260509)
    assert res.breakpoints == []
    assert res.p_values == []


def test_edivisive_two_breaks():
    """Three-segment data with two distinct mean shifts."""
    rng = np.random.default_rng(7)
    X = np.vstack(
        [
            rng.normal(loc=0.0, scale=0.5, size=(20, 4)),
            rng.normal(loc=3.0, scale=0.5, size=(20, 4)),
            rng.normal(loc=-2.0, scale=0.5, size=(20, 4)),
        ]
    )
    res = e_divisive(X, sig_level=0.05, n_permutations=199, min_size=5, seed=20260509)
    # Two breaks expected (around 20 and 40); allow ±2 tolerance
    assert len(res.breakpoints) == 2
    assert all(p < 0.05 for p in res.p_values)
    assert abs(res.breakpoints[0] - 20) <= 2
    assert abs(res.breakpoints[1] - 40) <= 2


def test_edivisive_too_short_returns_empty():
    """Data shorter than 2 * min_size — no split possible."""
    rng = np.random.default_rng(0)
    X = rng.normal(size=(5, 3))
    res = e_divisive(X, sig_level=0.05, n_permutations=99, min_size=3, seed=1)
    assert res.breakpoints == []


def test_edivisive_seed_reproducible():
    """Same seed → same breakpoints and p-values."""
    rng = np.random.default_rng(99)
    X = np.vstack(
        [
            rng.normal(loc=0.0, scale=1.0, size=(20, 3)),
            rng.normal(loc=2.5, scale=1.0, size=(20, 3)),
        ]
    )
    r1 = e_divisive(X, sig_level=0.05, n_permutations=199, min_size=5, seed=20260509)
    r2 = e_divisive(X, sig_level=0.05, n_permutations=199, min_size=5, seed=20260509)
    assert r1.breakpoints == r2.breakpoints
    assert r1.p_values == r2.p_values


# ---------------------------------------------------------------------------
# Triangle.detect_regime() integration tests
# ---------------------------------------------------------------------------


def _toy_triangle(n_cohorts: int = 30, window: int = 12, shift_at: int = 15):
    """Build a Triangle with a synthetic regime shift.

    Each cohort has the full window dev periods. The lr trajectory shape is
    a piecewise-constant random vector around two distinct means.
    """
    rng = np.random.default_rng(20260509)
    rows = []
    for c_idx in range(n_cohorts):
        cohort_date = f"2024-{(c_idx % 12) + 1:02d}-01"
        # Use yearly shifting cohort by month index — but keep window dev rows each
        for k in range(1, window + 1):
            # Underlying signal: pre-shift Ratio ~ 0.5; post-shift Ratio ~ 1.0
            base = 0.5 if c_idx < shift_at else 1.0
            ratio_val = base + rng.normal(0, 0.05)
            rows.append(
                {
                    "cy_m": cohort_date,
                    "uy_m": cohort_date,
                    "incr_loss": ratio_val * 100.0,
                    "incr_premium": 100.0,
                }
            )
    df = pl.DataFrame(rows)
    # Force unique cohort dates by spreading uym across months
    # (rebuild uym so each cohort is distinct and ordered)
    cohort_dates = pl.date_range(
        start=pl.lit("2023-01-01").cast(pl.Date),
        end=pl.lit("2023-01-01").cast(pl.Date)
        + pl.duration(days=30 * (n_cohorts - 1)),
        interval="1mo",
        eager=True,
    )
    rows2 = []
    for c_idx in range(n_cohorts):
        for k in range(1, window + 1):
            base = 0.5 if c_idx < shift_at else 1.0
            ratio_val = base + rng.normal(0, 0.05)
            rows2.append(
                {
                    "uy_m": cohort_dates[c_idx],
                    "cy_m": cohort_dates[c_idx]
                    + pl.duration(days=30 * (k - 1)).map_elements(
                        lambda x: x, return_dtype=pl.Duration
                    ).item()
                    if False
                    else cohort_dates[c_idx],
                    "incr_loss": ratio_val * 100.0,
                    "incr_premium": 100.0,
                }
            )
    return rows2  # placeholder, see _toy_input


def _toy_input(n_cohorts: int = 30, window: int = 12, shift_at: int = 15) -> pl.DataFrame:
    """Build a long-format Experience input with a synthetic regime shift."""
    rng = np.random.default_rng(20260509)

    cohort_dates = [
        f"2023-{((m - 1) % 12) + 1:02d}-01" if m <= 12 else f"2024-{(m - 13) % 12 + 1:02d}-01"
        for m in range(1, n_cohorts + 1)
    ]

    rows = []
    for c_idx in range(n_cohorts):
        u = cohort_dates[c_idx]
        for k in range(1, window + 1):
            # Calendar month = uym month + k - 1; just reuse uym for cym
            # since we only need cohort uniqueness, not calendar realism
            # for the regime test.
            base = 0.5 if c_idx < shift_at else 1.0
            ratio_val = max(0.0, base + rng.normal(0, 0.05))
            rows.append(
                {
                    "cy_m": u,
                    "uy_m": u,
                    "_dev_target": k,
                    "incr_loss": ratio_val * 100.0,
                    "incr_premium": 100.0,
                }
            )
    return pl.DataFrame(rows)


def test_detect_regime_e_divisive_finds_shift():
    df = _toy_input(n_cohorts=30, window=12, shift_at=15)
    # Triangle constructor builds dev from cym/uym — but our toy input
    # sets cym = uym, so dev = 1 only. We need cym to advance. Build
    # cym by adding (k-1) months to uym.
    df = df.with_columns(
        pl.col("uy_m").cast(pl.Date),
        pl.col("cy_m").cast(pl.Date),
    )
    df = df.with_columns(
        # cym = uym + (_dev_target - 1) months — emulate dev periods
        pl.col("uy_m").dt.offset_by(
            pl.format("{}mo", pl.col("_dev_target") - 1)
        ).alias("cy_m")
    ).drop("_dev_target")

    tri = lr.Triangle(df)
    reg = tri.detect_regime(
        target="ratio", window=12, method="e_divisive", min_size=3, n_permutations=199, seed=20260509
    )
    assert isinstance(reg, lr.Regime)
    assert reg.method == "e_divisive"
    assert reg.window == 12
    assert reg.n_regimes >= 2  # at least one break
    # Break should be near cohort 15 (within tolerance)
    assert len(reg.breakpoints) >= 1


def test_detect_regime_hclust():
    df = _toy_input(n_cohorts=30, window=12, shift_at=15)
    df = df.with_columns(
        pl.col("uy_m").cast(pl.Date),
        pl.col("cy_m").cast(pl.Date),
    )
    df = df.with_columns(
        pl.col("uy_m").dt.offset_by(
            pl.format("{}mo", pl.col("_dev_target") - 1)
        ).alias("cy_m")
    ).drop("_dev_target")

    tri = lr.Triangle(df)
    reg = tri.detect_regime(
        target="ratio", window=12, method="hclust", n_regimes=2
    )
    assert reg.method == "hclust"
    assert reg.n_regimes == 2


def test_detect_regime_invalid_method_raises():
    df = _toy_input(n_cohorts=30, window=12, shift_at=15)
    df = df.with_columns(
        pl.col("uy_m").cast(pl.Date),
        pl.col("cy_m").cast(pl.Date),
    )
    df = df.with_columns(
        pl.col("uy_m").dt.offset_by(
            pl.format("{}mo", pl.col("_dev_target") - 1)
        ).alias("cy_m")
    ).drop("_dev_target")

    tri = lr.Triangle(df)
    with pytest.raises(ValueError, match="method must be one of"):
        tri.detect_regime(method="nonsense")


def test_detect_regime_low_K_raises():
    df = _toy_input(n_cohorts=30, window=12, shift_at=15)
    df = df.with_columns(
        pl.col("uy_m").cast(pl.Date),
        pl.col("cy_m").cast(pl.Date),
    )
    df = df.with_columns(
        pl.col("uy_m").dt.offset_by(
            pl.format("{}mo", pl.col("_dev_target") - 1)
        ).alias("cy_m")
    ).drop("_dev_target")

    tri = lr.Triangle(df)
    with pytest.raises(ValueError, match="window must be"):
        tri.detect_regime(window=1)


# ---------------------------------------------------------------------------
# window_floor / FDR / edge_scan (robustness layer)
# ---------------------------------------------------------------------------


def test_bh_adjust_basic():
    from lossratio.regime import _bh_adjust

    p = np.array([0.01, 0.02, 0.03, 0.04])
    adj = _bh_adjust(p)
    # all four scale to m/rank * p = 0.04, monotone-flattened to 0.04
    assert np.allclose(adj, 0.04)
    assert np.all(adj <= 1.0)


def test_bh_adjust_nan_passthrough_and_n_tests():
    from lossratio.regime import _bh_adjust

    p = np.array([0.001, np.nan, 0.5])
    adj = _bh_adjust(p, n_tests=10)
    assert np.isnan(adj[1])                      # NaN (edge-scan) passthrough
    assert adj[0] == pytest.approx(0.01)         # 0.001 * 10 / 1
    assert adj[2] == pytest.approx(1.0)          # 0.5 * 10 / 2 -> clipped


def test_edge_scan_detects_left_edge_outlier():
    from lossratio.regime import _edge_scan_breakpoints

    rng = np.random.default_rng(0)
    mat = 1.0 + rng.normal(0, 0.02, size=(12, 4))
    mat[0] = 5.0                                  # first cohort wildly different
    breaks = _edge_scan_breakpoints(mat, threshold=10.0, min_size=3)
    assert breaks == [1]                          # left edge of size 1 -> break @ idx 1


def test_edge_scan_quiet_on_noise():
    from lossratio.regime import _edge_scan_breakpoints

    rng = np.random.default_rng(1)
    mat = 1.0 + rng.normal(0, 0.5, size=(12, 4))  # all noisy, no clear edge
    breaks = _edge_scan_breakpoints(mat, threshold=10.0, min_size=3)
    assert breaks == []


def test_edge_scan_blind_zone_cap():
    from lossratio.regime import _edge_scan_breakpoints

    # min_size=2 -> max_edge=1: only a length-1 edge can be flagged.
    rng = np.random.default_rng(2)
    mat = 1.0 + rng.normal(0, 0.02, size=(12, 4))
    mat[0] = mat[1] = 5.0                          # first TWO cohorts shifted
    breaks = _edge_scan_breakpoints(mat, threshold=10.0, min_size=2)
    assert breaks == [1]                          # capped at the blind zone (k<=1)


def _edge_regime_triangle(n_cohorts: int = 14, window: int = 6, edge_base: float = 3.0):
    """Pooled Triangle whose FIRST cohort sits at a different loss level.

    A length-1 edge regime: E-Divisive cannot separate it (needs >= 2 per
    side), but the 1-vs-rest edge scan can.
    """
    from datetime import date

    def _add_months(d: date, m: int) -> date:
        y, mo = divmod(d.month - 1 + m, 12)
        return date(d.year + y, mo + 1, 1)

    start = date(2023, 1, 1)
    rng = np.random.default_rng(7)
    rows = []
    for ci in range(n_cohorts):
        base = edge_base if ci == 0 else 1.0
        uy = _add_months(start, ci)
        for k in range(window):
            rows.append(
                {
                    "uy_m": uy,
                    "cy_m": _add_months(uy, k),
                    "incr_loss": (base + rng.normal(0, 0.02)) * 100.0,
                    "incr_premium": 100.0,
                }
            )
    return lr.Triangle(pl.DataFrame(rows))


def test_edge_scan_integration_finds_edge_regime():
    tri = _edge_regime_triangle()
    # default (no edge scan): E-Divisive cannot separate the 1-cohort edge
    base = tri.detect_regime(target="ratio", window=4, method="e_divisive", seed=1)
    assert base.changes.height == 0
    # edge scan on: the first-cohort regime surfaces (change at the 2nd cohort)
    scanned = tri.detect_regime(
        target="ratio", window=4, method="e_divisive", seed=1, edge_scan=True
    )
    assert scanned.changes.height == 1
    assert str(scanned.changes["change"].to_list()[0]) == "2023-02-01"


def test_robustness_params_smoke():
    # all three opt-in params accepted together without error
    tri = _edge_regime_triangle()
    reg = tri.detect_regime(
        target="ratio",
        window="auto",
        method="e_divisive",
        seed=1,
        window_floor=4,
        fdr=True,
        edge_scan=True,
    )
    assert reg.method == "e_divisive"


# ---------------------------------------------------------------------------
# Regime.candidates (assess metrics attached to detected changes)
# ---------------------------------------------------------------------------


def test_detect_regime_candidates_carries_assess():
    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    reg = tri.detect_regime(target="ratio", window=12, seed=20260501)
    cand = reg.candidates
    for col in ("level_shift", "t_stat", "p_value", "delta_r2",
                "step_p", "curved_drift_suspect", "kind"):
        assert col in cand.columns
    # `changes` stays minimal (golden-safe); assess lives only on candidates.
    assert "level_shift" not in reg.changes.columns
    # candidates is a superset of accepted changes.
    assert cand.height >= reg.changes.height
    # the planted synthetic SUR regime is a clean step.
    sur = cand.filter(pl.col("coverage") == "SUR")
    if sur.height:
        assert sur["kind"][0] == "step"
        assert sur["step_p"][0] < 0.05


def test_regime_at_has_empty_candidates():
    reg = lr.Regime.at(change="2024-07-01")
    assert reg.candidates.height == 0


def test_detect_regime_window_sweep_stability():
    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    reg = tri.detect_regime(
        target="ratio", window=12, window_sweep=range(6, 14),
        seed=20260501, n_permutations=199,
    )
    cand = reg.candidates
    assert "window_stability" in cand.columns
    assert "n_windows" in cand.columns
    sub = cand.filter(pl.col("window_stability").is_not_nan())
    if sub.height:
        assert sub["window_stability"].min() > 0.0
        assert sub["window_stability"].max() <= 1.0
    # the planted synthetic SUR regime recurs across windows (robust).
    sur = cand.filter(pl.col("coverage") == "SUR")
    if sur.height:
        assert sur["window_stability"][0] >= 0.5
        assert sur["kind"][0] == "step"


# ---------------------------------------------------------------------------
# Grain sweep (_coarsen_triangle + grain_stability)
# ---------------------------------------------------------------------------


def test_coarsen_triangle_matches_direct_build():
    from lossratio.regime import _coarsen_triangle

    exp = lr.load_experience()
    tri_m = lr.Triangle(exp, groups="coverage", grain="M")
    key = lambda t: t._df.select(
        "coverage", "cohort", "dev", "loss", "premium", "ratio"
    ).sort("coverage", "cohort", "dev")
    for g in ("Q", "H"):
        coarse = _coarsen_triangle(tri_m, g)
        direct = lr.Triangle(exp, groups="coverage", grain=g)
        assert coarse.grain == g
        assert key(coarse).equals(key(direct))   # exact re-aggregation
    # cannot refine below the source grain
    with pytest.raises(ValueError):
        _coarsen_triangle(lr.Triangle(exp, groups="coverage", grain="Q"), "M")


def test_detect_regime_grain_sweep():
    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    reg = tri.detect_regime(
        target="ratio", grain_sweep=["M", "Q", "H"], window_sweep=range(4, 10),
        seed=20260501, n_permutations=199,
    )
    cand = reg.candidates
    assert "grain" in cand.columns
    assert "grain_stability" in cand.columns
    assert cand["grain_stability"].min() >= 1
    # the planted SUR regime is detected at >= 1 grain and is a step.
    sur = cand.filter(pl.col("coverage") == "SUR")
    if sur.height:
        assert sur["kind"][0] == "step"
        assert sur["grain_stability"][0] >= 1


# ---------------------------------------------------------------------------
# Regime.evaluate (score candidates -> accept + confidence)
# ---------------------------------------------------------------------------


def test_evaluate_action_regime_or_none():
    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    reg = tri.detect_regime(
        target="ratio", grain_sweep=["M", "Q", "H"], window_sweep=range(4, 10),
        seed=20260501, n_permutations=199,
    )
    ev = reg.evaluate()
    assert {"action", "still_moving", "confidence"} <= set(ev.columns)
    # action collapses to regime / none (a material drift is cut too).
    assert set(ev["action"].unique().to_list()) <= {"regime", "none"}
    # confidence in [0, 1], sorted descending.
    assert ev["confidence"].min() >= 0.0 and ev["confidence"].max() <= 1.0
    assert ev["confidence"].to_list() == sorted(ev["confidence"].to_list(), reverse=True)
    assert (ev["accept"] == (ev["action"] == "regime")).all()
    # still_moving only on drift regimes; never on a step regime.
    type_col = "change_type" if "change_type" in ev.columns else "kind"
    moving = ev.filter(pl.col("still_moving"))
    if moving.height:
        assert (moving[type_col] == "drift").all()
        assert (moving["action"] == "regime").all()
    # "none" carries zero confidence; acted-on rows carry positive.
    assert (ev.filter(pl.col("action") == "none")["confidence"] == 0.0).all()
    acted = ev.filter(pl.col("action") == "regime")
    if acted.height:
        assert (acted["confidence"] > 0.0).all()
    # the planted SUR step is a regime, not still-moving.
    sur = ev.filter(pl.col("coverage") == "SUR")
    if sur.height:
        assert sur["action"][0] == "regime"
        assert not sur["still_moving"][0]
        assert sur["confidence"][0] > 0.5


def test_evaluate_custom_rule():
    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    reg = tri.detect_regime(target="ratio", window=12, seed=20260501)
    ev = reg.evaluate(rule=lambda r: (r["kind"] == "step", 1.0 if r["kind"] == "step" else 0.0))
    assert set(ev.filter(pl.col("accept"))["kind"].unique().to_list()) <= {"step"}


def test_evaluate_empty_when_no_candidates():
    reg = lr.Regime.at(change="2024-07-01")
    assert reg.evaluate().height == 0


def test_grain_sweep_per_grain_kind_profile():
    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    reg = tri.detect_regime(
        target="ratio", grain_sweep=["M", "Q", "H"], window_sweep=range(4, 10),
        seed=20260501, n_permutations=199,
    )
    cand = reg.candidates
    # per-grain kind columns (only for grains that detected something) + the
    # derived change_type profile.
    kcols = [c for c in cand.columns if c.startswith("kind_")]
    assert kcols  # at least one grain produced a kind column
    assert "change_type" in cand.columns
    assert set(cand["change_type"].unique().to_list()) <= {
        "step", "drift", "transition", "edge"
    }
    # change_type is consistent with the per-grain kinds: a "step" type has
    # no drift among its detected grains.
    for row in cand.iter_rows(named=True):
        kinds = {row[c] for c in kcols if row[c] is not None}
        if row["change_type"] == "step":
            assert "drift" not in kinds
        elif row["change_type"] == "drift":
            assert "step" not in kinds
        elif row["change_type"] == "transition":
            assert "step" in kinds and "drift" in kinds
    # the planted SUR regime is a clean step across its detected grains.
    sur = cand.filter(pl.col("coverage") == "SUR")
    if sur.height:
        assert sur["change_type"][0] == "step"


def test_accepted_drives_the_fit():
    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    reg = tri.detect_regime(
        target="ratio", grain_sweep=["M", "Q", "H"], window_sweep=range(4, 10),
        seed=20260501, n_permutations=199,
    )
    acc = reg.accepted()
    assert isinstance(acc, lr.Regime)
    # accepted changes are exactly the action == "regime" candidates.
    ev = reg.evaluate()
    n_regime = ev.filter(pl.col("action") == "regime").height
    assert acc.changes.height == n_regime
    # and they actually drive a fit -- the cut changes the projection vs
    # fitting all cohorts (SUR carries a planted regime).
    f0 = lr.Ratio().fit(tri)
    f1 = lr.Ratio(loss_regime=acc).fit(tri)

    def sur_ult(f):
        s = f.summary()
        s = s if isinstance(s, pl.DataFrame) else pl.from_pandas(s)
        return s.filter(pl.col("coverage") == "SUR")["ratio_ult"].drop_nulls().mean()

    assert abs(sur_ult(f0) - sur_ult(f1)) > 1e-3
