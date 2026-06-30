"""Tests for the EstimatorComparison meta-estimator (the OOS switch read)."""

import copy

import polars as pl
import pytest

import lossratio as lr
from lossratio.diagnostics.comparison import EstimatorComparisonFit


# ---------------------------------------------------------------------------
# Fixtures / helpers
# ---------------------------------------------------------------------------


def _triangle(groups=None) -> lr.Triangle:
    df = lr.load_experience()
    return lr.Triangle(df, groups=groups)


@pytest.fixture(scope="module")
def cmp_fit():
    """Two genuinely different estimators on the ungrouped triangle."""
    return lr.EstimatorComparison(
        {"link_ratio": lr.ChainLadder(), "pooled": lr.PooledLoss()},
        holdouts=(4, 8), target="loss",
    ).fit(_triangle())


@pytest.fixture(scope="module")
def self_cmp():
    """The SAME estimator under two labels: every cell is an exact tie."""
    est = lr.ChainLadder()
    return lr.EstimatorComparison(
        {"a": est, "b": est}, holdouts=(4, 8), target="loss"
    ).fit(_triangle())


@pytest.fixture(scope="module")
def grouped_cmp():
    return lr.EstimatorComparison(
        {"link_ratio": lr.ChainLadder(), "pooled": lr.PooledLoss()},
        holdouts=(4, 8), target="loss",
    ).fit(_triangle(groups="coverage"))


class _SkipSecondFit:
    """Estimator whose 2nd fold raises the package's degenerate-fold signal
    (ValueError), so its RollingBacktest skips the 2nd hold-out depth."""

    def __init__(self):
        self._inner = lr.ChainLadder()
        self.calls = 0

    def fit(self, triangle):
        self.calls += 1
        if self.calls == 2:
            raise ValueError("degenerate fold")
        return self._inner.fit(triangle)


class _NeverFits:
    """Estimator whose every fold raises the degenerate-fold signal."""

    def fit(self, triangle):
        raise ValueError("no anchor")


class _NoIncrFit:
    """Refit-result wrapper that hides the incremental projection."""

    def __init__(self, inner):
        self._inner = inner

    def to_polars(self):
        df = self._inner.to_polars()
        drop = [c for c in df.columns if c.startswith("incr_")]
        return df.drop(drop)


class _NoIncr:
    """Estimator whose refit output carries no incremental projection."""

    def __init__(self):
        self._inner = lr.ChainLadder()

    def fit(self, triangle):
        return _NoIncrFit(self._inner.fit(triangle))


# ---------------------------------------------------------------------------
# Construction / validation
# ---------------------------------------------------------------------------


def test_empty_estimators():
    with pytest.raises(ValueError, match="at least two"):
        lr.EstimatorComparison({})


def test_non_str_label():
    with pytest.raises(TypeError, match="label"):
        lr.EstimatorComparison(
            {1: lr.ChainLadder(), "b": lr.ChainLadder()}, target="loss"
        )


def test_blank_label():
    with pytest.raises(ValueError, match="label"):
        lr.EstimatorComparison(
            {"  ": lr.ChainLadder(), "b": lr.ChainLadder()}, target="loss"
        )


def test_bad_baseline():
    with pytest.raises(ValueError, match="baseline"):
        lr.EstimatorComparison(
            {"a": lr.ChainLadder(), "b": lr.ChainLadder()},
            target="loss", baseline="c",
        )


def test_baseline_defaults_to_first_key():
    cmp = lr.EstimatorComparison(
        {"b": lr.ChainLadder(), "a": lr.ChainLadder()}, target="loss"
    )
    assert cmp.baseline == "b"  # insertion order, not alphabetical


def test_holdouts_normalized_sorted_deduped():
    cmp = lr.EstimatorComparison(
        {"a": lr.ChainLadder(), "b": lr.ChainLadder()},
        holdouts=(12, 6, 6, 18), target="loss",
    )
    assert cmp.holdouts == (6, 12, 18)


def test_propagated_holdout_error():
    with pytest.raises(ValueError, match=">= 1"):
        lr.EstimatorComparison(
            {"a": lr.ChainLadder(), "b": lr.ChainLadder()},
            holdouts=(6, 0), target="loss",
        )


def test_propagated_target_error():
    with pytest.raises(ValueError, match="target"):
        lr.EstimatorComparison(
            {"a": lr.ChainLadder(), "b": lr.ChainLadder()}, target="bogus"
        )


def test_propagated_estimator_error():
    class Dummy:
        pass

    with pytest.raises(TypeError, match="fit"):
        lr.EstimatorComparison(
            {"a": lr.ChainLadder(), "b": Dummy()}, target="loss"
        )


def test_direct_init_raises():
    with pytest.raises(TypeError, match="EstimatorComparison"):
        EstimatorComparisonFit()


# ---------------------------------------------------------------------------
# Matched-cell assembly
# ---------------------------------------------------------------------------


def test_cells_long_layout(cmp_fit):
    cells = cmp_fit.cells
    lead = [
        "estimator", "holdout", "horizon", "anchor_duration",
        "cohort", "duration", "actual", "expected", "aeg", "ae_err",
    ]
    assert cells.columns[: len(lead)] == lead
    assert set(cells["estimator"].unique().to_list()) == {"link_ratio", "pooled"}
    # one row per (matched key x estimator)
    n_keys = cells.select(["holdout", "cohort", "duration"]).n_unique()
    assert cells.height == 2 * n_keys
    # repr's matched-cell count is the key count, not the row count
    assert f"n_matched_cells={n_keys}" in repr(cmp_fit)


def test_matched_keys_identical_across_estimators(cmp_fit):
    # horizon / anchor_duration are key-derived and method-independent, and
    # actual repeats per estimator on a matched key (a matching witness).
    cells = cmp_fit.cells
    per_key = cells.group_by(["holdout", "cohort", "duration"]).agg(
        pl.col("estimator").n_unique().alias("n_est"),
        pl.col("horizon").n_unique().alias("n_horizon"),
        pl.col("anchor_duration").n_unique().alias("n_anchor"),
        pl.col("actual").n_unique().alias("n_actual"),
    )
    assert per_key["n_est"].min() == 2
    assert per_key["n_est"].max() == 2
    assert per_key["n_horizon"].max() == 1
    assert per_key["n_anchor"].max() == 1
    assert per_key["n_actual"].max() == 1


def test_common_holdouts_and_fits(cmp_fit):
    assert cmp_fit.holdouts == (4, 8)
    assert set(cmp_fit.fits) == {"link_ratio", "pooled"}
    for f in cmp_fit.fits.values():
        assert type(f).__name__ == "BacktestFit"
    assert cmp_fit.skipped_holdouts == {"link_ratio": [], "pooled": []}


def test_match_summary_consistency(cmp_fit):
    ms = cmp_fit.match_summary
    assert ms.columns == [
        "estimator", "holdout", "n_cells", "n_matched", "n_dropped",
    ]
    bad = ms.filter(
        pl.col("n_cells") != pl.col("n_matched") + pl.col("n_dropped")
    )
    assert bad.height == 0
    # n_matched at a common depth is the same key count for every estimator
    per_depth = ms.group_by("holdout").agg(
        pl.col("n_matched").n_unique().alias("nu")
    )
    assert per_depth["nu"].max() == 1
    # and sums to the matched key total
    n_keys = cmp_fit.cells.select(
        ["holdout", "cohort", "duration"]
    ).n_unique()
    per_est = ms.group_by("estimator").agg(pl.col("n_matched").sum())
    assert per_est["n_matched"].to_list() == [n_keys, n_keys]


def test_matched_summary_recompute(cmp_fit):
    # The matched horizon summary must be computed on the matched cells,
    # not lifted from the per-fit summaries.
    cells = cmp_fit.cells
    hs = cmp_fit.horizon_summary
    sub = cells.filter(
        (pl.col("estimator") == "link_ratio") & (pl.col("horizon") == 1)
    )
    expected = (
        (sub["actual"] - sub["expected"]).abs().mean()
    )
    got = hs.filter(
        (pl.col("estimator") == "link_ratio") & (pl.col("horizon") == 1)
    )["abs_err_mean"][0]
    assert got == pytest.approx(expected)
    assert hs.filter(
        (pl.col("estimator") == "link_ratio") & (pl.col("horizon") == 1)
    )["n"][0] == sub.height


def test_summary_estimator_insertion_order(cmp_fit):
    # Insertion order ("link_ratio" before "pooled") is canonical, not alphabetical --
    # check the first block of rows belongs to the first label.
    hs = cmp_fit.horizon_summary
    assert hs["estimator"][0] == "link_ratio"
    hc = cmp_fit.horizon_comparison
    # comparison frames carry challengers only -- never the baseline
    assert set(hc["estimator"].unique().to_list()) == {"pooled"}


# ---------------------------------------------------------------------------
# Self-comparison: exact ties everywhere
# ---------------------------------------------------------------------------


def test_self_comparison_rel_zero_and_win_half(self_cmp):
    for comp in (
        self_cmp.horizon_comparison,
        self_cmp.anchor_comparison,
        self_cmp.holdout_comparison,
    ):
        for col in ("abs_err_rel", "ae_err_rel", "bias_rel"):
            vals = comp[col].to_list()
            assert all(v is None or abs(v) < 1e-12 for v in vals)
        # every matched cell is an exact tie -> 0.5 credit each
        assert comp["win_rate"].to_list() == [0.5] * comp.height
        # the baseline stats ride along and equal the challenger's
        eq = comp.filter(
            pl.col("abs_err_mean") != pl.col("base_abs_err_mean")
        )
        assert eq.height == 0


def test_self_comparison_crossover_all_null(self_cmp):
    out = self_cmp.crossover()
    assert out.columns == [
        "estimator", "crossover_at", "early_winner", "late_winner",
        "overall_winner", "n_flips", "max_horizon",
    ]
    assert out.height == 1  # one challenger
    assert out["estimator"][0] == "b"
    assert out["crossover_at"][0] is None
    assert out["early_winner"][0] is None
    assert out["late_winner"][0] is None
    assert out["overall_winner"][0] is None
    assert out["n_flips"][0] == 0


# ---------------------------------------------------------------------------
# Hold-out asymmetry: intersection + warning + match_summary record
# ---------------------------------------------------------------------------


def test_skipped_depth_intersection_and_warning():
    tri = _triangle()
    with pytest.warns(UserWarning, match=r"\[8\]"):
        fit = lr.EstimatorComparison(
            {"good": lr.ChainLadder(), "flaky": _SkipSecondFit()},
            holdouts=(4, 8), target="loss",
        ).fit(tri)
    assert fit.holdouts == (4,)
    assert fit.skipped_holdouts == {"good": [], "flaky": [8]}
    # the asymmetric depth shows up in repr's skipped block
    assert "skipped={'flaky': [8]}" in repr(fit)
    # match_summary records the excluded depth with n_matched = 0
    ms = fit.match_summary
    lost = ms.filter(
        (pl.col("estimator") == "good") & (pl.col("holdout") == 8)
    )
    assert lost.height == 1
    assert lost["n_matched"][0] == 0
    assert lost["n_dropped"][0] == lost["n_cells"][0]
    # flaky has no row at depth 8 -- it never survived there
    assert ms.filter(
        (pl.col("estimator") == "flaky") & (pl.col("holdout") == 8)
    ).height == 0
    # cells carry only the common depth
    assert fit.cells["holdout"].unique().to_list() == [4]


def test_empty_intersection_gives_typed_empty_frames():
    tri = _triangle()
    with pytest.warns(UserWarning, match="dropped"):
        fit = lr.EstimatorComparison(
            {"good": lr.ChainLadder(), "bad": _NeverFits()},
            holdouts=(4, 8), target="loss",
        ).fit(tri)
    assert fit.holdouts == ()
    assert fit.cells.height == 0
    assert "estimator" in fit.cells.columns
    for frame in (
        fit.horizon_summary, fit.anchor_summary, fit.holdout_summary,
        fit.horizon_comparison, fit.anchor_comparison,
        fit.holdout_comparison,
    ):
        assert frame.height == 0
        assert frame.schema["estimator"] == pl.Utf8
        assert "n" in frame.columns
    co = fit.crossover()
    assert co.height == 0
    assert co.columns == [
        "estimator", "crossover_at", "early_winner", "late_winner",
        "overall_winner", "n_flips", "max_horizon",
    ]
    assert co.schema["crossover_at"] == pl.Int64
    # the per-label evidence remains reachable
    assert fit.fits["good"].ae_err.height > 0


# ---------------------------------------------------------------------------
# Incremental lane: all-or-nothing
# ---------------------------------------------------------------------------


def test_incr_lane_present_when_all_carry_it(cmp_fit):
    assert "incr_ae_err" in cmp_fit.cells.columns
    assert "incr_abs_err_mean" in cmp_fit.horizon_summary.columns
    assert "incr_win_rate" in cmp_fit.horizon_comparison.columns
    assert "incr_base_abs_err_mean" in cmp_fit.horizon_comparison.columns
    # the incremental crossover walk runs
    out = cmp_fit.crossover(basis="incremental")
    assert out.height == 1


def test_incr_all_or_nothing():
    fit = lr.EstimatorComparison(
        {"link_ratio": lr.ChainLadder(), "noincr": _NoIncr()},
        holdouts=(4,), target="loss",
    ).fit(_triangle())
    assert not any(c.startswith("incr_") for c in fit.cells.columns)
    for frame in (
        fit.horizon_summary, fit.horizon_comparison, fit.match_summary,
    ):
        assert not any(c.startswith("incr_") for c in frame.columns)
    with pytest.raises(ValueError, match="incremental"):
        fit.crossover(basis="incremental")
    with pytest.raises(ValueError, match="incremental"):
        fit.plot(basis="incremental")


# ---------------------------------------------------------------------------
# Comparison-frame semantics
# ---------------------------------------------------------------------------


def test_comparison_frame_layout(cmp_fit):
    hc = cmp_fit.horizon_comparison
    cum = [
        "abs_err_mean",    "base_abs_err_mean",    "abs_err_rel",
        "ae_err_abs_mean", "base_ae_err_abs_mean", "ae_err_rel",
        "ae_err_wt",       "base_ae_err_wt",       "bias_rel",
        "win_rate",
    ]
    expected = (
        ["estimator", "horizon", "n"] + cum + ["incr_" + c for c in cum]
    )
    assert hc.columns == expected
    # n equals the matched cell count at that horizon (estimator-invariant)
    cells = cmp_fit.cells
    for h in hc["horizon"].to_list()[:3]:
        n_keys = cells.filter(
            (pl.col("estimator") == "pooled") & (pl.col("horizon") == h)
        ).height
        assert hc.filter(pl.col("horizon") == h)["n"][0] == n_keys
    # win_rate lives in [0, 1]
    assert hc["win_rate"].min() >= 0.0
    assert hc["win_rate"].max() <= 1.0


def test_comparison_rel_recompute(cmp_fit):
    hc = cmp_fit.horizon_comparison
    row = hc.row(0, named=True)
    assert row["abs_err_rel"] == pytest.approx(
        row["abs_err_mean"] / row["base_abs_err_mean"] - 1
    )
    assert row["ae_err_rel"] == pytest.approx(
        row["ae_err_abs_mean"] / row["base_ae_err_abs_mean"] - 1
    )
    assert row["bias_rel"] == pytest.approx(
        abs(row["ae_err_wt"]) / abs(row["base_ae_err_wt"]) - 1
    )


def test_rel_null_on_zero_baseline(self_cmp):
    # A zero baseline statistic must yield a null relative column (never
    # inf). Unit-level: patch the matched cells on a shallow copy and call
    # the internal comparison builder directly.
    patched = copy.copy(self_cmp)
    patched._groups = None
    patched._labels = ["base", "ch"]
    patched.baseline = "base"
    patched._has_incr = False
    patched._cells = pl.DataFrame(
        {
            "estimator":       ["base", "ch"],
            "holdout":         [4, 4],
            "horizon":         [1, 1],
            "anchor_duration": [3, 3],
            "cohort":          [1, 1],
            "duration":        [4, 4],
            "actual":          [100.0, 100.0],
            "expected":        [100.0, 90.0],   # baseline is exact -> 0 err
            "aeg":             [0.0, 10.0],
            "ae_err":          [0.0, 100.0 / 90.0 - 1],
        },
        schema_overrides={"holdout": pl.Int64, "horizon": pl.Int64},
    )
    out = patched._compare("horizon", patched._build_win_cells())
    assert out["abs_err_rel"][0] is None
    assert out["ae_err_rel"][0] is None
    assert out["bias_rel"][0] is None
    assert out["win_rate"][0] == 0.0  # challenger strictly loses
    assert out["n"][0] == 1


def test_win_rate_excludes_nonfinite_cells(self_cmp):
    # A NaN / inf cell earns NEITHER tie credit NOR a loss -- it is
    # excluded from the win_rate denominator entirely (NaN == NaN is True
    # in polars, so an unguarded tie branch would hand it 0.5; an inf on
    # the BASELINE side would hand the challenger a free win).
    nan, inf = float("nan"), float("inf")
    patched = copy.copy(self_cmp)
    patched._groups = None
    patched._labels = ["base", "ch"]
    patched.baseline = "base"
    patched._has_incr = False
    patched._cells = pl.DataFrame(
        {
            "estimator":       ["base", "ch"] * 3,
            "holdout":         [4] * 6,
            "horizon":         [1] * 6,
            "anchor_duration": [3] * 6,
            "cohort":          [1, 1, 2, 2, 3, 3],
            "duration":        [4] * 6,
            # cohort 1: challenger strictly loses; cohort 2: NaN actual on
            # both sides; cohort 3: inf expected on the baseline side.
            "actual":          [100.0, 100.0, nan, nan, 100.0, 100.0],
            "expected":        [100.0, 90.0, 95.0, 95.0, inf, 99.0],
            "aeg":             [0.0] * 6,
            "ae_err":          [0.0] * 6,
        },
        schema_overrides={"holdout": pl.Int64, "horizon": pl.Int64},
    )
    wc = patched._build_win_cells()
    assert wc.sort("cohort")["_win"].to_list() == [0.0, None, None]
    out = patched._compare("horizon", wc)
    # win_rate averages the single valid cell only (a 0.5-credit leak from
    # the NaN tie or a free win from the inf baseline would lift it)
    assert out["win_rate"][0] == 0.0
    assert out["n"][0] == 3  # the matched cell count itself is unchanged


def test_non_default_baseline_flips_roles():
    tri = _triangle()
    default = lr.EstimatorComparison(
        {"link_ratio": lr.ChainLadder(), "pooled": lr.PooledLoss()},
        holdouts=(4,), target="loss",
    ).fit(tri)
    flipped = lr.EstimatorComparison(
        {"link_ratio": lr.ChainLadder(), "pooled": lr.PooledLoss()},
        holdouts=(4,), target="loss", baseline="pooled",
    ).fit(tri)
    assert flipped.baseline == "pooled"
    hc_d = default.horizon_comparison.sort("horizon")
    hc_f = flipped.horizon_comparison.sort("horizon")
    # the challenger row set excludes the (non-first) baseline label
    assert set(hc_f["estimator"].unique().to_list()) == {"link_ratio"}
    # base_* now carries ed's stats (the default fit's challenger column)
    # and the challenger column carries cl's (the default fit's base_*)
    assert hc_f["base_abs_err_mean"].to_list() == pytest.approx(
        hc_d["abs_err_mean"].to_list()
    )
    assert hc_f["abs_err_mean"].to_list() == pytest.approx(
        hc_d["base_abs_err_mean"].to_list()
    )
    # rel signs flip: (a/b - 1) vs (b/a - 1) have opposite signs
    pairs = [
        (f, d)
        for f, d in zip(
            hc_f["abs_err_rel"].to_list(), hc_d["abs_err_rel"].to_list()
        )
        if f is not None and d is not None and d != 0
    ]
    assert pairs  # the sign-flip check is not vacuous
    for f, d in pairs:
        assert f * d < 0


# ---------------------------------------------------------------------------
# crossover(): validation + walk semantics
# ---------------------------------------------------------------------------


def test_crossover_validation(cmp_fit):
    with pytest.raises(ValueError, match="by"):
        cmp_fit.crossover(by="holdout")
    with pytest.raises(ValueError, match="metric"):
        cmp_fit.crossover(metric="bogus")
    with pytest.raises(ValueError, match="basis"):
        cmp_fit.crossover(basis="bogus")
    for bad in (0, -1, True, 2.5):
        with pytest.raises(ValueError, match="min_run"):
            cmp_fit.crossover(min_run=bad)


def test_entry_winners_static():
    F = EstimatorComparisonFit
    w = F._entry_winners(
        [1.0, None, 2.0, 0.5, float("nan")], [1.0, 1.0, 1.0, 1.0, 1.0]
    )
    assert w == [None, None, "b", "c", None]


def test_terminal_run_static():
    F = EstimatorComparisonFit
    # terminal run of 'c' x 2 after a 'b'
    assert F._terminal_run(["b", "c", "c"]) == (2, "c", 1, 1)
    # null at the deepest entry: zero-length run
    assert F._terminal_run(["c", "c", None]) == (0, None, 3, 0)
    # null mid-sequence breaks the backward walk but not the flip count
    assert F._terminal_run(["b", None, "c"]) == (1, "c", 2, 1)
    # uniform winner spans the whole sequence
    assert F._terminal_run(["c", "c", "c"]) == (3, "c", 0, 0)
    # empty sequence
    assert F._terminal_run([]) == (0, None, 0, 0)


def _patched_crossover_fit(
    template, horizons, ch_vals, base_vals, cells,
    cols=("abs_err_mean", "base_abs_err_mean"),
):
    """Shallow-copied fit with hand-built comparison + cells frames, so the
    crossover walk is fully controlled. ``cols`` names the (challenger,
    baseline) metric pair the comparison frame carries."""
    p = copy.copy(template)
    p._groups = None
    p._labels = ["base", "ch"]
    p.baseline = "base"
    p._has_incr = False
    p._cells = cells
    p._horizon_comparison = pl.DataFrame(
        {
            "estimator": ["ch"] * len(horizons),
            "horizon":   horizons,
            cols[0]:     ch_vals,
            cols[1]:     base_vals,
        },
        schema_overrides={"horizon": pl.Int64},
    )
    return p


def _cells_frame(horizons, base_err, ch_err):
    """One matched cell per horizon per side; actual = 100, expected offset
    by the requested absolute error (``ae_err`` derived consistently)."""
    rows = {
        "estimator": [], "horizon": [], "actual": [], "expected": [],
        "ae_err": [],
    }
    for h, be, ce in zip(horizons, base_err, ch_err):
        rows["estimator"] += ["base", "ch"]
        rows["horizon"] += [h, h]
        rows["actual"] += [100.0, 100.0]
        rows["expected"] += [100.0 - be, 100.0 - ce]
        rows["ae_err"] += [
            100.0 / (100.0 - be) - 1, 100.0 / (100.0 - ce) - 1,
        ]
    return pl.DataFrame(rows, schema_overrides={"horizon": pl.Int64})


def test_crossover_dominance_no_crossover(self_cmp):
    # Challenger better at EVERY horizon: the run starts at the first
    # observed entry, so crossover_at is null even though late_winner fires
    # -- and the read is "no crossover + overall_winner" (the deprecation
    # evidence).
    horizons = list(range(1, 11))
    p = _patched_crossover_fit(
        self_cmp, horizons,
        ch_vals=[0.5] * 10, base_vals=[1.0] * 10,
        cells=_cells_frame(horizons, [1.0] * 10, [0.5] * 10),
    )
    out = p.crossover(metric="abs_err")
    assert out.height == 1
    assert out["crossover_at"][0] is None
    assert out["early_winner"][0] is None   # run spans all entries
    assert out["late_winner"][0] == "ch"
    assert out["overall_winner"][0] == "ch"
    assert out["n_flips"][0] == 0
    assert out["max_horizon"][0] == 10


def test_crossover_sustained_flip(self_cmp):
    # Baseline better at horizons 1-4, challenger better at 5-10 (a 6-entry
    # terminal run >= min_run=6): a genuine, actionable crossover at 5.
    horizons = list(range(1, 11))
    base_err = [1.0] * 10
    ch_err = [10.0] * 4 + [0.1] * 6
    p = _patched_crossover_fit(
        self_cmp, horizons,
        ch_vals=ch_err, base_vals=base_err,
        cells=_cells_frame(horizons, base_err, ch_err),
    )
    out = p.crossover(metric="abs_err", min_run=6)
    assert out["crossover_at"][0] == 5
    assert out["early_winner"][0] == "base"
    assert out["late_winner"][0] == "ch"
    assert out["n_flips"][0] == 1
    # a min_run longer than the terminal run refuses the call
    strict = p.crossover(metric="abs_err", min_run=7)
    assert strict["crossover_at"][0] is None
    assert strict["late_winner"][0] is None


def test_crossover_flicker_below_min_run(self_cmp):
    # The ranking flickers at the deep end: the terminal run is 1 entry, so
    # the default min_run=6 refuses the call; min_run=1 reproduces the
    # unguarded read.
    horizons = list(range(1, 11))
    base_err = [1.0] * 10
    ch_err = [0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 2.0, 0.5, 2.0, 0.5]
    p = _patched_crossover_fit(
        self_cmp, horizons,
        ch_vals=ch_err, base_vals=base_err,
        cells=_cells_frame(horizons, base_err, ch_err),
    )
    out = p.crossover(metric="abs_err")
    assert out["late_winner"][0] is None
    assert out["crossover_at"][0] is None
    assert out["n_flips"][0] == 4
    unguarded = p.crossover(metric="abs_err", min_run=1)
    assert unguarded["late_winner"][0] == "ch"


def test_crossover_zero_length_run_nulls_early_winner(self_cmp):
    # A null winner at the DEEPEST entry yields a zero-length terminal run:
    # late_winner / crossover_at are null regardless of min_run, and
    # early_winner must be null too -- pooling over "the entries before the
    # run" would mislabel ALL entries as pre-run.
    horizons = [1, 2, 3]
    p = _patched_crossover_fit(
        self_cmp, horizons,
        ch_vals=[0.5, 0.5, None], base_vals=[1.0, 1.0, 1.0],
        cells=_cells_frame(horizons, [1.0] * 3, [0.5] * 3),
    )
    out = p.crossover(metric="abs_err", min_run=1)
    assert out["early_winner"][0] is None
    assert out["late_winner"][0] is None
    assert out["crossover_at"][0] is None
    # the pooled-over-all read stays available as overall_winner
    assert out["overall_winner"][0] == "ch"


def test_crossover_metric_bias_compares_magnitudes(self_cmp):
    # metric="bias" walks the SIGNED ae_err_wt pair on absolute magnitude:
    # challenger wt=-0.02 vs base wt=+0.05 means the challenger wins, and
    # alternating signs with constant magnitudes must NOT register flips
    # (a signed comparison would flicker).
    horizons = list(range(1, 11))
    ch_wt = [-0.02 if h % 2 else 0.02 for h in horizons]
    base_wt = [0.05 if h % 2 else -0.05 for h in horizons]
    p = _patched_crossover_fit(
        self_cmp, horizons,
        ch_vals=ch_wt, base_vals=base_wt,
        cells=_cells_frame(horizons, [5.0] * 10, [-2.0] * 10),
        cols=("ae_err_wt", "base_ae_err_wt"),
    )
    out = p.crossover(metric="bias")
    assert out["late_winner"][0] == "ch"
    assert out["n_flips"][0] == 0
    assert out["crossover_at"][0] is None  # dominance, no flip
    # pooled |sum(gap)/sum(expected)|: ch 20/1020 < base 50/950
    assert out["overall_winner"][0] == "ch"


def test_crossover_metric_ae_err(self_cmp):
    # metric="ae_err" walks the ae_err_abs_mean pair: a sustained flip at
    # horizon 5 fires exactly as in the abs_err walk.
    horizons = list(range(1, 11))
    base_err = [1.0] * 10
    ch_err = [10.0] * 4 + [0.1] * 6
    p = _patched_crossover_fit(
        self_cmp, horizons,
        ch_vals=[100.0 / (100.0 - e) - 1 for e in ch_err],
        base_vals=[100.0 / (100.0 - e) - 1 for e in base_err],
        cells=_cells_frame(horizons, base_err, ch_err),
        cols=("ae_err_abs_mean", "base_ae_err_abs_mean"),
    )
    out = p.crossover(metric="ae_err", min_run=6)
    assert out["crossover_at"][0] == 5
    assert out["early_winner"][0] == "base"  # pooled mean|ae_err| pre-run
    assert out["late_winner"][0] == "ch"
    assert out["n_flips"][0] == 1


def test_crossover_by_anchor(self_cmp):
    # The anchor walk reads anchor_comparison over anchor_duration and
    # reports max_anchor.
    anchors = list(range(1, 9))
    base_err = [1.0] * 8
    ch_err = [10.0, 10.0] + [0.1] * 6
    cells = _cells_frame(anchors, base_err, ch_err).rename(
        {"horizon": "anchor_duration"}
    )
    p = copy.copy(self_cmp)
    p._groups = None
    p._labels = ["base", "ch"]
    p.baseline = "base"
    p._has_incr = False
    p._cells = cells
    p._anchor_comparison = pl.DataFrame(
        {
            "estimator":         ["ch"] * 8,
            "anchor_duration":   anchors,
            "abs_err_mean":      ch_err,
            "base_abs_err_mean": base_err,
        },
        schema_overrides={"anchor_duration": pl.Int64},
    )
    out = p.crossover(by="anchor", metric="abs_err", min_run=6)
    assert out.columns[-1] == "max_anchor"
    assert out["max_anchor"][0] == 8
    assert out["crossover_at"][0] == 3
    assert out["early_winner"][0] == "base"
    assert out["late_winner"][0] == "ch"


def test_crossover_real_fit_shape(cmp_fit):
    out = cmp_fit.crossover()
    assert out.height == 1  # one challenger, ungrouped
    assert out["estimator"][0] == "pooled"
    assert out["max_horizon"][0] == cmp_fit.horizon_comparison["horizon"].max()


# ---------------------------------------------------------------------------
# Groups
# ---------------------------------------------------------------------------


def test_grouped_frames_carry_group_columns(grouped_cmp):
    for frame in (
        grouped_cmp.cells,
        grouped_cmp.horizon_summary,
        grouped_cmp.horizon_comparison,
        grouped_cmp.match_summary,
    ):
        assert frame.columns[0] == "coverage"
    co = grouped_cmp.crossover()
    assert co.columns == [
        "coverage", "estimator", "crossover_at", "early_winner",
        "late_winner", "overall_winner", "n_flips", "max_horizon",
    ]
    n_cov = grouped_cmp.cells["coverage"].n_unique()
    assert co.height == n_cov  # one row per (group x challenger)
    assert co["estimator"].unique().to_list() == ["pooled"]


def test_grouped_matching_is_within_group(grouped_cmp):
    # every matched key carries both estimators within its own group
    per_key = grouped_cmp.cells.group_by(
        ["coverage", "holdout", "cohort", "duration"]
    ).agg(pl.col("estimator").n_unique().alias("n_est"))
    assert per_key["n_est"].min() == 2
    assert per_key["n_est"].max() == 2


def test_multi_column_groups():
    df = lr.load_experience().filter(
        pl.col("coverage").is_in(["CI", "SURGERY"])
        & pl.col("channel").is_in(["FC", "TM"])
    )
    tri = lr.Triangle(df, groups=["coverage", "channel"])
    fit = lr.EstimatorComparison(
        {"link_ratio": lr.ChainLadder(), "pooled": lr.PooledLoss()},
        holdouts=(4, 8), target="loss",
    ).fit(tri)
    pairs = (
        fit.cells.select(["coverage", "channel"])
        .unique()
        .sort(["coverage", "channel"])
    )
    assert pairs.height == 4
    hc = fit.horizon_comparison
    assert hc.columns[:2] == ["coverage", "channel"]
    co = fit.crossover()
    assert co.columns[:3] == ["coverage", "channel", "estimator"]
    assert co.height == 4


# ---------------------------------------------------------------------------
# Input mirroring (pandas in -> pandas out)
# ---------------------------------------------------------------------------


def test_pandas_input_mirrors_out():
    pd = pytest.importorskip("pandas")
    df = lr.load_experience().to_pandas()
    tri = lr.Triangle(df)
    fit = lr.EstimatorComparison(
        {"link_ratio": lr.ChainLadder(), "pooled": lr.PooledLoss()},
        holdouts=(4,), target="loss",
    ).fit(tri)
    for frame in (
        fit.cells,
        fit.horizon_summary, fit.anchor_summary, fit.holdout_summary,
        fit.horizon_comparison, fit.anchor_comparison,
        fit.holdout_comparison,
        fit.match_summary,
        fit.crossover(),
    ):
        assert isinstance(frame, pd.DataFrame)


# ---------------------------------------------------------------------------
# repr
# ---------------------------------------------------------------------------


def test_repr(cmp_fit):
    text = repr(cmp_fit)
    assert "EstimatorComparisonFit" in text
    assert "estimators=['link_ratio', 'pooled']" in text
    assert "baseline='link_ratio'" in text
    assert "holdouts=[4, 8]" in text
    assert "target='loss'" in text
    assert "n_matched_cells=" in text
    assert "skipped" not in text  # nothing skipped -> block omitted


# ---------------------------------------------------------------------------
# scorecard / rank / best  (the metric-panel + mechanical-pick surface)
# ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def ratio_cmp():
    """Grouped ratio comparison: pooled (analytical SE -> coverage) vs credible
    (point-only -> null coverage). Single holdout keeps the pick unambiguous."""
    return lr.EstimatorComparison(
        {"pooled": lr.Ratio(loss=lr.PooledLoss()),
         "credible": lr.Ratio(loss=lr.CredibleLoss())},
        holdouts=12, target="ratio", baseline="pooled",
    ).fit(_triangle(groups="coverage"))


def test_scorecard_stacks_every_estimator(ratio_cmp):
    sc = ratio_cmp.scorecard()
    assert isinstance(sc, pl.DataFrame)
    assert set(sc["estimator"].unique()) == {"pooled", "credible"}
    for col in ("estimator", "coverage", "population", "lane", "bias", "mae", "rmse"):
        assert col in sc.columns
    # default has only the "all" population
    assert set(sc["population"].unique()) == {"all"}


def test_scorecard_terminal_adds_population(ratio_cmp):
    sc = ratio_cmp.scorecard(terminal=6)
    assert set(sc["population"].unique()) == {"all", "terminal"}


def test_rank_orders_best_first_within_group(ratio_cmp):
    r = ratio_cmp.rank(metric="mae")
    assert isinstance(r, pl.DataFrame)
    # within each (coverage, holdout) the rank-1 row has the minimum mae
    keys = ["coverage", "holdout"] if "holdout" in r.columns else ["coverage"]
    for (_, grp) in r.group_by(keys):
        assert grp.filter(pl.col("rank") == grp["rank"].min())["mae"].min() == grp["mae"].min()
    # sorted best-first
    assert r["rank"].to_list() == sorted(r["rank"].to_list()) or "coverage" in r.columns


def test_rank_unknown_metric_raises(ratio_cmp):
    with pytest.raises(ValueError, match="not in the scorecard"):
        ratio_cmp.rank(metric="nonsense")


def test_terminal_population_requires_terminal_arg(ratio_cmp):
    with pytest.raises(ValueError, match="terminal="):
        ratio_cmp.rank(metric="mae", population="terminal")


def test_best_rank_sum_equals_component_sum(ratio_cmp):
    b = ratio_cmp.best(metrics=("bias", "mae"))
    rank_cols = [c for c in b.columns if c.endswith("_rank")]
    assert set(rank_cols) == {"bias_rank", "mae_rank"}
    recomputed = b.select(pl.sum_horizontal(rank_cols)).to_series()
    assert (recomputed == b["rank_sum"]).all()
    # winner (min rank_sum) sorts first within each group
    keys = ["coverage", "holdout"] if "holdout" in b.columns else ["coverage"]
    for (_, grp) in b.group_by(keys):
        assert grp["rank_sum"].to_list()[0] == grp["rank_sum"].min()


def test_best_drops_partial_null_coverage(ratio_cmp):
    # credible is point-only -> coverage_80 is null for it -> the default panel
    # (bias, mae, coverage_80) must drop coverage_80 and warn.
    with pytest.warns(UserWarning, match="coverage"):
        b = ratio_cmp.best()
    assert "coverage_80_rank" not in b.columns
    assert {"bias_rank", "mae_rank", "rank_sum"} <= set(b.columns)


def test_best_all_metrics_unavailable_raises(ratio_cmp):
    with pytest.raises(ValueError, match="nothing to\\s+rank on"):
        ratio_cmp.best(metrics=("coverage_80",))
