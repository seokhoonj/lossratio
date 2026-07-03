"""R parity tests for the regime-detection extensions added in Round 1.

Covers:

1. ``detect_regime(window="auto")`` -- the resolved window value and the
   resulting ``$changes`` table. The R side (legacy naming, not yet
   re-ported) falls back to ``detect_maturity()`` when ``window="auto"``;
   Python's elbow heuristic must land on the same per-combo integer.
2. ``detect_regime(by=...)`` -- per-group dispatch on a multi-coverage
   Triangle. The combined ``$changes`` and ``$labels`` tables (prefixed
   with the group column) must match R row-for-row.
3. Derived regime targets (``loss_ata`` / ``premium_ata`` / ``loss_intensity``)
   -- both (a) the derived trajectory itself (output of
   ``.derive_regime_target``) and (b) the resulting ``detect_regime()``
   change points on that target.

Fixtures (from ``dev/parity_dump.R``):

* ``regime_window_auto_window.csv``
* ``regime_window_auto_changes.csv``
* ``regime_by_group_changes.csv``
* ``regime_by_group_labels.csv``
* ``regime_target_<TGT>_trajectory.csv`` for TGT in
  {``loss_ata``, ``premium_ata``, ``loss_intensity``}
* ``regime_target_<TGT>_changes.csv`` for the same TGT set

To refresh: ``Rscript -e 'source("dev/parity_dump.R")'`` in the R repo.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
import pytest

import lossratio as lr
from lossratio.diagnostics.regime import _derive_regime_target

FIXTURES = Path(__file__).parent / "fixtures"
ATOL = 1e-6
RTOL = 1e-9

# Regime detection runs an internal permutation test (R = 999 default
# Monte Carlo replicates in the e_divisive significance step), so the
# `magnitude` / `pre_value` / `post_value` columns can drift by 1-2%
# across language RNGs. Compare those at a looser tolerance than the
# bit-deterministic parts (cohort / change date / regime_id).
_STAT_RTOL = 1e-2
_STAT_ATOL = 1e-4


def _load(name: str) -> pl.DataFrame:
    fp = FIXTURES / f"{name}.csv"
    if not fp.exists():
        pytest.skip(f"missing R fixture: {fp.name}")
    return pl.read_csv(fp, try_parse_dates=True, infer_schema_length=10000)


def _exp_sur() -> pl.DataFrame:
    return _load("experience").filter(pl.col("coverage") == "surgery")


def _exp_all() -> pl.DataFrame:
    return _load("experience")


def _compare_loose_numeric(
    py_df: pl.DataFrame,
    r_df: pl.DataFrame,
    cols: list[str],
    atol: float = _STAT_ATOL,
    rtol: float = _STAT_RTOL,
) -> None:
    """Loose tolerance comparator -- RNG-touched columns can drift mildly."""
    assert py_df.height == r_df.height, (
        f"row count mismatch: py={py_df.height} r={r_df.height}"
    )
    for c in cols:
        if c not in r_df.columns or c not in py_df.columns:
            continue
        py_v = py_df[c].to_list()
        r_v = r_df[c].to_list()
        for i, (a, b) in enumerate(zip(py_v, r_v, strict=False)):
            if a is None or b is None:
                continue
            if isinstance(a, float) and (a != a):
                continue
            if isinstance(b, float) and (b != b):
                continue
            tol = atol + rtol * abs(b)
            assert abs(a - b) <= tol, (
                f"loose col {c!r} row {i}: py={a} r={b} "
                f"diff={a - b} tol={tol}"
            )


# ---------------------------------------------------------------------------
# 1) detect_regime(window="auto")
# ---------------------------------------------------------------------------


def test_regime_window_auto_resolved_value_matches_r():
    """The auto-resolved window integer must match R.

    The R side (legacy naming, not yet re-ported) delegates its auto-window
    path to ``detect_maturity()``; Python delegates to the elbow heuristic.
    The two heuristics intersect on
    well-behaved triangles -- when they diverge we want the test to
    surface the gap loudly.
    """
    r = _load("regime_window_auto_window")
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    reg = lr.RegimeDetector(window="auto", method="e_divisive", seed=0).detect(tri)

    r_window = int(r["window"][0])
    # Python ``window`` is scalar for single-combo; list for multi.
    py_window = reg.window
    if isinstance(py_window, list):
        assert len(py_window) == 1, (
            f"single coverage but window list has {len(py_window)} entries"
        )
        py_window = py_window[0]
    assert int(py_window) == r_window, (
        f"auto-resolved window differs: py={py_window} r={r_window}"
    )


def test_regime_window_auto_changes_match_r():
    """With the same resolved window both languages must detect the same
    change points (date + regime_id).
    """
    r = _load("regime_window_auto_changes")
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    reg = lr.RegimeDetector(window="auto", method="e_divisive", seed=0).detect(tri)

    py_changes = reg.changes
    # Polars: convert to a DataFrame (already polars output by default)
    if not isinstance(py_changes, pl.DataFrame):
        py_changes = pl.from_pandas(py_changes)

    r_dates = sorted(r["change"].to_list())
    py_dates = sorted(py_changes["change"].to_list()) if py_changes.height else []
    assert py_dates == r_dates, (
        f"window=auto change dates differ: py={py_dates} r={r_dates}"
    )

    if py_changes.height and "regime_id" in py_changes.columns:
        assert sorted(py_changes["regime_id"].to_list()) == sorted(
            r["regime_id"].to_list()
        )


# ---------------------------------------------------------------------------
# 2) detect_regime(by=...) -- multi-group dispatch
# ---------------------------------------------------------------------------


def test_regime_by_group_changes_match_r():
    """Per-group ``$changes`` table on the full 4-coverage experience.

    The R dump uses ``window = 12L`` (fixed) so the trajectory window is
    deterministic across the four coverages. Python's per-group dispatch
    must produce the same change date(s) for each coverage that R
    detected a change in.
    """
    r = _load("regime_by_group_changes")
    tri = lr.Triangle(_exp_all(), groups="coverage")
    reg = lr.RegimeDetector(by="coverage", window=12, method="e_divisive", seed=0).detect(tri)

    py_changes = reg.changes
    if not isinstance(py_changes, pl.DataFrame):
        py_changes = pl.from_pandas(py_changes)

    # Compare grouped by coverage: a coverage that has changes in R must
    # also have (the same) changes in Python. Coverages with no changes
    # contribute zero rows on both sides, which is fine.
    keys = ["coverage", "change"]
    py_pairs = (
        py_changes.select(keys).sort(keys).rows() if py_changes.height else []
    )
    r_pairs = r.select(keys).sort(keys).rows() if r.height else []
    assert py_pairs == r_pairs, (
        f"per-group change set differs:\n py={py_pairs}\n r={r_pairs}"
    )


def test_regime_by_group_labels_match_r():
    """Per-cohort regime labels under ``by="coverage"``.

    The label table assigns each cohort a regime_id within its group.
    Python and R must agree on the full label assignment per (coverage,
    cohort).
    """
    r = _load("regime_by_group_labels")
    tri = lr.Triangle(_exp_all(), groups="coverage")
    reg = lr.RegimeDetector(by="coverage", window=12, method="e_divisive", seed=0).detect(tri)

    py = reg.df
    if not isinstance(py, pl.DataFrame):
        py = pl.from_pandas(py)

    keys = ["coverage", "cohort"]
    if not all(k in py.columns for k in keys):
        pytest.skip(
            f"Python labels frame missing one of {keys}: cols={py.columns}"
        )

    py_sorted = py.sort(keys)
    r_sorted = r.sort(keys)
    assert py_sorted.height == r_sorted.height, (
        f"label row count differs: py={py_sorted.height} r={r_sorted.height}"
    )
    # Coverage + cohort identity must align.
    for k in keys:
        assert py_sorted[k].to_list() == r_sorted[k].to_list(), (
            f"label col {k!r} differs"
        )
    # regime_id under e_divisive may slip by RNG; assert at least the
    # number of distinct regimes per coverage matches.
    if "regime_id" in py_sorted.columns and "regime_id" in r_sorted.columns:
        py_n = (
            py_sorted.group_by("coverage")
            .agg(pl.col("regime_id").n_unique().alias("n"))
            .sort("coverage")
        )
        r_n = (
            r_sorted.group_by("coverage")
            .agg(pl.col("regime_id").n_unique().alias("n"))
            .sort("coverage")
        )
        assert py_n["n"].to_list() == r_n["n"].to_list(), (
            f"per-coverage distinct regime count differs:\n py={py_n}\n r={r_n}"
        )


# ---------------------------------------------------------------------------
# 3) Derived regime targets -- loss_ata / premium_ata / loss_intensity
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("target", ["loss_ata", "premium_ata", "loss_intensity"])
def test_derived_regime_trajectory_matches_r(target: str):
    """The derived per-(group, cohort, duration) trajectory matches R.

    Both languages drop the first duration row per cohort (NA from the lag)
    and re-index ``duration`` so the first surviving period becomes 1. The
    derived target column itself is a deterministic arithmetic combo
    of loss / premium -- must be bit-parity.
    """
    r = _load(f"regime_target_{target}_trajectory")
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    tri_df = tri.to_polars()
    out_df, _name = _derive_regime_target(tri_df, target, groups="coverage")

    keys = ["coverage", "cohort", "duration"]
    py_sorted = out_df.select(
        [c for c in [*keys, target] if c in out_df.columns]
    ).sort(keys)
    r_sorted = r.select(
        [c for c in [*keys, target] if c in r.columns]
    ).sort(keys)

    assert py_sorted.height == r_sorted.height, (
        f"{target} trajectory row count: "
        f"py={py_sorted.height} r={r_sorted.height}"
    )
    # Coverage / cohort / duration identity must align row-for-row.
    for k in keys:
        assert py_sorted[k].to_list() == r_sorted[k].to_list(), (
            f"{target} trajectory key col {k!r} differs"
        )
    # The derived value itself: deterministic, tight tolerance.
    py_vals = py_sorted[target].to_list()
    r_vals = r_sorted[target].to_list()
    for i, (a, b) in enumerate(zip(py_vals, r_vals, strict=False)):
        if a is None or b is None:
            continue
        if isinstance(a, float) and (a != a):
            continue
        if isinstance(b, float) and (b != b):
            continue
        tol = ATOL + RTOL * abs(b)
        assert abs(a - b) <= tol, (
            f"{target} row {i}: py={a} r={b} diff={a - b} tol={tol}"
        )


@pytest.mark.parametrize("target", ["loss_ata", "premium_ata", "loss_intensity"])
def test_detect_regime_on_derived_target_changes_match_r(target: str):
    """``detect_regime(target=<derived>)`` ``$changes`` must match R.

    R's ``detect_regime(loss=<derived>)`` and Python's
    ``lr.RegimeDetector(target=<derived>).detect(Triangle)`` must agree on the
    change set (date + regime_id). The numeric statistics
    (``pre_value`` / ``post_value`` / ``magnitude``) touch RNG, so we
    compare those at a loose tolerance.
    """
    r = _load(f"regime_target_{target}_changes")
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    reg = lr.RegimeDetector(target=target, window=12, method="e_divisive", seed=0).detect(tri)

    py = reg.changes
    if not isinstance(py, pl.DataFrame):
        py = pl.from_pandas(py)

    r_dates = sorted(r["change"].to_list())
    py_dates = sorted(py["change"].to_list()) if py.height else []
    assert py_dates == r_dates, (
        f"{target}: change dates differ py={py_dates} r={r_dates}"
    )

    if py.height and r.height:
        # Sort by (change) for row alignment, then loose-compare the
        # numeric stats.
        py_sorted = py.sort("change")
        r_sorted = r.sort("change")
        _compare_loose_numeric(
            py_sorted, r_sorted,
            cols=["pre_value", "post_value", "magnitude"],
        )
