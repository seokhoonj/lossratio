"""Golden-master regression — Phase 0 of the canonical Python rewrite.

The fixtures in ``tests/golden/*.parquet`` PIN the current package's numeric
outputs across every reachable surface. They are the durable regression ORACLE
for the planned rewrite: the rewrite (new API) must reproduce these numbers
bit-for-bit, EXCEPT where a deliberate methodology change is flagged (BF/CC
removal, an ED g->0 tail, etc.) — those are validated separately, not here.

This module uses the CURRENT API. When the rewrite changes the API, rewrite the
``golden_outputs()`` builder for the new API but KEEP the fixtures unchanged.

Regenerate fixtures (only when a change to the oracle is intended):
    python tests/test_golden_master.py
"""
from __future__ import annotations

import os
import sys

# Allow running as a plain script (regeneration); harmless under pytest.
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

import polars as pl  # noqa: E402
from polars.testing import assert_frame_equal  # noqa: E402
import pytest  # noqa: E402

import lossratio as lr  # noqa: E402

FIXTURES = os.path.join(os.path.dirname(__file__), "golden")
SEED = 42
B = 200


def _frame(obj) -> pl.DataFrame:
    """Coerce a result object / frame to a polars DataFrame."""
    if isinstance(obj, pl.DataFrame):
        return obj
    if hasattr(obj, "to_polars"):
        return obj.to_polars()
    d = obj.df
    return d if isinstance(d, pl.DataFrame) else d.to_polars()


def _sorted(df: pl.DataFrame) -> pl.DataFrame:
    """Deterministic row order: sort by the non-float (key) columns."""
    keys = [c for c, t in zip(df.columns, df.dtypes) if not t.is_float()]
    return df.sort(keys) if keys else df


def golden_outputs() -> dict[str, pl.DataFrame]:
    """Every pinned surface -> a polars frame of its numeric output (sorted)."""
    df = lr.load_experience()
    tri = lr.Triangle(df, groups="coverage")

    out: dict[str, pl.DataFrame] = {}

    # --- data ---
    out["triangle"] = _frame(tri)
    out["calendar"] = _frame(tri.calendar_agg())
    out["total"] = _frame(tri.total_agg())

    # --- point projections ---
    out["cl"] = _frame(lr.LinkRatio().fit(tri))
    out["ed"] = _frame(lr.PooledLoss().fit(tri))

    # --- multi-column groups ---
    mc_df = df.with_columns(
        pl.when(pl.col("uy_m").dt.year() % 2 == 0)
          .then(pl.lit("E")).otherwise(pl.lit("O")).alias("block")
    )
    mc = lr.Triangle(mc_df, groups=["coverage", "block"])

    out["mc_triangle"] = _frame(mc)
    out["mc_calendar"] = _frame(mc.calendar_agg())
    out["mc_total"] = _frame(mc.total_agg())
    out["mc_cl"] = _frame(lr.LinkRatio().fit(mc))
    out["mc_ed"] = _frame(lr.PooledLoss().fit(mc))

    return {k: _sorted(v) for k, v in out.items()}


CASE_NAMES = [
    "triangle", "calendar", "total",
    "cl", "ed",
    # multi-column groups oracle
    "mc_triangle", "mc_calendar", "mc_total",
    "mc_cl", "mc_ed",
]


# Cases whose RESULT CLASS changed in the rewrite (CLFit/EDFit -> the
# role-based LossFit): the fixtures keep the original schema, so we compare
# only the numeric columns BOTH schemas carry -- the loss projection and the
# Mack SE decomposition. Intentionally outside this intersection: the
# premium_* / loss_ci_* columns the new LossFit adds, and the *_se2 /
# proc_cv / param_cv columns the old CLFit/EDFit carried. The shared columns
# must still match the pinned oracle bit-for-bit on PROJECTED cells (both
# route _fit_mack); the one convention change is observed-cell projection SE,
# which the old CLFit/EDFit wrote as 0.0 and the new LossFit leaves null --
# normalized via fill_null(0.0) on both sides before the compare.
_REDESIGNED_SHARED_COLS: dict[str, list[str]] = {
    "cl": [
        "coverage", "cohort", "duration",
        "loss_obs", "loss_proj", "incr_loss_proj",
        "loss_proc_se", "loss_param_se", "loss_total_se", "loss_total_cv",
    ],
    "ed": [
        "coverage", "cohort", "duration",
        "loss_obs", "loss_proj", "incr_loss_proj",
        "loss_proc_se", "loss_param_se", "loss_total_se", "loss_total_cv",
    ],
    "mc_cl": [
        "block", "coverage", "cohort", "duration",
        "loss_obs", "loss_proj", "incr_loss_proj",
        "loss_proc_se", "loss_param_se", "loss_total_se", "loss_total_cv",
    ],
    "mc_ed": [
        "block", "coverage", "cohort", "duration",
        "loss_obs", "loss_proj", "incr_loss_proj",
        "loss_proc_se", "loss_param_se", "loss_total_se", "loss_total_cv",
    ],
}


@pytest.fixture(scope="session")
def outputs() -> dict[str, pl.DataFrame]:
    return golden_outputs()


@pytest.mark.parametrize("name", CASE_NAMES)
def test_golden_master(name: str, outputs: dict[str, pl.DataFrame]) -> None:
    path = os.path.join(FIXTURES, f"{name}.parquet")
    assert os.path.exists(path), (
        f"missing golden fixture for {name!r} — run `python tests/test_golden_master.py`"
    )
    expected = pl.read_parquet(path)
    got = outputs[name]
    cols = _REDESIGNED_SHARED_COLS.get(name)
    if cols is not None:
        # Normalize the observed-cell SE convention (old 0.0 vs new null) so
        # the compare pins the real projected values, not the sentinel.
        fill = pl.col(pl.Float64).fill_null(0.0)
        got = _sorted(got.select(cols).with_columns(fill))
        expected = _sorted(expected.select(cols).with_columns(fill))
    assert_frame_equal(got, expected, check_exact=False, rel_tol=1e-9, abs_tol=1e-9)


def _generate() -> None:
    os.makedirs(FIXTURES, exist_ok=True)
    out = golden_outputs()
    assert sorted(out.keys()) == sorted(CASE_NAMES), (
        f"CASE_NAMES out of sync: {sorted(out.keys())} vs {sorted(CASE_NAMES)}"
    )
    for name, frame in out.items():
        frame.write_parquet(os.path.join(FIXTURES, f"{name}.parquet"))
        print(f"wrote {name}: {frame.shape}")


if __name__ == "__main__":
    _generate()
