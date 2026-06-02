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
    sur = lr.Triangle(df.filter(pl.col("coverage") == "SUR"), groups="coverage")

    out: dict[str, pl.DataFrame] = {}

    # --- data + point projections + analytical SE ---
    out["triangle"] = _frame(tri)
    out["calendar"] = _frame(tri.calendar_agg())
    out["total"] = _frame(tri.total_agg())
    out["cl"] = _frame(lr.ChainLadder().fit(tri))
    out["ed"] = _frame(lr.ExposureDriven().fit(tri))
    out["loss_sa"] = _frame(lr.StageAdaptive().fit(tri))
    out["premium"] = _frame(lr.Premium().fit(tri))
    out["ratio_sa"] = _frame(lr.LossRatio(method="sa").fit(tri))
    out["ratio_ed_delta"] = _frame(lr.LossRatio(method="ed", se_method="delta").fit(tri))

    # --- maturity ---
    mat = tri.link().ata().maturity()
    out["maturity"] = _frame(mat)
    out["maturity_point"] = pl.DataFrame(
        {
            "group": list(mat.maturity_point.keys()),
            "point": [int(v) for v in mat.maturity_point.values()],
        }
    )

    # --- convergence (SUR) ---
    conv = lr.LossRatio(method="sa").fit(sur).convergence()
    out["convergence"] = _frame(conv)
    out["convergence_point"] = pl.DataFrame(
        {
            "convergence_point": [conv.convergence_point],
            "maturity_point": [conv.maturity_point],
        },
        schema={"convergence_point": pl.Int64, "maturity_point": pl.Int64},
    )

    # --- regime detection (seeded) + the treatment effect at fit time ---
    reg_sb = tri.detect_regime(target="ratio", seed=SEED, treatment="segment_bridged")
    reg_bb = tri.detect_regime(target="ratio", seed=SEED, treatment="segment_bridged_borrowed")
    out["regime_changes"] = reg_sb.changes
    out["loss_sa_regime_sb"] = _frame(lr.StageAdaptive(regime=reg_sb).fit(tri))
    out["loss_sa_regime_bb"] = _frame(lr.StageAdaptive(regime=reg_bb).fit(tri))

    # --- backtest ---
    bt = lr.Backtest(lr.LossRatio(method="sa"), holdout=6, target="ratio").fit(tri)
    out["bt_ae_err"] = bt.ae_err
    out["bt_col_summary"] = bt.col_summary
    out["bt_diag_summary"] = bt.diag_summary

    # --- bootstrap (seeded) — only the reachable combos (normal process) ---
    out["boot_analytical_cl"] = _frame(
        lr.Bootstrap(type="analytical", method="cl", seed=SEED, B=B).fit(tri, target="loss")
    )
    out["boot_parametric_cl"] = _frame(
        lr.Bootstrap(type="parametric", method="cl", seed=SEED, B=B).fit(tri, target="loss")
    )

    return {k: _sorted(v) for k, v in out.items()}


CASE_NAMES = [
    "triangle", "calendar", "total",
    "cl", "ed", "loss_sa", "premium", "ratio_sa", "ratio_ed_delta",
    "maturity", "maturity_point", "convergence", "convergence_point",
    "regime_changes", "loss_sa_regime_sb", "loss_sa_regime_bb",
    "bt_ae_err", "bt_col_summary", "bt_diag_summary",
    "boot_analytical_cl", "boot_parametric_cl",
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
        "coverage", "cohort", "dev",
        "loss_obs", "loss_proj", "incr_loss_proj",
        "loss_proc_se", "loss_param_se", "loss_total_se", "loss_total_cv",
    ],
    "ed": [
        "coverage", "cohort", "dev",
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
