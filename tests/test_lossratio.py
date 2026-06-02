"""Sanity tests for the placeholder package."""

from pathlib import Path

import polars as pl

import lossratio
import lossratio as lr


def test_import():
    assert lossratio is not None


def test_version_attribute_present():
    assert hasattr(lossratio, "__version__")
    assert isinstance(lossratio.__version__, str)
    assert len(lossratio.__version__) > 0


# ---------------------------------------------------------------------------
# Summary schema parity — R uses role-prefixed columns (loss_ult,
# premium_ult), not generic `ultimate*`. Smoke test on the dispatcher
# layer so any future drift is caught at the public API boundary.
# ---------------------------------------------------------------------------


def _exp_sur() -> pl.DataFrame:
    fp = Path(__file__).parent / "fixtures" / "experience.csv"
    return (
        pl.read_csv(fp, try_parse_dates=True, infer_schema_length=10000)
        .filter(pl.col("coverage") == "surgery")
    )


def test_loss_summary_uses_role_prefixed_columns():
    """``LossFit.summary()`` emits ``loss_ult`` (not ``ultimate``).

    R parity: ``summary.LossFit`` returns ``loss_ult`` /
    ``loss_total_se`` / ``loss_total_cv`` -- role-prefixed columns. The
    generic ``ultimate`` name was Python-only and is gone.
    """
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    summary = lr.ExposureDriven().fit(tri).summary()
    cols = set(summary.columns)
    assert {"loss_ult", "loss_total_se", "loss_total_cv"} <= cols, (
        f"summary missing role-prefixed columns; got {sorted(cols)}"
    )
    assert "ultimate" not in cols
    assert "ultimate_se" not in cols
    assert "ultimate_cv" not in cols


def test_premium_summary_uses_role_prefixed_columns():
    """``PremiumFit.summary()`` emits ``premium_ult`` (not ``ultimate``)."""
    tri = lr.Triangle(_exp_sur(), groups="coverage")
    summary = lr.Premium(method="ed").fit(tri).summary()
    cols = set(summary.columns)
    assert {"premium_ult", "premium_total_se", "premium_total_cv"} <= cols, (
        f"summary missing role-prefixed columns; got {sorted(cols)}"
    )
    assert "ultimate" not in cols
    assert "ultimate_se" not in cols
    assert "ultimate_cv" not in cols
