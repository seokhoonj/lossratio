"""Inception-stability diagnostic -- hand-computed oracle + contract tests.

Frozen arithmetic (full_credibility = 1082):
  coh=1: lambda = 1000*0.05 + 500*0.02 = 60   (seg c has no rate -> 0, n_unrated=300)
         Z = sqrt(60/1082) = 0.2354844,  cv = sqrt(1/60) = 0.1290994  -> thin
  coh=2: lambda = 2000*0.05 = 100
         Z = sqrt(100/1082) = 0.3040089, cv = 0.1                     -> marginal
  coh=1 with cv_sev=0.5: lambda_F = 1082*1.25 = 1352.5
         Z = sqrt(60/1352.5) = 0.2106237, cv = sqrt(1.25/60) = 0.1443376
"""

from __future__ import annotations

import math

import polars as pl
import pytest

from lossratio.inception import inception_stability

COUNTS = pl.DataFrame({
    "coh": [1, 1, 1, 2],
    "seg": ["a", "b", "c", "a"],
    "count": [1000, 500, 300, 2000],
})
RATES = pl.DataFrame({"seg": ["a", "b"], "rate": [0.05, 0.02]})


def _row(df, coh):
    return df.filter(pl.col("coh") == coh).to_dicts()[0]


def test_basic_oracle():
    out = inception_stability(COUNTS, RATES, on="seg", by="coh",
                              full_credibility=1082.0)
    r1, r2 = _row(out, 1), _row(out, 2)

    assert r1["n_policy"] == 1800
    assert r1["n_unrated"] == 300            # seg c, unmatched
    assert r1["lam"] == pytest.approx(60.0)
    assert r1["Z"] == pytest.approx(math.sqrt(60 / 1082), abs=1e-9)
    assert r1["cv"] == pytest.approx(math.sqrt(1 / 60), abs=1e-9)
    assert r1["status"] == "thin"            # Z 0.235 < 0.3

    assert r2["n_policy"] == 2000
    assert r2["n_unrated"] == 0
    assert r2["lam"] == pytest.approx(100.0)
    assert r2["Z"] == pytest.approx(math.sqrt(100 / 1082), abs=1e-9)
    assert r2["status"] == "marginal"        # 0.3 <= Z 0.304 < 0.5


def test_severity_inflation():
    sev = pl.DataFrame({"coh": [1, 2], "cv_sev": [0.5, 0.0]})
    out = inception_stability(COUNTS, RATES, on="seg", by="coh", severity=sev,
                              full_credibility=1082.0)
    r1 = _row(out, 1)
    assert r1["cv_sev"] == pytest.approx(0.5)
    assert r1["Z"] == pytest.approx(math.sqrt(60 / (1082 * 1.25)), abs=1e-9)
    assert r1["cv"] == pytest.approx(math.sqrt(1.25 / 60), abs=1e-9)


def test_z_caps_at_one():
    big = pl.DataFrame({"coh": [9], "seg": ["a"], "count": [10_000_000]})
    out = inception_stability(big, RATES, on="seg", by="coh")
    assert _row(out, 9)["Z"] == pytest.approx(1.0)
    assert _row(out, 9)["status"] == "usable"


def test_multi_key_join_and_grouping():
    counts = pl.DataFrame({
        "coh": [1, 1], "cov": ["x", "x"], "sex": [0, 1], "age": [40, 40],
        "count": [100, 200],
    })
    rates = pl.DataFrame({
        "sex": [0, 1], "age": [40, 40], "rate": [0.10, 0.05],
    })
    out = inception_stability(counts, rates, on=["sex", "age"],
                              by=["coh", "cov"])
    assert out.height == 1
    # lambda = 100*0.10 + 200*0.05 = 20
    assert out["lam"][0] == pytest.approx(20.0)


def test_pandas_mirroring():
    pd = pytest.importorskip("pandas")
    out = inception_stability(COUNTS.to_pandas(), RATES.to_pandas(),
                              on="seg", by="coh")
    assert isinstance(out, pd.DataFrame)


def test_missing_column_raises():
    with pytest.raises(ValueError, match="missing column"):
        inception_stability(COUNTS.drop("count"), RATES, on="seg", by="coh")


def test_bad_thresholds_raise():
    with pytest.raises(ValueError, match="thin"):
        inception_stability(COUNTS, RATES, on="seg", by="coh",
                            usable=0.2, thin=0.5)


def test_duplicate_rate_keys_rejected():
    # a rate table with duplicate join keys would fan out counts in the left
    # join and inflate lam/Z/cv; reject it up front.
    dup_rates = pl.DataFrame({"seg": ["a", "a", "b"], "rate": [0.05, 0.06, 0.02]})
    with pytest.raises(ValueError, match="duplicate"):
        inception_stability(COUNTS, dup_rates, on="seg", by="coh")
