"""Built-in synthetic datasets for examples and tests.

The package ships a pre-baked synthetic experience dataset
(:func:`load_experience`) generated via :func:`make_experience` with
seed 20260501. The generator uses fixed per-coverage calibration
constants and a planted regime shift; cell values depend on numpy's
RNG and so vary with the numpy version even at a fixed seed.

Each coverage is decomposed across ``age_band`` x ``channel`` so the
demo exercises the all / coverage / age / channel views with real data.
The decomposition is *marginal-preserving*: premium-mix weights split
the coverage premium, and the relative loss-ratio factors are normalised so the
premium-weighted average over the mix is 1 -- aggregating age x channel
back up reproduces the coverage-level target Ratio.

Calibration scalars (target Ratio, premium volume, cell noise CV) per
coverage were measured once on a real long-term Korean health-
insurance portfolio and are baked in here as constants so this module
ships without any real-data dependency. SURGERY carries a regime shift at
cohort 18 (2024-07): target Ratio is scaled by 0.6 to mimic the real
portfolio's underwriting tightening.

To regenerate the shipped parquet (e.g., after touching the recipe)::

    import lossratio as lr
    lr.make_experience().write_parquet(
        "src/lossratio/data/experience.parquet"
    )
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
import polars as pl

from ._period import derive_grain_columns

_DATA_PATH = Path(__file__).parent / "data" / "experience.parquet"

# --- Calibration constants (per coverage) -------------------------------
#
# Coverage codes (letter-first uppercase, valid bare identifiers):
#   CI   The two major non-cancer critical illnesses:
#          - cerebrovascular disease (stroke, cerebral infarction,
#            cerebral haemorrhage)
#          - ischemic heart disease (angina, acute myocardial
#            infarction)
#        Does NOT include cancer; cancer is the separate `CANCER` coverage.
#   CANCER     Cancer
#   INPATIENT  Hospitalisation (inpatient, per-day fixed benefit)
#   SURGERY    Surgery (per-event fixed benefit)
_CALIB: list[tuple[str, float, float, float, float]] = [
    # (coverage,  target_ratio, premium_mean,   premium_cv,   cell_cv)
    ("CI",        0.6041798, 490_082_826, 0.9332768, 1.3679838),
    ("CANCER",    0.4966633, 403_465_899, 0.8684393, 1.6660074),
    ("INPATIENT", 0.3533962,  32_725_571, 0.8545352, 0.8603264),
    ("SURGERY",   1.4291995, 704_738_057, 0.6738675, 0.3589258),
]

# Single regime shift on SURGERY at cohort idx 18 (2024-07): scale target
# Ratio by 0.6 (1.43 -> ~0.86).
_SHIFTS: dict[str, tuple[int, float]] = {"SURGERY": (18, 0.60)}

# --- Segment dimensions: age_band x channel -----------------------------
#
# Each coverage's premium is split across age x channel by the mix
# weights (each set sums to 1); the relative loss-ratio factors are normalised
# (see `_normalised`) so the premium-weighted average over the mix is 1,
# making the decomposition marginal-preserving at the coverage level.
_AGE_BANDS: tuple[str, ...] = ("20s", "30s", "40s", "50s", "60s", "70+")
_AGE_W: tuple[float, ...] = (0.18, 0.22, 0.23, 0.17, 0.13, 0.07)  # premium mix
# younger -> higher loss ratio: Korean health insurance prices elderly steeply, so
# the 20s portfolio loss ratio lands well above 100% while 70+ stays low.
_AGE_RATIO: tuple[float, ...] = (2.00, 1.45, 1.00, 0.75, 0.55, 0.40)

_CHANNELS: tuple[str, ...] = ("FC", "GA", "TM", "ON")
_CHAN_W: tuple[float, ...] = (0.40, 0.30, 0.20, 0.10)  # premium mix
_CHAN_RATIO: tuple[float, ...] = (0.90, 1.15, 1.05, 0.95)  # GA least-curated -> higher loss ratio

_DEFAULT_SEED = 20260501
_N_COHORTS = 36
_K = 36
_MAX_CYM_IDX = _N_COHORTS - 1


def _normalised(
    weights: tuple[float, ...], factors: tuple[float, ...]
) -> tuple[float, ...]:
    """Scale `factors` so the `weights`-weighted average is exactly 1."""
    m = sum(w * f for w, f in zip(weights, factors))
    return tuple(f / m for f in factors)


_AGE_RATIO_N = _normalised(_AGE_W, _AGE_RATIO)
_CHAN_RATIO_N = _normalised(_CHAN_W, _CHAN_RATIO)


def _make_weights() -> np.ndarray:
    """Constant per-duration weights with a small duration-1 dampening that
    mimics the waiting-period dip in real long-term health data."""
    weights = np.ones(_K)
    weights[0] = 0.2
    return weights / weights.sum()


def _ymd(year: int, month: int) -> str:
    return f"{year}-{month:02d}-01"


def make_experience(seed: int = _DEFAULT_SEED) -> pl.DataFrame:
    """Generate synthetic experience data.

    Layout:

    * 36 monthly cohorts: 2023-01 to 2025-12.
    * Up to 36 duration months per cohort, jagged triangle shape.
    * Four coverages (``CI``, ``CANCER``, ``INPATIENT``, ``SURGERY``), each split
      across six ``age_band`` x four ``channel`` segments (96 segments).
    * ``SURGERY`` carries a planted regime shift at cohort 2024-07 (cohort
      idx 18): target cumulative Ratio drops to roughly 0.6x the
      pre-break level. The other three coverages are stable.

    The age x channel decomposition is marginal-preserving: summing the
    segments back to ``coverage`` reproduces the per-coverage premium
    volume and target Ratio.

    Parameters
    ----------
    seed : int, default 20260501
        Seed for numpy's default RNG. Determinism is per-numpy-version;
        cell values vary with numpy releases that change the
        :class:`~numpy.random.Generator` defaults.

    Returns
    -------
    polars.DataFrame
        17 columns: the segment keys ``coverage`` / ``age_band`` /
        ``channel`` (str); the underwriting axis ``uy`` / ``uy_h`` /
        ``uy_q`` / ``uy_m`` (Date); the calendar axis ``cy`` / ``cy_h``
        / ``cy_q`` / ``cy_m`` (Date); the duration axis ``duration_y`` /
        ``duration_h`` / ``duration_q`` / ``duration_m`` (int); and ``incr_loss`` /
        ``incr_premium``. Pass directly to :class:`Triangle` with
        ``groups="coverage"`` (or ``"age_band"`` / ``"channel"``).
    """
    rng = np.random.default_rng(seed)
    weights = _make_weights()

    records: list[dict] = []
    for coverage, target_ratio, premium_mean, premium_cv, cell_cv in _CALIB:
        premium_mean_per_duration = premium_mean / _K
        shift = _SHIFTS.get(coverage)
        shift_at = shift[0] if shift else None
        shift_scale = shift[1] if shift else 1.0

        for ci in range(_N_COHORTS):
            # One premium-volume draw per (coverage, cohort); segments
            # split it by the mix weights so the coverage total is
            # preserved.
            cohort_mult = math.exp(rng.normal(0.0, premium_cv))
            premium_base = premium_mean_per_duration * cohort_mult
            eff_target = target_ratio * (
                shift_scale if shift_at is not None and ci >= shift_at else 1.0
            )

            cy_u, cm_u = divmod(ci, 12)
            uy_m = _ymd(2023 + cy_u, cm_u + 1)

            for ai, age in enumerate(_AGE_BANDS):
                for chi, channel in enumerate(_CHANNELS):
                    seg_premium_base = premium_base * _AGE_W[ai] * _CHAN_W[chi]
                    seg_ratio = eff_target * _AGE_RATIO_N[ai] * _CHAN_RATIO_N[chi]

                    for k in range(_K):
                        if ci + k > _MAX_CYM_IDX:
                            break
                        cy_c, cm_c = divmod(ci + k, 12)
                        cy_m = _ymd(2023 + cy_c, cm_c + 1)

                        incr_premium = seg_premium_base * (
                            1.0 + rng.normal(0.0, 0.05)
                        )
                        incr_premium = max(incr_premium, 0.0)

                        noise = math.exp(
                            rng.normal(0.0, math.log(1.0 + cell_cv))
                        )
                        incr_loss = (
                            incr_premium * seg_ratio * weights[k] * _K * noise
                        )

                        # Real-world premium / loss are recorded in won
                        # (integer).
                        records.append(
                            {
                                "coverage":     coverage,
                                "age_band":     age,
                                "channel":      channel,
                                "uy_m":         uy_m,
                                "cy_m":         cy_m,
                                "duration_m":        k + 1,
                                "incr_loss":    round(incr_loss),
                                "incr_premium": round(incr_premium),
                            }
                        )

    df = pl.DataFrame(records).with_columns(
        pl.col("uy_m").str.to_date(),
        pl.col("cy_m").str.to_date(),
    )

    # Enrich to the full M/Q/H/Y grain schema, then order columns.
    df = derive_grain_columns(df)
    return df.select(
        "coverage", "age_band", "channel",
        "uy", "uy_h", "uy_q", "uy_m",
        "cy", "cy_h", "cy_q", "cy_m",
        "duration_y", "duration_h", "duration_q", "duration_m",
        "incr_loss", "incr_premium",
    )


def load_experience() -> pl.DataFrame:
    """Load the pre-baked synthetic experience dataset.

    Equivalent to ``make_experience(seed=20260501)`` but reads a
    parquet shipped with the package — fast and deterministic across
    numpy versions.

    Returns
    -------
    polars.DataFrame
        Same 17-column shape as :func:`make_experience`.
    """
    return pl.read_parquet(_DATA_PATH)
