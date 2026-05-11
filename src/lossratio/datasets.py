"""Built-in synthetic datasets for examples and tests.

The package ships a pre-baked synthetic experience dataset
(:func:`load_experience`) generated via :func:`make_experience` with
seed 20260501. The generator mirrors R `lossratio`'s
`data-raw/make_experience.R` — same calibration constants, same regime
shift, same recipe — but cell values are NOT bit-identical to R because
numpy's RNG differs from R's `rnorm` even with the same seed.

Calibration scalars (target LR, premium volume, cell noise CV) per
coverage were measured once on a real long-term Korean health-
insurance portfolio and are baked in here as constants so this module
ships without any real-data dependency. SUR carries a regime shift at
cohort 18 (2024-07): target LR is scaled by 0.6 to mimic the real
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

_DATA_PATH = Path(__file__).parent / "data" / "experience.parquet"

# --- Calibration constants (per coverage) -------------------------------
#
# Coverage codes (letter-first uppercase, valid bare identifiers):
#   CI   The two major non-cancer critical illnesses:
#          - cerebrovascular disease (stroke, cerebral infarction,
#            cerebral haemorrhage)
#          - ischemic heart disease (angina, acute myocardial
#            infarction)
#        Does NOT include cancer; cancer is the separate `CAN` coverage.
#   CAN  Cancer
#   HOS  Hospitalisation (per-day fixed benefit)
#   SUR  Surgery (per-event fixed benefit)
_CALIB: list[tuple[str, float, float, float, float]] = [
    # (coverage, target_lr, prem_mean,   prem_cv,   cell_cv)
    ("CI",       0.6041798, 490_082_826, 0.9332768, 1.3679838),
    ("CAN",      0.4966633, 403_465_899, 0.8684393, 1.6660074),
    ("HOS",      0.3533962,  32_725_571, 0.8545352, 0.8603264),
    ("SUR",      1.4291995, 704_738_057, 0.6738675, 0.3589258),
]

# Single regime shift on SUR at cohort idx 18 (2024-07): scale target
# LR by 0.6 (1.43 -> ~0.86).
_SHIFTS: dict[str, tuple[int, float]] = {"SUR": (18, 0.60)}

_DEFAULT_SEED = 20260501
_N_COHORTS = 36
_K = 36
_MAX_CYM_IDX = _N_COHORTS - 1


def _make_weights() -> np.ndarray:
    """Constant per-dev weights with a small dev-1 dampening that
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
    * Up to 36 development months per cohort, jagged triangle shape.
    * Four coverages keyed by ``coverage`` (``CI``, ``CAN``, ``HOS``,
      ``SUR``).
    * ``SUR`` carries a planted regime shift at cohort 2024-07 (cohort
      idx 18): target cumulative LR drops to roughly 0.6× the
      pre-break level, reflecting the underwriting tightening in the
      real portfolio. The other three coverages are stable.

    Parameters
    ----------
    seed : int, default 20260501
        Seed for numpy's default RNG. Determinism is per-numpy-version;
        cell values vary with numpy releases that change the
        :class:`~numpy.random.Generator` defaults.

    Returns
    -------
    polars.DataFrame
        Columns ``coverage`` (str), ``uy_m`` (str, ISO date), ``cy_m``
        (str, ISO date), ``dev_m`` (int), ``loss_incr`` (float),
        ``premium_incr`` (float). Pass directly to :class:`Triangle`
        with ``group_var="coverage"``.
    """
    rng = np.random.default_rng(seed)
    weights = _make_weights()

    records: list[dict] = []
    for coverage, target_lr, prem_mean, prem_cv, cell_cv in _CALIB:
        prem_mean_per_dev = prem_mean / _K
        shift = _SHIFTS.get(coverage)
        shift_at = shift[0] if shift else None
        shift_scale = shift[1] if shift else 1.0

        for ci in range(_N_COHORTS):
            cohort_mult = math.exp(rng.normal(0.0, prem_cv))
            prem_base = prem_mean_per_dev * cohort_mult
            eff_target = target_lr * (
                shift_scale if shift_at is not None and ci >= shift_at else 1.0
            )

            cy_u, cm_u = divmod(ci, 12)
            uy_m = _ymd(2023 + cy_u, cm_u + 1)

            for k in range(_K):
                if ci + k > _MAX_CYM_IDX:
                    break
                cy_c, cm_c = divmod(ci + k, 12)
                cy_m = _ymd(2023 + cy_c, cm_c + 1)

                incr_premium = prem_base * (1.0 + rng.normal(0.0, 0.05))
                incr_premium = max(incr_premium, 0.0)

                noise = math.exp(rng.normal(0.0, math.log(1.0 + cell_cv)))
                incr_loss = incr_premium * eff_target * weights[k] * _K * noise

                records.append(
                    {
                        "coverage":     coverage,
                        "uy_m":         uy_m,
                        "cy_m":         cy_m,
                        "dev_m":        k + 1,
                        "loss_incr":    incr_loss,
                        "premium_incr": incr_premium,
                    }
                )

    return pl.DataFrame(records)


def load_experience() -> pl.DataFrame:
    """Load the pre-baked synthetic experience dataset.

    Equivalent to ``make_experience(seed=20260501)`` but reads a
    parquet shipped with the package — fast, deterministic across
    numpy versions, and shared with R `lossratio`'s `data(experience)`
    in *recipe* (not bit-identical due to RNG differences).

    Returns
    -------
    polars.DataFrame
        Same shape as :func:`make_experience`.
    """
    return pl.read_parquet(_DATA_PATH)
