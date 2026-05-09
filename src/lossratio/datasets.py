"""Built-in synthetic datasets for examples and tests."""

from __future__ import annotations

import numpy as np
import polars as pl


_COVERAGES = (
    # (cv_nm, base_target_lr, regime_shift)
    # regime_shift is None or (cohort_idx, post_break_target_lr)
    ("2CI", 0.40, None),
    ("CAN", 0.65, None),
    ("HOS", 0.55, None),
    ("SUR", 0.50, (18, 0.30)),
)


def load_experience(seed: int = 20260501) -> pl.DataFrame:
    """Synthetic long-format experience for the package's examples.

    Layout:

    * 36 monthly cohorts: 2024-01 to 2026-12.
    * Up to 36 development months (3 years) of observation per cohort.
    * "Today" is the last day of 2026-12, so each coverage produces the
      usual jagged triangle: cohort 2024-01 has 36 dev observed and
      cohort 2026-12 has only 1.
    * Four coverages keyed by ``cv_nm``, mirroring the Korean long-term
      health insurance product structure:

      - ``2CI`` — two major critical illnesses (cancer + acute cerebro-
        cardiovascular events such as stroke and heart attack), a
        common Korean rider name.
      - ``CAN`` — cancer-only rider (Korean insurers commonly sell
        both 2CI and a separate cancer rider on the same policy).
      - ``HOS`` — hospitalisation per-day fixed benefit.
      - ``SUR`` — surgery per-event fixed benefit.

      Each coverage shares the same runoff shape — a small dev-1
      "waiting-period" dip followed by exponential decay — and a base
      premium of 100, but starts from a different target cumulative
      loss ratio.
    * ``SUR`` has a single regime shift at cohort 2025-07 — its target
      cumulative LR drops from 0.50 to 0.30. The other three coverages
      are stable.

    The data has no real-portfolio dependency and is intended only as a
    friendly starting point for users exploring the package.

    Returns
    -------
    polars.DataFrame
        Columns ``cv_nm`` (str), ``cym`` (date string), ``uym`` (date
        string), ``loss_incr`` (float), ``premium_incr`` (float). Pass
        it directly to :class:`Experience`; pass ``group_var="cv_nm"``
        to :meth:`Experience.triangle` for a per-coverage triangle.
    """
    rng = np.random.default_rng(seed)
    n_cohorts, K, premium = 36, 36, 100.0
    max_cym_idx = n_cohorts - 1

    # Runoff: roughly constant incremental loss per dev after a small
    # dev-1 dampening that mimics the waiting-period dip in real
    # long-term health data. The resulting cumulative LR rises
    # monotonically toward the target, as it does in real portfolios,
    # and the f_{1->2} link factor lands in the 4-6 range.
    weights = np.ones(K)
    weights[0] = 0.2
    weights /= weights.sum()

    records = []
    for cv_nm, base_lr, shift in _COVERAGES:
        for ci in range(n_cohorts):
            target_lr = base_lr
            if shift is not None and ci >= shift[0]:
                target_lr = shift[1]
            total_loss = target_lr * K * premium
            cy_u, cm_u = divmod(ci, 12)
            uym = f"{2024 + cy_u}-{cm_u + 1:02d}-01"
            for k in range(K):
                if ci + k > max_cym_idx:
                    break
                cy_c, cm_c = divmod(ci + k, 12)
                cym = f"{2024 + cy_c}-{cm_c + 1:02d}-01"
                incr_loss = (
                    total_loss * weights[k] * float(rng.normal(1.0, 0.10))
                )
                records.append({
                    "cv_nm": cv_nm,
                    "cym": cym,
                    "uym": uym,
                    "loss_incr": float(incr_loss),
                    "premium_incr": premium,
                })
    return pl.DataFrame(records)
