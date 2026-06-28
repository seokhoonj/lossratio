"""Per-segment credibility reporting assembler (charter Sec.3.x).

``_segment_credibility_df`` assembles the per-cohort credibility weights of a
fitted segment into the long reporting frame. It is a Polars assembly helper
(estimator-layer reporting), not a numeric kernel, so it sits in ``estimators``
beside the fit cores rather than in ``_kernels``.
"""

from __future__ import annotations

from typing import Any

import numpy as np
import polars as pl

from .._kernels.io import fill_group_columns, normalize_groups


def _segment_credibility_df(
    fit: dict[str, np.ndarray],
    cohorts: list,
    groups: "str | list[str] | None",
    group_value: Any | None,
) -> pl.DataFrame:
    """One segment's per-cohort credibility diagnostics (CredibleLoss only).

    Columns ``[groups?, cohort, u, Z, psi]``: ``u`` the cohort credibility
    level (mean-1 scaling on the pooled intensity), ``Z`` its credibility
    weight, and ``psi`` the segment's between-cohort variance (constant per
    segment). ``u = 1`` / ``Z = 0`` is the pooled-collapse cohort.
    """
    n = len(cohorts)
    data: dict[str, Any] = {}
    if groups is not None:
        fill_group_columns(data, groups, group_value, n)
    data["cohort"] = list(cohorts)
    data["u"] = fit["u"].tolist()
    data["Z"] = fit["Z"].tolist()
    # psi is one scalar per segment, except the segment_wise cascade carries it
    # per cohort (each cohort's own regime's between-cohort variance).
    psi = fit["psi"]
    data["psi"] = (
        [float(psi)] * n if np.ndim(psi) == 0
        else np.asarray(psi, dtype=float).tolist()
    )
    df = pl.DataFrame(data)
    order = (["cohort", "u", "Z", "psi"] if groups is None
             else [*normalize_groups(groups), "cohort", "u", "Z", "psi"])
    return df.select(order)
