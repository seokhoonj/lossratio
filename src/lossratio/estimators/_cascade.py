"""Shared segment_wise (multi-regime) cascade primitives.

The orchestration pieces that the loss-side cascade (``loss.py``) and the
premium-side cascade (``premium.py``) both use: derive a segment's change dates,
produce a pooled level-invariant link-ratio donor over a cohort subset, pad a
matrix to the segment's global depth, and row-stack the per-regime fits into one
segment-level fit dict.

These are pure functions (numpy / polars / the ``recursion`` kernel only) -- no
estimator import -- so both ``loss.py`` and ``premium.py`` can share them without
a cycle.
"""
from __future__ import annotations

from typing import Any

import numpy as np
import polars as pl

from .._kernels.recursion import (
    fit_multiplicative,
    make_value_matrices,
    multiplicative_var,
)

# the loss cascade's stacked keys (the default for `_stack_cascade_fits`).
_LOSS_CASCADE_KEYS = [
    "loss_obs", "loss_proj", "premium_obs", "premium_proj",
    "proc_se", "param_se", "total_se", "grafted",
]


def _pad_cols(mat: np.ndarray, n_cols: int) -> np.ndarray:
    """Right-pad a matrix to ``n_cols`` columns with NaN (no-op if already wide
    enough)."""
    if mat.shape[1] >= n_cols:
        return mat
    pad = np.full((mat.shape[0], n_cols - mat.shape[1]), np.nan)
    return np.hstack([mat, pad])


def _cohort_subset_donor(
    seg_sub: pl.DataFrame, sigma_method: str, value: str,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Pooled level-invariant link ratio over an already-filtered cohort subset.

    The cascade (``segment_wise``) donor: ``segment_wise`` fits each regime on
    its own cohorts, and lends the deep durations from the link ratio ``f_k`` of
    the OLDER (deeper) regimes pooled -- their data-rich cohorts reach the depth
    a younger regime cannot. ``f_k`` cancels the level, so only shape is lent.
    The frame is already restricted to the donor cohorts (``value="loss"``
    cumulates ``incr_loss``; ``"premium"`` is already cumulative). Returns
    ``(f_k, sigma2_f_k, var_f_k)``.
    """
    sub = seg_sub.sort(["cohort", "duration"])
    if value == "loss":
        sub = sub.with_columns(
            pl.col("incr_loss").cum_sum().over("cohort").alias("loss")
        )
    (mat,), _, _ = make_value_matrices(sub, value_cols=(value,))
    mr = fit_multiplicative(mat, sigma_method=sigma_method)
    return (mr.f_k, mr.sigma2_k, multiplicative_var(mr))


def _segment_change_dates(regime: Any, group_value: Any) -> list:
    """The sorted change dates that apply to one segment (for ``segment_wise``).

    Reads the regime's own ``_changes_df`` and, when grouped, keeps only the
    rows matching this segment's group value (the group columns are read off the
    change frame itself). Derives the regime partition from the change DATES
    directly (not ``regime_id``), so it works the same for a hand-built
    ``Regime(change=...)`` and an auto-detected regime. A segment with no change
    returns ``[]`` -- the cascade then degenerates to a single plain fit.
    """
    changes = getattr(regime, "_changes_df", None)
    if changes is None or changes.is_empty():
        return []
    change_group_cols = [c for c in changes.columns if c not in ("change", "regime_id")]
    if change_group_cols:
        vals = group_value if isinstance(group_value, tuple) else (group_value,)
        keymap = dict(zip(change_group_cols, vals, strict=False))
        for g in change_group_cols:
            if g in keymap:
                changes = changes.filter(pl.col(g) == keymap[g])
    return sorted(changes.get_column("change").to_list())


def _stack_cascade_fits(
    parts: list[dict], mechanism: str, *,
    keys: list[str] | None = None,
    n_rows_key: str = "loss_obs",
) -> dict[str, Any]:
    """Row-stack the per-regime fits into one segment-level fit dict.

    Each regime's matrices are already widened to the segment's global horizon
    (same column count), and the regimes are date-disjoint with ascending
    cohorts, so a row-stack yields a single rectangular cohort x duration grid
    in globally-ascending cohort order -- exactly what the long-frame assembly
    expects. ``g_k`` / ``f_k`` are per-link (not per-cohort) and unused
    downstream, so they are dropped. ``psi`` is carried PER COHORT (each
    cohort's own regime's between-cohort variance).

    ``keys`` selects the stacked value columns (default = the loss cascade keys);
    the premium cascade passes its own (no ``loss_*`` columns). ``n_rows_key`` is
    the per-part column whose row count sizes the per-cohort ``psi`` vector.
    """
    if keys is None:
        keys = _LOSS_CASCADE_KEYS
    out: dict[str, Any] = {k: np.vstack([p[k] for p in parts]) for k in keys}
    if mechanism in ("credible", "smooth"):
        out["u"] = np.concatenate([np.asarray(p["u"]) for p in parts])
        out["Z"] = np.concatenate([np.asarray(p["Z"]) for p in parts])
        out["psi"] = np.concatenate(
            [np.full(p[n_rows_key].shape[0], float(p["psi"])) for p in parts]
        )
    if mechanism == "smooth":
        out["representable"] = all(bool(p["representable"]) for p in parts)
        out["smooth_converged"] = all(bool(p["smooth_converged"]) for p in parts)
    return out
