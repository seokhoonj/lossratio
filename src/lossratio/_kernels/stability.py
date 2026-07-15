"""Loss-ratio development-step primitives.

The cohort x duration matrix build and the per-link ratio development factor
``rho_k = f^L_k / f^P_k`` are shared by the ``Stability`` diagnostic and the
``LossRatio`` go-forward path. They are pure numpy / polars transforms, so they
live here in the kernel layer rather than in ``diagnostics`` (which would make
the estimator reach upward into a diagnostic to reuse them).
"""
from __future__ import annotations

import numpy as np
import polars as pl


def segment_matrices(sub: pl.DataFrame) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """cohort x duration cumulative loss / premium matrices + duration axis."""
    durations = sorted(sub.get_column("duration").unique().to_list())
    cohorts = sub.get_column("cohort").unique().sort().to_list()
    cohort_idx = {c: i for i, c in enumerate(cohorts)}
    duration_idx = {d: j for j, d in enumerate(durations)}
    L = np.full((len(cohorts), len(durations)), np.nan)
    P = np.full((len(cohorts), len(durations)), np.nan)
    for c, d, loss_val, premium_val in zip(
        sub.get_column("cohort").to_list(),
        sub.get_column("duration").to_list(),
        sub.get_column("loss").to_list(),
        sub.get_column("premium").to_list(),
        strict=False,
    ):
        L[cohort_idx[c], duration_idx[d]] = loss_val
        P[cohort_idx[c], duration_idx[d]] = premium_val
    return L, P, np.asarray(durations)


def ratio_step(L: np.ndarray, P: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    """per-link ratio development rho_k = f^L_k / f^P_k (same cohorts each link).

    ``rho_k - 1`` is the loss ratio's fractional change from duration k to k+1;
    ``rho_k -> 1`` means loss and premium are developing in lockstep, i.e. the
    cumulative loss ratio is flat. Returns ``(rho, n_cohorts)`` per link.
    """
    nlink = L.shape[1] - 1
    rho = np.full(nlink, np.nan)
    nco = np.zeros(nlink, dtype=int)
    for k in range(nlink):
        both = (
            ~np.isnan(L[:, k]) & ~np.isnan(L[:, k + 1])
            & ~np.isnan(P[:, k]) & ~np.isnan(P[:, k + 1])
        )
        nco[k] = both.sum()
        sLk, sLk1 = np.nansum(L[both, k]), np.nansum(L[both, k + 1])
        sPk, sPk1 = np.nansum(P[both, k]), np.nansum(P[both, k + 1])
        if sLk > 0 and sPk > 0 and sLk1 > 0 and sPk1 > 0:
            rho[k] = (sLk1 / sLk) / (sPk1 / sPk)
    return rho, nco
