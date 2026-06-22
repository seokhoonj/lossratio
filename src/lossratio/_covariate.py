"""Covariate fixed-effect intensity kernel (planned ``covariates=`` feature).

The credible / smooth loss rungs model an incremental-loss intensity
``E[dLoss] = exposure * g_k`` (``g_k`` = per-duration intensity, exposure =
predetermined cumulative premium, log-offset). A ``covariate`` adds cell-level
fixed effects ``X'beta`` to that log-mean:

    E[dLoss_cell] = exposure_cell * exp( s_k + X_cell' beta )

with ``s_k`` the *saturated* per-duration intercept (one free intercept per
observed duration, UNpenalized -- it carries the pooled shape) and ``beta`` the
covariate log-relativities (treatment-coded against a reference level per
covariate, RIDGE-penalized by ``lam`` so sparse / separated high-cardinality
levels shrink toward the reference instead of running to +/-inf).

The estimation is the kept penalized quasi-Poisson IRLS
(:func:`lossratio._smooth.penalized_irls`): the design is
``B = [duration one-hot | covariate dummies]`` and the penalty is the identity
on the covariate block, zero on the duration block. With NO covariates the
design is the saturated duration one-hot and ``exp(s_k) = sum(dLoss_k) /
sum(exposure_k)`` -- exactly :func:`lossratio._engine.saturated_intensity`, so
the covariate path nests the current pooled intensity cell-for-cell.

This module is the standalone, separately-tested numeric core; the
``CredibleLoss`` / ``SmoothLoss`` integration (design assembly from a
disaggregated source frame, the credibility level on the covariate-adjusted
mean, the marginalized projection) is wired on top of it.
"""
from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from ._smooth import penalized_irls


@dataclass
class CovariateFit:
    """Result of :func:`fit_covariate_intensity`.

    ``durations`` are the sorted observed from-durations; ``s`` is the
    per-duration intercept aligned to them (``g_k`` at the reference covariate
    cell = ``exp(s)``). ``levels`` maps each covariate name to its level list
    (the first is the dropped reference); ``beta`` maps ``(covariate, level)`` to
    its log-relativity (the reference level is implicitly 0.0 and absent from the
    dict). ``converged`` is the IRLS flag.
    """

    durations: list[int]
    s: np.ndarray
    levels: "dict[str, list]"
    beta: "dict[tuple[str, object], float]"
    converged: bool

    def intensity(self, duration: int, cell: "dict[str, object] | None" = None) -> float:
        """``g_k(x) = exp(s_k + sum_c beta[c, x_c])`` for a covariate cell ``x``
        (a ``{covariate: level}`` mapping; missing / reference levels contribute
        0). Returns ``nan`` for an unobserved duration."""
        idx = {d: i for i, d in enumerate(self.durations)}
        if duration not in idx:
            return float("nan")
        eta = float(self.s[idx[duration]])
        if cell:
            for c, lv in cell.items():
                eta += self.beta.get((c, lv), 0.0)
        return float(np.exp(eta))


def _covariate_design(
    duration: np.ndarray,
    covariates: "dict[str, np.ndarray]",
) -> "tuple[np.ndarray, np.ndarray, list[int], dict[str, list], list[tuple[str, object]]]":
    """Build ``B = [duration one-hot | covariate treatment dummies]`` and the
    ridge penalty mask (0 on the saturated duration block, 1 on the covariate
    block).

    Returns ``(B, penalty_diag, durations, levels, beta_cols)`` where
    ``durations`` indexes the duration columns, ``levels`` maps each covariate
    to its sorted level list (reference = first), and ``beta_cols`` labels the
    covariate columns ``(covariate, level)`` in design order.
    """
    n = duration.size
    durations = sorted(int(d) for d in np.unique(duration))
    dcol = {d: j for j, d in enumerate(durations)}
    Xd = np.zeros((n, len(durations)), dtype=np.float64)
    Xd[np.arange(n), [dcol[int(d)] for d in duration]] = 1.0

    blocks = [Xd]
    levels: "dict[str, list]" = {}
    beta_cols: "list[tuple[str, object]]" = []
    for name, codes in covariates.items():
        uniq = sorted(np.unique(codes).tolist(), key=lambda v: str(v))
        levels[name] = uniq
        for lv in uniq[1:]:                       # drop the first level (reference)
            blocks.append((codes == lv).astype(np.float64)[:, None])
            beta_cols.append((name, lv))

    B = np.hstack(blocks)
    penalty_diag = np.concatenate([
        np.zeros(len(durations)),                 # saturated duration block: unpenalized
        np.ones(len(beta_cols)),                  # covariate block: ridge-shrunk
    ])
    return B, penalty_diag, durations, levels, beta_cols


def fit_covariate_intensity(
    response: np.ndarray,
    exposure: np.ndarray,
    duration: np.ndarray,
    covariates: "dict[str, np.ndarray]",
    *,
    lam: float = 1.0,
) -> CovariateFit:
    """Penalized quasi-Poisson log-link GLM of the covariate-adjusted intensity.

    ``response`` = per-cell incremental loss, ``exposure`` = per-cell from-cell
    cumulative premium (> 0), ``duration`` = per-cell from-duration,
    ``covariates`` = ``{name: per-cell level codes}`` (empty dict = pooled
    intensity, exact nesting). ``lam`` is the ridge strength on the covariate
    coefficients (the duration shape is never penalized). Cells with
    non-positive exposure are dropped.
    """
    response = np.asarray(response, dtype=np.float64)
    exposure = np.asarray(exposure, dtype=np.float64)
    duration = np.asarray(duration)
    keep = np.isfinite(exposure) & (exposure > 0) & np.isfinite(response)
    if not keep.all():
        response, exposure, duration = response[keep], exposure[keep], duration[keep]
        covariates = {k: np.asarray(v)[keep] for k, v in covariates.items()}
    else:
        covariates = {k: np.asarray(v) for k, v in covariates.items()}

    B, penalty_diag, durations, levels, beta_cols = _covariate_design(
        duration, covariates
    )
    penalty = np.diag(penalty_diag)
    fit = penalized_irls(
        response, np.log(exposure), B, penalty, float(lam),
    )
    n_dur = len(durations)
    s = fit.beta[:n_dur]
    beta = {col: float(fit.beta[n_dur + j]) for j, col in enumerate(beta_cols)}
    return CovariateFit(
        durations=durations, s=s, levels=levels, beta=beta,
        converged=bool(fit.converged),
    )
