"""Smooth-mode intensity engine -- penalized IRLS P-spline.

The second of the two intensity modes. Where the saturated mode
(``engine.saturated_intensity``) is the closed-form ``g_k = sum y / sum P``
frozen bit-for-bit by the oracle, the smooth mode fits a penalized B-spline
shape ``s(k)`` by log-link quasi-Poisson IRLS so the duration curve
``g_k = exp(s(k))`` is regularised instead of free-per-duration:

    eta_cell = s(duration) + log(P_cell),   mu = exp(eta),   Var = phi * mu
    s(k)     = B(k) . beta,    penalty  lambda * ||D2 beta||^2   (2nd diff)

This module is the SHAPE BLOCK of the GLMM: it takes the
pooled increments (``u = 1``) and returns the smooth ``g_k``; the per-cohort
credibility level ``u_i`` rides the same dispersion-scaled conjugate the
saturated rung already uses (``loss_fit._credible_levels``). Segment random
levels and exogenous covariates are out of v1 scope, so the only thing
distinguishing the smooth rung from
the credible rung is this shape.

Golden-anchor tie: a one-hot basis with no penalty reduces
this penalized GLM to the saturated per-duration fit ``g_k = sum y / sum P``,
i.e. the same number the frozen ``saturated_intensity`` returns -- the smooth
mode's reduction to the only golden surface.

Non-representable policy: log-link cannot express a
non-positive mean, so a group whose total response is ``<= 0`` is a boundary
fit (``representable = False``, no IRLS); a duration whose pooled increment is
non-positive is clamped at the ``eta`` floor and reported, not silently fit.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
from scipy.interpolate import BSpline


# ---------------------------------------------------------------------------
# Basis + penalty
# ---------------------------------------------------------------------------


def bspline_design(
    duration: np.ndarray, n_basis: int, degree: int = 3
) -> tuple[np.ndarray, np.ndarray]:
    """Clamped B-spline design matrix + 2nd-difference penalty.

    ``duration`` is the per-cell 1-based duration index. The basis spans
    ``[min, max]`` of the observed durations with ``n_basis`` clamped B-spline
    functions of the given ``degree``; returns ``(B, D2'D2)`` where ``B`` is
    ``(n_cells, n_basis)`` and the penalty is the ``n_basis x n_basis``
    second-difference matrix ``D2' D2`` (the P-spline / Eilers-Marx penalty:
    the knots are generous and fixed, the penalty does the smoothing)."""
    d = np.asarray(duration, dtype=np.float64)
    lo, hi = float(d.min()), float(d.max())
    if hi <= lo:
        # a single distinct duration: a one-column intercept basis
        B = np.ones((d.size, 1), dtype=np.float64)
        return B, np.zeros((1, 1), dtype=np.float64)
    n_interior = n_basis - degree - 1
    if n_interior < 0:
        raise ValueError(
            f"n_basis ({n_basis}) must be >= degree + 1 ({degree + 1})"
        )
    interior = np.linspace(lo, hi, n_interior + 2)[1:-1]
    knots = np.r_[[lo] * (degree + 1), interior, [hi] * (degree + 1)]
    B = BSpline.design_matrix(
        np.clip(d, lo, hi), knots, degree, extrapolate=False
    ).toarray()
    # 2nd-order difference penalty on the coefficient vector
    D2 = np.diff(np.eye(n_basis), n=2, axis=0)
    return B, D2.T @ D2


def onehot_design(duration: np.ndarray) -> tuple[np.ndarray, np.ndarray, list[int]]:
    """One-hot (saturated) basis over the distinct durations + zero penalty.

    Returns ``(B, P, durs)`` where ``B[c, j] = 1`` iff cell ``c`` is at the
    ``j``-th distinct duration ``durs[j]``, and ``P`` is the zero penalty.
    With ``lambda = 0`` the penalized GLM on this basis reduces to the
    saturated per-duration fit (the golden-anchor reduction)."""
    durs = sorted(set(int(x) for x in duration))
    idx = {k: j for j, k in enumerate(durs)}
    B = np.zeros((len(duration), len(durs)), dtype=np.float64)
    for c, k in enumerate(duration):
        B[c, idx[int(k)]] = 1.0
    return B, np.zeros((len(durs), len(durs)), dtype=np.float64), durs


# ---------------------------------------------------------------------------
# Penalized IRLS
# ---------------------------------------------------------------------------


_ETA_FLOOR = -30.0          # exp(-30) ~ 1e-13: a clamped near-zero mean
_ETA_CEIL = 30.0


@dataclass
class _IRLSResult:
    beta: np.ndarray        # spline coefficients
    eta: np.ndarray         # per-cell linear predictor (incl. offset)
    mu: np.ndarray          # per-cell fitted mean
    edf: float              # effective degrees of freedom (trace of hat)
    pearson: float          # Pearson chi-square sum (y - mu)^2 / mu
    converged: bool
    n_iter: int


def penalized_irls(
    y: np.ndarray,
    offset: np.ndarray,
    B: np.ndarray,
    penalty: np.ndarray,
    lam: float,
    *,
    max_iter: int = 200,
    tol: float = 1e-8,
) -> _IRLSResult:
    """Penalized IRLS for a log-link quasi-Poisson (canonical, ``V = mu``).

    Solves ``(B'WB + lam P) beta = B'W (B beta + (y - mu)/mu)`` to convergence
    in ``max |d eta|``, with step-halving when a step does not reduce the
    penalized Pearson objective and an ``eta`` clamp guarding ``exp`` overflow /
    a singular zero-weight (IRLS guardrails). ``y`` may be
    negative (recoveries are legal -- the quasi-score needs only the first two
    moments); only a non-positive *mean* region is inexpressible and is held at
    the floor."""
    n = y.size
    p = B.shape[1]
    if n == 0:
        # nothing to fit: the convergence check max|d eta| reduces over an empty
        # array (undefined -- no identity), so guard it here and return a
        # degenerate, non-converged fit with NaN coefficients. Callers then
        # produce NaN intensities (a projection gap / degraded status) instead
        # of crashing. Reached when every cell of a segment is filtered out
        # (e.g. a coverage left with no usable links inside a thin train window).
        return _IRLSResult(
            np.full(p, np.nan), y.copy(), y.copy(), 0.0, float("nan"), False, 0
        )
    # seed beta at the crude pooled log-rate so the seed eta is CONSISTENT with
    # beta: B @ (s0 * ones) = s0 (the basis is a partition of unity), so a
    # line-search step toward the Newton point never silently drops the level.
    # (Seeding s0 into eta but leaving beta = 0 was the bug: tiny steps lost s0,
    # so every step looked worse than the seed -> false convergence at beta = 0
    # for wide-range shapes.)
    tot_y, tot_e = float(y.sum()), float(np.exp(offset).sum())
    s0 = np.log(max(tot_y, 1e-6) / tot_e) if tot_e > 0 else 0.0
    beta = np.full(p, s0, dtype=np.float64)
    eta = np.clip(B @ beta + offset, _ETA_FLOOR, _ETA_CEIL)

    def _neg_obj(bx: np.ndarray, ex: np.ndarray) -> float:
        # negative penalized quasi-Poisson log-likelihood (h-likelihood
        # shape block): minimize -Sum[y*eta - exp(eta)] +
        # (lam/2) beta'P beta. Concave loglik -> IRLS is monotone Newton; the
        # penalty gradient lam*P*beta matches the `lam * penalty` in the solve.
        return (-float(np.sum(y * ex - np.exp(ex)))
                + 0.5 * lam * float(bx @ penalty @ bx))

    obj = _neg_obj(beta, eta)
    converged = False
    it = 0
    for it in range(1, max_iter + 1):
        mu = np.exp(eta)
        w = mu                                   # IRLS weight = (dmu/deta)^2/V = mu
        z = (eta - offset) + (y - mu) / mu       # working response on the s scale
        BtW = B.T * w                            # (p, n)
        lhs = BtW @ B + lam * penalty
        rhs = BtW @ z
        try:
            beta_new = np.linalg.solve(lhs, rhs)
        except np.linalg.LinAlgError:
            beta_new = np.linalg.lstsq(lhs, rhs, rcond=None)[0]
        # backtracking line search along the (ascent) Newton direction: accept
        # only a step that does NOT worsen the objective. A worse iterate is
        # never kept -- for the concave objective, failing to find any ascent
        # step means we are already at the optimum.
        step = 1.0
        accepted = False
        beta_try = beta_new
        eta_try = np.clip(B @ beta_try + offset, _ETA_FLOOR, _ETA_CEIL)
        while step > 1e-6:
            beta_try = beta + step * (beta_new - beta)
            eta_try = np.clip(B @ beta_try + offset, _ETA_FLOOR, _ETA_CEIL)
            obj_try = _neg_obj(beta_try, eta_try)
            if obj_try <= obj + 1e-10 * abs(obj):
                accepted = True
                break
            step *= 0.5
        if not accepted:
            converged = True                     # no ascent step -> at optimum
            break
        d_eta = float(np.max(np.abs(eta_try - eta)))
        beta, eta, obj = beta_try, eta_try, obj_try
        if d_eta < tol:
            converged = True
            break

    mu = np.exp(eta)
    w = mu
    BtW = B.T * w
    lhs = BtW @ B + lam * penalty
    try:
        hat = np.linalg.solve(lhs, BtW @ B)
        edf = float(np.trace(hat))
    except np.linalg.LinAlgError:
        edf = float(p)
    pearson = float(np.sum((y - mu) ** 2 / mu))
    return _IRLSResult(beta, eta, mu, edf, pearson, converged, it)


# ---------------------------------------------------------------------------
# Public primitive: smooth intensity g_k = exp(s(k))
# ---------------------------------------------------------------------------


@dataclass
class SmoothResult:
    g: dict                 # {duration: g_k = exp(s(k))}
    lam: float              # selected smoothing parameter
    edf: float
    pearson: float
    representable: bool      # False -> boundary fit (sum y <= 0), g all 0
    converged: bool


def _default_n_basis(n_durations: int, degree: int = 3) -> int:
    """Generous fixed knot count (P-spline: the penalty smooths, not the knots),
    bounded so the basis stays well-posed for the duration span."""
    return int(min(n_durations, max(degree + 1, n_durations // 2 + 2)))


def smooth_intensity(
    *,
    response,
    exposure,
    duration,
    n_basis: "int | None" = None,
    lam: "float | str" = "auto",
    lam_grid: "np.ndarray | None" = None,
    degree: int = 3,
) -> SmoothResult:
    """Smooth duration intensity ``g_k = exp(s(k))`` by penalized IRLS.

    ``response`` / ``exposure`` / ``duration`` are parallel per-cell sequences
    (increment ``y``, predetermined premium ``P``, 1-based from-duration). The
    log-link quasi-Poisson penalized P-spline shape is fit on the pooled cells
    (``u = 1``); the per-duration intensity ``g_k = exp(s(k))`` is returned for
    every observed duration.

    ``lam = "auto"`` selects the smoothing parameter by GCV over ``lam_grid``
    (a default geometric grid); a float fixes it. ``n_basis`` defaults to a
    generous fixed count. A group whose total response is ``<= 0`` is a
    boundary fit (``representable = False``, all ``g_k = 0``)."""
    y = np.asarray(response, dtype=np.float64)
    P = np.asarray(exposure, dtype=np.float64)
    dur = np.asarray(duration, dtype=np.int64)

    # filter to fittable cells FIRST (positive, finite exposure + finite
    # response), so every downstream quantity -- the duration key set, the
    # basis sizing, the boundary gate, the prediction range -- is derived from
    # the data actually fit (boundary policy on the real total).
    ok = np.isfinite(P) & (P > 0.0) & np.isfinite(y)
    y, P, dur = y[ok], P[ok], dur[ok]
    durs = sorted(set(int(x) for x in dur))

    if y.size == 0 or float(y.sum()) <= 0.0:
        return SmoothResult(
            g={k: 0.0 for k in durs}, lam=0.0, edf=0.0, pearson=0.0,
            representable=False, converged=True,
        )

    offset = np.log(P)
    nb = n_basis if n_basis is not None else _default_n_basis(len(durs), degree)
    nb = max(degree + 1, min(nb, len(durs)))
    B, penalty = bspline_design(dur, nb, degree)

    if lam != "auto":
        fit = penalized_irls(y, offset, B, penalty, float(lam))
        best_lam = float(lam)
    else:
        # scale the grid to the data: the penalty lam*beta'P beta competes with
        # B'WB (weights ~ mu ~ premium, so 1e5+), so a fixed grid would never
        # bite on large-premium books. Reference scale = tr(B'WB)/tr(P) from an
        # unpenalized fit makes lam dimensionless (light -> heavy smoothing).
        f0 = penalized_irls(y, offset, B, penalty, 0.0)
        BtWB = (B.T * f0.mu) @ B
        tr_p = float(np.trace(penalty))
        scale = (float(np.trace(BtWB)) / tr_p) if tr_p > 0 else 1.0
        grid = (lam_grid if lam_grid is not None
                else scale * np.geomspace(1e-4, 1e4, 13))
        best_gcv = np.inf
        fit = f0
        best_lam = 0.0
        n = y.size
        for lg in grid:
            f = penalized_irls(y, offset, B, penalty, float(lg))
            denom = (n - f.edf)
            gcv = np.inf if denom <= 0 else n * f.pearson / denom ** 2
            if gcv < best_gcv:
                best_gcv, fit, best_lam = gcv, f, float(lg)

    # evaluate g_k = exp(s(k)) at each distinct duration (offset excluded)
    Bk, _ = bspline_design(np.array(durs, dtype=np.int64), nb, degree)
    s_k = Bk @ fit.beta
    g = {k: float(np.exp(s_k[j])) for j, k in enumerate(durs)}
    return SmoothResult(
        g=g, lam=best_lam, edf=fit.edf, pearson=fit.pearson,
        representable=True, converged=fit.converged,
    )
