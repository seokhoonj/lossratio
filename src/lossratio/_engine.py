"""Saturated-mode intensity engine primitives (charter Sec.4 / Sec.7-3).

The closed-form (no IRLS) path for the saturated model: one-hot duration
shape, ``psi`` carried by the conjugate level step. This is the redesigned
engine's first surface; the micro-oracle (``tests/test_oracle.py``) freezes
every primitive's output as exact rationals, and this module is its compile
target.

Long-format contract -- each primitive takes parallel per-cell sequences
(``response[c]``, ``exposure[c]``, ``cohort[c]``, ``duration[c]``); duration
-keyed results are ``dict {duration: value}``, cohort-keyed are
``dict {cohort: value}``. ``fitted`` is the per-cell fitted mean aligned to
the input order. Durations are the 1-based index (1, 2, 3, ...) regardless of
grain; the next link of ``k`` is ``k + 1``.

The two exposure aggregates the charter overloads as "m_i" are named apart
here (the oracle flagged the clash): ``A_i = sum(m0/phi)`` is the SHRINKAGE
exposure (drives Z and the conjugate level); ``m_i = (sum m0)^2 / sum(phi*m0)``
is the PSI-MOMENT exposure (drives the Buhlmann-Straub psi estimate). They
coincide only at constant phi.
"""

from __future__ import annotations

import math
from collections.abc import Sequence
from dataclasses import dataclass


def _sum_by(values: Sequence[float], keys: Sequence) -> dict:
    out: dict = {}
    for v, k in zip(values, keys):
        out[k] = out.get(k, 0.0) + v
    return out


def saturated_intensity(*, response, exposure, duration) -> dict:
    """Pooled (u=1) intensity ``g_k = sum response / sum exposure`` per
    duration. ``sum exposure <= 0`` -> ``g_k = 0.0`` (charter degeneracy
    policy); response sign is unconstrained (recoveries allowed)."""
    num = _sum_by(response, duration)
    den = _sum_by(exposure, duration)
    return {k: (num[k] / den[k] if den[k] != 0.0 else 0.0) for k in num}


def fitted_mean(*, g, exposure, duration) -> list:
    """Per-cell fitted mean at ``u = 1``: ``m0 = g_k * exposure``."""
    return [g[k] * p for p, k in zip(exposure, duration)]


def link_ratios(*, response, cohort, duration, include=None) -> dict:
    """Volume-weighted link ratio ``f_k = sum C_{i,k+1} / sum C_{i,k}`` over
    cohorts seen at both ``k`` and ``k+1``, on cumulative loss ``C``. Keyed by
    the lower duration ``k`` (the link ``k -> k+1``).

    ``include`` is an optional parallel boolean sequence (one flag per input
    cell): the cell ``(i, k)`` contributes to ``f_k`` (as the link's SOURCE)
    only when its flag is truthy. The internal cumulation always uses every
    cell -- the gate restricts which links FEED the factor estimate, not which
    cells build ``C``. This is the recent-diagonal fit mask: unlike the
    additive intensity (where masking is just dropping cells from the feed),
    the link ratio cumulates internally and so must gate at the sum step.
    ``None`` (default) is the no-gate path the micro-oracle freezes.

    A cohort enters ``f_k`` only when its SOURCE cumulative ``C_{i,k} > 0`` --
    a zero / negative base carries no defined development ratio, so (as in the
    standard volume-weighted estimate) it is dropped from both sums rather than
    silently inflating the numerator."""
    cell = {(i, k): y for y, i, k in zip(response, cohort, duration)}
    keep = (None if include is None
            else {(i, k) for inc, i, k in zip(include, cohort, duration) if inc})
    cohorts = sorted(set(cohort))
    durs = sorted(set(duration))
    cum: dict = {}
    for i in cohorts:
        run = 0.0
        for k in durs:
            if (i, k) in cell:
                run += cell[(i, k)]
                cum[(i, k)] = run
    f: dict = {}
    for k in durs:
        both = [i for i in cohorts
                if (i, k) in cum and (i, k + 1) in cum and cum[(i, k)] > 0.0
                and (keep is None or (i, k) in keep)]
        if not both:
            continue
        denom = sum(cum[(i, k)] for i in both)
        if denom == 0.0:
            continue
        f[k] = sum(cum[(i, k + 1)] for i in both) / denom
    return f


def pearson_dispersion(*, response, fitted, duration, sigma_method="locf") -> dict:
    """Per-duration Pearson dispersion ``phi_k = sum (y - m0)^2 / m0 / (n_k -
    1)``. Durations with ``edf`` deficit (``n_k <= 1``) are extrapolated by
    ``sigma_method`` (``"locf"`` = carry the last valid forward)."""
    if sigma_method != "locf":
        raise NotImplementedError(
            f"sigma_method={sigma_method!r} not in the saturated engine yet "
            f"(only 'locf')"
        )
    by: dict = {}
    for y, m0, k in zip(response, fitted, duration):
        by.setdefault(k, []).append((y, m0))
    durs = sorted(by)
    phi: dict = {}
    valid: list = []
    for k in durs:
        cells = by[k]
        df = len(cells) - 1
        if df <= 0:
            phi[k] = None
            continue
        phi[k] = sum((y - m0) ** 2 / m0 for y, m0 in cells) / df
        valid.append(k)
    if valid:
        for k in durs:                       # locf: carry last valid forward
            if phi[k] is None:
                phi[k] = phi[valid[-1]]
    return phi


def buhlmann_straub_psi(*, response, fitted, phi, cohort, duration) -> float:
    """Buhlmann-Straub moment estimate of the between-cohort variance ``psi``.

    Uses the PSI-MOMENT exposure ``m_i = (sum m0)^2 / sum(phi_k m0)`` and the
    raw per-cohort A/E ``r_i = sum y / sum m0``; floored at 0 (charter Sec.4.4
    degeneracy = exact complete-pooling intensity, ``PooledLoss``)."""
    sm = _sum_by(fitted, cohort)
    sy = _sum_by(response, cohort)
    sphim: dict = {}
    for m0, i, k in zip(fitted, cohort, duration):
        sphim[i] = sphim.get(i, 0.0) + phi[k] * m0
    cohorts = sorted(sm)
    r = {i: sy[i] / sm[i] for i in cohorts}
    m = {i: sm[i] ** 2 / sphim[i] for i in cohorts}     # psi-moment exposure
    mplus = sum(m.values())
    rbar = sum(m[i] * r[i] for i in cohorts) / mplus
    num = sum(m[i] * (r[i] - rbar) ** 2 for i in cohorts) - (len(cohorts) - 1)
    den = mplus - sum(m[i] ** 2 for i in cohorts) / mplus
    return max(0.0, num / den)


@dataclass
class LevelResult:
    """Per-cohort conjugate level estimate: ``u`` (mean-1 natural-scale level)
    and ``Z`` (credibility)."""

    u: dict
    Z: dict


def conjugate_levels(*, response, fitted, phi, psi, cohort, duration) -> LevelResult:
    """Dispersion-scaled conjugate (Buhlmann-Straub) cohort level.

    ``A_i = sum(m0/phi)`` (SHRINKAGE exposure), ``u_i = (1/psi + sum y/phi) /
    (1/psi + A_i)``, ``Z_i = A_i / (A_i + 1/psi)``. ``psi <= 0`` degenerates to
    the complete-pooling intensity ``PooledLoss`` (``u = 1``, ``Z = 0``)."""
    A: dict = {}
    sy: dict = {}
    for y, m0, i, k in zip(response, fitted, cohort, duration):
        A[i] = A.get(i, 0.0) + m0 / phi[k]
        sy[i] = sy.get(i, 0.0) + y / phi[k]
    cohorts = sorted(A)
    if psi <= 0.0:
        return LevelResult(u={i: 1.0 for i in cohorts},
                           Z={i: 0.0 for i in cohorts})
    inv = 1.0 / psi
    u = {i: (inv + sy[i]) / (inv + A[i]) for i in cohorts}
    Z = {i: A[i] / (A[i] + inv) for i in cohorts}
    return LevelResult(u=u, Z=Z)


def quasi_poisson_deviance(*, response, fitted) -> float:
    """Total quasi-Poisson deviance ``D = 2 sum [y log(y/mu) - (y - mu)]``
    (the ``y log(y/mu)`` term is 0 at ``y = 0``)."""
    d = 0.0
    for y, mu in zip(response, fitted):
        term = -(y - mu)
        if y > 0.0:
            term += y * math.log(y / mu)
        d += 2.0 * term
    return d
