"""Shared decay-law primitive for the tail and curve subsystems.

Both :class:`~lossratio.Tail` (the convergence-gated projection tail) and
:class:`~lossratio.Curve` (the ungated band-leg extrapolation) fit the same
2-parameter log-decay law to a development-factor excess series. This module
is the single home of that primitive -- the family vocabulary, the fitter,
the per-position evaluator, and the divergence boundary -- so neither sibling
imports its core math from the other.

Two curve families (the decay law of the excess, which decays toward 0):

* ``"inverse_power"`` (default): ``value = exp(a) * i**b`` -- a polynomial
  (heavy) tail.
* ``"exponential"``: ``value = exp(a + b*i)`` -- a geometric (light) tail.
"""

from __future__ import annotations

import math

import numpy as np

# The two supported decay-law families.
_FAMILIES = ("inverse_power", "exponential")

# Family-specific divergence boundary on the fitted decay slope `b`. A tail
# (a product of `1 + e^a*term_k` for CL, or a sum of `e^a*term_k` for ED) is
# finite iff `Sum_k term_k` converges:
#   - exponential, term = e^(b*k): Sum converges iff b < 0.
#   - inverse_power, term = k^b:   Sum converges iff b < -1 (a p-series).
# So `b >= threshold` is a divergent (horizon-dependent) tail and is gated.
_DIVERGENCE_SLOPE = {"exponential": 0.0, "inverse_power": -1.0}

# The other family -- the tail / curve is recomputed under it to disclose the
# model-choice band (e.g. inverse_power 6x vs exponential 1.2x).
_OTHER_FAMILY = {"inverse_power": "exponential", "exponential": "inverse_power"}


def _fit_decay(
    values: np.ndarray, idx: np.ndarray, family: str
) -> tuple[float, float, float]:
    """OLS fit of ``log(values) = a + b*X`` over the decaying positions.

    ``X = idx`` (exponential) or ``log(idx)`` (inverse_power); ``b < 0``
    means the values decay toward 0 in both forms. Returns
    ``(a, b, resid_std)`` -- the residual std of the log-scale fit measures
    how well the family describes the observed factors (a poor fit = a less
    trustworthy extrapolation).
    """
    X = idx if family == "exponential" else np.log(idx)
    A = np.column_stack([np.ones_like(X), X])
    y = np.log(values)
    coef, *_ = np.linalg.lstsq(A, y, rcond=None)
    a, b = float(coef[0]), float(coef[1])
    resid = y - (a + b * X)
    dof = max(int(X.size) - 2, 1)
    resid_std = float(np.sqrt(np.sum(resid ** 2) / dof))
    return a, b, resid_std


def _decay_value(a: float, b: float, i: float, family: str) -> float:
    """Evaluate the fitted decay law at position ``i``.

    The single inline kernel of the decay-law family, shared by every
    extrapolation walk so the formula lives in exactly one place:

    * ``"exponential"``: ``exp(a + b*i)`` (a geometric / light tail).
    * ``"inverse_power"``: ``exp(a) * i**b`` (a polynomial / heavy tail).
    """
    return math.exp(a + b * i) if family == "exponential" else math.exp(a) * i ** b
