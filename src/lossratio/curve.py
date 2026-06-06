"""Parametric development-factor curve fitting and extrapolation.

The :class:`Curve` spec fits a 2-parameter log-decay law to a
development-factor series and evaluates / extrapolates it at any
development position, including beyond the observed grid. Two factor
targets are supported:

* ``target="intensity"``: the exposure-driven intensity ``g_k =
  delta_loss / cumulative_premium`` (additive, decays toward 0).
* ``target="ata"``: the chain-ladder age-to-age factor ``f_k =
  C_{k+1} / C_k`` (multiplicative); the curve is fitted on the
  *excess* ``f_k - 1``, which decays toward 0.

Unlike the convergence-gated tail, the fit is NOT gated on convergence
evidence, so it can extend a young (not-yet-converged) segment. The
honest cost of that freedom is surfaced on :class:`CurveResult` --
residual std, point count, the alternate-law value, and a divergence
flag -- never hidden behind a bare point estimate. A 2-point fit is
exact (zero residual) yet says nothing about validity: read the band,
not the point.

The decay-law primitive (``_fit_decay``), the divergence boundary, and
the law-evaluation kernel (``_decay_value``) live in the shared
:mod:`lossratio._decay` module -- they are imported here and by the tail
subsystem alike, not re-implemented.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from ._decay import (
    _DIVERGENCE_SLOPE,
    _FAMILIES,
    _OTHER_FAMILY,
    _decay_value,
    _fit_decay,
)

_TARGETS = ("intensity", "ata")
# _FAMILIES (imported) == ("inverse_power", "exponential").

# Small step inside the convergent side when reporting a clamped slope.
_CLAMP_EPS = 1e-6


@dataclass(kw_only=True)
class Curve:
    """Parametric development-factor curve spec.

    Fits a 2-parameter log-decay law to a development-factor series and
    evaluates / extrapolates it at any 1-indexed development position,
    including beyond the observed grid. Unlike the convergence-gated
    tail, the fit is NOT gated on convergence evidence, so it can extend
    a young (not-yet-converged) segment; the honest cost is reported on
    :class:`CurveResult`.

    Parameters
    ----------
    target
        Factor series the curve is fitted on: ``"intensity"`` (the ED
        intensity ``g_k``, default) or ``"ata"`` (the CL age-to-age
        factor ``f_k`` -- the curve is fitted on the excess ``f_k - 1``).
    family
        Decay-law family: ``"inverse_power"`` (default, ``value = exp(a) *
        i**b`` -- a heavy polynomial tail) or ``"exponential"``
        (``value = exp(a + b*i)`` -- a lighter geometric tail). The tail
        spec :class:`~lossratio.Tail` spells this same choice ``family``.
    min_points
        Honesty floor on the number of points used in the OLS. A fit on
        fewer than ``min_points`` points is flagged ``under_determined``
        on the result (the point estimate is still returned, but it
        should be banded). Must be an int ``>= 2`` (a 2-parameter fit
        needs at least 2 points). Default ``3``.
    clamp
        When ``True`` (default) and the raw fitted slope is past the
        law's convergence boundary, populate ``clamped_slope`` (the
        convergent-side value) and set ``clamped`` on the result. This
        does NOT alter :meth:`CurveResult.evaluate` /
        :meth:`CurveResult.extrapolate`, which always use the raw
        fitted slope; the clamped value is data for a future tail-sum
        consumer to opt into.
    """

    target:     str  = "intensity"
    family:     str  = "inverse_power"
    min_points: int  = 3
    clamp:      bool = True

    def __post_init__(self) -> None:
        if self.target not in _TARGETS:
            raise ValueError(
                f"`target` must be one of {_TARGETS}; got {self.target!r}."
            )
        if self.family not in _FAMILIES:
            raise ValueError(
                f"`family` must be one of {_FAMILIES}; got {self.family!r}."
            )
        if (
            not isinstance(self.min_points, int)
            or isinstance(self.min_points, bool)
            or self.min_points < 2
        ):
            raise ValueError(
                f"`min_points` must be an int >= 2 (a 2-param fit needs "
                f">= 2 points); got {self.min_points!r}."
            )

    def fit(self, values: np.ndarray) -> "CurveResult":
        """Fit the configured law to a 1-D per-link factor series.

        ``values`` are at 1-indexed positions ``1..n`` (array slot ``k``
        maps to position ``i = k + 1``). For ``target="ata"`` the raw
        ``f_k`` are passed and the ``f_k - 1`` excess transform is
        applied internally. Always returns a :class:`CurveResult` (never
        raises on degenerate / empty / under-determined input -- such
        data is legitimate for a young segment; the result flags it).
        """
        series = np.asarray(values, dtype=float).ravel()
        if self.target == "ata":
            series = series - 1.0

        # Finite + positive mask: log needs positive values; non-positive
        # entries (ata factors <= 1 = no development) are dropped. Mask
        # BEFORE the peak search so a NaN-adjacent slot can't poison it.
        mask = np.isfinite(series) & (series > 0.0)
        idx_all = np.flatnonzero(mask) + 1  # original 1-indexed positions
        v_all = series[mask]

        if v_all.size == 0:
            # `"empty"`: no finite input at all (empty / all-NaN). Else
            # the input had finite values but none survived the `>0`
            # positivity filter -> `"non_positive"`.
            reason = "non_positive" if np.isfinite(series).any() else "empty"
            return _empty_result(self, reason)

        # Peak / decaying-region selection: the fit region is the suffix
        # from the peak onward (peak included). For a humped intensity
        # this drops the rising limb that would bias the slope toward 0;
        # for a monotone excess the peak degenerates to position 0 and the
        # region is the whole positive suffix.
        peak = int(np.argmax(v_all))  # first occurrence on ties
        region_v = v_all[peak:]
        region_idx = idx_all[peak:].astype(float)  # absolute, NOT re-packed
        peak_index = int(idx_all[peak])
        n_points = int(region_v.size)

        if n_points < 2:
            return CurveResult(
                target=self.target,
                family=self.family,
                intercept=None,
                slope=None,
                fit_resid_std=None,
                n_points=n_points,
                peak_index=peak_index,
                diverged=False,
                clamped=False,
                clamped_slope=None,
                under_determined=True,
                reason="no_decaying_region",
                alt_family=None,
                alt_slope=None,
                alt_diverged=None,
            )

        under_determined = n_points < self.min_points

        a, b, rstd = _fit_decay(region_v, region_idx, self.family)

        boundary = _DIVERGENCE_SLOPE[self.family]
        diverged = b >= boundary
        if self.clamp and diverged:
            clamped = True
            clamped_slope: float | None = boundary - _CLAMP_EPS
        else:
            clamped = False
            clamped_slope = None

        # Always-on alt-law band: the same kept points under the OTHER law
        # disclose the model-choice swing (a law-choice effect invisible
        # to the residual std).
        other = _OTHER_FAMILY[self.family]
        _, b_alt, _ = _fit_decay(region_v, region_idx, other)
        alt_diverged = b_alt >= _DIVERGENCE_SLOPE[other]

        return CurveResult(
            target=self.target,
            family=self.family,
            intercept=a,
            slope=b,  # RAW fitted slope -- evaluate uses this, never clamped
            fit_resid_std=rstd,
            n_points=n_points,
            peak_index=peak_index,
            diverged=diverged,
            clamped=clamped,
            clamped_slope=clamped_slope,
            under_determined=under_determined,
            reason="under_determined" if under_determined else "ok",
            alt_family=other,
            alt_slope=b_alt,
            alt_diverged=alt_diverged,
        )


@dataclass(frozen=True)
class CurveResult:
    """Honest provenance of a :class:`Curve` fit.

    A deterministic extrapolation, not an estimate with a sampling
    distribution. Read ``under_determined``, ``n_points``,
    ``fit_resid_std`` and the alt-law value together: a small residual on
    few points is degeneracy, not quality (a 2-point fit is exact).

    Attributes
    ----------
    target, family
        The fitted target and decay-law family (echoed from the spec).
    intercept, slope
        The fitted ``a`` and the RAW fitted ``b`` (never clamped);
        ``None`` when no fit was possible.
    fit_resid_std
        Log-scale OLS residual std (0.0 on a 2-point fit -- read as
        degeneracy, not perfection).
    n_points
        Points used in the OLS (post-peak, finite, positive).
    peak_index
        1-indexed position of the peak (the fit-region start); ``None``
        when no point survived the positivity filter.
    diverged
        The raw slope is past the law's convergence boundary.
    clamped, clamped_slope
        When ``clamp=True`` and ``diverged``, the convergent-side slope
        ``boundary - eps`` (data only; ``evaluate`` ignores it).
    under_determined
        ``n_points < min_points`` -- band the extrapolation.
    reason
        One-glance status: ``"ok"`` | ``"under_determined"`` |
        ``"no_decaying_region"`` | ``"non_positive"`` | ``"empty"``.
    alt_family, alt_slope, alt_diverged
        The OTHER family on the same kept points (the model-choice band).
    """

    target:           str
    family:           str
    intercept:        float | None
    slope:            float | None
    fit_resid_std:    float | None
    n_points:         int
    peak_index:       int | None
    diverged:         bool
    clamped:          bool
    clamped_slope:    float | None
    under_determined: bool
    reason:           str
    alt_family:       str | None
    alt_slope:        float | None
    alt_diverged:     bool | None

    def evaluate(self, dev: "int | np.ndarray") -> "float | np.ndarray":
        """Value of the fitted law at 1-indexed position(s) ``dev``.

        Returns the value in the *consumer scale*: ``g_k`` for
        ``target="intensity"``, ``f_k = 1 + law`` for ``target="ata"``.
        Uses the raw fitted slope (never clamped). Vectorized over numpy
        arrays. When no fit was possible (``slope is None``) returns
        ``nan`` (scalar input) / all-``nan`` (array input).
        """
        is_array = isinstance(dev, np.ndarray)
        if self.slope is None or self.intercept is None:
            if is_array:
                return np.full(np.asarray(dev).shape, np.nan, dtype=float)
            return float("nan")

        i = np.asarray(dev, dtype=float)
        if self.family == "exponential":
            value = np.exp(self.intercept + self.slope * i)
        else:  # inverse_power
            value = np.exp(self.intercept) * i ** self.slope
        if self.target == "ata":
            value = value + 1.0

        if is_array:
            return value
        return float(value)

    def extrapolate(
        self, *, start: int, horizon: int, tol: float = 1e-4
    ) -> np.ndarray:
        """Per-step consumer-scale values from position ``start``.

        Walks 1-indexed positions ``start, start+1, ...`` for up to
        ``horizon`` steps, stopping early once the *law value* (the
        pre-``+1`` decay term for ``target="ata"``) drops below ``tol``.
        Returns the per-step values un-summed / un-compounded -- the
        caller compounds ``1 + v`` (CL) or sums ``v`` (ED). For
        ``target="ata"`` each returned term is ``1 + law``. Returns an
        empty array when no fit was possible (``slope is None``) or when
        ``start`` is not a valid 1-indexed position (``start < 1``) --
        the latter mirrors :meth:`evaluate`'s no-raise posture rather
        than letting ``0 ** negative`` / a negative base trip an
        exception.
        """
        if self.slope is None or self.intercept is None:
            return np.empty(0, dtype=float)
        if start < 1:
            return np.empty(0, dtype=float)

        out: list[float] = []
        i = start
        for _ in range(max(horizon, 0)):
            v = _decay_value(self.intercept, self.slope, float(i), self.family)
            if not np.isfinite(v):
                break
            if v < tol:
                break
            out.append(v + 1.0 if self.target == "ata" else v)
            i += 1
        return np.asarray(out, dtype=float)


def _empty_result(spec: Curve, reason: str) -> CurveResult:
    """A no-fit :class:`CurveResult` for empty / non-positive input."""
    return CurveResult(
        target=spec.target,
        family=spec.family,
        intercept=None,
        slope=None,
        fit_resid_std=None,
        n_points=0,
        peak_index=None,
        diverged=False,
        clamped=False,
        clamped_slope=None,
        under_determined=True,
        reason=reason,
        alt_family=None,
        alt_slope=None,
        alt_diverged=None,
    )
