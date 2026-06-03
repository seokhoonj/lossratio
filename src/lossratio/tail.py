"""Tail extrapolation for chain-ladder loss projection.

The :class:`Tail` spec configures how the development factors are
extended beyond the observed window. Korean long-term protection
develops over decades, so ``f_k`` does NOT reach 1 within the observed
window -- the tail is an extrapolation whose validity must be GATED by
convergence evidence, not blindly curve-fitted:

* CONVERGED (the fitted excess ``f_k - 1`` decays, slope ``< 0``):
  evidence-based extrapolation continued to the convergence tolerance.
* NOT CONVERGED (slope ``>= 0`` -- the divergence guard fires): there is
  no finite evidence-based tail. ``on_diverge`` decides the policy --
  ``"flag"`` keeps the estimate, warns, and marks the fit so the
  uncertainty can be widened; ``"refuse"`` caps at the observed ultimate.

Curve family (the decay law of the excess ``f_k - 1``):

* ``"inverse_power"`` (default): ``f - 1 = exp(a) * i^b`` -- a polynomial
  (heavy) tail, the right default for long-tailed Korean health
  development.
* ``"exponential"``: ``f - 1 = exp(a + b*i)`` -- a geometric (light) tail
  that can under-state a heavy tail.

This module is the single source of truth for the tail subsystem; the
projection engine in :mod:`lossratio.loss` calls
:func:`compute_tail_factor` and :func:`apply_tail_to_long_df`.
"""

from __future__ import annotations

import math
import warnings
from dataclasses import dataclass
from typing import Any

import numpy as np
import polars as pl

from ._io import normalize_groups

# Development steps per year by grain -- used for the grain-aware horizon
# cap so "50 years" means the same calendar span at any granularity.
_PERIODS_PER_YEAR = {"M": 12, "Q": 4, "H": 2, "Y": 1}
_DEFAULT_HORIZON_YEARS = 50
_CURVES = ("inverse_power", "exponential")
_ON_DIVERGE = ("flag", "refuse")


@dataclass
class Tail:
    """Convergence-gated tail-extrapolation spec.

    Parameters
    ----------
    curve
        Decay law for the excess ``f_k - 1``: ``"inverse_power"``
        (default, ``f - 1 = exp(a) * i^b`` -- a heavy polynomial tail) or
        ``"exponential"`` (``f - 1 = exp(a + b*i)`` -- a lighter
        geometric tail).
    on_diverge
        Policy when the fitted decay slope is non-negative (the excess
        does not decay, so no convergent tail exists): ``"flag"``
        (default) keeps the estimate, emits a warning, and marks the fit
        for uncertainty widening; ``"refuse"`` caps at the observed
        ultimate (tail factor ``1.0``).
    tol
        Convergence tolerance on the per-step excess ``f - 1``.
        Extrapolation stops once the projected excess falls below this.
        Default ``1e-4``.
    max_horizon
        Hard cap on the number of extrapolation steps. ``None`` (default)
        uses a grain-aware ``50``-year horizon
        (``50 * periods_per_year``).
    """

    curve:       str = "inverse_power"
    on_diverge:  str = "flag"
    tol:         float = 1e-4
    max_horizon: int | None = None

    def __post_init__(self) -> None:
        if self.curve not in _CURVES:
            raise ValueError(
                f"`curve` must be one of {_CURVES}; got {self.curve!r}."
            )
        if self.on_diverge not in _ON_DIVERGE:
            raise ValueError(
                f"`on_diverge` must be one of {_ON_DIVERGE}; got "
                f"{self.on_diverge!r}."
            )
        if not (self.tol > 0.0) or not math.isfinite(self.tol):
            raise ValueError(f"`tol` must be a positive finite float; got {self.tol!r}.")
        if self.max_horizon is not None and self.max_horizon <= 0:
            raise ValueError(
                f"`max_horizon` must be a positive integer or None; got "
                f"{self.max_horizon!r}."
            )


@dataclass
class _TailResult:
    """Internal result of :func:`compute_tail_factor`."""

    factor:    float          # CL: multiplicative tail factor (>=1.0); ED: Sum g_k
    curve:     str | None     # fitted curve family (None for no-op / user factor)
    slope:     float | None   # fitted decay slope `b` (None when not fitted)
    n_steps:   int            # extrapolation steps taken
    diverged:  bool           # the decay slope was non-negative
    converged: bool           # the extrapolation reached `tol` within the horizon
    reason:    str            # short status tag


def validate_tail(tail: Any) -> None:
    """Validate the ``tail`` argument shape (bool / finite numeric / :class:`Tail`)."""
    if isinstance(tail, (bool, Tail)):
        return
    if isinstance(tail, (int, float)) and not isinstance(tail, bool):
        if not np.isfinite(tail):
            raise ValueError(
                f"`tail` must be a finite numeric, boolean, or Tail; got {tail!r}."
            )
        return
    raise TypeError(
        f"`tail` must be bool, numeric, or Tail; got {type(tail).__name__}."
    )


def _fit_decay(values: np.ndarray, idx: np.ndarray, curve: str) -> tuple[float, float]:
    """OLS fit of ``log(values) = a + b*X`` over the decaying positions.

    ``X = idx`` (exponential) or ``log(idx)`` (inverse_power); ``b < 0``
    means the values decay toward 0 in both forms.
    """
    X = idx if curve == "exponential" else np.log(idx)
    A = np.column_stack([np.ones_like(X), X])
    coef, *_ = np.linalg.lstsq(A, np.log(values), rcond=None)
    return float(coef[0]), float(coef[1])


def _extrapolate_terms(
    a: float, b: float, cfg: "Tail", grain: str | None, start: int
) -> tuple[list[float], int, bool]:
    """Project the fitted decay curve forward from index ``start``.

    Stops once a projected term drops below ``cfg.tol`` (converged) or the
    grain-aware horizon is reached. Returns ``(terms, n_steps, converged)``;
    the caller compounds (CL) or sums (ED) the terms.
    """
    ppy = _PERIODS_PER_YEAR.get(grain or "M", 12)
    max_h = cfg.max_horizon if cfg.max_horizon is not None else _DEFAULT_HORIZON_YEARS * ppy
    terms: list[float] = []
    i = start
    converged = False
    while len(terms) < max_h:
        v = math.exp(a + b * i) if cfg.curve == "exponential" else math.exp(a) * i ** b
        if not math.isfinite(v):
            break
        if v < cfg.tol:
            converged = True
            break
        terms.append(v)
        i += 1
    return terms, len(terms), converged


def compute_tail_factor(
    f_sel: np.ndarray,
    tail: bool | float | Tail,
    grain: str | None = None,
) -> _TailResult:
    """Compute the scalar tail factor from selected ATA factors.

    - ``tail=False`` -> ``1.0`` (no-op).
    - numeric scalar -> used directly (a user-supplied known tail factor).
    - ``tail=True`` / :class:`Tail` -> fit the configured decay curve on
      ``log(f_sel - 1)`` over the ``f_sel > 1`` entries, then extrapolate.
      A non-negative fitted slope trips the divergence guard (see
      :class:`Tail` ``on_diverge``); otherwise the product of the
      projected ``f`` is accumulated until the excess falls below ``tol``
      or the grain-aware horizon is reached.

    Unlike the legacy implementation there is NO magnitude clamp: a heavy
    but genuinely convergent tail factor is kept; only a non-decaying
    (divergent) fit is gated.
    """
    if isinstance(tail, bool):
        if not tail:
            return _TailResult(1.0, None, None, 0, False, True, "no_tail")
        cfg = Tail()
    elif isinstance(tail, Tail):
        cfg = tail
    else:  # finite numeric -- explicit user factor, no extrapolation
        return _TailResult(float(tail), None, None, 0, False, True, "user_factor")

    f_vals = f_sel[np.isfinite(f_sel)]
    if f_vals.size < 3 or np.any(f_vals <= 0):
        return _TailResult(1.0, cfg.curve, None, 0, False, False, "insufficient_factors")
    pos = np.flatnonzero(f_vals > 1.0)
    if pos.size < 2:
        return _TailResult(1.0, cfg.curve, None, 0, False, False, "no_decaying_excess")

    idx = (pos + 1).astype(float)  # 1-indexed positions (R `which` parity)
    excess = f_vals[pos] - 1.0     # > 0 on the f > 1 subset

    a, b = _fit_decay(excess, idx, cfg.curve)
    if b >= 0.0:
        # Non-decaying excess: there is no finite evidence-based tail.
        # Do not fabricate a convergent extrapolation from divergent data.
        reason = "diverged_refused" if cfg.on_diverge == "refuse" else "diverged_flagged"
        return _TailResult(1.0, cfg.curve, b, 0, True, False, reason)

    terms, steps, converged = _extrapolate_terms(a, b, cfg, grain, int(idx.max()) + 1)
    # Multiplicative tail factor = product of (1 + excess), via log-space.
    factor = math.exp(sum(math.log1p(v) for v in terms))
    if not math.isfinite(factor):
        return _TailResult(1.0, cfg.curve, b, steps, False, False, "overflow")
    reason = "converged" if converged else "horizon_capped"
    return _TailResult(factor, cfg.curve, b, steps, False, converged, reason)


def compute_tail_increment(
    g_sel: np.ndarray,
    tail: bool | Tail,
    grain: str | None = None,
) -> _TailResult:
    """Additive ED tail: the projected sum of future intensities ``Sum g_k``.

    The exposure-driven loss increment beyond the observed window is
    ``Sum_{future k} g_k * premium``; this returns the group-level
    ``Sum g_k`` (the per-cohort premium scaling is applied in
    :func:`apply_ed_tail_to_long_df`). ``g_sel`` decays toward 0, so the
    same decay fit / divergence guard / convergence horizon as the CL tail
    apply, but the projected terms are SUMMED additively rather than
    compounded. ``_TailResult.factor`` carries ``Sum g_k`` (``0.0`` when
    there is no tail).
    """
    if isinstance(tail, bool):
        if not tail:
            return _TailResult(0.0, None, None, 0, False, True, "no_tail")
        cfg = Tail()
    elif isinstance(tail, Tail):
        cfg = tail
    else:  # numeric is applied multiplicatively upstream, never reaches here
        raise TypeError("compute_tail_increment expects bool or Tail")

    pos = np.flatnonzero(np.isfinite(g_sel) & (g_sel > 0.0))
    if pos.size < 3:
        return _TailResult(0.0, cfg.curve, None, 0, False, False, "insufficient_factors")
    idx = (pos + 1).astype(float)
    g_vals = g_sel[pos]

    a, b = _fit_decay(g_vals, idx, cfg.curve)
    if b >= 0.0:
        reason = "diverged_refused" if cfg.on_diverge == "refuse" else "diverged_flagged"
        return _TailResult(0.0, cfg.curve, b, 0, True, False, reason)

    terms, steps, converged = _extrapolate_terms(a, b, cfg, grain, int(idx.max()) + 1)
    increment_sum = float(sum(terms))
    if not math.isfinite(increment_sum):
        return _TailResult(0.0, cfg.curve, b, steps, False, False, "overflow")
    reason = "converged" if converged else "horizon_capped"
    return _TailResult(increment_sum, cfg.curve, b, steps, False, converged, reason)


def apply_tail_to_long_df(
    long_df: pl.DataFrame,
    tail_factor: float,
    groups: str | list[str] | None,
    role: str = "loss",
) -> pl.DataFrame:
    """Append ``_tail``-suffixed companion columns to the last-dev row
    of each cohort.

    The non-tail columns are left untouched; users read ``<role>_tail``
    from the long table as the tail-adjusted ultimate. The tail-row SE
    columns are scaled by ``tail_factor`` so the CI widens with a larger
    (heavier) tail.
    """
    if tail_factor <= 1.0 or not np.isfinite(tail_factor):
        return long_df

    proj_col = f"{role}_proj"
    proc_se2 = f"{role}_proc_se2"
    param_se2 = f"{role}_param_se2"
    total_se2 = f"{role}_total_se2"
    proc_se = f"{role}_proc_se"
    param_se = f"{role}_param_se"
    total_se = f"{role}_total_se"

    keys = [*normalize_groups(groups), "cohort"]
    # Identify the last-dev row per cohort: rank by `dev` descending and pick rank==1.
    last_marker = long_df.with_columns(
        pl.col("dev").rank(method="dense", descending=True).over(keys).alias("_dev_rank")
    )
    is_last = pl.col("_dev_rank") == 1

    # Some columns may be absent in worker-level fits (no SE^2 cache);
    # fall back to deriving from the SE columns.
    has_se2 = proc_se2 in long_df.columns

    def _se2_expr(col_name: str, se_col: str) -> pl.Expr:
        if has_se2:
            return pl.col(col_name) * (tail_factor ** 2)
        return (pl.col(se_col) ** 2) * (tail_factor ** 2)

    out = last_marker.with_columns(
        pl.when(is_last)
        .then(pl.col(proj_col) * tail_factor)
        .otherwise(None)
        .alias(f"{role}_tail"),
        pl.when(is_last)
        .then(_se2_expr(proc_se2, proc_se))
        .otherwise(None)
        .alias(f"{role}_proc_se2_tail"),
        pl.when(is_last)
        .then(_se2_expr(param_se2, param_se))
        .otherwise(None)
        .alias(f"{role}_param_se2_tail"),
        pl.when(is_last)
        .then(_se2_expr(total_se2, total_se))
        .otherwise(None)
        .alias(f"{role}_total_se2_tail"),
    )

    out = out.with_columns(
        pl.col(f"{role}_proc_se2_tail").sqrt().alias(f"{role}_proc_se_tail"),
        pl.col(f"{role}_param_se2_tail").sqrt().alias(f"{role}_param_se_tail"),
        pl.col(f"{role}_total_se2_tail").sqrt().alias(f"{role}_total_se_tail"),
    )

    tail_col = f"{role}_tail"
    out = out.with_columns(
        pl.when(
            pl.col(tail_col).is_not_null()
            & pl.col(tail_col).is_finite()
            & (pl.col(tail_col) != 0.0)
        )
        .then(pl.col(f"{role}_proc_se_tail") / pl.col(tail_col).abs())
        .otherwise(None)
        .alias(f"{role}_proc_cv_tail"),
        pl.when(
            pl.col(tail_col).is_not_null()
            & pl.col(tail_col).is_finite()
            & (pl.col(tail_col) != 0.0)
        )
        .then(pl.col(f"{role}_param_se_tail") / pl.col(tail_col).abs())
        .otherwise(None)
        .alias(f"{role}_param_cv_tail"),
        pl.when(
            pl.col(tail_col).is_not_null()
            & pl.col(tail_col).is_finite()
            & (pl.col(tail_col) != 0.0)
        )
        .then(pl.col(f"{role}_total_se_tail") / pl.col(tail_col).abs())
        .otherwise(None)
        .alias(f"{role}_total_cv_tail"),
    )

    return out.drop("_dev_rank")


def apply_ed_tail_to_long_df(
    long_df: pl.DataFrame,
    increment_sum: float,
    groups: str | list[str] | None,
    role: str = "loss",
) -> pl.DataFrame:
    """Additive ED tail companion columns on the last-dev row of each cohort.

    ``<role>_tail = <role>_proj + premium_proj * Sum_g`` -- the premium is
    held at its last projected value (the matured exposure). The tail-row
    SE columns scale by the per-cohort effective factor
    ``<role>_tail / <role>_proj`` so the CI widens additively with the
    tail; the non-tail columns are left untouched.
    """
    if increment_sum <= 0.0 or not np.isfinite(increment_sum):
        return long_df

    proj_col = f"{role}_proj"
    proc_se2 = f"{role}_proc_se2"
    param_se2 = f"{role}_param_se2"
    total_se2 = f"{role}_total_se2"
    proc_se = f"{role}_proc_se"
    param_se = f"{role}_param_se"
    total_se = f"{role}_total_se"
    tail_col = f"{role}_tail"

    keys = [*normalize_groups(groups), "cohort"]
    marked = long_df.with_columns(
        pl.col("dev").rank(method="dense", descending=True).over(keys).alias("_dev_rank")
    )
    is_last = pl.col("_dev_rank") == 1

    # loss_tail = loss_proj + premium_proj * Sum_g on the last-dev row.
    out = marked.with_columns(
        pl.when(is_last)
        .then(pl.col(proj_col) + pl.col("premium_proj") * increment_sum)
        .otherwise(None)
        .alias(tail_col)
    )

    # Per-cohort effective multiplicative factor for SE scaling.
    has_se2 = proc_se2 in long_df.columns
    ef = pl.col(tail_col) / pl.col(proj_col)

    def _se2_expr(col_name: str, se_col: str) -> pl.Expr:
        base = pl.col(col_name) if has_se2 else pl.col(se_col) ** 2
        return base * (ef ** 2)

    out = out.with_columns(
        pl.when(is_last).then(_se2_expr(proc_se2, proc_se)).otherwise(None).alias(f"{role}_proc_se2_tail"),
        pl.when(is_last).then(_se2_expr(param_se2, param_se)).otherwise(None).alias(f"{role}_param_se2_tail"),
        pl.when(is_last).then(_se2_expr(total_se2, total_se)).otherwise(None).alias(f"{role}_total_se2_tail"),
    )
    out = out.with_columns(
        pl.col(f"{role}_proc_se2_tail").sqrt().alias(f"{role}_proc_se_tail"),
        pl.col(f"{role}_param_se2_tail").sqrt().alias(f"{role}_param_se_tail"),
        pl.col(f"{role}_total_se2_tail").sqrt().alias(f"{role}_total_se_tail"),
    )

    valid_tail = (
        pl.col(tail_col).is_not_null()
        & pl.col(tail_col).is_finite()
        & (pl.col(tail_col) != 0.0)
    )
    out = out.with_columns(
        pl.when(valid_tail).then(pl.col(f"{role}_proc_se_tail") / pl.col(tail_col).abs()).otherwise(None).alias(f"{role}_proc_cv_tail"),
        pl.when(valid_tail).then(pl.col(f"{role}_param_se_tail") / pl.col(tail_col).abs()).otherwise(None).alias(f"{role}_param_cv_tail"),
        pl.when(valid_tail).then(pl.col(f"{role}_total_se_tail") / pl.col(tail_col).abs()).otherwise(None).alias(f"{role}_total_cv_tail"),
    )

    return out.drop("_dev_rank")


def maybe_warn_tail(result: _TailResult, group: Any = None) -> None:
    """Emit a warning when the tail is not evidence-converged.

    Two non-converged cases warn (the tail is an extrapolation of
    uncertain validity -- treat the projection with wide uncertainty):

    * ``"diverged_flagged"`` -- the fitted decay slope is non-negative, so
      no convergent tail exists; the observed ultimate is kept.
    * ``"horizon_capped"`` -- the excess decays but did not reach the
      convergence tolerance within the horizon, so the factor is
      horizon-sensitive.
    """
    where = "" if group is None else f" (group {group!r})"
    if result.reason == "diverged_flagged":
        warnings.warn(
            f"Tail factors do not decay{where}: the fitted decay slope is "
            f"non-negative, so no convergent tail exists. Keeping the observed "
            f"ultimate (tail factor 1.0) and flagging the fit; treat the tail "
            f"with wide uncertainty. Pass on_diverge='refuse' to silence.",
            stacklevel=3,
        )
    elif result.reason == "horizon_capped":
        warnings.warn(
            f"Tail did not converge within the horizon{where}: the projected "
            f"term is still above `tol` at the horizon cap, so the tail "
            f"({result.factor:.3g}) is horizon-sensitive. Treat the tail with "
            f"wide uncertainty, or set a heavier-decay curve / shorter horizon.",
            stacklevel=3,
        )
