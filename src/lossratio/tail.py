"""Tail extrapolation for chain-ladder loss projection.

The :class:`Tail` spec configures how the development factors are
extended beyond the observed window. Korean long-term protection
develops over decades, so ``f_k`` does NOT reach 1 within the observed
window -- the tail is an extrapolation whose validity must be GATED by
convergence evidence, not blindly curve-fitted:

* CONVERGED (the fitted excess ``f_k - 1`` decays fast enough for the
  tail to be finite): evidence-based extrapolation continued to the
  convergence tolerance.
* NOT CONVERGED (the decay is too slow -- the divergence guard fires at
  the curve's convergence boundary: ``b >= 0`` for ``exponential``,
  ``b >= -1`` for ``inverse_power``, the p-series boundary): there is no
  finite evidence-based tail. ``on_diverge`` decides the policy --
  ``"flag"`` keeps the observed ultimate, warns, and marks the fit so the
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
from dataclasses import dataclass, replace
from typing import Any, Callable

import numpy as np
import polars as pl

from ._io import normalize_groups
from ._decay import (
    _DIVERGENCE_SLOPE,
    _FAMILIES,
    _OTHER_FAMILY,
    _decay_value,
    _fit_decay,
)

# Development steps per year by grain -- used for the grain-aware horizon
# cap so "50 years" means the same calendar span at any granularity.
_PERIODS_PER_YEAR = {"M": 12, "Q": 4, "H": 2, "Y": 1}
_DEFAULT_HORIZON_YEARS = 50
_ON_DIVERGE = ("flag", "refuse")


@dataclass
class Tail:
    """Convergence-gated tail-extrapolation spec.

    Parameters
    ----------
    family
        Decay-law family for the excess ``f_k - 1``: ``"inverse_power"``
        (default, ``f - 1 = exp(a) * i^b`` -- a heavy polynomial tail) or
        ``"exponential"`` (``f - 1 = exp(a + b*i)`` -- a lighter
        geometric tail).
    on_diverge
        Policy when the fit decays too slowly to converge (slope past the
        curve's convergence boundary: ``b >= 0`` for ``exponential``,
        ``b >= -1`` for ``inverse_power`` -- the p-series boundary): so no
        finite evidence-based tail exists. ``"flag"`` (default) keeps the
        observed ultimate, emits a warning, and marks the fit for
        uncertainty widening; ``"refuse"`` caps at the observed ultimate
        (tail factor ``1.0``) silently.
    tol
        Convergence tolerance on the per-step excess ``f - 1``.
        Extrapolation stops once the projected excess falls below this.
        Default ``1e-4``.
    max_horizon
        Hard cap on the number of extrapolation steps. ``None`` (default)
        uses a grain-aware ``50``-year horizon
        (``50 * periods_per_year``).
    """

    family:      str = "inverse_power"
    on_diverge:  str = "flag"
    tol:         float = 1e-4
    max_horizon: int | None = None

    def __post_init__(self) -> None:
        if self.family not in _FAMILIES:
            raise ValueError(
                f"`family` must be one of {_FAMILIES}; got {self.family!r}."
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
    """Internal result of the tail computations -- the full provenance of
    a tail number, surfaced to the user via ``fit.tail_report`` so an
    auditor can see exactly how it was produced (it is an extrapolation,
    not an estimate with a sampling distribution)."""

    factor:    float          # CL: multiplicative tail factor (>=1.0); ED: Sum g_k
    family:    str | None     # fitted family (None for no-op / user factor)
    slope:     float | None   # fitted decay slope `b` (None when not fitted)
    n_steps:   int            # extrapolation steps taken
    diverged:  bool           # the decay slope was past the convergence boundary
    converged: bool           # the extrapolation reached `tol` within the horizon
    reason:    str            # short status tag
    intercept: float | None = None    # fitted decay intercept `a`
    fit_resid_std: float | None = None  # residual std of the log-decay OLS fit
    alt_factor: float | None = None   # the OTHER family's factor (model band)
    alt_family: str | None = None     # the OTHER family's name


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
        v = _decay_value(a, b, i, cfg.family)
        if not math.isfinite(v):
            break
        if v < cfg.tol:
            converged = True
            break
        terms.append(v)
        i += 1
    return terms, len(terms), converged


def _attach_alt(
    res: _TailResult,
    tail: Any,
    recompute: "Callable[[Tail], _TailResult]",
) -> _TailResult:
    """Disclose the model-choice band: recompute the tail under the OTHER
    curve family and record its factor on ``res``. Skipped for a numeric /
    no-tail result (``res.family is None``), which has no fitted curve."""
    if res.family is None or tail is False:
        return res
    other = _OTHER_FAMILY[res.family]
    cfg_other = replace(tail, family=other) if isinstance(tail, Tail) else Tail(family=other)
    alt = recompute(cfg_other)
    res.alt_factor = alt.factor
    res.alt_family = other
    return res


def compute_tail_factor(
    f_sel: np.ndarray,
    tail: bool | float | Tail,
    grain: str | None = None,
) -> _TailResult:
    """Multiplicative CL tail factor with the model-choice band attached.

    Thin wrapper over :func:`_compute_tail_factor_one`; also records the
    other curve family's factor (``alt_factor``) for transparency.
    """
    res = _compute_tail_factor_one(f_sel, tail, grain)
    return _attach_alt(res, tail, lambda cfg: _compute_tail_factor_one(f_sel, cfg, grain))


def _compute_tail_factor_one(
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

    finite = np.isfinite(f_sel)
    f_finite = f_sel[finite]
    if f_finite.size < 3 or np.any(f_finite <= 0):
        return _TailResult(1.0, cfg.family, None, 0, False, False, "insufficient_factors")
    # Index the ORIGINAL f_sel so an interior NaN link keeps every later
    # dev at its true 1-indexed position -- compacting f_sel first would
    # shift the decay fit. Mirrors the g-path in compute_tail_increment.
    pos = np.flatnonzero(finite & (f_sel > 1.0))
    if pos.size < 2:
        return _TailResult(1.0, cfg.family, None, 0, False, False, "no_decaying_excess")

    idx = (pos + 1).astype(float)  # 1-indexed dev positions in the original array
    excess = f_sel[pos] - 1.0      # > 0 on the f > 1 subset

    a, b, rstd = _fit_decay(excess, idx, cfg.family)
    if b >= _DIVERGENCE_SLOPE[cfg.family]:
        # The excess decays too slowly to give a finite tail (b >= 0 for
        # exponential, b >= -1 for inverse_power -- the p-series boundary).
        # Do not fabricate a convergent extrapolation from divergent data.
        reason = "diverged_refused" if cfg.on_diverge == "refuse" else "diverged_flagged"
        return _TailResult(1.0, cfg.family, b, 0, True, False, reason, a, rstd)

    terms, steps, converged = _extrapolate_terms(a, b, cfg, grain, int(idx.max()) + 1)
    # Multiplicative tail factor = product of (1 + excess), via log-space.
    # fsum keeps the running log-sum exact; terms are positive and decaying.
    factor = math.exp(math.fsum(math.log1p(v) for v in terms))
    if not math.isfinite(factor):
        return _TailResult(1.0, cfg.family, b, steps, False, False, "overflow", a, rstd)
    reason = "converged" if converged else "horizon_capped"
    return _TailResult(factor, cfg.family, b, steps, False, converged, reason, a, rstd)


def compute_tail_increment(
    g_sel: np.ndarray,
    tail: bool | Tail,
    grain: str | None = None,
) -> _TailResult:
    """Additive ED tail ``Sum g_k`` with the model-choice band attached."""
    res = _compute_tail_increment_one(g_sel, tail, grain)
    return _attach_alt(res, tail, lambda cfg: _compute_tail_increment_one(g_sel, cfg, grain))


def _compute_tail_increment_one(
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
        return _TailResult(0.0, cfg.family, None, 0, False, False, "insufficient_factors")
    idx = (pos + 1).astype(float)
    g_vals = g_sel[pos]

    a, b, rstd = _fit_decay(g_vals, idx, cfg.family)
    if b >= _DIVERGENCE_SLOPE[cfg.family]:
        # Intensity decays too slowly to give a finite Sum g (b >= 0 for
        # exponential, b >= -1 for inverse_power -- the p-series boundary).
        reason = "diverged_refused" if cfg.on_diverge == "refuse" else "diverged_flagged"
        return _TailResult(0.0, cfg.family, b, 0, True, False, reason, a, rstd)

    terms, steps, converged = _extrapolate_terms(a, b, cfg, grain, int(idx.max()) + 1)
    increment_sum = float(sum(terms))
    if not math.isfinite(increment_sum):
        return _TailResult(0.0, cfg.family, b, steps, False, False, "overflow", a, rstd)
    reason = "converged" if converged else "horizon_capped"
    return _TailResult(increment_sum, cfg.family, b, steps, False, converged, reason, a, rstd)


def compute_ed_tail_increment_coupled(
    g_sel: np.ndarray,
    premium_f_k: np.ndarray | None,
    tail: bool | Tail,
    grain: str | None = None,
) -> _TailResult:
    """Coupled ED tail ``S`` with the model-choice band attached."""
    res = _compute_ed_tail_increment_coupled_one(g_sel, premium_f_k, tail, grain)
    return _attach_alt(
        res, tail,
        lambda cfg: _compute_ed_tail_increment_coupled_one(g_sel, premium_f_k, cfg, grain),
    )


def _compute_ed_tail_increment_coupled_one(
    g_sel: np.ndarray,
    premium_f_k: np.ndarray | None,
    tail: bool | Tail,
    grain: str | None = None,
) -> _TailResult:
    """Additive ED tail with a developing premium (coupled forward walk).

    The exposure-driven loss increment beyond the observed window is
    ``Sum_k g_k * P_k`` where the cumulative premium ``P_k`` itself keeps
    developing -- ``P_k = P_last * prod_{j<=k} fP_j``. This walks the loss
    intensity ``g_k -> 0`` and the premium factor ``fP_k -> 1`` forward
    together and returns ``S = Sum_k g_k * prod fP`` so that
    ``loss_tail = loss_proj + premium_proj * S`` (``premium_proj`` is
    ``P_last``). When ``premium_f_k`` gives no usable decay fit the premium
    is treated as flat and this reduces to
    :func:`compute_tail_increment`.
    """
    if isinstance(tail, bool):
        if not tail:
            return _TailResult(0.0, None, None, 0, False, True, "no_tail")
        cfg = Tail()
    elif isinstance(tail, Tail):
        cfg = tail
    else:
        raise TypeError("compute_ed_tail_increment_coupled expects bool or Tail")

    pos = np.flatnonzero(np.isfinite(g_sel) & (g_sel > 0.0))
    if pos.size < 3:
        return _TailResult(0.0, cfg.family, None, 0, False, False, "insufficient_factors")
    g_idx = (pos + 1).astype(float)
    a_g, b_g, rstd_g = _fit_decay(g_sel[pos], g_idx, cfg.family)
    if b_g >= _DIVERGENCE_SLOPE[cfg.family]:
        reason = "diverged_refused" if cfg.on_diverge == "refuse" else "diverged_flagged"
        return _TailResult(0.0, cfg.family, b_g, 0, True, False, reason, a_g, rstd_g)

    # Premium factor decay (on fP - 1); flat premium when no usable fit.
    a_p = b_p = None
    if premium_f_k is not None:
        ppos = np.flatnonzero(np.isfinite(premium_f_k) & (premium_f_k > 1.0))
        if ppos.size >= 2:
            a_p, b_p, _ = _fit_decay(
                premium_f_k[ppos] - 1.0, (ppos + 1).astype(float), cfg.family
            )

    def _term(a: float, b: float, i: int) -> float:
        return _decay_value(a, b, i, cfg.family)

    ppy = _PERIODS_PER_YEAR.get(grain or "M", 12)
    max_h = cfg.max_horizon if cfg.max_horizon is not None else _DEFAULT_HORIZON_YEARS * ppy
    i = int(g_idx.max()) + 1
    cum_p = 1.0
    total = 0.0
    steps = 0
    converged = False
    while steps < max_h:
        g_i = _term(a_g, b_g, i)
        if not math.isfinite(g_i):
            break
        if a_p is not None:
            fp_excess = _term(a_p, b_p, i)
            if math.isfinite(fp_excess) and fp_excess > 0.0:
                cum_p *= 1.0 + fp_excess
        term = g_i * cum_p
        if not math.isfinite(term):
            break
        if term < cfg.tol:  # the loss increment has vanished
            converged = True
            break
        total += term
        steps += 1
        i += 1

    if not math.isfinite(total):
        return _TailResult(0.0, cfg.family, b_g, steps, False, False, "overflow", a_g, rstd_g)
    reason = "converged" if converged else "horizon_capped"
    return _TailResult(total, cfg.family, b_g, steps, False, converged, reason, a_g, rstd_g)


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


def _fmt_group(g: Any) -> str | None:
    """Human-readable group label for the report frame."""
    if g is None:
        return None
    if isinstance(g, (list, tuple)):
        return " | ".join(str(x) for x in g)
    return str(g)


def tail_report_frame(
    results: dict[Any, _TailResult],
    tail: Any,
    role: str = "loss",
) -> pl.DataFrame:
    """Build a one-row-per-group provenance table for a tail.

    Discloses every input and intermediate behind each tail number so it
    can be audited and reproduced: the curve family and its fitted
    parameters, how well the curve fit the observed factors, whether the
    divergence guard / horizon cap fired, and the OTHER curve family's
    result as a model-choice band. The tail is an extrapolation, not an
    estimate with a sampling distribution -- this table is the honest
    statement of how it was produced.
    """
    cfg = tail if isinstance(tail, Tail) else (Tail() if tail is True else None)
    tol = cfg.tol if cfg is not None else None
    max_horizon = cfg.max_horizon if cfg is not None else None
    rows: list[dict[str, Any]] = []
    for g, r in results.items():
        rows.append({
            "role": role,
            "group": _fmt_group(g),
            "family": r.family,
            "intercept": r.intercept,
            "slope": r.slope,
            "fit_resid_std": r.fit_resid_std,
            "n_steps": r.n_steps,
            "converged": r.converged,
            "diverged": r.diverged,
            "reason": r.reason,
            "factor": r.factor,
            "alt_family": r.alt_family,
            "alt_factor": r.alt_factor,
            "tol": tol,
            "max_horizon": max_horizon,
        })
    return pl.DataFrame(rows)


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
            f"Tail does not converge{where}: the fitted factors decay too "
            f"slowly (slope b={result.slope:.3g} past the curve's convergence "
            f"boundary), so no finite evidence-based tail exists. Keeping the "
            f"observed ultimate (tail factor 1.0) and flagging the fit; treat "
            f"the tail with wide uncertainty. Pass on_diverge='refuse' to silence.",
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
