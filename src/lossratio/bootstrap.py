"""Triangle-level bootstrap -- Phase 1: analytical CL (Mack closed-form).

This module ports the bootstrap worker of the R sibling package. Phase 1
implements only the ``type="analytical"`` / ``method="cl"`` paradigm --
the Mack (1993) closed-form propagation -- together with the
paradigm-agnostic summary kernel that decomposes per-cell predictive
variance into parameter and process components.

The analytical CL paradigm draws new link factors from
``N(f_hat, sqrt(Var(f_hat)))`` per replicate, leaves observed
(upper-triangle) cells unchanged, forward-projects the lower triangle,
and adds Stage 2 chain-Markov process noise from ``sigma2_k``. The
parameter / process split then follows from the law of total variance.

Other paradigms -- ``nonparametric`` (England-Verrall residual
resampling) and ``parametric`` (textbook cell-distribution sampling),
and the ED / SA methods -- are deferred to later phases and currently
raise :class:`NotImplementedError`.

References
----------
Mack (1993), ASTIN Bulletin 23/2 -- ``sigma_k^2`` / ``Var(f_hat_k)``.
England & Verrall (1999), IME 25/3 -- bootstrap framing.
Davison & Hinkley (1997) -- type-1 ordinal percentile CI.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import mirror_output
from .link import _build_link_df
from .maturity import Maturity

if TYPE_CHECKING:
    from .triangle import Triangle


# ---------------------------------------------------------------------------
# Section 1 -- Anchor computation (Mack per-link f_hat / sigma2 / f_var)
# ---------------------------------------------------------------------------


@dataclass
class _Anchor:
    """Per-link Mack anchor for a single-group triangle.

    All arrays have length ``n_links = n_devs - 1`` and are indexed by
    ``k = 0..n_links - 1`` -- the link from dev ``k + 1`` to dev
    ``k + 2``.

    Attributes
    ----------
    ata_from, ata_to
        1-indexed source / destination development period of each link.
    f_hat
        Volume-weighted chain ladder factor ``sum(loss_to)/sum(loss_from)``.
    sigma2
        Mack ``sigma^2_k`` (LOCF tail-filled when the last link has a
        single contributing cohort).
    f_var
        ``Var(f_hat_k) = sigma2_k / sum(loss_from)``.
    n_cohorts
        Number of cohorts contributing to each link.
    sum_from
        ``sum(loss_from)`` over the contributing cohorts.
    """

    ata_from:  np.ndarray
    ata_to:    np.ndarray
    f_hat:     np.ndarray
    sigma2:    np.ndarray
    f_var:     np.ndarray
    n_cohorts: np.ndarray
    sum_from:  np.ndarray


def _boot_fill_sigma2(s2: np.ndarray) -> np.ndarray:
    """Mack tail-rule fill for ``sigma^2`` (LOCF, ordered by ``ata_from``).

    When a link's ``sigma^2`` is NaN (the last link has only one
    contributing cohort, ``n = 1``), carry forward the previous finite
    value. If no prior value exists, fall back to ``0``. Mirrors R's
    ``.boot_fill_sigma2``.
    """
    out = s2.astype(np.float64, copy=True)
    for i in range(len(out)):
        if np.isnan(out[i]):
            if i >= 1 and np.isfinite(out[i - 1]):
                out[i] = out[i - 1]
            else:
                out[i] = 0.0
    return out


def _boot_anchor_cl(loss_obs: np.ndarray) -> _Anchor:
    """Compute the per-link Mack anchor from an observed cumulative matrix.

    Mirrors R's ``.boot_anchor_cl`` (single group). For each link
    ``k`` (dev ``k + 1`` -> dev ``k + 2``), over the cohorts where both
    ``loss_from`` and ``loss_to`` are finite and ``loss_from > 0``:

    * ``f_hat   = sum(loss_to) / sum(loss_from)``
    * ``sigma2  = sum((loss_to - f_hat * loss_from)^2 / loss_from) / (n - 1)``
      (only when ``n >= 2``; NaN otherwise -- filled by the tail rule)
    * ``f_var   = sigma2 / sum(loss_from)``

    Parameters
    ----------
    loss_obs
        ``(n_cohorts, n_devs)`` observed cumulative loss matrix
        (``np.nan`` where unobserved).
    """
    n_cohorts, n_devs = loss_obs.shape
    n_links = max(n_devs - 1, 0)

    ata_from  = np.arange(1, n_links + 1, dtype=np.int64)
    ata_to    = np.arange(2, n_links + 2, dtype=np.int64)
    f_hat     = np.full(n_links, np.nan, dtype=np.float64)
    sigma2    = np.full(n_links, np.nan, dtype=np.float64)
    n_cohort  = np.zeros(n_links, dtype=np.int64)
    sum_from  = np.full(n_links, np.nan, dtype=np.float64)

    for k in range(n_links):
        loss_from = loss_obs[:, k]
        loss_to   = loss_obs[:, k + 1]
        ok = (
            np.isfinite(loss_from)
            & np.isfinite(loss_to)
            & (loss_from > 0.0)
        )
        n = int(ok.sum())
        n_cohort[k] = n
        if n == 0:
            continue

        c_from = loss_from[ok]
        c_to   = loss_to[ok]
        s_from = c_from.sum()
        sum_from[k] = s_from
        if s_from <= 0.0:
            continue

        f = c_to.sum() / s_from
        f_hat[k] = f
        if n >= 2:
            resid_sq = (c_to - f * c_from) ** 2 / c_from
            sigma2[k] = resid_sq.sum() / (n - 1)
        # n == 1 -> sigma2 stays NaN, filled by the tail rule below.

    sigma2 = _boot_fill_sigma2(sigma2)

    f_var = np.full(n_links, np.nan, dtype=np.float64)
    valid = (
        np.isfinite(sigma2)
        & np.isfinite(sum_from)
        & (sum_from > 0.0)
    )
    f_var[valid] = sigma2[valid] / sum_from[valid]

    return _Anchor(
        ata_from  = ata_from,
        ata_to    = ata_to,
        f_hat     = f_hat,
        sigma2    = sigma2,
        f_var     = f_var,
        n_cohorts = n_cohort,
        sum_from  = sum_from,
    )


# ---------------------------------------------------------------------------
# Section 2 -- Analytical CL Stage 1 + Stage 2 kernel (pure numpy)
# ---------------------------------------------------------------------------


@dataclass
class _Stage1Result:
    """Raw per-group output of the analytical CL kernel.

    Attributes
    ----------
    cum_mean
        ``(n_cohorts, n_devs, B)`` Stage 1 array -- forward projection
        means; observed cells equal the observed cumulative.
    cum_sampled
        ``(n_cohorts, n_devs, B)`` Stage 1 + Stage 2 array -- chain-Markov
        noisy simulation; observed cells equal the observed cumulative.
    cohorts
        Cohort identifiers (length ``n_cohorts``).
    devs
        Development period values (length ``n_devs``).
    """

    cum_mean:    np.ndarray
    cum_sampled: np.ndarray
    cohorts:     list
    devs:        list


_PROCESS_CODES = {"gamma": 1, "od_pois": 2, "normal": 3}


def _boot_kernel_cl_analytical(
    loss_obs:      np.ndarray,
    anchor:        _Anchor,
    B:             int,
    rng:           np.random.Generator,
    alpha:         float       = 1.0,
    process:       str         = "normal",
    _injected_fstar: np.ndarray | None = None,
) -> _Stage1Result:
    """Analytical CL bootstrap kernel (Mack 1993 closed-form propagation).

    Mirrors the R native kernel ``bootstrap_kernel_cl_parametric``
    followed by the Mack-paradigm Stage 2 simulation
    ``bootstrap_fwd_sim_cl_link``. Pure numpy, single group.

    Stages
    ------
    Draw
        ``f_star[k, b] ~ Normal(f_hat[k], sqrt(f_var[k]))`` when
        ``f_var[k]`` is finite and positive; otherwise ``f_star = f_hat``.
    Stage 1 (``cum_mean``)
        Observed cells stay at the observed cumulative. Lower-triangle
        cells are forward-projected:
        ``cum_mean[i, j] = f_star[k, b] * cum_mean[i, j - 1]``.
        Finite negatives are clipped to ``0``.
    Stage 2 (``cum_sampled``)
        Chain-Markov noisy recursion on the lower triangle:
        ``mu_step = f_star[k, b] * prev`` and
        ``var = sigma2[k] * prev^alpha``. For ``process="normal"``,
        ``cum_sampled[i, j] = mu_step + Normal(0, sqrt(var))``.
        Observed cells equal ``cum_mean``.

    Parameters
    ----------
    loss_obs
        ``(n_cohorts, n_devs)`` observed cumulative loss matrix.
    anchor
        Per-link Mack anchor from :func:`_boot_anchor_cl`.
    B
        Number of bootstrap replicates.
    rng
        ``numpy`` random generator (created from the user seed).
    alpha
        Variance exponent in Mack's ``Var(C_{k+1}|C_k) = sigma^2 C_k^alpha``.
    process
        Stage 2 process distribution. Phase 1 implements only
        ``"normal"``; other values raise :class:`NotImplementedError`.
    _injected_fstar
        Test-only hook. When supplied (shape ``(n_links, B)``), it is
        used instead of drawing ``f_star`` -- making the kernel
        deterministically testable.

    Returns
    -------
    _Stage1Result
        The ``cum_mean`` / ``cum_sampled`` array pair.
    """
    if process != "normal":
        raise NotImplementedError(
            f"process={process!r} not yet implemented in Python "
            f"bootstrap (Phase 1); only process='normal' is supported "
            f"for type='analytical'"
        )

    n_cohorts, n_devs = loss_obs.shape
    n_links = max(n_devs - 1, 0)

    # Last observed dev index per cohort (0-indexed; -1 if no observation).
    obs_mask = np.isfinite(loss_obs)
    has_obs  = obs_mask.any(axis=1)
    last_obs = np.where(
        has_obs,
        n_devs - 1 - obs_mask[:, ::-1].argmax(axis=1),
        -1,
    )

    # ----- Draw f_star[k, b] ------------------------------------------------
    if _injected_fstar is not None:
        f_star = np.asarray(_injected_fstar, dtype=np.float64)
        if f_star.shape != (n_links, B):
            raise ValueError(
                f"_injected_fstar must have shape {(n_links, B)}, "
                f"got {f_star.shape}"
            )
    else:
        f_star = np.empty((n_links, B), dtype=np.float64)
        for k in range(n_links):
            f_hat = anchor.f_hat[k]
            f_var = anchor.f_var[k]
            if np.isfinite(f_var) and f_var > 0.0:
                draws = rng.normal(0.0, 1.0, size=B)
                f_star[k, :] = (
                    f_hat + np.sqrt(f_var) * draws
                    if np.isfinite(f_hat)
                    else np.nan
                )
            else:
                f_star[k, :] = f_hat

    # ----- Stage 1 -- cum_mean: observed cells fixed, lower triangle proj ---
    # Layout (n_cohorts, n_devs, B); observed cells broadcast across B.
    cum_mean = np.full((n_cohorts, n_devs, B), np.nan, dtype=np.float64)
    for j in range(n_devs):
        col = loss_obs[:, j]
        # A cell is observed when last_obs[i] >= j.
        observed_col = (last_obs >= j) & np.isfinite(col)
        if observed_col.any():
            cum_mean[observed_col, j, :] = col[observed_col, None]

    # Forward project the lower triangle dev by dev.
    for j in range(1, n_devs):
        k = j - 1  # link index for dev (j) -> destination dev (j + 1)
        proj_rows = (last_obs >= 0) & (last_obs < j)
        if not proj_rows.any():
            continue
        prev = cum_mean[proj_rows, j - 1, :]
        f_b  = f_star[k, :][None, :]
        f_b  = np.where(np.isfinite(f_b), f_b, 1.0)
        cum_mean[proj_rows, j, :] = f_b * prev
    # Clip finite negatives to zero.
    neg = np.isfinite(cum_mean) & (cum_mean < 0.0)
    cum_mean[neg] = 0.0

    # ----- Stage 2 -- cum_sampled: chain-Markov noisy recursion ------------
    # Vectorised across cohorts at each dev step (loop-carry stays within
    # the dev axis -- prev[:, :] feeds into the next j). RNG draws happen
    # in dev-major order rather than cohort-major; statistical properties
    # are unchanged but bit-exact seed-output mapping differs from any
    # pre-vectorisation snapshot.
    cum_sampled = cum_mean.copy()
    for j in range(1, n_devs):
        k = j - 1
        active = (last_obs >= 0) & (last_obs < j)
        if not active.any():
            continue
        prev = cum_sampled[active, j - 1, :]
        f_b = f_star[k, :]
        f_b = np.where(np.isfinite(f_b), f_b, 1.0)
        mu_step = f_b[None, :] * prev
        s2_k = anchor.sigma2[k] if 0 <= k < n_links else np.nan

        new_sampled = mu_step.copy()
        if np.isfinite(s2_k) and s2_k > 0.0:
            var = s2_k * np.power(np.abs(prev), alpha)
            noisy = (
                np.isfinite(var) & (var > 0.0)
                & np.isfinite(mu_step) & (mu_step > 0.0)
                & np.isfinite(prev) & (prev > 0.0)
            )
            if noisy.any():
                eps = rng.normal(0.0, 1.0, size=int(noisy.sum()))
                new_sampled[noisy] = (
                    mu_step[noisy] + eps * np.sqrt(var[noisy])
                )
        neg_s = np.isfinite(new_sampled) & (new_sampled < 0.0)
        new_sampled[neg_s] = 0.0
        cum_sampled[active, j, :] = new_sampled

    return _Stage1Result(
        cum_mean    = cum_mean,
        cum_sampled = cum_sampled,
        cohorts     = [],   # filled by the caller
        devs        = [],   # filled by the caller
    )


# ---------------------------------------------------------------------------
# Section 3 -- Paradigm-agnostic summary kernel (Pythagorean SE decomposition)
# ---------------------------------------------------------------------------

# Davison & Hinkley (1997) type-1 ordinal percentile probabilities.
_CI_PROBS = (0.025, 0.975)


def _boot_summary_decompose(
    cum_mean:    np.ndarray,
    cum_sampled: np.ndarray,
    quantile_ci: bool = False,
) -> dict[str, np.ndarray]:
    """Per-cell Pythagorean SE decomposition from the array pair.

    Mirrors the R native ``bootstrap_summary_decompose``. Pure numpy.
    For each ``(cohort, dev)`` cell, over the replicates ``b`` where
    *both* ``cum_mean`` and ``cum_sampled`` are finite (``n >= 2``,
    else all NaN):

    * ``mean_proj  = mean(cum_mean)``
    * ``param_se   = std(cum_mean, ddof=1)``     (parameter uncertainty)
    * ``total_se   = std(cum_sampled, ddof=1)``  (full predictive)
    * ``proc_se    = sqrt(max(total_se^2 - param_se^2, 0))``  (process)
    * ``total_cv   = total_se / mean_proj``      (NaN when mean_proj <= 0)

    When ``quantile_ci`` is set, also returns ``ci_lo`` / ``ci_hi`` --
    the 2.5% / 97.5% quantiles of the finite ``cum_sampled`` values via
    ``np.quantile(method="inverted_cdf")`` (= R ``type = 1`` ordinal).

    Parameters
    ----------
    cum_mean, cum_sampled
        ``(n_cohorts, n_devs, B)`` array pair from the Stage 1 kernel.
    quantile_ci
        Whether to also compute empirical percentile CI columns.

    Returns
    -------
    dict
        Flattened ``(n_cohorts * n_devs,)`` arrays keyed by output name.
        Cohort is the fastest-varying axis (column-major over the
        ``(cohort, dev)`` grid).
    """
    n_cohorts, n_devs, B = cum_mean.shape

    both_finite = np.isfinite(cum_mean) & np.isfinite(cum_sampled)
    n_valid = both_finite.sum(axis=2)               # (n_cohorts, n_devs)

    # Masked arrays -- non-finite-pair replicates contribute nothing.
    m_masked = np.where(both_finite, cum_mean, np.nan)
    s_masked = np.where(both_finite, cum_sampled, np.nan)

    enough = n_valid >= 2
    with np.errstate(invalid="ignore", divide="ignore"):
        mean_proj = np.nanmean(m_masked, axis=2)
        param_se  = np.nanstd(m_masked, axis=2, ddof=1)
        total_se  = np.nanstd(s_masked, axis=2, ddof=1)

    mean_proj = np.where(enough, mean_proj, np.nan)
    param_se  = np.where(enough, param_se, np.nan)
    total_se  = np.where(enough, total_se, np.nan)

    var_p   = np.where(enough, total_se ** 2 - param_se ** 2, np.nan)
    var_p   = np.where(np.isfinite(var_p) & (var_p < 0.0), 0.0, var_p)
    proc_se = np.sqrt(var_p)

    with np.errstate(invalid="ignore", divide="ignore"):
        total_cv = np.where(
            enough & (mean_proj > 0.0),
            total_se / mean_proj,
            np.nan,
        )

    # Flatten column-major (cohort fastest) to match the R long-format
    # reshape order.
    out: dict[str, np.ndarray] = {
        "mean_proj": mean_proj.flatten(order="F"),
        "param_se":  param_se.flatten(order="F"),
        "proc_se":   proc_se.flatten(order="F"),
        "total_se":  total_se.flatten(order="F"),
        "total_cv":  total_cv.flatten(order="F"),
    }

    if quantile_ci:
        ci_lo = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
        ci_hi = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
        for i in range(n_cohorts):
            for j in range(n_devs):
                vals = cum_sampled[i, j, :]
                finite = vals[np.isfinite(vals)]
                if finite.size >= 2:
                    # method="inverted_cdf" == R stats::quantile type = 1.
                    q = np.quantile(
                        finite, _CI_PROBS, method="inverted_cdf"
                    )
                    ci_lo[i, j] = q[0]
                    ci_hi[i, j] = q[-1]
        out["ci_lo"] = ci_lo.flatten(order="F")
        out["ci_hi"] = ci_hi.flatten(order="F")

    return out


# ---------------------------------------------------------------------------
# Section 3b -- Deterministic residual machinery (Phase 2)
# ---------------------------------------------------------------------------
#
# Pure functions of the triangle -- no RNG, no resampling. These build the
# residual scaffolding that the (later) resampling kernels consume:
#
#   _boot_fitted_grid     chain-anchored fitted incremental backbone mu_hat
#   _boot_steps_cl        multiplicative CL step closures for the backbone
#   _boot_hat_diag_cl     ODP-GLM hat-matrix diagonal via QR
#   _cell_residuals_cl    ODP Pearson cell residuals + DF/hat correction
#   _cell_residuals_ed    ED-paradigm Pearson cell residuals
#   _link_residuals_cl    Mack standardized link residuals
#   _build_pool_cell      cell-residual pool assembly (pool_id by pooling)
#   _build_pool_link      link-residual pool assembly (pool_id by pooling)
#
# References:
#   Renshaw & Verrall (1998)  -- ODP MLE / CL equivalence (fitted backbone).
#   England & Verrall (1999)  -- Pearson cell residuals, DF correction.
#   England & Verrall (2002)  -- hat-matrix leverage adjustment (Addendum).
#   Pinheiro et al. (2003)    -- standardized link residuals.
#   Shapland (2010, Sec.4.2)  -- corner-cell zero-drop, residual centering.

_RESID_EPS = float(np.finfo(np.float64).eps)


@dataclass
class _CellResiduals:
    """Deterministic ODP / ED Pearson cell-residual table for one group.

    Attributes
    ----------
    cohort, dev
        1-d arrays of equal length -- the ``(cohort, dev)`` cell key of
        each residual. ``dev`` is the destination dev of the perturbed
        increment (for ED this equals the link ``ata_to``).
    residual
        The *adjusted* Pearson residual (post hat / DF correction).
        ``np.nan`` for cells excluded by the corner-drop rule.
    mu_hat
        The fitted incremental mean ``mu_hat_ij`` of that cell.
    phi
        ODP dispersion ``sum(r_raw^2) / (n_obs - p)`` from the *raw*
        (pre-adjustment) residuals. Scalar.
    n_obs
        Count of finite raw residuals (the ODP ``n``).
    p
        Parameter count used in the dispersion denominator.
    """

    cohort:   np.ndarray
    dev:      np.ndarray
    residual: np.ndarray
    mu_hat:   np.ndarray
    phi:      float
    n_obs:    int
    p:        int


@dataclass
class _LinkResiduals:
    """Mack standardized link-residual table for one group.

    Attributes
    ----------
    cohort
        Cohort identifier of each link row.
    ata_from, ata_to
        1-indexed source / destination dev of each link.
    residual
        Mack standardized residual
        ``(loss_to - f_hat_k loss_from) / sqrt(sigma2_k loss_from)``;
        ``np.nan`` where the standardizing scale is undefined.
    """

    cohort:    np.ndarray
    ata_from:  np.ndarray
    ata_to:    np.ndarray
    residual:  np.ndarray


@dataclass
class _ResidualPool:
    """A residual pool with per-residual ``pool_id`` membership.

    The resampling kernel draws, per target cell, uniformly from the
    sub-pool whose ``pool_id`` matches. Non-finite and exact-zero
    residuals are already dropped (Shapland 2010 corner-cell rule).

    Attributes
    ----------
    cohort
        Cohort identifier of each pooled residual.
    key
        For cell pools the destination ``dev``; for link pools the
        ``ata_to`` -- the per-link axis the pool may be split on.
    residual
        The pooled residual value (per-group de-meaned when
        ``demean=True``).
    pool_id
        String pool membership label. ``"separated"`` -> one id per
        link/dev, ``"pooled"`` -> one id per group, ``"tail_pooled"``
        -> per-link ids before the cut, a single ``"...|POST"`` id
        after.
    """

    cohort:   np.ndarray
    key:      np.ndarray
    residual: np.ndarray
    pool_id:  np.ndarray

    def __len__(self) -> int:
        return int(self.residual.shape[0])


def _boot_steps_cl(f_by_to: np.ndarray):
    """Multiplicative CL step closures for :func:`_boot_fitted_grid`.

    Mirrors R's ``.boot_steps_cl``. ``f_by_to[j]`` is the chain-ladder
    factor applied at the link ``(dev j-1 -> dev j)`` -- i.e. indexed by
    *destination* dev (0-indexed). The forward step multiplies; the
    backward step divides (skipping non-finite / non-positive factors,
    leaving ``cur`` unchanged).

    Returns
    -------
    (fwd, bwd)
        A pair of ``(cur, i, j) -> float`` closures.
    """

    def fwd(cur: float, i: int, j: int) -> float:
        f_k = f_by_to[j]
        return f_k * cur if np.isfinite(f_k) else cur

    def bwd(cur: float, i: int, j: int) -> float:
        f_k = f_by_to[j + 1]
        if np.isfinite(f_k) and f_k > 0.0:
            return cur / f_k
        return cur

    return fwd, bwd


def _boot_fitted_grid(
    mat_obs:      np.ndarray,
    last_obs_idx: np.ndarray,
    step_fwd,
    step_bwd,
) -> np.ndarray:
    """Chain-anchored fitted incremental backbone ``mu_hat_ij``.

    Mirrors R's ``.boot_fitted_grid``. For each cohort ``i`` with last
    observed dev index ``last_j = last_obs_idx[i]``:

    1. Anchor the fitted *cumulative* at the observed cumulative:
       ``c_hat[i, last_j] = mat_obs[i, last_j]``.
    2. Roll forward (``j > last_j``) with ``step_fwd``.
    3. Roll backward (``j < last_j``) with ``step_bwd``.
    4. Row-difference ``c_hat`` to fitted incrementals ``mu_hat``
       (``mu_hat[:, 0] = c_hat[:, 0]``).

    By construction the upper-triangle partial sums of ``mu_hat`` match
    the observed cumulative exactly -- the Renshaw-Verrall (1998) ODP MLE
    / chain-ladder equivalence.

    Parameters
    ----------
    mat_obs
        ``(n_coh, n_dev)`` observed *cumulative* matrix (``np.nan`` where
        unobserved).
    last_obs_idx
        ``(n_coh,)`` 0-indexed last observed dev per cohort; ``-1`` for a
        cohort with no observation.
    step_fwd, step_bwd
        Paradigm-specific step closures, e.g. from :func:`_boot_steps_cl`.

    Returns
    -------
    np.ndarray
        ``(n_coh, n_dev)`` fitted incremental matrix.
    """
    n_coh, n_dev = mat_obs.shape
    c_hat = np.full((n_coh, n_dev), np.nan, dtype=np.float64)

    for i in range(n_coh):
        last_j = int(last_obs_idx[i])
        if last_j < 0:
            continue
        base = mat_obs[i, last_j]
        if not np.isfinite(base):
            continue
        c_hat[i, last_j] = base
        if last_j < n_dev - 1:
            cur = float(base)
            for j in range(last_j + 1, n_dev):
                cur = step_fwd(cur, i, j)
                c_hat[i, j] = cur
        if last_j > 0:
            cur = float(base)
            for j in range(last_j - 1, -1, -1):
                cur = step_bwd(cur, i, j)
                c_hat[i, j] = cur

    mu_hat = np.full((n_coh, n_dev), np.nan, dtype=np.float64)
    mu_hat[:, 0] = c_hat[:, 0]
    if n_dev >= 2:
        for j in range(1, n_dev):
            mu_hat[:, j] = c_hat[:, j] - c_hat[:, j - 1]
    return mu_hat


def _boot_hat_diag_cl(
    mu_hat_obs: np.ndarray,
    coh_idx:    np.ndarray,
    dev_idx:    np.ndarray,
    n_coh:      int,
    n_dev:      int,
) -> np.ndarray:
    """ODP-GLM hat-matrix diagonal ``h_ii`` via QR.

    Mirrors R's ``.boot_hat_diag_cl``. The ODP GLM design encodes
    ``log(mu_ij) = alpha_i + beta_j`` with the corner constraint
    ``beta_1 = 0`` (the dev-1 indicator is dropped). The design has
    ``p = n_coh + n_dev - 1`` columns: ``n_coh`` cohort indicators
    followed by ``n_dev - 1`` dev indicators (dev ``2..n_dev``).

    With weight ``W = diag(mu_hat)``, the hat diagonal is
    ``h = diag(W^{1/2} X (X' W X)^{-1} X' W^{1/2})``. It is computed
    stably as ``rowSums(Q^2)`` from the (thin) QR of ``W^{1/2} X``,
    using only the leading ``rank`` columns of ``Q`` (rank-deficiency
    guard).

    When ``n_dev < 2`` the design degenerates (no dev indicators) and
    every cell gets ``h = 1`` -- the caller's corner-drop then excludes
    everything and falls back to the DF correction.

    Parameters
    ----------
    mu_hat_obs
        ``(n,)`` fitted incremental means of the observed cells.
    coh_idx, dev_idx
        ``(n,)`` 1-indexed cohort / dev position of each observed cell.
    n_coh, n_dev
        Triangle dimensions.

    Returns
    -------
    np.ndarray
        ``(n,)`` hat-matrix diagonal, each value in ``[0, 1]``.
    """
    n = int(mu_hat_obs.shape[0])
    if n == 0:
        return np.empty(0, dtype=np.float64)
    if n_dev < 2:
        return np.ones(n, dtype=np.float64)

    p = n_coh + n_dev - 1
    X = np.zeros((n, p), dtype=np.float64)
    for k in range(n):
        X[k, int(coh_idx[k]) - 1] = 1.0
        d = int(dev_idx[k])
        if d >= 2:
            X[k, n_coh + d - 2] = 1.0

    w_sqrt = np.sqrt(np.maximum(mu_hat_obs, 0.0))
    whx = w_sqrt[:, None] * X

    # Thin QR; rank-deficiency guard -- keep only the leading `rank`
    # columns of Q (matches R's qr.Q(qr_obj)[, seq_len(rank)]).
    q, r = np.linalg.qr(whx)
    diag_r = np.abs(np.diag(r))
    tol = diag_r.max() * max(whx.shape) * _RESID_EPS if diag_r.size else 0.0
    rank = int(np.sum(diag_r > tol))
    if rank < q.shape[1]:
        q = q[:, :rank]
    return np.sum(q ** 2, axis=1)


def _build_obs_matrix_from_df(
    df:     pl.DataFrame,
    target: str,
) -> tuple[np.ndarray, list, list]:
    """Build a ``(n_coh, n_dev)`` observed cumulative matrix.

    Rows are cohorts (sorted ascending), columns dev (sorted ascending);
    unobserved cells are ``np.nan``. Shared scaffolding for the cell
    residual helpers -- mirrors R's per-group ``mat_obs`` build.
    """
    df = df.sort(["cohort", "dev"])
    cohorts = sorted(df["cohort"].unique().to_list())
    devs    = sorted(df["dev"].unique().to_list())
    n_coh, n_dev = len(cohorts), len(devs)

    coh_pos = {c: i for i, c in enumerate(cohorts)}
    dev_pos = {d: j for j, d in enumerate(devs)}

    mat = np.full((n_coh, n_dev), np.nan, dtype=np.float64)
    for row in df.iter_rows(named=True):
        v = row[target]
        if v is not None:
            mat[coh_pos[row["cohort"]], dev_pos[row["dev"]]] = float(v)
    return mat, cohorts, devs


def _last_obs_idx(mat_obs: np.ndarray) -> np.ndarray:
    """0-indexed last observed dev per cohort row (``-1`` if none)."""
    finite = np.isfinite(mat_obs)
    has = finite.any(axis=1)
    n_dev = mat_obs.shape[1]
    last = np.where(
        has,
        n_dev - 1 - finite[:, ::-1].argmax(axis=1),
        -1,
    )
    return last.astype(np.int64)


def _cell_residuals_cl(
    triangle_df: pl.DataFrame,
    anchor:      _Anchor,
    target:      str  = "loss",
    hat_adj:     bool = True,
) -> _CellResiduals:
    """ODP Pearson cell residuals on incremental cells (single group).

    Mirrors R's ``.boot_cell_residuals_one_cl`` (England-Verrall
    1999/2002; ODP-GLM equivalence via Renshaw-Verrall 1998).

    The chain-anchored fitted backbone ``mu_hat`` comes from
    :func:`_boot_fitted_grid` using the per-link ``f_hat`` of ``anchor``.
    Observed incrementals are ``X_ij = C_ij - C_{i,j-1}`` (``X_i1 =
    C_i1``). The raw Pearson residual is

        ``r_raw = (X_obs - mu_hat) / sqrt(max(|mu_hat|, eps))``

    set to ``np.nan`` where non-finite or ``mu_hat <= 0``. The ODP
    dispersion ``phi = sum(r_raw^2) / (n_obs - p)`` (``p = n_coh +
    n_dev - 1``) is taken from these *raw* residuals -- it is invariant
    to the stage-correction choice.

    Exactly one stage correction is then applied:

    * ``hat_adj=True``  -- ``r_adj = r_raw / sqrt(max(1 - h_ii, eps))``
      with ``h_ii`` the ODP-GLM hat diagonal; cells with
      ``h_ii >= 1 - eps`` (corners) are dropped to ``np.nan``.
    * ``hat_adj=False`` -- ``r_adj = r_raw * sqrt(n_obs / (n_obs - p))``,
      the simple degrees-of-freedom factor.

    Parameters
    ----------
    triangle_df
        Single-group Triangle DataFrame -- carries the cumulative
        ``target`` column plus ``cohort`` / ``dev``.
    anchor
        Per-link Mack anchor from :func:`_boot_anchor_cl` (only
        ``f_hat`` / ``ata_to`` are read).
    target
        Cumulative metric column, ``"loss"`` or ``"premium"``.
    hat_adj
        Which stage correction to apply (see above).

    Returns
    -------
    _CellResiduals
        Per-cell adjusted residuals + ODP dispersion ``phi``.
    """
    mat_obs, cohorts, devs = _build_obs_matrix_from_df(triangle_df, target)
    n_coh, n_dev = mat_obs.shape
    last_obs = _last_obs_idx(mat_obs)

    # f indexed by 0-indexed destination dev.
    f_by_to = np.full(n_dev, np.nan, dtype=np.float64)
    dev_pos = {d: j for j, d in enumerate(devs)}
    for k in range(len(anchor.ata_to)):
        j = dev_pos.get(int(anchor.ata_to[k]))
        if j is not None:
            f_by_to[j] = anchor.f_hat[k]

    fwd, bwd = _boot_steps_cl(f_by_to)
    mu_hat_grid = _boot_fitted_grid(mat_obs, last_obs, fwd, bwd)

    # Observed incrementals X_ij.
    x_inc = np.full((n_coh, n_dev), np.nan, dtype=np.float64)
    x_inc[:, 0] = mat_obs[:, 0]
    if n_dev >= 2:
        for j in range(1, n_dev):
            x_inc[:, j] = mat_obs[:, j] - mat_obs[:, j - 1]

    obs_mask = np.isfinite(mat_obs) & np.isfinite(mu_hat_grid)
    if not obs_mask.any():
        return _CellResiduals(
            cohort   = np.array([], dtype=object),
            dev      = np.array([], dtype=np.int64),
            residual = np.array([], dtype=np.float64),
            mu_hat   = np.array([], dtype=np.float64),
            phi      = np.nan,
            n_obs    = 0,
            p        = n_coh + n_dev - 1,
        )

    # which() with arr.ind = TRUE iterates column-major in R, but the
    # output ordering only needs to be self-consistent here; row-major
    # is fine since coh/dev are carried per residual.
    coh_pos, dev_pos_arr = np.where(obs_mask)
    coh_idx = coh_pos + 1            # 1-indexed for the hat design
    dev_idx = dev_pos_arr + 1
    mu_obs  = mu_hat_grid[obs_mask]
    x_obs   = x_inc[obs_mask]

    denom = np.sqrt(np.maximum(np.abs(mu_obs), _RESID_EPS))
    with np.errstate(invalid="ignore", divide="ignore"):
        r_raw = (x_obs - mu_obs) / denom
    bad = ~np.isfinite(r_raw) | (mu_obs <= 0.0)
    r_raw = np.where(bad, np.nan, r_raw)

    n_obs_phi = int(np.isfinite(r_raw).sum())
    p_phi     = n_coh + n_dev - 1
    df_phi    = n_obs_phi - p_phi
    phi_val   = (
        float(np.nansum(r_raw ** 2) / df_phi) if df_phi > 0 else np.nan
    )

    if hat_adj:
        h = _boot_hat_diag_cl(mu_obs, coh_idx, dev_idx, n_coh, n_dev)
        eps = 1e-10
        drop = ~np.isfinite(h) | (h >= 1.0 - eps)
        with np.errstate(invalid="ignore", divide="ignore"):
            r_adj = r_raw / np.sqrt(np.maximum(1.0 - h, eps))
        r_adj = np.where(drop, np.nan, r_adj)
    else:
        df_factor = np.sqrt(n_obs_phi / df_phi) if df_phi > 0 else 1.0
        r_adj = r_raw * df_factor

    return _CellResiduals(
        cohort   = np.array([cohorts[i] for i in coh_pos], dtype=object),
        dev      = np.array([devs[j] for j in dev_pos_arr], dtype=np.int64),
        residual = r_adj.astype(np.float64),
        mu_hat   = mu_obs.astype(np.float64),
        phi      = phi_val,
        n_obs    = n_obs_phi,
        p        = p_phi,
    )


def _cell_residuals_ed(
    link_df:    pl.DataFrame,
    from_col:   str = "premium_from",
) -> _CellResiduals:
    """ED-paradigm Pearson cell residuals (single group).

    Mirrors R's ``.boot_cell_residuals_ed``. For each link cell
    ``(cohort, ata_to)`` of a dual-variable Link table:

        ``g_hat  = sum(loss_delta) / sum(premium_from)``   (per link)
        ``mu_ed  = g_hat * premium_from``
        ``r_raw  = (loss_delta - mu_ed) / sqrt(mu_ed)``

    where ``g_hat`` is the volume-weighted per-link intensity over
    observed link rows. Cells with non-positive ``mu_ed`` or non-finite
    endpoints are excluded (residual ``np.nan``).

    The ED dispersion is ``phi_ed = sum(r_raw^2) / (n_obs - n_links)``
    -- one parameter ``g_k`` per link. No hat / DF adjustment is applied
    (the ED design differs from the ODP GLM; the raw Pearson residual is
    used directly).

    The output uses the shared cell schema: ``dev = ata_to`` (the
    increment's own destination dev) and ``mu_hat = mu_ed``.

    Parameters
    ----------
    link_df
        Single-group dual-mode Link DataFrame -- must carry
        ``loss_delta`` and the from-side cumulative premium column
        named by ``from_col``.
    from_col
        Name of the from-side cumulative premium column. Both the R
        sibling and the Python ``Link`` emit ``premium_from`` -- the
        default matches.

    Returns
    -------
    _CellResiduals
        Per-cell ED Pearson residuals + dispersion ``phi``. ``p`` is the
        number of distinct links used.
    """
    if from_col not in link_df.columns:
        raise ValueError(
            f"link DataFrame is missing the from-side premium column "
            f"({from_col!r}); columns: {link_df.columns}"
        )

    df = link_df.select(
        ["cohort", "ata_to", "loss_delta", from_col]
    ).rename({from_col: "premium_from"})

    cohort       = df["cohort"].to_numpy()
    ata_to       = df["ata_to"].to_numpy().astype(np.float64)
    loss_delta   = df["loss_delta"].to_numpy().astype(np.float64)
    premium_from = df["premium_from"].to_numpy().astype(np.float64)

    # Per-link g_hat (volume-weighted) over observed link rows.
    g_ok = (
        np.isfinite(loss_delta)
        & np.isfinite(premium_from)
        & (premium_from > 0.0)
    )
    g_hat = np.full(loss_delta.shape, np.nan, dtype=np.float64)
    uniq_links = np.unique(ata_to[np.isfinite(ata_to)])
    for lk in uniq_links:
        sel = g_ok & (ata_to == lk)
        s_from = premium_from[sel].sum()
        if s_from > 0.0:
            g_hat[ata_to == lk] = loss_delta[sel].sum() / s_from

    mu_ed = g_hat * premium_from
    with np.errstate(invalid="ignore", divide="ignore"):
        r_raw = (loss_delta - mu_ed) / np.sqrt(mu_ed)
    bad = ~(np.isfinite(loss_delta) & np.isfinite(mu_ed) & (mu_ed > 0.0))
    residual = np.where(bad, np.nan, r_raw)

    fin = np.isfinite(residual)
    n_obs   = int(fin.sum())
    n_links = int(np.unique(ata_to[fin]).size) if n_obs else 0
    phi_val = (
        float(np.nansum(residual[fin] ** 2) / (n_obs - n_links))
        if n_obs > n_links
        else np.nan
    )

    return _CellResiduals(
        cohort   = cohort,
        dev      = ata_to.astype(np.int64),
        residual = residual.astype(np.float64),
        mu_hat   = mu_ed.astype(np.float64),
        phi      = phi_val,
        n_obs    = n_obs,
        p        = n_links,
    )


def _link_residuals_cl(
    link_df: pl.DataFrame,
    anchor:  _Anchor,
) -> _LinkResiduals:
    """Mack standardized link residuals (single group).

    Mirrors R's ``.boot_attach_residuals_cl`` (Pinheiro et al. 2003).
    For each Link row, with the per-link Mack anchor ``f_hat_k`` /
    ``sigma2_k`` looked up by ``ata_from`` / ``ata_to``:

        ``r_ik = (loss_to - f_hat_k loss_from)
                 / sqrt(sigma2_k loss_from)``

    set to ``np.nan`` unless ``loss_from > 0``, ``sigma2_k > 0`` and
    both ``loss_to`` / ``f_hat_k`` are finite.

    Parameters
    ----------
    link_df
        Single-group Link DataFrame -- carries ``cohort`` / ``ata_from``
        / ``ata_to`` / ``loss_from`` / ``loss_to``.
    anchor
        Per-link Mack anchor from :func:`_boot_anchor_cl`.

    Returns
    -------
    _LinkResiduals
        Per-row standardized link residuals.
    """
    cohort    = link_df["cohort"].to_numpy()
    ata_from  = link_df["ata_from"].to_numpy().astype(np.int64)
    ata_to    = link_df["ata_to"].to_numpy().astype(np.int64)
    loss_from = link_df["loss_from"].to_numpy().astype(np.float64)
    loss_to   = link_df["loss_to"].to_numpy().astype(np.float64)

    # Anchor lookup keyed by (ata_from, ata_to).
    anc_key = {
        (int(anchor.ata_from[k]), int(anchor.ata_to[k])): k
        for k in range(len(anchor.ata_from))
    }
    f_row  = np.full(cohort.shape, np.nan, dtype=np.float64)
    s2_row = np.full(cohort.shape, np.nan, dtype=np.float64)
    for r in range(cohort.shape[0]):
        k = anc_key.get((int(ata_from[r]), int(ata_to[r])))
        if k is not None:
            f_row[r]  = anchor.f_hat[k]
            s2_row[r] = anchor.sigma2[k]

    ok = (
        np.isfinite(loss_from) & (loss_from > 0.0)
        & np.isfinite(s2_row)  & (s2_row > 0.0)
        & np.isfinite(loss_to) & np.isfinite(f_row)
    )
    with np.errstate(invalid="ignore", divide="ignore"):
        r = (loss_to - f_row * loss_from) / np.sqrt(s2_row * loss_from)
    residual = np.where(ok, r, np.nan)

    return _LinkResiduals(
        cohort   = cohort,
        ata_from = ata_from,
        ata_to   = ata_to,
        residual = residual.astype(np.float64),
    )


def _tail_cut_auto(key: np.ndarray, min_pool: int) -> float:
    """Smallest link key whose residual count drops below ``min_pool``.

    Mirrors the ``tail = "auto"`` cut of R's pool builders: count
    residuals per link key (ascending), return the first key whose
    count ``< min_pool``; ``np.nan`` when every key meets the threshold.
    """
    if key.size == 0:
        return np.nan
    uniq = np.sort(np.unique(key))
    for u in uniq:
        if int(np.sum(key == u)) < min_pool:
            return float(u)
    return np.nan


def _assign_pool_id(
    key:      np.ndarray,
    pooling:  str,
    *,
    cut:      float = np.nan,
) -> np.ndarray:
    """Assign a ``pool_id`` string per residual for a single group.

    Mirrors the ``pool_id`` logic of R's ``.boot_build_pool_cell_cl`` /
    ``.boot_build_pool_cl`` for the single-group case (the group key
    prefix is empty -- ``""``):

    * ``"pooled"``      -> every residual gets ``"all"``.
    * ``"separated"``   -> ``"|<key>"`` per link/dev.
    * ``"tail_pooled"`` -> ``"|<key>"`` before ``cut``, ``"|POST"`` at
      or after ``cut`` (when ``cut`` is finite).
    """
    n = key.shape[0]
    if pooling == "pooled":
        return np.full(n, "all", dtype=object)
    if pooling == "separated":
        return np.array([f"|{int(k)}" for k in key], dtype=object)
    if pooling == "tail_pooled":
        out = np.empty(n, dtype=object)
        is_post = np.isfinite(cut) & (key >= cut)
        for i in range(n):
            out[i] = "|POST" if is_post[i] else f"|{int(key[i])}"
        return out
    raise ValueError(
        f"pooling must be 'pooled', 'separated' or 'tail_pooled', "
        f"got {pooling!r}"
    )


def _build_pool_cell(
    cell:     _CellResiduals,
    pooling:  str   = "pooled",
    tail:     str   = "auto",
    min_pool: int   = 5,
    maturity: float | None = None,
    demean:   bool  = True,
) -> _ResidualPool:
    """Assemble a cell-residual pool with ``pool_id`` (single group).

    Mirrors R's ``.boot_build_pool_cell_cl``. Steps:

    1. **Drop** non-finite *and* exact-zero residuals. Exact zeros are
       the per-cohort corner cells (fitted == observed); resampling a
       zero would make the pseudo cell deterministic and artificially
       narrow the bootstrap distribution (Shapland 2010, Sec.4.2).
    2. **De-mean** (when ``demean=True``): subtract the residual mean so
       the pool is zero-mean (Shapland 2010 discusses this as one
       option).
    3. **Assign ``pool_id``** per ``pooling`` -- keyed on ``dev``:
       ``"pooled"`` (one pool), ``"separated"`` (per-dev), or
       ``"tail_pooled"`` (per-dev before a cut, single ``"POST"``
       after). The cut is the smallest ``dev`` with count ``< min_pool``
       (``tail="auto"``) or the ``maturity`` change point
       (``tail="maturity"``).

    Parameters
    ----------
    cell
        Cell residuals from :func:`_cell_residuals_cl` /
        :func:`_cell_residuals_ed`.
    pooling
        Pool-grouping strategy.
    tail
        Tail-cut rule for ``pooling="tail_pooled"``.
    min_pool
        Minimum per-dev pool count for ``tail="auto"``.
    maturity
        Maturity change point (a dev value) for ``tail="maturity"``.
    demean
        Whether to subtract the per-group residual mean.

    Returns
    -------
    _ResidualPool
        The pooled residuals with their ``pool_id`` membership.
    """
    keep = np.isfinite(cell.residual) & (cell.residual != 0.0)
    cohort = cell.cohort[keep]
    dev    = cell.dev[keep].astype(np.float64)
    resid  = cell.residual[keep].astype(np.float64)

    if demean and resid.size:
        resid = resid - resid.mean()

    cut = np.nan
    if pooling == "tail_pooled":
        if tail == "maturity":
            cut = np.nan if maturity is None else float(maturity)
        else:
            cut = _tail_cut_auto(dev, int(min_pool))

    pool_id = _assign_pool_id(dev, pooling, cut=cut)

    return _ResidualPool(
        cohort   = cohort,
        key      = dev.astype(np.int64),
        residual = resid,
        pool_id  = pool_id,
    )


def _build_pool_link(
    link:     _LinkResiduals,
    pooling:  str   = "pooled",
    tail:     str   = "auto",
    min_pool: int   = 5,
    maturity: float | None = None,
) -> _ResidualPool:
    """Assemble a link-residual pool with ``pool_id`` (single group).

    Mirrors R's ``.boot_build_pool_cl``. Steps:

    1. **Drop** non-finite residuals. (Link residuals do not get the
       exact-zero drop -- only the cell path has corner zeros; this
       matches R, which filters ``is.finite(residual)`` only.)
    2. **Assign ``pool_id``** per ``pooling`` -- keyed on ``ata_to``:
       ``"pooled"`` (one pool), ``"separated"`` (per-link), or
       ``"tail_pooled"`` (per-link before a cut, single ``"POST"``
       after). The cut is the smallest ``ata_to`` with count
       ``< min_pool`` (``tail="auto"``) or the ``maturity`` change
       point (``tail="maturity"``).

    Link residuals are *not* de-meaned (R applies ``demean`` only to the
    cell path).

    Parameters
    ----------
    link
        Link residuals from :func:`_link_residuals_cl`.
    pooling, tail, min_pool, maturity
        As for :func:`_build_pool_cell`, keyed on ``ata_to``.

    Returns
    -------
    _ResidualPool
        The pooled residuals with their ``pool_id`` membership.
    """
    keep = np.isfinite(link.residual)
    cohort = link.cohort[keep]
    ata_to = link.ata_to[keep].astype(np.float64)
    resid  = link.residual[keep].astype(np.float64)

    cut = np.nan
    if pooling == "tail_pooled":
        if tail == "maturity":
            cut = np.nan if maturity is None else float(maturity)
        else:
            cut = _tail_cut_auto(ata_to, int(min_pool))

    pool_id = _assign_pool_id(ata_to, pooling, cut=cut)

    return _ResidualPool(
        cohort   = cohort,
        key      = ata_to.astype(np.int64),
        residual = resid,
        pool_id  = pool_id,
    )


# ---------------------------------------------------------------------------
# Section 3c -- Stage-1 + Stage-2 resampling kernels (Phase 2 Wave 2)
# ---------------------------------------------------------------------------
#
# Six fused phases per group, per paradigm (CL cell / CL link / ED cell /
# SA cell / parametric):
#
#   (a) place a perturbed / parametric increment in the upper triangle
#   (b) cumsum along dev per cohort x replicate
#   (c) mask the lower triangle to NaN
#   (d) refit f*/g* per link per replicate (BOTH cells finite; anchor
#       fallback on a non-positive denominator)
#   (e) deterministic forward-projection of the lower triangle -> cum_mean
#   (f) noisy forward simulation of the lower triangle -> cum_sampled
#
# These mirror the R native kernels in src/bootstrap_cl.c /
# src/bootstrap_ed.c / src/bootstrap_sa.c. They are vectorised over the
# B axis with numpy; the per-dev chain recursion runs a sequential loop.
#
# References:
#   Mack (1993, ASTIN Bull 23/2)        -- sigma_k^2 chain-Markov noise.
#   England & Verrall (1999, IME 25/3)  -- ODP cell-residual / parametric.
#   Renshaw & Verrall (1998)            -- ODP MLE / CL equivalence.
#   Pinheiro et al. (2003)              -- standardized link residuals.
#   Shapland (2010, Sec.4.2)            -- residual centering / zero-drop.


def _refit_fstar(
    cum:         np.ndarray,
    link_to_idx: np.ndarray,
    f_hat:       np.ndarray,
) -> np.ndarray:
    """Volume-weighted CL ``f_star`` refit per link, per replicate.

    Mirrors R's ``bootstrap_refit_cl_fstar``. For each link ``k``,
    ``f_star[k, b] = sum(cum_to) / sum(cum_from)`` over the cohorts
    whose pseudo cumulative is finite at *both* endpoints (the lower
    triangle has already been masked to NaN). When the destination dev
    is undefined or the denominator sum is non-positive, the factor
    falls back to the anchor ``f_hat[k]``.

    Parameters
    ----------
    cum
        ``(n_coh, n_dev, B)`` masked pseudo cumulative array.
    link_to_idx
        ``(n_links,)`` 1-indexed destination dev column of each link.
    f_hat
        ``(n_links,)`` anchor chain-ladder factors.

    Returns
    -------
    np.ndarray
        ``(n_links, B)`` per-replicate refit factors.
    """
    n_coh, n_dev, B = cum.shape
    n_links = link_to_idx.shape[0]
    f_star = np.empty((n_links, B), dtype=np.float64)
    for k in range(n_links):
        to_col_1 = int(link_to_idx[k])
        if to_col_1 < 2:
            f_star[k, :] = f_hat[k]
            continue
        from_col = to_col_1 - 2
        to_col   = to_col_1 - 1
        c_from = cum[:, from_col, :]
        c_to   = cum[:, to_col, :]
        ok  = np.isfinite(c_from) & np.isfinite(c_to)
        num = np.where(ok, c_to,   0.0).sum(axis=0)
        den = np.where(ok, c_from, 0.0).sum(axis=0)
        good = np.isfinite(den) & (den > 0.0)
        f_star[k, :] = np.where(good, num / np.where(good, den, 1.0),
                                f_hat[k])
    return f_star


def _refit_gstar(
    cum:           np.ndarray,
    premium_proj:  np.ndarray,
    link_to_idx:   np.ndarray,
    g_hat:         np.ndarray,
) -> np.ndarray:
    """Volume-weighted ED ``g_star`` refit per link, per replicate.

    Mirrors R's ``bootstrap_refit_ed_gstar``. For each link ``k``,
    ``g_star[k, b] = sum(cum_to - cum_from) / sum(premium_from)`` -- the
    numerator over the cohorts whose pseudo cumulative is finite at
    *both* endpoints, and the denominator summing ``premium_proj`` over
    the *same* cohort set (the fixed premium projection). A non-positive
    denominator falls back to the anchor ``g_hat[k]``.

    Parameters
    ----------
    cum
        ``(n_coh, n_dev, B)`` masked pseudo cumulative array.
    premium_proj
        ``(n_coh, n_dev)`` fixed projected premium matrix.
    link_to_idx
        ``(n_links,)`` 1-indexed destination dev column of each link.
    g_hat
        ``(n_links,)`` anchor ED intensities.

    Returns
    -------
    np.ndarray
        ``(n_links, B)`` per-replicate refit intensities.
    """
    n_coh, n_dev, B = cum.shape
    n_links = link_to_idx.shape[0]
    g_star = np.empty((n_links, B), dtype=np.float64)
    for k in range(n_links):
        to_col_1 = int(link_to_idx[k])
        if to_col_1 < 2:
            g_star[k, :] = g_hat[k]
            continue
        from_col = to_col_1 - 2
        to_col   = to_col_1 - 1
        c_from = cum[:, from_col, :]
        c_to   = cum[:, to_col, :]
        p_from = premium_proj[:, from_col][:, None]
        ok = (
            np.isfinite(c_from) & np.isfinite(c_to)
            & np.isfinite(p_from) & (p_from > 0.0)
        )
        num = np.where(ok, c_to - c_from, 0.0).sum(axis=0)
        den = np.where(ok, np.broadcast_to(p_from, c_from.shape),
                       0.0).sum(axis=0)
        good = np.isfinite(den) & (den > 0.0)
        g_star[k, :] = np.where(good, num / np.where(good, den, 1.0),
                                g_hat[k])
    return g_star


def _fwd_proj_cl(
    cum:         np.ndarray,
    f_star:      np.ndarray,
    last_obs:    np.ndarray,
    k_idx_by_j:  np.ndarray,
) -> None:
    """Multiplicative CL lower-triangle projection (in place).

    Mirrors R's ``bootstrap_fwd_proj_cl_and_clip``. For each dev ``j``,
    cohorts with ``last_obs[i] < j`` get ``cum[i, j] = f_star[k] *
    cum[i, j-1]`` where ``k = k_idx_by_j[j] - 1`` (``-1`` -> carry the
    previous dev forward unchanged). Finite negatives are clipped to 0.
    """
    n_coh, n_dev, B = cum.shape
    for j in range(1, n_dev):
        k_1 = int(k_idx_by_j[j])
        k   = k_1 - 1 if k_1 != -1 else -1
        proj = (last_obs >= 0) & (last_obs < j)
        if not proj.any():
            continue
        prev = cum[proj, j - 1, :]
        if k < 0:
            cum[proj, j, :] = prev
        else:
            f_b = f_star[k, :][None, :]
            f_b = np.where(np.isfinite(f_b), f_b, 1.0)
            cum[proj, j, :] = f_b * prev
    neg = np.isfinite(cum) & (cum < 0.0)
    cum[neg] = 0.0


def _fwd_proj_ed(
    cum:           np.ndarray,
    g_star:        np.ndarray,
    premium_proj:  np.ndarray,
    last_obs:      np.ndarray,
    k_idx_by_j:    np.ndarray,
) -> None:
    """Additive ED lower-triangle projection (in place).

    Mirrors R's ``bootstrap_fwd_proj_ed_and_clip``. For each dev ``j``,
    cohorts with ``last_obs[i] < j`` get ``cum[i, j] = cum[i, j-1] +
    g_star[k] * premium_proj[i, j-1]``. Finite negatives clipped to 0.
    """
    n_coh, n_dev, B = cum.shape
    for j in range(1, n_dev):
        k_1 = int(k_idx_by_j[j])
        k   = k_1 - 1 if k_1 != -1 else -1
        proj = (last_obs >= 0) & (last_obs < j)
        if not proj.any():
            continue
        prev = cum[proj, j - 1, :]
        if k < 0:
            cum[proj, j, :] = prev
        else:
            g_b = g_star[k, :][None, :]
            g_b = np.where(np.isfinite(g_b), g_b, 0.0)
            p_prev = premium_proj[proj, j - 1][:, None]
            inc = np.where(np.isfinite(p_prev), g_b * p_prev, 0.0)
            cum[proj, j, :] = prev + inc
    neg = np.isfinite(cum) & (cum < 0.0)
    cum[neg] = 0.0


def _fwd_proj_sa(
    cum:           np.ndarray,
    f_star:        np.ndarray,
    g_star:        np.ndarray,
    premium_proj:  np.ndarray,
    last_obs:      np.ndarray,
    k_idx_by_j:    np.ndarray,
    mat_k:         np.ndarray,
) -> None:
    """Stage-adaptive lower-triangle projection (in place).

    Mirrors R's ``bootstrap_sa_fwd_proj_and_clip``. Per cohort the stage
    switches at ``mat_k[i]`` -- the 1-indexed from-dev where CL begins:
    for to-dev ``j``, the link is CL when ``j >= mat_k[i]`` (and
    ``mat_k[i]`` is finite). CL step multiplies; ED step adds.
    """
    n_coh, n_dev, B = cum.shape
    big = np.iinfo(np.int64).max
    for j in range(1, n_dev):
        k_1 = int(k_idx_by_j[j])
        k   = k_1 - 1 if k_1 != -1 else -1
        for i in range(n_coh):
            lj = int(last_obs[i])
            if lj < 0 or lj >= j:
                continue  # upper triangle: keep
            prev = cum[i, j - 1, :]
            if k < 0:
                cum[i, j, :] = prev
                continue
            mk = int(mat_k[i])
            stage_cl = (mk != big) and (j >= mk)
            if stage_cl:
                f_b = f_star[k, :]
                f_b = np.where(np.isfinite(f_b), f_b, 1.0)
                cum[i, j, :] = f_b * prev
            else:
                g_b = g_star[k, :]
                g_b = np.where(np.isfinite(g_b), g_b, 0.0)
                p_prev = premium_proj[i, j - 1]
                inc = g_b * p_prev if np.isfinite(p_prev) else 0.0
                cum[i, j, :] = prev + inc
    neg = np.isfinite(cum) & (cum < 0.0)
    cum[neg] = 0.0


def _draw_gamma_cell(
    rng:      np.random.Generator,
    inc_mean: np.ndarray,
    phi:      float,
) -> np.ndarray:
    """Cell-paradigm Gamma draw: ``shape = mu/phi``, ``scale = phi``.

    Moment-matched ODP -- ``mean = mu``, ``var = phi * mu``. Cells with
    a non-positive / non-finite ``inc_mean`` keep the deterministic mean
    (no noise). ``rng.gamma`` is called only on the active subset.
    """
    out = inc_mean.copy()
    active = np.isfinite(inc_mean) & (inc_mean > 0.0)
    if active.any() and np.isfinite(phi) and phi > 0.0:
        shape = inc_mean[active] / phi
        out[active] = rng.gamma(shape=shape, scale=phi)
    return out


def _draw_normal_cell(
    rng:      np.random.Generator,
    inc_mean: np.ndarray,
    phi:      float,
    alpha:    float,
) -> np.ndarray:
    """Cell-paradigm Normal draw: ``mu + N(0, sqrt(phi*|mu|^alpha))``.

    Cells with non-positive / non-finite ``inc_mean`` keep the
    deterministic mean.
    """
    out = inc_mean.copy()
    active = np.isfinite(inc_mean) & (inc_mean > 0.0)
    if active.any() and np.isfinite(phi) and phi > 0.0:
        sd = np.sqrt(phi * np.power(np.abs(inc_mean[active]), alpha))
        out[active] = inc_mean[active] + rng.normal(0.0, 1.0,
                                                    size=int(active.sum())) * sd
    return out


def _fwd_sim_cell_step(
    rng:          np.random.Generator,
    inc_mean:     np.ndarray,
    phi:          float,
    alpha:        float,
    process_code: int,
) -> np.ndarray:
    """One cell-independent Stage-2 increment draw across the B axis."""
    if process_code in (1, 2):       # gamma / od_pois (moment-matched)
        return _draw_gamma_cell(rng, inc_mean, phi)
    if process_code == 3:            # normal
        return _draw_normal_cell(rng, inc_mean, phi, alpha)
    return inc_mean


def _fwd_sim_cl_cell(
    cum_mean:     np.ndarray,
    last_obs:     np.ndarray,
    rng:          np.random.Generator,
    phi:          float,
    alpha:        float,
    process_code: int,
) -> np.ndarray:
    """Cell-independent CL Stage-2 simulation.

    Mirrors R's ``bootstrap_fwd_sim_cl_cell``. The per-step *mean*
    increment is read from the noise-free ``cum_mean`` (``inc_mean =
    cum_mean[j] - cum_mean[j-1]``); a fresh process draw is added and
    ``cum_sampled`` accumulates additively. The upper triangle is copied
    from ``cum_mean`` unchanged.
    """
    n_coh, n_dev, B = cum_mean.shape
    cum_sampled = cum_mean.copy()
    if not (np.isfinite(phi) and phi > 0.0):
        return cum_sampled
    for i in range(n_coh):
        lj = int(last_obs[i])           # 0-indexed last observed dev
        if lj < 0 or lj >= n_dev - 1:
            continue                    # no obs, or fully observed
        prev = cum_sampled[i, lj, :].astype(np.float64, copy=True)
        if not np.isfinite(prev).any():
            continue
        for j in range(lj + 1, n_dev):
            cur = cum_mean[i, j, :]
            pre = cum_mean[i, j - 1, :]
            both = np.isfinite(cur) & np.isfinite(pre)
            inc_mean = np.where(both, cur - pre, np.nan)
            inc_sampled = _fwd_sim_cell_step(
                rng, inc_mean, phi, alpha, process_code
            )
            new = np.where(both, prev + inc_sampled, np.nan)
            cum_sampled[i, j, :] = new
            prev = np.where(both, new, prev)
    return cum_sampled


def _fwd_sim_ed_cell(
    cum_mean:      np.ndarray,
    g_star:        np.ndarray,
    premium_proj:  np.ndarray,
    last_obs:      np.ndarray,
    k_idx_by_j:    np.ndarray,
    rng:           np.random.Generator,
    phi:           float,
    alpha:         float,
    process_code:  int,
) -> np.ndarray:
    """Cell-independent ED Stage-2 simulation.

    Mirrors R's ``bootstrap_fwd_sim_ed_cell``. The per-step mean is the
    additive ED increment ``inc_mean = g_star[k] * premium_proj[j-1]``;
    a fresh process draw is added and ``cum_sampled`` accumulates.
    """
    n_coh, n_dev, B = cum_mean.shape
    n_links = g_star.shape[0]
    cum_sampled = cum_mean.copy()
    if not (np.isfinite(phi) and phi > 0.0):
        return cum_sampled
    for i in range(n_coh):
        lj = int(last_obs[i])           # 0-indexed last observed dev
        if lj < 0 or lj >= n_dev - 1:
            continue
        prev = cum_sampled[i, lj, :].astype(np.float64, copy=True)
        if not np.isfinite(prev).any():
            continue
        for j in range(lj + 1, n_dev):
            k_1 = int(k_idx_by_j[j])
            if k_1 == -1:
                cum_sampled[i, j, :] = prev
                continue
            k = k_1 - 1
            g_b = g_star[k, :] if 0 <= k < n_links else np.full(B, np.nan)
            p_prev = premium_proj[i, j - 1]
            if not np.isfinite(p_prev):
                cum_sampled[i, j, :] = prev
                continue
            inc_mean = np.where(np.isfinite(g_b), g_b * p_prev, np.nan)
            inc_sampled = _fwd_sim_cell_step(
                rng, inc_mean, phi, alpha, process_code
            )
            new = prev + inc_sampled
            cum_sampled[i, j, :] = new
            prev = new
    return cum_sampled


def _fwd_sim_sa_cell(
    cum_mean:     np.ndarray,
    last_obs:     np.ndarray,
    mat_k:        np.ndarray,
    rng:          np.random.Generator,
    phi_ed:       float,
    phi_cl:       float,
    alpha:        float,
    process_code: int,
) -> np.ndarray:
    """Stage-adaptive Stage-2 simulation.

    Mirrors R's ``bootstrap_sa_fwd_sim_cell``. Like the CL cell path the
    mean increment is read from the noise-free ``cum_mean``; the
    dispersion ``phi`` is selected per cell -- ``phi_cl`` when the cell
    is in the CL stage (``j >= mat_k[i]``), else ``phi_ed``.
    """
    n_coh, n_dev, B = cum_mean.shape
    cum_sampled = cum_mean.copy()
    big = np.iinfo(np.int64).max
    for i in range(n_coh):
        lj = int(last_obs[i])           # 0-indexed last observed dev
        if lj < 0 or lj >= n_dev - 1:
            continue
        prev = cum_sampled[i, lj, :].astype(np.float64, copy=True)
        if not np.isfinite(prev).any():
            continue
        mk = int(mat_k[i])
        for j in range(lj + 1, n_dev):
            cur = cum_mean[i, j, :]
            pre = cum_mean[i, j - 1, :]
            both = np.isfinite(cur) & np.isfinite(pre)
            inc_mean = np.where(both, cur - pre, np.nan)
            stage_cl = (mk != big) and (j >= mk)
            phi_use = phi_cl if stage_cl else phi_ed
            inc_sampled = _fwd_sim_cell_step(
                rng, inc_mean, phi_use, alpha, process_code
            )
            new = np.where(both, prev + inc_sampled, np.nan)
            cum_sampled[i, j, :] = new
            prev = np.where(both, new, prev)
    return cum_sampled


def _fwd_sim_cl_link(
    cum_mean:     np.ndarray,
    f_star:       np.ndarray,
    sigma2:       np.ndarray,
    last_obs:     np.ndarray,
    k_idx_by_j:   np.ndarray,
    rng:          np.random.Generator,
    alpha:        float,
    process_code: int,
) -> np.ndarray:
    """Chain-Markov CL link Stage-2 simulation.

    Mirrors R's ``bootstrap_fwd_sim_cl_link``. The recursion feeds the
    *noisy* ``prev_sampled`` forward: ``mu_step = f_star[k] * prev``,
    ``var = sigma2[k] * |prev|^alpha``. Gamma uses ``shape =
    mu^2/var``, ``scale = var/mu``; Normal adds ``N(0, sqrt(var))``.
    Pathological steps degenerate to the deterministic mean.
    """
    n_coh, n_dev, B = cum_mean.shape
    n_links = f_star.shape[0]
    cum_sampled = cum_mean.copy()
    for i in range(n_coh):
        lj = int(last_obs[i])           # 0-indexed last observed dev
        if lj < 0 or lj >= n_dev - 1:
            continue
        prev = cum_sampled[i, lj, :].astype(np.float64, copy=True)
        if not np.isfinite(prev).any():
            continue
        for j in range(lj + 1, n_dev):
            k_1 = int(k_idx_by_j[j])
            if k_1 == -1:
                cum_sampled[i, j, :] = prev
                continue
            k = k_1 - 1
            f_b = f_star[k, :]
            f_b = np.where(np.isfinite(f_b), f_b, 1.0)
            mu_step = f_b * prev
            s2_k = sigma2[k] if 0 <= k < n_links else np.nan

            new = mu_step.copy()
            if np.isfinite(s2_k) and s2_k > 0.0:
                var = s2_k * np.power(np.abs(prev), alpha)
                noisy = (
                    np.isfinite(var) & (var > 0.0)
                    & np.isfinite(mu_step) & (mu_step > 0.0)
                    & np.isfinite(prev) & (prev > 0.0)
                )
                if noisy.any():
                    if process_code in (1, 2):    # gamma / od_pois
                        shape = mu_step[noisy] ** 2 / var[noisy]
                        scale = var[noisy] / mu_step[noisy]
                        new[noisy] = rng.gamma(shape=shape, scale=scale)
                    elif process_code == 3:       # normal
                        eps = rng.normal(0.0, 1.0, size=int(noisy.sum()))
                        new[noisy] = mu_step[noisy] + eps * np.sqrt(var[noisy])
            neg = np.isfinite(new) & (new < 0.0)
            new[neg] = 0.0
            cum_sampled[i, j, :] = new
            prev = new
    return cum_sampled


def _draw_parametric_cell(
    rng:          np.random.Generator,
    mu:           float,
    phi:          float,
    alpha:        float,
    process_code: int,
    size:         int,
) -> np.ndarray:
    """Parametric Stage-1 cell draw, ``size`` replicates of one cell.

    Mirrors R's ``bootstrap_kernel_*_param`` phase (a). Gamma:
    ``Gamma(shape=mu/phi, scale=phi)``; Normal: ``mu + N(0,
    sqrt(phi*|mu|^alpha))`` clipped to ``>= 0``. Non-positive ``mu`` or
    non-positive ``phi`` -> deterministic ``mu``.
    """
    if not np.isfinite(mu):
        return np.full(size, mu, dtype=np.float64)
    if process_code in (1, 2):       # gamma / od_pois
        if mu <= 0.0 or not (np.isfinite(phi) and phi > 0.0):
            return np.full(size, mu, dtype=np.float64)
        return rng.gamma(shape=mu / phi, scale=phi, size=size)
    if process_code == 3:            # normal
        if not (np.isfinite(phi) and phi > 0.0):
            return np.full(size, mu, dtype=np.float64)
        sd = np.sqrt(phi * np.power(abs(mu), alpha))
        x = mu + rng.normal(0.0, 1.0, size=size) * sd
        return np.where(x < 0.0, 0.0, x)
    return np.full(size, mu, dtype=np.float64)


def _pool_dev_subpools(
    pool: "_ResidualPool | None",
) -> dict[int, np.ndarray]:
    """Map each pooled ``dev`` to the residual array of its ``pool_id``.

    A cell at dev ``d`` resamples from the sub-pool whose ``pool_id``
    covers ``d``. For ``pooling="pooled"`` every dev maps to the one
    ``"all"`` pool; for ``"separated"`` each dev maps to its own; for
    ``"tail_pooled"`` post-cut devs all map to the single ``POST``
    pool. Mirrors R's ``pid_by_dev`` -> ``pool_pos`` indirection.
    """
    out: dict[int, np.ndarray] = {}
    if pool is None or len(pool) == 0:
        return out
    # pool_id -> residual array.
    by_pid: dict[Any, np.ndarray] = {}
    for pid in np.unique(pool.pool_id):
        by_pid[pid] = pool.residual[pool.pool_id == pid]
    # dev -> pool_id (a dev maps to whichever pool_id its residuals carry).
    for d in np.unique(pool.key):
        sel = pool.key == d
        pid = pool.pool_id[sel][0]
        out[int(d)] = by_pid[pid]
    return out


def _resample_increment(
    rng:           np.random.Generator,
    mu:            np.ndarray,
    sqrt_term:     np.ndarray,
    pool:          "_ResidualPool | None",
    dev:           np.ndarray,
    B:             int,
    _injected:     np.ndarray | None = None,
) -> np.ndarray:
    """Resample a Pearson residual per active cell, per replicate.

    For active cell ``a`` at dev ``dev[a]`` the pseudo increment is
    ``mu[a] + r_star * sqrt_term[a]`` where ``r_star`` is drawn
    uniformly from the sub-pool covering that dev (see
    :func:`_pool_dev_subpools`). An empty / absent pool gives
    ``r_star = 0`` (the pseudo cell is then deterministic at ``mu``).

    Mirrors phase (a) of R's ``bootstrap_kernel_cl_cell`` /
    ``bootstrap_kernel_ed_cell``.

    Parameters
    ----------
    mu, sqrt_term
        ``(n_active,)`` fitted increment mean and its Pearson scale.
    pool
        The :class:`_ResidualPool`; ``None`` -> no resampling.
    dev
        ``(n_active,)`` per-cell *dev value* (not index).
    B
        Replicate count.
    _injected
        Test-only hook -- ``(n_active, B)`` integer indices into each
        cell's pool, used instead of drawing.

    Returns
    -------
    np.ndarray
        ``(n_active, B)`` perturbed pseudo increments.
    """
    n_active = mu.shape[0]
    inc = np.zeros((n_active, B), dtype=np.float64)
    subpools = _pool_dev_subpools(pool)

    for a in range(n_active):
        mu_a = mu[a]
        if not np.isfinite(mu_a):
            inc[a, :] = np.nan
            continue
        sub = subpools.get(int(dev[a]))
        if sub is None or sub.size == 0:
            inc[a, :] = mu_a
            continue
        if _injected is not None:
            idx = np.clip(_injected[a, :], 0, sub.size - 1)
        else:
            idx = (rng.random(B) * sub.size).astype(np.int64)
            idx = np.clip(idx, 0, sub.size - 1)
        inc[a, :] = mu_a + sub[idx] * sqrt_term[a]
    return inc


@dataclass
class _GroupKernelInputs:
    """Per-group scaffolding shared by the resampling kernels.

    Built once per group by :func:`_build_group_inputs` from the
    observed cumulative matrix + the Mack anchor. Carries the index
    vectors the C kernels receive precomputed from R.
    """

    loss_obs:    np.ndarray   # (n_coh, n_dev) observed cumulative
    last_obs:    np.ndarray   # (n_coh,) 0-indexed last obs dev, -1 none
    k_idx_by_j:  np.ndarray   # (n_dev,) 1-indexed link for to-dev j, -1
    link_to_idx: np.ndarray   # (n_links,) 1-indexed dest dev per link
    cohorts:     list
    devs:        list


def _build_group_inputs(
    loss_obs: np.ndarray,
    anchor:   _Anchor,
    cohorts:  list,
    devs:     list,
) -> _GroupKernelInputs:
    """Assemble the index scaffolding for a single group."""
    n_coh, n_dev = loss_obs.shape
    last_obs = _last_obs_idx(loss_obs)

    dev_pos = {d: j for j, d in enumerate(devs)}
    n_links = len(anchor.ata_to)

    # link_to_idx[k] = 1-indexed dev column of the link's destination.
    link_to_idx = np.full(n_links, -1, dtype=np.int64)
    for k in range(n_links):
        j = dev_pos.get(int(anchor.ata_to[k]))
        if j is not None:
            link_to_idx[k] = j + 1

    # k_idx_by_j[j] = 1-indexed link landing at to-dev j (-1 if none).
    k_idx_by_j = np.full(n_dev, -1, dtype=np.int64)
    to_to_k = {int(anchor.ata_to[k]): k for k in range(n_links)}
    for j, d in enumerate(devs):
        k = to_to_k.get(int(d))
        if k is not None:
            k_idx_by_j[j] = k + 1

    return _GroupKernelInputs(
        loss_obs    = loss_obs,
        last_obs    = last_obs,
        k_idx_by_j  = k_idx_by_j,
        link_to_idx = link_to_idx,
        cohorts     = cohorts,
        devs        = devs,
    )


def _fitted_grid_cl(
    loss_obs: np.ndarray,
    last_obs: np.ndarray,
    anchor:   _Anchor,
    devs:     list,
) -> np.ndarray:
    """Chain-anchored CL fitted incremental backbone ``mu_hat``."""
    n_dev = loss_obs.shape[1]
    f_by_to = np.full(n_dev, np.nan, dtype=np.float64)
    dev_pos = {d: j for j, d in enumerate(devs)}
    for k in range(len(anchor.ata_to)):
        j = dev_pos.get(int(anchor.ata_to[k]))
        if j is not None:
            f_by_to[j] = anchor.f_hat[k]
    fwd, bwd = _boot_steps_cl(f_by_to)
    return _boot_fitted_grid(loss_obs, last_obs, fwd, bwd)


def _premium_obs_matrix(
    df:      pl.DataFrame,
    cohorts: list,
    devs:    list,
) -> np.ndarray:
    """Observed cumulative premium matrix, aligned to ``cohorts`` / ``devs``."""
    n_coh, n_dev = len(cohorts), len(devs)
    coh_pos = {c: i for i, c in enumerate(cohorts)}
    dev_pos = {d: j for j, d in enumerate(devs)}
    mat = np.full((n_coh, n_dev), np.nan, dtype=np.float64)
    if "premium" not in df.columns:
        return mat
    for row in df.iter_rows(named=True):
        i = coh_pos.get(row["cohort"])
        j = dev_pos.get(row["dev"])
        v = row["premium"]
        if i is not None and j is not None and v is not None:
            mat[i, j] = float(v)
    return mat


def _premium_anchor_proj(
    exp_obs:  np.ndarray,
    last_obs: np.ndarray,
) -> np.ndarray:
    """Project the premium column forward via CL (fixed across replicates).

    Mirrors R's ``.boot_anchor_premium_cl`` + ``.boot_proj_premium_cl``.
    Volume-weighted per-link factor on observed cumulative premium, then
    a deterministic forward roll from each cohort's last observed dev.
    """
    n_coh, n_dev = exp_obs.shape
    out = exp_obs.copy()
    if n_dev < 2:
        return out
    f_by_to = np.full(n_dev, np.nan, dtype=np.float64)
    for j in range(1, n_dev):
        active = (last_obs >= 0) & (last_obs >= j)
        if active.any():
            num = exp_obs[active, j]
            den = exp_obs[active, j - 1]
            ok = np.isfinite(num) & np.isfinite(den) & (den > 0.0)
            s_den = den[ok].sum()
            if s_den > 0.0:
                f_by_to[j] = num[ok].sum() / s_den
    for i in range(n_coh):
        lj = int(last_obs[i])
        if lj < 0 or lj >= n_dev:
            continue
        cur = out[i, lj]
        if not np.isfinite(cur):
            continue
        for j in range(lj + 1, n_dev):
            f_k = f_by_to[j]
            if np.isfinite(f_k):
                cur = f_k * cur
            out[i, j] = cur
    return out


def _ed_intensity_anchor(
    link_df: pl.DataFrame,
    anchor:  _Anchor,
) -> np.ndarray:
    """Per-link volume-weighted ED intensity ``g_hat``, aligned to ``anchor``.

    ``g_k = sum(loss_delta) / sum(premium_from)`` over observed link
    rows; non-finite -> 0. Mirrors R's ``.boot_anchor_ed``.
    """
    sub = link_df.select(["ata_to", "loss_delta", "premium_from"])
    ata_to     = sub["ata_to"].to_numpy().astype(np.float64)
    loss_delta = sub["loss_delta"].to_numpy().astype(np.float64)
    prem_from  = sub["premium_from"].to_numpy().astype(np.float64)

    g_by_to: dict[int, float] = {}
    ok = (
        np.isfinite(loss_delta) & np.isfinite(prem_from) & (prem_from > 0.0)
    )
    for lk in np.unique(ata_to[np.isfinite(ata_to)]):
        sel = ok & (ata_to == lk)
        s_den = prem_from[sel].sum()
        if s_den > 0.0:
            g_by_to[int(lk)] = loss_delta[sel].sum() / s_den

    n_links = len(anchor.ata_to)
    g_hat = np.zeros(n_links, dtype=np.float64)
    for k in range(n_links):
        v = g_by_to.get(int(anchor.ata_to[k]))
        if v is not None and np.isfinite(v):
            g_hat[k] = v
    return g_hat


def _active_upper_cells(
    last_obs: np.ndarray,
    mu_grid:  np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Active upper-triangle cells with a finite fitted ``mu``.

    Returns ``(coh_idx, dev_idx, lin)`` -- the cohort / 0-indexed dev
    arrays and a column-major linear index into the ``(n_coh, n_dev)``
    grid (cohort fastest, matching R's ``which()``).
    """
    n_coh, n_dev = mu_grid.shape
    upper = np.zeros((n_coh, n_dev), dtype=bool)
    for i in range(n_coh):
        lj = int(last_obs[i])
        if lj >= 0:
            upper[i, : lj + 1] = True
    active = upper & np.isfinite(mu_grid)
    # column-major: cohort fastest (R which() order).
    lin = np.where(active.flatten(order="F"))[0]
    coh_idx = lin % n_coh
    dev_idx = lin // n_coh
    return coh_idx, dev_idx, lin


def _place_increments(
    inc:     np.ndarray,
    lin:     np.ndarray,
    n_coh:   int,
    n_dev:   int,
    B:       int,
) -> np.ndarray:
    """Scatter per-cell increments into a ``(n_coh, n_dev, B)`` grid.

    Inactive cells are 0 (so the cumsum carries the active increments);
    NaN increments propagate as NaN.
    """
    cum = np.zeros((n_coh, n_dev, B), dtype=np.float64)
    if lin.size:
        coh = lin % n_coh
        dev = lin // n_coh
        cum[coh, dev, :] = inc
    return cum


def _cumsum_mask(
    cum:      np.ndarray,
    last_obs: np.ndarray,
) -> None:
    """Cumsum along dev per cohort, then mask the lower triangle to NaN.

    In place. Mirrors phases (b) + (c) of the R cell kernels. A cohort
    with no observation (``last_obs == -1``) is masked from dev 0.
    """
    n_coh, n_dev, B = cum.shape
    np.cumsum(cum, axis=1, out=cum)
    for i in range(n_coh):
        L = int(last_obs[i])
        # `last_obs` is 0-indexed -- keep cells 0..L, mask L+1 onward.
        # A cohort with no observation (L == -1) is masked from dev 0.
        j_start = 0 if L < 0 else L + 1
        cum[i, j_start:, :] = np.nan


def _process_code(process: str) -> int:
    """Map a process name to the integer code shared with the R kernels."""
    return _PROCESS_CODES.get(process, 1)


def _boot_kernel_cl_cell(
    gi:           _GroupKernelInputs,
    anchor:       _Anchor,
    mu_grid:      np.ndarray,
    pool:         "_ResidualPool | None",
    phi:          float,
    B:            int,
    rng:          np.random.Generator,
    alpha:        float,
    process:      str,
    _injected_resample: np.ndarray | None = None,
) -> _Stage1Result:
    """CL cell-residual kernel (England-Verrall 1999 / Renshaw-Verrall 1998).

    Six fused phases: residual-perturb the active upper cells, cumsum,
    mask, refit ``f_star``, deterministic CL projection (``cum_mean``),
    cell-independent Stage-2 simulation (``cum_sampled``).
    """
    loss_obs = gi.loss_obs
    n_coh, n_dev = loss_obs.shape
    coh_idx, dev_idx, lin = _active_upper_cells(gi.last_obs, mu_grid)
    mu_active   = mu_grid.flatten(order="F")[lin]
    sqrt_active = np.sqrt(np.abs(mu_active))

    dev_vals = np.array([int(d) for d in gi.devs], dtype=np.int64)[dev_idx]
    inc = _resample_increment(
        rng, mu_active, sqrt_active, pool, dev_vals, B,
        _injected=_injected_resample,
    )
    cum = _place_increments(inc, lin, n_coh, n_dev, B)
    _cumsum_mask(cum, gi.last_obs)

    f_star = _refit_fstar(cum, gi.link_to_idx, anchor.f_hat)
    _fwd_proj_cl(cum, f_star, gi.last_obs, gi.k_idx_by_j)

    cum_sampled = _fwd_sim_cl_cell(
        cum, gi.last_obs, rng, phi, alpha, _process_code(process)
    )
    return _Stage1Result(cum, cum_sampled, gi.cohorts, gi.devs)


def _boot_kernel_cl_link(
    gi:           _GroupKernelInputs,
    anchor:       _Anchor,
    pool:         "_ResidualPool | None",
    B:            int,
    rng:          np.random.Generator,
    alpha:        float,
    process:      str,
    _injected_resample: np.ndarray | None = None,
) -> _Stage1Result:
    """CL link-residual kernel (Mack 1993 / Pinheiro et al. 2003).

    Chain residual resample on the cumulative recursion:
    ``cum[to] = f_hat[k] * prev + r_star * sqrt(sigma2[k] * prev)``
    over cohorts observed at the destination with ``prev > 0``; then
    refit ``f_star``, project, and run the chain-Markov Stage-2.
    """
    loss_obs = gi.loss_obs
    n_coh, n_dev = loss_obs.shape
    n_links = len(anchor.ata_to)

    cum = np.full((n_coh, n_dev, B), np.nan, dtype=np.float64)
    cum[:, 0, :] = loss_obs[:, 0][:, None]

    # Per-link residual sub-pools, keyed by ata_to.
    pools: dict[int, np.ndarray] = {}
    if pool is not None and len(pool) > 0:
        for k in range(n_links):
            sub = pool.residual[pool.key == int(anchor.ata_to[k])]
            if sub.size:
                pools[k] = sub

    for k in range(n_links):
        to_col_1 = int(gi.link_to_idx[k])
        if to_col_1 < 2:
            continue
        from_col = to_col_1 - 2
        to_col   = to_col_1 - 1
        f_k  = anchor.f_hat[k]
        s2_k = anchor.sigma2[k]
        if not (np.isfinite(s2_k) and s2_k >= 0.0):
            s2_k = 0.0
        obs_to = loss_obs[:, to_col]
        sub    = pools.get(k)
        for i in range(n_coh):
            if not np.isfinite(obs_to[i]):
                continue
            prev = cum[i, from_col, :]
            ok = np.isfinite(prev) & (prev > 0.0)
            if not ok.any():
                continue
            r_star = np.zeros(B, dtype=np.float64)
            if sub is not None and sub.size:
                if _injected_resample is not None:
                    idx = np.clip(_injected_resample[k, :], 0, sub.size - 1)
                else:
                    idx = np.clip(
                        (rng.random(B) * sub.size).astype(np.int64),
                        0, sub.size - 1,
                    )
                r_star = sub[idx]
            new = (
                f_k * prev + r_star * np.sqrt(s2_k * np.abs(prev))
            )
            cum[i, to_col, :] = np.where(ok, new, cum[i, to_col, :])

    # Pre-refit clip: zero finite negatives.
    neg = np.isfinite(cum) & (cum < 0.0)
    cum[neg] = 0.0

    f_star = _refit_fstar(cum, gi.link_to_idx, anchor.f_hat)
    _fwd_proj_cl(cum, f_star, gi.last_obs, gi.k_idx_by_j)

    cum_sampled = _fwd_sim_cl_link(
        cum, f_star, anchor.sigma2, gi.last_obs, gi.k_idx_by_j,
        rng, alpha, _process_code(process),
    )
    return _Stage1Result(cum, cum_sampled, gi.cohorts, gi.devs)


def _boot_kernel_ed_cell(
    gi:            _GroupKernelInputs,
    anchor:        _Anchor,
    mu_ed_grid:    np.ndarray,
    g_hat:         np.ndarray,
    premium_proj:  np.ndarray,
    pool:          "_ResidualPool | None",
    phi:           float,
    B:             int,
    rng:           np.random.Generator,
    alpha:         float,
    process:       str,
    _injected_resample: np.ndarray | None = None,
) -> _Stage1Result:
    """ED cell-residual kernel (exposure-driven additive recursion).

    Like the CL cell path but the projection is additive: refit
    ``g_star``, ``cum[j] = cum[j-1] + g_star[k] * premium_proj[j-1]``,
    and the Stage-2 increment mean is ``g_star[k] * premium_proj[j-1]``.
    """
    loss_obs = gi.loss_obs
    n_coh, n_dev = loss_obs.shape
    coh_idx, dev_idx, lin = _active_upper_cells(gi.last_obs, mu_ed_grid)
    mu_active   = mu_ed_grid.flatten(order="F")[lin]
    sqrt_active = np.sqrt(np.abs(mu_active))

    dev_vals = np.array([int(d) for d in gi.devs], dtype=np.int64)[dev_idx]
    inc = _resample_increment(
        rng, mu_active, sqrt_active, pool, dev_vals, B,
        _injected=_injected_resample,
    )
    cum = _place_increments(inc, lin, n_coh, n_dev, B)
    _cumsum_mask(cum, gi.last_obs)

    g_star = _refit_gstar(cum, premium_proj, gi.link_to_idx, g_hat)
    _fwd_proj_ed(cum, g_star, premium_proj, gi.last_obs, gi.k_idx_by_j)

    cum_sampled = _fwd_sim_ed_cell(
        cum, g_star, premium_proj, gi.last_obs, gi.k_idx_by_j,
        rng, phi, alpha, _process_code(process),
    )
    return _Stage1Result(cum, cum_sampled, gi.cohorts, gi.devs)


def _boot_kernel_sa_cell(
    gi:            _GroupKernelInputs,
    anchor:        _Anchor,
    mu_cl_grid:    np.ndarray,
    mu_ed_grid:    np.ndarray,
    g_hat:         np.ndarray,
    premium_proj:  np.ndarray,
    mat_k:         np.ndarray,
    pool_ed:       "_ResidualPool | None",
    pool_cl:       "_ResidualPool | None",
    phi_ed:        float,
    phi_cl:        float,
    B:             int,
    rng:           np.random.Generator,
    alpha:         float,
    process:       str,
    _injected_resample: np.ndarray | None = None,
) -> _Stage1Result:
    """SA cell-residual kernel (stage-adaptive ED-before / CL-after).

    Each active cell is classified ED or CL by its from-dev vs
    ``mat_k``; the ED cells draw from the ED pool with ``mu_ed`` /
    ``sqrt(|mu_ed|)``, the CL cells from the CL pool with ``mu_cl`` /
    ``sqrt(|mu_cl|)``. The two pools are never merged. Projection and
    Stage-2 switch paradigm per cohort at ``mat_k``.
    """
    loss_obs = gi.loss_obs
    n_coh, n_dev = loss_obs.shape
    big = np.iinfo(np.int64).max

    # Active upper cells -- a cell is active iff its paradigm-appropriate
    # fitted grid is finite.
    upper = np.zeros((n_coh, n_dev), dtype=bool)
    for i in range(n_coh):
        lj = int(gi.last_obs[i])
        if lj >= 0:
            upper[i, : lj + 1] = True
    lin_all = np.where(upper.flatten(order="F"))[0]
    a_i = lin_all % n_coh
    a_j = lin_all // n_coh
    # CL iff the to-dev j reaches the CL-start from-dev mat_k[i].
    is_cl = (mat_k[a_i] != big) & (a_j >= mat_k[a_i])

    cl_fin = np.isfinite(mu_cl_grid.flatten(order="F")[lin_all])
    ed_fin = np.isfinite(mu_ed_grid.flatten(order="F")[lin_all])
    keep   = np.where(is_cl, cl_fin, ed_fin)
    lin_all = lin_all[keep]
    a_j     = a_j[keep]
    is_cl   = is_cl[keep]

    mu_cl_flat = mu_cl_grid.flatten(order="F")
    mu_ed_flat = mu_ed_grid.flatten(order="F")
    mu_active   = np.where(is_cl, mu_cl_flat[lin_all], mu_ed_flat[lin_all])
    sqrt_active = np.sqrt(np.abs(mu_active))

    # Per-cell residual draw -- ED cells from pool_ed, CL cells from
    # pool_cl. The two pools stay disjoint (paradigm-tagged).
    inc = np.zeros((mu_active.shape[0], B), dtype=np.float64)
    ed_sub = _pool_dev_subpools(pool_ed)
    cl_sub = _pool_dev_subpools(pool_cl)

    for a in range(mu_active.shape[0]):
        mu_a = mu_active[a]
        if not np.isfinite(mu_a):
            inc[a, :] = np.nan
            continue
        dev_a = int(gi.devs[a_j[a]])
        sub = cl_sub.get(dev_a) if is_cl[a] else ed_sub.get(dev_a)
        if sub is None or sub.size == 0:
            inc[a, :] = mu_a
            continue
        if _injected_resample is not None:
            idx = np.clip(_injected_resample[a, :], 0, sub.size - 1)
        else:
            idx = np.clip(
                (rng.random(B) * sub.size).astype(np.int64),
                0, sub.size - 1,
            )
        inc[a, :] = mu_a + sub[idx] * sqrt_active[a]

    cum = _place_increments(inc, lin_all, n_coh, n_dev, B)
    _cumsum_mask(cum, gi.last_obs)

    f_star = _refit_fstar(cum, gi.link_to_idx, anchor.f_hat)
    g_star = _refit_gstar(cum, premium_proj, gi.link_to_idx, g_hat)
    _fwd_proj_sa(cum, f_star, g_star, premium_proj, gi.last_obs,
                 gi.k_idx_by_j, mat_k)

    cum_sampled = _fwd_sim_sa_cell(
        cum, gi.last_obs, mat_k, rng, phi_ed, phi_cl, alpha,
        _process_code(process),
    )
    return _Stage1Result(cum, cum_sampled, gi.cohorts, gi.devs)


def _boot_kernel_cl_parametric(
    gi:           _GroupKernelInputs,
    anchor:       _Anchor,
    mu_grid:      np.ndarray,
    phi:          float,
    B:            int,
    rng:          np.random.Generator,
    alpha:        float,
    process:      str,
) -> _Stage1Result:
    """CL parametric kernel (textbook England-Verrall 1999).

    Each active upper cell is drawn directly from
    ``ProcessDist(mu_hat, phi)``; then cumsum / mask / refit ``f_star``
    / CL projection / cell-independent Stage-2 (same as the CL cell
    path beyond phase (a)).
    """
    loss_obs = gi.loss_obs
    n_coh, n_dev = loss_obs.shape
    coh_idx, dev_idx, lin = _active_upper_cells(gi.last_obs, mu_grid)
    mu_active = mu_grid.flatten(order="F")[lin]
    pc = _process_code(process)

    inc = np.empty((lin.size, B), dtype=np.float64)
    for a in range(lin.size):
        inc[a, :] = _draw_parametric_cell(
            rng, float(mu_active[a]), phi, alpha, pc, B
        )
    cum = _place_increments(inc, lin, n_coh, n_dev, B)
    _cumsum_mask(cum, gi.last_obs)

    f_star = _refit_fstar(cum, gi.link_to_idx, anchor.f_hat)
    _fwd_proj_cl(cum, f_star, gi.last_obs, gi.k_idx_by_j)

    cum_sampled = _fwd_sim_cl_cell(cum, gi.last_obs, rng, phi, alpha, pc)
    return _Stage1Result(cum, cum_sampled, gi.cohorts, gi.devs)


def _boot_kernel_ed_parametric(
    gi:            _GroupKernelInputs,
    anchor:        _Anchor,
    mu_ed_grid:    np.ndarray,
    g_hat:         np.ndarray,
    premium_proj:  np.ndarray,
    phi:           float,
    B:             int,
    rng:           np.random.Generator,
    alpha:         float,
    process:       str,
) -> _Stage1Result:
    """ED parametric kernel (textbook England-Verrall 1999, additive)."""
    loss_obs = gi.loss_obs
    n_coh, n_dev = loss_obs.shape
    coh_idx, dev_idx, lin = _active_upper_cells(gi.last_obs, mu_ed_grid)
    mu_active = mu_ed_grid.flatten(order="F")[lin]
    pc = _process_code(process)

    inc = np.empty((lin.size, B), dtype=np.float64)
    for a in range(lin.size):
        inc[a, :] = _draw_parametric_cell(
            rng, float(mu_active[a]), phi, alpha, pc, B
        )
    cum = _place_increments(inc, lin, n_coh, n_dev, B)
    _cumsum_mask(cum, gi.last_obs)

    g_star = _refit_gstar(cum, premium_proj, gi.link_to_idx, g_hat)
    _fwd_proj_ed(cum, g_star, premium_proj, gi.last_obs, gi.k_idx_by_j)

    cum_sampled = _fwd_sim_ed_cell(
        cum, g_star, premium_proj, gi.last_obs, gi.k_idx_by_j,
        rng, phi, alpha, pc,
    )
    return _Stage1Result(cum, cum_sampled, gi.cohorts, gi.devs)


def _boot_kernel_sa_parametric(
    gi:            _GroupKernelInputs,
    anchor:        _Anchor,
    mu_cl_grid:    np.ndarray,
    mu_ed_grid:    np.ndarray,
    g_hat:         np.ndarray,
    premium_proj:  np.ndarray,
    mat_k:         np.ndarray,
    phi_ed:        float,
    phi_cl:        float,
    B:             int,
    rng:           np.random.Generator,
    alpha:         float,
    process:       str,
) -> _Stage1Result:
    """SA parametric kernel (textbook England-Verrall 1999, stage-adaptive).

    Each active cell is drawn from ``ProcessDist(mu, phi)`` with both
    ``mu`` and ``phi`` selected by the per-cohort stage at ``mat_k``.
    """
    loss_obs = gi.loss_obs
    n_coh, n_dev = loss_obs.shape
    big = np.iinfo(np.int64).max
    pc = _process_code(process)

    upper = np.zeros((n_coh, n_dev), dtype=bool)
    for i in range(n_coh):
        lj = int(gi.last_obs[i])
        if lj >= 0:
            upper[i, : lj + 1] = True
    lin_all = np.where(upper.flatten(order="F"))[0]
    a_i = lin_all % n_coh
    a_j = lin_all // n_coh
    is_cl = (mat_k[a_i] != big) & (a_j >= mat_k[a_i])

    mu_cl_flat = mu_cl_grid.flatten(order="F")
    mu_ed_flat = mu_ed_grid.flatten(order="F")
    cl_fin = np.isfinite(mu_cl_flat[lin_all])
    ed_fin = np.isfinite(mu_ed_flat[lin_all])
    keep   = np.where(is_cl, cl_fin, ed_fin)
    lin_all = lin_all[keep]
    is_cl   = is_cl[keep]
    mu_active = np.where(is_cl, mu_cl_flat[lin_all], mu_ed_flat[lin_all])

    inc = np.empty((mu_active.shape[0], B), dtype=np.float64)
    for a in range(mu_active.shape[0]):
        phi_use = phi_cl if is_cl[a] else phi_ed
        inc[a, :] = _draw_parametric_cell(
            rng, float(mu_active[a]), phi_use, alpha, pc, B
        )
    cum = _place_increments(inc, lin_all, n_coh, n_dev, B)
    _cumsum_mask(cum, gi.last_obs)

    f_star = _refit_fstar(cum, gi.link_to_idx, anchor.f_hat)
    g_star = _refit_gstar(cum, premium_proj, gi.link_to_idx, g_hat)
    _fwd_proj_sa(cum, f_star, g_star, premium_proj, gi.last_obs,
                 gi.k_idx_by_j, mat_k)

    cum_sampled = _fwd_sim_sa_cell(
        cum, gi.last_obs, mat_k, rng, phi_ed, phi_cl, alpha, pc,
    )
    return _Stage1Result(cum, cum_sampled, gi.cohorts, gi.devs)


# ---------------------------------------------------------------------------
# Section 4 -- BootstrapTriangle result class
# ---------------------------------------------------------------------------


class BootstrapTriangle:
    """Result of a Triangle-level bootstrap.

    Model-agnostic container produced by :meth:`Bootstrap.fit`. Holds
    the per-cell parameter / process SE decomposition in :attr:`summary`,
    the Mack anchor (:attr:`f_anchor` / :attr:`sigma2_anchor`), and the
    run metadata (:attr:`meta`).

    Attributes
    ----------
    summary
        Long-format DataFrame with columns
        ``[groups?, cohort, dev, mean_proj, param_se, proc_se,
        total_se, total_cv (, ci_lo, ci_hi)]``.
    f_anchor
        Per-link ``f_hat`` and ``n_cohorts``.
    sigma2_anchor
        Per-link ``sigma2`` and ``f_var``.
    meta
        Run-configuration dictionary.
    pseudo_triangles
        ``None`` unless ``keep_pseudo=True`` -- then a long-format
        DataFrame of the per-replicate trajectories with columns
        ``[groups?, cohort, dev, rep, {target}_mean, {target}_sampled]``
        (the Stage-1 deterministic projection and the Stage-1 + Stage-2
        process-noisy simulation).
    """

    def __init__(self) -> None:
        # Populated via the _build classmethod.
        self._summary:          pl.DataFrame
        self._f_anchor:         pl.DataFrame
        self._sigma2_anchor:    pl.DataFrame
        self._pseudo_triangles: pl.DataFrame | None
        self._meta:             dict[str, Any]
        self._output_type:      str
        self._groups:           str | None

    # -- properties ---------------------------------------------------------

    @property
    def summary(self):
        """Per-cell SE decomposition, in the original input format."""
        return mirror_output(self._summary, self._output_type)

    @property
    def f_anchor(self):
        """Per-link ``f_hat`` and contributing cohort count."""
        return mirror_output(self._f_anchor, self._output_type)

    @property
    def sigma2_anchor(self):
        """Per-link Mack ``sigma2`` and ``Var(f_hat)``."""
        return mirror_output(self._sigma2_anchor, self._output_type)

    @property
    def pseudo_triangles(self):
        """Per-replicate trajectories (``None`` unless ``keep_pseudo``)."""
        if self._pseudo_triangles is None:
            return None
        return mirror_output(self._pseudo_triangles, self._output_type)

    @property
    def meta(self) -> dict[str, Any]:
        """Run-configuration metadata."""
        return self._meta

    # -- conversion ---------------------------------------------------------

    def to_polars(self) -> pl.DataFrame:
        """Return the ``summary`` slot as a polars DataFrame."""
        return self._summary

    def to_pandas(self):
        """Return the ``summary`` slot as a pandas DataFrame."""
        return self._summary.to_pandas()

    def __repr__(self) -> str:
        n_rows = self._summary.height
        m = self._meta
        bits = [f"type={m['type']}", f"method={m['method']}", f"B={m['B']}"]
        if self._groups is not None:
            n_groups = self._summary[self._groups].n_unique()
            bits.insert(0, f"{n_groups} groups")
        bits.append(f"{n_rows} rows")
        return f"<BootstrapTriangle: {', '.join(bits)}>"


# ---------------------------------------------------------------------------
# Section 5 -- Bootstrap config estimator (sklearn-style)
# ---------------------------------------------------------------------------


class Bootstrap:
    """Triangle-level bootstrap configuration estimator.

    sklearn-style: construct with the run configuration, then call
    :meth:`fit` on a :class:`Triangle` to obtain a
    :class:`BootstrapTriangle`.

    Phase 1 implements only ``type="analytical"`` with ``method="cl"``
    -- the Mack (1993) closed-form propagation. ``nonparametric`` /
    ``parametric`` types and the ``ed`` / ``sa`` methods are deferred to
    later phases and raise :class:`NotImplementedError`.

    Parameters
    ----------
    type
        Bootstrap paradigm. Phase 1 supports only ``"analytical"``.
    method
        Fit-model paradigm. Phase 1 supports only ``"cl"``.
    process
        Stage 2 process distribution. ``type="analytical"`` requires
        ``"normal"`` (Mack closed-form propagation).
    B
        Number of bootstrap replicates. Default ``499`` -- the Davison &
        Hinkley (1997) convention so that ``(B + 1) p`` is integer for
        common reporting quantiles.
    seed
        Optional integer seed for reproducibility.
    alpha
        Variance exponent in Mack's
        ``Var(C_{k+1}|C_k) = sigma^2 C_k^alpha``. Default ``1``.
    quantile_ci
        Whether to emit empirical percentile CI columns
        (``ci_lo`` / ``ci_hi``) in :attr:`BootstrapTriangle.summary`.
    keep_pseudo
        Whether to materialise the per-replicate ``pseudo_triangles``
        slot. Default ``False`` (only ``summary`` is built).

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> bt = lr.Bootstrap(type="analytical", method="cl",
    ...                    B=999, seed=1).fit(tri, target="loss")
    >>> bt.summary
    """

    def __init__(
        self,
        type:        str          = "analytical",
        method:      str          = "cl",
        residual:    str          = "cell",
        process:     str          = "normal",
        pooling:     str          = "pooled",
        hat_adj:     bool         = True,
        demean:      bool         = True,
        tail:        str          = "auto",
        min_pool:    int          = 5,
        maturity:    Any          = None,
        B:           int          = 499,
        seed:        int | None   = None,
        alpha:       float        = 1.0,
        quantile_ci: bool         = False,
        keep_pseudo: bool         = False,
    ) -> None:
        self.type        = type
        self.method      = method
        self.residual    = residual
        self.process     = process
        self.pooling     = pooling
        self.hat_adj     = hat_adj
        self.demean      = demean
        self.tail        = tail
        self.min_pool    = min_pool
        self.maturity    = maturity
        self.B           = B
        self.seed        = seed
        self.alpha       = alpha
        self.quantile_ci = quantile_ci
        self.keep_pseudo = keep_pseudo

        # ----- Argument-combination validation ----------------------------
        if type not in ("analytical", "nonparametric", "parametric"):
            raise ValueError(
                f"type must be one of 'analytical', 'nonparametric', "
                f"'parametric', got {type!r}"
            )
        if method not in ("cl", "ed", "sa"):
            raise ValueError(
                f"method must be one of 'cl', 'ed', 'sa', got {method!r}"
            )
        if residual not in ("cell", "link"):
            raise ValueError(
                f"residual must be 'cell' or 'link', got {residual!r}"
            )
        if process not in ("gamma", "od_pois", "normal"):
            raise ValueError(
                f"process must be one of 'gamma', 'od_pois', 'normal', "
                f"got {process!r}"
            )
        if pooling not in ("pooled", "separated", "tail_pooled"):
            raise ValueError(
                f"pooling must be 'pooled', 'separated' or 'tail_pooled', "
                f"got {pooling!r}"
            )

        # type='analytical' is the Mack closed-form -> CL + Normal only.
        if type == "analytical":
            if method != "cl":
                raise ValueError(
                    "type='analytical' (Mack 1993 closed-form propagation) "
                    f"implements only method='cl', got {method!r}."
                )
            if process != "normal":
                raise ValueError(
                    "type='analytical' (Mack 1993 closed-form propagation) "
                    f"requires process='normal', got {process!r}."
                )
        # ODP cell path (England-Verrall 1999/2002) needs a positivity-
        # preserving process; normal violates the ODP assumption.
        if type == "nonparametric" and residual == "cell" \
                and process == "normal":
            raise ValueError(
                "residual='cell' (ODP path, England-Verrall 1999/2002) "
                "requires a positivity-preserving process distribution. "
                "Use process='gamma' (default) or 'od_pois'."
            )
        # ED + link residuals is mathematically incoherent.
        if type == "nonparametric" and method == "ed" and residual == "link":
            raise ValueError(
                "method='ed' (exposure-driven additive recursion) requires "
                "residual='cell'. ED + link residuals is not implemented."
            )
        # Additive ED / composite SA need a positivity-preserving process.
        if type == "parametric" and method in ("ed", "sa") \
                and process == "normal":
            raise ValueError(
                f"type='parametric' with method={method!r} requires a "
                "positivity-preserving process distribution. Use "
                "process='gamma' (default) or 'od_pois'."
            )

        if not isinstance(B, (int, np.integer)) or B < 1:
            raise ValueError("`B` must be a positive integer.")
        if not np.isfinite(alpha):
            raise ValueError("`alpha` must be a finite numeric value.")

    def fit(
        self,
        triangle: "Triangle",
        target:   str = "loss",
    ) -> "BootstrapTriangle":
        """Bootstrap a :class:`Triangle`.

        Parameters
        ----------
        triangle
            Source :class:`Triangle`.
        target
            Cumulative metric to perturb. One of ``"loss"`` (default)
            or ``"premium"``.

        Returns
        -------
        BootstrapTriangle
            The per-cell SE decomposition + anchor + metadata.
        """
        if target not in ("loss", "premium"):
            raise ValueError(
                f"target must be 'loss' or 'premium', got {target!r}"
            )

        tri_df = triangle._df
        groups = triangle._groups
        if target not in tri_df.columns:
            raise ValueError(
                f"`target={target!r}` column missing from Triangle. "
                f"Available columns: {tri_df.columns}"
            )

        # numpy generator -- one per fit so a fixed seed is reproducible.
        rng = np.random.default_rng(self.seed)

        summary_parts: list[pl.DataFrame] = []
        f_anchor_parts:   list[pl.DataFrame] = []
        sigma2_anchor_parts: list[pl.DataFrame] = []
        pseudo_parts: list[pl.DataFrame] = []

        if groups is None:
            group_values: list[Any] = [None]
        else:
            group_values = (
                tri_df[groups].unique(maintain_order=True).to_list()
            )

        # Resolve a per-group maturity map for the SA paradigm.
        mat_k_map = self._resolve_maturity_map(triangle)

        for g in group_values:
            sub = (
                tri_df
                if g is None
                else tri_df.filter(pl.col(groups) == g)
            )
            loss_obs, cohorts, devs = self._build_obs_matrix(sub, target)

            anchor = _boot_anchor_cl(loss_obs)
            stage1 = self._run_group_kernel(
                sub      = sub,
                loss_obs = loss_obs,
                cohorts  = cohorts,
                devs     = devs,
                anchor   = anchor,
                target   = target,
                mat_k    = mat_k_map.get(g),
                rng      = rng,
            )
            stage1.cohorts = cohorts
            stage1.devs    = devs

            decomp = _boot_summary_decompose(
                stage1.cum_mean,
                stage1.cum_sampled,
                quantile_ci = self.quantile_ci,
            )
            summary_parts.append(
                self._decomp_to_df(decomp, cohorts, devs, groups, g)
            )
            f_anchor_parts.append(
                self._anchor_to_df(
                    anchor, groups, g,
                    cols = ["f_hat", "n_cohorts"],
                )
            )
            sigma2_anchor_parts.append(
                self._anchor_to_df(
                    anchor, groups, g,
                    cols = ["sigma2", "f_var"],
                )
            )
            if self.keep_pseudo:
                pseudo_parts.append(
                    self._pseudo_to_df(stage1, groups, g, target)
                )

        out = BootstrapTriangle()
        out._output_type   = triangle._output_type
        out._groups        = groups
        out._summary       = (
            pl.concat(summary_parts) if summary_parts else pl.DataFrame()
        )
        out._f_anchor      = (
            pl.concat(f_anchor_parts) if f_anchor_parts else pl.DataFrame()
        )
        out._sigma2_anchor = (
            pl.concat(sigma2_anchor_parts)
            if sigma2_anchor_parts
            else pl.DataFrame()
        )
        out._pseudo_triangles = (
            pl.concat(pseudo_parts)
            if (self.keep_pseudo and pseudo_parts)
            else None
        )
        out._meta = {
            "type":        self.type,
            "method":      self.method,
            "residual":    self.residual,
            "process":     self.process,
            "pooling":     self.pooling,
            "hat_adj":     self.hat_adj,
            "demean":      self.demean,
            "B":           int(self.B),
            "seed":        self.seed,
            "alpha":       float(self.alpha),
            "quantile_ci": self.quantile_ci,
            "target":      target,
            "groups":      groups,
        }
        return out

    # -- internal helpers ---------------------------------------------------

    def _resolve_maturity_map(
        self,
        triangle: "Triangle",
    ) -> dict[Any, int | None]:
        """Resolve the SA stage-transition point to a per-group ``mat_k``.

        Only the SA paradigm consumes this. The ``maturity`` config
        argument accepts four forms (R-parity 4-type dispatch):

        * ``None`` -- auto-detect via ``triangle.link().ata().maturity()``.
        * a :class:`Maturity` object -- read its ``mat_k``.
        * a callable ``f(triangle) -> Maturity``.
        * an ``int`` (or per-group ``dict``) -- a literal ``mat_k``.

        Returns a ``{group_value: mat_k}`` map. ``mat_k`` is the
        ``ata_to`` maturity point; the kernels convert it to the
        1-indexed from-dev ``mat_k - 1`` where CL begins. ``group_value``
        is ``None`` for an ungrouped Triangle.
        """
        if self.method != "sa":
            return {}

        mat = self.maturity
        if callable(mat) and not isinstance(mat, (int, np.integer)):
            mat = mat(triangle)

        groups = triangle._groups

        if mat is None:
            # Auto-detect through the link -> ata -> maturity chain.
            try:
                mat = triangle.link(target="loss").ata().maturity()
            except Exception:
                mat = None

        if isinstance(mat, Maturity):
            mk = mat.mat_k
            if isinstance(mk, dict):
                return mk
            return {None: mk} if groups is None else {}

        if isinstance(mat, dict):
            return mat
        if isinstance(mat, (int, np.integer)):
            return {None: int(mat)}
        return {}

    def _run_group_kernel(
        self,
        sub:      pl.DataFrame,
        loss_obs: np.ndarray,
        cohorts:  list,
        devs:     list,
        anchor:   _Anchor,
        target:   str,
        mat_k:    int | None,
        rng:      np.random.Generator,
    ) -> _Stage1Result:
        """Dispatch one group's triangle to the configured kernel.

        Routes ``(type, method, residual)`` to the matching Stage-1 +
        Stage-2 kernel, building the per-group residual pools / ED
        intensity anchor / projected premium as each paradigm requires.
        """
        B     = int(self.B)
        alpha = float(self.alpha)
        gi    = _build_group_inputs(loss_obs, anchor, cohorts, devs)
        n_coh, n_dev = loss_obs.shape

        # ---- type='analytical' -- Mack closed-form (CL only) -------------
        if self.type == "analytical":
            return _boot_kernel_cl_analytical(
                loss_obs = loss_obs,
                anchor   = anchor,
                B        = B,
                rng      = rng,
                alpha    = alpha,
                process  = self.process,
            )

        # ---- type='parametric' -- textbook cell-distribution sampling ---
        if self.type == "parametric":
            if self.method == "cl":
                mu_grid = _fitted_grid_cl(loss_obs, gi.last_obs, anchor,
                                          devs)
                cell = _cell_residuals_cl(
                    sub, anchor, target=target, hat_adj=self.hat_adj
                )
                return _boot_kernel_cl_parametric(
                    gi, anchor, mu_grid, cell.phi, B, rng, alpha,
                    self.process,
                )
            # ED / SA parametric need the dual-variable Link.
            link_df = _build_link_df(
                sub, None, target, "premium", None, drop_invalid=True
            )
            g_hat = _ed_intensity_anchor(link_df, anchor)
            exp_obs = _premium_obs_matrix(sub, cohorts, devs)
            premium_proj = _premium_anchor_proj(exp_obs, gi.last_obs)
            mu_ed_grid = self._ed_fitted_grid(
                loss_obs, gi.last_obs, anchor, g_hat, premium_proj, devs
            )
            if self.method == "ed":
                cell = _cell_residuals_ed(link_df)
                return _boot_kernel_ed_parametric(
                    gi, anchor, mu_ed_grid, g_hat, premium_proj,
                    cell.phi, B, rng, alpha, self.process,
                )
            # SA parametric.
            mu_cl_grid = _fitted_grid_cl(loss_obs, gi.last_obs, anchor,
                                         devs)
            cell_ed = _cell_residuals_ed(link_df)
            cell_cl = _cell_residuals_cl(
                sub, anchor, target=target, hat_adj=self.hat_adj
            )
            mat_vec = self._mat_k_vec(mat_k, n_coh)
            return _boot_kernel_sa_parametric(
                gi, anchor, mu_cl_grid, mu_ed_grid, g_hat, premium_proj,
                mat_vec, cell_ed.phi, cell_cl.phi, B, rng, alpha,
                self.process,
            )

        # ---- type='nonparametric' -- residual resampling ----------------
        if self.residual == "link":
            link_df = _build_link_df(
                sub, None, target, None, None, drop_invalid=True
            )
            link_res = _link_residuals_cl(link_df, anchor)
            pool = _build_pool_link(
                link_res, pooling=self.pooling, tail=self.tail,
                min_pool=self.min_pool,
            )
            return _boot_kernel_cl_link(
                gi, anchor, pool, B, rng, alpha, self.process,
            )

        # residual == "cell"
        if self.method == "cl":
            mu_grid = _fitted_grid_cl(loss_obs, gi.last_obs, anchor, devs)
            cell = _cell_residuals_cl(
                sub, anchor, target=target, hat_adj=self.hat_adj
            )
            pool = _build_pool_cell(
                cell, pooling=self.pooling, tail=self.tail,
                min_pool=self.min_pool, demean=self.demean,
            )
            return _boot_kernel_cl_cell(
                gi, anchor, mu_grid, pool, cell.phi, B, rng, alpha,
                self.process,
            )

        # ED / SA cell -- need the dual-variable Link.
        link_df = _build_link_df(
            sub, None, target, "premium", None, drop_invalid=True
        )
        g_hat = _ed_intensity_anchor(link_df, anchor)
        exp_obs = _premium_obs_matrix(sub, cohorts, devs)
        premium_proj = _premium_anchor_proj(exp_obs, gi.last_obs)
        mu_ed_grid = self._ed_fitted_grid(
            loss_obs, gi.last_obs, anchor, g_hat, premium_proj, devs
        )

        if self.method == "ed":
            cell = _cell_residuals_ed(link_df)
            pool = _build_pool_cell(
                cell, pooling=self.pooling, tail=self.tail,
                min_pool=self.min_pool, demean=self.demean,
            )
            return _boot_kernel_ed_cell(
                gi, anchor, mu_ed_grid, g_hat, premium_proj, pool,
                cell.phi, B, rng, alpha, self.process,
            )

        # SA cell -- dual residual pool (ED + CL, never merged).
        mu_cl_grid = _fitted_grid_cl(loss_obs, gi.last_obs, anchor, devs)
        cell_ed = _cell_residuals_ed(link_df)
        cell_cl = _cell_residuals_cl(
            sub, anchor, target=target, hat_adj=self.hat_adj
        )
        pool_ed = _build_pool_cell(
            cell_ed, pooling=self.pooling, tail=self.tail,
            min_pool=self.min_pool, demean=self.demean,
        )
        pool_cl = _build_pool_cell(
            cell_cl, pooling=self.pooling, tail=self.tail,
            min_pool=self.min_pool, demean=self.demean,
        )
        mat_vec = self._mat_k_vec(mat_k, n_coh)
        return _boot_kernel_sa_cell(
            gi, anchor, mu_cl_grid, mu_ed_grid, g_hat, premium_proj,
            mat_vec, pool_ed, pool_cl, cell_ed.phi, cell_cl.phi,
            B, rng, alpha, self.process,
        )

    @staticmethod
    def _ed_fitted_grid(
        loss_obs:      np.ndarray,
        last_obs:      np.ndarray,
        anchor:        _Anchor,
        g_hat:         np.ndarray,
        premium_proj:  np.ndarray,
        devs:          list,
    ) -> np.ndarray:
        """Chain-anchored ED fitted incremental backbone ``mu_ed``.

        Additive mirror of :func:`_fitted_grid_cl` -- each cohort
        anchors at its observed cumulative and rolls with additive
        steps ``g_hat[k] * premium_from`` (Renshaw-Verrall 1998's
        additive analogue). The premium matrix is the fixed projected
        premium so lower-triangle steps have a finite multiplier.
        """
        n_dev = loss_obs.shape[1]
        g_by_to = np.full(n_dev, np.nan, dtype=np.float64)
        dev_pos = {d: j for j, d in enumerate(devs)}
        for k in range(len(anchor.ata_to)):
            j = dev_pos.get(int(anchor.ata_to[k]))
            if j is not None:
                g_by_to[j] = g_hat[k]

        def fwd(cur: float, i: int, j: int) -> float:
            g_k = g_by_to[j]
            e_from = premium_proj[i, j - 1]
            if np.isfinite(g_k) and np.isfinite(e_from):
                return cur + g_k * e_from
            return cur

        def bwd(cur: float, i: int, j: int) -> float:
            g_k = g_by_to[j + 1]
            e_from = premium_proj[i, j]
            if np.isfinite(g_k) and np.isfinite(e_from):
                return cur - g_k * e_from
            return cur

        return _boot_fitted_grid(loss_obs, last_obs, fwd, bwd)

    @staticmethod
    def _mat_k_vec(mat_k: int | None, n_coh: int) -> np.ndarray:
        """Per-cohort 1-indexed from-dev where CL begins (SA stage switch).

        ``mat_k`` is the ``ata_to`` maturity point; the kernels switch
        to CL once the to-dev ``j >= mat_k - 1`` -- equivalently the
        from-dev ``>= mat_k - 1``. ``None`` -> all-ED
        (``iinfo(int64).max`` sentinel).
        """
        big = np.iinfo(np.int64).max
        if mat_k is None or not np.isfinite(mat_k):
            return np.full(n_coh, big, dtype=np.int64)
        # mat_k = ata_to (= change); from-dev where CL begins is mat_k - 1.
        return np.full(n_coh, max(int(mat_k) - 1, 0), dtype=np.int64)

    @staticmethod
    def _build_obs_matrix(
        df:     pl.DataFrame,
        target: str,
    ) -> tuple[np.ndarray, list, list]:
        """Build a ``(n_cohorts, n_devs)`` observed cumulative matrix.

        Rows are cohorts (sorted), columns are dev ``1..max_dev``.
        Unobserved cells are ``np.nan``.
        """
        df = df.sort(["cohort", "dev"])
        cohorts = df["cohort"].unique(maintain_order=True).to_list()
        devs    = sorted(df["dev"].unique().to_list())
        n_cohorts = len(cohorts)
        n_devs    = len(devs)

        cohort_index = {c: i for i, c in enumerate(cohorts)}
        dev_index    = {d: j for j, d in enumerate(devs)}

        mat = np.full((n_cohorts, n_devs), np.nan, dtype=np.float64)
        for row in df.iter_rows(named=True):
            i = cohort_index[row["cohort"]]
            j = dev_index[row["dev"]]
            v = row[target]
            if v is not None:
                mat[i, j] = v
        return mat, cohorts, devs

    @staticmethod
    def _decomp_to_df(
        decomp:  dict[str, np.ndarray],
        cohorts: list,
        devs:    list,
        groups:  str | None,
        group_value: Any | None,
    ) -> pl.DataFrame:
        """Assemble the per-cell SE-decomposition long-format DataFrame.

        ``decomp`` arrays are column-major (cohort fastest) over the
        ``(cohort, dev)`` grid -- matching ``np.repeat`` / ``np.tile``
        below.
        """
        n_cohorts = len(cohorts)
        n_devs    = len(devs)

        # cohort fastest: tile cohorts, repeat devs.
        cohort_col = cohorts * n_devs
        dev_col    = np.repeat(devs, n_cohorts).tolist()

        data: dict[str, Any] = {}
        if groups is not None:
            data[groups] = [group_value] * (n_cohorts * n_devs)
        data["cohort"] = cohort_col
        data["dev"]    = dev_col
        for key in ("mean_proj", "param_se", "proc_se",
                    "total_se", "total_cv"):
            data[key] = decomp[key]
        if "ci_lo" in decomp:
            data["ci_lo"] = decomp["ci_lo"]
            data["ci_hi"] = decomp["ci_hi"]

        return pl.DataFrame(data).sort(
            ([groups] if groups is not None else []) + ["cohort", "dev"]
        )

    @staticmethod
    def _anchor_to_df(
        anchor:      _Anchor,
        groups:      str | None,
        group_value: Any | None,
        cols:        list[str],
    ) -> pl.DataFrame:
        """Assemble a per-link anchor DataFrame for the requested columns."""
        data: dict[str, Any] = {}
        n_links = len(anchor.ata_from)
        if groups is not None:
            data[groups] = [group_value] * n_links
        data["ata_from"] = anchor.ata_from
        data["ata_to"]   = anchor.ata_to
        for c in cols:
            data[c] = getattr(anchor, c)
        return pl.DataFrame(data)

    @staticmethod
    def _pseudo_to_df(
        stage1:      _Stage1Result,
        groups:      str | None,
        group_value: Any | None,
        target:      str = "loss",
    ) -> pl.DataFrame:
        """Long-format per-replicate trajectories (Stage-1 mean + sampled).

        Columns: ``[groups?, cohort, dev, rep, {target}_mean,
        {target}_sampled]`` -- ``rep`` ranges over ``1..B``. The
        ``{target}_mean`` column is the Stage-1 deterministic projection
        (parameter uncertainty only); ``{target}_sampled`` adds the
        Stage-2 process noise. Built only when ``keep_pseudo=True``.

        The value columns are named after the bootstrap ``target`` so the
        BF / Cape Cod composition can read ``loss_mean`` / ``premium_mean``
        from the two paired :class:`BootstrapTriangle` objects -- mirrors
        R's ``.boot_build_pseudo_long`` (``R/bootstrap.R``).
        """
        cum_mean    = stage1.cum_mean        # (n_cohorts, n_devs, B)
        cum_sampled = stage1.cum_sampled
        n_cohorts, n_devs, B = cum_sampled.shape
        cohorts = stage1.cohorts
        devs    = stage1.devs

        # column-major flatten (cohort fastest, then dev, then rep).
        cohort_col = cohorts * (n_devs * B)
        dev_col = np.tile(
            np.repeat(devs, n_cohorts), B
        ).tolist()
        rep_col = np.repeat(
            np.arange(1, B + 1), n_cohorts * n_devs
        ).tolist()

        data: dict[str, Any] = {}
        if groups is not None:
            data[groups] = [group_value] * (n_cohorts * n_devs * B)
        data["cohort"]            = cohort_col
        data["dev"]               = dev_col
        data["rep"]               = rep_col
        data[f"{target}_mean"]    = cum_mean.flatten(order="F")
        data[f"{target}_sampled"] = cum_sampled.flatten(order="F")
        return pl.DataFrame(data)


# ---------------------------------------------------------------------------
# Section 6 -- Shared resolve / overlay helpers (R sibling parity)
# ---------------------------------------------------------------------------


def _resolve_bootstrap(
    bootstrap: Any,
    triangle:  "Triangle",
    *,
    target: str = "loss",
    **kw:   Any,
) -> "BootstrapTriangle | None":
    """Resolve a polymorphic ``bootstrap`` argument to a result or ``None``.

    Mirrors R's ``.resolve_bootstrap`` (``R/bootstrap.R``). Accepted
    forms for ``bootstrap``:

    * ``None`` / ``False`` -> ``None`` (bootstrap is opt-in).
    * a :class:`Bootstrap` config instance -> ``bootstrap.fit(triangle,
      target=target)``.
    * ``True`` / ``"auto"`` -> a default ``Bootstrap(**kw)`` config is
      built and fitted.
    * an existing :class:`BootstrapTriangle` -> returned as-is after
      validating that its ``meta["target"]`` equals ``target``.
    * a callable ``f`` -> ``_resolve_bootstrap(f(triangle), triangle,
      target=target)`` (the result is itself resolved recursively).

    Anything else raises :class:`TypeError`.

    Parameters
    ----------
    bootstrap
        The polymorphic bootstrap specification (see above).
    triangle
        The :class:`Triangle` the bootstrap is computed on.
    target
        Cumulative metric the bootstrap perturbs -- one of ``"loss"`` or
        ``"premium"``. Used both to fit fresh configs and to validate a
        pre-built :class:`BootstrapTriangle`.
    **kw
        Default :class:`Bootstrap` keyword arguments forwarded when
        ``bootstrap`` resolves to ``True`` / ``"auto"``.

    Returns
    -------
    BootstrapTriangle or None
        The resolved bootstrap result, or ``None`` when bootstrap is
        disabled.
    """
    if bootstrap is None or bootstrap is False:
        return None

    if bootstrap is True or bootstrap == "auto":
        return Bootstrap(**kw).fit(triangle, target=target)

    if isinstance(bootstrap, Bootstrap):
        return bootstrap.fit(triangle, target=target)

    if isinstance(bootstrap, BootstrapTriangle):
        boots_target = bootstrap.meta.get("target", "loss")
        if boots_target != target:
            raise ValueError(
                f"supplied `BootstrapTriangle` has meta['target'] = "
                f"{boots_target!r} but this fit expects target = "
                f"{target!r}."
            )
        return bootstrap

    if callable(bootstrap):
        return _resolve_bootstrap(
            bootstrap(triangle), triangle, target=target, **kw
        )

    raise TypeError(
        "`bootstrap` must be None, True/False, 'auto', a `Bootstrap` "
        "config, a `BootstrapTriangle`, or a callable returning one; "
        f"got {type(bootstrap).__name__}."
    )


def _apply_bootstrap_overlay(
    full_df: pl.DataFrame,
    boots:   "BootstrapTriangle",
    *,
    role:    str,
    se_cols: list[str],
    keys:    list[str],
) -> pl.DataFrame:
    """Overlay bootstrap SE / CV onto the projected cells of a fit grid.

    Mirrors R's ``.apply_bootstrap_overlay`` (``R/bootstrap.R``). Takes a
    fit's long-format ``$full`` DataFrame and a resolved
    :class:`BootstrapTriangle`, renames the bootstrap ``summary`` columns
    to the caller's ``<role>_*`` schema, left-joins on ``keys``, and --
    on *projected cells only* (cells where ``{role}_obs`` is null) --
    overlays the ``se_cols`` (and ``ci_lo`` / ``ci_hi`` when present)
    over the analytical values. Observed cells keep their analytical SE.

    The point projection (``{role}_proj``) is never overlaid; it always
    stays analytical.

    Parameters
    ----------
    full_df
        The fit's long-format ``$full`` DataFrame.
    boots
        A resolved :class:`BootstrapTriangle`; its ``summary`` slot
        carries ``param_se``, ``proc_se``, ``total_se``, ``total_cv``,
        and optionally ``ci_lo`` / ``ci_hi``.
    role
        Column-name prefix, ``"loss"`` or ``"premium"``.
    se_cols
        Statistic suffixes to overlay -- e.g. ``["param_se", "proc_se",
        "total_se", "total_cv"]``. The point projection is never
        overlaid.
    keys
        Join key columns, ``[groups?, "cohort", "dev"]``.

    Returns
    -------
    pl.DataFrame
        ``full_df`` with the bootstrap statistics overlaid on projected
        cells and the temporary ``_boot`` columns dropped.
    """
    bsum = boots.to_polars()

    rename_map: dict[str, str] = {
        "param_se": f"{role}_param_se_boot",
        "proc_se":  f"{role}_proc_se_boot",
        "total_se": f"{role}_total_se_boot",
        "total_cv": f"{role}_total_cv_boot",
    }
    has_ci = "ci_lo" in bsum.columns and "ci_hi" in bsum.columns
    if has_ci:
        rename_map["ci_lo"] = f"{role}_ci_lo_boot"
        rename_map["ci_hi"] = f"{role}_ci_hi_boot"

    # Keep only the join keys + the columns we are about to overlay.
    keep = list(keys) + [c for c in rename_map if c in bsum.columns]
    bsum = bsum.select(keep).rename(
        {c: rename_map[c] for c in rename_map if c in bsum.columns}
    )

    out = full_df.join(bsum, on=keys, how="left")

    # Projected cells: those where the role's observed column is null.
    obs_col = f"{role}_obs"
    is_proj = pl.col(obs_col).is_null()

    overlays = list(se_cols)
    if has_ci:
        overlays = overlays + ["ci_lo", "ci_hi"]

    exprs: list[pl.Expr] = []
    for sfx in overlays:
        col      = f"{role}_{sfx}"
        boot_col = f"{col}_boot"
        if boot_col not in out.columns:
            continue
        boot_val = pl.col(boot_col)
        if col in out.columns:
            base = pl.col(col)
        else:
            base = pl.lit(None, dtype=pl.Float64)
        exprs.append(
            pl.when(is_proj & boot_val.is_finite())
            .then(boot_val)
            .otherwise(base)
            .alias(col)
        )
    if exprs:
        out = out.with_columns(exprs)

    drop_cols = [
        f"{role}_{sfx}_boot"
        for sfx in (list(se_cols) + (["ci_lo", "ci_hi"] if has_ci else []))
    ]
    drop_cols = [c for c in drop_cols if c in out.columns]
    if drop_cols:
        out = out.drop(drop_cols)
    return out


# ---------------------------------------------------------------------------
# Section 7 -- BF / Cape Cod bootstrap composition (Phase 4)
# ---------------------------------------------------------------------------
#
# fit_bf / fit_cc with `bootstrap` non-None run TWO Triangle-level
# bootstraps -- one for `target="loss"`, one for `target="premium"` --
# with a SHARED seed, `method="cl"`, and `keep_pseudo=True`. The two
# per-replicate Stage-1 mean trajectories are then composed, one
# replicate at a time, into a BF / Cape Cod ultimate distribution.
#
# References:
#   Bornhuetter & Ferguson (1972)  -- the BF blend formula.
#   Stanard (1985)                 -- the Cape Cod pooled-ELR variant.
#   Mack (2008)                    -- the analytical MSEP path (kept for
#                                     `bootstrap=None`).


def _resolve_bootstrap_bf(
    bootstrap: Any,
    triangle:  "Triangle",
    *,
    B:        int            = 999,
    seed:     int | None     = None,
    type:     str            = "parametric",
    residual: str            = "cell",
    process:  str            = "gamma",
) -> "dict[str, BootstrapTriangle] | None":
    """Resolve the ``bootstrap`` argument of :class:`BF` / :class:`CC`.

    Four-type dispatch mirroring :func:`_resolve_bootstrap` but returning
    a *pair* of :class:`BootstrapTriangle` objects -- BF / Cape Cod
    compose loss-side parameter uncertainty (via ``q_i^b``) and
    premium-side parameter uncertainty (via ``E_i^{ult,b}``) into a
    single ultimate distribution. Mirrors R's
    ``.resolve_bootstrap_bf()`` (``R/bf.R``).

    Accepted forms for ``bootstrap``:

    * ``None`` / ``False`` -> ``None`` (point estimate only).
    * ``True`` / ``"auto"`` -> two internal :class:`Bootstrap` fits (one
      per target) sharing ``seed`` so replicate indices align, both with
      ``method="cl"`` and ``keep_pseudo=True``.
    * a ``dict`` ``{"loss": BootstrapTriangle, "premium":
      BootstrapTriangle}`` -- validated for matching ``meta["B"]`` and
      the correct per-side ``meta["target"]``.
    * a callable ``f(triangle) -> dict`` -- invoked, then re-resolved.

    Parameters
    ----------
    bootstrap
        The polymorphic specification (see above).
    triangle
        The :class:`Triangle` the bootstrap is computed on.
    B, seed
        Replicate count and shared seed for the ``"auto"`` path.
    type
        Bootstrap residual paradigm -- ``"parametric"`` /
        ``"nonparametric"`` select a simulation paradigm.
    residual
        Residual scope for ``type="nonparametric"`` (``"cell"`` /
        ``"link"``).
    process
        Stage-2 process distribution.

    Returns
    -------
    dict or None
        ``{"loss": BootstrapTriangle, "premium": BootstrapTriangle}`` or
        ``None`` when bootstrap is disabled.
    """
    if bootstrap is None or bootstrap is False:
        return None

    if bootstrap is True or bootstrap == "auto":
        # Force keep_pseudo=True -- the BF composition needs the
        # per-replicate cohort-by-dev Stage-1 means. Same seed for both
        # calls so replicate indices align across the two simulations.
        cfg_kw: dict[str, Any] = dict(
            type        = type,
            method      = "cl",
            process     = process,
            B           = B,
            seed        = seed,
            keep_pseudo = True,
            quantile_ci = False,
        )
        if type == "nonparametric":
            cfg_kw["residual"] = residual
        bt_loss = Bootstrap(**cfg_kw).fit(triangle, target="loss")
        bt_exp  = Bootstrap(**cfg_kw).fit(triangle, target="premium")
        return {"loss": bt_loss, "premium": bt_exp}

    if isinstance(bootstrap, dict) and not callable(bootstrap):
        if "loss" not in bootstrap or "premium" not in bootstrap:
            raise ValueError(
                "`bootstrap` dict must carry both a 'loss' and a "
                "'premium' BootstrapTriangle."
            )
        bt_loss = bootstrap["loss"]
        bt_exp  = bootstrap["premium"]
        if not isinstance(bt_loss, BootstrapTriangle):
            raise ValueError(
                "`bootstrap['loss']` must be a BootstrapTriangle."
            )
        if not isinstance(bt_exp, BootstrapTriangle):
            raise ValueError(
                "`bootstrap['premium']` must be a BootstrapTriangle."
            )
        t_loss = bt_loss.meta.get("target")
        t_exp  = bt_exp.meta.get("target")
        if t_loss != "loss":
            raise ValueError(
                f"`bootstrap['loss']` has meta['target'] = {t_loss!r} "
                "but BF / CC expects target = 'loss'."
            )
        if t_exp != "premium":
            raise ValueError(
                f"`bootstrap['premium']` has meta['target'] = {t_exp!r} "
                "but BF / CC expects target = 'premium'."
            )
        if bt_loss.meta.get("B") != bt_exp.meta.get("B"):
            raise ValueError(
                f"`bootstrap['loss']` B ({bt_loss.meta.get('B')}) must "
                f"equal `bootstrap['premium']` B "
                f"({bt_exp.meta.get('B')})."
            )
        if bt_loss.pseudo_triangles is None \
                or bt_exp.pseudo_triangles is None:
            raise ValueError(
                "BF / CC bootstrap composition requires keep_pseudo=True "
                "on both BootstrapTriangle objects."
            )
        return {"loss": bt_loss, "premium": bt_exp}

    if callable(bootstrap):
        return _resolve_bootstrap_bf(
            bootstrap(triangle), triangle,
            B=B, seed=seed, type=type, residual=residual, process=process,
        )

    raise TypeError(
        "`bootstrap` must be None, True/False, 'auto', a dict "
        "{'loss': BootstrapTriangle, 'premium': BootstrapTriangle}, or a "
        f"callable returning one; got {type(bootstrap).__name__}."
    )


class BFBootstrap:
    """Bootstrap helper for a :class:`BF` fit.

    Holds the two paired :class:`BootstrapTriangle` objects (loss-side
    and premium-side), the per-replicate ultimate table, and the run
    metadata. R-parity name for ``R/bf.R``'s ``BFBootstrap`` structure.

    Attributes
    ----------
    loss_bootstrap, premium_bootstrap
        The loss-side and premium-side :class:`BootstrapTriangle`.
    ult_replicates
        Long-format DataFrame ``[groups?, cohort, b, q_b, premium_ult_b,
        elr_b, loss_ult_b]`` -- the per-replicate composed ultimate.
    B, seed, type, residual, process
        Run-configuration metadata.
    """

    _is_cape_cod = False

    def __init__(
        self,
        loss_bootstrap:    "BootstrapTriangle",
        premium_bootstrap: "BootstrapTriangle",
        ult_replicates:    pl.DataFrame,
        *,
        B:        int,
        seed:     int | None,
        type:     str,
        residual: str,
        process:  str,
    ) -> None:
        self.loss_bootstrap    = loss_bootstrap
        self.premium_bootstrap = premium_bootstrap
        self.ult_replicates    = ult_replicates
        self.B        = B
        self.seed     = seed
        self.type     = type
        self.residual = residual
        self.process  = process

    def __repr__(self) -> str:
        return (
            f"<BFBootstrap: B={self.B}, type={self.type}, "
            f"seed={self.seed}>"
        )


class CCBootstrap(BFBootstrap):
    """Bootstrap helper for a :class:`CC` fit.

    Like :class:`BFBootstrap` but additionally carries
    :attr:`elr_cc_replicates` -- the per-replicate pooled-ELR draws,
    since the Cape Cod ELR is itself data-driven and thus uncertain.
    R-parity name for ``R/bf.R``'s ``CCBootstrap`` structure.

    Attributes
    ----------
    elr_cc_replicates
        Long-format DataFrame ``[groups?, b, elr_cc_b]`` -- the
        per-replicate data-pooled ELR.
    """

    _is_cape_cod = True

    def __init__(
        self,
        loss_bootstrap:    "BootstrapTriangle",
        premium_bootstrap: "BootstrapTriangle",
        ult_replicates:    pl.DataFrame,
        elr_cc_replicates: pl.DataFrame,
        *,
        B:        int,
        seed:     int | None,
        type:     str,
        residual: str,
        process:  str,
    ) -> None:
        super().__init__(
            loss_bootstrap, premium_bootstrap, ult_replicates,
            B=B, seed=seed, type=type, residual=residual, process=process,
        )
        self.elr_cc_replicates = elr_cc_replicates

    def __repr__(self) -> str:
        return (
            f"<CCBootstrap: B={self.B}, type={self.type}, "
            f"seed={self.seed}>"
        )


def _last_dev_value(df: pl.DataFrame, value_col: str,
                    by_cols: list[str]) -> pl.DataFrame:
    """Per-key value of ``value_col`` at the row with the largest ``dev``.

    Mirrors R's ``.SD[which.max(dev)]`` per-group extraction.
    """
    return (
        df.sort("dev")
        .group_by(by_cols, maintain_order=True)
        .agg(pl.col(value_col).last().alias(value_col))
    )


def _quantile_type1(x: np.ndarray, p: float) -> float:
    """Empirical quantile, R ``type = 1`` (== numpy ``inverted_cdf``)."""
    finite = x[np.isfinite(x)]
    if finite.size == 0:
        return np.nan
    return float(np.quantile(finite, p, method="inverted_cdf"))


def _bf_compose_bootstrap(
    boots:           "dict[str, BootstrapTriangle]",
    *,
    full_df:         pl.DataFrame,
    summary_df:      pl.DataFrame,
    groups:          str | None,
    cape_cod:        bool,
    prior_df:        pl.DataFrame | None,
    conf_level:      float,
    rng:             np.random.Generator | None = None,
) -> dict[str, Any]:
    """Per-replicate BF / Cape Cod composition from two BootstrapTriangle.

    Given paired loss-side and premium-side :class:`BootstrapTriangle`
    objects (both ``keep_pseudo=True``, so the per-replicate
    cohort-by-dev Stage-1 means are available), compose the BF / Cape Cod
    ultimate distribution per replicate. Mirrors R's
    ``.bf_compose_bootstrap()`` (``R/bf.R``).

    Per replicate ``b``:

    * ``q_b = loss_latest / loss_ult_cl_b`` where ``loss_ult_cl_b`` is
      the loss-side Stage-1 mean trajectory's last-dev cell.
    * ``premium_ult_b`` -- the premium-side Stage-1 mean last-dev cell.
    * ``elr_b`` -- BF: the prior ELR, drawn ``Normal(elr, elr_se)``
      floored at 0 per replicate when the prior carries a finite
      ``elr_se``; CC: per group ``sum(loss_latest) / sum(premium_ult_b *
      q_b)``.
    * ``loss_ult_b = loss_latest + (1 - q_b) * elr_b * premium_ult_b``.
    * cell-level: scale the per-replicate CL emergence pattern so each
      cohort lands at ``loss_ult_b`` (the same flat-step-to-ultimate rule
      the analytical :func:`_bf_cell_projection` uses).

    SE = SD across replicates; CI = type-1 quantiles (2.5 / 97.5%).

    Parameters
    ----------
    boots
        ``{"loss": BootstrapTriangle, "premium": BootstrapTriangle}``.
    full_df
        The point-estimate ``$full`` cell-level DataFrame.
    summary_df
        The point-estimate cohort-level ``$summary`` DataFrame.
    groups
        The grouping column name, or ``None``.
    cape_cod
        Whether to data-pool the ELR per replicate (Cape Cod) or use the
        supplied per-cohort prior (BF).
    prior_df
        Per-cohort ELR table ``[groups?, cohort, elr, elr_se]`` for BF;
        ``None`` for Cape Cod.
    conf_level
        Confidence level for the quantile CI.
    rng
        Random generator for the distribution-prior ELR draw. A fresh
        default generator is used when ``None``.

    Returns
    -------
    dict
        ``{"full", "summary", "ult_replicates", "elr_cc_replicates"}`` --
        the SE/CI-overlaid cell-level + cohort-level tables, the
        per-replicate ultimate table, and (Cape Cod only) the
        per-replicate pooled-ELR table.
    """
    if rng is None:
        rng = np.random.default_rng()

    bt_loss = boots["loss"]
    bt_exp  = boots["premium"]
    pl_pseudo = bt_loss.pseudo_triangles
    pe_pseudo = bt_exp.pseudo_triangles
    if pl_pseudo is None or pe_pseudo is None:
        raise ValueError(
            "BF / CC bootstrap composition requires keep_pseudo=True on "
            "both BootstrapTriangle objects."
        )
    # mirror_output may have handed back pandas; normalise to polars.
    if not isinstance(pl_pseudo, pl.DataFrame):
        pl_pseudo = pl.from_pandas(pl_pseudo)
    if not isinstance(pe_pseudo, pl.DataFrame):
        pe_pseudo = pl.from_pandas(pe_pseudo)

    by_cols  = ([groups] if groups is not None else []) + ["cohort"]
    by_grp   = [groups] if groups is not None else []
    cell_key = by_cols + ["dev"]
    alpha2   = (1.0 - conf_level) / 2.0

    # --- per (by_cols, rep) Stage-1 mean ultimates -------------------------
    # loss_ult_cl_b = loss-side Stage-1 mean last-dev cell.
    ult_loss = (
        _last_dev_value(pl_pseudo, "loss_mean", by_cols + ["rep"])
        .rename({"loss_mean": "loss_ult_cl_b"})
    )
    # premium_ult_b = premium-side Stage-1 mean last-dev cell.
    ult_exp = (
        _last_dev_value(pe_pseudo, "premium_mean", by_cols + ["rep"])
        .rename({"premium_mean": "premium_ult_b"})
    )

    # per-cohort observed latest loss (anchor) from the point-estimate grid.
    latest_loss = (
        full_df.filter(pl.col("loss_obs").is_not_null())
        .pipe(_last_dev_value, "loss_obs", by_cols)
        .rename({"loss_obs": "loss_latest"})
    )

    ult_b = (
        ult_loss
        .join(ult_exp, on=by_cols + ["rep"], how="inner")
        .join(latest_loss, on=by_cols, how="left")
    )
    ult_b = ult_b.with_columns(
        pl.when(
            pl.col("loss_ult_cl_b").is_finite()
            & (pl.col("loss_ult_cl_b") > 0.0)
        )
        .then(pl.col("loss_latest") / pl.col("loss_ult_cl_b"))
        .otherwise(None)
        .alias("q_b")
    )

    elr_cc_replicates: pl.DataFrame | None = None
    if cape_cod:
        # Per (group, rep) data-pooled ELR.
        elr_boot = (
            ult_b.group_by(by_grp + ["rep"], maintain_order=True)
            .agg(
                (
                    pl.col("loss_latest").sum()
                    / (pl.col("premium_ult_b") * pl.col("q_b")).sum()
                ).alias("elr_cc_b")
            )
        )
        join_on = by_grp + ["rep"] if by_grp else ["rep"]
        ult_b = ult_b.join(elr_boot, on=join_on, how="left")
        ult_b = ult_b.with_columns(
            pl.col("elr_cc_b").alias("elr_b")
        )
        elr_cc_replicates = (
            ult_b.select(by_grp + ["rep", "elr_cc_b"])
            .unique(maintain_order=True)
            .rename({"rep": "b"})
        )
    else:
        # BF: join the per-cohort prior. A finite elr_se draws a
        # per-replicate Normal(elr, elr_se) ELR (floored at 0); a
        # deterministic prior keeps the fixed point ELR per replicate.
        if prior_df is None:
            raise ValueError("BF composition requires `prior_df`.")
        ult_b = ult_b.join(prior_df, on=by_cols, how="left")
        elr_arr    = ult_b["elr"].to_numpy().astype(np.float64)
        elr_se_arr = ult_b["elr_se"].to_numpy().astype(np.float64)
        elr_b_arr  = elr_arr.copy()
        has_se = np.isfinite(elr_se_arr) & (elr_se_arr > 0.0)
        if has_se.any():
            draws = rng.normal(
                elr_arr[has_se], elr_se_arr[has_se]
            )
            elr_b_arr[has_se] = np.maximum(0.0, draws)
        ult_b = ult_b.with_columns(
            pl.Series("elr_b", elr_b_arr)
        )

    ult_b = ult_b.with_columns(
        (
            pl.col("loss_latest")
            + (1.0 - pl.col("q_b")) * pl.col("elr_b")
            * pl.col("premium_ult_b")
        ).alias("loss_ult_b")
    )

    # --- cohort-level SE / CI on loss_ult_b across replicates -------------
    ult_summary = _agg_sd_quantile(
        ult_b, "loss_ult_b", by_cols, alpha2
    )
    ult_summary = ult_summary.join(
        summary_df.select(by_cols + ["loss_ult"]), on=by_cols, how="left"
    )
    ult_summary = ult_summary.with_columns(
        pl.when(
            pl.col("loss_ult").is_finite() & (pl.col("loss_ult") > 0.0)
        )
        .then(pl.col("loss_total_se") / pl.col("loss_ult"))
        .otherwise(None)
        .alias("loss_total_cv")
    )

    summary_out = _overlay_cohort_se(
        summary_df, ult_summary, by_cols,
        ["loss_total_se", "loss_total_cv", "loss_ci_lo", "loss_ci_hi"],
    )

    # --- cell-level projection per replicate -----------------------------
    # Per (by_cols, dev, rep): scale the per-replicate CL emergence
    # pattern (loss_mean) so each cohort lands at loss_ult_b.
    cell = (
        pl_pseudo
        .join(latest_loss, on=by_cols, how="left")
        .join(
            ult_b.select(by_cols + ["rep", "loss_ult_b"]),
            on=by_cols + ["rep"], how="left",
        )
    )
    cell = _cell_proj_per_rep(cell, by_cols)

    cell_summary = _agg_sd_quantile(
        cell, "loss_proj_b", cell_key, alpha2
    )

    full_out = _overlay_cell_se(
        full_df, cell_summary, cell_key,
        ["loss_total_se", "loss_ci_lo", "loss_ci_hi"],
    )

    # per-replicate ultimate table kept on the helper.
    ult_replicates = (
        ult_b.select(
            by_cols + ["rep", "q_b", "premium_ult_b", "elr_b",
                       "loss_ult_b"]
        )
        .rename({"rep": "b"})
    )

    return {
        "full":              full_out,
        "summary":           summary_out,
        "ult_replicates":    ult_replicates,
        "elr_cc_replicates": elr_cc_replicates,
    }


def _agg_sd_quantile(
    df:       pl.DataFrame,
    value:    str,
    by_cols:  list[str],
    alpha2:   float,
) -> pl.DataFrame:
    """Per-key SD + type-1 lower/upper quantiles of ``value``.

    Returns columns ``by_cols + [loss_total_se, loss_ci_lo,
    loss_ci_hi]`` -- the SD becomes the bootstrap SE, the quantiles the
    CI. Mirrors R's ``stats::sd`` + ``stats::quantile(type = 1L)``.
    """
    rows: list[dict[str, Any]] = []
    for key, sub in df.group_by(by_cols, maintain_order=True):
        vals = sub[value].to_numpy().astype(np.float64)
        finite = vals[np.isfinite(vals)]
        row: dict[str, Any] = {}
        key_tuple = key if isinstance(key, tuple) else (key,)
        for col, kv in zip(by_cols, key_tuple):
            row[col] = kv
        if finite.size >= 2:
            row["loss_total_se"] = float(np.std(finite, ddof=1))
        elif finite.size == 1:
            row["loss_total_se"] = 0.0
        else:
            row["loss_total_se"] = None
        row["loss_ci_lo"] = _quantile_type1(vals, alpha2)
        row["loss_ci_hi"] = _quantile_type1(vals, 1.0 - alpha2)
        rows.append(row)
    return pl.DataFrame(rows, infer_schema_length=None)


def _cell_proj_per_rep(
    cell:    pl.DataFrame,
    by_cols: list[str],
) -> pl.DataFrame:
    """Per-replicate BF / CC cell-level projection ``loss_proj_b``.

    Per ``(by_cols, rep)`` group: ``cl_rem = loss_mean - loss_latest``,
    ``scale_b = (loss_ult_b - loss_latest) / cl_rem`` evaluated at the
    last dev (a cohort scalar; 0 when CL projects no remainder),
    ``loss_proj_b = loss_latest + cl_rem * scale_b``. Mirrors the
    ``loss_proj_b`` block of R's ``.bf_compose_bootstrap()``.
    """
    eps = float(np.finfo(np.float64).eps)
    parts: list[pl.DataFrame] = []
    for _key, sub in cell.group_by(
        by_cols + ["rep"], maintain_order=True
    ):
        sub = sub.sort("dev")
        loss_mean   = sub["loss_mean"].to_numpy().astype(np.float64)
        loss_latest = sub["loss_latest"].to_numpy().astype(np.float64)
        loss_ult_b  = sub["loss_ult_b"].to_numpy().astype(np.float64)
        cl_rem      = loss_mean - loss_latest
        cl_rem_last = cl_rem[-1] if cl_rem.size else np.nan
        ult_rem     = loss_ult_b[0] - loss_latest[0] if loss_ult_b.size \
            else np.nan
        if np.isfinite(cl_rem_last) and abs(cl_rem_last) > eps:
            scale_b = ult_rem / cl_rem_last
        else:
            scale_b = 0.0
        loss_proj_b = loss_latest + cl_rem * scale_b
        parts.append(
            sub.with_columns(pl.Series("loss_proj_b", loss_proj_b))
        )
    return pl.concat(parts) if parts else cell.with_columns(
        pl.lit(None, dtype=pl.Float64).alias("loss_proj_b")
    )


def _overlay_cohort_se(
    summary_df: pl.DataFrame,
    boot_se:    pl.DataFrame,
    by_cols:    list[str],
    cols:       list[str],
) -> pl.DataFrame:
    """Replace the analytical cohort-level SE/CI columns with bootstrap.

    Drops any stale ``cols`` from ``summary_df``, then left-joins the
    bootstrap ``boot_se`` table on ``by_cols``. Mirrors the
    ``drop_cols`` + ``merge`` block of R's ``.bf_compose_bootstrap()``.
    """
    drop = [c for c in cols if c in summary_df.columns]
    out = summary_df.drop(drop) if drop else summary_df
    return out.join(
        boot_se.select(by_cols + cols), on=by_cols, how="left"
    )


def _overlay_cell_se(
    full_df:   pl.DataFrame,
    boot_se:   pl.DataFrame,
    cell_key:  list[str],
    cols:      list[str],
) -> pl.DataFrame:
    """Overlay bootstrap cell-level SE/CI onto a BF / CC ``$full`` grid.

    Drops any stale SE/CI columns, left-joins ``boot_se`` on
    ``cell_key``, then derives ``loss_total_cv`` from the joined
    ``loss_total_se`` and the point ``loss_proj``. Mirrors the cell-level
    ``merge`` + ``loss_total_cv`` block of R's
    ``.bf_compose_bootstrap()``.
    """
    se_ci = ["loss_total_se", "loss_total_cv", "loss_ci_lo", "loss_ci_hi"]
    drop = [c for c in se_ci if c in full_df.columns]
    out = full_df.drop(drop) if drop else full_df
    out = out.join(
        boot_se.select(cell_key + cols), on=cell_key, how="left"
    )
    out = out.with_columns(
        pl.when(
            pl.col("loss_proj").is_finite() & (pl.col("loss_proj") > 0.0)
        )
        .then(pl.col("loss_total_se") / pl.col("loss_proj"))
        .otherwise(None)
        .alias("loss_total_cv")
    )
    return out
