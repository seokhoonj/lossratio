"""Regime go-forward band: a second, independent tail estimate.

A regime (segment-borrowed) loss-ratio fit projects the recent segment's
unobserved tail one way -- by borrowing the donor segment's development
shape (the *borrow leg*). That tail is genuinely unobserved, so a single
number hides how much of it is data versus assumption.

This module adds a SECOND, independent tail estimate -- the recent
segment's OWN intensity ``g_k`` extrapolated by a parametric
:class:`~lossratio.Curve` (the *curve leg*) -- and exposes the SPREAD
between the two legs as an honest band. When the legs agree (a mature
recent segment) the band is narrow and the go-forward loss ratio is
trustworthy; when they diverge (a young recent segment) the band is wide
and the projection is assumption territory.

The band is recent-segment-scoped: one row per group that has a regime
change, about the newest segment only (latest-only treatment). Groups
without a regime change produce no row. The curve fit's provenance
(point count, under-determined flag, alternate-law swing) rides along on
the output so a confident point is never emitted on a degenerate fit.

This is additive: the borrow leg is read verbatim from
:meth:`RatioFit.segment_summary`, the curve leg never writes back into
the fit, and a run that does not ask for the band is byte-identical.
"""

from __future__ import annotations

import dataclasses
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import normalize_groups
from .curve import Curve
from .regime import _GRAIN_MONTHS, _coarsen_triangle

if TYPE_CHECKING:
    from .curve import CurveResult
    from .ratio import RatioFit


# Output schema (group keys are prepended at build time). Kept here so the
# empty-frame path and the populated path share one column order / dtype
# contract.
_BAND_SCHEMA: dict[str, pl.DataType] = {
    "segment":                pl.Utf8,
    "change_from":            pl.Date,
    "n_cohorts":              pl.Int64,
    "premium_proj":            pl.Float64,
    "loss_proj_borrow":        pl.Float64,
    "ratio_proj_borrow":       pl.Float64,
    "loss_proj_curve":         pl.Float64,
    "ratio_proj_curve":        pl.Float64,
    "loss_proj_mean":          pl.Float64,
    "ratio_proj_mean":         pl.Float64,
    "band_lo":                pl.Float64,
    "band_hi":                pl.Float64,
    "band_width":             pl.Float64,
    "band_status":            pl.Utf8,
    "curve_n_points":         pl.Int64,
    "curve_under_determined": pl.Boolean,
    "curve_reason":           pl.Utf8,
    "curve_diverged":         pl.Boolean,
    "curve_alt_ratio_proj":    pl.Float64,
}


def _assign_segments(cohorts: list[Any], change_dates: list[Any]) -> list[int]:
    """Per-cohort segment id = count of change dates at or before the cohort.

    Oldest cohorts (before any change) are segment ``0``; each change
    advances the id. The single source of truth for the cohort -> segment
    assignment shared by :meth:`RatioFit.segment_summary` and the band, so
    the two surfaces never drift.
    """
    return [sum(1 for d in change_dates if d <= c) for c in cohorts]


def _recent_segment_cohorts(
    cohorts: list[Any], change_dates: list[Any]
) -> tuple[list[Any], Any | None]:
    """Identify the recent-segment cohorts and the date the segment starts.

    Mirrors :meth:`RatioFit.segment_summary`'s assignment exactly: each
    cohort's segment id is the count of change dates at or before it, and
    the recent segment is the maximum such id. Returns the cohorts in the
    recent segment and ``change_from`` (``max(change_dates)``), or
    ``([], None)`` when there is no change.
    """
    if not change_dates:
        return [], None
    seg = _assign_segments(cohorts, change_dates)
    recent = max(seg)
    if recent == 0:
        # No cohort actually reached a post-change segment.
        return [], None
    recent_cohorts = [c for c, s in zip(cohorts, seg) if s == recent]
    change_from = sorted(change_dates)[-1]
    return recent_cohorts, change_from


def _own_intensity(sub: pl.DataFrame, n_durations: int) -> np.ndarray:
    """Recent-segment OWN aggregated intensity ``g_k`` per link.

    Builds dense ``(n_cohorts, n_durations)`` cumulative ``loss_obs`` (``L``)
    and ``premium_obs`` (``P``) matrices (array slot ``j`` = duration ``j+1``),
    then aggregates the additive intensity over cohorts present at both
    ends of each link with a positive from-duration premium::

        g_k = sum(L[:, k+1] - L[:, k]) / sum(P[:, k])

    over the valid cohorts. ``g_k[j]`` is the intensity for the link
    starting at from-duration ``j+1`` (length ``n_durations - 1``). Uses the
    segment's own observed development -- no donor, so no level
    contamination.
    """
    cohorts = sorted(sub["cohort"].unique().to_list())
    row_of = {c: i for i, c in enumerate(cohorts)}
    L = np.full((len(cohorts), n_durations), np.nan, dtype=float)
    P = np.full((len(cohorts), n_durations), np.nan, dtype=float)
    for c, d, lo, po in zip(
        sub["cohort"].to_list(),
        sub["duration"].to_list(),
        sub["loss_obs"].to_list(),
        sub["premium_obs"].to_list(),
    ):
        j = int(d) - 1
        if 0 <= j < n_durations:
            i = row_of[c]
            L[i, j] = lo if lo is not None else np.nan
            P[i, j] = po if po is not None else np.nan

    dl = L[:, 1:] - L[:, :-1]          # col j = link from-duration (j+1) -> (j+2)
    pk = P[:, :-1]                      # premium at the FROM duration of each link
    valid = np.isfinite(dl) & np.isfinite(pk) & (pk > 0.0)
    num = np.where(valid, dl, 0.0).sum(axis=0)
    den = np.where(valid, pk, 0.0).sum(axis=0)
    with np.errstate(invalid="ignore", divide="ignore"):
        g_k = np.where(den > 0.0, num / den, np.nan)
    return g_k


def _curve_leg_loss_proj(
    cr: "CurveResult", sub: pl.DataFrame, n_durations: int
) -> tuple[float, bool]:
    """Curve-leg ultimate loss summed over the recent cohorts.

    Per cohort with last observed duration ``last`` (1-indexed), the ultimate
    loss is the observed cumulative loss at ``last`` plus the
    premium-weighted sum of the fitted intensity over the unobserved
    from-duration positions ``last .. n_durations-1``::

        loss_proj = L_obs[last] + sum_k cr.evaluate(k) * premium_proj[k]

    ``cr.evaluate(k)`` is the intensity at from-duration ``k``; the increment
    lands at duration ``k+1`` (the engine's ED from-column convention). Terms
    where ``evaluate`` is ``nan`` or ``premium_proj`` is null are skipped.
    Returns ``(loss_proj_curve, any_term)`` -- ``any_term`` is ``True``
    when at least one tail term was added (used to detect an all-empty
    degenerate sum).
    """
    cohorts = sorted(sub["cohort"].unique().to_list())
    row_of = {c: i for i, c in enumerate(cohorts)}
    L = np.full((len(cohorts), n_durations), np.nan, dtype=float)
    Pp = np.full((len(cohorts), n_durations), np.nan, dtype=float)
    for c, d, lo, pp in zip(
        sub["cohort"].to_list(),
        sub["duration"].to_list(),
        sub["loss_obs"].to_list(),
        sub["premium_proj"].to_list(),
    ):
        j = int(d) - 1
        if 0 <= j < n_durations:
            i = row_of[c]
            L[i, j] = lo if lo is not None else np.nan
            Pp[i, j] = pp if pp is not None else np.nan

    loss_proj = 0.0
    any_term = False
    for i in range(len(cohorts)):
        finite = np.flatnonzero(np.isfinite(L[i]))
        if finite.size == 0:
            continue
        last = int(finite[-1]) + 1  # 1-indexed last observed duration
        loss_proj += float(L[i, last - 1])
        ks = np.arange(last, n_durations)  # from-duration positions, 1-indexed
        if ks.size == 0:
            continue
        ev = cr.evaluate(ks.astype(float))
        pr = Pp[i, ks - 1]
        term_mask = np.isfinite(ev) & np.isfinite(pr)
        if term_mask.any():
            any_term = True
            loss_proj += float(np.sum(ev[term_mask] * pr[term_mask]))
    return loss_proj, any_term


# Two-leg spread (relative to the level) at or below which the band counts
# as "narrow" -- the borrow and curve legs agree, so the go-forward is
# well determined by both tail methods. Above it the legs disagree enough
# that the tail is assumption territory.
_NARROW_BAND_REL = 0.10


def _band_status(
    *,
    degenerate: bool,
    under_determined: bool,
    band_width: float | None,
    ratio_proj_borrow: float | None,
) -> str:
    """One-glance honesty flag -- ``"narrow"`` / ``"wide"`` / ``"degenerate"``.

    Driven by the agreement of the two principal legs (borrow vs curve)
    relative to the level -- that is the band this increment exists to
    expose. ``"degenerate"``: no curve fit. ``"wide"``: the curve is
    under-determined (too few points, so a small spread is not earned), OR
    the legs disagree by more than ``_NARROW_BAND_REL`` of the level.
    ``"narrow"``: the legs agree on a data-sufficient fit. The curve's own
    divergence and alt-law swing are reported separately
    (``curve_diverged`` / ``curve_alt_ratio_proj``) -- they qualify the
    curve leg but do not override a genuine two-leg agreement.
    """
    if degenerate:
        return "degenerate"

    # Narrowness must be earned by data sufficiency: a 2-point fit can land
    # an accidentally small spread, so an under-determined curve is wide
    # regardless of width.
    if under_determined:
        return "wide"

    if (
        band_width is not None
        and ratio_proj_borrow is not None
        and ratio_proj_borrow != 0.0
        and band_width / abs(ratio_proj_borrow) > _NARROW_BAND_REL
    ):
        return "wide"

    return "narrow"


def _segment_band(
    fit: "RatioFit", *, curve: Curve | None, tol: float
) -> pl.DataFrame:
    """Build the regime go-forward band frame (one row per regime group).

    See :meth:`RatioFit.segment_band` for the public contract. ``tol`` is
    accepted for forward-compatibility with the curve contract but is
    currently inert: the per-position ``evaluate`` path does not
    early-stop, so the premium-weighted sum is never truncated.
    """
    del tol  # currently inert; see docstring.

    if curve is None:
        curve = Curve(target="intensity", family="inverse_power", min_points=3)
    if curve.target != "intensity":
        raise ValueError(
            "segment_band requires a curve with target='intensity' (the "
            f"band formula is g_k-based); got target={curve.target!r}."
        )

    gcols = normalize_groups(fit._groups)
    empty = _empty_band(gcols, fit._output_type)

    reg = fit._regime
    if reg is None or reg._changes_df.is_empty():
        return empty

    df = fit.to_polars()
    df = df if isinstance(df, pl.DataFrame) else pl.from_pandas(df)
    df = df.with_columns(pl.col("cohort").cast(pl.Date))

    changes = reg._changes_df

    # Borrow leg, verbatim: pull the recent-segment row per group from the
    # existing surface so the two tables never drift.
    seg = fit.segment_summary()
    seg = seg if isinstance(seg, pl.DataFrame) else pl.from_pandas(seg)

    parts = (
        list(df.partition_by(gcols, as_dict=True).items())
        if gcols
        else [((), df)]
    )

    rows: list[dict] = []
    for key, gdf in sorted(parts, key=lambda kv: _sort_key(kv[0])):
        keyvals = key if isinstance(key, tuple) else (key,)

        change_dates: list = []
        chg = changes
        for c, v in zip(gcols, keyvals):
            if c in chg.columns:
                chg = chg.filter(pl.col(c) == v)
        change_dates = sorted(chg["change"].to_list())
        if not change_dates:
            continue  # group without a regime change -> no row.

        cohorts = sorted(gdf["cohort"].unique().to_list())
        recent_cohorts, change_from = _recent_segment_cohorts(
            cohorts, change_dates
        )
        if not recent_cohorts:
            continue

        sub = gdf.filter(pl.col("cohort").is_in(recent_cohorts))
        n_durations = int(gdf["duration"].max())

        # Borrow leg from segment_summary (recent segment row of this group).
        borrow = _borrow_row(seg, gcols, keyvals, change_from)
        if borrow is None:
            continue
        loss_proj_borrow = borrow["loss_proj"]
        ratio_proj_borrow = borrow["ratio_proj"]
        premium_proj = borrow["premium_proj"]
        n_cohorts = borrow["n_cohorts"]

        # Curve leg: own intensity -> Curve fit -> premium-weighted tail.
        g_k = _own_intensity(sub, n_durations)
        cr = curve.fit(g_k)
        loss_proj_curve, any_term = _curve_leg_loss_proj(cr, sub, n_durations)

        degenerate = cr.slope is None or not any_term
        if degenerate:
            ratio_proj_curve = None
            loss_proj_curve_out = None
            band_lo = band_hi = band_width = None
            curve_alt_ratio_proj = None
        else:
            loss_proj_curve_out = loss_proj_curve
            ratio_proj_curve = (
                loss_proj_curve / premium_proj if premium_proj else None
            )
            if ratio_proj_curve is None:
                band_lo = band_hi = band_width = None
            else:
                band_lo = min(ratio_proj_borrow, ratio_proj_curve)
                band_hi = max(ratio_proj_borrow, ratio_proj_curve)
                band_width = band_hi - band_lo
            curve_alt_ratio_proj = _alt_curve_ratio(
                cr, g_k, sub, n_durations, premium_proj
            )

        # Mean leg: the midpoint of the two tail estimates -- a single
        # computable headline number (the band rides alongside on its own
        # columns). Falls back to the borrow leg when the curve is
        # degenerate, so the column is never null.
        if not degenerate and ratio_proj_curve is not None:
            loss_proj_mean = (loss_proj_borrow + loss_proj_curve_out) / 2.0
            ratio_proj_mean = (ratio_proj_borrow + ratio_proj_curve) / 2.0
        else:
            loss_proj_mean = loss_proj_borrow
            ratio_proj_mean = ratio_proj_borrow

        status = _band_status(
            degenerate=degenerate,
            under_determined=bool(cr.under_determined),
            band_width=band_width,
            ratio_proj_borrow=ratio_proj_borrow,
        )

        row: dict[str, Any] = {c: v for c, v in zip(gcols, keyvals)}
        row.update(
            segment=borrow["segment"],
            change_from=change_from,
            n_cohorts=int(n_cohorts),
            premium_proj=premium_proj,
            loss_proj_borrow=loss_proj_borrow,
            ratio_proj_borrow=ratio_proj_borrow,
            loss_proj_curve=loss_proj_curve_out,
            ratio_proj_curve=ratio_proj_curve,
            loss_proj_mean=loss_proj_mean,
            ratio_proj_mean=ratio_proj_mean,
            band_lo=band_lo,
            band_hi=band_hi,
            band_width=band_width,
            band_status=status,
            curve_n_points=int(cr.n_points),
            curve_under_determined=bool(cr.under_determined),
            curve_reason=cr.reason,
            curve_diverged=bool(cr.diverged),
            curve_alt_ratio_proj=curve_alt_ratio_proj,
        )
        rows.append(row)

    if not rows:
        return empty

    schema = {**{c: pl.Utf8 for c in gcols}, **_BAND_SCHEMA}
    # Infer group-column dtypes from the fit frame so the populated frame
    # matches the empty frame's group-key dtypes.
    out = pl.DataFrame(rows, schema=_resolve_schema(schema, df, gcols))
    from ._io import mirror_output

    return mirror_output(out, fit._output_type)


def _empty_band(gcols: list[str], output_type: str) -> pl.DataFrame:
    """Zero-row, fully-typed band frame (the no-regime / no-row posture)."""
    from ._io import mirror_output

    schema = {**{c: pl.Utf8 for c in gcols}, **_BAND_SCHEMA}
    return mirror_output(
        pl.DataFrame(schema=schema), output_type
    )


def _resolve_schema(
    schema: dict[str, pl.DataType], df: pl.DataFrame, gcols: list[str]
) -> dict[str, pl.DataType]:
    """Use the source frame's dtypes for the group columns."""
    resolved = dict(schema)
    for c in gcols:
        if c in df.columns:
            resolved[c] = df.schema[c]
    return resolved


def _sort_key(key: Any) -> tuple:
    """Deterministic sort key for group partitions (scalar or tuple)."""
    if isinstance(key, tuple):
        return tuple(str(k) for k in key)
    return (str(key),)


def _borrow_row(
    seg: pl.DataFrame,
    gcols: list[str],
    keyvals: tuple,
    change_from: Any,
) -> dict | None:
    """Recent-segment row from ``segment_summary`` for one group.

    The recent segment is the row whose ``change_from`` equals this
    group's ``max(change_dates)`` (excludes the ``"total"`` row, whose
    ``change_from`` is null). Returns ``None`` when not found.
    """
    sub = seg
    for c, v in zip(gcols, keyvals):
        if c in sub.columns:
            sub = sub.filter(pl.col(c) == v)
    sub = sub.filter(pl.col("change_from") == change_from)
    if sub.is_empty():
        return None
    r = sub.row(0, named=True)
    return {
        "segment":     r["segment"],
        "change_from": r["change_from"],
        "n_cohorts":   r["n_cohorts"],
        "loss_proj":    r["loss_proj"],
        "premium_proj": r["premium_proj"],
        "ratio_proj":   r["ratio_proj"],
    }


def _alt_curve_ratio(
    cr: "CurveResult",
    g_k: np.ndarray,
    sub: pl.DataFrame,
    n_durations: int,
    premium_proj: float,
) -> float | None:
    """Curve-leg ratio recomputed under the fit's alternate law.

    Surfaces the model-choice swing as a ratio: re-fit the alternate law
    (``cr.alt_family``) on the SAME ``g_k`` series via a fresh
    :class:`~lossratio.Curve`, run the same premium-weighted tail sum, and
    divide by the shared denominator. This is the genuine alternate-law
    extrapolation (intercept and slope both from the alt fit, not a slope
    swap on the primary intercept). Returns ``None`` when the alt fit is
    degenerate or no tail term is produced.
    """
    if cr.alt_family is None or not premium_proj:
        return None
    alt_cr = Curve(target=cr.target, family=cr.alt_family).fit(g_k)
    if alt_cr.slope is None:
        return None
    loss_proj, any_term = _curve_leg_loss_proj(alt_cr, sub, n_durations)
    if not any_term:
        return None
    return loss_proj / premium_proj


# ---------------------------------------------------------------------------
# Auto-grain tail extrapolation (opt-in; default path above is untouched).
#
# A fresh regime is too young to observe its own tail. The Curve fits the
# intensity decay against the development INDEX (k = 1, 2, ...), not real
# time, so on an immature fine grain it recovers a slope above the
# convergence boundary -- an unphysical, divergent tail that over-projects
# the ultimate. The observed segment loss ratio is grain-invariant (the
# same actual increments over the same cumulative premium), so all of that
# divergence lives in the unobserved tail.
#
# The fix coarsens the TAIL grain (M -> Q -> H -> Y) and stops at the
# finest grain where BOTH signals fire: (a) the curve slope is past the
# convergence boundary (physical, not divergent) AND (b) the two legs
# agree (the band narrows below a relative threshold). The AND is
# essential -- signal (a) vetoes a narrow band (b) that is two legs jointly
# wrong (e.g. an immature divergent curve sitting near the borrow leg). If
# no grain fires both, the band is "insufficient" (not yet estimable) and
# the headline falls back to the more robust borrow leg.
#
# An ultimate is grain-invariant, so the reported ultimate / band just take
# the selected-grain values -- no interpolation back to the display grain
# (that display-path step is a separate, deferred increment).
# ---------------------------------------------------------------------------

# Candidate tail grains, fine -> coarse. The walk starts at the fit's own
# (display) grain and coarsens; refining is never attempted.
_AUTO_GRAIN_ORDER: tuple[str, ...] = ("M", "Q", "H", "Y")

# Two-leg agreement threshold for the auto-grain SELECTION (signal (b)):
# the band counts as converged when band_width / |ratio_proj_borrow| is at
# or below this. Seeded to the display narrowness threshold but kept a
# distinct, separately-tunable constant so a future tightening of the
# selection rule never silently moves the display verdict.
_AUTO_GRAIN_REL: float = _NARROW_BAND_REL

# ON-path schema: the OFF schema plus one column. The OFF `_BAND_SCHEMA`
# is intentionally NOT edited, so the default empty / populated frames stay
# byte-identical.
_AUTO_BAND_SCHEMA: dict[str, pl.DataType] = {
    **_BAND_SCHEMA,
    "selected_grain": pl.Utf8,
}


def _refit_at_grain(fit: "RatioFit", g: str) -> "RatioFit | None":
    """Re-fit the same estimator on the triangle coarsened to grain ``g``.

    Returns the fit itself when ``g`` is the display grain (no work), and
    ``None`` when the coarse triangle cannot be built. The re-fit reuses
    ``fit._estimator`` -- the same method, regime, recent window, etc. --
    so the coarse band legs are exactly what the engine produces at grain
    ``g``. A ``loss_regime="auto"`` re-detects on the coarse triangle (the
    honest coarse regime); a manually pinned ``Regime.at(change=...)``
    carries through unchanged (a change date is grain-agnostic).
    """
    if g == fit._triangle.grain:
        return fit
    try:
        tri_g = _coarsen_triangle(fit._triangle, g)
    except (ValueError, pl.exceptions.PolarsError):
        return None
    # `Ratio.fit` only reads the estimator, but clone defensively so a
    # candidate re-fit can never perturb the caller's config.
    estimator = dataclasses.replace(fit._estimator)
    try:
        return estimator.fit(tri_g)
    except (ValueError, pl.exceptions.PolarsError):
        return None


def _grain_legs(
    fit: "RatioFit",
    g: str,
    keyvals: tuple,
    gcols: list[str],
    *,
    curve: Curve | None,
    tol: float,
) -> tuple[dict | None, "RatioFit | None"]:
    """One coarse re-fit -> the single-grain band row for one group.

    Re-fits the estimator at grain ``g`` and runs the byte-tested
    single-grain :func:`_segment_band` on it, returning the band row for
    this group (matched on the group key columns). Returns ``(None, refit)``
    when the group has no band row at grain ``g`` (no recent segment, or a
    partial boundary), and ``(None, None)`` when the coarse fit itself
    could not be built.
    """
    refit = _refit_at_grain(fit, g)
    if refit is None:
        return None, None
    band_g = _segment_band(refit, curve=curve, tol=tol)
    band_g = (
        band_g if isinstance(band_g, pl.DataFrame) else pl.from_pandas(band_g)
    )
    sub = band_g
    for c, v in zip(gcols, keyvals):
        if c in sub.columns:
            sub = sub.filter(pl.col(c) == v)
    if sub.is_empty():
        return None, refit
    return sub.row(0, named=True), refit


def _select_grain_row(
    fit: "RatioFit",
    keyvals: tuple,
    gcols: list[str],
    *,
    curve: Curve | None,
    tol: float,
) -> tuple[dict | None, str | None, str]:
    """Walk ``M -> Q -> H -> Y``; pick the finest grain that converges.

    Returns ``(row, selected_grain, status)`` where ``status`` is
    ``"selected"``, ``"insufficient"``, or ``"degenerate"``. On
    ``"selected"`` the row is the single-grain band row at the finest grain
    where BOTH signals fire. Otherwise the row is the finest grain that
    produced any band row (the borrow leg is the honest fallback), or
    ``None`` when no grain produced a row at all. The fallback status is
    ``"degenerate"`` when NO grain in the whole walk had a fittable curve
    (so the distinct default-path degenerate state is preserved), else
    ``"insufficient"`` (a curve existed at some grain but none converged).
    The degeneracy verdict is tracked across the entire walk, not read off
    the finest fallback row alone: a finest grain that degenerates while a
    coarser grain has a fittable-but-non-converging curve is
    ``"insufficient"``, not ``"degenerate"``.

    Signal (a) -- slope physicality -- reads the row's ``curve_diverged``
    (the single source of truth is :data:`_decay._DIVERGENCE_SLOPE`, not
    re-derived here). Signal (b) -- leg agreement -- compares the band
    width to the borrow level (the robust anchor) against
    :data:`_AUTO_GRAIN_REL`. A degenerate or under-determined curve at a
    grain is skipped (a narrow band is not earned there), which lets the
    point-count floor self-limit the walk: coarsening monotonically reduces
    the point count, so the coarsest grains fall out on their own.
    """
    fine = fit._triangle.grain
    try:
        start = _AUTO_GRAIN_ORDER.index(fine)
    except ValueError:
        start = 0
    fallback_row: dict | None = None
    saw_any_curve = False
    for g in _AUTO_GRAIN_ORDER[start:]:
        row, _ = _grain_legs(fit, g, keyvals, gcols, curve=curve, tol=tol)
        if row is None:
            continue  # not evaluable at this grain; coarsen further.
        if fallback_row is None:
            fallback_row = row  # finest grain that produced any row.
        if row["band_status"] != "degenerate":
            saw_any_curve = True  # a curve was fittable at some grain.

        # Degeneracy / data-sufficiency gate: a narrow band is not earned
        # without a fittable, over-determined curve leg here.
        if row["ratio_proj_curve"] is None:
            continue
        if bool(row["curve_under_determined"]):
            continue

        # Signal (a): the curve slope is physical (not past the boundary).
        slope_ok = not bool(row["curve_diverged"])

        # Signal (b): the two legs agree relative to the borrow level.
        bw = row["band_width"]
        rub = row["ratio_proj_borrow"]
        agree = (
            bw is not None
            and rub not in (None, 0.0)
            and bw / abs(rub) <= _AUTO_GRAIN_REL
        )

        if slope_ok and agree:  # the AND -- both signals must fire.
            return row, g, "selected"

    if fallback_row is None:
        return None, None, "insufficient"
    # Preserve the distinct degenerate state only when NO grain in the whole
    # walk had a fittable curve. A finest grain that degenerated while a
    # coarser grain produced a curve (that just never converged) is
    # insufficient, not degenerate.
    if not saw_any_curve:
        return fallback_row, None, "degenerate"
    return fallback_row, None, "insufficient"


def _segment_band_auto(
    fit: "RatioFit", *, curve: Curve | None, tol: float
) -> pl.DataFrame:
    """Auto-grain go-forward band (the ``auto_grain=True`` branch).

    For each regime group, the tail grain is selected by
    :func:`_select_grain_row`; the reported ultimate / band are the
    selected-grain values verbatim (an ultimate is grain-invariant -- no
    interpolation back to the display grain). When no grain converges the
    band is ``"insufficient"`` and the headline falls back to the borrow
    leg, so the loss-ratio number is never null. The OFF-path
    :func:`_segment_band` is reused per candidate grain, so every leg is
    exactly what the engine produces at that grain.
    """
    if curve is not None and curve.target != "intensity":
        raise ValueError(
            "segment_band requires a curve with target='intensity' (the "
            f"band formula is g_k-based); got target={curve.target!r}."
        )

    gcols = normalize_groups(fit._groups)
    empty = _empty_auto_band(gcols, fit._output_type)

    reg = fit._regime
    if reg is None or reg._changes_df.is_empty():
        return empty

    df = fit.to_polars()
    df = df if isinstance(df, pl.DataFrame) else pl.from_pandas(df)
    df = df.with_columns(pl.col("cohort").cast(pl.Date))
    changes = reg._changes_df

    parts = (
        list(df.partition_by(gcols, as_dict=True).items())
        if gcols
        else [((), df)]
    )

    rows: list[dict] = []
    for key, _gdf in sorted(parts, key=lambda kv: _sort_key(kv[0])):
        keyvals = key if isinstance(key, tuple) else (key,)

        chg = changes
        for c, v in zip(gcols, keyvals):
            if c in chg.columns:
                chg = chg.filter(pl.col(c) == v)
        if not sorted(chg["change"].to_list()):
            continue  # group without a regime change -> no row.

        row, selected_grain, status = _select_grain_row(
            fit, keyvals, gcols, curve=curve, tol=tol
        )
        if row is None:
            continue  # no recent segment at any grain -> no row (as today).

        out_row: dict[str, Any] = {c: v for c, v in zip(gcols, keyvals)}
        out_row.update(_auto_row_payload(row, selected_grain, status))
        rows.append(out_row)

    if not rows:
        return empty

    schema = {**{c: pl.Utf8 for c in gcols}, **_AUTO_BAND_SCHEMA}
    out = pl.DataFrame(rows, schema=_resolve_schema(schema, df, gcols))
    from ._io import mirror_output

    return mirror_output(out, fit._output_type)


def _auto_row_payload(
    row: dict, selected_grain: str | None, status: str
) -> dict[str, Any]:
    """Map a selected single-grain band row to the auto-band payload.

    On ``"selected"`` the row's band columns pass through verbatim (the
    ultimate is grain-invariant). On ``"insufficient"`` the curve leg and
    band columns are nulled and the headline mean falls back to the borrow
    leg, so the loss-ratio number is never null and a non-converged tail is
    never dressed up as a point.
    """
    payload = {k: row[k] for k in _BAND_SCHEMA}
    if status == "insufficient":
        payload["band_status"] = "insufficient"
        payload["loss_proj_curve"] = None
        payload["ratio_proj_curve"] = None
        payload["band_lo"] = None
        payload["band_hi"] = None
        payload["band_width"] = None
        payload["curve_alt_ratio_proj"] = None
        payload["loss_proj_mean"] = row["loss_proj_borrow"]
        payload["ratio_proj_mean"] = row["ratio_proj_borrow"]
    payload["selected_grain"] = selected_grain
    return payload


def _empty_auto_band(gcols: list[str], output_type: str) -> pl.DataFrame:
    """Zero-row, fully-typed auto-band frame (no-regime / no-row posture)."""
    from ._io import mirror_output

    schema = {**{c: pl.Utf8 for c in gcols}, **_AUTO_BAND_SCHEMA}
    return mirror_output(pl.DataFrame(schema=schema), output_type)


# ---------------------------------------------------------------------------
# Developing path (segment_path) -- the duration-by-duration companion to the band.
#
# segment_band reports the recent segment's ULTIMATE two ways (borrow / curve)
# and their spread. segment_path reports the same two legs AS A DEVELOPING
# PATH: the recent-segment aggregate cumulative loss ratio at each development
# period, observed part plus projected tail, so a chart can draw the trajectory
# (solid where observed, dashed in the tail) with the band shaded around it.
#
# The observed region is always at the fit's display (fine) grain -- it keeps
# the real period-to-period dynamics and is grain-invariant. With
# ``auto_grain=True`` the unobserved tail is extrapolated at the SELECTED
# coarse grain (the band's mature, convergent grain) and then INTERPOLATED back
# onto the fine display-grain duration positions. Interpolation introduces no new
# extrapolation error: the legs are only ever extrapolated at the selected
# grain, and the fine path linearly connects those computed marks. By
# construction the ultimate (last) row equals segment_band's reported ultimates
# (both reduce to ``sum loss_proj / sum premium_proj`` at the ultimate duration).
# ---------------------------------------------------------------------------

_PATH_SCHEMA: dict[str, pl.DataType] = {
    "duration":          pl.Int64,
    "ratio_borrow": pl.Float64,
    "ratio_curve":  pl.Float64,
    "ratio_mean":   pl.Float64,
    "band_lo":      pl.Float64,
    "band_hi":      pl.Float64,
    "observed":     pl.Boolean,
}

_AUTO_PATH_SCHEMA: dict[str, pl.DataType] = {
    **_PATH_SCHEMA,
    "selected_grain": pl.Utf8,
}


def _curve_leg_cum_by_duration(
    cr: "CurveResult", sub: pl.DataFrame, n_durations: int
) -> tuple[np.ndarray | None, bool]:
    """Curve-leg cumulative loss aggregated over recent cohorts, per duration.

    The per-development-period generalisation of :func:`_curve_leg_loss_proj`:
    instead of only the ultimate, it returns the recent-segment aggregate
    cumulative curve-leg loss at EVERY duration ``1 .. n_durations`` (array slot ``j`` =
    duration ``j+1``). Within a cohort's observed span the cumulative loss is the
    observed ``L_obs``; beyond it the observed last value plus the running
    premium-weighted sum of the fitted intensity ``cr.evaluate(k)``. Summed
    across cohorts at each duration. Returns ``(cum, any_term)`` -- ``cum`` is
    ``None`` when the curve is degenerate (no slope), ``any_term`` reports
    whether any tail term was added. The ultimate slot equals
    :func:`_curve_leg_loss_proj`'s scalar by construction.
    """
    if cr.slope is None:
        return None, False

    cohorts = sorted(sub["cohort"].unique().to_list())
    row_of = {c: i for i, c in enumerate(cohorts)}
    L = np.full((len(cohorts), n_durations), np.nan, dtype=float)
    Pp = np.full((len(cohorts), n_durations), np.nan, dtype=float)
    for c, d, lo, pp in zip(
        sub["cohort"].to_list(),
        sub["duration"].to_list(),
        sub["loss_obs"].to_list(),
        sub["premium_proj"].to_list(),
    ):
        j = int(d) - 1
        if 0 <= j < n_durations:
            i = row_of[c]
            L[i, j] = lo if lo is not None else np.nan
            Pp[i, j] = pp if pp is not None else np.nan

    cum = np.zeros(n_durations, dtype=float)
    any_term = False
    # Curve value at each 1-indexed duration, evaluated once (vectorized) instead
    # of per cohort x tail-duration; `ev_by_duration[k - 1]` mirrors `cr.evaluate(k)`.
    ev_by_duration = cr.evaluate(np.arange(1, n_durations + 1, dtype=float))
    for i in range(len(cohorts)):
        finite = np.flatnonzero(np.isfinite(L[i]))
        if finite.size == 0:
            continue
        last = int(finite[-1]) + 1  # 1-indexed last observed duration
        running = float(L[i, last - 1])
        # Observed span: the cumulative loss is the observed value. Carry the
        # last valid value across any interior gap (cumulative loss is
        # monotone) so a NaN cell never poisons the aggregate -- matching the
        # gap-robustness of the scalar `_curve_leg_loss_proj`. The engine's
        # triangles are gap-free, so this is hardening, not a live path.
        carry = 0.0
        for d in range(1, last + 1):
            val = L[i, d - 1]
            if np.isfinite(val):
                carry = float(val)
            cum[d - 1] += carry
        # Tail: accumulate premium-weighted fitted intensity, duration by duration.
        for d in range(last + 1, n_durations + 1):
            k = d - 1  # from-duration of the link landing at duration d
            ev = ev_by_duration[k - 1]
            pr = Pp[i, k - 1]
            if np.isfinite(ev) and np.isfinite(pr):
                running += float(ev) * float(pr)
                any_term = True
            cum[d - 1] += running
    if not any_term:
        return None, False
    return cum, True


def _segment_duration_table(
    fit: "RatioFit",
    keyvals: tuple,
    gcols: list[str],
    *,
    curve: Curve,
) -> dict | None:
    """Per-duration aggregate borrow / curve ratio marks for one group's recent seg.

    Returns ``None`` when the group has no recent segment. Otherwise a dict
    with the development-month position of each duration (``duration * step``), the
    borrow-leg and curve-leg aggregate cumulative loss ratios per duration, the
    observed boundary in months (``m_obs``), and the grain step. The borrow
    leg is ``sum(loss_proj) / sum(premium_proj)`` over the recent cohorts at
    each duration (full grid -- every cohort has a projected value at every duration);
    the curve leg divides :func:`_curve_leg_cum_by_duration` by the same premium.
    """
    reg = fit._regime
    if reg is None or reg._changes_df.is_empty():
        return None

    df = fit.to_polars()
    df = df if isinstance(df, pl.DataFrame) else pl.from_pandas(df)
    df = df.with_columns(pl.col("cohort").cast(pl.Date))
    for c, v in zip(gcols, keyvals):
        if c in df.columns:
            df = df.filter(pl.col(c) == v)

    chg = reg._changes_df
    for c, v in zip(gcols, keyvals):
        if c in chg.columns:
            chg = chg.filter(pl.col(c) == v)
    change_dates = sorted(chg["change"].to_list())
    if not change_dates:
        return None

    cohorts = sorted(df["cohort"].unique().to_list())
    recent_cohorts, _change_from = _recent_segment_cohorts(cohorts, change_dates)
    if not recent_cohorts:
        return None

    sub = df.filter(pl.col("cohort").is_in(recent_cohorts))
    n_durations = int(sub["duration"].max())
    step = _GRAIN_MONTHS[fit._triangle.grain]

    # Borrow leg + the shared premium denominator, per duration.
    agg = (
        sub.group_by("duration")
        .agg(
            pl.col("loss_proj").sum().alias("loss"),
            pl.col("premium_proj").sum().alias("premium"),
        )
        .sort("duration")
    )
    loss_dense = np.full(n_durations, np.nan, dtype=float)
    prem_dense = np.full(n_durations, np.nan, dtype=float)
    for d, lo, pr in zip(
        agg["duration"].to_list(), agg["loss"].to_list(), agg["premium"].to_list()
    ):
        loss_dense[int(d) - 1] = lo if lo is not None else np.nan
        prem_dense[int(d) - 1] = pr if pr is not None else np.nan
    with np.errstate(invalid="ignore", divide="ignore"):
        ratio_borrow = np.where(prem_dense > 0.0, loss_dense / prem_dense, np.nan)

    # Observed boundary (months): the latest duration with real observed loss.
    obs = sub.filter(pl.col("loss_obs").is_not_null())
    max_obs_duration = int(obs["duration"].max()) if not obs.is_empty() else 0
    m_obs = max_obs_duration * step

    # Curve leg, per duration (own intensity -> Curve -> premium-weighted cum).
    g_k = _own_intensity(sub, n_durations)
    cr = curve.fit(g_k)
    cum_curve, any_term = _curve_leg_cum_by_duration(cr, sub, n_durations)
    if cum_curve is None or not any_term:
        ratio_curve = None
    else:
        with np.errstate(invalid="ignore", divide="ignore"):
            ratio_curve = np.where(
                prem_dense > 0.0, cum_curve / prem_dense, np.nan
            )

    return {
        "grain":        fit._triangle.grain,
        "step":         step,
        "n_durations":       n_durations,
        "duration_month":    (np.arange(1, n_durations + 1) * step).astype(int),
        "ratio_borrow": ratio_borrow,
        "ratio_curve":  ratio_curve,
        "m_obs":        m_obs,
    }


def _interp_tail(
    knots_x: list[float], knots_y: list[float], months: np.ndarray
) -> np.ndarray:
    """Linear interpolation of a leg's coarse marks onto fine month positions.

    ``knots_x`` / ``knots_y`` are the coarse development-month marks (boundary
    anchor first, then the tail marks) and ``months`` the fine positions to
    fill. ``np.interp`` clamps outside the knot range, which never triggers
    here (the boundary anchor and the ultimate mark bracket the tail).
    """
    return np.interp(months, knots_x, knots_y)


def _build_path_rows(
    keyvals: tuple,
    gcols: list[str],
    fine: dict,
    coarse: dict,
    *,
    selected_grain: str | None,
    curve_available: bool,
) -> list[dict]:
    """Assemble the per-duration path rows for one group from fine + coarse tables.

    Rows are at the DISPLAY grain: ``duration`` is the display-grain duration index and
    its development-month position is ``duration * fine_step``. The observed region
    (month ``<= m_obs``) takes the fine borrow path verbatim (both legs
    coincide there). The tail interpolates each available leg's coarse marks
    (at their coarse-grain month positions) onto the display-duration month
    positions, anchored at the observed boundary for continuity. The horizon
    is the coarse (selected-grain) ultimate, which may run past the fine grid;
    tail rows beyond it come purely from interpolation, never from the fine
    borrow array. ``selected_grain`` is attached when not ``None`` (auto mode);
    ``curve_available`` gates the curve leg / band columns.
    """
    m_obs = fine["m_obs"]
    fine_borrow = fine["ratio_borrow"]
    fine_step = fine["step"]
    ult_month = int(coarse["duration_month"][-1])
    j_ult = ult_month // fine_step           # display durations out to the horizon.
    j_obs = m_obs // fine_step               # last observed display duration.

    # Observed-boundary anchor ratio (fine borrow path at the last obs duration).
    anchor = (
        float(fine_borrow[j_obs - 1]) if j_obs >= 1 else float("nan")
    )

    # Tail knots (development months): boundary anchor + coarse marks beyond it.
    cm = coarse["duration_month"]
    cb = coarse["ratio_borrow"]
    bx = [float(m_obs)] + [float(m) for m in cm if m > m_obs]
    by = [anchor] + [float(cb[i]) for i, m in enumerate(cm) if m > m_obs]

    if curve_available and coarse["ratio_curve"] is not None:
        cc = coarse["ratio_curve"]
        cy = [anchor] + [float(cc[i]) for i, m in enumerate(cm) if m > m_obs]
    else:
        cy = None

    rows: list[dict] = []
    for j in range(1, j_ult + 1):
        month = j * fine_step
        observed = month <= m_obs
        if observed:
            # Observed region: show the single data-anchored borrow trajectory
            # and collapse the band onto it. The leg divergence is a tail
            # property (where the segment as a whole has no more data); the
            # partial-projection of younger staggered cohorts inside this
            # region is not surfaced as width -- a display choice, not a claim
            # that the legs are mathematically equal here.
            rb = float(fine_borrow[j - 1])
            rc = rb if curve_available else None
            mean = rb
            lo = rb if curve_available else None
            hi = rb if curve_available else None
        else:
            rb = float(_interp_tail(bx, by, np.array([float(month)]))[0])
            if cy is not None:
                rc = float(_interp_tail(bx, cy, np.array([float(month)]))[0])
                mean = (rb + rc) / 2.0
                lo = min(rb, rc)
                hi = max(rb, rc)
            else:
                rc = None
                mean = rb
                lo = hi = None
        row: dict[str, Any] = {c: v for c, v in zip(gcols, keyvals)}
        row.update(
            duration=j,
            ratio_borrow=rb,
            ratio_curve=rc,
            ratio_mean=mean,
            band_lo=lo,
            band_hi=hi,
            observed=observed,
        )
        if selected_grain is not None:
            row["selected_grain"] = selected_grain
        rows.append(row)
    return rows


def _segment_path(
    fit: "RatioFit", *, curve: Curve | None, tol: float, auto_grain: bool
) -> pl.DataFrame:
    """Build the recent-segment developing-path frame (one block per group).

    See :meth:`RatioFit.segment_path` for the public contract.
    """
    if curve is None:
        curve = Curve(target="intensity", family="inverse_power", min_points=3)
    if curve.target != "intensity":
        raise ValueError(
            "segment_path requires a curve with target='intensity' (the "
            f"band formula is g_k-based); got target={curve.target!r}."
        )

    gcols = normalize_groups(fit._groups)
    auto = bool(auto_grain)
    empty = _empty_path(gcols, fit._output_type, auto=auto)

    reg = fit._regime
    if reg is None or reg._changes_df.is_empty():
        return empty

    df = fit.to_polars()
    df = df if isinstance(df, pl.DataFrame) else pl.from_pandas(df)
    df = df.with_columns(pl.col("cohort").cast(pl.Date))
    changes = reg._changes_df

    parts = (
        list(df.partition_by(gcols, as_dict=True).items())
        if gcols
        else [((), df)]
    )

    rows: list[dict] = []
    for key, _gdf in sorted(parts, key=lambda kv: _sort_key(kv[0])):
        keyvals = key if isinstance(key, tuple) else (key,)

        chg = changes
        for c, v in zip(gcols, keyvals):
            if c in chg.columns:
                chg = chg.filter(pl.col(c) == v)
        if not sorted(chg["change"].to_list()):
            continue  # group without a regime change -> no rows.

        fine = _segment_duration_table(fit, keyvals, gcols, curve=curve)
        if fine is None:
            continue  # no recent segment at the display grain -> no rows.

        selected_grain, coarse, curve_available = _resolve_path_tail(
            fit, keyvals, gcols, fine, curve=curve, tol=tol, auto=auto
        )
        rows.extend(
            _build_path_rows(
                keyvals,
                gcols,
                fine,
                coarse,
                selected_grain=selected_grain if auto else None,
                curve_available=curve_available,
            )
        )

    if not rows:
        return empty

    schema_body = _AUTO_PATH_SCHEMA if auto else _PATH_SCHEMA
    schema = {**{c: pl.Utf8 for c in gcols}, **schema_body}
    out = pl.DataFrame(rows, schema=_resolve_schema(schema, df, gcols))
    from ._io import mirror_output

    return mirror_output(out, fit._output_type)


def _resolve_path_tail(
    fit: "RatioFit",
    keyvals: tuple,
    gcols: list[str],
    fine: dict,
    *,
    curve: Curve,
    tol: float,
    auto: bool,
) -> tuple[str | None, dict, bool]:
    """Pick the tail grain and its duration table for one group.

    Non-auto: the tail stays at the display grain (``coarse = fine``), the
    curve leg is available when the fine curve fit produced one.
    Auto: walk ``M -> Q -> H -> Y`` via :func:`_select_grain_row`; on a
    ``"selected"`` verdict re-fit at that grain and take its duration table.
    On ``"insufficient"`` / ``"degenerate"`` fall back to the fine borrow
    leg only (curve unavailable -- a non-converged tail is never drawn as a
    confident curve). Returns ``(selected_grain, coarse_table, curve_available)``.
    """
    if not auto:
        return None, fine, fine["ratio_curve"] is not None

    _row, selected_grain, status = _select_grain_row(
        fit, keyvals, gcols, curve=curve, tol=tol
    )
    if status != "selected" or selected_grain is None:
        # No grain converged: borrow-only fallback at the display grain.
        return None, fine, False

    if selected_grain == fine["grain"]:
        return selected_grain, fine, fine["ratio_curve"] is not None

    refit = _refit_at_grain(fit, selected_grain)
    if refit is None:
        return None, fine, False
    coarse = _segment_duration_table(refit, keyvals, gcols, curve=curve)
    if coarse is None:
        return None, fine, False
    return selected_grain, coarse, coarse["ratio_curve"] is not None


def _empty_path(
    gcols: list[str], output_type: str, *, auto: bool
) -> pl.DataFrame:
    """Zero-row, fully-typed developing-path frame (no-regime posture)."""
    from ._io import mirror_output

    schema_body = _AUTO_PATH_SCHEMA if auto else _PATH_SCHEMA
    schema = {**{c: pl.Utf8 for c in gcols}, **schema_body}
    return mirror_output(pl.DataFrame(schema=schema), output_type)
