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

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import normalize_groups
from .curve import Curve

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
    "premium_ult":            pl.Float64,
    "loss_ult_borrow":        pl.Float64,
    "ratio_ult_borrow":       pl.Float64,
    "loss_ult_curve":         pl.Float64,
    "ratio_ult_curve":        pl.Float64,
    "band_lo":                pl.Float64,
    "band_hi":                pl.Float64,
    "band_width":             pl.Float64,
    "band_status":            pl.Utf8,
    "curve_n_points":         pl.Int64,
    "curve_under_determined": pl.Boolean,
    "curve_reason":           pl.Utf8,
    "curve_diverged":         pl.Boolean,
    "curve_alt_ratio_ult":    pl.Float64,
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


def _own_intensity(sub: pl.DataFrame, n_devs: int) -> np.ndarray:
    """Recent-segment OWN aggregated intensity ``g_k`` per link.

    Builds dense ``(n_cohorts, n_devs)`` cumulative ``loss_obs`` (``L``)
    and ``premium_obs`` (``P``) matrices (array slot ``j`` = dev ``j+1``),
    then aggregates the additive intensity over cohorts present at both
    ends of each link with a positive from-dev premium::

        g_k = sum(L[:, k+1] - L[:, k]) / sum(P[:, k])

    over the valid cohorts. ``g_k[j]`` is the intensity for the link
    starting at from-dev ``j+1`` (length ``n_devs - 1``). Uses the
    segment's own observed development -- no donor, so no level
    contamination.
    """
    cohorts = sorted(sub["cohort"].unique().to_list())
    row_of = {c: i for i, c in enumerate(cohorts)}
    L = np.full((len(cohorts), n_devs), np.nan, dtype=float)
    P = np.full((len(cohorts), n_devs), np.nan, dtype=float)
    for c, d, lo, po in zip(
        sub["cohort"].to_list(),
        sub["dev"].to_list(),
        sub["loss_obs"].to_list(),
        sub["premium_obs"].to_list(),
    ):
        j = int(d) - 1
        if 0 <= j < n_devs:
            i = row_of[c]
            L[i, j] = lo if lo is not None else np.nan
            P[i, j] = po if po is not None else np.nan

    dl = L[:, 1:] - L[:, :-1]          # col j = link from-dev (j+1) -> (j+2)
    pk = P[:, :-1]                      # premium at the FROM dev of each link
    valid = np.isfinite(dl) & np.isfinite(pk) & (pk > 0.0)
    num = np.where(valid, dl, 0.0).sum(axis=0)
    den = np.where(valid, pk, 0.0).sum(axis=0)
    with np.errstate(invalid="ignore", divide="ignore"):
        g_k = np.where(den > 0.0, num / den, np.nan)
    return g_k


def _curve_leg_loss_ult(
    cr: "CurveResult", sub: pl.DataFrame, n_devs: int
) -> tuple[float, bool]:
    """Curve-leg ultimate loss summed over the recent cohorts.

    Per cohort with last observed dev ``last`` (1-indexed), the ultimate
    loss is the observed cumulative loss at ``last`` plus the
    premium-weighted sum of the fitted intensity over the unobserved
    from-dev positions ``last .. n_devs-1``::

        loss_ult = L_obs[last] + sum_k cr.evaluate(k) * premium_proj[k]

    ``cr.evaluate(k)`` is the intensity at from-dev ``k``; the increment
    lands at dev ``k+1`` (the engine's ED from-column convention). Terms
    where ``evaluate`` is ``nan`` or ``premium_proj`` is null are skipped.
    Returns ``(loss_ult_curve, any_term)`` -- ``any_term`` is ``True``
    when at least one tail term was added (used to detect an all-empty
    degenerate sum).
    """
    cohorts = sorted(sub["cohort"].unique().to_list())
    row_of = {c: i for i, c in enumerate(cohorts)}
    L = np.full((len(cohorts), n_devs), np.nan, dtype=float)
    Pp = np.full((len(cohorts), n_devs), np.nan, dtype=float)
    for c, d, lo, pp in zip(
        sub["cohort"].to_list(),
        sub["dev"].to_list(),
        sub["loss_obs"].to_list(),
        sub["premium_proj"].to_list(),
    ):
        j = int(d) - 1
        if 0 <= j < n_devs:
            i = row_of[c]
            L[i, j] = lo if lo is not None else np.nan
            Pp[i, j] = pp if pp is not None else np.nan

    loss_ult = 0.0
    any_term = False
    for i in range(len(cohorts)):
        finite = np.flatnonzero(np.isfinite(L[i]))
        if finite.size == 0:
            continue
        last = int(finite[-1]) + 1  # 1-indexed last observed dev
        loss_ult += float(L[i, last - 1])
        ks = np.arange(last, n_devs)  # from-dev positions, 1-indexed
        if ks.size == 0:
            continue
        ev = cr.evaluate(ks.astype(float))
        pr = Pp[i, ks - 1]
        term_mask = np.isfinite(ev) & np.isfinite(pr)
        if term_mask.any():
            any_term = True
            loss_ult += float(np.sum(ev[term_mask] * pr[term_mask]))
    return loss_ult, any_term


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
    ratio_ult_borrow: float | None,
) -> str:
    """One-glance honesty flag -- ``"narrow"`` / ``"wide"`` / ``"degenerate"``.

    Driven by the agreement of the two principal legs (borrow vs curve)
    relative to the level -- that is the band this increment exists to
    expose. ``"degenerate"``: no curve fit. ``"wide"``: the curve is
    under-determined (too few points, so a small spread is not earned), OR
    the legs disagree by more than ``_NARROW_BAND_REL`` of the level.
    ``"narrow"``: the legs agree on a data-sufficient fit. The curve's own
    divergence and alt-law swing are reported separately
    (``curve_diverged`` / ``curve_alt_ratio_ult``) -- they qualify the
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
        and ratio_ult_borrow is not None
        and ratio_ult_borrow != 0.0
        and band_width / abs(ratio_ult_borrow) > _NARROW_BAND_REL
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
        curve = Curve(target="intensity", law="inverse_power", min_points=3)
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
        n_devs = int(gdf["dev"].max())

        # Borrow leg from segment_summary (recent segment row of this group).
        borrow = _borrow_row(seg, gcols, keyvals, change_from)
        if borrow is None:
            continue
        loss_ult_borrow = borrow["loss_ult"]
        ratio_ult_borrow = borrow["ratio_ult"]
        premium_ult = borrow["premium_ult"]
        n_cohorts = borrow["n_cohorts"]

        # Curve leg: own intensity -> Curve fit -> premium-weighted tail.
        g_k = _own_intensity(sub, n_devs)
        cr = curve.fit(g_k)
        loss_ult_curve, any_term = _curve_leg_loss_ult(cr, sub, n_devs)

        degenerate = cr.slope is None or not any_term
        if degenerate:
            ratio_ult_curve = None
            loss_ult_curve_out = None
            band_lo = band_hi = band_width = None
            curve_alt_ratio_ult = None
        else:
            loss_ult_curve_out = loss_ult_curve
            ratio_ult_curve = (
                loss_ult_curve / premium_ult if premium_ult else None
            )
            if ratio_ult_curve is None:
                band_lo = band_hi = band_width = None
            else:
                band_lo = min(ratio_ult_borrow, ratio_ult_curve)
                band_hi = max(ratio_ult_borrow, ratio_ult_curve)
                band_width = band_hi - band_lo
            curve_alt_ratio_ult = _alt_curve_ratio(
                cr, g_k, sub, n_devs, premium_ult
            )

        status = _band_status(
            degenerate=degenerate,
            under_determined=bool(cr.under_determined),
            band_width=band_width,
            ratio_ult_borrow=ratio_ult_borrow,
        )

        row: dict[str, Any] = {c: v for c, v in zip(gcols, keyvals)}
        row.update(
            segment=borrow["segment"],
            change_from=change_from,
            n_cohorts=int(n_cohorts),
            premium_ult=premium_ult,
            loss_ult_borrow=loss_ult_borrow,
            ratio_ult_borrow=ratio_ult_borrow,
            loss_ult_curve=loss_ult_curve_out,
            ratio_ult_curve=ratio_ult_curve,
            band_lo=band_lo,
            band_hi=band_hi,
            band_width=band_width,
            band_status=status,
            curve_n_points=int(cr.n_points),
            curve_under_determined=bool(cr.under_determined),
            curve_reason=cr.reason,
            curve_diverged=bool(cr.diverged),
            curve_alt_ratio_ult=curve_alt_ratio_ult,
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
        "loss_ult":    r["loss_ult"],
        "premium_ult": r["premium_ult"],
        "ratio_ult":   r["ratio_ult"],
    }


def _alt_curve_ratio(
    cr: "CurveResult",
    g_k: np.ndarray,
    sub: pl.DataFrame,
    n_devs: int,
    premium_ult: float,
) -> float | None:
    """Curve-leg ratio recomputed under the fit's alternate law.

    Surfaces the model-choice swing as a ratio: re-fit the alternate law
    (``cr.alt_law``) on the SAME ``g_k`` series via a fresh
    :class:`~lossratio.Curve`, run the same premium-weighted tail sum, and
    divide by the shared denominator. This is the genuine alternate-law
    extrapolation (intercept and slope both from the alt fit, not a slope
    swap on the primary intercept). Returns ``None`` when the alt fit is
    degenerate or no tail term is produced.
    """
    if cr.alt_law is None or not premium_ult:
        return None
    alt_cr = Curve(target=cr.target, law=cr.alt_law).fit(g_k)
    if alt_cr.slope is None:
        return None
    loss_ult, any_term = _curve_leg_loss_ult(alt_cr, sub, n_devs)
    if not any_term:
        return None
    return loss_ult / premium_ult
