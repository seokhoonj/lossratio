"""Prospective inception-stability diagnostic (credibility from count x rate).

At cohort inception -- before any elapsed duration -- a planner asks "can this
cohort's own experience be trusted yet, or must it pool?". This is the
classical credibility question, answerable from the cohort's policy COUNT and
a priced incidence RATE alone:

    lambda    = sum(count * rate) * horizon            expected claims
    lambda_F  = full_credibility * (1 + cv_sev**2)     severity-inflated standard
    Z         = min(1, sqrt(lambda / lambda_F))        partial credibility
    cv        = sqrt((1 + cv_sev**2) / lambda)         expected loss-ratio CV

``Z`` is IDEALISED sampling credibility (the rate is assumed, not estimated);
realised instability is larger (dispersion / lumpiness dominate -- see the
``dev/`` validation harnesses). Treat ``Z`` as a planning figure / lower bound
on uncertainty, not a realised-stability guarantee.

This is a standalone, engine-independent reader: the caller supplies an
already-segmented count frame and a matching rate frame (the coverage->cause/
rider crosswalk and rate-table loading live with the caller, not here). It is
a charter-extension diagnostic -- public-surface wiring follows the redesign
diagnostics phase.
"""

from __future__ import annotations

from collections.abc import Sequence
from typing import TYPE_CHECKING, Any

import polars as pl

from .._kernels.io import detect_input_type, mirror_output, normalize_groups, to_polars

if TYPE_CHECKING:
    from .._kernels.io import FrameLike

# 1082 = (z / k)**2 for 90% within +/-5% (z=1.645, k=0.05) -- the classical
# full-credibility standard for frequency.
FULL_CREDIBILITY_90_5 = (1.645 / 0.05) ** 2


def inception_credibility(
    counts: Any,
    rates: Any,
    *,
    on: "str | Sequence[str]",
    by: "str | Sequence[str]",
    count: str = "count",
    rate: str = "rate",
    severity: Any = None,
    cv_sev: str = "cv_sev",
    full_credibility: float = FULL_CREDIBILITY_90_5,
    horizon: float = 1.0,
    usable: float = 0.5,
    thin: float = 0.3,
) -> "FrameLike":
    """Credibility ``Z`` per ``by`` group from segmented counts and rates.

    Parameters
    ----------
    counts
        Frame at the ``by`` x ``on`` grain with a ``count`` column (policy
        count per cell). polars or pandas; the output mirrors this type.
    rates
        Frame keyed by ``on`` with a ``rate`` column (annual incidence).
        Joined to ``counts`` on ``on``; cells with no matching rate are
        counted in ``n_unrated`` and contribute 0 to ``lambda``.
    on
        Join key(s) shared by ``counts`` and ``rates`` (e.g. ``["sex",
        "age"]`` plus a cause / rider column already present on ``counts``).
    by
        Output grouping (e.g. ``["cohort", "coverage"]`` or ``"coverage"``).
    count, rate
        Column names for the count (in ``counts``) and rate (in ``rates``).
    severity
        Optional frame keyed by ``by`` with a per-group severity coefficient
        of variation (``cv_sev`` column) that inflates the full-credibility
        standard. ``None`` -> no inflation (``cv_sev = 0``).
    cv_sev
        Column name of the severity CV in ``severity``.
    full_credibility
        Expected claims for full credibility (default 1082: 90% / +/-5%).
    horizon
        Exposure horizon in the rate's time unit (default 1 = one year).
    usable, thin
        ``Z`` thresholds for the ``status`` label (``usable`` / ``marginal``
        / ``thin``).

    Returns
    -------
    Frame (mirroring ``counts``' type) with one row per ``by`` group:
    ``[*by, n_policy, n_unrated, lambda, cv_sev, Z, cv, status]``.
    """
    if not (0.0 < thin <= usable < 1.0):
        raise ValueError(
            f"need 0 < thin <= usable < 1, got thin={thin}, usable={usable}"
        )
    if full_credibility <= 0:
        raise ValueError(f"full_credibility must be > 0, got {full_credibility}")
    if horizon <= 0:
        raise ValueError(f"horizon must be > 0, got {horizon}")

    out_type = detect_input_type(counts)
    cdf = to_polars(counts)
    rdf = to_polars(rates)
    on_cols = normalize_groups(on)
    by_cols = normalize_groups(by)
    if not on_cols:
        raise ValueError("`on` must name at least one join column")
    if not by_cols:
        raise ValueError("`by` must name at least one grouping column")
    for nm, df, need in (("counts", cdf, [*by_cols, *on_cols, count]),
                         ("rates", rdf, [*on_cols, rate])):
        missing = [c for c in need if c not in df.columns]
        if missing:
            raise ValueError(f"{nm} is missing column(s): {missing}")

    rsel = rdf.select([*on_cols, rate])
    if rsel.select(on_cols).is_duplicated().any():
        raise ValueError(
            f"rates has duplicate keys over {on_cols}; each key must be unique "
            f"(a left join on duplicates would fan out counts and inflate lam/Z/cv)"
        )
    joined = cdf.join(rsel, on=on_cols, how="left")
    joined = joined.with_columns(
        _exp=pl.col(count) * pl.col(rate) * horizon
    )
    agg = joined.group_by(by_cols).agg(
        n_policy=pl.col(count).sum(),
        n_unrated=pl.col(count).filter(pl.col(rate).is_null()).sum(),
        lam=pl.col("_exp").sum(),
    )

    if severity is not None:
        sdf = to_polars(severity)
        miss = [c for c in [*by_cols, cv_sev] if c not in sdf.columns]
        if miss:
            raise ValueError(f"severity is missing column(s): {miss}")
        agg = agg.join(sdf.select([*by_cols, cv_sev]), on=by_cols, how="left")
        agg = agg.with_columns(pl.col(cv_sev).fill_null(0.0).alias("cv_sev"))
    else:
        agg = agg.with_columns(pl.lit(0.0).alias("cv_sev"))

    lam_f = full_credibility * (1.0 + pl.col("cv_sev") ** 2)
    out = (
        agg.with_columns(n_unrated=pl.col("n_unrated").fill_null(0))
        .with_columns(
            Z=pl.min_horizontal(pl.lit(1.0), (pl.col("lam") / lam_f).sqrt()),
            cv=((1.0 + pl.col("cv_sev") ** 2) / pl.col("lam")).sqrt(),
        )
        .with_columns(
            status=pl.when(pl.col("Z") >= usable).then(pl.lit("usable"))
            .when(pl.col("Z") >= thin).then(pl.lit("marginal"))
            .otherwise(pl.lit("thin"))
        )
        .select(*by_cols, "n_policy", "n_unrated", "lam", "cv_sev",
                "Z", "cv", "status")
        .sort(by_cols)
    )
    return mirror_output(out, out_type)
