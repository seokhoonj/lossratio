"""Rolling-origin generalisation of the calendar-diagonal hold-out backtest.

Where :class:`~lossratio.backtest.Backtest` evaluates a single as-of origin
(one ``holdout`` depth), :class:`RollingBacktest` evaluates several at once.
Each hold-out depth ``H`` fixes an as-of date ``T - H`` (mask the most recent
``H`` calendar diagonals, refit, score the held-out cells). Every held-out cell
is then annotated with two coordinates:

* ``horizon`` -- how many calendar periods past the as-of date the cell sits
  (how far forward it is projected), and
* ``anchor_duration`` -- the duration the cohort had already been observed to
  at that as-of date (how much history the projection was anchored on);
  ``anchor_duration = duration - horizon``.

Pooling the per-cell errors and aggregating by ``horizon`` yields the package's
reliability curve: how projection error grows the further past the as-of date a
cell is projected. Aggregating by ``anchor_duration`` shows how error depends on
how much history a cohort had when it was projected -- a brand-new cohort with a
single observation is projected far less reliably than a mature one.

The class composes :class:`~lossratio.backtest.Backtest` per hold-out depth; it
does not reimplement the masking / refit. Horizon is derived from the sequential
calendar index ``cal_idx`` (grain-agnostic: months for an ``M`` grain,
quarters for ``Q``, ...), never from a hardcoded period unit.

Reading the horizon curve -- the population shifts
---------------------------------------------------
A given physical ``(cohort, duration)`` cell can appear under MULTIPLE hold-out
depths: a cell on the most recent diagonal is held out by every depth deep
enough to reach it, scored once per depth at a different horizon each time. So
the pooled per-cell frame deliberately recurs a cell across depths (that is
exactly what a horizon curve needs -- the same cell observed at horizon 1 under
one depth and at a deeper horizon under another). It also means any statistic
that pools over ALL of ``ae_err`` without grouping by ``holdout`` -- including
``holdout_summary``, which groups by hold-out depth -- double-counts physical
cells across depths; the per-cell frame carries ``holdout`` so a caller can
de-duplicate if a single-count read is wanted.

A given ``horizon`` is also NOT the same set of ``(cohort, duration)`` cells
across depths. Because ``horizon = cal_idx - (max_cal - holdout)``, a fixed
horizon maps a *lower* ``cal_idx`` (an earlier diagonal) under a deeper depth,
so the deeper a depth runs the more its low-horizon cells sit at *higher*
duration / older cohorts. The pooled per-horizon population therefore drifts
toward larger durations as horizon grows. On the *cumulative* lane
(``abs_err_mean`` / ``ae_err_*``) this confounds two effects -- error genuinely
growing with projection distance, and error growing simply because deeper
horizons are dominated by higher-duration cells whose cumulative magnitude is
larger.

To read a horizon curve free of that confound, prefer the **incremental** lane
(``incr_abs_err_mean`` / ``incr_ae_err_*``): a per-period A/E is not inflated by
the cumulative-magnitude ramp, so it isolates how a single period's projection
degrades with horizon. Both lanes are emitted in ``horizon_summary`` /
``anchor_summary`` / ``holdout_summary`` whenever the refit exposes an
incremental projection (the same condition under which
:class:`~lossratio.backtest.BacktestFit` emits its own ``incr_*`` block).
"""

from __future__ import annotations

import math
from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import mirror_output, normalize_groups
from .backtest import Backtest, _VALID_TARGETS, _add_cal_idx

if TYPE_CHECKING:
    from .triangle import Triangle


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class RollingBacktest:
    """Rolling-origin hold-out backtest of a fit estimator.

    Runs :class:`~lossratio.backtest.Backtest` at each depth in ``holdouts``
    (each depth ``H`` is an as-of date ``T - H``), annotates every held-out
    cell with its ``horizon`` (calendar periods projected past that as-of date)
    and ``anchor_duration`` (the duration the cohort was observed to at that
    date), and pools the per-cell A/E errors. The headline output is
    ``horizon_summary``: the reliability curve of error against horizon.

    Parameters
    ----------
    estimator
        An ``lr.ChainLadder`` / ``lr.ExposureDriven`` / ``lr.StageAdaptive``
        / ``lr.Ratio`` instance (or any estimator whose ``fit(triangle)``
        returns a result class with a projection column in ``.df``). The same
        estimator / target compatibility rules as
        :class:`~lossratio.backtest.Backtest` apply (a ratio-fit estimator
        only supports ``target="ratio"``); they are enforced by the inner
        ``Backtest`` constructed per hold-out depth.
    holdouts
        The hold-out depths to evaluate, one as-of origin each (the plural of
        :class:`~lossratio.backtest.Backtest`'s ``holdout``). De-duplicated and
        sorted ascending. Each must be a positive integer in units of the
        triangle's grain (months for ``M``, quarters for ``Q``, ...). A depth
        that meets or exceeds the triangle's calendar span yields no reachable
        held-out cells; it is skipped (recorded in ``skipped_holdouts``) rather
        than raising. A single-element ``holdouts`` is just one ``Backtest``.
    target
        Which projection to score: ``"ratio"`` (default), ``"loss"``, or
        ``"premium"``. Same semantics as
        :class:`~lossratio.backtest.Backtest`.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> rbt = lr.RollingBacktest(
    ...     estimator=lr.Ratio(method="sa"), holdouts=(6, 12, 18, 24)
    ... ).fit(tri)
    >>> rbt.horizon_summary   # error vs horizon (the reliability curve)
    >>> rbt.anchor_summary    # error vs how much history the cohort had
    >>> rbt.holdout_summary   # error vs hold-out depth
    >>> rbt.ae_err            # combined per-cell frame (holdout + horizon)
    """

    def __init__(
        self,
        estimator: Any,
        holdouts: "tuple[int, ...] | list[int]" = (6, 12, 18, 24),
        target: str = "ratio",
    ) -> None:
        if not hasattr(estimator, "fit"):
            raise TypeError(
                f"estimator must implement .fit(triangle); got "
                f"{type(estimator).__name__}"
            )
        if target not in _VALID_TARGETS:
            raise ValueError(
                f"target must be one of {_VALID_TARGETS}, got {target!r}"
            )

        norm = self._normalize_holdouts(holdouts)

        # Defer the estimator / target / bootstrap-leakage compatibility
        # checks to a probe Backtest built on the first depth. Constructing it
        # raises exactly the same errors a single Backtest would (e.g. a
        # ratio-fit estimator with target != "ratio", or a pre-built
        # BootstrapTriangle on the estimator), so RollingBacktest fails fast at
        # construction with an identical message -- no duplicated logic.
        Backtest(estimator=estimator, holdout=norm[0], target=target)

        self.estimator = estimator
        self.holdouts = norm
        self.target = target

    @staticmethod
    def _normalize_holdouts(
        holdouts: "tuple[int, ...] | list[int]",
    ) -> tuple[int, ...]:
        """Validate, de-duplicate, and sort the hold-out depths.

        Each depth must be a positive integer (``bool`` is rejected even
        though it is an ``int`` subclass, since ``True``/``False`` are never a
        meaningful hold-out depth). Returns a sorted ascending tuple with
        duplicates removed.
        """
        items = list(holdouts)
        if not items:
            raise ValueError("holdouts must contain at least one hold-out depth")
        clean: set[int] = set()
        for h in items:
            if isinstance(h, bool) or not isinstance(h, int):
                raise TypeError(
                    f"each hold-out depth must be a positive int; got {h!r} "
                    f"({type(h).__name__})"
                )
            if h < 1:
                raise ValueError(f"each hold-out depth must be >= 1, got {h}")
            clean.add(h)
        return tuple(sorted(clean))

    def fit(self, triangle: "Triangle") -> "RollingBacktestFit":
        return RollingBacktestFit._from_triangle(triangle, self)


class RollingBacktestFit:
    """Result of a rolling-origin hold-out backtest.

    Properties
    ----------
    ae_err : DataFrame
        Combined per-cell hold-out comparison across all hold-out depths
        ``[groups?, holdout, horizon, anchor_duration, cohort, duration,
        actual, expected, aeg, ae_err, incr_actual, incr_expected, incr_aeg,
        incr_ae_err]``. ``holdout`` is the depth the cell was scored under;
        ``horizon`` (>= 1) is the number of calendar periods the cell is
        projected past that depth's as-of date; ``anchor_duration``
        (= ``duration - horizon``) is the duration the cohort was observed to
        at that as-of date. A physical ``(cohort, duration)`` cell recurs once
        per hold-out depth that holds it out (scored at a different horizon
        each time) -- so pooling over all rows without grouping by ``holdout``
        double-counts cells; ``holdout`` is carried so a caller can
        de-duplicate. The ``incr_*`` columns are carried through when the refit
        emits an incremental projection (the same condition as
        :class:`~lossratio.backtest.BacktestFit`); the internal ``cal_idx`` is
        dropped (``horizon`` supersedes it).
    horizon_summary : DataFrame
        The reliability curve -- error aggregated by ``horizon`` (within
        group): ``[groups?, horizon, n, abs_err_mean, ae_err_mean, ae_err_med,
        ae_err_wt]`` plus the incremental block ``[incr_abs_err_mean,
        incr_ae_err_mean, incr_ae_err_med, incr_ae_err_wt]`` when available.
        ``abs_err_mean = mean(|actual - expected|)`` is the cumulative
        target-unit absolute error; ``ae_err_*`` are the relative ``actual /
        expected - 1`` statistics; ``ae_err_wt = sum(actual - expected) /
        sum(expected)`` is the exposure-weighted pooled A/E - 1 (as in
        :class:`~lossratio.backtest.BacktestFit`). The ``incr_*`` companions
        are the per-period counterparts. Because the per-horizon cell
        population drifts toward higher durations as horizon grows (see the
        module docstring), prefer the **incremental** lane to read the curve
        free of the cumulative-magnitude confound; for ``target="ratio"`` the
        relative ``ae_err_*`` / ``incr_ae_err_*`` are more interpretable than
        the unit-bearing ``abs_err_mean`` (a ratio gap, not currency).
    anchor_summary : DataFrame
        Error aggregated by ``anchor_duration`` (within group), same
        statistics as ``horizon_summary``. Shows how projection error depends
        on how much history a cohort had when it was projected: a cohort with a
        single observation (``anchor_duration = 1``) is projected far less
        reliably than a mature one. Complements the horizon curve -- error is
        driven as much by how little history anchored the projection as by how
        far ahead it reaches.
    holdout_summary : DataFrame
        Error aggregated by hold-out depth ``holdout`` (within group), same
        statistics as ``horizon_summary``.
    skipped_holdouts : list[int]
        Hold-out depths that produced no reachable held-out cells (e.g. a
        depth at or beyond the triangle's calendar span) and were dropped.
    fits : dict[int, BacktestFit]
        The inner per-depth ``BacktestFit`` for each depth that produced
        cells, keyed by hold-out depth -- for drill-down / inspection.
    """

    # The incremental companion columns the inner Backtest may carry through.
    _INCR_CELL_COLS = ("incr_actual", "incr_expected", "incr_aeg", "incr_ae_err")

    def __init__(self) -> None:
        raise TypeError(
            "RollingBacktestFit is the result of "
            "`RollingBacktest(...).fit(triangle)`, not a direct constructor."
        )

    @classmethod
    def _from_triangle(
        cls, triangle: "Triangle", rbt: "RollingBacktest"
    ) -> "RollingBacktestFit":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self.estimator = rbt.estimator
        self.target = rbt.target
        self.holdouts = rbt.holdouts

        gcols = normalize_groups(triangle._groups)

        # Per-group max calendar index over the FULL (unmasked) triangle.
        # A depth H masks cells with cal_idx > max_cal - H, so a held-out
        # cell's horizon is `cal_idx - (max_cal - H)`, ranging 1..H. Computing
        # max_cal from the full triangle (not from a fold's surviving cells)
        # keeps the horizon anchored to the true as-of date even when the
        # oldest cohort does not reach the latest diagonal.
        full = _add_cal_idx(triangle._df, triangle._groups)
        max_cal_scalar = 0
        max_cal: pl.DataFrame | None = None
        if gcols:
            max_cal = full.group_by(gcols).agg(
                pl.col("cal_idx").max().alias("_max_cal")
            )
        else:
            max_cal_scalar = int(full["cal_idx"].max())

        per_holdout: list[pl.DataFrame] = []
        fits: dict[int, Any] = {}
        skipped: list[int] = []

        for h in rbt.holdouts:
            bt_fit = cls._run_holdout(rbt, triangle, h)
            if bt_fit is None:
                skipped.append(h)
                continue

            ae = bt_fit._ae_err  # internal polars frame: keeps cal_idx
            if ae.height == 0:
                skipped.append(h)
                continue

            # horizon = cal_idx - (max_cal - h); 1..h on held cells.
            if gcols:
                assert max_cal is not None
                ae = ae.join(max_cal, on=gcols, how="left")
                ae = ae.with_columns(
                    (pl.col("cal_idx") - (pl.col("_max_cal") - h)).alias(
                        "horizon"
                    )
                ).drop("_max_cal")
            else:
                ae = ae.with_columns(
                    (pl.col("cal_idx") - (max_cal_scalar - h)).alias("horizon")
                )

            ae = ae.with_columns(
                pl.lit(h, dtype=pl.Int64).alias("holdout"),
                # how much history the cohort had at this as-of date.
                (pl.col("duration") - pl.col("horizon")).alias(
                    "anchor_duration"
                ),
            )
            # `cal_idx` has done its job (horizon is derived) -- drop it so the
            # public per-cell frame is keyed by the rolling annotations
            # (holdout, horizon, anchor_duration) not the internal index.
            per_holdout.append(ae.drop("cal_idx"))
            fits[h] = bt_fit

        self._skipped = skipped
        self._fits = fits

        # The incremental lane is available only if every surviving fold
        # carried it (a heterogeneous mix would null-fill -- avoid that).
        has_incr = bool(per_holdout) and all(
            all(c in df.columns for c in cls._INCR_CELL_COLS)
            for df in per_holdout
        )

        if per_holdout:
            # vertical concat is intentional: a single estimator refit across
            # folds emits a stable column set, so a column mismatch here is a
            # real bug we want surfaced, not silently null-unioned.
            combined = pl.concat(per_holdout, how="vertical")
            lead = [*gcols, "holdout", "horizon", "anchor_duration"]
            rest = [c for c in combined.columns if c not in lead]
            combined = combined.select(lead + rest).sort(
                [*gcols, "holdout", "cohort", "duration"]
            )
        else:
            # No depth produced cells -- emit an empty, well-typed frame so
            # downstream code (and input-mirroring) still works.
            combined = cls._empty_ae_err(gcols, triangle)

        self._ae_err = combined
        self._has_incr = has_incr

        self._horizon_summary = self._aggregate(
            combined, [*gcols, "horizon"], has_incr
        )
        self._anchor_summary = self._aggregate(
            combined, [*gcols, "anchor_duration"], has_incr
        )
        self._holdout_summary = self._aggregate(
            combined, [*gcols, "holdout"], has_incr
        )

        return self

    @staticmethod
    def _run_holdout(
        rbt: "RollingBacktest", triangle: "Triangle", holdout: int
    ) -> Any | None:
        """Run one depth's inner Backtest, returning ``None`` if unrunnable.

        A depth that meets or exceeds the calendar span produces a 0-height
        ``ae_err`` rather than raising (the caller treats a 0-height frame as a
        skip), so the common skip path needs no exception handling. We narrowly
        guard only :class:`ValueError` (the package's own "no anchor" /
        degenerate-fold signal) so a genuinely unrunnable fold is skipped
        without masking unrelated failures (an estimator bug, a polars schema
        error, an OOM, a keyboard interrupt) -- those propagate.

        Caveat: a misconfigured-estimator error that itself surfaces as a
        fit-time ``ValueError`` would be absorbed here as a silent skip. The
        ``__init__`` probe ``Backtest`` only *constructs* on ``holdouts[0]`` (it
        does not ``.fit``), so a config error that raises only at fit time is
        not caught up front. In practice an over-deep depth yields a 0-height
        frame rather than raising, so ``ValueError`` is rarely the actual skip
        trigger; the narrow catch is the deliberate trade.
        """
        try:
            return Backtest(
                estimator=rbt.estimator,
                holdout=holdout,
                target=rbt.target,
            ).fit(triangle)
        except ValueError:
            return None

    # -- aggregation ---------------------------------------------------------

    @classmethod
    def _aggregate(
        cls, ae_err: pl.DataFrame, by_cols: list[str], has_incr: bool
    ) -> pl.DataFrame:
        """Aggregate the combined per-cell frame to a per-key summary.

        Always emits the cumulative block ``(n, abs_err_mean, ae_err_mean,
        ae_err_med, ae_err_wt)``; emits the incremental companion block
        (``incr_abs_err_mean`` / ``incr_ae_err_*``) when ``has_incr``. The
        cumulative ``abs_err_mean = mean(|actual - expected|)`` is target-unit
        absolute error (currency for ``"loss"`` / ``"premium"``, a ratio gap
        for ``"ratio"``); the relative ``ae_err_*`` are dimensionless. Because
        the per-horizon cell population drifts toward higher durations as
        horizon grows, the incremental lane is the confound-free reading of the
        reliability curve.
        """
        agg_exprs = cls._agg_exprs(has_incr)
        if ae_err.height == 0:
            schema = {c: ae_err.schema[c] for c in by_cols}
            schema["n"] = pl.UInt32
            schema.update(
                {name: pl.Float64 for name in cls._summary_stat_names(has_incr)}
            )
            return pl.DataFrame(schema=schema)
        return ae_err.group_by(by_cols).agg(agg_exprs).sort(by_cols)

    @staticmethod
    def _summary_stat_names(has_incr: bool) -> list[str]:
        """Ordered names of the float summary columns (single source of truth
        for both the populated and the empty-frame schema)."""
        names = ["abs_err_mean", "ae_err_mean", "ae_err_med", "ae_err_wt"]
        if has_incr:
            names += [
                "incr_abs_err_mean",
                "incr_ae_err_mean",
                "incr_ae_err_med",
                "incr_ae_err_wt",
            ]
        return names

    @staticmethod
    def _agg_exprs(has_incr: bool) -> list[pl.Expr]:
        """The aggregation expressions matching :meth:`_summary_stat_names`."""
        exprs: list[pl.Expr] = [
            pl.len().alias("n"),
            (pl.col("actual") - pl.col("expected")).abs().mean().alias(
                "abs_err_mean"
            ),
            pl.col("ae_err").mean().alias("ae_err_mean"),
            pl.col("ae_err").median().alias("ae_err_med"),
            (
                (pl.col("actual") - pl.col("expected")).sum()
                / pl.col("expected").sum()
            ).alias("ae_err_wt"),
        ]
        if has_incr:
            exprs += [
                (pl.col("incr_actual") - pl.col("incr_expected"))
                .abs()
                .mean()
                .alias("incr_abs_err_mean"),
                pl.col("incr_ae_err").mean().alias("incr_ae_err_mean"),
                pl.col("incr_ae_err").median().alias("incr_ae_err_med"),
                (
                    (pl.col("incr_actual") - pl.col("incr_expected")).sum()
                    / pl.col("incr_expected").sum()
                ).alias("incr_ae_err_wt"),
            ]
        return exprs

    @staticmethod
    def _empty_ae_err(gcols: list[str], triangle: "Triangle") -> pl.DataFrame:
        """A 0-row combined frame with the expected schema.

        ``cohort`` / ``duration`` dtypes are read from the source triangle
        (cohort may be a ``Date`` underwriting period or an integer
        underwriting year -- never hardcoded), so an all-skipped result labels
        its empty frame consistently with a populated one.
        """
        tri_schema = triangle._df.schema
        cohort_dt = tri_schema.get("cohort", pl.Date)
        duration_dt = tri_schema.get("duration", pl.Int64)
        schema: dict[str, Any] = {c: tri_schema.get(c, pl.Utf8) for c in gcols}
        schema.update(
            {
                "holdout": pl.Int64,
                "horizon": pl.Int64,
                "anchor_duration": pl.Int64,
                "cohort": cohort_dt,
                "duration": duration_dt,
                "actual": pl.Float64,
                "expected": pl.Float64,
                "aeg": pl.Float64,
                "ae_err": pl.Float64,
            }
        )
        return pl.DataFrame(schema=schema)

    # -- accessors -----------------------------------------------------------

    @property
    def ae_err(self):
        return mirror_output(self._ae_err, self._output_type)

    @property
    def horizon_summary(self):
        return mirror_output(self._horizon_summary, self._output_type)

    @property
    def anchor_summary(self):
        return mirror_output(self._anchor_summary, self._output_type)

    @property
    def holdout_summary(self):
        return mirror_output(self._holdout_summary, self._output_type)

    @property
    def skipped_holdouts(self) -> list[int]:
        return list(self._skipped)

    @property
    def fits(self) -> dict[int, Any]:
        return dict(self._fits)

    # -- evidence readers ----------------------------------------------------

    def _resolve_bias_col(self, tol: float, lane: str) -> str:
        """Validate the shared ``tol`` / ``lane`` arguments of the evidence
        readers and return the pooled signed-bias column to walk."""
        if (
            isinstance(tol, bool)
            or not isinstance(tol, (int, float))
            or not math.isfinite(tol)
            or tol <= 0
        ):
            raise ValueError(
                f"tol must be a positive finite number, got {tol!r}"
            )
        if lane not in ("cumulative", "incremental"):
            raise ValueError(
                f'lane must be "cumulative" or "incremental", got {lane!r}'
            )
        if lane == "incremental" and not self._has_incr:
            raise ValueError(
                'lane="incremental" is unavailable for this fit: not every '
                "surviving hold-out depth carried an incremental projection, "
                "so the summaries have no incr_* lane (see the module "
                "docstring)"
            )
        return "ae_err_wt" if lane == "cumulative" else "incr_ae_err_wt"

    @staticmethod
    def _threshold_walk(
        summ: pl.DataFrame,
        gcols: list[str],
        axis: str,
        bias: str,
        tol: float,
        mode: str,
        value_col: str,
        max_col: str,
        min_run: int = 1,
    ) -> pl.DataFrame:
        """Per-group tolerance walk over a summary axis.

        ``mode="suffix"`` finds the smallest axis value from which EVERY
        observed entry at or beyond it keeps ``|bias| <= tol`` (null when the
        walk never starts -- including a violation at the very last entry),
        and additionally requires the in-band suffix to hold at least
        ``min_run`` observed entries (null otherwise; ``min_run`` is ignored
        in prefix mode). ``mode="prefix"`` finds the largest axis value
        reached while every entry from the front stays within tolerance (0
        when the first entry already violates). A null bias counts as a
        violation in both modes. The summaries are tiny, so a clear per-group
        Python walk is preferred over a window-expression formulation.
        """
        axis_dt = summ.schema[axis]
        schema: dict[str, Any] = {c: summ.schema[c] for c in gcols}
        schema[value_col] = axis_dt
        schema[max_col] = axis_dt
        if summ.height == 0:
            return pl.DataFrame(schema=schema)
        parts = (
            summ.partition_by(gcols, maintain_order=True) if gcols else [summ]
        )
        rows: list[dict[str, Any]] = []
        for part in parts:
            part = part.sort(axis)
            values = part[axis].to_list()
            within = [
                b is not None and abs(b) <= tol for b in part[bias].to_list()
            ]
            result: Any
            if mode == "suffix":
                result = None
                run = 0
                for v, ok in zip(reversed(values), reversed(within)):
                    if not ok:
                        break
                    result = v
                    run += 1
                if run < min_run:
                    result = None
            else:  # prefix
                result = 0
                for v, ok in zip(values, within):
                    if not ok:
                        break
                    result = v
            row: dict[str, Any] = {c: part[c][0] for c in gcols}
            row[value_col] = result
            row[max_col] = values[-1]
            rows.append(row)
        out = pl.DataFrame(rows, schema=schema)
        return out.sort(gcols) if gcols else out

    def convergence(
        self, tol: float = 0.03, lane: str = "cumulative", min_run: int = 6
    ):
        """Smallest anchor duration from which the pooled bias stays in band.

        Reads the ANCHOR axis of the rolling backtest: how much observed
        history a cohort needs before its out-of-sample projections settle.
        Per group, the ``anchor_summary`` pooled signed bias (``ae_err_wt``
        for ``lane="cumulative"``, ``incr_ae_err_wt`` for
        ``lane="incremental"``) is walked over ``anchor_duration``;
        ``converged_at`` is the smallest observed anchor duration such that
        EVERY observed anchor duration at or beyond it keeps
        ``|bias| <= tol`` (a null bias counts as a violation) AND the
        in-band suffix holds at least ``min_run`` observed anchor durations.
        When no such anchor exists -- including when the largest observed
        anchor still violates -- ``converged_at`` is null. That null is
        honest, not a failure: the bias never settles within the observed
        range, and ``max_anchor`` (the largest observed anchor duration) is
        reported so the null can be judged against how far that range
        actually reaches.

        Why a SIGNED pooled bias rather than absolute error: on a lumpy
        low-frequency book the per-period absolute error never shrinks (it
        is irreducible noise), so an absolute-error criterion would never
        trigger. Systematic over/under-projection, by contrast, averages out
        across the many pooled out-of-sample cells, so the exposure-weighted
        signed bias is the statistic that can actually settle. And why the
        tolerance defaults TIGHT: the cumulative lane's denominator keeps
        growing with duration, so an apparent flattening of the cumulative
        loss ratio is an inertia illusion -- eyeballing that curve is not
        evidence; the out-of-sample bias is. ``lane`` selects which bias to
        walk: the cumulative lane is the smoother headline read, while the
        incremental lane strips the cumulative-magnitude confound (see the
        module docstring's lane discussion).

        Why ``min_run`` guards the walk: the suffix-all test alone fails
        OPEN at the data edge. Two mechanisms make the deepest observed
        anchors unreliable witnesses on the cumulative lane. First, few
        rolling-origin cells reach a deep anchor, so the pooled bias there
        is computed from a handful of cells -- noise, not evidence. Second,
        a cohort observed to a deep anchor carries a large accumulated
        denominator, so projecting a few periods ahead moves its cumulative
        ratio mechanically little: denominator inertia damps the relative
        bias toward zero regardless of model quality (the same inertia that
        makes the cumulative loss-ratio curve LOOK settled). A thin, damped
        tail of a few in-band anchors can therefore fire the unguarded read
        even when every anchor before it violates badly. Raising a
        minimum-cell filter does not fix this: on a book whose bias never
        settles, the spurious point simply chases the truncation edge as
        the filter rises, whereas a genuine convergence point is
        edge-stable under any such filter. ``min_run`` instead demands a
        sustained in-band run, so a thin damped tail cannot fire the call
        on its own. A young triangle whose whole observed anchor range is
        shorter than ``min_run`` honestly reads null -- not enough evidence
        yet -- and ``anchor_summary``'s ``n`` column is the drill-down for
        judging how much pooled evidence each anchor carries.

        This is an anchor-axis question (how much history until the
        projection settles), not a horizon-axis question (how far ahead is
        trustworthy) -- the two must not be conflated; see
        :meth:`reliable_horizon` for the horizon axis.

        Parameters
        ----------
        tol
            Tolerance band on the pooled signed bias, ``|bias| <= tol``.
            Must be a positive finite number. Default ``0.03``.
        lane
            ``"cumulative"`` (default) or ``"incremental"``. The incremental
            lane is available only when every surviving hold-out depth
            carried an incremental projection.
        min_run
            Minimum number of observed anchor durations the in-band suffix
            must hold before ``converged_at`` fires. Must be an int >= 1;
            ``1`` reproduces the unguarded suffix-all read. Default ``6``.

        Returns
        -------
        DataFrame
            Input-mirrored, one row per group, sorted by the group columns:
            ``[groups?, converged_at, max_anchor]``. ``converged_at`` keeps
            the ``anchor_duration`` dtype and is null when the bias never
            settles within the observed range (or settles over fewer than
            ``min_run`` observed anchors); ``max_anchor`` is non-null.
        """
        if (
            isinstance(min_run, bool)
            or not isinstance(min_run, int)
            or min_run < 1
        ):
            raise ValueError(
                f"min_run must be an int >= 1, got {min_run!r}"
            )
        bias = self._resolve_bias_col(tol, lane)
        gcols = normalize_groups(self._groups)
        out = self._threshold_walk(
            self._anchor_summary,
            gcols,
            axis="anchor_duration",
            bias=bias,
            tol=tol,
            mode="suffix",
            value_col="converged_at",
            max_col="max_anchor",
            min_run=min_run,
        )
        return mirror_output(out, self._output_type)

    def reliable_horizon(self, tol: float = 0.03, lane: str = "cumulative"):
        """Largest horizon the projections stay in band for, from the front.

        Reads the HORIZON axis of the rolling backtest: how far past the
        as-of date a projection can be pushed before the pooled signed bias
        leaves the tolerance band. Per group, the ``horizon_summary`` bias
        (``ae_err_wt`` for ``lane="cumulative"``, ``incr_ae_err_wt`` for
        ``lane="incremental"``) is walked over ``horizon`` from the smallest
        observed horizon upward; ``reliable_horizon`` is the largest observed
        horizon ``H`` such that EVERY observed horizon up to and including
        ``H`` keeps ``|bias| <= tol`` -- a contiguous-from-the-front
        definition that stops at the first violation (a null bias counts as
        a violation). Reliability must be contiguous: a horizon that drifts
        back into band beyond an out-of-band stretch is not trustworthy,
        since reaching it means trusting the stretch that was not. When the
        very first observed horizon already violates, ``reliable_horizon``
        is ``0`` -- an honest "not reliable even one step ahead". (The ``0``
        here versus :meth:`convergence`'s null is semantic, not cosmetic:
        this method reports the largest contiguous reach, which can
        legitimately be none; ``convergence`` reports the smallest point
        FROM which the bias settles, which may simply not exist within the
        observed range.)

        The signed-pooled-bias rationale of :meth:`convergence` applies
        unchanged -- on a lumpy book the per-period absolute error never
        shrinks, so the bias band, not the absolute error, is what carries
        the evidence. ``lane`` selects which bias to walk: the cumulative
        lane is the smoother headline read, while the incremental lane
        strips the cumulative-magnitude confound (the per-horizon population
        drifts toward higher durations as horizon grows -- see the module
        docstring's lane discussion). Unlike :meth:`convergence` there is no
        ``min_run`` guard here: the prefix walk starts where the pooled data
        is densest (the shallowest horizons) and stops at the first
        violation, so it fails CLOSED at the thin data edge rather than open.

        This is a horizon-axis question (how far ahead is trustworthy), not
        an anchor-axis question (how much history until the projection
        settles) -- the two must not be conflated; see :meth:`convergence`
        for the anchor axis.

        Parameters
        ----------
        tol
            Tolerance band on the pooled signed bias, ``|bias| <= tol``.
            Must be a positive finite number. Default ``0.03``.
        lane
            ``"cumulative"`` (default) or ``"incremental"``. The incremental
            lane is available only when every surviving hold-out depth
            carried an incremental projection.

        Returns
        -------
        DataFrame
            Input-mirrored, one row per group, sorted by the group columns:
            ``[groups?, reliable_horizon, max_horizon]``. ``reliable_horizon``
            keeps the ``horizon`` dtype and is ``0`` (non-null) when the
            first observed horizon already violates; ``max_horizon`` is the
            largest observed horizon.
        """
        bias = self._resolve_bias_col(tol, lane)
        gcols = normalize_groups(self._groups)
        out = self._threshold_walk(
            self._horizon_summary,
            gcols,
            axis="horizon",
            bias=bias,
            tol=tol,
            mode="prefix",
            value_col="reliable_horizon",
            max_col="max_horizon",
        )
        return mirror_output(out, self._output_type)

    def plot(
        self,
        by: str = "horizon",
        metric: str = "ae_err",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Reliability-curve line plot, backed by matplotlib.

        ``by`` selects the axis: ``"horizon"`` (default; error vs how far
        ahead -- the headline reliability curve), ``"anchor"`` (error vs how
        much history the cohort had at the as-of date), or ``"holdout"`` (error
        vs hold-out depth). ``metric`` is ``"ae_err"`` (default; relative
        ``actual / expected - 1``) or ``"abs_err"`` (``mean |actual -
        expected|``). The cumulative and per-period ``incr_*`` lanes are drawn
        together; for ``by="horizon"`` the incremental lane is the
        confound-free read (see the class docstring). Per-as-of-depth calendar
        heatmaps are available via
        ``self.fits[holdout].plot_triangle(x="calendar")``.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from ._rolling_backtest_vis import plot_rolling_backtest
        return plot_rolling_backtest(
            self, by=by, metric=metric, nrow=nrow, ncol=ncol, figsize=figsize,
        )

    def __repr__(self) -> str:
        est_name = type(self.estimator).__name__
        used = sorted(self._fits)
        n_cells = self._ae_err.height
        skipped = f", skipped={self._skipped}" if self._skipped else ""
        return (
            f"<RollingBacktestFit: estimator={est_name}, "
            f"holdouts={used}, target={self.target!r}, "
            f"n_cells={n_cells}{skipped}>"
        )
