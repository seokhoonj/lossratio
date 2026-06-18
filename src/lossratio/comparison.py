"""Out-of-sample estimator comparison on a shared rolling-origin backtest.

:class:`EstimatorComparison` runs one
:class:`~lossratio.backtest.Backtest` per labelled estimator
on the SAME triangle and the same hold-out depths, then compares the
estimators on the MATCHED cell population only -- the ``(group, holdout,
cohort, duration)`` keys that EVERY estimator scored. The matching is what
makes the read honest: different estimators reach different held-out cells
(a refit that needs an observed anchor drops cohorts a more forgiving model
keeps -- on real data one refit reached only about half the cells another
did), and pooled error over different populations is not an apples-to-apples
comparison. ``match_summary`` records how many cells each estimator lost to
the matching, so the population restriction is never silent.

Three reads come out of the matched cells:

* the **matched summaries** (``horizon_summary`` / ``anchor_summary`` /
  ``holdout_summary``) -- each estimator's reliability curve recomputed on
  the matched population (deliberately NOT the per-fit summaries, whose
  populations may differ across estimators);
* the **comparison frames** (``horizon_comparison`` / ``anchor_comparison``
  / ``holdout_comparison``) -- one row per challenger with the baseline's
  statistics riding along as ``base_*`` and relative-improvement columns
  (negative = challenger better), plus a per-cell ``win_rate``;
* the **crossover read** (:meth:`EstimatorComparisonFit.crossover`) -- the
  axis value where the better method flips, if a sustained flip exists at
  all. "No crossover + an overall winner" is the deprecation-evidence read:
  one method dominates over the whole observed range and the other adds
  nothing out of sample.
"""

from __future__ import annotations

import math
import warnings
from collections.abc import Mapping
from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import mirror_output, normalize_groups
from .backtest import Backtest, BacktestFit

if TYPE_CHECKING:
    from .triangle import Triangle


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class EstimatorComparison:
    """Out-of-sample comparison of several labelled estimators.

    Runs a :class:`~lossratio.backtest.Backtest` per
    labelled estimator on the same triangle / hold-out depths / target, and
    assembles the per-cell A/E errors into matched-population comparison
    frames (see the module docstring). The headline outputs are the
    baseline-relative ``horizon_comparison`` and the
    :meth:`EstimatorComparisonFit.crossover` read.

    Parameters
    ----------
    estimators
        A ``Mapping`` of label -> estimator, e.g. ``{"pooled":
        lr.PooledLoss(), "credible": lr.CredibleLoss()}``. Labels are
        required (two estimator instances can differ only by configuration,
        so no name can be derived automatically); insertion order is
        canonical -- it fixes the estimator order in every output frame and
        plot. At least two entries are required; for a single estimator use
        :class:`~lossratio.backtest.Backtest` directly. Each
        value must satisfy the same estimator / target compatibility rules
        as :class:`~lossratio.backtest.Backtest` (enforced per label by the
        inner ``Backtest``).
    holdouts
        The shared hold-out depths, forwarded to every inner
        ``Backtest`` (de-duplicated and sorted ascending there).
        Sharing the depths is the by-construction comparability guarantee:
        every estimator is scored on the same as-of origins.
    target
        Which projection to score: ``"ratio"`` (default), ``"loss"``, or
        ``"premium"``. Same semantics as
        :class:`~lossratio.backtest.Backtest`.
    baseline
        The label every other estimator (a "challenger") is compared
        against in the comparison frames and the crossover read. Defaults
        to the first key of ``estimators``.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> cmp = lr.EstimatorComparison(
    ...     {"pooled": lr.Ratio(loss=lr.PooledLoss()), "credible": lr.Ratio(loss=lr.CredibleLoss())},
    ...     holdouts=(6, 12, 18, 24),
    ... ).fit(tri)
    >>> cmp.horizon_comparison   # challenger vs baseline, by horizon
    >>> cmp.crossover()          # where (if anywhere) the better method flips
    >>> cmp.plot()               # matched reliability curves, one line each
    """

    def __init__(
        self,
        estimators: "Mapping[str, Any]",
        holdouts: "tuple[int, ...] | list[int]" = (6, 12, 18, 24),
        target: str = "ratio",
        baseline: str | None = None,
    ) -> None:
        if not isinstance(estimators, Mapping):
            raise TypeError(
                "estimators must be a Mapping of label -> estimator, e.g. "
                '{"pooled": lr.Ratio(loss=lr.PooledLoss()), "credible": lr.Ratio(loss=lr.CredibleLoss())}'
                f"; got {type(estimators).__name__}. Labels are required "
                "because two estimator instances can differ only by "
                "configuration (two lr.Ratio instances with different "
                "loss/premium estimators), so no name can be derived "
                "automatically."
            )
        if len(estimators) == 0:
            raise ValueError(
                "estimators must contain at least two labelled estimators; "
                "got 0"
            )
        if len(estimators) == 1:
            raise ValueError(
                "estimators must contain at least two labelled estimators; "
                "for a single estimator use lr.Backtest directly"
            )
        for label in estimators:
            if not isinstance(label, str):
                raise TypeError(
                    f"estimator labels must be str; got {label!r} "
                    f"({type(label).__name__})"
                )
            if not label.strip():
                raise ValueError(
                    f"estimator labels must be non-empty; got {label!r}"
                )

        # One inner Backtest per label: this REUSES its validation
        # (holdout normalization, target enum, .fit presence, and the probe
        # Backtest's estimator / target / bootstrap-leakage compatibility
        # checks) with identical error messages -- no duplicated logic.
        rbts = {
            label: Backtest(
                estimator=est, holdouts=holdouts, target=target
            )
            for label, est in estimators.items()
        }

        labels = list(estimators)
        if baseline is None:
            baseline = labels[0]
        elif baseline not in estimators:
            raise ValueError(
                f"baseline must be one of the estimator labels {labels!r}, "
                f"got {baseline!r}"
            )

        self.estimators = dict(estimators)
        self.holdouts = next(iter(rbts.values())).holdouts
        self.target = target
        self.baseline = baseline
        self._rbts = rbts

    def fit(self, triangle: "Triangle") -> "EstimatorComparisonFit":
        # Every inner Backtest runs on the SAME triangle object --
        # the by-construction comparability guarantee.
        fits = {
            label: rbt.fit(triangle) for label, rbt in self._rbts.items()
        }
        return EstimatorComparisonFit._from_fits(fits, baseline=self.baseline)


class EstimatorComparisonFit:
    """Result of an out-of-sample estimator comparison.

    Properties
    ----------
    cells : DataFrame
        Matched per-cell frame, LONG layout -- one row per (matched key x
        estimator): ``[groups?, estimator, holdout, horizon,
        anchor_duration, cohort, duration, actual, expected, aeg, ae_err]``
        plus the ``incr_*`` companions when every estimator carried the
        incremental lane. A matched key is a ``(group, holdout, cohort,
        duration)`` combination scored by EVERY estimator; ``horizon`` and
        ``anchor_duration`` are key-derived and method-independent, and
        ``actual`` repeats identically per estimator on a matched key (the
        duplication doubles as a matching witness).
    horizon_summary, anchor_summary, holdout_summary : DataFrame
        Each estimator's reliability curve recomputed on the MATCHED
        population (not the per-fit summaries): ``[groups?, estimator,
        <axis>, n, abs_err_mean, ae_err_mean, ae_err_med, ae_err_wt]`` plus
        the ``incr_*`` block when available -- the same statistics as
        :class:`~lossratio.backtest.BacktestFit`.
    horizon_comparison, anchor_comparison, holdout_comparison : DataFrame
        Baseline-relative comparison, one row per (group x challenger x
        axis value); the baseline never appears as a row -- its statistics
        ride along as ``base_*``. ``abs_err_mean = mean|actual -
        expected|``; ``ae_err_abs_mean = mean|ae_err|`` (the ABSOLUTE
        companion of the summaries' signed ``ae_err_mean`` -- ranking needs
        a magnitude); ``ae_err_wt = sum(actual - expected) / sum(expected)``
        (signed pooled bias, calibration diagnostic). The relative columns
        (negative = challenger better): ``abs_err_rel = abs_err_mean /
        base_abs_err_mean - 1``, ``ae_err_rel`` the ``ae_err_abs_mean``
        analogue, ``bias_rel = |ae_err_wt| / |base_ae_err_wt| - 1``; a
        zero / null denominator yields a null (never inf). ``win_rate`` is
        the share of matched cells the challenger beats the baseline on
        (strict win 1, exact tie 0.5, loss 0; cells whose absolute error
        is null or non-finite on either side are excluded from the
        denominator). Exact ties earn 0.5 credit, and ties can be plentiful
        when estimators coincide on many cells (e.g. a per-cohort scale
        falling back to 1 reproduces the baseline exactly), so ``win_rate``
        should be read alongside the strict-win share implied by ``n``.
        ``n`` is the matched cell count at that axis value (identical
        across estimators by construction). The ``incr_*`` block mirrors
        every column when the incremental lane is available.
    match_summary : DataFrame
        Transparency record of the population loss: ``[groups?, estimator,
        holdout, n_cells, n_matched, n_dropped]`` over ALL of each
        estimator's surviving depths -- ``n_cells`` is the estimator's own
        per-depth cell count, ``n_matched`` the cells surviving the matched
        join (0 at depths excluded by the depth intersection), ``n_dropped``
        the difference. A group (e.g. one coverage) whose cells one
        estimator never reaches drops out of the matched comparison frames
        entirely; ``match_summary`` is where that loss is visible (the
        depth-asymmetry warning covers depths, not groups).
    holdouts : tuple[int, ...]
        The COMMON surviving depths -- the sorted intersection of every
        estimator's surviving hold-out depths. Depths lost to asymmetry are
        warned about once at fit time.
    skipped_holdouts : dict[str, list[int]]
        Each estimator's own skipped depths, keyed by label.
    fits : dict[str, BacktestFit]
        The per-estimator inner fit, keyed by label -- the drill-down for
        the unmatched, per-estimator evidence.
    """

    # The incremental companion columns the inner fits may carry through.
    _INCR_CELL_COLS = BacktestFit._INCR_CELL_COLS

    # crossover(): metric name -> (challenger column, baseline column) on
    # the comparison frames; "bias" compares the absolute values.
    _METRIC_PAIRS = {
        "abs_err": ("abs_err_mean",    "base_abs_err_mean"),
        "ae_err":  ("ae_err_abs_mean", "base_ae_err_abs_mean"),
        "bias":    ("ae_err_wt",       "base_ae_err_wt"),
    }

    def __init__(self) -> None:
        raise TypeError(
            "EstimatorComparisonFit is the result of "
            "`EstimatorComparison(...).fit(triangle)`, not a direct "
            "constructor."
        )

    @classmethod
    def _from_fits(
        cls, fits: "dict[str, BacktestFit]", baseline: str
    ) -> "EstimatorComparisonFit":
        self = cls.__new__(cls)
        labels = list(fits)
        first = fits[labels[0]]
        self._output_type = first._output_type
        self._groups = first._groups
        self._labels = labels
        self._fits = dict(fits)
        self.baseline = baseline
        self.target = first.target

        gcols = normalize_groups(self._groups)

        # COMMON hold-out depths: the sorted intersection of every
        # estimator's surviving depths. A depth that survived for some
        # estimators but not all is dropped from the comparison -- pooled
        # error over different fold sets is not apples-to-apples -- and
        # warned about once (the per-label evidence stays in .fits[label]).
        surviving = {label: set(f._fits) for label, f in fits.items()}
        common = sorted(set.intersection(*surviving.values()))
        self._common_holdouts = tuple(common)

        lost = sorted(set().union(*surviving.values()) - set(common))
        if lost:
            warnings.warn(
                f"hold-out depths {lost} survived for some estimators but "
                f"not all and were dropped from the comparison (pooled "
                f"error over different fold sets is not an apples-to-apples "
                f"comparison); the per-estimator evidence remains reachable "
                f"via .fits[label]."
            )

        # MATCHED cells: restrict each estimator's per-cell frame to the
        # common depths, then keep only the keys present for ALL estimators
        # (iterative inner join of the distinct key frames, then a semi-join
        # of each estimator's frame onto the final key set).
        key_cols = [*gcols, "holdout", "cohort", "duration"]
        matched: dict[str, pl.DataFrame] = {}
        keys: pl.DataFrame | None = None
        for label in labels:
            frame = fits[label]._ae_err.filter(
                pl.col("holdout").is_in(list(common))
            )
            matched[label] = frame
            k = frame.select(key_cols).unique()
            keys = k if keys is None else keys.join(
                k, on=key_cols, how="inner"
            )
        assert keys is not None
        for label in labels:
            matched[label] = matched[label].join(
                keys, on=key_cols, how="semi"
            )
        self._n_matched = keys.height

        # The incremental lane is all-or-nothing: available only if EVERY
        # estimator's matched frame carries all four incr cell columns (a
        # heterogeneous mix would null-fill -- same rationale as
        # BacktestFit).
        has_incr = all(
            all(c in matched[label].columns for c in cls._INCR_CELL_COLS)
            for label in labels
        )
        self._has_incr = has_incr

        value_cols = ["actual", "expected", "aeg", "ae_err"]
        if has_incr:
            value_cols += list(cls._INCR_CELL_COLS)
        lead = [
            *gcols, "holdout", "horizon", "anchor_duration",
            "cohort", "duration",
        ]
        parts: list[pl.DataFrame] = []
        for i, label in enumerate(labels):
            parts.append(
                matched[label]
                .select([*lead, *value_cols])
                .with_columns(
                    pl.lit(label, dtype=pl.Utf8).alias("estimator"),
                    pl.lit(i, dtype=pl.Int64).alias("_est_order"),
                )
            )
        cells = pl.concat(parts, how="vertical")
        cells = (
            cells.sort([*gcols, "holdout", "cohort", "duration", "_est_order"])
            .drop("_est_order")
            .select([*gcols, "estimator", *lead[len(gcols):], *value_cols])
        )
        self._cells = cells

        self._horizon_summary = self._aggregate_matched("horizon")
        self._anchor_summary = self._aggregate_matched("anchor_duration")
        self._holdout_summary = self._aggregate_matched("holdout")

        win_cells = self._build_win_cells()
        self._horizon_comparison = self._compare("horizon", win_cells)
        self._anchor_comparison = self._compare("anchor_duration", win_cells)
        self._holdout_comparison = self._compare("holdout", win_cells)

        self._match_summary = self._build_match_summary()

        return self

    # -- assembly helpers -----------------------------------------------------

    def _sort_with_estimator(
        self, df: pl.DataFrame, pre: list[str], post: list[str]
    ) -> pl.DataFrame:
        """Sort ``df`` by ``[*pre, estimator-in-insertion-order, *post]``.

        ``estimators``' insertion order is canonical (it fixes the method
        order in frames and plots), so the sort goes through a temporary
        order-index column rather than the alphabetical label.
        """
        order = pl.DataFrame(
            {
                "estimator":  self._labels,
                "_est_order": list(range(len(self._labels))),
            },
            schema={"estimator": pl.Utf8, "_est_order": pl.Int64},
        )
        return (
            df.join(order, on="estimator", how="left")
            .sort([*pre, "_est_order", *post])
            .drop("_est_order")
        )

    def _aggregate_matched(self, axis: str) -> pl.DataFrame:
        """Per-estimator summary over the MATCHED cells along one axis.

        Reuses :meth:`BacktestFit._agg_exprs` /
        :meth:`BacktestFit._summary_stat_names` verbatim (single
        source of truth for the summary statistics), grouped by ``[groups?,
        estimator, axis]`` -- so each estimator's curve is computed on the
        same matched population, unlike the per-fit summaries.
        """
        gcols = normalize_groups(self._groups)
        by = [*gcols, "estimator", axis]
        cells = self._cells
        if cells.height == 0:
            schema: dict[str, Any] = {c: cells.schema[c] for c in by}
            schema["n"] = pl.UInt32
            schema.update(
                {
                    name: pl.Float64
                    for name in BacktestFit._summary_stat_names(
                        self._has_incr
                    )
                }
            )
            return pl.DataFrame(schema=schema)
        out = cells.group_by(by).agg(
            BacktestFit._agg_exprs(self._has_incr)
        )
        return self._sort_with_estimator(out, gcols, [axis])

    @staticmethod
    def _comparison_stat_names(has_incr: bool) -> list[str]:
        """Ordered names of the float comparison columns (single source of
        truth for both the populated and the empty-frame schema)."""
        cum = [
            "abs_err_mean",    "base_abs_err_mean",    "abs_err_rel",
            "ae_err_abs_mean", "base_ae_err_abs_mean", "ae_err_rel",
            "ae_err_wt",       "base_ae_err_wt",       "bias_rel",
            "win_rate",
        ]
        if not has_incr:
            return cum
        return cum + ["incr_" + name for name in cum]

    @staticmethod
    def _comparison_agg_exprs(has_incr: bool) -> list[pl.Expr]:
        """Per-(estimator, axis value) statistics over the matched cells.

        ``abs_err_mean = mean|actual - expected|`` (cell accuracy);
        ``ae_err_abs_mean = mean|ae_err|`` (relative cell accuracy -- the
        ABSOLUTE companion of the summaries' signed ``ae_err_mean``, which
        averages opposite signs away and cannot rank); ``ae_err_wt`` is the
        signed pooled bias. The pooled division is null-guarded (a zero /
        null denominator yields null, never inf).
        """
        def lane(pre: str) -> list[pl.Expr]:
            gap = pl.col(f"{pre}actual") - pl.col(f"{pre}expected")
            den = pl.col(f"{pre}expected").sum()
            return [
                gap.abs().mean().alias(f"{pre}abs_err_mean"),
                pl.col(f"{pre}ae_err")
                .abs()
                .mean()
                .alias(f"{pre}ae_err_abs_mean"),
                pl.when(den.is_not_null() & den.is_finite() & (den != 0))
                .then(gap.sum() / den)
                .otherwise(None)
                .alias(f"{pre}ae_err_wt"),
            ]

        exprs = [pl.len().alias("n"), *lane("")]
        if has_incr:
            exprs += lane("incr_")
        return exprs

    @staticmethod
    def _base_name(name: str) -> str:
        """``X -> base_X``; ``incr_X -> incr_base_X`` (the ``incr_`` prefix
        stays outermost so the incremental block is a uniform prefix scan)."""
        if name.startswith("incr_"):
            return "incr_base_" + name[len("incr_"):]
        return "base_" + name

    @staticmethod
    def _rel_expr(
        num: str, den: str, alias: str, absolute: bool = False
    ) -> pl.Expr:
        """Relative-improvement expression ``num / den - 1`` (negative =
        challenger better), null-safe: a null / non-finite / zero
        denominator -- or a null / non-finite numerator -- yields null,
        never inf. ``absolute=True`` compares magnitudes (the ``bias_rel``
        case, where the underlying statistic is signed)."""
        n = pl.col(num).abs() if absolute else pl.col(num)
        d = pl.col(den).abs() if absolute else pl.col(den)
        return (
            pl.when(
                n.is_not_null() & n.is_finite()
                & d.is_not_null() & d.is_finite() & (d != 0)
            )
            .then(n / d - 1)
            .otherwise(None)
            .alias(alias)
        )

    def _build_win_cells(self) -> pl.DataFrame:
        """Per-cell challenger-vs-baseline win credit on the matched cells.

        One row per (challenger, matched key), carrying the axis columns
        and ``_win`` (1 the challenger's ``|actual - expected|`` is strictly
        smaller, 0.5 exact tie, 0 loss; null -- excluded from the mean --
        when either side's ``expected`` is null or its absolute error is
        null / non-finite, so a NaN / inf cell earns neither tie credit nor
        a loss). The ``_incr_win`` companion mirrors it on the incremental
        lane.
        """
        gcols = normalize_groups(self._groups)
        key = [*gcols, "holdout", "cohort", "duration"]
        cells = self._cells

        def side(df: pl.DataFrame, tag: str, carry: list[str]) -> pl.DataFrame:
            sel: list[Any] = [
                *key,
                *carry,
                (pl.col("actual") - pl.col("expected"))
                .abs()
                .alias(f"_{tag}_abs"),
                pl.col("expected").alias(f"_{tag}_expected"),
            ]
            if self._has_incr:
                sel += [
                    (pl.col("incr_actual") - pl.col("incr_expected"))
                    .abs()
                    .alias(f"_{tag}_incr_abs"),
                    pl.col("incr_expected").alias(f"_{tag}_incr_expected"),
                ]
            return df.select(sel)

        base = side(
            cells.filter(pl.col("estimator") == self.baseline), "base", []
        )
        ch = side(
            cells.filter(pl.col("estimator") != self.baseline),
            "ch",
            ["estimator", "horizon", "anchor_duration"],
        )
        wc = ch.join(base, on=key, how="inner")

        def win(pre: str) -> pl.Expr:
            ch_abs = pl.col(f"_ch_{pre}abs")
            base_abs = pl.col(f"_base_{pre}abs")
            valid = (
                pl.col(f"_ch_{pre}expected").is_not_null()
                & pl.col(f"_base_{pre}expected").is_not_null()
                & ch_abs.is_not_null()
                & ch_abs.is_finite()
                & base_abs.is_not_null()
                & base_abs.is_finite()
            )
            return (
                pl.when(~valid)
                .then(None)
                .when(ch_abs < base_abs)
                .then(1.0)
                .when(ch_abs == base_abs)
                .then(0.5)
                .otherwise(0.0)
            )

        exprs = [win("").alias("_win")]
        if self._has_incr:
            exprs.append(win("incr_").alias("_incr_win"))
        return wc.with_columns(exprs)

    def _compare(self, axis: str, win_cells: pl.DataFrame) -> pl.DataFrame:
        """Baseline-relative comparison frame along one axis.

        One row per (group x challenger x axis value); the baseline's
        statistics are joined alongside as ``base_*`` and the relative /
        win-rate columns are derived (see the class docstring for the
        definitions).
        """
        gcols = normalize_groups(self._groups)
        cells = self._cells
        stat_names = self._comparison_stat_names(self._has_incr)
        if cells.height == 0:
            schema: dict[str, Any] = {c: cells.schema[c] for c in gcols}
            schema["estimator"] = pl.Utf8
            schema[axis] = cells.schema[axis]
            schema["n"] = pl.UInt32
            schema.update({name: pl.Float64 for name in stat_names})
            return pl.DataFrame(schema=schema)

        by = [*gcols, "estimator", axis]
        stats = cells.group_by(by).agg(
            self._comparison_agg_exprs(self._has_incr)
        )

        own = ["abs_err_mean", "ae_err_abs_mean", "ae_err_wt"]
        if self._has_incr:
            own += ["incr_" + name for name in own[:3]]
        base_stats = (
            stats.filter(pl.col("estimator") == self.baseline)
            .drop("estimator", "n")
            .rename({name: self._base_name(name) for name in own})
        )
        out = stats.filter(pl.col("estimator") != self.baseline).join(
            base_stats, on=[*gcols, axis], how="left"
        )

        win_aggs = [pl.col("_win").mean().alias("win_rate")]
        if self._has_incr:
            win_aggs.append(pl.col("_incr_win").mean().alias("incr_win_rate"))
        wins = win_cells.group_by(by).agg(win_aggs)
        out = out.join(wins, on=by, how="left")

        rel_exprs = [
            self._rel_expr(
                "abs_err_mean", "base_abs_err_mean", "abs_err_rel"
            ),
            self._rel_expr(
                "ae_err_abs_mean", "base_ae_err_abs_mean", "ae_err_rel"
            ),
            self._rel_expr(
                "ae_err_wt", "base_ae_err_wt", "bias_rel", absolute=True
            ),
        ]
        if self._has_incr:
            rel_exprs += [
                self._rel_expr(
                    "incr_abs_err_mean",
                    "incr_base_abs_err_mean",
                    "incr_abs_err_rel",
                ),
                self._rel_expr(
                    "incr_ae_err_abs_mean",
                    "incr_base_ae_err_abs_mean",
                    "incr_ae_err_rel",
                ),
                self._rel_expr(
                    "incr_ae_err_wt",
                    "incr_base_ae_err_wt",
                    "incr_bias_rel",
                    absolute=True,
                ),
            ]
        out = out.with_columns(rel_exprs)
        out = out.select([*gcols, "estimator", axis, "n", *stat_names])
        return self._sort_with_estimator(out, gcols, [axis])

    def _build_match_summary(self) -> pl.DataFrame:
        """Per-(estimator, depth) record of the matched-population loss.

        Covers ALL of each estimator's surviving depths (not just the
        common ones), so a depth excluded by the intersection shows up with
        ``n_matched = 0`` rather than disappearing.
        """
        gcols = normalize_groups(self._groups)
        by = [*gcols, "holdout"]
        parts: list[pl.DataFrame] = []
        for label in self._labels:
            own = self._fits[label]._ae_err
            n_cells = own.group_by(by).agg(
                pl.len().cast(pl.Int64).alias("n_cells")
            )
            n_matched = (
                self._cells.filter(pl.col("estimator") == label)
                .group_by(by)
                .agg(pl.len().cast(pl.Int64).alias("n_matched"))
            )
            parts.append(
                n_cells.join(n_matched, on=by, how="left")
                .with_columns(pl.col("n_matched").fill_null(0))
                .with_columns(
                    (pl.col("n_cells") - pl.col("n_matched")).alias(
                        "n_dropped"
                    ),
                    pl.lit(label, dtype=pl.Utf8).alias("estimator"),
                )
                .select(
                    [
                        *gcols, "estimator", "holdout",
                        "n_cells", "n_matched", "n_dropped",
                    ]
                )
            )
        out = pl.concat(parts, how="vertical")
        return self._sort_with_estimator(out, gcols, ["holdout"])

    # -- accessors -----------------------------------------------------------

    @property
    def cells(self):
        return mirror_output(self._cells, self._output_type)

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
    def horizon_comparison(self):
        return mirror_output(self._horizon_comparison, self._output_type)

    @property
    def anchor_comparison(self):
        return mirror_output(self._anchor_comparison, self._output_type)

    @property
    def holdout_comparison(self):
        return mirror_output(self._holdout_comparison, self._output_type)

    @property
    def match_summary(self):
        return mirror_output(self._match_summary, self._output_type)

    @property
    def holdouts(self) -> tuple[int, ...]:
        return self._common_holdouts

    @property
    def skipped_holdouts(self) -> dict[str, list[int]]:
        return {
            label: f.skipped_holdouts for label, f in self._fits.items()
        }

    @property
    def fits(self) -> "dict[str, BacktestFit]":
        return dict(self._fits)

    # -- crossover reader ------------------------------------------------------

    @staticmethod
    def _entry_winners(
        ch_vals: list, base_vals: list
    ) -> "list[str | None]":
        """Per-entry winner tokens: ``"c"`` (challenger's value strictly
        smaller), ``"b"`` (baseline's), or ``None`` on an exact tie or any
        null / non-finite value (no evidence either way)."""
        winners: "list[str | None]" = []
        for c, b in zip(ch_vals, base_vals):
            if c is None or b is None:
                winners.append(None)
            elif not (math.isfinite(c) and math.isfinite(b)):
                winners.append(None)
            elif c < b:
                winners.append("c")
            elif c > b:
                winners.append("b")
            else:
                winners.append(None)
        return winners

    @staticmethod
    def _terminal_run(
        winners: "list[str | None]",
    ) -> "tuple[int, str | None, int, int]":
        """Walk backward from the deepest entry for the MAXIMAL TERMINAL
        same-winner run (a null winner breaks the run).

        Returns ``(run_length, owner, run_start_index, n_flips)`` --
        ``owner`` is the run's winner token (``None`` when the deepest
        entry has no winner), ``run_start_index`` the index of the run's
        first entry (``len(winners)`` for a zero-length run), and
        ``n_flips`` the number of winner changes between consecutive
        non-null-winner entries over the WHOLE sequence (0 = stable
        ranking). The comparison frames are tiny, so a clear Python walk is
        preferred over a window-expression formulation.
        """
        run = 0
        owner: "str | None" = None
        for w in reversed(winners):
            if w is None:
                break
            if owner is None:
                owner = w
            elif w != owner:
                break
            run += 1
        seq = [w for w in winners if w is not None]
        n_flips = sum(1 for a, b in zip(seq, seq[1:]) if a != b)
        return run, owner, len(winners) - run, n_flips

    @staticmethod
    def _pooled_value(
        sub: pl.DataFrame, metric: str, lane: str
    ) -> "float | None":
        """Pooled metric value over a matched-cell subset (one estimator's
        side): ``"abs_err"`` -> ``mean|actual - expected|``, ``"ae_err"``
        -> ``mean|ae_err|``, ``"bias"`` -> ``|sum(actual - expected) /
        sum(expected)|``. ``None`` on an empty subset, a zero / null
        denominator, or a non-finite result."""
        pre = "incr_" if lane == "incremental" else ""
        if sub.height == 0:
            return None
        if metric == "abs_err":
            v = sub.select(
                (pl.col(f"{pre}actual") - pl.col(f"{pre}expected"))
                .abs()
                .mean()
            ).item()
        elif metric == "ae_err":
            v = sub.select(pl.col(f"{pre}ae_err").abs().mean()).item()
        else:  # bias
            num = sub.select(
                (pl.col(f"{pre}actual") - pl.col(f"{pre}expected")).sum()
            ).item()
            den = sub.select(pl.col(f"{pre}expected").sum()).item()
            if (
                num is None
                or den is None
                or not math.isfinite(den)
                or den == 0
            ):
                return None
            v = abs(num / den)
        if v is None or not math.isfinite(v):
            return None
        return float(v)

    @classmethod
    def _pooled_winner(
        cls,
        ch_cells: pl.DataFrame,
        base_cells: pl.DataFrame,
        metric: str,
        lane: str,
    ) -> "str | None":
        """Winner token of the pooled metric over a matched-cell subset:
        ``"c"`` / ``"b"`` for the strictly smaller side, ``None`` on a tie
        or when either side's pooled value is unavailable."""
        cv = cls._pooled_value(ch_cells, metric, lane)
        bv = cls._pooled_value(base_cells, metric, lane)
        if cv is None or bv is None:
            return None
        if cv < bv:
            return "c"
        if cv > bv:
            return "b"
        return None

    def crossover(
        self,
        by: str = "horizon",
        metric: str = "abs_err",
        lane: str = "cumulative",
        min_run: int = 6,
    ):
        """Where (if anywhere) the better method flips along an axis.

        Per (group, challenger), the comparison frame of the chosen axis is
        walked over its observed entries; each entry's winner is whichever
        side has the strictly smaller metric value (null on a tie or any
        null value). The read then looks for the MAXIMAL TERMINAL
        same-winner run -- walking backward from the deepest observed entry,
        a null winner breaking the run. The suffix semantics are deliberate:
        a flip that later flips back is not actionable (acting on it means
        trusting a ranking that did not hold), so only a flip that HOLDS to
        the end of the observed range counts. ``late_winner`` is the run's
        owner if the run holds at least ``min_run`` observed entries (null
        otherwise); ``early_winner`` is the pooled winner recomputed over
        the matched CELLS at the entries before the run -- null when the
        terminal run has length 0 (a null winner at the deepest entry
        leaves no entries-before-the-run region to pool); ``crossover_at``
        is the smallest axis value of the run -- non-null only when the run
        is long enough, does not start at the first observed entry, and
        ``early_winner`` is non-null and differs from ``late_winner``.

        A null ``crossover_at`` is honest, not a failure: there is no
        sustained flip -- either one method dominates the whole observed
        range or the ranking flickers without settling. "No crossover +
        ``overall_winner`` = X" is the deprecation-evidence read: the other
        method adds nothing out of sample anywhere in the observed range.

        Why three metrics, and why ``abs_err`` is the default:

        * ``"abs_err"`` compares ``abs_err_mean`` -- mean per-cell absolute
          error in target units. Cell-level accuracy; the default.
        * ``"ae_err"`` compares ``ae_err_abs_mean`` -- mean per-cell
          ``|actual / expected - 1|``. The relative (dimensionless)
          companion, which weights every cell equally regardless of size.
        * ``"bias"`` compares ``|ae_err_wt|`` -- the magnitude of the
          pooled signed bias. A calibration read, but cancellation-prone:
          cohorts' opposite-signed errors cancel inside the pooled sum, so
          a method can look unbiased while being inaccurate everywhere --
          it cannot distinguish "accurate" from "offsetting errors". That
          is why ``abs_err``, not ``bias``, is the default.

        Why ``min_run`` guards the call: at the deep end of either axis the
        matched population thins out, so a one- or two-entry terminal run
        is noise, not evidence of a flip (the same fail-OPEN data-edge
        failure mode the rolling backtest's convergence reader guards
        against). Demanding a sustained run keeps a thin tail from firing
        the call on its own; ``min_run=1`` reproduces the unguarded read.

        Parameters
        ----------
        by
            ``"horizon"`` (default) or ``"anchor"`` -- which comparison
            axis to walk (``anchor`` walks ``anchor_comparison`` over
            ``anchor_duration``).
        metric
            ``"abs_err"`` (default), ``"ae_err"``, or ``"bias"`` -- see
            above.
        lane
            ``"cumulative"`` (default) or ``"incremental"``. The
            incremental lane is available only when every estimator's
            surviving folds carried an incremental projection.
        min_run
            Minimum number of observed entries the terminal run must hold
            before ``late_winner`` (and so ``crossover_at``) fires. Must be
            an int >= 1; ``1`` reproduces the unguarded read. Default
            ``6``.

        Returns
        -------
        DataFrame
            Input-mirrored, one row per (group x challenger), sorted by the
            group columns then estimator insertion order: ``[groups?,
            estimator, crossover_at, early_winner, late_winner,
            overall_winner, n_flips, max_horizon]`` (``max_anchor`` for
            ``by="anchor"``). ``crossover_at`` keeps the axis dtype; the
            winner columns are estimator labels (Utf8); ``overall_winner``
            is the pooled winner over ALL matched cells (null on a tie or
            an empty population); ``max_*`` is the largest observed axis
            entry.
        """
        if by not in ("horizon", "anchor"):
            raise ValueError(
                f'by must be "horizon" or "anchor", got {by!r}'
            )
        if metric not in self._METRIC_PAIRS:
            raise ValueError(
                f"metric must be one of {tuple(self._METRIC_PAIRS)}, "
                f"got {metric!r}"
            )
        if lane not in ("cumulative", "incremental"):
            raise ValueError(
                f'lane must be "cumulative" or "incremental", got {lane!r}'
            )
        if lane == "incremental" and not self._has_incr:
            raise ValueError(
                'lane="incremental" is unavailable for this fit: not every '
                "estimator's surviving hold-out depths carried an "
                "incremental projection, so the matched cells have no "
                "incr_* lane (see the module docstring)"
            )
        if (
            isinstance(min_run, bool)
            or not isinstance(min_run, int)
            or min_run < 1
        ):
            raise ValueError(
                f"min_run must be an int >= 1, got {min_run!r}"
            )

        axis = "horizon" if by == "horizon" else "anchor_duration"
        max_col = "max_horizon" if by == "horizon" else "max_anchor"
        comp = (
            self._horizon_comparison
            if by == "horizon"
            else self._anchor_comparison
        )
        ch_col, base_col = self._METRIC_PAIRS[metric]
        if lane == "incremental":
            ch_col, base_col = "incr_" + ch_col, "incr_" + base_col

        gcols = normalize_groups(self._groups)
        axis_dt = comp.schema[axis]
        schema: dict[str, Any] = {c: comp.schema[c] for c in gcols}
        schema["estimator"] = pl.Utf8
        schema["crossover_at"] = axis_dt
        schema["early_winner"] = pl.Utf8
        schema["late_winner"] = pl.Utf8
        schema["overall_winner"] = pl.Utf8
        schema["n_flips"] = pl.Int64
        schema[max_col] = axis_dt
        if comp.height == 0:
            return mirror_output(
                pl.DataFrame(schema=schema), self._output_type
            )

        rows: list[dict[str, Any]] = []
        for part in comp.partition_by(
            [*gcols, "estimator"], maintain_order=True
        ):
            part = part.sort(axis)
            label = part["estimator"][0]
            values = part[axis].to_list()
            ch_vals = part[ch_col].to_list()
            base_vals = part[base_col].to_list()
            if metric == "bias":
                ch_vals = [None if v is None else abs(v) for v in ch_vals]
                base_vals = [
                    None if v is None else abs(v) for v in base_vals
                ]
            winners = self._entry_winners(ch_vals, base_vals)
            run, owner, run_start, n_flips = self._terminal_run(winners)
            late = owner if run >= min_run else None

            sub = self._cells
            for c in gcols:
                sub = sub.filter(pl.col(c) == part[c][0])
            ch_cells = sub.filter(pl.col("estimator") == label)
            base_cells = sub.filter(pl.col("estimator") == self.baseline)

            overall = self._pooled_winner(ch_cells, base_cells, metric, lane)

            if run == 0 or run_start == 0:
                # No pre-run cells to pool: either the deepest entry has no
                # winner (a zero-length terminal run, where "the entries
                # before the run" would mislabel ALL entries as pre-run), or
                # the run spans every observed entry.
                early = None
            else:
                pre = values[:run_start]
                early = self._pooled_winner(
                    ch_cells.filter(pl.col(axis).is_in(pre)),
                    base_cells.filter(pl.col(axis).is_in(pre)),
                    metric,
                    lane,
                )

            crossover_at = None
            if (
                late is not None
                and run_start > 0
                and early is not None
                and early != late
            ):
                crossover_at = values[run_start]

            def lab(token: "str | None") -> "str | None":
                if token == "c":
                    return label
                if token == "b":
                    return self.baseline
                return None

            row: dict[str, Any] = {c: part[c][0] for c in gcols}
            row["estimator"] = label
            row["crossover_at"] = crossover_at
            row["early_winner"] = lab(early)
            row["late_winner"] = lab(late)
            row["overall_winner"] = lab(overall)
            row["n_flips"] = n_flips
            row[max_col] = values[-1]
            rows.append(row)

        out = pl.DataFrame(rows, schema=schema)
        return mirror_output(out, self._output_type)

    # -- plotting --------------------------------------------------------------

    def plot(
        self,
        by: str = "horizon",
        metric: str = "abs_err",
        lane: str = "cumulative",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Matched reliability curves, one line per estimator, backed by
        matplotlib.

        ``by`` selects the axis: ``"horizon"`` (default), ``"anchor"``, or
        ``"holdout"``. ``metric`` selects the y statistic from the matched
        summaries: ``"abs_err"`` (default) -> ``abs_err_mean``;
        ``"ae_err"`` -> ``ae_err_mean`` (the SIGNED mean, with a zero
        line); ``"bias"`` -> ``ae_err_wt`` (signed pooled bias, zero
        line). ``lane="incremental"`` switches to the ``incr_*`` companions
        (available only when every estimator carried the incremental
        lane). One facet per group; the estimator insertion order fixes the
        line / colour order and the single legend.

        Returns
        -------
        matplotlib.figure.Figure
        """
        from ._comparison_vis import plot_estimator_comparison
        return plot_estimator_comparison(
            self, by=by, metric=metric, lane=lane,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )

    def __repr__(self) -> str:
        skipped = {
            label: s for label, s in self.skipped_holdouts.items() if s
        }
        sk = f", skipped={skipped}" if skipped else ""
        return (
            f"<EstimatorComparisonFit: estimators={self._labels}, "
            f"baseline={self.baseline!r}, "
            f"holdouts={list(self._common_holdouts)}, "
            f"target={self.target!r}, "
            f"n_matched_cells={self._n_matched}{sk}>"
        )
