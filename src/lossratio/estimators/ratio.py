"""Loss-ratio composition (numerator / denominator).

``Ratio`` pairs a loss-side estimator (``PooledLoss`` / ``CredibleLoss`` /
``SmoothLoss`` / ``ChainLadder``) with a premium-side estimator
(``PooledPremium``) and composes the projected loss ratio
``ratio_proj = loss_proj / premium_proj`` cell by cell, banding the result from
the loss projection's uncertainty.

The premium denominator is treated as known: ``ratio_se = loss_total_se /
premium_proj``. The risk premium is an allocated exposure (rate x in-force), not
a stochastic claims-development process -- its development-factor scatter is
compositional, not forecast uncertainty -- so banding it would inject an
artifact that, where it does not simply cancel against the numerator, only
widens the ratio band spuriously. The ratio band therefore carries the loss
fit's own uncertainty, divided by the projected denominator.

The result is a :class:`RatioFit` -- a long-format frame with ``ratio_proj`` +
the ratio band, carrying the contributing ``loss_proj`` / ``premium_proj`` and
their SEs for transparency.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from .._kernels.io import mirror_output, normalize_groups
from ._base import _LossEstimatorBase
from .loss import LossFit
from ._base import _PremiumEstimatorBase
from .premium import PremiumFit

if TYPE_CHECKING:
    from .._kernels.io import FrameLike
    from ..core.triangle import Triangle


_RATIO_COLUMNS = [
    "cohort", "duration",
    "loss_proj", "premium_proj", "ratio_proj",
    "loss_total_se", "premium_total_se",
    "ratio_se", "ratio_ci_lo", "ratio_ci_hi",
    "source",
]


@dataclass(kw_only=True)
class Ratio:
    """Loss-ratio composition of a loss estimator and a premium estimator.

    Parameters
    ----------
    loss
        A loss-side estimator (``PooledLoss`` / ``CredibleLoss`` /
        ``SmoothLoss`` / ``ChainLadder``).
    premium
        A premium-side estimator (``PooledPremium``). Defaults to a plain
        ``PooledPremium()``.
    confidence_level
        Two-sided confidence level for the ratio CI columns.
    """

    loss: "_LossEstimatorBase"
    premium: "_PremiumEstimatorBase" = field(default_factory=lambda: _default_premium())
    confidence_level: float = 0.95

    def __post_init__(self) -> None:
        if not isinstance(self.loss, _LossEstimatorBase):
            raise TypeError(
                "loss must be a loss-side estimator (PooledLoss / CredibleLoss "
                f"/ SmoothLoss / ChainLadder), got {type(self.loss).__name__}"
            )
        if not isinstance(self.premium, _PremiumEstimatorBase):
            raise TypeError(
                "premium must be a premium-side estimator (PooledPremium), got "
                f"{type(self.premium).__name__}"
            )
        if not (0.0 < self.confidence_level < 1.0):
            raise ValueError(f"confidence_level must be in (0, 1), got {self.confidence_level!r}")

    def fit(self, triangle: "Triangle") -> "RatioFit":
        """Fit both sides on ``triangle`` and compose the loss ratio."""
        loss_fit = self.loss.fit(triangle)
        premium_fit = self.premium.fit(triangle)
        groups = normalize_groups(triangle.groups)
        keys = (groups or []) + ["cohort", "duration"]

        left = loss_fit._df.select(
            [*keys, "loss_proj", "loss_total_se", "source"]
        )
        right = premium_fit._df.select([*keys, "premium_proj", "premium_total_se"])
        joined = left.join(right, on=keys, how="left")

        z = float(norm.ppf((1 + self.confidence_level) / 2))
        # The denominator is known (allocated exposure); a zero/null premium
        # yields a null ratio rather than an inf.
        safe_pp = (
            pl.when(pl.col("premium_proj").is_null() | (pl.col("premium_proj") == 0.0))
            .then(None)
            .otherwise(pl.col("premium_proj"))
        )
        ratio_proj = pl.col("loss_proj") / safe_pp
        ratio_se = pl.col("loss_total_se") / safe_pp.abs()

        composed = joined.with_columns(
            ratio_proj.alias("ratio_proj"),
        ).with_columns(
            ratio_se.alias("ratio_se"),
        ).with_columns(
            # floor the lower bound at 0 (a loss ratio is non-negative) only
            # when the band exists; a null projection or null SE -> null bounds
            # (no spurious ci_lo=0 sitting against a null ci_hi).
            ratio_ci_lo=pl.when(
                pl.col("ratio_proj").is_null() | pl.col("ratio_se").is_null()
            )
            .then(None)
            .otherwise(
                pl.max_horizontal(
                    pl.col("ratio_proj") - z * pl.col("ratio_se"), pl.lit(0.0)
                )
            ),
            ratio_ci_hi=pl.col("ratio_proj") + z * pl.col("ratio_se"),
        )

        order = (groups or []) + _RATIO_COLUMNS
        df = composed.select(order)

        return RatioFit(
            df,
            groups=triangle.groups,
            loss_model=loss_fit.model,
            premium_model=premium_fit.model,
            confidence_level=self.confidence_level,
            output_type=triangle._output_type,
            loss_fit=loss_fit,
            premium_fit=premium_fit,
            triangle=triangle,
        )


def _default_premium() -> "_PremiumEstimatorBase":
    # local import avoids a module-load cycle (pooled_premium -> premium_fit;
    # ratio -> pooled_premium would otherwise import at module scope).
    from .pooled_premium import PooledPremium

    return PooledPremium()


def _segment_premium_growth(sub: pl.DataFrame, window: int) -> float:
    """Recent per-period premium growth factor at the frontier.

    Geometric mean of the last ``window`` volume-weighted premium link ratios
    ``f^P_k = sum P_{k+1} / sum P_k``. Returns ~1.0 if premium has plateaued
    (no new premium coming in), > 1 if premium is still being paid in. Used to
    grow the frozen-ratio amount projection beyond the frontier.
    """
    from ..diagnostics.stability import _segment_matrices

    _, P, _ = _segment_matrices(sub.select(["cohort", "duration", "loss", "premium"]))
    fp: list[float] = []
    for k in range(P.shape[1] - 1):
        both = ~np.isnan(P[:, k]) & ~np.isnan(P[:, k + 1])
        sk = np.nansum(P[both, k])
        if both.sum() >= 1 and sk > 0:
            r = np.nansum(P[both, k + 1]) / sk
            if np.isfinite(r) and r > 0:
                fp.append(r)
    if not fp:
        return 1.0
    return float(np.exp(np.mean(np.log(fp[-window:]))))


# ---------------------------------------------------------------------------
# Result object
# ---------------------------------------------------------------------------


class RatioFit:
    """Composed loss-ratio projection result.

    The long-format frame (one row per cohort x duration cell) carries
    ``ratio_proj`` + the ratio band, plus the contributing ``loss_proj`` /
    ``premium_proj`` and their SEs. The underlying ``loss_fit`` / ``premium_fit``
    are retained for drill-down.
    """

    def __init__(
        self,
        df: pl.DataFrame,
        *,
        groups: "str | list[str] | None",
        loss_model: str,
        premium_model: str,
        confidence_level: float,
        output_type: str,
        loss_fit: LossFit,
        premium_fit: PremiumFit,
        triangle: "Triangle | None" = None,
    ) -> None:
        self._df = df
        self._output_type = output_type
        self._triangle = triangle
        self.groups = groups
        self.loss_model = loss_model
        self.premium_model = premium_model
        self.confidence_level = confidence_level
        self.loss_fit = loss_fit
        self.premium_fit = premium_fit

    @property
    def df(self) -> "FrameLike":
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def summary(self) -> "FrameLike":
        """Per-cohort summary: the latest within-triangle projected loss ratio
        and its SE."""
        keys = (normalize_groups(self.groups) or []) + ["cohort"]
        agg = self._df.group_by(keys, maintain_order=True).agg(
            loss_proj=pl.col("loss_proj").drop_nulls().last(),
            premium_proj=pl.col("premium_proj").drop_nulls().last(),
            ratio_proj=pl.col("ratio_proj").drop_nulls().last(),
            ratio_se=pl.col("ratio_se").drop_nulls().last(),
        )
        return mirror_output(agg, self._output_type)

    def predict(self) -> "FrameLike":
        """Per-cell projection surface: projected loss, projected premium, the
        projected loss ratio, and each cell's ``source``. A focused view of
        :attr:`df` without the SE / CI columns."""
        keys = (normalize_groups(self.groups) or []) + ["cohort", "duration"]
        cols = keys + ["loss_proj", "premium_proj", "ratio_proj", "source"]
        return mirror_output(self._df.select(cols), self._output_type)

    def plot(
        self,
        metric: str = "ratio",
        *,
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: "tuple[float, float] | None" = None,
    ) -> Any:
        """Per-cohort cumulative-projection trajectories, faceted by group --
        the observed portion solid, the projected tail dashed. ``metric`` is
        ``"ratio"`` (default; the projected loss ratio), ``"loss"``, or
        ``"premium"``."""
        from .._plot.fit import plot_fit, resolve_fit_metric

        value_col, ylabel, hline = resolve_fit_metric(
            metric, ("ratio", "loss", "premium")
        )
        return plot_fit(
            self._df, value_col=value_col, ylabel=ylabel,
            title="loss ratio projection", groups=self.groups, hline=hline,
            nrow=nrow, ncol=ncol, figsize=figsize,
        )

    def extend(
        self, *, horizon: int, window: int = 6, tol: float = 0.01,
        amounts: bool = False,
    ) -> "FrameLike":
        """Extend the projected loss ratio flat to a target ``horizon`` duration.

        Beyond the observed loss-ratio frontier the honest go-forward is to hold
        the frontier loss ratio flat -- but only once the loss ratio has settled
        (real-data OOS: loss and premium co-develop and their tails cancel in the
        ratio, so a flat freeze beats extrapolating the components). This runs the
        :class:`~lossratio.diagnostics.stability.Stability` gate (same ``window`` / ``tol``)
        and, per segment, extends each cohort's last projected ratio out to
        ``horizon``:

        * **stable** segment -> the frontier ratio is held flat
          (``status="frozen"``); every duration past the frontier carries the
          same value.
        * **developing** (or ``insufficient_depth``) segment -> the forward ratio
          is left ``null`` with ``status="uncertain"``. The ratio has not settled,
          so NO flat value is fabricated -- the go-forward is genuinely unknown.

        With ``amounts=True`` the extension also carries ``loss`` and ``premium``:
        the loss ratio is frozen, but premium keeps being paid in, so premium is
        grown by its recent per-period growth factor (``~1`` once premium has
        plateaued, ``>1`` while still inflowing) and ``loss = ratio * premium``.
        Amounts are emitted only where the verdict is ``frozen``; ``uncertain``
        rows leave them ``null`` too.

        Returns a long frame (segment keys, ``cohort``, ``duration``, ``ratio``,
        ``status`` [+ ``loss`` / ``premium`` when ``amounts``]): observed /
        within-frontier rows carry ``status="projected"``, extension rows carry
        ``frozen`` / ``uncertain``.
        """
        if self._triangle is None:
            raise ValueError(
                "extend requires the source triangle; build the fit via "
                "Ratio(...).fit(triangle)."
            )
        if not isinstance(horizon, int) or horizon < 1:
            raise ValueError(f"horizon must be a positive int, got {horizon!r}")
        from ..diagnostics.stability import Stability

        group_cols = normalize_groups(self.groups) or []
        base = self._df

        base_cols = [*group_cols, "cohort", "duration", pl.col("ratio_proj").alias("ratio")]
        if amounts:
            base_cols += [pl.col("loss_proj").alias("loss"),
                          pl.col("premium_proj").alias("premium")]
        base_out = base.select(base_cols).with_columns(status=pl.lit("projected"))

        # per (segment, cohort) freeze point = deepest non-null projected ratio
        keys = group_cols + ["cohort"]
        froze = (
            base.filter(pl.col("ratio_proj").is_not_null())
            .group_by(keys, maintain_order=True)
            .agg(
                freeze_dur=pl.col("duration").max(),
                freeze_ratio=pl.col("ratio_proj").sort_by("duration").last(),
                freeze_premium=pl.col("premium_proj").sort_by("duration").last(),
            )
        )

        # base owns durations up to each cohort's freeze point; `ext` owns
        # (freeze_dur, horizon]. A cohort with a null-ratio TAIL (an unfittable
        # projection gap above its deepest non-null ratio) would otherwise emit
        # those tail cells twice -- once here as "projected", once in `ext` --
        # so trim base to <= freeze_dur (cohorts with no projection at all, not
        # in `froze`, keep every base row).
        base_out = base_out.join(
            froze.select([*keys, "freeze_dur"]), on=keys, how="left"
        ).filter(
            pl.col("freeze_dur").is_null()
            | (pl.col("duration") <= pl.col("freeze_dur"))
        ).drop("freeze_dur")

        # segment stability verdict + (for amounts) recent premium growth
        rep = Stability(window=window, tol=tol).assess(self._triangle).to_polars()
        if group_cols:
            froze = froze.join(rep.select([*group_cols, "stable"]), on=group_cols, how="left")
        else:
            sv = rep.get_column("stable")[0]
            froze = froze.with_columns(stable=pl.lit(bool(sv) if sv is not None else False))
        if amounts:
            froze = froze.join(self._premium_growth(group_cols, window),
                               on=group_cols if group_cols else None,
                               how="cross" if not group_cols else "left")

        ext = (
            froze.with_columns(
                duration=pl.int_ranges(pl.col("freeze_dur") + 1, horizon + 1)
            )
            .explode("duration")
            .filter(pl.col("duration").is_not_null())
            .with_columns(is_stable=pl.col("stable").fill_null(False))
        )
        ext = ext.with_columns(
            ratio=pl.when(pl.col("is_stable")).then(pl.col("freeze_ratio")).otherwise(None),
            status=pl.when(pl.col("is_stable")).then(pl.lit("frozen")).otherwise(pl.lit("uncertain")),
        )
        if amounts:
            steps = (pl.col("duration") - pl.col("freeze_dur")).cast(pl.Float64)
            premium = pl.col("freeze_premium") * pl.col("_gP").pow(steps)
            ext = ext.with_columns(
                premium=pl.when(pl.col("is_stable")).then(premium).otherwise(None),
            ).with_columns(
                loss=pl.when(pl.col("is_stable"))
                .then(pl.col("ratio") * pl.col("premium"))
                .otherwise(None),
            )
        out_cols = [*group_cols, "cohort", "duration", "ratio", "status"]
        if amounts:
            out_cols = [*group_cols, "cohort", "duration", "loss", "premium", "ratio", "status"]
        out = pl.concat(
            [base_out.select(out_cols), ext.select(out_cols)], how="vertical_relaxed"
        ).sort([*group_cols, "cohort", "duration"])
        return mirror_output(out, self._output_type)

    def _premium_growth(self, group_cols: list[str], window: int) -> pl.DataFrame:
        """Per-segment recent premium growth factor (``_gP``) for amount
        extension."""
        df_tri = self._triangle.to_polars()
        if group_cols:
            seg_keys = df_tri.select(group_cols).unique(maintain_order=True).rows()
        else:
            seg_keys = [()]
        rows = []
        for key in seg_keys:
            if group_cols:
                m = pl.lit(True)
                for col, val in zip(group_cols, key):
                    m = m & (pl.col(col) == val)
                sub = df_tri.filter(m)
            else:
                sub = df_tri
            g = _segment_premium_growth(sub, window)
            rows.append({**{c: v for c, v in zip(group_cols, key)}, "_gP": g})
        return pl.DataFrame(rows)

    def __repr__(self) -> str:
        return (
            f"RatioFit(loss_model={self.loss_model!r}, "
            f"premium_model={self.premium_model!r}, rows={self._df.height})"
        )
