"""Loss-ratio composition (charter Sec.5.3 -- numerator / denominator).

``Ratio`` pairs a loss-side estimator (``PooledLoss`` / ``CredibleLoss`` /
``SmoothLoss`` / ``ChainLadder``) with a premium-side estimator
(``PooledPremium``) and composes the projected loss ratio
``ratio_proj = loss_proj / premium_proj`` cell by cell, propagating the two
projections' uncertainty into a ratio band.

Two ``se_method`` settings control how premium uncertainty enters:

* ``"fixed"`` -- premium is treated as known (the projected denominator is
  deterministic): ``ratio_se = loss_total_se / premium_proj``. This is the
  band the loss fit already carries internally; ``Ratio(se_method="fixed")``
  reproduces it while letting the denominator model be chosen and inspected.
* ``"delta"`` -- the full first-order delta method on ``R = L / P``, adding the
  premium variance and an optional loss-premium correlation ``rho``::

      ratio_se = (1 / |P|) * sqrt(seL^2 + R^2 seP^2 - 2 R rho seL seP)

  ``rho = "auto"`` (default) estimates the coupling per group from the data
  (the correlation of the loss and premium relative residuals) instead of
  assuming a value. A fixed ``rho = 0`` treats the loss and premium projections
  as independent; a positive ``rho`` (claims and premium moving together within
  a cohort) narrows the band.

The result is a :class:`RatioFit` -- a long-format frame with ``ratio_proj`` +
the ratio band, carrying the contributing ``loss_proj`` / ``premium_proj`` and
their SEs for transparency.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, Literal

import numpy as np
import polars as pl
from scipy.stats import norm

from ._io import mirror_output, normalize_groups
from .loss import LossFit, _LossEstimatorBase
from .premium import PremiumFit, _PremiumEstimatorBase

if TYPE_CHECKING:
    from ._io import FrameLike
    from .triangle import Triangle


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
    se_method
        ``"fixed"`` (premium deterministic; default) or ``"delta"`` (full
        delta method with premium variance + ``rho``).
    rho
        Loss-premium correlation used by ``se_method="delta"``. ``"auto"``
        (default) estimates it per group from the data (the correlation of the
        loss and premium relative residuals); or a float in ``[-1, 1]``
        (``0.0`` = independent). Ignored when ``se_method="fixed"``.
    conf_level
        Two-sided confidence level for the ratio CI columns.
    """

    loss: "_LossEstimatorBase"
    premium: "_PremiumEstimatorBase" = field(default_factory=lambda: _default_premium())
    se_method: str = "fixed"
    rho: "float | Literal['auto']" = "auto"
    conf_level: float = 0.95

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
        if self.se_method not in ("fixed", "delta"):
            raise ValueError(
                f"se_method must be 'fixed' or 'delta', got {self.se_method!r}"
            )
        if isinstance(self.rho, str):
            if self.rho != "auto":
                raise ValueError(
                    f"rho must be a float in [-1, 1] or 'auto', got {self.rho!r}"
                )
        elif not (-1.0 <= self.rho <= 1.0):
            raise ValueError(
                f"rho must be a float in [-1, 1] or 'auto', got {self.rho!r}"
            )
        if not (0.0 < self.conf_level < 1.0):
            raise ValueError(f"conf_level must be in (0, 1), got {self.conf_level!r}")

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

        z = float(norm.ppf((1 + self.conf_level) / 2))
        # deterministic denominator unless `delta`; a zero/null premium yields a
        # null ratio rather than an inf.
        safe_pp = (
            pl.when(pl.col("premium_proj").is_null() | (pl.col("premium_proj") == 0.0))
            .then(None)
            .otherwise(pl.col("premium_proj"))
        )
        ratio_proj = pl.col("loss_proj") / safe_pp
        if self.se_method == "fixed":
            ratio_se = pl.col("loss_total_se") / safe_pp.abs()
        else:
            seL = pl.col("loss_total_se")
            seP = pl.col("premium_total_se")
            if self.rho == "auto":
                rho_frame = _estimate_rho(triangle, groups)
                if groups:
                    joined = joined.join(rho_frame, on=groups, how="left")
                    rho_expr = pl.col("_rho_auto").fill_null(0.0)
                else:
                    rho_expr = pl.lit(float(rho_frame["_rho_auto"][0]))
            else:
                rho_expr = pl.lit(float(self.rho))
            radicand = (
                seL**2
                + ratio_proj**2 * seP**2
                - 2.0 * rho_expr * ratio_proj * seL * seP
            )
            # clamp a negative radicand to 0 while PRESERVING null: a missing
            # loss/premium SE (e.g. on an observed cell) must yield a null
            # ratio_se, matching the "fixed" path -- pl.max_horizontal would
            # swallow the null into 0 (a spurious zero-width band).
            clamped = pl.when(radicand < 0.0).then(pl.lit(0.0)).otherwise(radicand)
            ratio_se = clamped.sqrt() / safe_pp.abs()

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
            se_method=self.se_method,
            rho=self.rho,
            conf_level=self.conf_level,
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


def _estimate_rho(triangle: "Triangle", groups: "list[str] | None") -> pl.DataFrame:
    """Per-group empirical loss-premium correlation for ``rho="auto"``.

    Returns a frame ``[*groups, _rho_auto]``. ``_rho_auto`` is the correlation,
    within each group, of the RELATIVE residuals of the incremental loss and
    incremental premium after removing the group's pooled per-duration intensity
    ``g_k = sum(loss_delta) / sum(premium_from)`` and premium link ratio
    ``f^P_k = sum(premium_to) / sum(premium_from)``. It captures how the loss and
    premium NOISE co-move -- the coupling the delta method takes as ``rho``.
    Model-agnostic (built from the triangle's own pooled factors, not the chosen
    estimator). Groups with < 10 usable links fall back to 0.
    """
    gcols = groups or []
    link = triangle.link().to_polars()
    fac = link.group_by([*gcols, "duration_from"]).agg(
        _g=pl.col("loss_delta").sum() / pl.col("premium_from").sum(),
        _fp=pl.col("premium_to").sum() / pl.col("premium_from").sum(),
    )
    work = (
        link.join(fac, on=[*gcols, "duration_from"], how="left")
        .with_columns(
            _lhat=pl.col("_g") * pl.col("premium_from"),
            _phat=(pl.col("_fp") - 1.0) * pl.col("premium_from"),
        )
        .filter((pl.col("_lhat").abs() > 1.0) & (pl.col("_phat").abs() > 1.0))
        .with_columns(
            _rl=(pl.col("loss_delta") - pl.col("_lhat")) / pl.col("_lhat"),
            _rp=(pl.col("premium_delta") - pl.col("_phat")) / pl.col("_phat"),
        )
        .filter(pl.col("_rl").is_finite() & pl.col("_rp").is_finite())
    )
    if gcols:
        rho = work.group_by(gcols).agg(
            _rho_auto=pl.corr("_rl", "_rp"), _n=pl.len()
        )
    else:
        rho = work.select(_rho_auto=pl.corr("_rl", "_rp"), _n=pl.len())
    rho = rho.with_columns(
        _rho_auto=pl.when(pl.col("_n") >= 10)
        .then(pl.col("_rho_auto").clip(-1.0, 1.0).fill_null(0.0))
        .otherwise(0.0)
    )
    return rho.select([*gcols, "_rho_auto"])


def _segment_premium_growth(sub: pl.DataFrame, window: int) -> float:
    """Recent per-period premium growth factor at the frontier.

    Geometric mean of the last ``window`` volume-weighted premium link ratios
    ``f^P_k = sum P_{k+1} / sum P_k``. Returns ~1.0 if premium has plateaued
    (no new premium coming in), > 1 if premium is still being paid in. Used to
    grow the frozen-ratio amount projection beyond the frontier.
    """
    from .stability import _segment_matrices

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
        se_method: str,
        rho: float,
        conf_level: float,
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
        self.se_method = se_method
        self.rho = rho
        self.conf_level = conf_level
        self.loss_fit = loss_fit
        self.premium_fit = premium_fit

    @property
    def df(self) -> "FrameLike":
        return mirror_output(self._df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def summary(self) -> "FrameLike":
        """Per-cohort headline: the latest within-triangle projected loss ratio
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
        from ._fit_vis import plot_fit, resolve_fit_metric

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

        Beyond the observed development frontier the honest go-forward is to hold
        the frontier loss ratio flat -- but only once the development has settled
        (real-data OOS: loss and premium co-develop and their tails cancel in the
        ratio, so a flat freeze beats extrapolating the components). This runs the
        :class:`~lossratio.stability.Stability` gate (same ``window`` / ``tol``)
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
        from .stability import Stability

        seg_cols = normalize_groups(self.groups) or []
        base = self._df

        base_cols = [*seg_cols, "cohort", "duration", pl.col("ratio_proj").alias("ratio")]
        if amounts:
            base_cols += [pl.col("loss_proj").alias("loss"),
                          pl.col("premium_proj").alias("premium")]
        base_out = base.select(base_cols).with_columns(status=pl.lit("projected"))

        # per (segment, cohort) freeze point = deepest non-null projected ratio
        keys = seg_cols + ["cohort"]
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
        if seg_cols:
            froze = froze.join(rep.select([*seg_cols, "stable"]), on=seg_cols, how="left")
        else:
            sv = rep.get_column("stable")[0]
            froze = froze.with_columns(stable=pl.lit(bool(sv) if sv is not None else False))
        if amounts:
            froze = froze.join(self._premium_growth(seg_cols, window),
                               on=seg_cols if seg_cols else None,
                               how="cross" if not seg_cols else "left")

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
            prem = pl.col("freeze_premium") * pl.col("_gP").pow(steps)
            ext = ext.with_columns(
                premium=pl.when(pl.col("is_stable")).then(prem).otherwise(None),
            ).with_columns(
                loss=pl.when(pl.col("is_stable"))
                .then(pl.col("ratio") * pl.col("premium"))
                .otherwise(None),
            )
        out_cols = [*seg_cols, "cohort", "duration", "ratio", "status"]
        if amounts:
            out_cols = [*seg_cols, "cohort", "duration", "loss", "premium", "ratio", "status"]
        out = pl.concat(
            [base_out.select(out_cols), ext.select(out_cols)], how="vertical_relaxed"
        ).sort([*seg_cols, "cohort", "duration"])
        return mirror_output(out, self._output_type)

    def _premium_growth(self, seg_cols: list[str], window: int) -> pl.DataFrame:
        """Per-segment recent premium growth factor (``_gP``) for amount
        extension."""
        df_tri = self._triangle.to_polars()
        if seg_cols:
            seg_keys = df_tri.select(seg_cols).unique(maintain_order=True).rows()
        else:
            seg_keys = [()]
        rows = []
        for key in seg_keys:
            if seg_cols:
                m = pl.lit(True)
                for col, val in zip(seg_cols, key):
                    m = m & (pl.col(col) == val)
                sub = df_tri.filter(m)
            else:
                sub = df_tri
            g = _segment_premium_growth(sub, window)
            rows.append({**{c: v for c, v in zip(seg_cols, key)}, "_gP": g})
        return pl.DataFrame(rows)

    def __repr__(self) -> str:
        return (
            f"RatioFit(loss_model={self.loss_model!r}, "
            f"premium_model={self.premium_model!r}, "
            f"se_method={self.se_method!r}, rows={self._df.height})"
        )
