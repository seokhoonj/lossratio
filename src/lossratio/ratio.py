"""Loss-ratio composition layer (``Ratio``).

:class:`Ratio` is the composition layer over :class:`Loss` and
:class:`Premium`. It delegates loss-side projection to :class:`Loss`,
retrieves the embedded :class:`PremiumFit`, and composes the loss-ratio
point + variance via the delta method.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import polars as pl
from scipy.stats import norm

from ._io import group_eq, mirror_output, normalize_groups
from ._recent import validate_recent as _validate_recent
from ._sigma import VALID_SIGMA_METHODS
from .loss import Loss, LossFit
from .premium import Premium, PremiumFit

if TYPE_CHECKING:
    from ._io import FrameLike
    from ._types import MaturityArg, RegimeArg, TailArg, UncertaintyArg
    from .curve import Curve
    from .triangle import Triangle


_VALID_METHODS = ("ed", "cl", "sa")
_VALID_PREMIUM_METHODS = ("ed", "cl")
_VALID_SE_METHODS = ("fixed", "delta")


def _compose_ratio_stats(
    full: pl.DataFrame,
    se_method: str,
    rho: float,
    conf_level: float,
) -> pl.DataFrame:
    """Attach ``ratio_se`` / ``ratio_cv`` / ``ratio_ci_lo`` / ``ratio_ci_hi``.

    ``ratio_se`` is the fixed-premium ratio ``SE_L / P`` when
    ``se_method == "fixed"``, else the full delta-method standard error
    including premium uncertainty and the loss-premium correlation
    ``rho``. Shared by the analytical RatioFit build and its
    bootstrap-overlay recompute; the two differ only in the upstream
    ``loss_total_se`` feeding it. Assumes ``ratio_proj`` already present.
    """
    if se_method == "fixed":
        full = full.with_columns(
            pl.when(
                pl.col("loss_total_se").is_not_null()
                & pl.col("premium_proj").is_not_null()
                & (pl.col("premium_proj") != 0.0)
            )
            .then(pl.col("loss_total_se") / pl.col("premium_proj"))
            .otherwise(None)
            .alias("ratio_se")
        )
    else:
        # delta: Var(L/P) ~ (SE_L / P)^2 + (L * SE_P / P^2)^2
        #                   - 2 rho L SE_L SE_P / P^3
        full = full.with_columns(
            (
                (pl.col("loss_total_se") / pl.col("premium_proj")) ** 2
                + (
                    pl.col("loss_proj")
                    * pl.col("premium_total_se")
                    / (pl.col("premium_proj") ** 2)
                )
                ** 2
                - 2
                * rho
                * pl.col("loss_proj")
                * pl.col("loss_total_se")
                * pl.col("premium_total_se")
                / (pl.col("premium_proj") ** 3)
            ).alias("_var_ratio")
        )
        full = full.with_columns(
            pl.when(
                pl.col("loss_proj").is_not_null()
                & pl.col("premium_proj").is_not_null()
                & (pl.col("premium_proj") > 0.0)
                & pl.col("loss_total_se").is_not_null()
                & pl.col("premium_total_se").is_not_null()
            )
            .then(pl.col("_var_ratio").clip(lower_bound=0.0).sqrt())
            .otherwise(None)
            .alias("ratio_se")
        ).drop("_var_ratio")

    z_alpha = float(norm.ppf((1 + conf_level) / 2))
    full = full.with_columns(
        pl.when(
            pl.col("ratio_proj").is_not_null()
            & (pl.col("ratio_proj") != 0.0)
            & pl.col("ratio_se").is_not_null()
        )
        .then(pl.col("ratio_se") / pl.col("ratio_proj").abs())
        .otherwise(None)
        .alias("ratio_cv"),
    )
    full = full.with_columns(
        pl.when(
            pl.col("ratio_proj").is_not_null() & pl.col("ratio_se").is_not_null()
        )
        .then(
            pl.max_horizontal(
                pl.lit(0.0),
                pl.col("ratio_proj") - z_alpha * pl.col("ratio_se"),
            )
        )
        .otherwise(None)
        .alias("ratio_ci_lo"),
        pl.when(
            pl.col("ratio_proj").is_not_null() & pl.col("ratio_se").is_not_null()
        )
        .then(pl.col("ratio_proj") + z_alpha * pl.col("ratio_se"))
        .otherwise(None)
        .alias("ratio_ci_hi"),
    )
    return full


@dataclass(kw_only=True)
class Ratio:
    """Loss-ratio estimator (composition over :class:`Loss` + :class:`Premium`).

    Parameters
    ----------
    method
        Loss projection method: ``"ed"`` (default), ``"cl"``, or
        ``"sa"``. See :class:`Loss`. The loss-ratio is the composed
        loss projection over the premium projection.
    loss_alpha
        Variance-structure exponent for the loss fit. Default ``1``.
    loss_regime
        Loss-side regime filter. Four-type dispatch: ``None`` (no
        filter), :class:`Regime` (eager), ``"auto"`` (auto-detect on
        the fit triangle), or a callable ``f(tri) -> Regime`` (lazy
        spec — re-detected per backtest fold). Cohorts strictly
        before the latest change date are dropped from the loss fit.
    premium_method
        One of ``"ed"`` (default) or ``"cl"``. Forwarded to
        :class:`Premium`.
    premium_alpha
        Variance-structure exponent for the premium fit. Default ``1``.
    maturity
        Maturity specification for the ``method="sa"`` switch.
        Four-type dispatch: ``"auto"`` (default,
        auto-detect tuned by ``max_cv`` / ``max_rse`` / ``min_run``),
        a :class:`~lossratio.Maturity` object (explicit override), a
        callable ``f(triangle) -> Maturity`` (lazy spec — e.g.
        :meth:`~lossratio.Maturity.detect`), or ``None`` (no switch, SA
        falls back to ED). Forwarded to the inner :class:`Loss`.
        Consulted only when ``method="sa"``.
    premium_regime
        Premium-side regime filter. Same four-type dispatch as
        ``loss_regime``. Defaults to ``None`` (no filter); pass an
        independent value when the underwriting / premium regime
        differs from the loss regime.
    sigma_method
        ``"locf"`` (default), ``"min_last2"``, or ``"loglinear"``.
    recent
        Optional positive integer. When supplied, only the most-recent
        ``recent`` calendar diagonals feed factor estimation across the
        inner loss and premium fits; the point projection still covers
        the full grid. ``None`` (default) leaves the fit byte-unchanged.
        This is a calendar-diagonal filter shared by both the loss and
        premium sides — orthogonal to the cohort-axis ``loss_regime`` /
        ``premium_regime`` cuts.
    se_method
        How ``ratio_se`` is computed:

        * ``"fixed"`` (default): premium treated as fixed —
          ``ratio_se = loss_total_se / premium``.
        * ``"delta"``: full delta method including premium uncertainty
          and the loss-premium correlation ``rho``.
    rho
        Loss-premium correlation in (-1, 1). Used only when
        ``se_method = "delta"``. Default ``0.95``.
    conf_level
        Confidence level for ``ratio_ci_lo`` / ``ratio_ci_hi``. Default
        ``0.95``.
    uncertainty
        Uncertainty strategy governing the ratio SE. ``None`` (default) /
        :class:`~lossratio.Analytical` keeps the closed-form analytical
        ratio SE. :class:`~lossratio.ResidualBootstrap` /
        :class:`~lossratio.ParametricBootstrap` overlay a loss-side
        bootstrap SE onto the *projected* cells of ``$full`` (premium is
        *not* bootstrapped) and recompute ``ratio_se`` / ``ratio_cv`` /
        ``ratio_ci_*`` from the bootstrap-derived ``loss_total_se``. The
        overlay follows the loss ``method`` (an ED headline gets an ED
        bootstrap), so the band is centred on the same projection that
        drives the point estimate.
    """

    method:         str            = "ed"
    loss_alpha:     float          = 1.0
    loss_regime:    RegimeArg      = None
    premium_method: str            = "ed"
    premium_alpha:  float          = 1.0
    premium_regime: RegimeArg      = None
    sigma_method:   str            = "locf"
    recent:         int | None     = None
    maturity:       MaturityArg    = "auto"
    max_cv:         float          = 0.15
    max_rse:        float          = 0.05
    min_run:        int            = 2
    se_method:      str            = "fixed"
    rho:            float          = 0.95
    conf_level:     float          = 0.95
    tail:           TailArg        = False
    uncertainty:    UncertaintyArg = None

    def __post_init__(self) -> None:
        from .tail import validate_tail

        if self.method not in _VALID_METHODS:
            raise ValueError(
                f"method must be one of {_VALID_METHODS}, got {self.method!r}"
            )
        if self.premium_method not in _VALID_PREMIUM_METHODS:
            raise ValueError(
                f"premium_method must be one of {_VALID_PREMIUM_METHODS}, "
                f"got {self.premium_method!r}"
            )
        if self.se_method not in _VALID_SE_METHODS:
            raise ValueError(
                f"se_method must be one of {_VALID_SE_METHODS}, "
                f"got {self.se_method!r}"
            )
        if self.sigma_method not in VALID_SIGMA_METHODS:
            raise ValueError(
                f"sigma_method must be one of {VALID_SIGMA_METHODS}, "
                f"got {self.sigma_method!r}"
            )
        if self.loss_alpha != 1.0:
            raise NotImplementedError(
                f"loss_alpha={self.loss_alpha} not yet implemented; "
                f"only alpha=1 is supported"
            )
        if self.premium_alpha != 1.0:
            raise NotImplementedError(
                f"premium_alpha={self.premium_alpha} not yet implemented; "
                f"only alpha=1 is supported"
            )
        if not (-1 < self.rho < 1):
            raise ValueError(f"rho must be in (-1, 1), got {self.rho!r}")
        if not (0.0 < self.conf_level < 1.0):
            raise ValueError(
                f"conf_level must be in (0, 1), got {self.conf_level!r}"
            )
        _validate_recent(self.recent)
        validate_tail(self.tail)

    def fit(self, triangle: "Triangle") -> "RatioFit":
        """Fit the Ratio estimator on a Triangle."""
        return RatioFit._from_triangle(triangle, self)


class RatioFit:
    """Result of a loss-ratio fit.

    Properties
    ----------
    df : DataFrame
        Long-format triangle with columns ``[groups?, cohort, dev,
        loss_obs, loss_proj, incr_loss_proj, premium_obs, premium_proj,
        incr_premium_proj, maturity_from, loss_*_se, loss_total_cv,
        loss_ci_*, ratio_proj, incr_ratio_proj, ratio_se, ratio_cv,
        ratio_ci_lo, ratio_ci_hi]`` (plus ``premium_total_se`` /
        ``premium_total_cv`` when ``se_method="delta"``).
    method : str
        ``"ed"``, ``"cl"``, or ``"sa"``.
    maturity_point :
        Detected maturity for ``"sa"`` (single value or dict per group).
    loss_fit, premium_fit :
        The embedded :class:`LossFit` and :class:`PremiumFit`.
    """

    def __init__(self) -> None:
        raise TypeError(
            "RatioFit is the result of `Ratio().fit(triangle)`, not a direct constructor."
        )

    @classmethod
    def _from_triangle(
        cls, triangle: "Triangle", estimator: "Ratio"
    ) -> "RatioFit":
        self = cls.__new__(cls)
        # Retained for `convergence()`, which re-drives backtests on the
        # source triangle with this fit's own estimator config.
        self._triangle = triangle
        self._estimator = estimator
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self._cohort = triangle._cohort
        self._dev = triangle._dev
        self.method = estimator.method
        self.loss_alpha = estimator.loss_alpha
        self.premium_alpha = estimator.premium_alpha
        self.se_method = estimator.se_method
        self.rho = estimator.rho
        self.recent = estimator.recent
        self.conf_level = estimator.conf_level
        self.tail = estimator.tail
        # Bootstrap slots default to the pure-analytical state.
        self.boots = None
        self.ci_type = "analytical"

        # 1) build premium fit first so the loss side can see it, allowing
        # premium_regime to apply independently of loss_regime.
        premium_fit = Premium(
            method=estimator.premium_method,
            alpha=estimator.premium_alpha,
            sigma_method=estimator.sigma_method,
            conf_level=estimator.conf_level,
            regime=estimator.premium_regime,
            recent=estimator.recent,
            tail=estimator.tail,
        ).fit(triangle)
        self.premium_fit = premium_fit

        # 2) delegate loss-side projection to Loss --------------------------
        # For `sa` / `ed` / `cl` the embedded premium_fit is reused.
        loss_kwargs: dict[str, Any] = dict(
            method=estimator.method,
            alpha=estimator.loss_alpha,
            sigma_method=estimator.sigma_method,
            conf_level=estimator.conf_level,
            regime=estimator.loss_regime,
            recent=estimator.recent,
            premium_fit=premium_fit,
            premium_method=estimator.premium_method,
            premium_alpha=estimator.premium_alpha,
            maturity=estimator.maturity,
            max_cv=estimator.max_cv,
            max_rse=estimator.max_rse,
            min_run=estimator.min_run,
        )
        # Forward the tail to the loss-side Loss (effective for every
        # loss method: cl / ed / sa).
        loss_kwargs["tail"] = estimator.tail
        loss_fit = Loss(**loss_kwargs).fit(triangle)
        self.loss_fit = loss_fit

        # Resolve the loss-side regime once (a Regime object passes through;
        # "auto" re-resolves) so `segment_summary` can split the ultimates by
        # regime segment using its change points.
        from .regime import _resolve_regime

        self._regime = _resolve_regime(estimator.loss_regime, triangle)

        full = loss_fit._df.clone()

        # 3) join premium SE columns for delta method ----------------------
        if estimator.se_method == "delta":
            pf_df = self.premium_fit._df
            keys = [*normalize_groups(self._groups), "cohort", "dev"]
            pf_keep = pf_df.select(
                keys + ["premium_total_se", "premium_total_cv"]
            )
            full = full.join(pf_keep, on=keys, how="left")

        # 4) Ratio point projection -------------------------------------------
        full = full.with_columns(
            pl.when(
                pl.col("loss_proj").is_not_null()
                & pl.col("premium_proj").is_not_null()
                & (pl.col("premium_proj") != 0.0)
            )
            .then(pl.col("loss_proj") / pl.col("premium_proj"))
            .otherwise(None)
            .alias("ratio_proj"),
            pl.when(
                pl.col("incr_loss_proj").is_not_null()
                & pl.col("incr_premium_proj").is_not_null()
                & (pl.col("incr_premium_proj") > 0.0)
            )
            .then(pl.col("incr_loss_proj") / pl.col("incr_premium_proj"))
            .otherwise(None)
            .alias("incr_ratio_proj"),
        )

        # 4b) Ratio tail = the tailed ultimate loss / tailed ultimate premium.
        # Both sides are developed with the same tail (loss runoff and the
        # cumulative-premium runoff), so ratio_tail = loss_proj / premium_proj
        # on the last-dev row -- consistent rather than freezing premium.
        if "loss_tail" in full.columns and "premium_tail" in full.columns:
            full = full.with_columns(
                pl.when(
                    pl.col("loss_tail").is_not_null()
                    & pl.col("premium_tail").is_not_null()
                    & (pl.col("premium_tail") != 0.0)
                )
                .then(pl.col("loss_tail") / pl.col("premium_tail"))
                .otherwise(None)
                .alias("ratio_tail")
            )

        # 5) ratio_se / ratio_cv / analytical CI ------------------------------
        full = _compose_ratio_stats(
            full, estimator.se_method, estimator.rho, estimator.conf_level
        )

        # 6) optional loss-side bootstrap overlay (strictly opt-in) -----------
        # With no bootstrap, `full` is the pure analytical ratio fit and is
        # left byte-unchanged. Otherwise the loss SE columns are overlaid
        # from a loss-side bootstrap and the ratio SE / CV / CI are
        # recomputed from the bootstrap-derived `loss_total_se`. Premium is
        # never bootstrapped (loss-only convention).
        full = self._maybe_overlay_bootstrap(full, triangle, estimator)

        self._df = full
        return self

    def _maybe_overlay_bootstrap(
        self,
        full: pl.DataFrame,
        triangle: "Triangle",
        estimator: "Ratio",
    ) -> pl.DataFrame:
        """Resolve + overlay a loss-side bootstrap onto the ratio ``$full``.

        No-op (returns ``full`` unchanged) when ``estimator.uncertainty``
        resolves to nothing (``None`` / :class:`~lossratio.Analytical` ->
        the closed-form analytical ratio SE stands). Otherwise resolves the
        uncertainty strategy to a loss-side bootstrap, overlays the
        bootstrap loss SE onto the projected cells, and recomputes
        ``ratio_se`` / ``ratio_cv`` / ``ratio_ci_lo`` / ``ratio_ci_hi``
        from the now-bootstrap ``loss_total_se``. The premium side is not
        bootstrapped. Sets :attr:`boots` / :attr:`ci_type`.
        """
        from .bootstrap import _apply_bootstrap_overlay
        from .uncertainty import resolve_uncertainty

        # The bootstrap SE overlay follows the loss `method`: an ED
        # headline gets an ED bootstrap, a CL headline a CL bootstrap, so
        # the predictive dispersion is centred on the *same* projection
        # that drives the point estimate on `$full` (no ED-point /
        # CL-band mismatch). The bootstrap paradigms honour the method
        # (ResidualBootstrap -> nonparametric; ParametricBootstrap ->
        # parametric). The one exception is Analytical(simulate=True) --
        # the simulated Mack factor draw, which is CL-only by nature and
        # stays a CL band regardless of the headline method. None /
        # Analytical -> no overlay (the closed-form analytical ratio SE,
        # itself method-consistent, stands).
        boots = resolve_uncertainty(
            estimator.uncertainty, triangle, target="loss",
            method=estimator.method,
        )
        if boots is None:
            return full

        groups = self._groups
        keys = [*normalize_groups(groups), "cohort", "dev"]
        full = _apply_bootstrap_overlay(
            full, boots,
            role    = "loss",
            se_cols = ["param_se", "proc_se", "total_se", "total_cv"],
            keys    = keys,
        )

        # Recompute ratio SE / CV / CI from the bootstrap-derived
        # `loss_total_se`. `ratio_proj` and observed cells are unchanged --
        # the overlay only touched projected-cell loss SE columns.
        full = _compose_ratio_stats(
            full, estimator.se_method, estimator.rho, estimator.conf_level
        )

        self.boots = boots
        self.ci_type = "bootstrap"
        return full

    @property
    def df(self):
        return mirror_output(self._df, self._output_type)

    @property
    def tail_report(self):
        """Combined provenance for the loss-ratio tail (both sides).

        Stacks the loss and premium tail provenance (``role`` column
        distinguishes them) so an auditor sees exactly how both the tailed
        ultimate loss and the tailed ultimate premium -- and hence
        ``ratio_tail`` -- were produced. Empty when no tail was requested.
        """
        from .tail import tail_report_frame

        loss_r = tail_report_frame(
            getattr(self.loss_fit, "_tail_results", {}),
            getattr(self.loss_fit, "tail", False),
            role="loss",
        )
        premium_r = tail_report_frame(
            getattr(self.premium_fit, "_tail_results", {}),
            getattr(self.premium_fit, "tail", False),
            role="premium",
        )
        combined = pl.concat([loss_r, premium_r]) if (loss_r.height or premium_r.height) else loss_r
        return mirror_output(combined, self._output_type)

    @property
    def maturity_point(self):
        """Detected maturity (delegated to LossFit)."""
        return self.loss_fit.maturity_point

    def convergence(self, **kwargs: Any) -> "Convergence":
        """Detect where this fit's projected loss ratio converges.

        Re-drives a calendar-diagonal hold-out backtest over candidate
        development periods on the source triangle, using this fit's own
        estimator config (pass ``estimator=`` to override), and flags the
        first dev at which the projected loss ratio stabilises (drift /
        slope / dispersion criteria). Returns a :class:`Convergence`.

        Extra keyword arguments (``method`` / ``max_drift`` / ``max_slope``
        / ``max_dispersion`` / ``window`` / ``maturity_point`` /
        ``holdout_max`` / ``min_n_cohorts``) are forwarded to the
        convergence detector.
        """
        from .convergence import detect_convergence

        kwargs.setdefault("estimator", self._estimator)
        return detect_convergence(self._triangle, **kwargs)

    def convergence_tail(self, **kwargs: Any) -> pl.DataFrame:
        """Convergence(k**)-anchored tail for the portfolio loss ratio.

        For long-term health the loss ratio rises then PLATEAUS -- the tail
        question is "has it reached its mature level?" not "develop the
        factors to ultimate". This gates the tail on :meth:`convergence`:

        * **Converged** (a k** is found in-window): the headline
          (``ratio_headline``) is ANCHORED to the observed converged level
          -- the stable backtest loss ratio -- not the factor-runoff
          extrapolation. The factor (CL/ED) tail rides along as
          ``ratio_factor_tail`` so any disagreement (the factor tail
          overshooting the observed plateau) is disclosed as the band.
        * **Immature** (no k** -- data ends while the ratio is still
          drifting): the factor tail is the only forward estimate, so it
          becomes the headline and ``status`` is ``"immature"`` -- the
          honest "this is assumption territory" signal. (The band is the
          spread of the two legs, not a total-uncertainty interval: when
          the factor tail is inert it can be narrow even while immature, so
          read ``status``, not band width, as the trust signal.)

        One portfolio-level row (k** is a book-level scalar). Extra keyword
        arguments are forwarded to :meth:`convergence`.

        Returns
        -------
        DataFrame
            One row: ``status`` (``"converged"`` / ``"immature"``),
            ``k_conv`` (k**, ``null`` when immature), ``ratio_latest`` (the
            stable / latest backtest loss ratio), ``ratio_factor_tail`` (the
            factor-runoff ultimate ratio), ``ratio_headline`` (the anchored
            point), and the band (``band_lo`` / ``band_hi`` /
            ``band_width``) = the spread of the two legs. Input mirroring is
            preserved.
        """
        from .convergence import convergence_tail_frame

        return convergence_tail_frame(self, **kwargs)

    def at_grain(self, target_grain: str) -> "FrameLike":
        """Display this fine-grain forecast aggregated to a COARSER grain.

        Re-bins the fitted PROJECTED triangle up to ``target_grain``
        (M -> Q / H / Y) by pure summation -- no re-fitting. The fine-grain
        forecast is reconstructed as per-cell incremental loss / premium
        DERIVED from the cumulative projection (``loss_proj`` /
        ``premium_proj``, which are complete on every fit path), re-binned
        through the :class:`Triangle` constructor at ``target_grain``, and
        the cumulative loss / premium / ratio are recomputed on the coarse
        cells. This keeps an M-grain and a Q-grain DISPLAY consistent:
        they are the same M forecast, just summed into coarser buckets.

        Returns :attr:`df` unchanged when ``target_grain`` equals the
        source grain. Raises if asked to REFINE (a coarser display cannot
        be un-aggregated to a finer one).

        Parameters
        ----------
        target_grain : str
            One of ``"M"`` / ``"Q"`` / ``"H"`` / ``"Y"``, at least as
            coarse as this fit's source grain.

        Returns
        -------
        DataFrame
            Long-format coarse-grain projection with columns
            ``[groups?, cohort, dev, loss_proj, incr_loss_proj,
            premium_proj, incr_premium_proj, ratio_proj, incr_ratio_proj]``.
            ``*_proj`` are CUMULATIVE; ``incr_*_proj`` are the per-period
            increments. Input mirroring is preserved.
        """
        from .regime import _GRAIN_MONTHS

        if target_grain not in _GRAIN_MONTHS:
            raise ValueError(f"unknown grain {target_grain!r}")
        src = self._triangle.grain
        if target_grain == src:
            return self.df
        if _GRAIN_MONTHS[target_grain] < _GRAIN_MONTHS[src]:
            raise ValueError(
                f"cannot refine grain {src!r} -> {target_grain!r} (coarsen only)"
            )

        mpp = _GRAIN_MONTHS[src]
        groups = normalize_groups(self._groups)
        # Per-cell projected increments derived from the CUMULATIVE
        # projection (`loss_proj` / `premium_proj`). The cumulative columns
        # are complete (no nulls) on every fit path, whereas the stored
        # `incr_*_proj` column is NULL at dev 1 on the regime
        # (segment_borrowed) path -- so re-binning from the cumulative is
        # the only source that captures the dev-1 mass for every fit. Per
        # (groups, cohort) sorted by dev, the increment is `<role>_proj -
        # <role>_proj.shift(1)` with dev0 = 0 (fill_value=0.0), so the first
        # increment = the first cumulative -- matching `_nan_skip_diff`
        # semantics used by the non-regime `_from_triangle` builder.
        # calendar of cell (cohort, dev) = cohort + (dev - 1) source periods.
        diff_keys = [*groups, "cohort"]
        recon = (
            self._df.sort([*diff_keys, "dev"])
            .with_columns(
                (
                    pl.col("loss_proj")
                    - pl.col("loss_proj").shift(1, fill_value=0.0).over(diff_keys)
                ).alias("incr_loss"),
                (
                    pl.col("premium_proj")
                    - pl.col("premium_proj")
                    .shift(1, fill_value=0.0)
                    .over(diff_keys)
                ).alias("incr_premium"),
            )
            .select(
                *groups,
                pl.col("cohort"),
                pl.col("cohort")
                .dt.offset_by(pl.format("{}mo", (pl.col("dev") - 1) * mpp))
                .alias("_calendar"),
                pl.col("incr_loss"),
                pl.col("incr_premium"),
            )
        )
        from .triangle import Triangle

        coarse = Triangle(
            recon,
            groups=self._groups,
            cohort="cohort",
            calendar="_calendar",
            loss="incr_loss",
            premium="incr_premium",
            grain=target_grain,
            cell_type="incremental",
        )
        keep = [*groups, "cohort", "dev"]
        out = coarse._df.select(
            *keep,
            pl.col("loss").alias("loss_proj"),
            pl.col("incr_loss").alias("incr_loss_proj"),
            pl.col("premium").alias("premium_proj"),
            pl.col("incr_premium").alias("incr_premium_proj"),
            pl.col("ratio").alias("ratio_proj"),
            pl.col("incr_ratio").alias("incr_ratio_proj"),
        )

        # 2) Fold any active tail mass into the coarse ultimate -----------
        # When a tail is active, the tail's extra ultimate mass lives as a
        # scalar companion column (`loss_tail` / `premium_tail`) on each
        # fine cohort's last-dev row -- NOT in `incr_*_proj`. The pure
        # re-binning above is therefore tail-blind. Fold the per-fine-cohort
        # tail mass (`<role>_tail - <role>_proj` at its last dev) into the
        # coarse cohort's last-dev cumulative so the coarse ultimate matches
        # the tail-inclusive fine ultimate. When no tail is active (columns
        # absent or all null), this is a no-op and `out` is byte-identical.
        out = self._fold_tail_into_coarse(out, target_grain, groups)

        return mirror_output(out, self._output_type)

    def _fold_tail_into_coarse(
        self,
        out: pl.DataFrame,
        target_grain: str,
        groups: list[str],
    ) -> pl.DataFrame:
        """Add per-coarse-cohort tail mass to the coarse last-dev ultimate.

        No-op (returns ``out`` unchanged) when no ``*_tail`` columns are
        present or every tail value is null.
        """
        from ._period import floor_to_period

        roles = [
            r
            for r in ("loss", "premium")
            if f"{r}_tail" in self._df.columns
        ]
        if not roles:
            return out

        # Per fine cohort, the tail mass = <role>_tail - <role>_proj on the
        # rows where <role>_tail is non-null (precisely the last-dev rows).
        tail_mass_exprs = [
            (pl.col(f"{r}_tail") - pl.col(f"{r}_proj")).alias(f"_{r}_tail_mass")
            for r in roles
        ]
        any_tail = pl.any_horizontal(
            *[pl.col(f"{r}_tail").is_not_null() for r in roles]
        )
        fine_mass = (
            self._df.filter(any_tail)
            .with_columns(*tail_mass_exprs)
            .select(
                *groups,
                floor_to_period(pl.col("cohort"), target_grain).alias("cohort"),
                *[pl.col(f"_{r}_tail_mass") for r in roles],
            )
        )
        if fine_mass.height == 0:
            return out

        keys = [*groups, "cohort"]
        coarse_mass = fine_mass.group_by(keys).agg(
            *[
                pl.col(f"_{r}_tail_mass").sum().alias(f"_{r}_tail_mass")
                for r in roles
            ]
        )

        # Identify each coarse cohort's last-dev row.
        marked = out.with_columns(
            pl.col("dev")
            .rank(method="dense", descending=True)
            .over(keys)
            .alias("_dev_rank")
        )
        merged = marked.join(coarse_mass, on=keys, how="left")
        is_last = pl.col("_dev_rank") == 1

        add_exprs: list[pl.Expr] = []
        for r in roles:
            mass = pl.col(f"_{r}_tail_mass").fill_null(0.0)
            add_exprs.append(
                pl.when(is_last)
                .then(pl.col(f"{r}_proj") + mass)
                .otherwise(pl.col(f"{r}_proj"))
                .alias(f"{r}_proj")
            )
            add_exprs.append(
                pl.when(is_last)
                .then(pl.col(f"incr_{r}_proj") + mass)
                .otherwise(pl.col(f"incr_{r}_proj"))
                .alias(f"incr_{r}_proj")
            )
        merged = merged.with_columns(*add_exprs)

        # Recompute the ratios on the (now tail-inclusive) coarse cells.
        merged = merged.with_columns(
            pl.when(
                pl.col("loss_proj").is_not_null()
                & pl.col("premium_proj").is_not_null()
                & (pl.col("premium_proj") != 0.0)
            )
            .then(pl.col("loss_proj") / pl.col("premium_proj"))
            .otherwise(None)
            .alias("ratio_proj"),
            pl.when(
                pl.col("incr_loss_proj").is_not_null()
                & pl.col("incr_premium_proj").is_not_null()
                & (pl.col("incr_premium_proj") > 0.0)
            )
            .then(pl.col("incr_loss_proj") / pl.col("incr_premium_proj"))
            .otherwise(None)
            .alias("incr_ratio_proj"),
        )

        drop_cols = ["_dev_rank", *[f"_{r}_tail_mass" for r in roles]]
        return merged.drop(drop_cols).select(out.columns)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def summary(self) -> pl.DataFrame:
        """Per-cohort ultimate loss, premium, and Ratio."""
        df = self._df
        keys: list[str] = [*normalize_groups(self._groups), "cohort"]

        # `latest` / `ratio_latest` are the last *observed* cumulative
        # loss and observed loss ratio (sorted by dev, not the dev
        # index).
        observed = (
            df.filter(pl.col("loss_obs").is_not_null())
            .sort(keys + ["dev"])
            .group_by(keys)
            .agg(
                pl.col("loss_obs").last().alias("latest"),
                pl.col("premium_obs").last().alias("_premium_latest"),
            )
        )

        # A fit can leave trailing null cells past a cohort's reach
        # (e.g. segment-bridged regimes), so "ultimate" must look at the
        # last non-null projection per cohort, not the last row.
        #
        # Tail cascade: when a tail is active the fit carries scalar
        # companion columns `loss_tail` / `premium_tail` on each cohort's
        # last-dev row (the tail-inclusive ultimate, vs `loss_proj` /
        # `premium_proj` = the within-triangle cum to dev_max). The headline
        # ULTIMATE must be tail-inclusive, so pull those through and fold
        # them below. With no tail (the default) `*_tail` is absent and the
        # behaviour is byte-identical to before.
        has_tail = "loss_tail" in df.columns and "premium_tail" in df.columns
        tail_agg = (
            [
                pl.col("loss_tail").drop_nulls().last().alias("loss_tail"),
                pl.col("premium_tail").drop_nulls().last().alias("premium_tail"),
            ]
            if has_tail
            else []
        )
        ultimate = (
            df.sort(keys + ["dev"])
            .group_by(keys)
            .agg(
                pl.col("loss_proj").drop_nulls().last().alias("loss_proj"),
                pl.col("premium_proj").drop_nulls().last().alias("premium_proj"),
                pl.col("ratio_proj").drop_nulls().last().alias("ratio_proj"),
                *tail_agg,
                # The columns below keep their own name -- last() preserves
                # it, so no .alias() is needed.
                pl.col("maturity_from").drop_nulls().last(),
                pl.col("loss_proc_se").drop_nulls().last(),
                pl.col("loss_param_se").drop_nulls().last(),
                pl.col("loss_total_se").drop_nulls().last(),
                pl.col("loss_total_cv").drop_nulls().last(),
                pl.col("ratio_se").drop_nulls().last(),
                pl.col("ratio_cv").drop_nulls().last(),
                pl.col("ratio_ci_lo").drop_nulls().last(),
                pl.col("ratio_ci_hi").drop_nulls().last(),
            )
        )

        # Fold the tail into the headline ultimate. Match the RatioFit /
        # convergence tail composition: tail BOTH sides together or NEITHER
        # -- never a tailed numerator over an untailed denominator. Where
        # `*_tail` is missing for a cohort, fall both back to the untailed
        # projection (exactly the `ratio_tail`-null case). `ratio_proj` is
        # recomputed from the folded ultimate.
        if has_tail:
            both = (
                pl.col("loss_tail").is_not_null()
                & pl.col("premium_tail").is_not_null()
                & (pl.col("premium_tail") != 0.0)
            )
            ultimate = ultimate.with_columns(
                pl.when(both)
                .then(pl.col("loss_tail"))
                .otherwise(pl.col("loss_proj"))
                .alias("loss_proj"),
                pl.when(both)
                .then(pl.col("premium_tail"))
                .otherwise(pl.col("premium_proj"))
                .alias("premium_proj"),
            ).with_columns(
                pl.when(
                    pl.col("loss_proj").is_not_null()
                    & pl.col("premium_proj").is_not_null()
                    & (pl.col("premium_proj") != 0.0)
                )
                .then(pl.col("loss_proj") / pl.col("premium_proj"))
                .otherwise(pl.col("ratio_proj"))
                .alias("ratio_proj"),
            ).drop("loss_tail", "premium_tail")

        out = observed.join(ultimate, on=keys, how="inner")

        # `reserve` = ultimate projected loss - last observed cumulative
        # loss; `ratio_latest` = last observed loss / premium (guarded).
        out = out.with_columns(
            (pl.col("loss_proj") - pl.col("latest")).alias("reserve"),
            pl.when(
                pl.col("_premium_latest").is_not_null()
                & (pl.col("_premium_latest") != 0.0)
            )
            .then(pl.col("latest") / pl.col("_premium_latest"))
            .otherwise(None)
            .alias("ratio_latest"),
        ).drop("_premium_latest")

        out = out.select(
            keys
            + [
                "latest",
                "loss_proj",
                "reserve",
                "premium_proj",
                "ratio_latest",
                "ratio_proj",
                "maturity_from",
                "loss_proc_se",
                "loss_param_se",
                "loss_total_se",
                "loss_total_cv",
                "ratio_se",
                "ratio_cv",
                "ratio_ci_lo",
                "ratio_ci_hi",
            ]
        ).sort(keys)
        return mirror_output(out, self._output_type)

    def segment_summary(self) -> pl.DataFrame:
        """Ultimate loss ratio per regime SEGMENT, plus a total row.

        Splits :meth:`summary`'s per-cohort ultimates by the loss regime's
        change points and aggregates ``loss_proj`` / ``premium_proj`` ->
        ``ratio_proj`` per segment (oldest = segment ``"0"``), with a
        ``"total"`` row per group. So one fit yields the go-forward LR of
        the most recent segment, the legacy run-off LR, and the whole-book
        ratio at once -- different questions, one table. With no regime the
        result is the single ``"total"`` row per group.

        Columns: groups, ``segment`` (``"0"``, ``"1"``, ... / ``"total"``),
        ``change_from`` (the date that segment starts; null for segment 0
        and total), ``n_cohorts``, ``loss_proj``, ``premium_proj``,
        ``ratio_proj``.
        """
        s = self.summary()
        s = s if isinstance(s, pl.DataFrame) else pl.from_pandas(s)
        s = s.with_columns(pl.col("cohort").cast(pl.Date))
        gcols = normalize_groups(self._groups)
        reg = self._regime
        changes = (
            reg._changes_df
            if reg is not None and not reg._changes_df.is_empty()
            else None
        )

        parts = (
            list(s.partition_by(gcols, as_dict=True).items())
            if gcols
            else [((), s)]
        )
        rows: list[dict] = []
        for key, gs in parts:
            keyvals = key if isinstance(key, tuple) else (key,)
            change_dates: list = []
            if changes is not None:
                chg = changes
                for c, v in zip(gcols, keyvals):
                    if c in chg.columns:
                        chg = chg.filter(pl.col(c) == v)
                change_dates = sorted(chg["change"].to_list())

            from ._band import _assign_segments

            cohorts = gs["cohort"].to_list()
            seg = _assign_segments(cohorts, change_dates)
            gs = gs.with_columns(pl.Series("_segment", seg, dtype=pl.Int64))

            def _block(sub, label, change_from):
                lo = float(sub["loss_proj"].sum())
                pr = float(sub["premium_proj"].sum())
                row = {c: v for c, v in zip(gcols, keyvals)}
                row.update(
                    segment=label,
                    change_from=change_from,
                    n_cohorts=int(sub.height),
                    loss_proj=lo,
                    premium_proj=pr,
                    ratio_proj=(lo / pr) if pr else float("nan"),
                )
                return row

            for seg_id in sorted(set(seg)):
                cf = change_dates[seg_id - 1] if seg_id >= 1 else None
                rows.append(
                    _block(gs.filter(pl.col("_segment") == seg_id), str(seg_id), cf)
                )
            rows.append(_block(gs, "total", None))

        return mirror_output(pl.DataFrame(rows), self._output_type)

    def segment_band(
        self,
        *,
        curve: "Curve | None" = None,
        tol: float = 1e-4,
        auto_grain: bool = False,
    ) -> pl.DataFrame:
        """Honest go-forward band for the recent regime segment's tail.

        The recent segment's unobserved tail is projected two independent
        ways. The *borrow leg* (read verbatim from
        :meth:`segment_summary`) borrows the donor segment's development
        shape; the *curve leg* extrapolates the recent segment's OWN
        intensity ``g_k`` with a parametric :class:`~lossratio.Curve`. The
        SPREAD between the two ultimate loss ratios is the band: narrow
        when the legs agree (a mature recent segment, trustworthy), wide
        when they diverge (a young segment, assumption territory).

        One row per group that has a regime change, about the newest
        segment only (latest-only treatment). Groups without a change
        produce no row; with no regime at all the result is an empty,
        correctly-typed frame (never a whole-book fabrication).

        The method is fully additive: it reads :meth:`segment_summary`
        and the fit frame, never writes back, and does not change any
        existing output. A run that never calls it is byte-identical.

        Parameters
        ----------
        curve
            Curve spec for the curve leg. ``None`` (default) uses
            ``Curve(target="intensity", family="inverse_power",
            min_points=3)``. A spec with ``target != "intensity"`` raises
            ``ValueError`` (the band formula is ``g_k``-based).
        tol
            Reserved for forward-compatibility with the curve contract.
            Currently inert: the per-position evaluation never truncates
            the premium-weighted tail sum.
        auto_grain
            When ``False`` (default) the band is computed at the fit's
            display grain only, byte-identical to prior behaviour. When
            ``True`` the tail grain is auto-selected per group by
            coarsening ``M -> Q -> H -> Y`` and stopping at the finest
            grain where both signals fire -- the curve slope is past the
            convergence boundary (physical, not divergent) AND the borrow
            and curve legs agree (the band narrows). At a fresh regime the
            display grain over-extrapolates an unphysical tail; a coarser,
            more mature grain recovers a convergent slope and a real,
            narrow band. The result then carries a ``selected_grain``
            column and ``band_status`` can read ``"insufficient"`` when no
            grain converges (a borrow-only fallback). The observed segment
            loss ratio is grain-invariant, so the auto-grain mode changes
            only the tail; it never re-bins the displayed observation.

        Returns
        -------
        DataFrame
            One row per regime group with the borrow / curve ultimate loss
            ratios, the band (``band_lo`` / ``band_hi`` / ``band_width`` /
            ``band_status``), and the curve fit's provenance
            (``curve_n_points`` / ``curve_under_determined`` /
            ``curve_reason`` / ``curve_diverged`` /
            ``curve_alt_ratio_proj``). Curve-leg columns are null on a
            degenerate fit (``band_status="degenerate"``), with the borrow
            leg always preserved. With ``auto_grain=True`` the frame is a
            strict superset: it adds ``selected_grain`` (the fired grain,
            ``null`` when no grain converged) and ``band_status`` may also
            read ``"insufficient"``.

        See Also
        --------
        segment_path : the dev-by-dev developing trajectory of the same two
            legs (and the band around them), for charting rather than just the
            ultimate headline.
        """
        if not auto_grain:
            from ._band import _segment_band

            return _segment_band(self, curve=curve, tol=tol)

        from ._band import _segment_band_auto

        return _segment_band_auto(self, curve=curve, tol=tol)

    def segment_path(
        self,
        *,
        curve: "Curve | None" = None,
        tol: float = 1e-4,
        auto_grain: bool = False,
    ) -> pl.DataFrame:
        """Developing path of the recent segment's go-forward loss ratio.

        The dev-by-dev companion to :meth:`segment_band`. Where
        ``segment_band`` reports the recent segment's ULTIMATE loss ratio two
        ways (the donor-shape *borrow leg* and the own-intensity *curve leg*)
        and their spread, this returns the same two legs AS A DEVELOPING PATH:
        the recent-segment aggregate cumulative loss ratio at each development
        period, observed part plus projected tail, so a chart can draw the
        trajectory (solid where observed, dashed in the tail) with the band
        shaded around it.

        One block of rows per group that has a regime change, about the newest
        segment only (latest-only treatment, matching ``segment_band``).
        Groups without a change produce no rows; with no regime at all the
        result is an empty, correctly-typed frame.

        The observed region is always at the fit's display grain -- it keeps
        the real period-to-period dynamics and is grain-invariant. There the
        path shows the single data-anchored borrow trajectory with the band
        collapsed onto it; the band fans open at the dev where the segment as a
        whole runs out of data (so a chart reads as a solid certain line that
        opens into a shaded tail). By construction the last (ultimate) row of
        each leg equals ``segment_band``'s reported ultimate for that leg
        (``ratio_borrow`` <-> ``ratio_proj_borrow``, ``ratio_curve`` <->
        ``ratio_proj_curve``, ``ratio_mean`` <-> ``ratio_proj_mean``). With
        ``auto_grain=True`` the horizon is the selected coarse grain's
        ultimate, so the dev index can run a few display periods past the fit's
        own projection grid to reach that grain's fully-developed point.

        The method is fully additive: it reads the fit and re-fits coarsened
        copies (auto mode) but never writes back, so a run that never calls it
        is byte-identical.

        Parameters
        ----------
        curve
            Curve spec for the curve leg. ``None`` (default) uses
            ``Curve(target="intensity", family="inverse_power", min_points=3)``.
            A spec with ``target != "intensity"`` raises ``ValueError``.
        tol
            Reserved for forward-compatibility with the curve contract;
            currently inert (the tail sum never truncates).
        auto_grain
            When ``False`` (default) the whole path -- observed region and
            tail -- is at the fit's display grain. When ``True`` the
            unobserved tail is extrapolated at the per-group selected coarse
            grain (the same grain :meth:`segment_band` would pick) and then
            INTERPOLATED back onto the display-grain dev positions, so a
            fresh regime's tail is drawn from a mature, convergent grain
            without re-binning the observed period-to-period detail.
            Interpolation introduces no new extrapolation error. The result
            then carries a ``selected_grain`` column; when no grain converges
            the path falls back to the borrow leg only (curve / band columns
            null).

        Returns
        -------
        DataFrame
            One row per ``group x dev`` of the recent segment, with columns
            ``dev``, ``ratio_borrow``, ``ratio_curve`` (null when the curve
            leg is unavailable), ``ratio_mean``, ``band_lo`` / ``band_hi``
            (the two-leg spread; null in the observed region's collapsed band
            only when the curve is unavailable), and ``observed`` (``True``
            where real data backs the period). With ``auto_grain=True`` the
            frame adds ``selected_grain`` (``null`` on a borrow-only
            fallback).
        """
        from ._band import _segment_path

        return _segment_path(
            self, curve=curve, tol=tol, auto_grain=auto_grain
        )

    def ultimate_ratio_samples(self, *, per_group: bool = False):
        """Per-replicate portfolio ultimate loss ratio (predictive draws).

        For each bootstrap replicate, ``(sum of cohort ultimate sampled
        loss) / (sum of cohort ultimate premium)``. Premium is fixed
        (loss-only bootstrap), so only the numerator varies across
        replicates. Use for an ultimate loss-ratio predictive histogram
        (e.g. p50 / p75 / p95).

        Returns ``None`` when the fit carries no sampling bootstrap
        (``uncertainty=None`` / :class:`~lossratio.Analytical` -- the
        closed-form ratio CI band stands; there are no draws to bin).

        Parameters
        ----------
        per_group
            ``False`` (default) pools all groups into one portfolio ratio
            per replicate and returns a ``(n_replicates,)`` numpy array.
            ``True`` returns a long DataFrame
            ``[groups?, rep, ratio_proj_sampled]`` (per-group ratio), in
            the fit's input format.

        Notes
        -----
        * The numerator is *pre-tail* -- the bootstrap develops to the
          last observed dev, not through the deterministic tail factor;
          and the loss-side overlay is the CL (Mack) bootstrap regardless
          of the loss ``method``, so the distribution is CL-centred even
          when the headline point estimate is ED / SA.
        * In the pooled (``per_group=False``) case groups are summed by
          replicate index; the per-group bootstrap streams are treated as
          independent (no cross-group correlation), matching the
          bootstrap's own assumption.
        """
        boots = self.boots
        samples = (
            None if boots is None
            else getattr(boots, "_ultimate_samples", None)
        )
        if samples is None:
            return None

        gcols = normalize_groups(self._groups)

        # Portfolio ultimate premium per group -- fixed across replicates
        # (loss-only bootstrap). Last non-null projection per cohort,
        # summed over cohorts (mirrors `summary`'s ultimate aggregation).
        premium_coh = (
            self._df.sort([*gcols, "cohort", "dev"])
            .group_by([*gcols, "cohort"], maintain_order=True)
            .agg(pl.col("premium_proj").drop_nulls().last().alias("_p"))
        )

        if per_group:
            premium_grp = (
                premium_coh.group_by(gcols).agg(pl.col("_p").sum().alias("premium_proj"))
                if gcols
                else premium_coh.select(pl.col("_p").sum().alias("premium_proj"))
            )
            joined = (
                samples.join(premium_grp, on=gcols, how="inner")
                if gcols
                else samples.join(premium_grp, how="cross")
            )
            out = (
                joined.with_columns(
                    (pl.col("loss_proj_sampled") / pl.col("premium_proj"))
                    .alias("ratio_proj_sampled")
                )
                .select([*gcols, "rep", "ratio_proj_sampled"])
                .sort([*gcols, "rep"])
            )
            return mirror_output(out, self._output_type)

        # Pooled portfolio: sum loss over groups per replicate, divide by
        # the total ultimate premium.
        total_premium = premium_coh.select(pl.col("_p").sum()).item()
        loss_per_rep = (
            samples.group_by("rep")
            .agg(pl.col("loss_proj_sampled").sum().alias("loss_proj"))
            .sort("rep")
        )
        ratio = loss_per_rep.with_columns(
            (pl.col("loss_proj") / total_premium).alias("ratio_proj_sampled")
        )
        return ratio.get_column("ratio_proj_sampled").to_numpy()

    @property
    def n_rows(self) -> int:
        return self._df.height

    def plot(
        self,
        metric: str = "ratio",
        cell_type: str = "cumulative",
        per_group: bool | None = None,
        conf_level: float | None = None,
        show_interval: bool = True,
        amount_divisor: float | str = "auto",
        nrow: int | None = None,
        ncol: int | None = None,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """Projection-curve plot, backed by matplotlib.

        Parameters
        ----------
        metric
            ``"ratio"`` (default), ``"loss"``, or ``"premium"``.
        cell_type
            ``"cumulative"`` (default) or ``"incremental"``. Confidence
            bands are drawn only for cumulative metrics.
        per_group
            When ``True`` (auto for multi-group fits), produce one
            figure per group and return ``list[Figure]``. When ``False``
            (auto for single-group fits), facets every
            ``(group, cohort)`` pair in a single figure.
        conf_level
            Override the fit's stored ``conf_level``. Used only for the
            caption label; CI columns were computed at fit time.
        show_interval
            Draw a confidence ribbon on projected cells. No-op when the
            requested metric has no CI columns (incremental metrics
            always; premium-side CI only exists when the Ratio was fit
            with ``se_method="delta"``).
        amount_divisor
            ``"auto"`` (default) picks the largest divisor in
            ``{1, 1e3, 1e6, 1e9, 1e12}`` such that the median plotted
            value formats as a non-zero label at ``%.1f``. Ignored for
            ratio metrics.
        nrow, ncol
            Facet layout. Defaults to a near-square grid.
        figsize
            Passed to ``plt.subplots``. Defaults to a size scaled by
            facet count.

        Returns
        -------
        matplotlib.figure.Figure
            Single figure for the combined-facet form.
        list of matplotlib.figure.Figure
            One figure per group when ``per_group=True``.
        """
        from ._ratio_vis import plot_ratio_fit
        return plot_ratio_fit(
            self,
            metric=metric,
            cell_type=cell_type,
            per_group=per_group,
            conf_level=conf_level,
            show_interval=show_interval,
            amount_divisor=amount_divisor,
            nrow=nrow,
            ncol=ncol,
            figsize=figsize,
        )

    def __repr__(self) -> str:
        n_rows = self._df.height
        if self._groups is not None:
            n_groups = (
                self._df.select(normalize_groups(self._groups)).unique().height
            )
            return (
                f"<RatioFit(method={self.method!r}): "
                f"{n_groups} groups, {n_rows} rows>"
            )
        return f"<RatioFit(method={self.method!r}): {n_rows} rows>"
