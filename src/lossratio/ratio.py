"""Loss-ratio (Ratio) composition layer.

:class:`Ratio` is the composition layer over :class:`Loss` and
:class:`Premium`. It delegates loss-side projection to :class:`Loss`,
retrieves the embedded :class:`PremiumFit`, and composes the loss-ratio
point + variance via the delta method.

Python sibling of R ``fit_ratio()`` (see ``R/lr.R``).
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from ._io import mirror_output
from ._sigma import VALID_SIGMA_METHODS
from .loss import Loss, LossFit
from .premium import Premium, PremiumFit

if TYPE_CHECKING:
    from .triangle import Triangle


_VALID_METHODS = ("sa", "ed", "cl")
_VALID_PREMIUM_METHODS = ("cl", "ed")
_VALID_SE_METHODS = ("fixed", "delta")


class Ratio:
    """Loss-ratio estimator (composition over :class:`Loss` + :class:`Premium`).

    Parameters
    ----------
    method
        Loss projection method: ``"sa"`` (default), ``"ed"``, or
        ``"cl"``. See :class:`Loss`.
    loss_alpha
        Variance-structure exponent for the loss fit. Default ``1``.
    loss_regime
        Loss-side regime filter. Four-type dispatch: ``None`` (no
        filter), :class:`Regime` (eager), ``"auto"`` (auto-detect on
        the fit triangle), or a callable ``f(tri) -> Regime`` (lazy
        spec — re-detected per backtest fold). Cohorts strictly
        before the latest change date are dropped from the loss fit.
    premium_method
        One of ``"cl"`` (default) or ``"ed"``. Forwarded to
        :class:`Premium`.
    premium_alpha
        Variance-structure exponent for the premium fit. Default ``1``.
    premium_regime
        Premium-side regime filter. Same four-type dispatch as
        ``loss_regime``. Defaults to ``None`` (no filter); pass an
        independent value when the underwriting / premium regime
        differs from the loss regime.
    sigma_method
        ``"locf"`` (default), ``"min_last2"``, or ``"loglinear"``.
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
    bootstrap
        Optional bootstrap specification. Bootstrap is strictly opt-in:
        when ``None`` / ``False`` (the default) the fit is the pure
        analytical ratio SE, byte-unchanged. When supplied, a loss-side
        bootstrap is run (premium is *not* bootstrapped) and the
        bootstrap-derived ``loss_total_se`` is overlaid onto the
        *projected* cells of ``$full``; ``ratio_se`` / ``ratio_cv`` /
        ``ratio_ci_*`` are then recomputed from it. The bootstrap runs
        with the Ratio's own loss ``method`` paradigm -- ``method="ed"``
        bootstraps with ED, ``"sa"`` with SA, ``"cl"`` with CL. Accepted
        forms:

        * ``True`` / ``"auto"`` -- a default :class:`Bootstrap` config
          inheriting this estimator's loss ``method``.
        * a :class:`Bootstrap` config instance -- its own settings win.
        * a pre-built loss-side :class:`BootstrapTriangle`.
        * a callable ``f(triangle) -> BootstrapTriangle``.
    B
        Integer number of bootstrap replicates. Used only when
        ``bootstrap`` resolves to ``"auto"``. Default ``999``.
    seed
        Optional integer seed for a reproducible bootstrap.
    """

    def __init__(
        self,
        method: str = "sa",
        loss_alpha: float = 1.0,
        loss_regime: Any = None,
        premium_method: str = "cl",
        premium_alpha: float = 1.0,
        premium_regime: Any = None,
        sigma_method: str = "locf",
        max_cv: float = 0.15,
        max_rse: float = 0.05,
        min_run: int = 2,
        se_method: str = "fixed",
        rho: float = 0.95,
        conf_level: float = 0.95,
        bootstrap: Any = None,
        B: int = 999,
        seed: int | None = None,
        # backwards-compat alias for loss_alpha (pre-Phase-5 callers)
        alpha: float | None = None,
    ) -> None:
        if method not in _VALID_METHODS:
            raise ValueError(
                f"method must be one of {_VALID_METHODS}, got {method!r}"
            )
        if premium_method not in _VALID_PREMIUM_METHODS:
            raise ValueError(
                f"premium_method must be one of {_VALID_PREMIUM_METHODS}, "
                f"got {premium_method!r}"
            )
        if se_method not in _VALID_SE_METHODS:
            raise ValueError(
                f"se_method must be one of {_VALID_SE_METHODS}, "
                f"got {se_method!r}"
            )
        if sigma_method not in VALID_SIGMA_METHODS:
            raise ValueError(
                f"sigma_method must be one of {VALID_SIGMA_METHODS}, "
                f"got {sigma_method!r}"
            )
        if alpha is not None:
            # legacy callers: alpha == loss_alpha
            loss_alpha = float(alpha)
        if loss_alpha != 1.0:
            raise NotImplementedError(
                f"loss_alpha={loss_alpha} not yet implemented; "
                f"only alpha=1 is supported"
            )
        if premium_alpha != 1.0:
            raise NotImplementedError(
                f"premium_alpha={premium_alpha} not yet implemented; "
                f"only alpha=1 is supported"
            )
        if not (-1 < rho < 1):
            raise ValueError(f"rho must be in (-1, 1), got {rho!r}")
        if not (0.0 < conf_level < 1.0):
            raise ValueError(
                f"conf_level must be in (0, 1), got {conf_level!r}"
            )
        if not isinstance(B, int) or B < 1:
            raise ValueError(f"B must be a positive integer, got {B!r}")

        self.method = method
        self.loss_alpha = loss_alpha
        self.loss_regime = loss_regime
        self.premium_method = premium_method
        self.premium_alpha = premium_alpha
        self.premium_regime = premium_regime
        self.sigma_method = sigma_method
        self.max_cv = max_cv
        self.max_rse = max_rse
        self.min_run = min_run
        self.se_method = se_method
        self.rho = rho
        self.conf_level = conf_level
        self.bootstrap = bootstrap
        self.B = B
        self.seed = seed

    # alias for backwards compatibility with code reading `.alpha`
    @property
    def alpha(self) -> float:
        return self.loss_alpha

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
        ``"sa"``, ``"ed"``, or ``"cl"``.
    mat_k :
        Detected maturity for ``"sa"`` (single value or dict per group).
    loss_fit, premium_fit :
        The embedded :class:`LossFit` and :class:`PremiumFit`.
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._output_type: str
        self._groups: str | None
        self._cohort: str
        self._dev: str
        self.method: str
        self.loss_alpha: float
        self.premium_alpha: float
        self.se_method: str
        self.rho: float
        self.conf_level: float
        self.loss_fit: LossFit
        self.premium_fit: PremiumFit
        # Bootstrap slots -- ci_type is "analytical" unless a bootstrap ran.
        self.boots: Any = None
        self.ci_type: str = "analytical"

    @classmethod
    def _from_triangle(
        cls, triangle: "Triangle", estimator: "Ratio"
    ) -> "RatioFit":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self._cohort = triangle._cohort
        self._dev = triangle._dev
        self.method = estimator.method
        self.loss_alpha = estimator.loss_alpha
        self.premium_alpha = estimator.premium_alpha
        self.se_method = estimator.se_method
        self.rho = estimator.rho
        self.conf_level = estimator.conf_level
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
        ).fit(triangle)
        self.premium_fit = premium_fit

        # 2) delegate loss-side projection to Loss --------------------------
        loss_fit = Loss(
            method=estimator.method,
            alpha=estimator.loss_alpha,
            sigma_method=estimator.sigma_method,
            premium_fit=premium_fit,
            premium_method=estimator.premium_method,
            premium_alpha=estimator.premium_alpha,
            max_cv=estimator.max_cv,
            max_rse=estimator.max_rse,
            min_run=estimator.min_run,
            conf_level=estimator.conf_level,
            regime=estimator.loss_regime,
        ).fit(triangle)
        self.loss_fit = loss_fit

        full = loss_fit._df.clone()

        # 2) join premium SE columns for delta method ----------------------
        if estimator.se_method == "delta":
            pf_df = self.premium_fit._df
            keys = []
            if self._groups is not None:
                keys.append(self._groups)
            keys += ["cohort", "dev"]
            pf_keep = pf_df.select(
                keys + ["premium_total_se", "premium_total_cv"]
            )
            full = full.join(pf_keep, on=keys, how="left")

        # 3) Ratio point projection -------------------------------------------
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

        # 4) ratio_se via fixed-premium or delta method -----------------------
        if estimator.se_method == "fixed":
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
            rho = estimator.rho
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

        # 5) ratio_cv + analytical CI -----------------------------------------
        z_alpha = float(norm.ppf((1 + estimator.conf_level) / 2))
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

        No-op (returns ``full`` unchanged) when ``estimator.bootstrap`` is
        ``None`` / ``False``. Otherwise resolves a *loss-side* bootstrap
        with the Ratio's own loss ``method`` paradigm (``cl`` -> analytical
        Mack, ``ed`` / ``sa`` -> the positivity-preserving parametric
        paradigm; ``sa`` also threads the detected per-group maturity),
        overlays the bootstrap loss SE onto the projected cells, and then
        recomputes ``ratio_se`` / ``ratio_cv`` / ``ratio_ci_lo`` /
        ``ratio_ci_hi`` from the now-bootstrap ``loss_total_se``. The
        premium side is not bootstrapped. Sets :attr:`boots` /
        :attr:`ci_type`.
        """
        bootstrap = estimator.bootstrap
        if bootstrap is None or bootstrap is False:
            return full

        from .bootstrap import _apply_bootstrap_overlay, _resolve_bootstrap

        # Default Bootstrap kwargs for the True / "auto" form. The Ratio
        # composition layer always bootstraps loss with the analytical
        # CL (Mack closed-form) paradigm -- `type="analytical"`,
        # `process="normal"` -- regardless of the Ratio's loss `method`.
        # This mirrors R `fit_ratio()` (`R/ratio.R`), whose wrap-only
        # bootstrap path is hard-wired to `type = "analytical"` /
        # `process = "normal"`. The loss `method` still drives the
        # *point* projection on `$full`; only the SE overlay is the
        # analytical bootstrap.
        kw: dict[str, Any] = {
            "method":  "cl",
            "type":    "analytical",
            "process": "normal",
            "B":       estimator.B,
            "seed":    estimator.seed,
        }

        boots = _resolve_bootstrap(
            bootstrap, triangle,
            target      = "loss",
            quantile_ci = True,
            keep_pseudo = False,
            **kw,
        )
        if boots is None:
            return full

        groups = self._groups
        keys = ([groups] if groups is not None else []) + ["cohort", "dev"]
        full = _apply_bootstrap_overlay(
            full, boots,
            role    = "loss",
            se_cols = ["param_se", "proc_se", "total_se", "total_cv"],
            keys    = keys,
        )

        # Recompute ratio SE / CV / CI from the bootstrap-derived
        # `loss_total_se`. `ratio_proj` and observed cells are unchanged --
        # the overlay only touched projected-cell loss SE columns.
        z_alpha = float(norm.ppf((1 + estimator.conf_level) / 2))
        if estimator.se_method == "fixed":
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
            rho = estimator.rho
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
                pl.col("ratio_proj").is_not_null()
                & pl.col("ratio_se").is_not_null()
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
                pl.col("ratio_proj").is_not_null()
                & pl.col("ratio_se").is_not_null()
            )
            .then(pl.col("ratio_proj") + z_alpha * pl.col("ratio_se"))
            .otherwise(None)
            .alias("ratio_ci_hi"),
        )

        self.boots = boots
        self.ci_type = "bootstrap"
        return full

    @property
    def df(self):
        return mirror_output(self._df, self._output_type)

    @property
    def mat_k(self):
        """Detected maturity (delegated to LossFit)."""
        return self.loss_fit.mat_k

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def summary(self) -> pl.DataFrame:
        """Per-cohort ultimate loss, premium, and Ratio."""
        df = self._df
        keys: list[str] = []
        if self._groups is not None:
            keys.append(self._groups)
        keys.append("cohort")

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

        # segment_wise fits produce trailing null cells past each
        # segment's reach (R parity), so "ultimate" must look at the
        # last non-null projection per cohort, not the last row.
        ultimate = (
            df.sort(keys + ["dev"])
            .group_by(keys)
            .agg(
                pl.col("loss_proj").drop_nulls().last().alias("loss_ult"),
                pl.col("premium_proj").drop_nulls().last().alias("premium_ult"),
                pl.col("ratio_proj").drop_nulls().last().alias("ratio_ult"),
                pl.col("maturity_from").drop_nulls().last().alias("maturity_from"),
                pl.col("loss_proc_se").drop_nulls().last().alias("loss_proc_se"),
                pl.col("loss_param_se").drop_nulls().last().alias("loss_param_se"),
                pl.col("loss_total_se").drop_nulls().last().alias("loss_total_se"),
                pl.col("loss_total_cv").drop_nulls().last().alias("loss_total_cv"),
                pl.col("ratio_se").drop_nulls().last().alias("ratio_se"),
                pl.col("ratio_cv").drop_nulls().last().alias("ratio_cv"),
                pl.col("ratio_ci_lo").drop_nulls().last().alias("ratio_ci_lo"),
                pl.col("ratio_ci_hi").drop_nulls().last().alias("ratio_ci_hi"),
            )
        )

        out = observed.join(ultimate, on=keys, how="inner")

        # `reserve` = ultimate projected loss - last observed cumulative
        # loss; `ratio_latest` = last observed loss / premium (guarded).
        out = out.with_columns(
            (pl.col("loss_ult") - pl.col("latest")).alias("reserve"),
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
                "loss_ult",
                "reserve",
                "premium_ult",
                "ratio_latest",
                "ratio_ult",
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

    @property
    def n_rows(self) -> int:
        return self._df.height

    def __repr__(self) -> str:
        n_rows = self._df.height
        if self._groups is not None:
            n_groups = self._df[self._groups].n_unique()
            return (
                f"<RatioFit(method={self.method!r}): "
                f"{n_groups} groups, {n_rows} rows>"
            )
        return f"<RatioFit(method={self.method!r}): {n_rows} rows>"
