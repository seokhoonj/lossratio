"""Loss-ratio (LR) composition layer.

:class:`LR` is the composition layer over :class:`Loss` and
:class:`Premium`. It delegates loss-side projection to :class:`Loss`,
retrieves the embedded :class:`PremiumFit`, and composes the loss-ratio
point + variance via the delta method.

Python sibling of R ``fit_lr()`` (see ``R/lr.R``).
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl
from scipy.stats import norm

from ._io import mirror_output
from ._sigma import VALID_SIGMA_METHODS
from .loss import Loss, LossFit
from .premium import PremiumFit

if TYPE_CHECKING:
    from .triangle import Triangle


_VALID_METHODS = ("sa", "ed", "cl")
_VALID_PREMIUM_METHODS = ("cl", "ed")
_VALID_SE_METHODS = ("fixed", "delta")


class LR:
    """Loss-ratio estimator (composition over :class:`Loss` + :class:`Premium`).

    Parameters
    ----------
    method
        Loss projection method: ``"sa"`` (default), ``"ed"``, or
        ``"cl"``. See :class:`Loss`.
    loss_alpha
        Variance-structure exponent for the loss fit. Default ``1``.
    loss_regime_break
        Loss-side regime break (not yet implemented in Python).
    premium_method
        One of ``"cl"`` (default) or ``"ed"``. Forwarded to
        :class:`Premium`.
    premium_alpha
        Variance-structure exponent for the premium fit. Default ``1``.
    premium_regime_break
        Premium-side regime break (not yet implemented in Python).
    sigma_method
        ``"locf"`` (default), ``"min_last2"``, or ``"loglinear"``.
    se_method
        How ``lr_se`` is computed:

        * ``"fixed"`` (default): premium treated as fixed —
          ``lr_se = loss_total_se / premium``.
        * ``"delta"``: full delta method including premium uncertainty
          and the loss-premium correlation ``rho``.
    rho
        Loss-premium correlation in (-1, 1). Used only when
        ``se_method = "delta"``. Default ``0.95``.
    conf_level
        Confidence level for ``lr_ci_lower`` / ``lr_ci_upper``. Default
        ``0.95``.
    bootstrap
        Not yet implemented in Python; must be ``False`` (default).
    """

    def __init__(
        self,
        method: str = "sa",
        loss_alpha: float = 1.0,
        loss_regime_break: Any = None,
        premium_method: str = "cl",
        premium_alpha: float = 1.0,
        premium_regime_break: Any = None,
        sigma_method: str = "locf",
        max_cv: float = 0.15,
        max_rse: float = 0.05,
        min_run: int = 2,
        se_method: str = "fixed",
        rho: float = 0.95,
        conf_level: float = 0.95,
        bootstrap: bool = False,
        B: int = 1000,
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
        if loss_regime_break is not None or premium_regime_break is not None:
            raise NotImplementedError(
                "regime_break not yet implemented in LR (Python)"
            )
        if not (-1 < rho < 1):
            raise ValueError(f"rho must be in (-1, 1), got {rho!r}")
        if not (0.0 < conf_level < 1.0):
            raise ValueError(
                f"conf_level must be in (0, 1), got {conf_level!r}"
            )
        if bootstrap:
            raise NotImplementedError(
                "bootstrap not yet implemented in LR (Python)"
            )

        self.method = method
        self.loss_alpha = loss_alpha
        self.loss_regime_break = loss_regime_break
        self.premium_method = premium_method
        self.premium_alpha = premium_alpha
        self.premium_regime_break = premium_regime_break
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

    def fit(self, triangle: "Triangle") -> "LRFit":
        """Fit the LR estimator on a Triangle."""
        return LRFit._from_triangle(triangle, self)


class LRFit:
    """Result of a loss-ratio fit.

    Properties
    ----------
    df : DataFrame
        Long-format triangle with columns ``[group_var?, cohort, dev,
        loss_obs, loss_proj, loss_incr_proj, premium_obs, premium_proj,
        premium_incr_proj, loss_*_se, loss_total_cv, loss_ci_*, lr_proj,
        lr_incr_proj, lr_se, lr_cv, lr_ci_lower, lr_ci_upper]`` (plus
        ``premium_total_se`` / ``premium_total_cv`` when
        ``se_method="delta"``).
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
        self._group_var: str | None
        self._cohort_var: str
        self._dev_var: str
        self.method: str
        self.loss_alpha: float
        self.premium_alpha: float
        self.se_method: str
        self.rho: float
        self.conf_level: float
        self.loss_fit: LossFit
        self.premium_fit: PremiumFit

    @classmethod
    def _from_triangle(
        cls, triangle: "Triangle", estimator: "LR"
    ) -> "LRFit":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._group_var = triangle._group_var
        self._cohort_var = triangle._cohort_var
        self._dev_var = triangle._dev_var
        self.method = estimator.method
        self.loss_alpha = estimator.loss_alpha
        self.premium_alpha = estimator.premium_alpha
        self.se_method = estimator.se_method
        self.rho = estimator.rho
        self.conf_level = estimator.conf_level

        # 1) delegate loss-side projection to Loss --------------------------
        loss_fit = Loss(
            method=estimator.method,
            alpha=estimator.loss_alpha,
            sigma_method=estimator.sigma_method,
            premium_method=estimator.premium_method,
            premium_alpha=estimator.premium_alpha,
            max_cv=estimator.max_cv,
            max_rse=estimator.max_rse,
            min_run=estimator.min_run,
            conf_level=estimator.conf_level,
        ).fit(triangle)
        self.loss_fit = loss_fit
        self.premium_fit = loss_fit.premium_fit

        full = loss_fit._df.clone()

        # 2) join premium SE columns for delta method ----------------------
        if estimator.se_method == "delta":
            pf_df = self.premium_fit._df
            keys = []
            if self._group_var is not None:
                keys.append(self._group_var)
            keys += ["cohort", "dev"]
            pf_keep = pf_df.select(
                keys + ["premium_total_se", "premium_total_cv"]
            )
            full = full.join(pf_keep, on=keys, how="left")

        # 3) LR point projection -------------------------------------------
        full = full.with_columns(
            pl.when(
                pl.col("loss_proj").is_not_null()
                & pl.col("premium_proj").is_not_null()
                & (pl.col("premium_proj") != 0.0)
            )
            .then(pl.col("loss_proj") / pl.col("premium_proj"))
            .otherwise(None)
            .alias("lr_proj"),
            pl.when(
                pl.col("loss_incr_proj").is_not_null()
                & pl.col("premium_incr_proj").is_not_null()
                & (pl.col("premium_incr_proj") > 0.0)
            )
            .then(pl.col("loss_incr_proj") / pl.col("premium_incr_proj"))
            .otherwise(None)
            .alias("lr_incr_proj"),
        )

        # 4) lr_se via fixed-premium or delta method -----------------------
        if estimator.se_method == "fixed":
            full = full.with_columns(
                pl.when(
                    pl.col("loss_total_se").is_not_null()
                    & pl.col("premium_proj").is_not_null()
                    & (pl.col("premium_proj") != 0.0)
                )
                .then(pl.col("loss_total_se") / pl.col("premium_proj"))
                .otherwise(None)
                .alias("lr_se")
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
                ).alias("_var_lr")
            )
            full = full.with_columns(
                pl.when(
                    pl.col("loss_proj").is_not_null()
                    & pl.col("premium_proj").is_not_null()
                    & (pl.col("premium_proj") > 0.0)
                    & pl.col("loss_total_se").is_not_null()
                    & pl.col("premium_total_se").is_not_null()
                )
                .then(pl.col("_var_lr").clip(lower_bound=0.0).sqrt())
                .otherwise(None)
                .alias("lr_se")
            ).drop("_var_lr")

        # 5) lr_cv + analytical CI -----------------------------------------
        z_alpha = float(norm.ppf((1 + estimator.conf_level) / 2))
        full = full.with_columns(
            pl.when(
                pl.col("lr_proj").is_not_null()
                & (pl.col("lr_proj") != 0.0)
                & pl.col("lr_se").is_not_null()
            )
            .then(pl.col("lr_se") / pl.col("lr_proj").abs())
            .otherwise(None)
            .alias("lr_cv"),
        )
        full = full.with_columns(
            pl.when(
                pl.col("lr_proj").is_not_null() & pl.col("lr_se").is_not_null()
            )
            .then(
                pl.max_horizontal(
                    pl.lit(0.0),
                    pl.col("lr_proj") - z_alpha * pl.col("lr_se"),
                )
            )
            .otherwise(None)
            .alias("lr_ci_lower"),
            pl.when(
                pl.col("lr_proj").is_not_null() & pl.col("lr_se").is_not_null()
            )
            .then(pl.col("lr_proj") + z_alpha * pl.col("lr_se"))
            .otherwise(None)
            .alias("lr_ci_upper"),
        )

        self._df = full
        return self

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
        """Per-cohort ultimate loss, premium, and LR."""
        df = self._df
        keys: list[str] = []
        if self._group_var is not None:
            keys.append(self._group_var)
        keys.append("cohort")

        observed = df.filter(pl.col("loss_obs").is_not_null())
        latest = observed.group_by(keys).agg(
            pl.col("dev").max().alias("latest"),
        )

        ultimate = (
            df.sort(keys + ["dev"])
            .group_by(keys)
            .agg(
                pl.col("loss_proj").last().alias("loss_ult"),
                pl.col("premium_proj").last().alias("premium_ult"),
                pl.col("lr_proj").last().alias("lr_ult"),
                pl.col("loss_total_se").last().alias("loss_total_se"),
                pl.col("loss_total_cv").last().alias("loss_total_cv"),
                pl.col("lr_se").last().alias("lr_se"),
                pl.col("lr_cv").last().alias("lr_cv"),
                pl.col("lr_ci_lower").last().alias("lr_ci_lower"),
                pl.col("lr_ci_upper").last().alias("lr_ci_upper"),
            )
        )

        out = latest.join(ultimate, on=keys, how="inner").sort(keys)
        return mirror_output(out, self._output_type)

    @property
    def n_rows(self) -> int:
        return self._df.height

    def __repr__(self) -> str:
        n_rows = self._df.height
        if self._group_var is not None:
            n_groups = self._df[self._group_var].n_unique()
            return (
                f"<LRFit(method={self.method!r}): "
                f"{n_groups} groups, {n_rows} rows>"
            )
        return f"<LRFit(method={self.method!r}): {n_rows} rows>"
