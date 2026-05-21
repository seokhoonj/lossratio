"""Cape Cod (CC) estimator.

Cape Cod (Stanard, 1985) is the *prior-free* Bornhuetter-Ferguson
variant: the a priori expected loss ratio is *estimated from the data
itself* as a portfolio-pooled quantity, then plugged into the BF
formula::

    elr_cc   = sum(loss_latest) / sum(premium_ult * q)        # within group
    loss_ult = loss_latest + (1 - q) * elr_cc * premium_ult

The Cape Cod ELR is itself data-driven, so the analytical SE path also
reports its uncertainty (``elr_cc_se`` / ``elr_cc_cv`` /
``elr_cc_ci_lo`` / ``elr_cc_ci_hi``) via the delta method.

Python sibling of R ``fit_cc()`` (see ``R/cc.R``). The shared machinery
(emergence table, credibility blend, analytical SE, cell projection)
lives in :mod:`lossratio.bf`. Only the analytical SE path is
implemented; the bootstrap path raises ``NotImplementedError``.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import mirror_output
from ._sigma import VALID_SIGMA_METHODS
from .bf import (
    _bf_full_df,
    _bf_summary_df,
    _fit_bf_cc_single,
    _resolve_credibility,
)
from .cl import _build_value_matrix

if TYPE_CHECKING:
    from .triangle import Triangle


# ---------------------------------------------------------------------------
# Public API: CC estimator + CCFit result class
# ---------------------------------------------------------------------------


class CC:
    """Cape Cod estimator (prior-free Bornhuetter-Ferguson).

    Estimates a portfolio-pooled expected loss ratio from the data
    itself (within each group), then plugs it into the BF formula.

    Parameters
    ----------
    alpha
        Variance-structure exponent. Only ``alpha = 1`` is supported.
    sigma_method
        Tail-sigma extrapolation method for the inner chain-ladder
        fits. Default ``"locf"``.
    conf_level
        Confidence level for ``loss_ci_lo`` / ``loss_ci_hi`` and the
        pooled-ELR CI. Default ``0.95``.
    credibility
        ``None`` (default) for the classical CC blend (weight = the
        emergence fraction ``q``), or ``{"method": "bs", "K": None}``
        for a Buehlmann-Straub credibility blend with the pooled ELR
        as the prior.

    Examples
    --------
    >>> import lossratio as lr
    >>> tri = lr.Triangle(df, groups="coverage")
    >>> fit = lr.CC().fit(tri)
    >>> fit.summary()
    """

    def __init__(
        self,
        alpha: float = 1.0,
        sigma_method: str = "locf",
        conf_level: float = 0.95,
        credibility: Any = None,
        bootstrap: Any = None,
        type: str = "analytical",
    ) -> None:
        if alpha != 1.0:
            raise NotImplementedError(
                f"alpha={alpha} not yet implemented; only alpha=1 "
                "is supported"
            )
        if sigma_method not in VALID_SIGMA_METHODS:
            raise ValueError(
                f"sigma_method must be one of {VALID_SIGMA_METHODS}, "
                f"got {sigma_method!r}"
            )
        if not (0.0 < conf_level < 1.0):
            raise ValueError(
                f"conf_level must be in (0, 1), got {conf_level!r}"
            )
        if bootstrap is not None and bootstrap is not False:
            raise NotImplementedError(
                "bootstrap not yet implemented in BF/CC (Python)"
            )
        if type != "analytical":
            raise NotImplementedError(
                "bootstrap not yet implemented in BF/CC (Python)"
            )
        self.alpha = alpha
        self.sigma_method = sigma_method
        self.conf_level = conf_level
        self.credibility = _resolve_credibility(credibility)

    def fit(
        self,
        triangle: "Triangle",
        loss: str = "loss",
        exposure: str = "premium",
    ) -> "CCFit":
        """Fit the Cape Cod estimator on a Triangle.

        Parameters
        ----------
        triangle
            Source :class:`Triangle`.
        loss
            Cumulative loss column to project. Default ``"loss"``.
        exposure
            Cumulative premium column used as the ELR denominator.
            Default ``"premium"``.
        """
        return CCFit._from_triangle(
            triangle, self, loss=loss, exposure=exposure
        )


class CCFit:
    """Result of a Cape Cod fit.

    Properties
    ----------
    df : DataFrame
        Cell-level ``$full`` grid ``[groups?, cohort, dev, loss_obs,
        loss_proj, incr_loss_proj, premium_obs, premium_proj,
        incr_premium_proj]`` -- point estimates, no per-cell SE.
    summary : DataFrame
        Cohort-level reserve table ``[groups?, cohort, latest,
        loss_ult, reserve, elr, q, loss_total_se, loss_total_cv,
        loss_ci_lo, loss_ci_hi, elr_cc, elr_cc_se, elr_cc_cv,
        elr_cc_ci_lo, elr_cc_ci_hi]``.
    elr_cc : DataFrame
        The pooled ELR per group (``[groups?, elr_cc]``).
    """

    method = "cc"

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._summary_df: pl.DataFrame
        self._elr_cc_df: pl.DataFrame
        self._output_type: str
        self._groups: str | None
        self._cohort: str
        self._dev: str

    @classmethod
    def _from_triangle(
        cls,
        triangle: "Triangle",
        estimator: "CC",
        loss: str = "loss",
        exposure: str = "premium",
    ) -> "CCFit":
        self = cls.__new__(cls)
        self._output_type = triangle._output_type
        self._groups = triangle._groups
        self._cohort = triangle._cohort
        self._dev = triangle._dev
        self.loss = loss
        self.premium = exposure
        self.alpha = estimator.alpha
        self.sigma_method = estimator.sigma_method
        self.conf_level = estimator.conf_level
        self.credibility = estimator.credibility
        self.ci_type = "analytical"

        tri_df = triangle._df
        groups = triangle._groups

        if loss not in tri_df.columns:
            raise ValueError(
                f"`loss={loss!r}` column missing from Triangle."
            )
        if exposure not in tri_df.columns:
            raise ValueError(
                f"`exposure={exposure!r}` column missing from Triangle."
            )

        full_parts: list[pl.DataFrame] = []
        summary_parts: list[pl.DataFrame] = []
        elr_rows: list[dict[str, Any]] = []

        if groups is None:
            loss_obs, cohorts, _ = _build_value_matrix(tri_df, loss)
            premium_obs, _, _ = _build_value_matrix(tri_df, exposure)
            result = _fit_bf_cc_single(
                loss_obs,
                premium_obs,
                cohorts,
                sigma_method=estimator.sigma_method,
                conf_level=estimator.conf_level,
                credibility=estimator.credibility,
                cape_cod=True,
                prior=None,
                group_value=None,
            )
            full_parts.append(_bf_full_df(result, None, None))
            summary_parts.append(
                _bf_summary_df(result, None, None, cape_cod=True)
            )
            elr_rows.append({"elr_cc": result.cc_extra.get("elr_cc")})
        else:
            for g in tri_df[groups].unique(maintain_order=True).to_list():
                sub = tri_df.filter(pl.col(groups) == g)
                loss_obs, cohorts, _ = _build_value_matrix(sub, loss)
                premium_obs, _, _ = _build_value_matrix(sub, exposure)
                result = _fit_bf_cc_single(
                    loss_obs,
                    premium_obs,
                    cohorts,
                    sigma_method=estimator.sigma_method,
                    conf_level=estimator.conf_level,
                    credibility=estimator.credibility,
                    cape_cod=True,
                    prior=None,
                    group_value=g,
                )
                full_parts.append(_bf_full_df(result, groups, g))
                summary_parts.append(
                    _bf_summary_df(result, groups, g, cape_cod=True)
                )
                elr_rows.append(
                    {groups: g, "elr_cc": result.cc_extra.get("elr_cc")}
                )

        self._df = (
            pl.concat(full_parts) if full_parts else pl.DataFrame()
        )
        self._summary_df = (
            pl.concat(summary_parts)
            if summary_parts
            else pl.DataFrame()
        )
        self._elr_cc_df = pl.DataFrame(
            elr_rows, infer_schema_length=None
        )
        return self

    @property
    def df(self):
        """Cell-level projected triangle in the original input format."""
        return mirror_output(self._df, self._output_type)

    @property
    def proj(self):
        """``$full`` with observed-cell projection columns NA'd out."""
        proj_cols = [
            "loss_proj",
            "incr_loss_proj",
            "premium_proj",
            "incr_premium_proj",
        ]
        df = self._df.with_columns(
            [
                pl.when(pl.col("loss_obs").is_not_null())
                .then(None)
                .otherwise(pl.col(c))
                .alias(c)
                for c in proj_cols
                if c in self._df.columns
            ]
        )
        return mirror_output(df, self._output_type)

    @property
    def elr_cc(self):
        """The data-pooled ELR per group."""
        return mirror_output(self._elr_cc_df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def summary(self) -> pl.DataFrame:
        """Cohort-level reserve summary (with pooled-ELR columns)."""
        return mirror_output(self._summary_df, self._output_type)

    @property
    def n_rows(self) -> int:
        return self._df.height

    def __repr__(self) -> str:
        n_rows = self._df.height
        cred = "" if self.credibility is None else ", credibility=bs"
        if self._groups is not None:
            n_groups = self._df[self._groups].n_unique()
            return (
                f"<CCFit: {n_groups} groups, {n_rows} rows{cred}>"
            )
        return f"<CCFit: {n_rows} rows{cred}>"
