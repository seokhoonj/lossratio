"""ATA maturity detection."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import mirror_output
from .cl import _build_loss_matrix, _fit_mack

if TYPE_CHECKING:
    from .ata import ATA
    from .triangle import Triangle


# ---------------------------------------------------------------------------
# Internal computation
# ---------------------------------------------------------------------------


@dataclass
class _MaturityResult:
    """Single-group maturity detection result."""

    f_k: np.ndarray         # (n_links,)
    sigma2_k: np.ndarray    # (n_links,)
    cv_k: np.ndarray        # (n_links,)  CV of individual link factors
    rse_k: np.ndarray       # (n_links,)  RSE of pooled f_k
    stable_k: np.ndarray    # (n_links,)  bool
    k_star: int | None      # 1-indexed dev where stability begins
    n_devs: int


def _compute_cv_rse(
    loss_obs: np.ndarray,
    f_k: np.ndarray,
    sigma2_k: np.ndarray,
) -> tuple[np.ndarray, np.ndarray]:
    """Compute CV (across cohort link factors) and RSE (of pooled f_k)."""
    n_cohorts, n_devs = loss_obs.shape
    n_links = n_devs - 1

    cv_k = np.full(n_links, np.nan, dtype=np.float64)
    rse_k = np.full(n_links, np.nan, dtype=np.float64)

    for k in range(n_links):
        col_k = loss_obs[:, k]
        col_k1 = loss_obs[:, k + 1]
        mask = ~np.isnan(col_k) & ~np.isnan(col_k1)
        n_k = int(mask.sum())

        # Cross-cohort CV of individual link factors (needs n_k >= 2,
        # and the denominator C^L_{i,k} > 0 for each contributing cohort).
        if n_k >= 2:
            ck = col_k[mask]
            ck1 = col_k1[mask]
            ck_pos = ck > 0
            if ck_pos.sum() >= 2:
                indiv = ck1[ck_pos] / ck[ck_pos]
                f_mean = float(indiv.mean())
                f_sd = float(indiv.std(ddof=1))
                if f_mean != 0:
                    cv_k[k] = f_sd / f_mean

        # RSE of pooled f_k. Three cases:
        #   n_k >= 2, sigma^2 > 0  -> RSE = sqrt(sigma^2 / sum_j C_j) / f_k
        #   n_k >= 2, sigma^2 == 0 -> perfectly stable estimate, RSE = 0
        #   n_k <  2               -> insufficient samples, leave NaN
        sum_col = float(col_k[~np.isnan(col_k)].sum())
        if n_k >= 2 and sum_col > 0 and f_k[k] > 0:
            if sigma2_k[k] > 0:
                f_se = np.sqrt(sigma2_k[k] / sum_col)
                rse_k[k] = float(f_se / f_k[k])
            else:
                rse_k[k] = 0.0

    return cv_k, rse_k


def _detect_k_star(stable_k: np.ndarray, min_run: int) -> int | None:
    """First link index k where stable_k[k : k + min_run] are all True.

    Returns the 1-indexed dev value (link source dev), or ``None`` if
    no such window exists.
    """
    n_links = len(stable_k)
    if min_run < 1 or n_links < min_run:
        return None
    for k in range(n_links - min_run + 1):
        if bool(np.all(stable_k[k : k + min_run])):
            return k + 1
    return None


def _compute_maturity(
    loss_obs: np.ndarray,
    max_cv: float,
    max_rse: float,
    min_run: int,
) -> _MaturityResult:
    mack = _fit_mack(loss_obs)
    cv_k, rse_k = _compute_cv_rse(loss_obs, mack.f_k, mack.sigma2_k)
    stable_k = np.zeros(len(cv_k), dtype=bool)
    for k in range(len(cv_k)):
        if not np.isnan(cv_k[k]) and not np.isnan(rse_k[k]):
            stable_k[k] = (cv_k[k] < max_cv) and (rse_k[k] < max_rse)
    k_star = _detect_k_star(stable_k, min_run)
    return _MaturityResult(
        f_k=mack.f_k,
        sigma2_k=mack.sigma2_k,
        cv_k=cv_k,
        rse_k=rse_k,
        stable_k=stable_k,
        k_star=k_star,
        n_devs=loss_obs.shape[1],
    )


def _diagnostic_to_df(
    result: _MaturityResult,
    group_var: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """Convert a maturity result into a long-format diagnostic DataFrame."""
    rows = []
    for k in range(len(result.f_k)):
        row: dict[str, Any] = {}
        if group_var is not None:
            row[group_var] = group_value
        row["dev"] = k + 1
        row["f"] = float(result.f_k[k]) if not np.isnan(result.f_k[k]) else None
        row["sigma2"] = (
            float(result.sigma2_k[k])
            if not np.isnan(result.sigma2_k[k])
            else None
        )
        row["cv"] = (
            float(result.cv_k[k]) if not np.isnan(result.cv_k[k]) else None
        )
        row["rse"] = (
            float(result.rse_k[k]) if not np.isnan(result.rse_k[k]) else None
        )
        row["stable"] = bool(result.stable_k[k])
        rows.append(row)
    return pl.DataFrame(rows)


def _kstar_to_df(
    result: _MaturityResult,
    group_var: str | None,
    group_value: Any | None,
) -> pl.DataFrame:
    """One-row summary DataFrame for the detected k_star."""
    row: dict[str, Any] = {}
    if group_var is not None:
        row[group_var] = group_value
    row["k_star"] = result.k_star
    row["n_links"] = len(result.f_k)
    row["n_stable_links"] = int(result.stable_k.sum())
    return pl.DataFrame([row])


# ---------------------------------------------------------------------------
# Public result class
# ---------------------------------------------------------------------------


class Maturity:
    """Result of ATA maturity detection.

    Maturity point ``k*`` is the first development period at which the
    age-to-age factors are jointly *stable*: ``CV(f_k) < max_cv`` and
    ``RSE(f_k) < max_rse``, sustained for ``min_run`` consecutive
    links.

    Properties
    ----------
    df : DataFrame
        Per-link diagnostic table:
        ``[group_var?, dev, f, sigma2, cv, rse, stable]``.
    k_star :
        Detected maturity dev. Returns ``None`` (no group_var) or a dict
        ``{group_value: k_star_or_None}`` (group_var set). ``None`` value
        means stability was not reached within the observation window.
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._kstar_df: pl.DataFrame
        self._output_type: str
        self._group_var: str | None
        self._cohort_var: str
        self._dev_var: str
        self._dev_unit: str
        self.max_cv: float
        self.max_rse: float
        self.min_run: int

    @classmethod
    def _from_ata(
        cls,
        ata: "ATA",
        max_cv: float,
        max_rse: float,
        min_run: int,
    ) -> "Maturity":
        """Build Maturity by applying stability thresholds on top of an
        existing ATA factor diagnostic. This is the canonical
        constructor — :meth:`Triangle.maturity` is a thin shortcut
        that calls ``triangle.ata().maturity()``.
        """
        self = cls.__new__(cls)
        self._output_type = ata._output_type
        self._group_var = ata._group_var
        self._cohort_var = ata._cohort_var
        self._dev_var = ata._dev_var
        self._dev_unit = ata._dev_unit
        self.max_cv = max_cv
        self.max_rse = max_rse
        self.min_run = min_run

        diag_df = ata._df.with_columns(
            (
                pl.col("cv").is_not_null()
                & pl.col("rse").is_not_null()
                & (pl.col("cv") < max_cv)
                & (pl.col("rse") < max_rse)
            ).alias("stable")
        )

        if self._group_var is None:
            stable_arr = diag_df["stable"].to_numpy()
            k_star = _detect_k_star(stable_arr, min_run)
            kstar_df = pl.DataFrame(
                [
                    {
                        "k_star": k_star,
                        "n_links": int(len(stable_arr)),
                        "n_stable_links": int(stable_arr.sum()),
                    }
                ]
            )
        else:
            kstar_rows: list[dict[str, Any]] = []
            for g in (
                diag_df[self._group_var].unique(maintain_order=True).to_list()
            ):
                sub = diag_df.filter(pl.col(self._group_var) == g)
                stable_arr = sub["stable"].to_numpy()
                k_star = _detect_k_star(stable_arr, min_run)
                kstar_rows.append(
                    {
                        self._group_var: g,
                        "k_star": k_star,
                        "n_links": int(len(stable_arr)),
                        "n_stable_links": int(stable_arr.sum()),
                    }
                )
            kstar_df = (
                pl.DataFrame(kstar_rows) if kstar_rows else pl.DataFrame()
            )

        self._df = diag_df
        self._kstar_df = kstar_df
        return self

    @classmethod
    def _from_triangle(
        cls,
        triangle: "Triangle",
        max_cv: float,
        max_rse: float,
        min_run: int,
    ) -> "Maturity":
        """Backward-compat shortcut: build ATA factor diagnostic on the
        triangle, then apply maturity thresholds. Equivalent to
        ``triangle.ata().maturity(...)``.
        """
        return triangle.ata().maturity(
            max_cv=max_cv,
            max_rse=max_rse,
            min_run=min_run,
        )

    @property
    def df(self):
        """Diagnostic table per link in the original input format."""
        return mirror_output(self._df, self._output_type)

    @property
    def k_star(self):
        """Detected maturity point.

        If the source Triangle has no ``group_var``, returns an ``int``
        or ``None``. Otherwise returns ``dict[group_value, int | None]``.
        """
        if self._group_var is None:
            row = self._kstar_df.row(0, named=True)
            return row["k_star"]
        return dict(
            zip(
                self._kstar_df[self._group_var].to_list(),
                self._kstar_df["k_star"].to_list(),
            )
        )

    def summary(self):
        """One-row-per-group summary of detected k_star."""
        return mirror_output(self._kstar_df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def __repr__(self) -> str:
        thresh = (
            f"max_cv={self.max_cv}, max_rse={self.max_rse}, m={self.min_run}"
        )
        if self._group_var is None:
            return f"<Maturity: k_star={self.k_star} ({thresh})>"
        n_groups = self._kstar_df.height
        return f"<Maturity: {n_groups} groups ({thresh})>"
