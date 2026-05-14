"""ATA maturity detection.

``Maturity`` is a *post-processing* step that operates on top of an
ATA factor diagnostic (:class:`ATA`). Use :meth:`ATA.maturity` to
construct one. ``Triangle`` no longer carries a ``.maturity()``
shortcut — call ``triangle.link().ata().maturity(...)``.
"""

from __future__ import annotations

from collections.abc import Callable, Mapping, Sequence
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from ._io import mirror_output
from .cl import _fit_mack

if TYPE_CHECKING:
    from .ata import ATA
    from .triangle import Triangle


@dataclass
class _MaturityResult:
    """Internal single-group maturity detection result.

    Used by :mod:`lr` (stage-adaptive method) to locate the
    ED-to-CL switch point. End users get the public
    :class:`Maturity` (built via :meth:`ATA.maturity`).
    """

    f_k: np.ndarray
    sigma2_k: np.ndarray
    cv_k: np.ndarray
    rse_k: np.ndarray
    stable_k: np.ndarray
    mat_k: int | None
    n_devs: int


# ---------------------------------------------------------------------------
# Internal computation
# ---------------------------------------------------------------------------


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
        #
        # The denominator must match the cohorts that actually contributed
        # to f_k: those with both c_k > 0 and c_{k+1} finite (R's lm fit
        # uses the same subset). Summing all finite c_k would understate
        # SE and bias rse downward.
        fit_mask = mask & (col_k > 0)
        sum_col = float(col_k[fit_mask].sum())
        if n_k >= 2 and sum_col > 0 and f_k[k] > 0:
            if sigma2_k[k] > 0:
                f_se = np.sqrt(sigma2_k[k] / sum_col)
                rse_k[k] = float(f_se / f_k[k])
            else:
                rse_k[k] = 0.0

    return cv_k, rse_k


def _detect_k_star(stable_k: np.ndarray, min_run: int) -> int | None:
    """First link index k where stable_k[k : k + min_run] are all True.

    Returns the *target* dev value of that link (1-indexed; equivalent
    to ``ata_to`` in R sibling). With this convention the development
    region splits as ED = ``dev < mat_k`` and CL = ``dev >= mat_k``.

    Returns ``None`` if no such window exists.
    """
    n_links = len(stable_k)
    if min_run < 1 or n_links < min_run:
        return None
    for k in range(n_links - min_run + 1):
        if bool(np.all(stable_k[k : k + min_run])):
            # link k goes from dev (k+1) -> dev (k+2); target dev = k + 2
            return k + 2
    return None


def _compute_maturity(
    loss_obs: np.ndarray,
    max_cv: float,
    max_rse: float,
    min_run: int,
) -> _MaturityResult:
    """Internal: compute factor stats + stability flags + mat_k.

    Used by :mod:`lr` for the stage-adaptive method's switch point.
    The public path is ``triangle.link().ata().maturity(...)``.
    """
    mack = _fit_mack(loss_obs)
    cv_k, rse_k = _compute_cv_rse(loss_obs, mack.f_k, mack.sigma2_k)
    stable_k = np.zeros(len(cv_k), dtype=bool)
    for k in range(len(cv_k)):
        if not np.isnan(cv_k[k]) and not np.isnan(rse_k[k]):
            stable_k[k] = (cv_k[k] < max_cv) and (rse_k[k] < max_rse)
    mat_k = _detect_k_star(stable_k, min_run)
    return _MaturityResult(
        f_k=mack.f_k,
        sigma2_k=mack.sigma2_k,
        cv_k=cv_k,
        rse_k=rse_k,
        stable_k=stable_k,
        mat_k=mat_k,
        n_devs=loss_obs.shape[1],
    )


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
        ``[groups?, dev, f, sigma2, cv, rse, stable]``.
    mat_k :
        Detected maturity dev. Returns ``None`` (no groups) or a dict
        ``{group_value: k_star_or_None}`` (groups set). ``None`` value
        means stability was not reached within the observation window.
    """

    def __init__(self) -> None:
        self._df: pl.DataFrame
        self._kstar_df: pl.DataFrame
        self._output_type: str
        self._groups: str | None
        self._cohort: str
        self._dev: str
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
        existing ATA factor diagnostic. The user-facing chain is
        ``triangle.link().ata().maturity(...)``.
        """
        self = cls.__new__(cls)
        self._output_type = ata._output_type
        self._groups = ata._groups
        self._cohort = ata._cohort
        self._dev = ata._dev
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

        if self._groups is None:
            stable_arr = diag_df["stable"].to_numpy()
            mat_k = _detect_k_star(stable_arr, min_run)
            kstar_df = pl.DataFrame(
                [
                    {
                        "mat_k": mat_k,
                        "n_links": int(len(stable_arr)),
                        "n_stable_links": int(stable_arr.sum()),
                    }
                ]
            )
        else:
            kstar_rows: list[dict[str, Any]] = []
            for g in (
                diag_df[self._groups].unique(maintain_order=True).to_list()
            ):
                sub = diag_df.filter(pl.col(self._groups) == g)
                stable_arr = sub["stable"].to_numpy()
                mat_k = _detect_k_star(stable_arr, min_run)
                kstar_rows.append(
                    {
                        self._groups: g,
                        "mat_k": mat_k,
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
    def _manual(
        cls,
        *,
        change: list[int],
        groups: Mapping[str, Sequence[Any]] | None,
    ) -> "Maturity":
        """Construct a Maturity by hand (no auto-detection).

        Used by :func:`maturity_at` to wrap a user-supplied ``mat_k``
        (and optional per-group values). The per-link diagnostic frame
        is intentionally empty -- there is no factor data behind a
        manual specification.
        """
        self = cls.__new__(cls)
        self._output_type = "polars"
        self.max_cv = float("nan")
        self.max_rse = float("nan")
        self.min_run = 0
        self._cohort = ""
        self._dev = ""

        n = len(change)
        if groups:
            group_col = next(iter(groups))
            group_values = list(groups[group_col])
            kstar_df = pl.DataFrame(
                {
                    group_col: group_values,
                    "mat_k": change,
                    "n_links": [0] * n,
                    "n_stable_links": [0] * n,
                }
            )
        else:
            group_col = None
            kstar_df = pl.DataFrame(
                {
                    "mat_k": change,
                    "n_links": [0] * n,
                    "n_stable_links": [0] * n,
                }
            )

        self._groups = group_col
        self._df = pl.DataFrame()
        self._kstar_df = kstar_df
        return self

    @property
    def df(self):
        """Diagnostic table per link in the original input format."""
        return mirror_output(self._df, self._output_type)

    @property
    def mat_k(self):
        """Detected maturity point.

        If the source Triangle has no ``groups``, returns an ``int``
        or ``None``. Otherwise returns ``dict[group_value, int | None]``.
        """
        if self._groups is None:
            row = self._kstar_df.row(0, named=True)
            return row["mat_k"]
        return dict(
            zip(
                self._kstar_df[self._groups].to_list(),
                self._kstar_df["mat_k"].to_list(),
            )
        )

    def summary(self):
        """One-row-per-group summary of detected mat_k."""
        return mirror_output(self._kstar_df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def __repr__(self) -> str:
        thresh = (
            f"max_cv={self.max_cv}, max_rse={self.max_rse}, m={self.min_run}"
        )
        if self._groups is None:
            return f"<Maturity: mat_k={self.mat_k} ({thresh})>"
        n_groups = self._kstar_df.height
        return f"<Maturity: {n_groups} groups ({thresh})>"


# ---------------------------------------------------------------------------
# Helper factories (R parity: maturity_at + maturity_spec)
# ---------------------------------------------------------------------------


def maturity_at(
    change: int | Sequence[int],
    *,
    groups: Mapping[str, Sequence[Any]] | None = None,
) -> "Maturity":
    """Build a :class:`Maturity` from an explicit, user-supplied ``mat_k``.

    Use when you want to override auto-detection with a fixed maturity
    dev across backtest folds. Contrast with :func:`maturity_spec`,
    which defers detection so each fold uses its own masked training
    triangle.

    Parameters
    ----------
    change
        Maturity dev (the ``ata_to`` index). A single integer or, when
        the Triangle is grouped and groups carry different maturities,
        a sequence aligned 1:1 with ``groups``.
    groups
        Optional mapping ``{column_name: [values]}`` of group columns
        aligned 1:1 with ``change``.

    Examples
    --------
    >>> maturity_at(change=6)
    >>> maturity_at(
    ...     change=[6, 8],
    ...     groups={"coverage": ["SUR", "CI"]},
    ... )
    """
    if isinstance(change, (int, np.integer)):
        change_seq: list[int] = [int(change)]
    elif isinstance(change, Sequence) and not isinstance(change, str):
        change_seq = [int(v) for v in change]
    else:
        raise TypeError(
            f"`change` must be int or Sequence[int], got {type(change).__name__}"
        )
    if not change_seq:
        raise ValueError("`change` must have length >= 1")
    n = len(change_seq)

    groups = dict(groups) if groups else {}
    for col, vals in groups.items():
        if not isinstance(vals, Sequence) or isinstance(vals, str):
            vals = [vals]
            groups[col] = vals
        if len(vals) != n:
            raise ValueError(
                f"All arguments must have equal length; "
                f"`change`={n} but `groups[{col!r}]`={len(vals)}"
            )

    return Maturity._manual(change=change_seq, groups=groups or None)


def maturity_spec(
    target: str = "loss",
    exposure: str | None = "prem",
    weight: str | None = None,
    *,
    max_cv: float = 0.15,
    max_rse: float = 0.05,
    min_run: int = 2,
) -> Callable[["Triangle"], "Maturity"]:
    """Build a lazy maturity-detection spec.

    Captures the ``triangle.link(...).ata().maturity(...)`` chain
    arguments without running detection. The returned closure is
    invoked by the consumer (fit / backtest) on its own *internal*
    triangle -- inside backtest this is the **masked** training
    triangle, so the detected ``mat_k`` never peeks at held-out cells.

    Contrast with :func:`maturity_at`, which produces an eager
    Maturity fixed at construction time.
    """
    def _spec(tri: "Triangle") -> "Maturity":
        return (
            tri.link(target=target, exposure=exposure, weight=weight)
            .ata()
            .maturity(max_cv=max_cv, max_rse=max_rse, min_run=min_run)
        )

    return _spec
