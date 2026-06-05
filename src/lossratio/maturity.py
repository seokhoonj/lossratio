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

from ._io import (
    _iter_group_frames,
    mirror_output,
    normalize_groups,
    set_group_values,
)
from ._mack import _fit_mack

if TYPE_CHECKING:
    from .ata import ATA
    from .triangle import Triangle


@dataclass
class _MaturityResult:
    """Internal single-group maturity detection result.

    Used by the stage-adaptive method to locate the
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
    link_mask: np.ndarray | None = None,
) -> tuple[np.ndarray, np.ndarray]:
    """Compute CV (across cohort link factors) and RSE (of pooled f_k).

    ``link_mask`` is the optional recent-diagonal *link-level* fit mask
    (see :mod:`lossratio._recent`). When supplied, both the cross-cohort
    CV and the pooled RSE are computed only from links inside the
    recent wedge. ``None`` (default) is the byte-identical no-filter
    path.
    """
    n_cohorts, n_devs = loss_obs.shape
    n_links = n_devs - 1

    cv_k = np.full(n_links, np.nan, dtype=np.float64)
    rse_k = np.full(n_links, np.nan, dtype=np.float64)

    for k in range(n_links):
        col_k = loss_obs[:, k]
        col_k1 = loss_obs[:, k + 1]
        mask = ~np.isnan(col_k) & ~np.isnan(col_k1)
        if link_mask is not None:
            mask = mask & link_mask[:, k]
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
        # to f_k: those with both c_k > 0 and c_{k+1} finite (the pooled
        # factor fit uses the same subset). Summing all finite c_k would
        # understate SE and bias rse downward.
        fit_mask = mask & (col_k > 0)
        sum_col = float(col_k[fit_mask].sum())
        if n_k >= 2 and sum_col > 0 and f_k[k] > 0:
            if sigma2_k[k] > 0:
                f_se = np.sqrt(sigma2_k[k] / sum_col)
                rse_k[k] = float(f_se / f_k[k])
            else:
                rse_k[k] = 0.0

    return cv_k, rse_k


def _detect_mat_k(stable_k: np.ndarray, min_run: int) -> int | None:
    """First link index k where stable_k[k : k + min_run] are all True.

    Returns the *target* dev value of that link (1-indexed; the
    ``ata_to`` index). With this convention the development
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


def _detect_first_stable_index(
    stable_k: np.ndarray, min_run: int
) -> int | None:
    """First link *array index* k where stable_k[k : k + min_run] holds.

    Same scan as :func:`_detect_mat_k`, but returns the 0-indexed
    position into the per-link diagnostic frame (so callers can slice
    the matched row), not the ``ata_to`` dev value.
    """
    n_links = len(stable_k)
    if min_run < 1 or n_links < min_run:
        return None
    for k in range(n_links - min_run + 1):
        if bool(np.all(stable_k[k : k + min_run])):
            return k
    return None


def _compute_maturity(
    loss_obs: np.ndarray,
    max_cv: float,
    max_rse: float,
    min_run: int,
    link_mask: np.ndarray | None = None,
) -> _MaturityResult:
    """Internal: compute factor stats + stability flags + mat_k.

    Used by the stage-adaptive method's switch point.
    The public path is ``triangle.link().ata().maturity(...)``.

    ``link_mask`` is the optional recent-diagonal *link-level* fit mask
    (see :mod:`lossratio._recent`): when supplied, the factor stats and
    stability detection run on the recent wedge (the recent filter is
    applied before resolving maturity). ``None`` (default) is the
    byte-identical no-filter path.
    """
    mack = _fit_mack(loss_obs, link_mask=link_mask)
    cv_k, rse_k = _compute_cv_rse(
        loss_obs, mack.f_k, mack.sigma2_k, link_mask=link_mask
    )
    stable_k = np.zeros(len(cv_k), dtype=bool)
    for k in range(len(cv_k)):
        if not np.isnan(cv_k[k]) and not np.isnan(rse_k[k]):
            stable_k[k] = (cv_k[k] < max_cv) and (rse_k[k] < max_rse)
    mat_k = _detect_mat_k(stable_k, min_run)
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
# Per-link descriptive stats from Link
# ---------------------------------------------------------------------------


# Column order for the Maturity output.
_R_STAT_COLS: tuple[str, ...] = (
    "ata_from",
    "change",
    "ata_link",
    "mean",
    "median",
    "wt",
    "cv",
    "f",
    "f_se",
    "rse",
    "sigma",
    "n_cohorts",
    "n_valid",
    "n_inf",
    "n_nan",
    "valid_ratio",
)


def _link_descriptive_stats(link_df: pl.DataFrame) -> pl.DataFrame:
    """Per-link descriptive stats: mean / median / wt / counts.

    Computed directly off the long-format ``Link`` DataFrame (one row
    per ``(cohort, ata_from)`` pair, with cell-level ``ata`` and
    ``loss_from`` / ``loss_to``):

    * ``mean``   -- mean of finite per-cohort ``ata`` factors
    * ``median`` -- median of finite per-cohort ``ata`` factors
    * ``wt``     -- volume-weighted factor ``sum(loss_to) / sum(loss_from)``
    * ``n_cohorts``  -- total rows in the link group
    * ``n_valid``    -- count of finite ``ata`` values
    * ``n_inf``      -- count of +/- Inf ``ata`` values
    * ``n_nan``      -- count of NaN ``ata`` values
    * ``valid_ratio`` -- ``n_valid / n_cohorts``

    Grouping is on ``ata_from`` (plus the upstream ``groups`` column if
    present). The caller (``Maturity._from_ata``) joins this onto the
    ATA frame's WLS columns ``f / f_se / sigma / rse / cv``.
    """
    if link_df.is_empty():
        return link_df

    has_group_cols = [
        c for c in link_df.columns if c not in {
            "cohort", "ata_from", "ata_to", "ata_link",
            "loss_from", "loss_to", "loss_delta", "ata",
            "premium_from", "premium_to", "premium_delta", "intensity",
            "weight",
        }
    ]

    ata_finite = pl.col("ata").is_finite()
    ata_inf = pl.col("ata").is_infinite()
    ata_nan = pl.col("ata").is_nan()

    by_cols = [*has_group_cols, "ata_from", "ata_to"]
    out = link_df.group_by(by_cols, maintain_order=True).agg(
        [
            pl.col("ata").filter(ata_finite).mean().alias("mean"),
            pl.col("ata").filter(ata_finite).median().alias("median"),
            (
                pl.col("loss_to").sum()
                / pl.when(pl.col("loss_from").sum() != 0)
                .then(pl.col("loss_from").sum())
                .otherwise(None)
            ).alias("wt"),
            pl.len().alias("n_cohorts"),
            ata_finite.sum().alias("n_valid"),
            ata_inf.sum().alias("n_inf"),
            ata_nan.sum().alias("n_nan"),
        ]
    )

    out = out.with_columns(
        (pl.col("n_valid") / pl.col("n_cohorts")).alias("valid_ratio")
    )
    # Cast to int64 so they round-trip cleanly (downstream selects cast
    # back to float).
    out = out.with_columns(
        [
            pl.col("n_cohorts").cast(pl.Int64),
            pl.col("n_valid").cast(pl.Int64),
            pl.col("n_inf").cast(pl.Int64),
            pl.col("n_nan").cast(pl.Int64),
        ]
    )
    return out.sort([*has_group_cols, "ata_from"])


def _enriched_ata_diagnostic(
    ata_df: pl.DataFrame,
    link_df: pl.DataFrame,
    groups: str | list[str] | None,
) -> pl.DataFrame:
    """Per-link diagnostic with the full ``ATASummary`` column set.

    Joins three sources keyed on (``groups?``, ``ata_from``):

    * ``ata_df``   -- ATA's per-link Mack outputs (``f``, ``sigma2``,
      ``cv``, ``rse``, ``n_cohorts``); ``dev`` column there is the
      ``ata_from`` index.
    * ``link_df``  -- the underlying long-format Link table; supplies
      ``mean / median / wt / n_valid / n_inf / n_nan / valid_ratio``
      via :func:`_link_descriptive_stats`.

    Adds derived columns:

    * ``ata_to``   = ``ata_from + 1``
    * ``ata_link`` = ``"<from>-<to>"``
    * ``sigma``    = ``sqrt(sigma2)``
    * ``f_se``     = ``rse * f``  (since ``rse = f_se / f``)

    Returns one row per (group, link) with columns ordered to match
    ``ATASummary``. Used by :meth:`Maturity._from_ata` -- the
    per-group "first mature row" pick then slices straight off this.
    """
    if ata_df.is_empty():
        return ata_df

    # ATA frame: dev (= ata_from), f, sigma2, cv, rse, n_cohorts
    a = ata_df.rename({"dev": "ata_from"})

    stats = _link_descriptive_stats(link_df)
    if stats.is_empty():
        return a

    join_keys = [*normalize_groups(groups), "ata_from"]
    # Drop overlapping cols from stats (n_cohorts is on both -- keep
    # stats' which is the row count over the link table).
    a = a.drop("n_cohorts")

    merged = a.join(stats, on=join_keys, how="left")

    merged = merged.with_columns(
        [
            pl.col("ata_to").cast(pl.Int64),
            pl.format(
                "{}-{}", pl.col("ata_from"), pl.col("ata_to")
            ).alias("ata_link"),
            pl.when(pl.col("sigma2").is_not_null())
            .then(pl.col("sigma2").sqrt())
            .otherwise(None)
            .alias("sigma"),
            pl.when(
                pl.col("f").is_not_null() & pl.col("rse").is_not_null()
            )
            .then(pl.col("f") * pl.col("rse"))
            .otherwise(None)
            .alias("f_se"),
        ]
    )

    return merged


def _na_row(groups: str | list[str] | None, group_value: Any | None) -> dict[str, Any]:
    """All-NaN row for a group where no stable ATA run was found.

    The no-match branch: every numeric column is null/NaN and
    ``ata_link`` is null. Float dtype keeps polars concat/dtype-stable
    across groups.
    """
    row: dict[str, Any] = {}
    set_group_values(row, groups, group_value)
    for col in _R_STAT_COLS:
        row[col] = None if col == "ata_link" else float("nan")
    return row


def _slice_first_stable_row(
    diag_df: pl.DataFrame,
    min_run: int,
    groups: str | list[str] | None,
    group_value: Any | None,
) -> dict[str, Any]:
    """Pick the first stable-run row of a single-group diagnostic frame.

    Finds the first link where the ``stable`` boolean run holds for
    ``min_run`` consecutive links and returns that row coerced to the
    stat-column dict. Returns an all-NaN row (see :func:`_na_row`)
    when no such run exists.
    """
    stable_arr = diag_df["stable"].to_numpy()
    idx = _detect_first_stable_index(stable_arr, min_run)
    if idx is None:
        return _na_row(groups, group_value)

    matched = diag_df.row(idx, named=True)
    row: dict[str, Any] = {}
    set_group_values(row, groups, group_value)
    row["ata_from"] = float(matched["ata_from"])
    row["change"] = float(matched["ata_to"])
    row["ata_link"] = matched["ata_link"]
    for col in ("mean", "median", "wt", "cv", "f", "f_se", "rse", "sigma"):
        v = matched.get(col)
        row[col] = float(v) if v is not None else float("nan")
    for col in ("n_cohorts", "n_valid", "n_inf", "n_nan"):
        v = matched.get(col)
        # int -> float for column-type stability with NaN branch.
        row[col] = float(v) if v is not None else float("nan")
    vr = matched.get("valid_ratio")
    row["valid_ratio"] = float(vr) if vr is not None else float("nan")
    return row


def _build_mat_k_df(
    diag_df: pl.DataFrame,
    groups: str | list[str] | None,
    min_run: int,
) -> pl.DataFrame:
    """Per-group "first mature row" frame -- the ``Maturity`` schema.

    One row per group with columns ``[groups?, *_R_STAT_COLS]``.
    """
    rows = [
        _slice_first_stable_row(sub, min_run, groups=groups, group_value=g)
        for g, sub in _iter_group_frames(diag_df, groups)
    ]
    return pl.DataFrame(rows) if rows else pl.DataFrame()


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
    point :
        Detected maturity dev. Returns ``None`` (no groups) or a dict
        ``{group_value: dev_or_None}`` (groups set). ``None`` value means
        stability was not reached within the observation window.
    """

    def __init__(self) -> None:
        raise TypeError(
            "Maturity is produced by `triangle.link().ata().maturity()` / "
            "`Maturity.at()` / `.detect()`, not a direct constructor."
        )

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

        The output ``summary()`` is one row per group with columns
        ``[groups?, ata_from, change, ata_link, mean, median, wt, cv,
        f, f_se, rse, sigma, n_cohorts, n_valid, n_inf, n_nan,
        valid_ratio]``. ``change`` is the maturity point (``ata_to`` of
        the first stable link). When no stable run is found for a
        group, the stat columns are filled with ``NaN``.
        """
        self = cls.__new__(cls)
        self._output_type = ata._output_type
        self._groups = ata._groups
        self._cohort = ata._cohort
        self._dev = ata._dev
        self.max_cv = max_cv
        self.max_rse = max_rse
        self.min_run = min_run

        # Per-link diagnostic enriched with all the ATASummary columns
        # (mean / median / wt / counts via the Link long-format frame).
        link_df = ata._link._df
        diag_df = _enriched_ata_diagnostic(ata._df, link_df, self._groups)

        diag_df = diag_df.with_columns(
            (
                pl.col("cv").is_not_null()
                & pl.col("rse").is_not_null()
                & (pl.col("cv") < max_cv)
                & (pl.col("rse") < max_rse)
            ).alias("stable")
        )

        mat_k_df = _build_mat_k_df(
            diag_df, self._groups, min_run
        )

        self._df = diag_df
        self._mat_k_df = mat_k_df
        return self

    @classmethod
    def _manual(
        cls,
        *,
        change: list[int],
        groups: Mapping[str, Sequence[Any]] | None,
    ) -> "Maturity":
        """Construct a Maturity by hand (no auto-detection).

        Used by :meth:`Maturity.at` to wrap a user-supplied maturity point
        (and optional per-group values). The per-link diagnostic frame
        is intentionally empty -- there is no factor data behind a
        manual specification, so all stat columns are ``NaN``.
        """
        self = cls.__new__(cls)
        self._output_type = "polars"
        self.max_cv = float("nan")
        self.max_rse = float("nan")
        self.min_run = 0
        self._cohort = ""
        self._dev = ""

        n = len(change)
        ata_from = [c - 1 for c in change]
        ata_link = [f"{f}-{t}" for f, t in zip(ata_from, change)]

        cols: dict[str, list[Any]] = {}
        if groups:
            group_col = next(iter(groups))
            cols[group_col] = list(groups[group_col])
        else:
            group_col = None

        cols["ata_from"] = [float(v) for v in ata_from]
        cols["change"] = [float(v) for v in change]
        cols["ata_link"] = ata_link
        for stat in (
            "mean", "median", "wt", "cv",
            "f", "f_se", "rse", "sigma",
            "n_cohorts", "n_valid", "n_inf", "n_nan", "valid_ratio",
        ):
            cols[stat] = [float("nan")] * n

        mat_k_df = pl.DataFrame(cols)

        self._groups = group_col
        self._df = pl.DataFrame()
        self._mat_k_df = mat_k_df
        return self

    @classmethod
    def at(
        cls,
        change: int | Sequence[int],
        *,
        groups: Mapping[str, Sequence[Any]] | None = None,
    ) -> "Maturity":
        """Build a :class:`Maturity` from an explicit, user-supplied maturity point.

        Use when you want to override auto-detection with a fixed maturity
        dev across backtest folds. Contrast with :meth:`detect`, which
        defers detection so each fold uses its own masked training
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
        >>> Maturity.at(change=6)
        >>> Maturity.at(
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

        return cls._manual(change=change_seq, groups=groups or None)

    @classmethod
    def detect(
        cls,
        target: str = "loss",
        exposure: str | None = "premium",
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
        triangle, so the detected maturity point never peeks at held-out cells.

        Contrast with :meth:`at`, which produces an eager Maturity fixed
        at construction time.
        """
        def _spec(tri: "Triangle") -> "Maturity":
            return (
                tri.link(target=target, exposure=exposure, weight=weight)
                .ata()
                .maturity(max_cv=max_cv, max_rse=max_rse, min_run=min_run)
            )

        return _spec

    @property
    def df(self):
        """Diagnostic table per link in the original input format."""
        return mirror_output(self._df, self._output_type)

    @property
    def point(self):
        """Detected maturity point (``k*``), the headline accessor.

        If the source Triangle has no ``groups``, returns an ``int``
        or ``None``. Otherwise returns ``dict[group_value, int | None]``.

        ``None`` is returned for a group where no stable run was found
        (the all-NaN row).
        """
        def _coerce(v: Any) -> int | None:
            if v is None:
                return None
            # `change` is always a Float64 column -> v is float | None;
            # `v != v` is the NaN test (no numpy dispatch needed).
            if isinstance(v, float) and v != v:
                return None
            return int(v)

        if self._groups is None:
            row = self._mat_k_df.row(0, named=True)
            return _coerce(row.get("change"))
        if isinstance(self._groups, str):
            keys = self._mat_k_df[self._groups].to_list()
        else:
            keys = [
                tuple(r)
                for r in self._mat_k_df.select(
                    normalize_groups(self._groups)
                ).iter_rows()
            ]
        return dict(
            zip(
                keys,
                [_coerce(v) for v in self._mat_k_df["change"].to_list()],
            )
        )

    def summary(self):
        """One-row-per-group summary of the detected maturity link.

        Schema::

            [groups?, ata_from, change, ata_link,
             mean, median, wt, cv,
             f, f_se, rse, sigma,
             n_cohorts, n_valid, n_inf, n_nan, valid_ratio]

        ``change`` is the maturity point (i.e. ``ata_to`` of the first
        stable link). Groups with no stable run have ``NaN`` in every
        stat column.
        """
        return mirror_output(self._mat_k_df, self._output_type)

    def to_polars(self) -> pl.DataFrame:
        return self._df

    def to_pandas(self):
        return self._df.to_pandas()

    def __repr__(self) -> str:
        thresh = (
            f"max_cv={self.max_cv}, max_rse={self.max_rse}, m={self.min_run}"
        )
        if self._groups is None:
            return f"<Maturity: point={self.point} ({thresh})>"
        n_groups = self._mat_k_df.height
        return f"<Maturity: {n_groups} groups ({thresh})>"




# ---------------------------------------------------------------------------
# Maturity input dispatcher
# ---------------------------------------------------------------------------


def _resolve_maturity(
    arg: Any,
    triangle: "Triangle",
) -> "Maturity | None":
    """Resolve a 4-type ``maturity`` input to a Maturity object (or None).

    Used by :class:`~lossratio.Loss` / :class:`~lossratio.Ratio` to
    normalise the ``maturity`` argument into a single representation:
    either ``None`` (no maturity override -- caller's default behaviour)
    or a :class:`Maturity` object.

    The four accepted input forms:

    * ``None`` -- returns ``None`` (no maturity; SA falls back to ED
      throughout).
    * a :class:`Maturity` object -- returned as-is (eager override).
    * ``"auto"`` -- runs detection on ``triangle`` via
      ``triangle.link().ata().maturity()``.
    * a callable ``f(triangle) -> Maturity`` -- e.g. the closure from
      :meth:`Maturity.detect`; invoked with ``triangle``. Its return
      value must be a :class:`Maturity`.

    Parameters
    ----------
    arg
        The ``maturity`` input.
    triangle
        The :class:`~lossratio.Triangle` detection runs against -- the
        fit's own (already regime / recent-filtered as appropriate)
        triangle, or, inside backtest, the masked training triangle.
    """
    if arg is None:
        return None
    if isinstance(arg, Maturity):
        return arg
    if isinstance(arg, str):
        if arg == "auto":
            return triangle.link().ata().maturity()
        raise ValueError(
            f"`maturity` string must be \"auto\", got {arg!r}."
        )
    if callable(arg):
        out = arg(triangle)
        if not isinstance(out, Maturity):
            raise TypeError(
                "`maturity` callable must return a Maturity object; "
                f"got {type(out).__name__}."
            )
        return out
    raise TypeError(
        "`maturity` must be None, a Maturity object, \"auto\", or a "
        f"callable returning a Maturity; got {type(arg).__name__}."
    )
