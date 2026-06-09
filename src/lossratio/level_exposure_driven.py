"""Cohort-level exposure-driven loss projection (EXPERIMENTAL).

``LevelExposureDriven`` extends the additive exposure-driven (ED) projection by
giving each cohort its own level multiplying the shared per-duration intensity::

    incr_loss[c, k] = cohort_level[c] * g_k * premium[c, k-1]

``g_k`` is the pooled intensity -- identical to :class:`ExposureDriven`'s shape
(``g_k = sum incr_loss / sum premium_prev`` across cohorts at duration ``k``).
The only addition is ``cohort_level``, the per-cohort height, estimated as the
median of ``(g_cohort / g_pooled)`` over the cohort's observed durations and
shrunk toward ``1.0`` (the global average level) by Buhlmann credibility of
strength ``credibility`` (K)::

    cohort_level = (n * raw_median + K * 1.0) / (n + K)

The projection is additive and anchored on the observed cumulative loss, exactly
like ED -- only the increment is scaled by ``cohort_level``. The denominator
(premium) is projected with the shared pooled premium intensity, so the loss
ratio is ``loss_proj / premium_proj``.

Uncertainty: an exact analytical SE is intractable here (the credibility-
estimated ``cohort_level`` carries its own variance, the ``cohort_level * g_k``
product, and their covariance all enter), and an approximation would understate
it. ``n_bootstrap`` therefore drives a residual bootstrap that refits the whole
pipeline (``g_k`` + ``cohort_level`` + projection) on resampled residuals and
adds process noise to future cells, capturing every source at once.

EXPERIMENTAL: not wired into the public API. It is ED plus one term; its
eventual home is an ``ExposureDriven`` option once validated on real data.
"""

from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass
from typing import TYPE_CHECKING

import numpy as np
import polars as pl

from ._io import mirror_output, normalize_groups

if TYPE_CHECKING:
    from ._io import FrameLike
    from .triangle import Triangle

# cells[cohort][duration] = (cum_loss, cum_premium, incr_loss, incr_premium)
_Cells = dict[object, dict[int, tuple[float, float, float, float]]]


def _smooth(curve: dict[int, float], window: int) -> dict[int, float]:
    """Rolling-mean smooth a duration-indexed curve (denoise lumpy books)."""
    keys = sorted(curve)
    vals = np.array([curve[k] for k in keys], dtype=np.float64)
    kernel = np.ones(window, dtype=np.float64) / window
    return dict(zip(keys, np.convolve(vals, kernel, mode="same")))


def _intensities(
    cells: _Cells, durations: list[int], smooth: int | None
) -> tuple[dict[int, float], dict[int, float]]:
    """Pooled loss and premium intensities (increment over prior cum premium)."""
    g_pool: dict[int, float] = {}
    gp_pool: dict[int, float] = {}
    for k in durations:
        if k - 1 not in durations:
            continue
        num_l = num_p = den = 0.0
        for c in cells.values():
            if k in c and (k - 1) in c and c[k - 1][1] > 0:
                num_l += c[k][2]
                num_p += c[k][3]
                den += c[k - 1][1]
        if den > 0:
            g_pool[k] = num_l / den
            gp_pool[k] = num_p / den
    if smooth and g_pool:
        g_pool = _smooth(g_pool, smooth)
        gp_pool = _smooth(gp_pool, smooth)
    return g_pool, gp_pool


def _levels(cells: _Cells, g_pool: dict[int, float], K: float) -> dict[object, float]:
    """Per-cohort level: median(g_cohort / g_pooled), Buhlmann-shrunk to 1.0."""
    level: dict[object, float] = {}
    for c, cdict in cells.items():
        ratios = []
        for k in sorted(cdict):
            gk = g_pool.get(k)
            prev = cdict.get(k - 1)
            if gk and gk != 0 and prev and prev[1] > 0:
                ratios.append((cdict[k][2] / prev[1]) / gk)
        if ratios:
            n = len(ratios)
            level[c] = (n * float(np.median(ratios)) + K * 1.0) / (n + K)
        else:
            level[c] = 1.0
    return level


def _residuals(
    cells: _Cells, level: dict[object, float], g_pool: dict[int, float]
) -> np.ndarray:
    """Pearson residuals of observed increments: (incr - fitted)/sqrt(prem_prev)."""
    res = []
    for c, cdict in cells.items():
        for k in sorted(cdict):
            gk = g_pool.get(k)
            prev = cdict.get(k - 1)
            if gk and prev and prev[1] > 0:
                fitted = level[c] * gk * prev[1]
                res.append((cdict[k][2] - fitted) / np.sqrt(prev[1]))
    arr = np.array(res, dtype=np.float64)
    # Center: the median-based level does not zero the mean residual, so an
    # uncentered pool would bias the bootstrap upward (loss is right-skewed).
    return arr - arr.mean() if arr.size else arr


@dataclass(kw_only=True)
class LevelExposureDriven:
    """Cohort-level exposure-driven (additive) loss projection (experimental).

    Parameters
    ----------
    credibility
        Buhlmann credibility strength ``K`` shrinking each ``cohort_level``
        toward ``1.0`` (the global average level). Larger ``K`` -> more
        shrinkage; ``0`` trusts each cohort's raw median fully. Default ``3.0``.
    smooth
        Optional rolling-mean window for the pooled intensities. ``None``
        (default) applies no smoothing; a small odd window (e.g. ``3``)
        denoises lumpy books.
    n_bootstrap
        Number of residual-bootstrap replicates for the CI. ``None`` (default)
        skips uncertainty (point projection only).
    conf_level
        Confidence level for the bootstrap CI. Default ``0.95``.
    seed
        Seed for the bootstrap RNG (reproducibility). Default ``None``.
    """

    credibility: float      = 3.0
    smooth:      int | None = None
    n_bootstrap: int | None = None
    conf_level:  float      = 0.95
    seed:        int | None = None

    def __post_init__(self) -> None:
        if self.credibility < 0:
            raise ValueError(f"credibility must be >= 0, got {self.credibility!r}")
        if self.smooth is not None and self.smooth < 1:
            raise ValueError(f"smooth must be a positive integer or None, got {self.smooth!r}")
        if self.n_bootstrap is not None and self.n_bootstrap < 1:
            raise ValueError(f"n_bootstrap must be a positive integer or None, got {self.n_bootstrap!r}")
        if not (0.0 < self.conf_level < 1.0):
            raise ValueError(f"conf_level must be in (0, 1), got {self.conf_level!r}")

    def fit(self, triangle: "Triangle") -> "LevelExposureDrivenFit":
        """Fit the cohort-level ED projection on a Triangle."""
        df = triangle.to_polars()
        gcols = normalize_groups(triangle.groups)
        rng = np.random.default_rng(self.seed)

        proj_parts, level_parts, gk_parts = [], [], []
        if gcols:
            for key, sub in df.group_by(gcols, maintain_order=True):
                key = key if isinstance(key, tuple) else (key,)
                proj, level, gk = self._fit_group(sub, rng)
                for col, val in zip(gcols, key):
                    proj = proj.with_columns(pl.lit(val).alias(col))
                    level = level.with_columns(pl.lit(val).alias(col))
                    gk = gk.with_columns(pl.lit(val).alias(col))
                proj_parts.append(proj); level_parts.append(level); gk_parts.append(gk)
        else:
            proj, level, gk = self._fit_group(df, rng)
            proj_parts.append(proj); level_parts.append(level); gk_parts.append(gk)

        lead = list(gcols)
        ratio_cols = ["ratio_proj"]
        if self.n_bootstrap:
            ratio_cols += ["ratio_lo", "ratio_hi"]
        proj_df = pl.concat(proj_parts).select(
            lead + ["cohort", "duration", "loss_proj", "premium_proj"] + ratio_cols
        )
        level_df = pl.concat(level_parts).select(lead + ["cohort", "cohort_level"])
        gk_df = pl.concat(gk_parts).select(lead + ["duration", "g_k"])

        return LevelExposureDrivenFit(
            df=proj_df, cohort_level=level_df, g_k=gk_df,
            output_type=triangle._output_type,
            credibility=self.credibility, smooth=self.smooth,
            n_bootstrap=self.n_bootstrap, conf_level=self.conf_level,
        )

    @staticmethod
    def _extract(sub: pl.DataFrame) -> _Cells:
        cells: _Cells = defaultdict(dict)
        for r in sub.sort(["cohort", "duration"]).iter_rows(named=True):
            cells[r["cohort"]][int(r["duration"])] = (
                r["loss"], r["premium"], r["incr_loss"], r["incr_premium"]
            )
        return cells

    def _project(
        self, cells: _Cells, level: dict[object, float],
        g_pool: dict[int, float], gp_pool: dict[int, float], max_dur: int,
        residuals: np.ndarray | None = None, rng: "np.random.Generator | None" = None,
    ) -> list[dict]:
        """Additive anchored projection; ``residuals`` adds process noise to future."""
        rows: list[dict] = []
        for c, cdict in cells.items():
            for k in sorted(cdict):
                cl, cp = cdict[k][0], cdict[k][1]
                rows.append({"cohort": c, "duration": k, "loss_proj": cl,
                             "premium_proj": cp, "ratio_proj": (cl / cp) if cp else None})
            last = max(cdict)
            cl, cp = cdict[last][0], cdict[last][1]
            for k in range(last + 1, max_dur + 1):
                gk, gpk = g_pool.get(k), gp_pool.get(k)
                if gk is None or gpk is None:
                    break
                prev_p = cp
                incr = level[c] * gk * prev_p
                if residuals is not None and rng is not None and prev_p > 0:
                    incr += float(rng.choice(residuals)) * np.sqrt(prev_p)
                cl = cl + incr
                cp = prev_p * (1.0 + gpk)
                rows.append({"cohort": c, "duration": k, "loss_proj": cl,
                             "premium_proj": cp, "ratio_proj": (cl / cp) if cp else None})
        return rows

    def _fit_group(
        self, sub: pl.DataFrame, rng: "np.random.Generator"
    ) -> tuple[pl.DataFrame, pl.DataFrame, pl.DataFrame]:
        cells = self._extract(sub)
        durations = sorted({d for c in cells.values() for d in c})
        max_dur = max(durations)
        g_pool, gp_pool = _intensities(cells, durations, self.smooth)
        level = _levels(cells, g_pool, self.credibility)

        proj_df = pl.DataFrame(self._project(cells, level, g_pool, gp_pool, max_dur))

        if self.n_bootstrap:
            ci = self._bootstrap(cells, durations, max_dur, rng)
            proj_df = proj_df.join(ci, on=["cohort", "duration"], how="left")
        proj_df = proj_df.sort(["cohort", "duration"])

        level_df = pl.DataFrame({"cohort": list(level), "cohort_level": list(level.values())})
        gk_df = pl.DataFrame({"duration": list(g_pool), "g_k": list(g_pool.values())}).sort("duration")
        return proj_df, level_df, gk_df

    def _bootstrap(
        self, cells: _Cells, durations: list[int], max_dur: int,
        rng: "np.random.Generator",
    ) -> pl.DataFrame:
        """Residual bootstrap CI: refit on resampled residuals + process noise."""
        g_pool, gp_pool = _intensities(cells, durations, self.smooth)
        level = _levels(cells, g_pool, self.credibility)
        res = _residuals(cells, level, g_pool)
        samples: dict[tuple, list[float]] = defaultdict(list)
        if res.size == 0:
            return pl.DataFrame({"cohort": [], "duration": [], "ratio_lo": [], "ratio_hi": []})

        for _ in range(int(self.n_bootstrap)):
            # Re-inject noise into observed increments -> pseudo cumulative loss.
            pseudo: _Cells = {}
            for c, cdict in cells.items():
                ks = sorted(cdict)
                cum = cdict[ks[0]][0]
                pc: dict[int, tuple] = {ks[0]: cdict[ks[0]]}
                for k in ks[1:]:
                    prev_p = cdict[k - 1][1]
                    fitted = level[c] * g_pool.get(k, 0.0) * prev_p
                    pincr = fitted + float(rng.choice(res)) * np.sqrt(prev_p) if prev_p > 0 else cdict[k][2]
                    cum = pc[k - 1][0] + pincr
                    pc[k] = (cum, cdict[k][1], pincr, cdict[k][3])
                pseudo[c] = pc
            g_b, gp_b = _intensities(pseudo, durations, self.smooth)
            lvl_b = _levels(pseudo, g_b, self.credibility)
            for row in self._project(pseudo, lvl_b, g_b, gp_b, max_dur, residuals=res, rng=rng):
                if row["ratio_proj"] is not None:
                    samples[(row["cohort"], row["duration"])].append(row["ratio_proj"])

        lo_q = (1.0 - self.conf_level) / 2.0
        hi_q = 1.0 - lo_q
        out = {"cohort": [], "duration": [], "ratio_lo": [], "ratio_hi": []}
        for (c, k), vals in samples.items():
            arr = np.array(vals)
            out["cohort"].append(c); out["duration"].append(k)
            out["ratio_lo"].append(float(np.quantile(arr, lo_q)))
            out["ratio_hi"].append(float(np.quantile(arr, hi_q)))
        return pl.DataFrame(out)


class LevelExposureDrivenFit:
    """Result of :meth:`LevelExposureDriven.fit` (experimental).

    Attributes
    ----------
    df
        Full projection grid: ``[<groups>, cohort, duration, loss_proj,
        premium_proj, ratio_proj]`` (plus ``ratio_lo`` / ``ratio_hi`` when
        ``n_bootstrap`` is set). Observed cells carry observed values; cells
        beyond a cohort's last observation are projected.
    cohort_level
        Per-cohort estimated level (``[<groups>, cohort, cohort_level]``).
    g_k
        Pooled per-duration intensity / shape (``[<groups>, duration, g_k]``).
    """

    def __init__(
        self, *, df: pl.DataFrame, cohort_level: pl.DataFrame, g_k: pl.DataFrame,
        output_type: str, credibility: float, smooth: int | None,
        n_bootstrap: int | None, conf_level: float,
    ) -> None:
        self._df = df
        self._cohort_level = cohort_level
        self._g_k = g_k
        self._output_type = output_type
        self.credibility = credibility
        self.smooth = smooth
        self.n_bootstrap = n_bootstrap
        self.conf_level = conf_level

    @property
    def df(self) -> "FrameLike":
        """Full projection grid (with CI columns when bootstrapped)."""
        return mirror_output(self._df, self._output_type)

    @property
    def cohort_level(self) -> "FrameLike":
        """Per-cohort estimated level."""
        return mirror_output(self._cohort_level, self._output_type)

    @property
    def g_k(self) -> "FrameLike":
        """Pooled per-duration intensity (shape)."""
        return mirror_output(self._g_k, self._output_type)

    def summary(self) -> "FrameLike":
        """Per-(group, cohort) latest projected ratio at the last duration."""
        keys = [c for c in self._df.columns
                if c not in ("cohort", "duration", "loss_proj", "premium_proj",
                             "ratio_proj", "ratio_lo", "ratio_hi")] + ["cohort"]
        latest = (
            self._df.sort("duration").group_by(keys, maintain_order=True).last()
            .rename({"duration": "duration_last", "ratio_proj": "ratio_latest"})
        )
        return mirror_output(latest, self._output_type)

    def __repr__(self) -> str:
        return (f"LevelExposureDrivenFit(cells={self._df.height}, "
                f"cohorts={self._cohort_level.height}, credibility={self.credibility}, "
                f"smooth={self.smooth}, n_bootstrap={self.n_bootstrap})")
