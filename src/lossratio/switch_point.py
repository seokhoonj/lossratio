"""Backtest-selected ED->CL switch point.

``SwitchPoint`` locates the development period at which the stage-adaptive
loss model should hand off from the exposure-driven (ED) stage to the chain
ladder (CL) stage. Unlike the deprecated CV/RSE stability heuristic, the
switch is the BOUNDARY that minimises out-of-sample loss-projection error --
"where CL starts to out-predict ED" -- found by calendar-diagonal
backtesting COMPLETE ``SA(k)`` models (ED for ``duration < k``, CL for
``duration >= k``), NOT a per-link CL-vs-ED error crossing.

The selection is deliberately conservative (so a switch is only taken when
the evidence is real, not a denominator-effect artifact):

* pure ED and pure CL are baselines; CL replaces ED only if it beats it by
  ``min_improve`` at the deep holdout AND also beats it at the guard holdout;
* a mid-``k`` switch replaces the pure baseline only under the same
  two-holdout test;
* ties prefer the simpler pure model;
* groups with too few evaluable held-out cells are DEFERRED (``point`` is
  ``None`` -- no auto-switch; the caller should roll up to a coarser
  aggregate or specify the switch by hand).

``point`` encodes the switch directly: ``None`` = no switch (pure ED),
``1`` = switch at the first link (pure CL), ``k >= 2`` = ED before ``k``
and CL from ``k`` on.
"""

from __future__ import annotations

from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import polars as pl

from ._io import mirror_output, normalize_groups, set_group_values

if TYPE_CHECKING:
    from .triangle import Triangle

# Selection defaults.
_MIN_IMPROVE = 0.05   # relative-WAPE noise floor for "robustly better"
_MIN_EVAL = 30        # min common held-out cells to auto-select (else defer)
_MAX_CANDS = 10       # cap on the mid-k candidate grid (cost control)


# ---------------------------------------------------------------------------
# Selection engine
# ---------------------------------------------------------------------------


def _default_grid(duration_max: int) -> list[int]:
    """Strided mid-k candidate grid in ``[2, duration_max - 2]`` (<= _MAX_CANDS)."""
    hi = max(2, duration_max - 2)
    cand = list(range(2, hi + 1))
    if len(cand) > _MAX_CANDS:
        step = len(cand) / _MAX_CANDS
        cand = sorted({cand[int(i * step)] for i in range(_MAX_CANDS)})
    return cand


def _default_holdouts(duration_max: int) -> list[int]:
    """Two evaluation depths: a guard (shallow) and a deep (long horizon)."""
    deep = max(3, duration_max // 3)
    guard = max(2, duration_max // 5)
    if guard >= deep:
        guard = max(2, deep - 2)
    return sorted({guard, deep}) or [deep]


def _per_group_wape(
    df_model: pl.DataFrame, common: pl.DataFrame, gcols: list[str]
) -> dict[Any, tuple[float | None, int]]:
    """WAPE = ``sum|A-E| / sum A`` per group on the common held-out cells."""
    d = df_model.join(common, on=[*gcols, "cohort", "duration"], how="inner")
    d = d.with_columns((pl.col("actual") - pl.col("expected")).abs().alias("_abs"))
    if gcols:
        agg = d.group_by(gcols).agg(
            pl.len().alias("n"),
            (pl.col("_abs").sum() / pl.col("actual").sum()).alias("wape"),
        )
        out: dict[Any, tuple[float | None, int]] = {}
        for r in agg.iter_rows(named=True):
            key = r[gcols[0]] if len(gcols) == 1 else tuple(r[c] for c in gcols)
            out[key] = (r["wape"], r["n"])
        return out
    total = d["actual"].sum()
    wape = (d["_abs"].sum() / total) if total else None
    return {None: (wape, d.height)}


def _candidate_wape(
    triangle: "Triangle", cands: dict[str, Any], h: int, gcols: list[str]
) -> dict[tuple[Any, str], tuple[float | None, int]]:
    """Per-(group, candidate) WAPE at one holdout, on common-reachable cells."""
    from .backtest import Backtest

    tables: dict[str, pl.DataFrame] = {}
    for name, est in cands.items():
        ae = Backtest(estimator=est, holdout=h, target="loss").fit(triangle)
        df = ae.ae_err
        tables[name] = df.select([*gcols, "cohort", "duration", "actual", "expected"])

    common: pl.DataFrame | None = None
    for df in tables.values():
        keys = df.select([*gcols, "cohort", "duration"]).unique()
        common = keys if common is None else common.join(
            keys, on=[*gcols, "cohort", "duration"], how="inner"
        )

    per: dict[tuple[Any, str], tuple[float | None, int]] = {}
    for name, df in tables.items():
        for gkey, (w, n) in _per_group_wape(df, common, gcols).items():
            per[(gkey, name)] = (w, n)
    return per


def _select_switch(
    triangle: "Triangle",
    *,
    alpha: float = 1.0,
    sigma_method: str = "locf",
    recent: int | None = None,
    regime: Any = None,
    k_grid: list[int] | None = None,
    holdouts: list[int] | None = None,
    min_improve: float = _MIN_IMPROVE,
    min_eval: int = _MIN_EVAL,
) -> dict[Any, tuple[int | None, str]]:
    """Per-group ``(point, status)``.

    ``point``: ``None`` (no switch) / ``1`` (CL) / ``k`` (mid switch).
    ``status``: ``"ed"`` / ``"cl"`` / ``"switch"`` / ``"deferred"`` -- a
    deferred group also carries ``point=None`` but is distinct from a
    positively-validated ``"ed"`` (it means too few cells to decide).

    The candidate models inherit the calling estimator's ``alpha`` /
    ``sigma_method`` / ``recent`` / ``regime`` so the switch is selected
    under the SAME filtering / regime-borrow regime it will be used in.
    """
    from .chain_ladder import ChainLadder
    from .exposure_driven import ExposureDriven
    from .stage_adaptive import StageAdaptive

    gcols = normalize_groups(triangle._groups)
    duration_max = int(triangle._df["duration"].max())
    k_grid = k_grid or _default_grid(duration_max)
    holdouts = sorted(holdouts or _default_holdouts(duration_max))
    h_deep = holdouts[-1]
    h_guard = holdouts[-2] if len(holdouts) > 1 else holdouts[-1]

    cfg = dict(alpha=alpha, sigma_method=sigma_method, recent=recent, regime=regime)
    cands: dict[str, Any] = {
        "ED": ExposureDriven(**cfg),
        "CL": ChainLadder(**cfg),
    }
    for k in k_grid:
        cands[f"k{k}"] = StageAdaptive(switch=SwitchPoint.at(k), **cfg)

    deep = _candidate_wape(triangle, cands, h_deep, gcols)
    guard = _candidate_wape(triangle, cands, h_guard, gcols)

    def robust_better(better: str, base: str, g: Any) -> bool:
        bd = deep.get((g, better), (None,))[0]
        ad = deep.get((g, base), (None,))[0]
        bg = guard.get((g, better), (None,))[0]
        ag = guard.get((g, base), (None,))[0]
        if None in (bd, ad, bg, ag) or ad == 0:
            return False
        return bd <= ad * (1 - min_improve) and bg < ag

    groups = sorted({g for (g, _) in deep}, key=lambda x: (x is not None, str(x)))
    out: dict[Any, tuple[int | None, str]] = {}
    for g in groups:
        ed_d, n = deep.get((g, "ED"), (None, 0))
        if ed_d is None or ed_d == 0 or n < min_eval:
            out[g] = (None, "deferred")  # too few cells -> no auto-switch
            continue
        # pure baseline: better of ED / CL (CL only if robustly better)
        pure = "CL" if robust_better("CL", "ED", g) else "ED"
        chosen = pure
        midk = [(int(nm[1:]), deep[(g, nm)][0]) for nm in cands
                if nm.startswith("k") and deep.get((g, nm), (None,))[0] is not None]
        if midk:
            bk = min(midk, key=lambda x: x[1])[0]
            if robust_better(f"k{bk}", pure, g):
                chosen = f"k{bk}"
        if chosen == "ED":
            out[g] = (None, "ed")
        elif chosen == "CL":
            out[g] = (1, "cl")
        else:
            out[g] = (int(chosen[1:]), "switch")
    return out


# ---------------------------------------------------------------------------
# Public result class
# ---------------------------------------------------------------------------


_SP_COLS: tuple[str, ...] = ("point",)


class SwitchPoint:
    """Backtest-selected ED->CL switch point for the stage-adaptive model.

    Build via :meth:`detect` (lazy, auto-selected by backtest on the
    consumer's own triangle -- leakage-safe inside a backtest fold) or
    :meth:`at` (eager, an explicit fixed switch).

    Properties
    ----------
    point :
        Selected switch. ``None`` (no groups) or ``dict[group, int | None]``
        (groups set). Value semantics: ``None`` = no switch (pure ED),
        ``1`` = switch at the first link (pure CL), ``k >= 2`` = ED before
        ``k`` and CL from ``k`` on.
    """

    def __init__(self) -> None:
        raise TypeError(
            "SwitchPoint is produced by `SwitchPoint.detect()` / "
            "`SwitchPoint.at()`, not a direct constructor."
        )

    @classmethod
    def _from_selection(
        cls,
        selection: dict[Any, tuple[int | None, str]],
        groups: "str | Sequence[str] | None",
        output_type: str,
    ) -> "SwitchPoint":
        self = cls.__new__(cls)
        self._output_type = output_type
        self._groups = groups if groups else None
        self._selection = selection
        return self

    @staticmethod
    def _status_for(point: int) -> str:
        return "cl" if point == 1 else "switch"

    @classmethod
    def _manual(
        cls,
        *,
        point: list[int],
        groups: Mapping[str, Sequence[Any]] | None,
    ) -> "SwitchPoint":
        self = cls.__new__(cls)
        self._output_type = "polars"
        sel = [(p, cls._status_for(p)) for p in point]
        if groups:
            cols = list(groups)
            keys = (
                list(groups[cols[0]]) if len(cols) == 1
                else [tuple(vals) for vals in zip(*(groups[c] for c in cols))]
            )
            self._groups = cols[0] if len(cols) == 1 else cols
            self._selection = dict(zip(keys, sel))
        else:
            self._groups = None
            self._selection = {None: sel[0]}
        return self

    @classmethod
    def at(
        cls,
        point: int | Sequence[int],
        *,
        groups: Mapping[str, Sequence[Any]] | None = None,
    ) -> "SwitchPoint":
        """Build a :class:`SwitchPoint` from an explicit, fixed switch.

        ``point`` semantics match :attr:`point`: ``1`` = pure CL,
        ``k >= 2`` = ED before ``k`` / CL after. A single int (applied to
        all groups) or a sequence aligned 1:1 with ``groups``.
        """
        def _check(v: Any) -> int:
            # `point` semantics: 1 = pure CL, k >= 2 = switch. Reject bools
            # (a bool is an int subclass) and anything below 1.
            if isinstance(v, bool):
                raise TypeError("`point` must not be a bool")
            if not isinstance(v, int):
                raise TypeError(
                    f"`point` must be int, got {type(v).__name__}"
                )
            if v < 1:
                raise ValueError(
                    f"`point` must be >= 1 (1 = pure CL, k >= 2 = switch), "
                    f"got {v}"
                )
            return int(v)

        if isinstance(point, bool):
            raise TypeError("`point` must not be a bool")
        if isinstance(point, int):
            seq = [_check(point)]
        elif isinstance(point, Sequence) and not isinstance(point, str):
            if not point:
                raise ValueError("`point` must have length >= 1")
            seq = [_check(v) for v in point]
        else:
            raise TypeError(
                f"`point` must be int or Sequence[int], got {type(point).__name__}"
            )
        groups = dict(groups) if groups else {}
        for col, vals in groups.items():
            if not isinstance(vals, Sequence) or isinstance(vals, str):
                groups[col] = [vals]
            if len(groups[col]) != len(seq):
                raise ValueError(
                    f"All arguments must have equal length; `point`={len(seq)} "
                    f"but `groups[{col!r}]`={len(groups[col])}"
                )
        return cls._manual(point=seq, groups=groups or None)

    @classmethod
    def detect(
        cls,
        *,
        k_grid: list[int] | None = None,
        holdouts: list[int] | None = None,
        min_improve: float = _MIN_IMPROVE,
        min_eval: int = _MIN_EVAL,
    ) -> "_SwitchSpec":
        """Build a lazy switch-detection spec (selection parameters only).

        Returns a :class:`_SwitchSpec` -- a callable ``spec(triangle) ->
        SwitchPoint`` that runs the backtest selection on the triangle it is
        given (inside a backtest this is the MASKED training triangle, so the
        switch never peeks at held-out cells).

        The spec holds only the SELECTION parameters (``k_grid`` / ``holdouts``
        / ``min_improve`` / ``min_eval``). The candidate models' ``alpha`` /
        ``sigma_method`` / ``recent`` / ``regime`` are injected by the
        CONSUMER at resolution time -- ``StageAdaptive(switch=...)`` passes its
        own settings; standalone callers may pass them as keyword arguments to
        the spec (``spec(triangle, regime=...)``).
        """
        return _SwitchSpec(
            k_grid=k_grid, holdouts=holdouts,
            min_improve=min_improve, min_eval=min_eval,
        )

    @property
    def point(self):
        """Selected switch point (``None`` / ``1`` / ``k``); see class doc."""
        if self._groups is None:
            return self._selection.get(None, (None, "deferred"))[0]
        return {k: v[0] for k, v in self._selection.items()}

    @property
    def status(self):
        """Selection status: ``"ed"`` / ``"cl"`` / ``"switch"`` / ``"deferred"``.

        Distinguishes a positively-validated pure-ED choice (``"ed"``) from a
        group that could not be decided for lack of evaluable held-out cells
        (``"deferred"``) -- both carry ``point=None`` but mean different things.
        """
        if self._groups is None:
            return self._selection.get(None, (None, "deferred"))[1]
        return {k: v[1] for k, v in self._selection.items()}

    def summary(self):
        """One-row-per-group frame ``[groups?, point, status]``."""
        rows = []
        if self._groups is None:
            pt, st = self._selection.get(None, (None, "deferred"))
            rows.append({"point": pt, "status": st})
        else:
            for key, (pt, st) in self._selection.items():
                row: dict[str, Any] = {}
                set_group_values(row, self._groups, key)
                row["point"] = pt
                row["status"] = st
                rows.append(row)
        return mirror_output(pl.DataFrame(rows), self._output_type)

    def __repr__(self) -> str:
        if self._groups is None:
            return f"<SwitchPoint: point={self.point} ({self.status})>"
        return f"<SwitchPoint: {len(self._selection)} groups>"


@dataclass(frozen=True)
class _SwitchSpec:
    """Lazy switch-detection spec produced by :meth:`SwitchPoint.detect`.

    Holds only the selection parameters; the candidate models' modelling
    config (``alpha`` / ``sigma_method`` / ``recent`` / ``regime``) is
    supplied by the consumer when the spec is resolved, so the switch is
    selected under the SAME regime / filtering the model will run in.
    """

    k_grid: list[int] | None = None
    holdouts: list[int] | None = None
    min_improve: float = _MIN_IMPROVE
    min_eval: int = _MIN_EVAL

    def __call__(
        self,
        triangle: "Triangle",
        *,
        alpha: float = 1.0,
        sigma_method: str = "locf",
        recent: int | None = None,
        regime: Any = None,
    ) -> "SwitchPoint":
        sel = _select_switch(
            triangle, alpha=alpha, sigma_method=sigma_method, recent=recent,
            regime=regime, k_grid=self.k_grid, holdouts=self.holdouts,
            min_improve=self.min_improve, min_eval=self.min_eval,
        )
        return SwitchPoint._from_selection(
            sel, triangle._groups, triangle._output_type
        )
