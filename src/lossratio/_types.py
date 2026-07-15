"""Shared type aliases for the polymorphic dispatch arguments.

The public estimators accept a few arguments whose runtime ``_resolve_*``
dispatchers enumerate an exact, closed union (a strategy object, a deferred
detector config, or ``None``). These aliases name that contract once so the
annotations stay precise and the shipped ``py.typed`` marker carries real
information.

The aliases live under ``TYPE_CHECKING`` only: with ``from __future__
import annotations`` every annotation is a string that is never evaluated
at runtime, so referencing these names costs nothing at import time and
cannot introduce an import cycle.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Literal, TypeAlias

if TYPE_CHECKING:
    from datetime import date

    from .diagnostics.regime import Regime, RegimeDetector

    # Regime (cohort-axis) filter: a concrete Regime, a deferred
    # RegimeDetector (detected at fit / backtest time), a raw change date
    # (one break, all segments) or a per-segment mapping of change dates, or
    # None (no filter). The runtime ``_resolve_*`` dispatchers accept exactly
    # this closed union.
    RegimeArg: TypeAlias = Regime | RegimeDetector | date | dict[str, date] | None

# The consumption knob for a resolved Regime: keep only the latest regime,
# fit each regime segment on its own cohorts, or carry a covariate cut.
Treatment: TypeAlias = Literal["latest_only", "segment_wise", "covariate"]

# The x-axis of a cohort x period heatmap: the duration index or the
# synthesised calendar period.
XAxis: TypeAlias = Literal["duration", "calendar"]

# Cell-label detail in the triangle value heatmap: the metric alone, or the
# metric plus its loss / premium breakdown.
LabelStyle: TypeAlias = Literal["plain", "detail"]
