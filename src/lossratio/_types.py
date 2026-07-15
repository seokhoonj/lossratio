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
    from .diagnostics.regime import Regime, RegimeDetector

    # Regime (cohort-axis) filter: a concrete Regime, a deferred
    # RegimeDetector (detected at fit / backtest time), or None (no filter).
    RegimeArg: TypeAlias = Regime | RegimeDetector | None

# The x-axis of a cohort x period heatmap: the duration index or the
# synthesised calendar period.
XAxis: TypeAlias = Literal["duration", "calendar"]

# Cell-label detail in the triangle value heatmap: the metric alone, or the
# metric plus its loss / premium breakdown.
LabelStyle: TypeAlias = Literal["plain", "detail"]
