"""Shared type aliases for the polymorphic dispatch arguments.

The public estimators accept a few arguments whose runtime ``_resolve_*``
dispatchers enumerate an exact, closed union (a strategy object, a lazy
``Callable`` recipe, the ``"auto"`` sentinel, or ``None``). These aliases
name that contract once so the annotations stay precise and the shipped
``py.typed`` marker carries real information.

The aliases live under ``TYPE_CHECKING`` only: with ``from __future__
import annotations`` every annotation is a string that is never evaluated
at runtime, so referencing these names costs nothing at import time and
cannot introduce an import cycle.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from collections.abc import Callable
    from typing import Literal

    from .maturity import Maturity
    from .regime import Regime
    from .tail import Tail
    from .triangle import Triangle
    from .uncertainty import Analytical, MonteCarlo, ResidualBootstrap

    # Uncertainty strategy: a strategy instance, a callable producing one,
    # or None (the fit keeps its closed-form analytical SE).
    UncertaintyArg = (
        Analytical
        | ResidualBootstrap
        | MonteCarlo
        | Callable[[Triangle], object]
        | None
    )

    # Maturity switch: a concrete Maturity, a lazy recipe, the "auto"
    # auto-detect sentinel, or None (no switch).
    MaturityArg = (
        Maturity | Callable[[Triangle], Maturity] | Literal["auto"] | None
    )

    # Regime (cohort-axis) filter: a concrete Regime, a lazy recipe, the
    # "auto" sentinel, or None (no filter).
    RegimeArg = (
        Regime | Callable[[Triangle], Regime] | Literal["auto"] | None
    )

    # Tail extrapolation: off (False), an explicit multiplicative factor,
    # or a Tail spec (True resolves to the default Tail()).
    TailArg = bool | float | Tail
