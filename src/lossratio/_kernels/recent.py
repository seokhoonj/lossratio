"""Recent-diagonal (calendar-wedge) factor-estimation filter.

The ``recent`` argument restricts *which cells contribute to factor
estimation* (``f_k`` / ``g_k``) to the most-recent ``N`` calendar
diagonals — a right-bottom wedge of the triangle. The point projection
still covers the full ``cohort x duration`` grid; only the cells feeding the
per-link factor estimate are masked.

This is a calendar-diagonal filter, *not* a cohort cut: young
early-duration cells of older cohorts still leak into the wedge (that is
intentional — strict cohort-axis cuts are the separate ``regime``
argument).

The numpy kernels here work on the ``(n_cohorts, n_durations)`` value matrix,
so this helper produces a boolean *link-level fit mask* of shape
``(n_cohorts, n_durations - 1)``: entry ``[i, k]`` is the link from duration
``k + 1`` to duration ``k + 2`` of cohort ``i``. The filter keys on the
link's *source* cell ``(i, k)`` -- i.e. the calendar diagonal of the
duration the link starts from.
"""

from __future__ import annotations

import numpy as np


def validate_recent(recent: int | None) -> None:
    """Validate the ``recent`` argument shared by every estimator.

    ``None`` (no filter) is accepted; otherwise ``recent`` must be a
    positive integer. Raises :class:`ValueError` on anything else.
    """
    if recent is None:
        return
    if (
        not isinstance(recent, (int, np.integer))
        or isinstance(recent, bool)
        or recent < 1
    ):
        raise ValueError(
            f"`recent` must be None or a positive integer, got {recent!r}"
        )


def recent_link_mask(
    value_obs: np.ndarray,
    recent: int | None,
) -> np.ndarray | None:
    """Boolean link-level fit mask for the recent-``N`` calendar wedge.

    A *link* ``k`` of cohort ``i`` runs from duration ``k + 1`` to duration
    ``k + 2``; it exists when both endpoint cells ``(i, k)`` and
    ``(i, k + 1)`` are observed. The recent filter keys on the link's
    *source* duration; the calendar index of that source cell is::

        cal_idx = coh_rank + duration_from - 1 = (i + 1) + (k + 1) - 1
                = i + k + 1

    A link is kept (``True``) when ``cal_idx > max_cal - recent``,
    where ``max_cal`` is the largest source calendar index over every
    *existing* link — the latest calendar diagonal among the links
    themselves (not the full triangle).

    Parameters
    ----------
    value_obs
        Observed value matrix, shape ``(n_cohorts, n_durations)``; cells are
        ``NaN`` where unobserved.
    recent
        ``None`` (no filter) or a positive integer.

    Returns
    -------
    np.ndarray | None
        ``None`` when ``recent`` is ``None`` (the no-filter path);
        otherwise a boolean array of shape ``(n_cohorts, n_durations - 1)``
        — ``True`` for links inside the recent-diagonal wedge.
    """
    if recent is None:
        return None
    validate_recent(recent)

    n_cohorts, n_durations = value_obs.shape
    n_links = n_durations - 1
    if n_links < 1:
        return np.zeros((n_cohorts, 0), dtype=bool)

    observed = np.isfinite(value_obs)
    # A link exists when both endpoint cells are observed.
    link_exists = observed[:, :-1] & observed[:, 1:]

    i = np.arange(n_cohorts).reshape(-1, 1)
    k = np.arange(n_links).reshape(1, -1)
    # Calendar index of the link's source cell.
    cal_idx = i + k + 1

    if not link_exists.any():
        return np.zeros((n_cohorts, n_links), dtype=bool)

    max_cal = int(cal_idx[link_exists].max())
    return link_exists & (cal_idx > (max_cal - int(recent)))
