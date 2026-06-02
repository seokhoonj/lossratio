"""Unit tests for the multi-column `groups` foundation helpers (Phase: groups).

These pin the polymorphic str | Sequence[str] surface added in the foundation
commit; the model/diagnostic sweep and the Triangle representation flip build
on them.
"""

from __future__ import annotations

import numpy as np
import polars as pl

from lossratio._io import (
    _arrays_to_long_df,
    _iter_group_frames,
    fill_group_columns,
    format_group_value,
    group_eq,
    normalize_groups,
    set_group_values,
)


def test_format_group_value():
    assert format_group_value(None) == ""
    assert format_group_value("SUR") == "SUR"
    assert format_group_value(("SUR", "TM")) == "SUR | TM"
    assert format_group_value(6) == "6"


def test_normalize_groups():
    assert normalize_groups(None) == []
    assert normalize_groups("coverage") == ["coverage"]
    assert normalize_groups(["coverage", "channel"]) == ["coverage", "channel"]
    assert normalize_groups(("a", "b")) == ["a", "b"]


def _df() -> pl.DataFrame:
    return pl.DataFrame(
        {
            "coverage": ["SUR", "SUR", "CI", "CI"],
            "channel":  ["TM", "GA", "TM", "GA"],
            "x":        [1, 2, 3, 4],
        }
    )


def test_group_eq_single_col():
    df = _df()
    out = df.filter(group_eq("coverage", "CI"))
    assert out["x"].to_list() == [3, 4]


def test_group_eq_multi_col():
    df = _df()
    out = df.filter(group_eq(["coverage", "channel"], ("CI", "TM")))
    assert out["x"].to_list() == [3]


def test_iter_group_frames_none():
    df = _df()
    items = list(_iter_group_frames(df, None))
    assert len(items) == 1
    key, sub = items[0]
    assert key is None
    assert sub.height == 4


def test_iter_group_frames_single_col_yields_scalar():
    df = _df()
    items = list(_iter_group_frames(df, "coverage"))
    keys = [k for k, _ in items]
    # first-seen order, scalar keys
    assert keys == ["SUR", "CI"]
    assert all(isinstance(k, str) for k in keys)


def test_iter_group_frames_multi_col_yields_tuple():
    df = _df()
    items = list(_iter_group_frames(df, ["coverage", "channel"]))
    keys = [k for k, _ in items]
    assert keys == [("SUR", "TM"), ("SUR", "GA"), ("CI", "TM"), ("CI", "GA")]
    assert all(isinstance(k, tuple) and len(k) == 2 for k in keys)


def test_arrays_to_long_df_single_col():
    out = _arrays_to_long_df(
        {"dev": np.array([1, 2]), "v": np.array([1.0, 2.0])},
        groups="coverage",
        group_value="SUR",
    )
    assert out.columns == ["coverage", "dev", "v"]
    assert out["coverage"].to_list() == ["SUR", "SUR"]


def test_arrays_to_long_df_multi_col():
    out = _arrays_to_long_df(
        {"dev": np.array([1, 2]), "v": np.array([1.0, 2.0])},
        groups=["coverage", "channel"],
        group_value=("SUR", "TM"),
    )
    assert out.columns == ["coverage", "channel", "dev", "v"]
    assert out["coverage"].to_list() == ["SUR", "SUR"]
    assert out["channel"].to_list() == ["TM", "TM"]


def test_fill_group_columns():
    d: dict = {}
    fill_group_columns(d, None, None, 3)
    assert d == {}
    fill_group_columns(d, "coverage", "SUR", 2)
    assert d == {"coverage": ["SUR", "SUR"]}
    d2: dict = {}
    fill_group_columns(d2, ["coverage", "channel"], ("CI", "GA"), 2)
    assert d2 == {"coverage": ["CI", "CI"], "channel": ["GA", "GA"]}


def test_set_group_values():
    r: dict = {}
    set_group_values(r, None, None)
    assert r == {}
    set_group_values(r, "coverage", "SUR")
    assert r == {"coverage": "SUR"}
    r2: dict = {}
    set_group_values(r2, ["coverage", "channel"], ("CI", "GA"))
    assert r2 == {"coverage": "CI", "channel": "GA"}
