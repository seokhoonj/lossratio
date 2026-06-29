"""Shared fixtures for the test suite."""

from __future__ import annotations

import polars as pl
import pytest

import lossratio as lr


@pytest.fixture(scope="module")
def exp() -> pl.DataFrame:
    """Raw synthetic experience frame."""
    return lr.load_experience()


@pytest.fixture(scope="module")
def tri() -> lr.Triangle:
    """Coverage-grouped triangle over the full synthetic experience."""
    return lr.Triangle(lr.load_experience(), groups="coverage")


@pytest.fixture
def sample_triangle():
    """Factory for a SURGERY+CI triangle: grouped (default) or SURGERY-only."""
    def _make(group: bool = True) -> lr.Triangle:
        exp = lr.load_experience().filter(pl.col("coverage").is_in(["SURGERY", "CI"]))
        if group:
            return lr.Triangle(exp, groups="coverage")
        return lr.Triangle(exp.filter(pl.col("coverage") == "SURGERY"))
    return _make


@pytest.fixture
def toy_input() -> pl.DataFrame:
    """5-cohort, 5-duration toy experience with finite premium."""
    return pl.DataFrame(
        {
            "cy_m": [
                "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-03-01", "2024-04-01", "2024-05-01",
                "2024-04-01", "2024-05-01",
                "2024-05-01",
            ],
            "uy_m": [
                "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01",
                "2024-02-01", "2024-02-01", "2024-02-01", "2024-02-01",
                "2024-03-01", "2024-03-01", "2024-03-01",
                "2024-04-01", "2024-04-01",
                "2024-05-01",
            ],
            "incr_loss": [
                100.0, 100.0, 120.0, 100.0, 80.0,
                150.0, 130.0, 160.0, 130.0,
                120.0, 130.0, 130.0,
                180.0, 190.0,
                200.0,
            ],
            "incr_premium": [100.0] * 15,
        }
    )
