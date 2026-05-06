"""Sanity tests for the placeholder package."""

import lossratio


def test_import():
    assert lossratio is not None


def test_version_attribute_present():
    assert hasattr(lossratio, "__version__")
    assert isinstance(lossratio.__version__, str)
    assert len(lossratio.__version__) > 0
