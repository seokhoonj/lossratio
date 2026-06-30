

def test_detect_regime_output_is_usable_in_a_fit():
    # the detect_regime() -> fit workflow: passing the Regime object (or a
    # RegimeDetector) to an estimator must resolve to the same cut as the
    # manual dict.
    import lossratio as lr
    from lossratio.diagnostics.regime import _resolve_regime

    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    reg = tri.detect_regime(target="ratio")

    resolved = _resolve_regime(reg, tri)
    assert isinstance(resolved, dict) and resolved   # {coverage: date}

    a = lr.PooledLoss(regime=reg).fit(tri).to_polars()
    b = lr.PooledLoss(regime=resolved).fit(tri).to_polars()
    assert a.equals(b)                                # Regime obj == manual dict

    # a default RegimeDetector and a Regime also flow through premium / ratio /
    # backtest without error
    lr.PooledLoss(regime=lr.RegimeDetector()).fit(tri)
    lr.PooledPremium(regime=reg).fit(tri)
    lr.Ratio(loss=lr.PooledLoss(regime=reg), premium=lr.PooledPremium()).fit(tri)
    lr.Backtest(estimator=lr.PooledLoss(regime=reg), holdouts=4, target="loss").fit(tri)


def test_regime_detector_flows_into_a_fit():
    # A deferred RegimeDetector must be accepted by the estimator and resolved
    # at fit time -- so backtest detects per masked fold (leakage-safe). An
    # invalid type is rejected at construction.
    import lossratio as lr
    import pytest

    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    det = lr.RegimeDetector(window=12)

    lr.PooledLoss(regime=det).fit(tri)
    lr.PooledPremium(regime=det).fit(tri)
    lr.Ratio(loss=lr.PooledLoss(regime=det), premium=lr.PooledPremium()).fit(tri)
    lr.Backtest(estimator=lr.PooledLoss(regime=det), holdouts=4,
                target="loss").fit(tri)

    # a bare callable is no longer a valid regime input (use RegimeDetector)
    with pytest.raises(TypeError, match="regime must be"):
        lr.PooledLoss(regime=lambda t: 42)
    with pytest.raises(TypeError, match="regime must be"):
        lr.PooledLoss(regime=5)


def test_regime_string_is_rejected():
    # regime strings (the old "auto" sentinel included) are no longer accepted;
    # use a RegimeDetector. The error is clear both at the resolver and at
    # estimator construction.
    import lossratio as lr
    import pytest
    from lossratio.diagnostics.regime import _resolve_regime

    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    with pytest.raises(TypeError, match="regime must be"):
        _resolve_regime("foo", tri)
    with pytest.raises(TypeError, match="regime must be"):
        lr.PooledLoss(regime="auto")
