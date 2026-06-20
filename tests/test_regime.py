

def test_detect_regime_output_is_usable_in_a_fit():
    # the detect_regime() -> fit workflow: passing the Regime object (or 'auto')
    # to an estimator must resolve to the same cut as the manual dict.
    import lossratio as lr
    from lossratio.regime import _resolve_regime

    tri = lr.Triangle(lr.load_experience(), groups="coverage")
    reg = tri.detect_regime(target="ratio")

    resolved = _resolve_regime(reg, tri)
    assert isinstance(resolved, dict) and resolved   # {coverage: date}

    a = lr.PooledLoss(regime=reg).fit(tri).to_polars()
    b = lr.PooledLoss(regime=resolved).fit(tri).to_polars()
    assert a.equals(b)                                # Regime obj == manual dict

    # 'auto' and Regime also flow through premium / ratio / backtest without error
    lr.PooledLoss(regime="auto").fit(tri)
    lr.PooledPremium(regime=reg).fit(tri)
    lr.Ratio(loss=lr.PooledLoss(regime=reg), premium=lr.PooledPremium()).fit(tri)
    lr.Backtest(estimator=lr.PooledLoss(regime=reg), holdouts=4, target="loss").fit(tri)
