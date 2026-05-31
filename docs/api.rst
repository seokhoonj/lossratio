API 레퍼런스
============

공개 API 전체 목록입니다. 설명문(docstring)은 영어로 작성되어 있으며,
개념적 배경과 사용 흐름은 :doc:`튜토리얼 <tutorial/index>`을 참고하세요.

데이터와 삼각형
---------------

.. autofunction:: lossratio.load_experience

.. autofunction:: lossratio.make_experience

.. autofunction:: lossratio.validate_experience

.. autofunction:: lossratio.derive_grain_columns

.. autoclass:: lossratio.Triangle
   :members:

.. autofunction:: lossratio.as_calendar

.. autoclass:: lossratio.Calendar
   :members:

.. autofunction:: lossratio.as_total

.. autoclass:: lossratio.Total
   :members:

발전 양상 진단
--------------

.. autoclass:: lossratio.Link
   :members:

.. autoclass:: lossratio.ATA
   :members:

.. autoclass:: lossratio.Intensity
   :members:

.. autoclass:: lossratio.Maturity
   :members:

.. autofunction:: lossratio.maturity_at

.. autofunction:: lossratio.maturity_spec

.. autoclass:: lossratio.Convergence
   :members:

.. autofunction:: lossratio.detect_convergence

구조 변화 탐지
--------------

.. autoclass:: lossratio.Regime
   :members:

.. autofunction:: lossratio.regime_at

.. autofunction:: lossratio.regime_spec

손해율 예측
-----------

.. autoclass:: lossratio.Ratio
   :members:

.. autoclass:: lossratio.RatioFit
   :members:

.. autoclass:: lossratio.Loss
   :members:

.. autoclass:: lossratio.LossFit
   :members:

.. autoclass:: lossratio.Premium
   :members:

.. autoclass:: lossratio.PremiumFit
   :members:

.. autoclass:: lossratio.CL
   :members:

.. autoclass:: lossratio.CLFit
   :members:

.. autoclass:: lossratio.ED
   :members:

.. autoclass:: lossratio.EDFit
   :members:

.. autoclass:: lossratio.BF
   :members:

.. autoclass:: lossratio.BFFit
   :members:

.. autoclass:: lossratio.CC
   :members:

.. autoclass:: lossratio.CCFit
   :members:

불확실성
--------

.. autoclass:: lossratio.Bootstrap
   :members:

.. autoclass:: lossratio.BootstrapTriangle
   :members:

검증
----

.. autoclass:: lossratio.Backtest
   :members:

.. autoclass:: lossratio.BacktestFit
   :members:
