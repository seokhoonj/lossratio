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

.. autoclass:: lossratio.Calendar
   :members:

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

.. autoclass:: lossratio.Convergence
   :members:

.. autofunction:: lossratio.detect_convergence

구조 변화 탐지
--------------

.. autoclass:: lossratio.Regime
   :members:

손해율 예측
-----------

.. autoclass:: lossratio.LossRatio
   :members:

.. autoclass:: lossratio.RatioFit
   :members:

.. autoclass:: lossratio.ChainLadder
   :members:

.. autoclass:: lossratio.ExposureDriven
   :members:

.. autoclass:: lossratio.StageAdaptive
   :members:

.. autoclass:: lossratio.Tail
   :members:

.. autoclass:: lossratio.LossFit
   :members:

.. autoclass:: lossratio.Premium
   :members:

.. autoclass:: lossratio.PremiumFit
   :members:

불확실성
--------

.. autoclass:: lossratio.Analytical
   :members:

.. autoclass:: lossratio.ResidualBootstrap
   :members:

.. autoclass:: lossratio.MonteCarlo
   :members:

검증
----

.. autoclass:: lossratio.Backtest
   :members:

.. autoclass:: lossratio.BacktestFit
   :members:
