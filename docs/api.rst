API 레퍼런스
============

공개 API 전체 목록입니다. 설명문(docstring)은 영어로 작성되어 있으며,
개념적 배경과 사용 흐름은 튜토리얼(:doc:`tutorial/index`)을 참고하세요.

데이터와 삼각형
---------------

.. autofunction:: lossratio.load_experience

.. autofunction:: lossratio.make_experience

.. autofunction:: lossratio.validate_experience

.. autofunction:: lossratio.derive_grain_columns

.. autofunction:: lossratio.inception_stability

.. autoclass:: lossratio.Triangle
   :members:

.. autoclass:: lossratio.TriangleValidation
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

구조 변화 탐지
--------------

.. autoclass:: lossratio.Regime
   :members:

손해 예측 (강도 사다리 + 벤치마크)
----------------------------------

완전 풀링 → 코호트 신뢰도 → 평활 형상의 구조 사다리(``PooledLoss`` ->
``CredibleLoss`` -> ``SmoothLoss``)와, 사다리 밖의 자기손해 링크비 벤치마크
``LinkRatio``. 넷 다 :class:`~lossratio.LossFit` 을 반환합니다.

.. autoclass:: lossratio.PooledLoss
   :members:

.. autoclass:: lossratio.CredibleLoss
   :members:

.. autoclass:: lossratio.SmoothLoss
   :members:

.. autoclass:: lossratio.LinkRatio
   :members:

.. autoclass:: lossratio.LossFit
   :members:

보험료 예측 (자기 링크비 사다리)
--------------------------------

보험료는 외부 익스포저가 없어 자기 링크비로 self-develop 하며, 손해 사다리의
분모 대칭형입니다. :class:`~lossratio.PremiumFit` 을 반환합니다.

.. autoclass:: lossratio.PooledPremium
   :members:

.. autoclass:: lossratio.CrediblePremium
   :members:

.. autoclass:: lossratio.SmoothPremium
   :members:

.. autoclass:: lossratio.PremiumFit
   :members:

손해율 합성
-----------

.. autoclass:: lossratio.Ratio
   :members:

.. autoclass:: lossratio.RatioFit
   :members:

Go-forward 안정성
-----------------

관측 경과 너머의 손해율은, 발전이 정착한 경우에만 마지막 값을 평탄 연장하는
것이 정직합니다. :class:`~lossratio.Stability` 게이트가 그 판정을 합니다.

.. autoclass:: lossratio.Stability
   :members:

.. autoclass:: lossratio.StabilityReport
   :members:

불확실성
--------

.. autoclass:: lossratio.ResidualBootstrap
   :members:

검증
----

.. autoclass:: lossratio.Backtest
   :members:

.. autoclass:: lossratio.BacktestFit
   :members:

.. autoclass:: lossratio.EstimatorComparison
   :members:

.. autoclass:: lossratio.EstimatorComparisonFit
   :members:
