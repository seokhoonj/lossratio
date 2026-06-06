# 5장. 예측의 불확실성

4장은 빈 칸에 **하나의 숫자**를 채웠습니다 — 어린 코호트의 최종 손해율을
ED는 1.38, CL은 0.87로요. 그런데 숫자 하나만 보면 그 값을 *얼마나 믿어도
되는지*가 가려집니다. 성숙한 코호트의 1.52와 갓 인수된 코호트의 0.90은
같은 "예측치"여도 신뢰도가 전혀 다릅니다.

예측에는 두 축이 있습니다 — **얼마가 될까**(4장의 점 예측)와 **얼마나 믿을
수 있나**(이 장의 불확실성). 둘은 서로 독립이라 따로 봅니다. 그리고 2장에서
약속한 대로, 불확실성은 코호트 간 흩어짐을 회귀로 추정하는 부분이라 손으로
따라가지 않고 **패키지가 계산**합니다. 이 장은 그 출력을 *읽는* 법입니다.

## 5.1 두 가지 오차 — 과정오차와 모수오차

예측이 빗나가는 원인은 둘로 나뉩니다.

- **과정오차**(process error) — 미래의 손해 자체가 **확률적**입니다. 설령
  참된 진전 인자를 정확히 안다 해도, 실제로 발생하는 손해는 그 평균
  주위로 흩어집니다. 보험이라는 현상에 본디 박힌 무작위성입니다.
- **모수오차**(parameter error) — 우리는 인자(`f_k`·`g_k`)를 **데이터로
  추정**했습니다. 그 추정 자체가 틀릴 수 있고, 데이터가 적을수록 더
  불확실합니다.

둘을 직교(독립)로 보고 제곱합으로 합칩니다(Mack 1993).

$$
\text{총 표준오차} = \sqrt{\text{과정오차}^2 + \text{모수오차}^2}
$$

두 오차 모두 데이터가 적은 어린 코호트에서 커집니다 — 채워야 할 빈 칸이
많고(과정오차↑), 인자를 받쳐 줄 관측이 적기(모수오차↑) 때문입니다. 적합
결과에는 이 분해가 그대로 들어 있습니다(`loss_proc_se`, `loss_param_se`,
`loss_total_se`).

## 5.2 표준오차와 변동계수

**표준오차**(standard error, SE)는 예측의 흔들림을 절대 단위(원)로
나타냅니다. 그런데 손해 규모가 큰 코호트는 SE도 자연히 크기 마련이라,
코호트끼리 비교하려면 크기를 정규화해야 합니다. 그것이 **변동계수**
(coefficient of variation, CV)입니다.

$$
\text{CV} = \frac{\text{표준오차}}{\text{예측치}}
$$

CV는 무차원이라 "예측치 대비 몇 % 흔들리는가"로 코호트를 곧장 비교할 수
있습니다. SUR 담보를 CL로 적합해 코호트별 손해율 CV를 그려 봅니다.

```{eval-rst}
.. plot::
   :context:
   :nofigs:
   :include-source: false

   import polars as pl
   import lossratio as lr
   import matplotlib.pyplot as plt

   df = lr.load_experience().filter(pl.col("coverage") == "SUR")
   tri = lr.Triangle(df, groups="coverage")
   s = lr.Ratio(method="cl").fit(tri).summary().sort("cohort")
   x = list(range(1, s.height + 1))
   cv = [None if v is None else v * 100 for v in s["ratio_cv"].to_list()]

.. plot::
   :context: close-figs
   :caption: SUR 코호트별 예측 손해율의 변동계수(CV). 충분히 발전한 오래된 코호트는 0.5% 안팎으로 거의 확실하지만, 갓 인수된 어린 코호트는 18%까지 치솟는다.

   fig, ax = plt.subplots(figsize=(6.2, 3.4))
   ax.plot(x, cv, "-o", color="#9467bd", ms=3)
   ax.set_xlabel("cohort (old -> young)")
   ax.set_ylabel("loss-ratio CV (%)")
```

오래된 코호트의 CV는 0.5% 안팎으로 바닥에 깔려 있습니다 — 거의 다 발전해
예측할 게 없으니 확실합니다. 그러나 가장 어린 코호트로 갈수록 CV가
가파르게 올라 **약 18%**에 이릅니다.

```{admonition} CV는 점 예측의 정직한 경고등
:class: tip

4장에서 CL이 어린 코호트에 위험하다고 했습니다. CV는 그 위험을 **숫자로
경고**합니다. 가장 어린 코호트의 CL 예측치 0.90 옆에는 CV 18%가 붙습니다 —
"이 0.90을 그대로 믿지 말라"는 신호입니다. 점 예측과 CV는 늘 함께 읽어야
합니다.
```

## 5.3 신뢰구간

CV가 "몇 % 흔들리나"라면, **신뢰구간**(confidence interval, CI)은 그 흔들림을
구체적인 범위로 보여 줍니다. 적합 결과의 `ratio_ci_lo`·`ratio_ci_hi`는
예측치에 표준오차의 약 1.96배를 더하고 뺀 95% 구간입니다(정규 근사).

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SUR")
tri = lr.Triangle(df, groups="coverage")
fit = lr.Ratio(method="cl").fit(tri)

fit.summary().select(["cohort", "ratio_proj", "ratio_cv", "ratio_ci_lo", "ratio_ci_hi"])
#> 성숙 코호트 (2023-03):  ult 1.52,  CV 0.5%,   CI [1.506, 1.539]
#> 어린 코호트 (2025-12):  ult 0.90,  CV 17.9%,  CI [0.587, 1.223]
```

대비가 선명합니다. 성숙 코호트의 95% 구간은 폭이 0.03밖에 안 되지만, 어린
코호트는 **[0.59, 1.22]** — 폭이 20배 넓습니다. 이 구간은 "보험료의 절반만
나갈 수도, 보험료를 넘길 수도 있다"는 뜻이라, 어린 코호트의 점 예측
0.90은 사실상 단독으로는 쓸 수 없는 값입니다.

## 5.4 부트스트랩

지금까지의 표준오차·신뢰구간은 **공식**(Mack의 분산 분해)에 기댑니다.
이는 빠르지만 정규 근사 같은 분포 가정을 깔고 있습니다. **부트스트랩**
(bootstrap, 데이터를 재표집해 적합을 수없이 반복하는 시뮬레이션)은 그 가정
없이, **잔차를 재표집 -> 가상의 삼각형 재구성 -> 다시 적합**하기를 수백 번
반복해 예측치의 *경험적 분포*를 직접 그립니다.

```python
boot = lr.ChainLadder(
    uncertainty=lr.ResidualBootstrap(n_replicates=1000, seed=42, quantile_ci=True)
).fit(tri)
boot.boots.summary.select(["cohort", "dev", "total_cv", "ci_lo", "ci_hi"])
```

불확실성은 모형에 `uncertainty=`로 끼워 넣습니다 — 잔차 재표집은
`lr.ResidualBootstrap`, 모수에서 직접 뽑는 모수 부트스트랩은
`lr.ParametricBootstrap`, 공식 기반(기본값)은 `lr.Analytical`입니다.

부트스트랩은 분포가 비대칭이거나(꼬리가 한쪽으로 길거나) 정규 가정이
의심스러울 때 진가를 발휘합니다. 그렇지 않은 경우엔 분석적 공식과 거의
같은 답을 줍니다 — 실제로 이 데이터의 가장 어린 코호트에서 부트스트랩 CV는
약 18.3%로, 5.2절의 분석적 CV 17.9%와 사실상 일치합니다. 두 길이 같은 답에
닿는다는 것은 양쪽 모두를 신뢰할 근거가 됩니다.

## 5.5 함께 보기

- {doc}`4장 — 손해율 예측 <04-projection>`: 이 장이 불확실성을 입힌 점 예측.
  예측치와 CV·CI는 항상 함께 읽습니다.
- {doc}`7장 — 예측 검증 <07-backtest>`: 과거 시점에서 예측을 돌려 실제와
  맞춰 보는 백테스트로, 불확실성 추정이 실전에서 맞는지 확인합니다.
- {doc}`API 레퍼런스 <../api>`의 `Ratio`, `ChainLadder`, `Analytical`,
  `ResidualBootstrap`, `ParametricBootstrap`
