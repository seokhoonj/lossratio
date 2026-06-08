# 5장. 예측의 불확실성

4장은 빈 칸에 **하나의 숫자**를 채웠다 — 어린 코호트의 최종 손해율을 ED는
1.30, CL은 0.88로. 그런데 숫자 하나만 보면 그 값을 *얼마나 믿어도 되는지*가
가려진다. 성숙한 코호트의 1.50과 갓 인수된 코호트의 0.88은 같은 "예측치"
여도 신뢰도가 전혀 다르다.

예측에는 두 축이 있다 — **얼마가 될까**(4장의 점 예측)와 **얼마나 믿을 수
있나**(이 장의 불확실성). 둘은 서로 독립이라 따로 본다. 그리고 2장에서
약속한 대로, 불확실성은 코호트 간 흩어짐을 회귀로 추정하는 부분이라 손으로
따라가지 않고 **패키지가 계산**한다. 이 장은 그 출력을 *읽는* 법이다.

## 5.1 두 가지 오차 — 과정오차와 모수오차

예측이 빗나가는 원인은 둘로 나뉜다.

- **과정오차**(process error) — 미래의 손해 자체가 **확률적**이다. 설령 참된
  진전 인자를 정확히 안다 해도, 실제로 발생하는 손해는 그 평균 주위로
  흩어진다. 보험이라는 현상에 본디 박힌 무작위성이다.
- **모수오차**(parameter error) — 우리는 인자(`f_k`·`g_k`)를 **데이터로
  추정**했다. 그 추정 자체가 틀릴 수 있고, 데이터가 적을수록 더 불확실하다.

둘을 직교(독립)로 보고 제곱합으로 합친다(Mack 1993).

$$
\text{총 표준오차} = \sqrt{\text{과정오차}^2 + \text{모수오차}^2}
$$

두 오차 모두 데이터가 적은 어린 코호트에서 커진다 — 채워야 할 빈 칸이
많고(과정오차↑), 인자를 받쳐 줄 관측이 적기(모수오차↑) 때문이다. 적합
결과에는 이 분해가 그대로 들어 있다(`loss_proc_se`, `loss_param_se`,
`loss_total_se`).

## 5.2 표준오차와 변동계수

**표준오차**(standard error, SE)는 예측의 흔들림을 절대 단위(원)로
나타낸다. 그런데 손해 규모가 큰 코호트는 SE도 자연히 크기 마련이라,
코호트끼리 비교하려면 크기를 정규화해야 한다. 그것이 **변동계수**
(coefficient of variation, CV)다.

$$
\text{CV} = \frac{\text{표준오차}}{\text{예측치}}
$$

CV는 무차원이라 "예측치 대비 몇 % 흔들리는가"로 코호트를 곧장 비교할 수
있다. SUR 담보를 분기 단위로 집계해 CL로 적합하고, 코호트별 손해율 CV를
그려 본다.

```{eval-rst}
.. plot::
   :context:
   :nofigs:
   :include-source: false

   import polars as pl
   import lossratio as lr
   import matplotlib.pyplot as plt

   df = lr.load_experience().filter(pl.col("coverage") == "SUR")
   tri = lr.Triangle(df, groups="coverage", grain="Q")
   s = lr.Ratio(method="cl").fit(tri).summary().sort("cohort")
   labels = [c.strftime("%y.%m") for c in s["cohort"].to_list()]
   x = list(range(s.height))
   cv = [None if v is None else v * 100 for v in s["ratio_cv"].to_list()]

.. plot::
   :context: close-figs
   :caption: SUR 코호트별 예측 손해율의 변동계수(CV). 충분히 발전한 오래된 코호트는 거의 확실하지만(바닥), 갓 인수된 어린 코호트는 약 23%까지 치솟는다.

   fig, ax = plt.subplots(figsize=(6.4, 3.6))
   ax.plot(x, cv, "-o", color="#9467bd", ms=4)
   ax.set_xticks(x)
   ax.set_xticklabels(labels, rotation=45, ha="right", fontsize=8)
   ax.set_xlabel("underwriting cohort")
   ax.set_ylabel("loss-ratio CV (%)")
   fig.tight_layout()
```

오래된 코호트의 CV는 바닥에 깔려 있다 — 거의 다 발전해 예측할 게 없으니
확실하다. 그러나 가장 어린 코호트로 갈수록 CV가 가파르게 올라 **약 23%**에
이른다.

```{admonition} CV는 점 예측의 정직한 경고등
:class: tip

4장에서 CL이 어린 코호트에 위험하다고 했다. CV는 그 위험을 **숫자로
경고**한다. 가장 어린 코호트의 CL 예측치 0.86 옆에는 CV 약 23%가 붙는다 —
"이 0.86을 그대로 믿지 말라"는 신호다. 점 예측과 CV는 늘 함께 읽어야 한다.
```

## 5.3 신뢰구간

CV가 "몇 % 흔들리나"라면, **신뢰구간**(confidence interval, CI)은 그 흔들림을
구체적인 범위로 보여 준다. 적합 결과의 `ratio_ci_lo`·`ratio_ci_hi`는 예측치에
표준오차의 약 1.96배를 더하고 뺀 95% 구간이다(정규 근사).

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SUR")
tri = lr.Triangle(df, groups="coverage", grain="Q")
fit = lr.Ratio(method="cl").fit(tri)

fit.summary().select(["cohort", "ratio_proj", "ratio_cv", "ratio_ci_lo", "ratio_ci_hi"])
#> 성숙 코호트 (2023-2분기):  proj 1.50,  CV 0.2%,   CI [1.493, 1.502]
#> 어린 코호트 (2025-4분기):  proj 0.86,  CV 22.7%,  CI [0.479, 1.246]
```

대비가 선명하다. 성숙 코호트의 95% 구간은 폭이 0.01밖에 안 되지만, 어린
코호트는 **[0.48, 1.25]** — 폭이 수십 배 넓다. 이 구간은 "보험료의 절반만
나갈 수도, 보험료를 넘길 수도 있다"는 뜻이라, 어린 코호트의 점 예측 0.86은
사실상 단독으로는 쓸 수 없는 값이다.

## 5.4 손해율의 분산 — 분모도 흔들릴 때

지금까지 본 `loss_proc_se`·`loss_param_se`는 **분자**(손해)의 분산이다. 그런데
손해율은 손해를 위험보험료로 나눈 **비율**이고, 그 `ratio_se`를 만들려면
**분모**(보험료)의 불확실성도 함께 따져야 한다. lossratio는 두 가지 방식을
제공한다(`Ratio(se_method=...)`).

- `se_method="fixed"`(기본값) — 분모를 **확정값**으로 본다. 보험료는 이미
  거의 다 들어와 흔들림이 작다는 가정 아래, 손해율의 표준오차를 분자 SE를
  분모로 나눠 구한다.

  $$
  \text{ratio\_se} = \frac{\text{loss\_total\_se}}{\text{premium}}
  $$

- `se_method="delta"` — 분자와 분모의 불확실성을 **델타 방법**(delta method,
  1차 테일러 전개로 비율의 분산을 근사)으로 함께 전파하고, 둘의 상관계수
  `rho`까지 반영한다.

  $$
  \operatorname{Var}\!\left(\tfrac{L}{P}\right) \approx
  \left(\tfrac{\text{SE}_L}{P}\right)^{2}
  + \left(\tfrac{L\,\text{SE}_P}{P^{2}}\right)^{2}
  - 2\,\rho\,\frac{L\,\text{SE}_L\,\text{SE}_P}{P^{3}}
  $$

  마지막 항이 핵심이다. 손해와 보험료의 추정 오차는 보통 **같은 방향**으로
  움직인다 — 물량이 많은 코호트는 손해도 보험료도 함께 크다(양의 `rho`).
  분자와 분모가 같이 흔들리면 그 **비율은 오히려 덜 흔들린다**. 그래서
  델타 방법의 손해율 SE는 분모를 고정으로 본 값보다 **작아질 수 있다**.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SUR")
tri = lr.Triangle(df, groups="coverage", grain="Q")

# 분모 고정 (기본값)
lr.Ratio(method="cl", se_method="fixed").fit(tri).summary().select(
    ["cohort", "ratio_proj", "ratio_cv"]
)
#> 어린 코호트 (2025-4분기):  proj 0.86,  ratio_cv 22.7%

# 분모도 전개 + 상관 반영
lr.Ratio(method="cl", se_method="delta", rho=0.9).fit(tri).summary().select(
    ["cohort", "ratio_proj", "ratio_cv"]
)
#> 어린 코호트 (2025-4분기):  proj 0.86,  ratio_cv 21.2%
```

같은 점 예측(0.86)에 분산만 달라진다. `delta`는 보험료의 불확실성
(`premium_total_se`·`premium_total_cv` 컬럼이 함께 붙는다 — 여기선 약 1.7%)을
더하면서도, 양의 상관 덕분에 손해율 CV가 22.7% -> 21.2%로 **줄었다**.

```{admonition} 어느 쪽을 쓰나
:class: tip

보험료가 거의 다 들어와 분모가 사실상 확정이면 `fixed`로 충분하고 둘은
거의 같다. 분모도 한참 전개해야 하거나(아주 어린 코호트), 손해·보험료의
상관을 명시적으로 반영해 손해율 구간을 더 정확히 보고하고 싶을 때
`se_method="delta"`를 쓴다. `rho`는 (-1, 1) 범위의 손해-보험료 상관이다.
```

## 5.5 부트스트랩

지금까지의 표준오차·신뢰구간은 **공식**(Mack의 분산 분해)에 기댔다. 빠르지만
불확실성을 표준오차 하나로 요약하고, 신뢰구간은 정규 근사다. **부트스트랩**
(bootstrap, 적합을 수없이 반복하는 시뮬레이션)은 표준오차 하나가 아니라
예측치의 **전체 분포**를 직접 그린다 — 분포가 한쪽으로 치우치거나 꼬리가
길어도 그대로 드러나고, 어떤 분위수든 읽을 수 있다.

부트스트랩에는 두 갈래가 있고, *무엇을 재표집하느냐*가 다르다.

- **비모수**(`lr.ResidualBootstrap`): 관측 잔차를 그대로 재표집한다. 잡음의
  분포 모양에 **특정 가정을 두지 않고**, 데이터가 보여 준 잔차 분포를 쓴다.
- **모수**(`lr.ParametricBootstrap`): 추정한 **분포**(예: 감마·과대산포
  포아송·정규)에서 직접 표집한다. 분포를 **가정하는** 대신, 잔차가 너무 적어
  재표집이 불안정한 상황에서도 안정적이다.

그래서 "분포 가정을 피한다"는 말은 **비모수 쪽에만** 해당한다. 모수
부트스트랩은 오히려 분포를 명시적으로 가정하며 — 분석적 공식은 거기에
닫힌형 + 정규 근사까지 더한 것이다. 셋 다 모형에 `uncertainty=`로 끼워 넣는다.

```{list-table}
:header-rows: 1
:widths: 26 74

* - 전략
  - 방식
* - `lr.Analytical`
  - 공식 기반(기본값). Mack의 분산 분해로 닫힌형 SE + 정규 근사 신뢰구간. 가장 빠르다
* - `lr.ResidualBootstrap`
  - **비모수**. 관측 잔차를 재표집 -> 가상 삼각형 재구성 -> 다시 적합을 수백 번. 분포 모양을 가정하지 않는다
* - `lr.ParametricBootstrap`
  - **모수**. 추정한 분포(감마·과대산포 포아송·정규)에서 직접 표집해 반복. 분포를 가정한다
```

```python
boot = lr.ChainLadder(
    uncertainty=lr.ResidualBootstrap(n_replicates=1000, seed=42, quantile_ci=True)
).fit(tri)
boot.boots.summary.select(["cohort", "dev", "total_cv", "ci_lo", "ci_hi"])
```

부트스트랩은 분포가 비대칭이거나(꼬리가 한쪽으로 길거나) 정규 가정이
의심스러울 때 진가를 발휘한다. 그렇지 않은 경우엔 분석적 공식과 거의 같은
답을 준다 — 두 길이 같은 답에 닿는다는 것은 양쪽 모두를 신뢰할 근거가 된다.

::::{admonition} SA는 잔차 부트스트랩을 받지 않는다
:class: warning

`lr.StageAdaptive`는 `lr.ResidualBootstrap`을 **거부**한다.
ED(덧셈)+CL(곱셈) 두 단계가 한 전환 지점에서 이어지는 적합이라, 잔차를
한 통에 모을 일관된 풀이 없기 때문이다. SA의 불확실성은 분포 기반인
`lr.ParametricBootstrap`이나 기본 분석적 SE로 구한다.

```python
# 거부된다
lr.StageAdaptive(switch=3, uncertainty=lr.ResidualBootstrap(n_replicates=500))
#> ValueError: StageAdaptive does not support ResidualBootstrap ...

# 대신 이렇게
lr.StageAdaptive(switch=3, uncertainty=lr.ParametricBootstrap(n_replicates=500, seed=42))
lr.StageAdaptive(switch=3)   # 또는 기본 분석적 SE
```
::::

## 5.6 함께 보기

- {doc}`4장 — 손해율 예측 <04-projection>`: 이 장이 불확실성을 입힌 점 예측.
  예측치와 CV·CI는 항상 함께 읽는다.
- {doc}`7장 — 예측 검증 <07-backtest>`: 과거 시점에서 예측을 돌려 실제와
  맞춰 보는 백테스트로, 불확실성 추정이 실전에서 맞는지 확인한다.
- {doc}`API 레퍼런스 <../api>`의 `Ratio`, `ChainLadder`, `Analytical`,
  `ResidualBootstrap`, `ParametricBootstrap`
