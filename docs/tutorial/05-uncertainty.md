# 4장. 예측의 불확실성

3장은 빈 칸에 **하나의 숫자**를 채웠다 — 최근 코호트의 예측 손해율을
풀링(`PooledLoss`)은 1.30, link ratio(`ChainLadder`)는 0.88로. 그런데 숫자
하나만 보면 그 값을 *얼마나 믿어도 되는지*가 가려진다. 충분히 경과한
코호트의 1.50과 갓 인수된 코호트의 0.88은 같은 "예측치"여도 신뢰도가 전혀
다르다.

예측에는 두 축이 있다 — **얼마가 될까**(3장의 점추정)와 **얼마나 믿을 수
있나**(이 장의 불확실성). 둘은 서로 독립이라 따로 본다. 그리고 부록에서
약속한 대로, 불확실성은 코호트 간 흩어짐을 회귀로 추정하는 부분이라 손으로
따라가지 않고 **패키지가 계산**한다. 이 장은 그 출력을 *읽는* 법이다.

## 4.1 두 가지 오차 — 과정오차와 모수오차

예측이 빗나가는 원인은 둘로 나뉜다.

- **과정오차**(process error) — 미래의 손해 자체가 **확률적**이다. 설령 참된
  진전 인자를 정확히 안다 해도, 실제로 발생하는 손해는 그 평균 주위로
  흩어진다. 보험이라는 현상에 본디 박힌 무작위성이다.
- **모수오차**(parameter error) — 우리는 인자(`f_k`·`g_k`)를 **데이터로
  추정**했다. 그 추정 자체가 틀릴 수 있고, 데이터가 적을수록 더 불확실하다.

둘을 직교(독립)로 보고 제곱합으로 합친다(과정오차와 모수오차를 독립으로 가정).

$$
\text{SE}_{\text{total}} = \sqrt{\text{SE}_{\text{process}}^2 + \text{SE}_{\text{parameter}}^2}
$$

두 오차 모두 데이터가 적은 최근 코호트에서 커진다 — 채워야 할 빈 칸이
많고(과정오차↑), 인자를 받쳐 줄 관측이 적기(모수오차↑) 때문이다.

이 분해는 **기본 적합 결과에 이미 들어 있다**. `PooledLoss().fit(tri)`만
호출해도 `loss_proc_se`·`loss_param_se`·`loss_total_se` 컬럼이 셀 단위로
채워진다(`.to_polars()`로 볼 수 있다) — 분산 분해를 닫힌형(closed-form)으로
계산한 **기본 분석적 밴드**다. 불확실성을 켜기 위해 따로 넘길 인자가
없다는 점이 핵심이다 — 분석적 표준오차가 기본값이고, 4.5절의 부트스트랩이
그 *대안*이다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df, groups="coverage", grain="Q")

# 추가 인자 없이도 분석적 분해가 채워진다
own = lr.PooledLoss().fit(tri).to_polars().filter(pl.col("source") == "own")
own.select(
    ["cohort", "duration", "loss_proc_se", "loss_param_se", "loss_total_se"]
).tail(2)
#> 최근 코호트의 한 셀:  proc_se 1.34e8,  param_se 4.39e7,  total_se 1.41e8
```

두 오차가 정말 직각으로 합쳐지는지는 그 셀에서 곧장 확인된다 — 과정오차와
모수오차의 제곱합의 제곱근이 `loss_total_se`와 한 치도 어긋나지 않는다.

```python
cell = own.tail(1)
proc = cell["loss_proc_se"][0]
param = cell["loss_param_se"][0]
total = cell["loss_total_se"][0]
print(f"proc={proc:.4e}  param={param:.4e}")          #> proc=1.3353e+08  param=4.3900e+07
print(f"sqrt(proc^2 + param^2) = {(proc**2 + param**2) ** 0.5:.4e}")  #> sqrt(proc^2 + param^2) = 1.4056e+08
print(f"loss_total_se          = {total:.4e}")        #> loss_total_se          = 1.4056e+08
```

## 4.2 표준오차와 변동계수

**표준오차**(standard error, SE)는 예측의 흔들림을 절대 단위(원)로
나타낸다. 그런데 손해 규모가 큰 코호트는 SE도 자연히 크기 마련이라,
코호트끼리 비교하려면 크기를 정규화해야 한다. 그것이 **변동계수**
(coefficient of variation, CV)다.

$$
\text{CV} = \frac{\text{표준오차}}{\text{예측치}}
$$

CV는 무차원이라 "예측치 대비 몇 % 흔들리는가"로 코호트를 곧장 비교할 수
있다. 코호트 요약(`summary()`)의 `ratio_se`를 `ratio_proj`로 나누면
손해율 CV가 된다. 수술담보를 분기 단위로 집계해 link ratio로 적합하고,
코호트별 손해율 CV를 그려 본다.

```{eval-rst}
.. plot::
   :context:
   :nofigs:
   :include-source: false

   import polars as pl
   import lossratio as lr
   import matplotlib.pyplot as plt

   df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
   tri = lr.Triangle(df, groups="coverage", grain="Q")
   s = (
       lr.Ratio(loss=lr.ChainLadder(), premium=lr.PooledPremium())
       .fit(tri)
       .summary()
       .sort("cohort")
       .with_columns((pl.col("ratio_se") / pl.col("ratio_proj") * 100).alias("ratio_cv"))
   )
   labels = [c.strftime("%y.%m") for c in s["cohort"].to_list()]
   x = list(range(s.height))
   cv = [None if v is None else v for v in s["ratio_cv"].to_list()]

.. plot::
   :context: close-figs
   :caption: 수술담보 코호트별 예측 손해율의 변동계수(CV). 충분히 경과한 코호트는 거의 확실하지만(바닥), 갓 인수된 최근 코호트는 약 23%까지 치솟는다.

   fig, ax = plt.subplots(figsize=(6.4, 3.6))
   ax.plot(x, cv, "-o", color="#9467bd", ms=4)
   ax.set_xticks(x)
   ax.set_xticklabels(labels, rotation=45, ha="right", fontsize=8)
   ax.set_xlabel("underwriting cohort")
   ax.set_ylabel("loss-ratio CV (%)")
   fig.tight_layout()
```

오래된 코호트의 CV는 바닥에 깔려 있다 — 거의 다 경과해 예측할 게 없으니
확실하다. 그러나 가장 최근 코호트로 갈수록 CV가 가파르게 올라 **약 23%**에
이른다.

```{admonition} CV는 점추정의 정직한 경고등
:class: tip

3장에서 link ratio가 최근 코호트에 위험하다고 했다. CV는 그 위험을 **숫자로
경고**한다. 가장 최근 코호트의 예측치 0.86 옆에는 CV 약 23%가 붙는다 —
"이 0.86을 그대로 믿지 말라"는 신호다. 점추정과 CV는 늘 함께 읽어야 한다.
```

## 4.3 신뢰구간

CV가 "몇 % 흔들리나"라면, **신뢰구간**(confidence interval, CI)은 그 흔들림을
구체적인 범위로 보여 준다. 적합 결과의 `ratio_ci_lo`·`ratio_ci_hi`는 예측치에
표준오차의 약 1.96배를 더하고 뺀 95% 구간이다(정규 근사). 이 두 컬럼은
셀 단위 산출이라 `.to_polars()`에 들어 있다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df, groups="coverage", grain="Q")
fit = lr.Ratio(loss=lr.ChainLadder(), premium=lr.PooledPremium()).fit(tri)

# 코호트 수준의 점추정 + SE
s = fit.summary().sort("cohort").with_columns(
    (pl.col("ratio_se") / pl.col("ratio_proj") * 100).alias("ratio_cv"),
    (pl.col("ratio_proj") - 1.96 * pl.col("ratio_se")).alias("ratio_ci_lo"),
    (pl.col("ratio_proj") + 1.96 * pl.col("ratio_se")).alias("ratio_ci_hi"),
)
s.select(["cohort", "ratio_proj", "ratio_cv", "ratio_ci_lo", "ratio_ci_hi"])
#> 오래된 코호트 (2023-2분기):  proj 1.50,  CV 0.2%,   CI [1.493, 1.502]
#> 최근 코호트   (2025-4분기):  proj 0.86,  CV 22.7%,  CI [0.479, 1.246]
```

대비가 선명하다. 오래된 코호트의 95% 구간은 폭이 0.01밖에 안 되지만, 최근
코호트는 **[0.48, 1.25]** — 폭이 수십 배 넓다. 이 구간은 "보험료의 절반만
나갈 수도, 보험료를 넘길 수도 있다"는 뜻이라, 최근 코호트의 점추정 0.86은
사실상 단독으로는 쓸 수 없는 값이다.

위에서는 설명을 위해 `ratio_proj ± 1.96·ratio_se`를 손으로 더했지만, 그 95%
밴드는 **셀 단위 출력에 이미 들어 있다** — `fit.to_polars()`의
`ratio_ci_lo` / `ratio_ci_hi`가 바로 같은 구간이다(정규 분위수를 곱한 값이라
손계산과 소수점 미세 차이만 난다). 구간 폭은 추정기의 `confidence_level`
(기본 `0.95`)이 정하는 분위수로 조절한다.

## 4.4 손해율의 분산 — 분모는 알려진 노출

지금까지 본 `loss_proc_se`·`loss_param_se`는 **분자**(손해)의 분산이다. 손해율은
손해를 위험보험료로 나눈 **비율**이니, 그 `ratio_se`는 분자 SE를 분모로 나눠
만든다:

$$
\text{ratio\_se} = \frac{\text{loss\_total\_se}}{\text{premium}}
$$

즉 **분모(보험료)는 알려진 값으로 두고 분산을 얹지 않는다.** 왜 그런가:

- **위험보험료는 확률 과정이 아니라 *할당된 노출*이다.** `위험률 x 가입금액`으로
  계산되는 값이라, 손해처럼 매 기간 새로 *뽑히는* 양이 아니다. 이미 걷힌 보험료는
  장부에 적힌 확정값이고, 미래 보험료의 유일한 불확실성은 유지 건수(해지·사망 등
  줄어드는 사건)뿐이다 — 그런데 그건 손해와 보험료를 *같이* 움직여 **비율에서
  상쇄**되고, 남는 역선택(나쁜 위험만 남는 효과)은 분모가 아니라 손해강도 쪽에
  귀속된다.
- **그러니 분모에 분산을 매기면 *없는 위험을 지어낸다*.** 보험료 link ratio의
  코호트 간 산포는 예측 불확실성이 아니라 코호트 구성·시점 차이에서 오는 노이즈라,
  그걸 비율 밴드에 전파하면 — 상쇄되지 않는 자리에서 — 손해율 구간만 가짜로
  부푼다.

따라서 손해율 밴드는 **손해 적합이 이미 갖고 있는 불확실성을 알려진 분모로 나눈
것**이다. 분모 모델(`PooledPremium` 등)은 *점추정*을 위해 고르고 들여다보되,
분산은 분자에만 둔다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df, groups="coverage", grain="Q")

fit = lr.Ratio(loss=lr.ChainLadder(), premium=lr.PooledPremium()).fit(tri)
d = fit.to_polars().filter(pl.col("source") == "own")

# 분모는 SE를 내지 않고(premium_total_se = null), 손해율 밴드는
# 손해 SE를 분모로 나눈 것과 정확히 같다.
d.select(
    pl.col("premium_total_se").max().alias("premium_se"),
    (pl.col("ratio_se") - pl.col("loss_total_se") / pl.col("premium_proj"))
    .abs().max().alias("ratio_se_identity"),
)
#> premium_se: null,   ratio_se_identity: 0.0
```

분모 쪽 SE 컬럼은 비어 있고(`null`), 손해율 SE는 손해 SE를 분모로 나눈 값과
한 치도 어긋나지 않는다. 불확실성은 온전히 분자에서 온다.

```{admonition} 보험료가 *진짜로* 불확실하면
:class: tip

해지율이 빗나갈 수 있다는 *가정*에서 오는 장래 보험료 불확실성은 실재한다.
다만 그건 데이터의 link ratio 산포에는 없는(=부트스트랩도 Mack도 못 잡는) 양이라
요율·해지표 같은 외부 정보로 *주입*해야 하고, 게다가 비율에서 대부분 상쇄된다.
그래서 기본 손해율 밴드는 분모를 알려진 값으로 두며, 그 주입 경로는 해당 데이터가
있을 때 별도로 다룬다.
```

## 4.5 부트스트랩 — 분석적 밴드의 대안

지금까지의 표준오차·신뢰구간은 **기본값인 분석적 밴드**(해석적 분산 분해)에
기댔다. 빠르지만 불확실성을 표준오차 하나로 요약하고, 신뢰구간은 정규
근사다. **부트스트랩**(bootstrap, 적합을 수없이 반복하는 시뮬레이션)은
표준오차 하나가 아니라 예측치의 **전체 분포**를 직접 그린다 — 분포가
한쪽으로 치우치거나 꼬리가 길어도 그대로 드러나고, 어떤 분위수든 읽을 수
있다.

부트스트랩은 추정기에 `uncertainty=`로 끼워 넣는 **선택지**이고, 두 가지가
있다. 기본 도구는 **잔차 부트스트랩**(`lr.ResidualBootstrap`, 관측 잔차를
그대로 재표집하는 비모수 방식)이다. 잔차 분포에 특정 모양을 가정하지 않고,
데이터가 보여 준 잔차를 그대로 써서 가상의 삼각형을 재구성하고, 전체
파이프라인을 매 복제마다 다시 적합한다.

또 하나는 **가중 부트스트랩**(`lr.WeightedBootstrap`, fractional-random-weight
= 잔차를 다시 *뽑는* 대신 모든 셀에 무작위 *가중치*를 실어 매 복제마다 적합을
가중하는 방식)이다. 같은 `loss_total_se` · `loss_ci_lo` · `loss_ci_hi` 컬럼을
채우고 `n_jobs` 병렬도 그대로다. 잔차를 버리고 다시 뽑지 않으니 얇은 경과·
차용(borrowed) 꼬리에서 잔차 풀 고갈 없이 매끄럽게 도는 대신, 보통 잔차
부트스트랩보다 살짝 넓게 나온다. 다만 아직 준비금(reserving) 검증을 거치지
않은 실험적 경로라, 기본은 잔차 부트스트랩이다.

골격은 네 단계다(England-Verrall **과대산포 포아송**(over-dispersed Poisson, ODP) 잔차 부트스트랩, England & Verrall 2002).

**1) 잔차를 뽑는다.** 적합된 평균 $\hat\mu_{ik} = \hat g_k\,P_{i,k-1}$(강도 x 직전
누적 보험료; `CredibleLoss`는 코호트 스케일을 곱해 $\hat u_i\,\hat g_k\,P_{i,k-1}$)
에서 경과별 Pearson 분산 $\hat\phi_k$를 구해, 각 셀의 **Pearson 잔차**를 만든다:

$$r_{ik} = \frac{y_{ik} - \hat\mu_{ik}}{\sqrt{\hat\phi_k\,\hat\mu_{ik}}}\,(1-h_{ii})^{-1/2}$$

마지막 항은 레버리지 $h_{ii}$ 보정 — 적합이 자기 점에 끌려가 잔차가 작아지는
편향을 되돌린다. 이 보정은 `ResidualBootstrap(hat_adjust=True)`(기본)으로 켜져
있다.

**2) 가상의 삼각형을 만든다.** 잔차 풀에서 다시 뽑은 $r^*_{ik}$로 증분을
재구성한다:

$$y^*_{ik} = \hat\mu_{ik} + r^*_{ik}\sqrt{\hat\phi_k\,\hat\mu_{ik}}$$

(얇은 경과는 그 경과 군집 안에서, 잔차가 `min_pool`(기본 5)개 미만이면 전역
풀에서 재표집한다.)

**3) 전체를 다시 적합한다.** $y^*$ 삼각형에 **전체 파이프라인**($\hat g_k$, 그리고
`CredibleLoss`면 $\psi\,$,$\,\hat u_i$까지)을 처음부터 재추정해 예측 $\hat R^{(b)}$를
낸다 — 이게 *모수 불확실성*(추정 자체의 흔들림)을 푼다. 미래 셀엔 과대산포
**프로세스 노이즈**를 더한다: $\operatorname{Var}(\Delta C_k \mid C_{k-1}) = \hat\sigma^2_k\,C_{k-1}$.
이 노이즈의 분포는 `process`(기본 `"gamma"`)로 고르고, `drift=True`(기본)면
복제 평균의 표류(drift)까지 밴드에 싣는다. 세그먼트별 복제는 서로 독립이라
`n_jobs`로 병렬 처리할 수 있다.

**4) 분포를 읽는다.** $B$개 복제 $\{\hat R^{(b)}\}$에서 두 분산을 합치고(law of
total variance) 신뢰구간은 경험 분위수로 잡는다:

$$\text{SE}_{\text{total}} = \sqrt{\underbrace{\operatorname{Var}_b\!\big(\hat\mu^{(b)}\big)}_{\text{parameter}} + \underbrace{\overline{\hat\sigma^2_k\,C}}_{\text{process}}},\qquad \big[\,q_{2.5\%},\ q_{97.5\%}\,\big]$$

곧 `loss_param_se`, `loss_proc_se`, `loss_total_se` 컬럼이 이 분해다. (곱셈식
`ChainLadder`도 같은 골격에 England-Verrall ODP 잔차를 쓰되 평균이
$\hat\mu_{ik}=(\hat f_k-1)\,C_{i,k-1}$로 바뀐다.)

```{list-table}
:header-rows: 1
:widths: 30 70

* - 전략
  - 방식
* - 기본값 (인자 없음)
  - 분석적 밴드. 해석적 분산 분해로 닫힌형 SE + 정규 근사 신뢰구간. 가장 빠르다
* - `uncertainty=lr.ResidualBootstrap(...)`
  - 잔차 부트스트랩. 관측 잔차를 재표집 -> 가상 삼각형 재구성 -> 다시 적합을 수백 번. 분포 모양을 가정하지 않고 경험적 예측 밴드를 그린다
* - `uncertainty=lr.WeightedBootstrap(...)`
  - 가중(FRW) 부트스트랩. 잔차를 다시 뽑는 대신 셀마다 무작위 가중치를 실어 적합을 수백 번. 같은 SE/CI 컬럼을 채우고, 얇은 꼬리에서 잔차 풀 고갈이 없다(준비금 검증 전 실험적, 보통 살짝 넓다)
```

부트스트랩을 켜면 같은 `loss_total_se`·`loss_ci_lo`·`loss_ci_hi` 컬럼이
이번엔 **재표집으로** 채워진다 — 분석적 SE를 부트스트랩 흩어짐 + 경험적
예측 밴드로 갈아 끼우는 것이다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df, groups="coverage", grain="Q")

# 기본값: 분석적 밴드
base = lr.PooledLoss().fit(tri)

# 대안 1: 잔차 부트스트랩 (같은 컬럼을 재표집으로 채운다)
boot = lr.PooledLoss(
    uncertainty=lr.ResidualBootstrap(n_replicates=999, seed=42)
).fit(tri)

# 대안 2: 가중(FRW) 부트스트랩 (같은 컬럼을 가중 재적합으로 채운다)
wboot = lr.PooledLoss(
    uncertainty=lr.WeightedBootstrap(n_replicates=999, seed=42, n_jobs=1)
).fit(tri)

boot.to_polars().filter(pl.col("source") == "own").select(
    ["cohort", "duration", "loss_total_se", "loss_total_cv", "loss_ci_lo", "loss_ci_hi"]
).tail(3)
```

세 길의 같은 셀(가장 깊은 예측 칸)을 나란히 두면 가중 부트스트랩이 잔차
부트스트랩과 분석적 밴드 사이에 놓이는 것을 볼 수 있다.

```python
def se_last(fit):
    return (fit.to_polars().filter(pl.col("source") == "own")
            .select("loss_total_se").tail(1).item())

print(f"analytical        {se_last(base):.3e}")   #> analytical        1.406e+08
print(f"residual boot     {se_last(boot):.3e}")   #> residual boot     2.013e+08
print(f"weighted  boot    {se_last(wboot):.3e}")  #> weighted  boot    1.812e+08
```

부트스트랩은 분포가 비대칭이거나(꼬리가 한쪽으로 길거나) 정규 가정이
의심스러울 때 진가를 발휘한다. 그렇지 않은 경우엔 분석적 밴드와 거의 같은
답을 준다. 한 걸음 내다보는 *첫 예측 셀*에서 두 길이 실제로 겹치는지 나란히
놓고 본다 — 각 코호트의 가장 얕은 외삽 칸에서 두 `loss_total_se`가 몇 %
안쪽으로 일치한다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df, groups="coverage", grain="Q")

ana = lr.PooledLoss().fit(tri).to_polars()
boot = lr.PooledLoss(
    uncertainty=lr.ResidualBootstrap(n_replicates=999, seed=42)
).fit(tri).to_polars()

first = (
    ana.filter(pl.col("source") == "own")
    .select(["cohort", "duration", "loss_total_se"])
    .join(
        boot.select(["cohort", "duration",
                     pl.col("loss_total_se").alias("boot_se")]),
        on=["cohort", "duration"],
    )
    .sort("duration").group_by("cohort").first().sort("cohort")
    .with_columns((pl.col("boot_se") / pl.col("loss_total_se")).alias("boot/ana"))
)
first.select(["cohort", "duration", "loss_total_se", "boot_se", "boot/ana"]).tail(3)
#> 2025-04-01  dur 4   analytical 5.682e7   boot 5.693e7   boot/ana 1.00
#> 2025-07-01  dur 3   analytical 6.979e7   boot 6.924e7   boot/ana 0.99
#> 2025-10-01  dur 2   analytical 6.658e7   boot 6.773e7   boot/ana 1.02
```

닫힌형 분산 분해와 재표집이라는 *서로 다른 두 길*이 첫 예측 셀에서 같은 SE에
닿는다(비율 0.99-1.02) — 양쪽 모두를 신뢰할 근거다. 외삽이 깊어질수록
부트스트랩이 조금씩 더 넓어진다(이 담보에서 중앙값 약 8%, 가장 깊은 꼬리에서
더): 닫힌형의 정규 근사가 깊은 꼬리에서 낙관적인 자리를 재표집이 정직하게
벌려 주는 쪽이라, 깊은 예측에서는 부트스트랩이 더 보수적이다.

::::{admonition} CredibleLoss·SmoothLoss의 불확실성은 부트스트랩 전용이다
:class: warning

기본 추정기 `PooledLoss`(완전 풀링)는 분석적 밴드를 닫힌형으로 산출한다.
그러나 3장의 `CredibleLoss`(부분 풀링)와 `SmoothLoss`(매끄러운 형상)에는
**닫힌형 표준오차가 없다** — 신뢰도로 추정한 코호트 수준 자체의 분산,
수준과 형상의 곱, 둘의 공분산이 모두 얽혀 해석식이 성립하지 않고, 근사하면
불확실성을 과소평가한다. 그래서 이 둘은 **부트스트랩이 유일한 구간**이다.
인자 없이 적합하면 SE 컬럼이 `null`로 남고, `uncertainty=`를 넘겨야
`loss_total_se`와 분위수 기반의 신뢰구간이 채워진다 — 분포가 치우쳐 있으면
구간도 치우친 그대로 나온다.

```python
# 부분 풀링: 인자 없이 적합하면 SE가 null
lr.CredibleLoss().fit(tri).to_polars()["loss_total_se"].max()
#> None  (닫힌형 표준오차 없음)

# 부트스트랩을 켜야 구간이 채워진다
lr.CredibleLoss(
    uncertainty=lr.ResidualBootstrap(n_replicates=999, seed=42)
).fit(tri).to_polars()["loss_total_se"].max()
#> 1.72e8  (재표집으로 채워진 총 표준오차)
```
::::

## 4.6 함께 보기

- {doc}`2장 — 손해율 예측 <02-projection>`: 이 장이 불확실성을 입힌 점추정.
  예측치와 CV·CI는 항상 함께 읽는다.
- {doc}`7장 — 예측 검증 <07-backtest>`: 과거 시점에서 예측을 돌려 실제와
  맞춰 보는 백테스트로, 불확실성 추정이 실전에서 맞는지 확인한다.
- {doc}`API 레퍼런스 <../api>`의 `Ratio`, `ChainLadder`, `PooledLoss`,
  `CredibleLoss`, `ResidualBootstrap`, `WeightedBootstrap`
