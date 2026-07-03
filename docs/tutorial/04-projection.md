# 3장. 코호트 보정 — CredibleLoss와 SmoothLoss

2장은 빈 칸을 채우는 기본 방법으로 **PooledLoss**(완전 풀링)를 세웠다 — 강도
`g_k`를 코호트 전체에서 풀링해 추정하고, 그 강도를 누적 위험보험료에 곱한
증분을 더해 나갔다. 풀링은 PooledLoss를 별도의 사전 판단 없이 어디서나
작동하는 안전한 기본값으로 만든다. 이 장은 그 기본값을 **두 방향으로 다듬는다**
— 서로 직교하는 두 보정이다.

- **CredibleLoss** — 각 코호트의 *수준(level)*을 신뢰도로 보정한다(부분 풀링).
  풀링된 형상(`g_k`)은 그대로 두고, 포트폴리오 평균과 다르게 달려온 코호트의
  수준 차이를 그 코호트의 관측이 쌓인 만큼만 되돌린다.
- **SmoothLoss** — 풀링된 *형상(shape)*을 매끄럽게 다듬는다. 경과마다 자유롭게
  추정한 강도 `g_k`가 얇은 세그먼트에서 들쭉날쭉할 때, P-spline으로 그 형상에
  추세만 남긴다.

수준의 축(CredibleLoss)과 형상의 축(SmoothLoss)은 별개다. 이 장은 각각이 무엇을
어떻게 고치는지, 그리고 언제 채택하는지를 본다.

이 장도 2장처럼 "그 칸이 *얼마가 될까*"(점추정)만 다룬다. "그 값을 *얼마나 믿을
수 있나*"(표준오차·신뢰구간)는 4장에서 따로 본다 — 게다가 CredibleLoss와
SmoothLoss는 닫힌형 표준오차가 없어 **부트스트랩 전용**이라, 구간 자체가 4장의
주제다.

## 3.1 하나의 최근 코호트로 따라가기

두 보정 모두 **관측이 얇은 최근 코호트**에서 작동이 또렷하다. 이 장은 내장
수술담보(SURGERY)를 분기(Quarter) 단위로 집계한 삼각형에서, 갓 인수되어 경과
네 분기치밖에 없는 **2025-1분기 코호트** 하나를 실마리로 삼는다. 이 코호트의
관측 누적 손해율은 경과 1-4에서 0.42 → 0.71 → 0.80 → 0.84로 오른다. 2장의
베이스라인 PooledLoss로 빈 칸을 채우면, 이 코호트의 예측 손해율은 포트폴리오
평균 노출 수준을 따라 약 **1.30**까지 올라간다. 아래 두 절은 이 같은 칸을
CredibleLoss와 SmoothLoss가 어떻게 다르게 채우는지 본다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df, groups="coverage", grain="Q")
COH = pl.lit("2025-01-01").str.to_date()

lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(tri).summary().filter(
    pl.col("cohort") == COH).select(["cohort", "ratio_proj"])
#> ┌────────────┬───────────┐
#> │ cohort     ┆ ratio_proj│
#> ╞════════════╪═══════════╡
#> │ 2025-01-01 ┆ 1.299     │
#> └────────────┴───────────┘
```

## 3.2 코호트 수준 보정 — CredibleLoss

PooledLoss의 강도 `g_k`는 **코호트 전체에서 풀링**해 추정한다. 풀링이
PooledLoss를 안정적으로 만들지만, 대가가 하나 있다 — 어떤 코호트가
포트폴리오 평균보다 일관되게 높게(또는 낮게) 달려와도, PooledLoss의 예측
증분은 포트폴리오 공통의 강도를 쓰므로 그 **수준 차이가 지워진다**.
PooledLoss가 예측을 노출(보험료) 수준으로 당겨 보는 성질의 뒷면이다.

**CredibleLoss(부분 풀링)**는 그 수준만 돌려준다. 형상(`g_k`)은
PooledLoss의 풀링을 그대로 쓰고, 코호트별 스케일 하나를 증분에 곱한다.

$$
\text{(예측) 증분 손해}_{c,k} = s_c \times g_k \times \text{누적 보험료}_{c,k-1}
$$

스케일 $s_c$는 그 코호트의 관측 강도가 풀링 강도의 몇 배였는지의
중앙값을, **신뢰도**(credibility) 가중으로 1(포트폴리오 평균)을 향해
줄인 값이다.

$$
s_c = \frac{n \times \text{관측 배율의 중앙값} + K \times 1}{n + K}
$$

$n$은 그 코호트의 관측이 갖는 무게(노출을 분산으로 가중한 *유효표본*이지,
관측 경과의 단순 *개수*가 아니다 — 분기마다 큰 보험료가 실릴수록 같은 개수라도
$n$이 커진다), $K$는 신뢰도 상수다. `CredibleLoss`는 이 $K$를 직접 노출하지
않고, 코호트 간 분산 `psi`(기본 `"auto"` — Buhlmann-Straub 적률로 추정)에서
$K = 1/\psi$로 내부적으로 정한다. `psi = 0`이면 모든 코호트가 1에 묶여
PooledLoss와 같아진다.
갓 인수되어 $n$이 작은 코호트는 1에 묶여 사실상 PooledLoss와 같고, 관측이
쌓일수록 자기 수준을 더 믿는다 — "자기 경험을 말할 자격이 생긴 만큼만
반영"하는 구조다.

실제 한 코호트로 채워 보면 작동이 또렷하다. 앞 절의 최근 수술담보
코호트(2025-1분기)는 관측된 경과가 3개(경과 2,3,4의 증분)뿐이고, 각 경과의
관측 강도가 풀링 강도의 약 [0.87, 0.77, 0.74]배 — 중앙값 약 0.78로 포트폴리오
평균(= 1)보다 낮게 달려왔다. `psi="auto"`가 코호트 간 분산을 $\hat\psi \approx
0.0101$로 추정하니 신뢰도 상수는 $K = 1/\hat\psi \approx 99$다. 여기서 신뢰도
가중의 "표본 크기"는 관측 경과의 개수 3이 아니라, 그 3개 링크를 받쳐 주는
보험료 노출을 분산으로 가중한 유효표본 $n \approx 48$이다. 그래서 신뢰도는

$$
Z = \frac{n}{n + K} = \frac{48}{48 + 99} \approx 0.33,
$$

곧 자기 수준을 1/3만 믿고 2/3는 평균으로 당긴다. 스케일은

$$
s_c = \frac{n \times 0.78 + K \times 1}{n + K}
    = Z \times 0.78 + (1 - Z) \times 1
    \approx 0.33 \times 0.78 + 0.67 \times 1 \approx 0.93.
$$

예측 증분이 0.93배로 눌리니, 이 코호트의 CredibleLoss 예측 손해율(아래 표의
1.23)이 PooledLoss(1.30) 아래로 내려앉는다.

```{eval-rst}
.. plot::
   :caption: 같은 최근 수술담보 코호트(2025-1분기)에 CredibleLoss를 적용. 형상은 PooledLoss와 같지만, 이 코호트가 포트폴리오 평균보다 낮게 달려온 만큼 수준이 아래로 보정되어 약 1.23에 닿는다(PooledLoss는 약 1.30).

   import polars as pl
   import lossratio as lr
   import matplotlib.pyplot as plt

   df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
   tri = lr.Triangle(df, groups="coverage", grain="Q")
   COH = pl.lit("2025-01-01").str.to_date()

   def _traj(model):
       return (model().fit(tri).df
               .filter(pl.col("cohort") == COH).sort("duration"))

   pooled, cred = _traj(lr.PooledLoss), _traj(lr.CredibleLoss)
   duration = pooled["duration"].to_list()
   src = pooled["source"].to_list()
   ratio_all = pooled["ratio_proj"].to_list()
   # 관측된 손해율 = source == "observed" 인 셀의 ratio_proj
   obs_pairs = [(d, r) for d, r, s in zip(duration, ratio_all, src) if s == "observed"]

   fig, ax = plt.subplots(figsize=(6.2, 3.6))
   ax.plot([d for d, r in obs_pairs], [r for d, r in obs_pairs],
           "-o", color="black", ms=4, label="observed")
   ax.plot(duration, pooled["ratio_proj"].to_list(), "--", color="#1f77b4",
           alpha=0.4, label="PooledLoss")
   ax.plot(duration, cred["ratio_proj"].to_list(), "-", color="#2ca02c", lw=2,
           label="CredibleLoss")
   ax.set_xlabel("duration (quarters)")
   ax.set_ylabel("cumulative loss ratio")
   ax.legend()
```

코호트별 예측 손해율을 PooledLoss와 나란히 두면 보정의 방향이 보인다.

```python
df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df, groups="coverage", grain="Q")

pooled = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(tri).summary()
cred = lr.Ratio(loss=lr.CredibleLoss(), premium=lr.PooledPremium()).fit(tri).summary()
#> 코호트        PooledLoss ratio_proj   CredibleLoss ratio_proj
#> 2025-01-01    1.299                   1.227    <- 평균보다 낮게 달려온 코호트: 아래로
#> 2025-04-01    1.345                   1.237
#> 2025-07-01    1.375                   1.346
#> 2025-10-01    1.406                   1.406    <- 관측 1개뿐: 스케일이 1에 묶여 PooledLoss와 같음
```

```{admonition} CredibleLoss는 어디에 좋고 어디에 좋지 않나
:class: important

CredibleLoss가 고치는 것은 **집계 수준의 체계적 편향**이다 — 코호트 수준
차이를 지우는 PooledLoss의 풀링이 포트폴리오 합계를 한쪽으로 치우치게 할
때, 특히 관측이 얇은 최근 코호트가 많은 크고 매끄러운 담보에서 효과가
있다. 반면 **셀 단위 정확도는 대체로 PooledLoss가 낫다** — 코호트별
스케일은 수준 편향을 줄이는 대신 셀 단위의 잡음을 더한다. 그리고 담보에
따라서는 보정이 집계 편향을 오히려 키우기도 하므로, 일괄 적용하지 말고
**담보별 백테스트로 확인한 뒤** 채택한다(7장).

또 하나 — CredibleLoss의 표준오차·신뢰구간은 닫힌형이 없어 **부트스트랩
전용**이다. `uncertainty=lr.ResidualBootstrap(...)`로 켜며, 4장에서 다시
본다.
```

배합에 쓰인 코호트별 신뢰도 $Z$는 결과 객체에서 직접 꺼내 볼 수 있다 —
`.credibility`가 **코호트마다 한 줄씩** 돌려준다.

```python
lr.CredibleLoss().fit(tri).credibility.head(3)
#> shape: (3, 5)
#> ┌──────────┬────────────┬──────────┬──────────┬──────────┐
#> │ coverage ┆ cohort     ┆ u        ┆ Z        ┆ psi      │
#> │ str      ┆ date       ┆ f64      ┆ f64      ┆ f64      │
#> ╞══════════╪════════════╪══════════╪══════════╪══════════╡
#> │ SURGERY  ┆ 2023-01-01 ┆ 1.000962 ┆ 0.998398 ┆ 0.010064 │
#> │ SURGERY  ┆ 2023-04-01 ┆ 1.001590 ┆ 0.995594 ┆ 0.010064 │
#> │ SURGERY  ┆ 2023-07-01 ┆ 1.005517 ┆ 0.990339 ┆ 0.010064 │
#> └──────────┴────────────┴──────────┴──────────┴──────────┘
```

- `u` — 그 코호트의 신뢰도-가중 **수준 배율**(위 $s_c$; `u=1`이면 풀 평균과 같다).
- `Z` — 그 코호트의 신뢰도 **가중치**(위 배합의 $Z$). 노출이 쌓인 초기 코호트는
  `Z`가 1에 가깝고(자기 수준을 거의 그대로 믿는다), 갓 인수된 얇은 코호트는
  0에 가깝다(대부분 풀 평균으로 당겨진다).
- `psi` — 코호트 간 분산 $\hat\psi$(모든 코호트 공통 값).

```{admonition} `Z`를 "이만큼 믿을 수 있다"로 읽지 말 것
:class: warning

`Z`는 **코호트당 하나**다 — 경과 셀별로 나오는 값이 아니라, 그 코호트의 *자기
관측*을 최종 추정에 얼마나 반영했는지의 배합 비율이다(`1-Z`는 풀 평균에서
차용). 그래서 "이 예측이 `Z`만큼 맞다"는 신뢰·확률 점수가 **아니다**. 게다가
`Z`는 노출($n$)과 코호트 간 이질성($\psi$)만 반영하므로 실제 예측 불안정성의
일부만 설명한다 — 어느 코호트를 자기 데이터로 믿을 만한지의 *순서* 지표이지,
정밀한 신뢰 점수가 아니다.
```

## 3.3 형상 평활 — SmoothLoss

CredibleLoss가 코호트의 *수준*을 보정했다면, **SmoothLoss**는 풀링 *형상*을
다듬는다. PooledLoss의 강도 $g_k$는 경과마다 자유모수로(경과별로 따로) 추정하므로,
관측이 두꺼운 큰 담보에선 그 형상이 이미 매끄럽다 — 그런 곳에선 SmoothLoss와
PooledLoss가 사실상 겹친다(앞 절들이 SURGERY 같은 큰 담보를 쓴 이유다).

값이 갈리는 곳은 **얇은 세그먼트의 후기 경과**다. 거기선 한 경과에 모인
코호트가 몇 개뿐이라 풀링 강도 자체가 들쭉날쭉해진다. SmoothLoss는 그 형상에
P-spline(2차 차분 벌점)을 씌워 경과에 따라 매끄럽게 변하도록 강제한다 —
$g_k = \exp(s(k))$, $s(k) = B(k)\cdot\beta$.

암(CANCER)담보의 한 채널(TM) 세그먼트로 보면 분명하다(이 세그먼트는 한 경과에
모인 코호트가 30개 안팎에서 시작해 후기 경과로 갈수록 한 자릿수까지 준다).
경과별 강도 $g_k$를 풀링(raw)과 평활로 나란히 두면, 풀링 강도는 경과마다 위아래로
튀는데 SmoothLoss는 그 추세만 남긴다. 풀링 강도는 `tri.link().intensity()`의
`intensity` 열이 경과(from-duration)별로 그대로 돌려준다.

```{list-table} 얇은 세그먼트(암담보·TM)의 후기 경과별 강도 $g_k$ — 풀링(raw) vs 평활
:header-rows: 1
:widths: 18 28 28

* - 경과(월)
  - PooledLoss $g_k$ (풀링 raw)
  - SmoothLoss $g_k$ (평활)
* - 12
  - 0.091  ↑ 튐
  - 0.073
* - 13
  - 0.057  ↓
  - 0.066
* - 14
  - 0.054
  - 0.061
* - 15
  - 0.072  ↑ 튐
  - 0.057
* - 16
  - 0.039  ↓
  - 0.053
* - 17
  - 0.063  ↑ 튐
  - 0.050
* - 18
  - 0.041  ↓
  - 0.047
* - 19
  - 0.048
  - 0.045
* - 20
  - 0.047
  - 0.044
```

풀링 강도(가운데 열)는 0.091→0.057→0.054→…처럼 코호트가 적어 생긴 잡음으로
들쭉날쭉하지만, SmoothLoss(오른쪽 열)는 0.073→0.066→0.061→…으로 매끄럽게
내려간다. 들쭉날쭉함은 단언 대신 숫자로 잴 수 있다 — 곡률(curvature)을 경과축
*2차 차분의 RMS*로 정의하면(작을수록 매끄럽다), 위 표의 두 열에서 곧장 나온다.

```python
import numpy as np

# 위 표의 풀링(raw) vs 평활(smooth) g_k (경과 12-20)
raw    = np.array([0.091, 0.057, 0.054, 0.072, 0.039, 0.063, 0.041, 0.048, 0.047])
smooth = np.array([0.073, 0.066, 0.061, 0.057, 0.053, 0.050, 0.047, 0.045, 0.044])

curv = lambda g: float(np.sqrt(np.mean(np.diff(g, 2) ** 2)))   # 2차 차분의 RMS
print(round(curv(raw), 4), round(curv(smooth), 4),
      round(curv(raw) / curv(smooth), 1))
#> 0.0383 0.0011 35.8
```

곡률이 0.038에서 0.0011로 **약 36배** 줄어든다 — P-spline이 추세만 남기고
표본 잡음을 걷어낸 결과다.

```{admonition} 정직하게 — 이 합성 데이터엔 보탬이 적다
:class: note

위 대비는 *얇은 세그먼트의 후기 경과*에서만 두드러진다. 큰 매끄러운
담보(앞 절의 SURGERY 등)에서는 풀링 강도가 이미 매끄러워 SmoothLoss와
PooledLoss의 예측 손해율이 셀 단위로 거의 같다(중앙값 0%). 그러니 SmoothLoss를
일괄 채택하지 말고, *형상이 거친 세그먼트*에서만 — 7장 백테스트로 보탬을 확인한 뒤
— 쓴다. CredibleLoss처럼 표준오차·신뢰구간은 닫힌형이 없어 **부트스트랩
전용**이다(4장).
```

```{admonition} 보정·평활을 더 다루는 인자들 (covariates / lam_cov / balance)
:class: seealso

`CredibleLoss`·`SmoothLoss`에는 이 장이 쓰지 않은 인자가 더 있다.

- **`covariates=[...]`** — 코호트 *수준*을 코호트마다 따로 추정하는 대신,
  `age_band`·`channel` 같은 세그먼트 키를 공유해 *세그먼트 안에서 풀링*한다.
  얇은 세그먼트의 수준이 그 코호트 하나가 아니라 같은 키를 가진 코호트들에서
  추정되어 더 안정된다. `lam_cov`(기본 `0.0`)는 그 세그먼트 수준을 평균으로
  당기는 능선(ridge) 벌점 — `"auto"`면 데이터에서 고른다. 이 covariate 경로가
  바로 **6장의 covariate regime 처리**가 올라타는 구조다.
- **`balance=False`** — 켜면 적합된 증분의 경과별 합이 관측 합과 맞도록
  강도를 미세 조정한다(집계 정합용). 기본은 끔.

세 인자 모두 *기본값에선 이 장의 결과를 바꾸지 않는다* — 필요할 때만 켜는
확장 손잡이다. 자세한 사용은 6장과 {doc}`API 레퍼런스 <../api>`를 본다.
```

## 3.4 신뢰의 스펙트럼으로 정리

PooledLoss와 ChainLadder는 대립하는 두 방법이 아니라 **하나의 축 위
양 끝**이다 — *관측된 진전 패턴을 얼마나 믿느냐*의 스펙트럼이다.

- **PooledLoss** — 진전 대신 **노출(보험료)에 고정**한다. 게다가 강도
  `g_k`를 *코호트 전체에서 풀링*해 추정하므로, 한 최근 코호트의 들쭉날쭉한
  관측이 아니라 포트폴리오 평균 노출 패턴이 예측을 끈다(안정적인 대신
  노출 패턴 유지를 가정).
- **ChainLadder** — 진전을 전적으로 믿는다. 코호트가 쌓은 누적 손해에 ATA
  인자를 곱해, 관측된 진전을 그대로 미래로 연장한다. 충분히 경과한
  코호트엔 정확하지만, 얇은 초기 관측에 곱셈을 거듭하면 출렁인다.

이 "노출 앵커 + 코호트 풀링"이 핵심이다. 손해보험 reserving에서는 외부
prior 손해율을 손수 넣거나(Bornhuetter-Ferguson), 데이터에서 풀링해(Cape
Cod) 최근 코호트를 안정시킨다. 장기 건강보험에서는 **PooledLoss가 이미 그
역할을 한다** — 보험료라는 외부 기준에 고정하고 강도를 코호트 간 풀링하므로,
별도의 prior 층을 얹지 않아도 최근 코호트가 노출 수준에 머문다.

CredibleLoss는 이 스펙트럼과 **직교하는 보정**이다. 진전 패턴(형상)에 대한
믿음은 PooledLoss의 풀링 그대로 두고, 코호트가 자기 *수준*을 말할 자격 —
곧 관측이 쌓인 정도 — 만큼만 그 수준을 반영한다. 형상의 축(PooledLoss vs
ChainLadder)과 수준의 축(풀링 vs 코호트별)이 별개라는 것이, 이 장 방법들의
가장 간결한 지도다. SmoothLoss는 이 지도에서 *형상의 축*에 놓인다 — PooledLoss의
풀링 형상을 매끄러운 쪽으로 다듬은 변형이라, 수준의 축에선 PooledLoss와 같은
자리다(그래서 매끄러운 담보에선 둘이 사실상 겹친다).

## 3.5 어느 방법을 언제

```{list-table}
:header-rows: 1
:widths: 12 88

* - 방법
  - 어울리는 상황
* - PooledLoss
  - 초기 구간이 많거나 ATA 인자가 불안정할 때의 안전한 기본값. 의심스러우면 여기서 시작
* - CredibleLoss
  - 포트폴리오 집계 수준의 체계적 편향이 보일 때. 담보별 백테스트로 보정 효과를 확인한 뒤 채택
* - SmoothLoss
  - 경과별 강도 형상이 들쭉날쭉할 때(얇은 late-tail·thin 세그먼트). 매끄러운 큰 담보에선 PooledLoss와 사실상 같아 추가 이득이 없음
* - ChainLadder
  - 업계 관행과의 소통·검증, 방법 비교의 참조점. 최근 코호트엔 위험
```

베이스라인 `PooledLoss`로 시작해, 수준 보정이 보탬이 된다는 근거가
백테스트로 확인되면 CredibleLoss를 채택하는 것이 실무의 출발점이다. 어느
방법이 *내 데이터*에 맞는지는 취향이 아니라 검증으로 가린다 — 같은
hold-out에서 두 방법의 held-out 셀을 매칭 비교하는 `lr.EstimatorComparison`이
그 도구다(7장).

## 3.6 함께 보기

- {doc}`2장 — 손해율 예측 <02-projection>`: 이 장이 다듬는 베이스라인
  PooledLoss·PooledPremium·Ratio와 결과 객체의 `.predict()`·`.plot()`.
- {doc}`4장 — 예측의 불확실성 <05-uncertainty>`: 이 장의 점추정에 표준오차와
  신뢰구간을 입힌다. CredibleLoss·SmoothLoss는 닫힌형이 없어 부트스트랩으로만
  구간을 얻는다.
- {doc}`7장 — 예측 검증 <07-backtest>`: 수준 보정·형상 평활이 *내 데이터*에
  보탬이 되는지를 담보별 백테스트와 방법 간 매칭 비교로 가린다.
- {doc}`부록 — 손해의 진전 <02-ata>`와 {doc}`부록 — 강도의 직관 <03-intensity>`:
  이 장이 다듬는 강도 $g_k$(와 대비되는 ATA 인자)의 발전 인자 진단.
- {doc}`투영 메커니즘 <../cookbook/projection-mechanics>`: Pooled/Credible/
  Smooth와 신뢰도 레벨 $u_i$가 각 셀을 어떻게 투영하는지 수식으로 정리.
- {doc}`API 레퍼런스 <../api>`의 `PooledLoss`, `CredibleLoss`, `SmoothLoss`, `Ratio`
