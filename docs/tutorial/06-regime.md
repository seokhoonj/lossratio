# 5장. 구조 변화 탐지

지금까지의 예측은 조용한 가정 하나에 기대고 있었다 — **모든 코호트가 같은
손해 진전 패턴을 공유한다**는 것. ATA 인자도 강도도 여러 코호트를 합쳐
구했으니, 코호트들이 한목소리를 낸다고 믿은 셈이다.

그러나 현실에서는 어느 시점에 **구조가 바뀐다**. 약관이 개정되고, 요율이
조정되고, 손해사정 방식이 달라진다. 그 전후로 인수된 코호트는 손해 추이가
다른데, 이를 한데 섞어 인자를 구하면 예측이 양쪽의 어중간한 평균으로
뭉개진다. 이 장은 그런 변화를 **찾아내고 예측에 반영하는** 법을 다룬다.

## 5.1 regime이란

**regime**(유사한 손해 추이를 공유하는 인수 코호트들의 묶음)은 "같은 규칙
아래 인수된 코호트들"을 가리킨다. 규칙이 바뀌면 새 regime이 시작된다.

예를 들어 어느 수술 담보가 2024년 7월에 약관을 개정해 보장 범위를 좁혔다고
하자. 그 이후 코호트는 구조적으로 손해율이 낮아진다 — 위험이 좋아져서가
아니라 **상품 자체가 달라졌기** 때문이다. 2024년 6월 코호트와 8월 코호트는
이름만 이웃일 뿐, 서로 다른 모집단이다.

```{admonition} 왜 섞으면 안 되는가
:class: important

링크비(`ChainLadder`)의 숨은 전제는 "하나의 일관된 진전 패턴"이다. regime 변화는 이
전제를 깬다. 변화 전(높은 손해율)과 후(낮은 손해율) 코호트를 합쳐 인자를
구하면, 그 인자는 어느 쪽도 아닌 평균이 된다. 그 평균으로 최근 코호트를
예측하면 **실제보다 높게** 잡힌다 — 이미 끝난 옛 규칙의 손해를 새 코호트에
덮어씌우는 셈이다.
```

## 5.2 변화 지점 찾기 — E-Divisive

변화가 *언제* 일어났는지 눈대중하지 않고 데이터가 말하게 한다.
`RegimeDetector()`은 코호트별 손해율 흐름에서 **분포가 달라지는 지점**을
찾는다. 기본 알고리즘은 **E-Divisive**(분포 변화점을 비모수적으로 찾는
방법 — 정규성 같은 가정 없이 통계적으로 유의한 단절만 짚어 낸다)다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df, groups="coverage", grain="Q")

reg = lr.RegimeDetector().detect(tri)
reg.changes
#> ┌──────────┬────────────┬───────────┐
#> │ coverage ┆ change     ┆ regime_id │
#> ╞══════════╪════════════╪═══════════╡
#> │ SURGERY  ┆ 2024-07-01 ┆ 2         │
#> └──────────┴────────────┴───────────┘
```

내장 수술담보 데이터에는 2024년 7월에 손해율이 한 단계 낮아지는 구조 변화가
심어져 있는데, `RegimeDetector()`이 정확히 그 지점을 짚어 낸다.

```{admonition} 자동 윈도우는 ATA 인자 안정점에서 온다
:class: note

`RegimeDetector(window="auto")`(기본값)는 코호트별 손해율 흐름을 얼마나 긴
경과까지 보고 변화를 가릴지를 **ATA 인자 안정점**(age-to-age factor가
CV·RSE 기준으로 안정되는 경과)에서 자동으로 정한다. 이 신호가 없으면
elbow 휴리스틱으로, 그래도 안 잡히면 고정 기본값으로 차례로 물러난다.
정수를 직접 주면(`window=12`) 그 길이로 고정한다.
```

코호트를 regime별로 색칠해 보면 변화가 한눈에 들어온다.

```{eval-rst}
.. plot::
   :context:
   :nofigs:
   :include-source: false

   import polars as pl
   import lossratio as lr

   df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
   tri = lr.Triangle(df, groups="coverage", grain="Q")
   reg = lr.RegimeDetector().detect(tri)

.. plot::
   :context: close-figs
   :caption: 수술담보 코호트의 regime 분할. 2024년 7월(24.07)을 경계로 이전(regime 1)과 이후(regime 2)가 나뉜다.

   reg.plot()
```

## 5.3 후보가 진짜 regime인가 — step vs drift

E-Divisive는 분포가 *달라지는* 지점을 짚지만, 그 자체로 "regime 변화"를
보장하진 않는다. 손해율이 *완만한 추세*로만 움직여도(예: 의료수가 트렌드로
서서히 오름) 그 흐름을 둘로 가르는 후보가 나올 수 있는데, 그건 *불연속
단차*(step)가 아니라 *연속 표류*(drift)다.

두 양상은 그림으로 갈린다 — drift는 직선 하나로 잘 맞고, step은 어느 시점에
계단이 있어 직선으론 못 맞춘다:

```text
drift -- 매끄럽게 흐름: 직선 하나로 잘 맞는다

  손해율
  0.70|                   o
      |                o
      |             o
  0.60|          o
      |       o
  0.50|    o
      +---------------------- 코호트 순번

step -- 한 시점에 계단: 직선으론 못 맞춘다

  손해율
  0.70|             o  o  o  o
      |
  0.50| o  o  o  o
      +---------------------- 코호트 순번
                  ^ 변화점에서 점프
```

그래서 `RegimeDetector()`은 각 후보를 **step/drift F-검정**으로 한 번 더 거른다.
코호트 수준 손해율을 *선형 추세*만으로 적합한 것(귀무)과, 거기에 *변화점의
단차*를 더한 것(대립)을 비교해, 단차가 통계적으로 유의하게 적합을 개선할
때만 **step**(진짜 regime)으로 인정한다. 여기서 적합하는 건 손해 진전
모형(CL)이 아니라 **코호트별 손해율 수준을 코호트 순번에 회귀**하는
진단용 직선이다 — 절편은 기준 수준, 기울기는 완만한 추세, 단차는 점프를
담는다:

$$F = \frac{(\text{SSE}_{\text{추세}} - \text{SSE}_{\text{추세}+\text{단차}})/1}{\text{SSE}_{\text{추세}+\text{단차}}/(n-3)},\qquad \text{step\_p} = \Pr\!\big(F_{1,\,n-3} > F\big)$$

분모의 `n-3`은 대립모형이 모수 *3개*(절편·기울기·단차)를 쓰고 남은
**잔차 자유도**다 — 그것으로 나눠야 "잡음 한 칸"의 분산이 된다. 그래서
코호트가 4개 미만이면(`n-3 < 1`) 검정을 건너뛰고 `step_p`를 비워 둔다.

- **step** (`step_p < 0.05`) — 추세 위에 *불연속 단차*가 유의 -> 진짜 regime.
- **drift** (`step_p >= 0.05`) — 추세가 이미 흐름을 설명 -> regime 아님.

투명 테이블 `reg.candidates`가 후보마다 이 진단(`kind`/`step_p`/단차 크기
`level_shift`)을 함께 싣는다.

```python
reg.candidates.select(["change", "kind", "step_p", "level_shift"])
#> ┌────────────┬──────┬──────────┬─────────────┐
#> │ change     ┆ kind ┆ step_p   ┆ level_shift │
#> ╞════════════╪══════╪══════════╪═════════════╡
#> │ 2024-07-01 ┆ step ┆ 0.003543 ┆ -0.403261   │
#> └────────────┴──────┴──────────┴─────────────┘
```

심어 둔 2024-07 변화는 `step_p = 0.0035`(< 0.05)라 **step**이고, `level_shift =
-0.40`은 손해율이 약 40% 낮아졌다는 뜻 — 정확히 심어 둔 0.6배 수준 하락이다.

F 값이 어디서 오는지 실제 코호트 계열로 직접 확인할 수 있다. 코호트별 손해율
수준(경과 `window`까지의 평균 누적 손해율)을 *추세만으로* 적합한 잔차제곱합
`SSE_trend`과, 거기에 2024-07 단차를 더해 적합한 `SSE_step`을 비교한 비가 F다.

```python
import numpy as np
from datetime import date
from scipy.stats import f as f_dist

w = reg.window  # auto 로 정해진 추세 윈도우
lvl = (tri.to_polars()
       .filter(pl.col("duration") <= w)
       .group_by("cohort")
       .agg(n=pl.col("duration").n_unique(), level=pl.col("ratio").mean())
       .filter(pl.col("n") >= w)
       .sort("cohort"))
y = lvl["level"].to_numpy()
n = len(y)
change_idx = lvl["cohort"].to_list().index(date(2024, 7, 1))  # regime 2 첫 코호트
idx, ones = np.arange(n, dtype=float), np.ones(n)
step = (idx >= change_idx).astype(float)

def sse(cols):
    beta, *_ = np.linalg.lstsq(np.column_stack(cols), y, rcond=None)
    resid = y - np.column_stack(cols) @ beta
    return float(resid @ resid)

sse_trend = sse([ones, idx])               # 절편 + 추세
sse_step  = sse([ones, idx, step])         # 절편 + 추세 + 단차
F = ((sse_trend - sse_step) / 1.0) / (sse_step / (n - 3))
print(round(sse_trend, 5), round(sse_step, 5), round(F, 2),
      round(float(f_dist.sf(F, 1, n - 3)), 6))
#> 0.14125 0.04587 16.63 0.003543
```

단차를 넣으니 잔차제곱합이 `0.14125 -> 0.04587`로 1/3 토막 난다 — 추세만으로는
못 잡던 변동의 대부분을 단차가 설명한다는 뜻이다. `F = 16.63`은 그 개선을
잔차 한 칸 분산으로 정규화한 값이고, `step_p = 0.0035`는 위 `reg.candidates`의
값과 정확히 일치한다.

```{admonition} 왜 단차 *크기*가 아니라 F-검정인가
:class: note

F-검정은 **n-aware** — 표본 크기와 잡음 규모를 반영한다. 그래서 "큰 단차지만
잡음도 커서 못 믿겠다"와 "작지만 또렷한 단차"를 옳게 가른다. raw 단차 크기
문턱은 이 둘을 구분 못 한다. (Welch t값도 `reg.candidates`에 함께 보고되지만
*게이트로는 쓰지 않는다* — 추세가 만든 양쪽 평균 차이에도 반응해 step/drift
판정엔 부적합하다.)
```

후보 테이블을 사람이 일일이 읽는 대신, `reg.evaluate()`가 각 후보를
**수용 게이트**로 점수화한다 — 변화 크기(`level_shift`)가 충분히 크고
(`material`) 통계적으로 유의하면(`sig`) `action = "regime"`으로 채택하고,
`confidence`(0~1)를 매긴다.

```python
reg.evaluate(material=0.05, sig=0.05).select(
    ["change", "kind", "level_shift", "step_p", "action", "confidence", "accept"]
)
#> ┌────────────┬──────┬─────────────┬──────────┬────────┬────────────┬────────┐
#> │ change     ┆ kind ┆ level_shift ┆ step_p   ┆ action ┆ confidence ┆ accept │
#> ╞════════════╪══════╪═════════════╪══════════╪════════╪════════════╪════════╡
#> │ 2024-07-01 ┆ step ┆ -0.403261   ┆ 0.003543 ┆ regime ┆ 0.849965   ┆ true   │
#> └────────────┴──────┴─────────────┴──────────┴────────┴────────────┴────────┘
```

`reg.accepted()`는 한 걸음 더 나아가, 채택된 변화만 추려 **다시 `Regime`
객체로** 돌려준다 — 그대로 적합의 `regime` 슬롯에 넘기면 검토한 그 변화점이
실제 예측을 끊는다. 점수화·검토와 적합이 같은 변화점을 쓰도록 닫는 고리다.

```python
fit = lr.Ratio(loss=lr.PooledLoss(regime=reg.accepted())).fit(tri)
```

## 5.4 예측에 반영하기

찾은 변화를 적합에 넘기면, 기본 동작(`treatment = "latest_only"`)에서는 패키지가
그 시점 이전의 코호트를 잘라 내 **새 regime의 코호트만으로 인자를 추정**해 변화
전후를 섞지 않는다. 손해쪽 추정기(`PooledLoss` 등)의 `regime` 인자에
`RegimeDetector()`이 돌려준 `Regime` 객체를 그대로 넘기면 된다 — 적합 시점에
코호트 컷으로 해소된다. (옛 regime을 *버리지 않고* 함께 쓰는 다른 두 방식은
§5.5에서 다룬다.)

```python
base    = lr.Ratio(                       # regime 무시
    loss=lr.PooledLoss(),
    premium=lr.PooledPremium(),
).fit(tri)

trimmed = lr.Ratio(                       # RegimeDetector 의 Regime 을 그대로 반영
    loss=lr.PooledLoss(regime=reg),
    premium=lr.PooledPremium(),
).fit(tri)

# 가장 최근 코호트의 예측 손해율 대비
both = (base.summary().select(["cohort", "ratio_proj"])
        .join(trimmed.summary().select(["cohort", "ratio_proj"]),
              on="cohort", suffix="_trimmed")
        .sort("cohort").tail(2))
print(both)
#> ┌────────────┬────────────┬────────────────────┐
#> │ cohort     ┆ ratio_proj ┆ ratio_proj_trimmed │
#> ╞════════════╪════════════╪════════════════════╡
#> │ 2025-07-01 ┆ 1.374637   ┆ 0.890296           │
#> │ 2025-10-01 ┆ 1.406452   ┆ 0.919844           │
#> └────────────┴────────────┴────────────────────┘
```

값이 갈리는 까닭은 옛 regime 코호트의 손해율이 실제로 높기 때문이다. §5.3에서
구한 코호트별 손해율 수준을 보면, 잘려 나가는 2024-07 이전 코호트가 1.0
안팎인 반면 살아남는 이후 코호트는 0.6~0.7대다. `latest_only`가 이 높은 옛
코호트들을 빼므로 최근 코호트 예측이 `1.41 -> 0.92`로 내려간다.

```python
lvl.with_columns(
    regime=pl.when(pl.col("cohort") >= date(2024, 7, 1)).then(pl.lit(2)).otherwise(1)
).select(["cohort", "level", "regime"])
#> ┌────────────┬──────────┬────────┐
#> │ cohort     ┆ level    ┆ regime │
#> ╞════════════╪══════════╪════════╡
#> │ 2023-01-01 ┆ 1.187284 ┆ 1      │   ← regime 1 (잘림): 손해율 높음
#> │   …        ┆   …      ┆ 1      │
#> │ 2024-04-01 ┆ 1.032135 ┆ 1      │
#> │ 2024-07-01 ┆ 0.681348 ┆ 2      │   ← regime 2 (유지): 손해율 낮음
#> │   …        ┆   …      ┆ 2      │
#> │ 2025-07-01 ┆ 0.637823 ┆ 2      │
#> └────────────┴──────────┴────────┘
```

```{admonition} regime 인자가 받는 형태
:class: note

`regime`은 `None`, `datetime.date`(그 시점 이전 코호트를 드롭),
`dict[담보 -> date]`(담보별 시점)에 더해, **`RegimeDetector()`이 돌려준
`Regime` 객체**, **`Regime(change=...)`로 손수 지정한 변화점**, 그리고
**`RegimeDetector(...)` 탐지기 객체**(탐지를 적합·백테스트 시점까지 미룸)까지
받는다 — `latest_only`에서는 모두 적합 시점에 코호트 컷으로 해소된다.

`Regime(change=...)`은 변화 시점을 *이미 알 때*(예: 약관 개정일) 즉시 고정한 `Regime`을
만든다. 반면 `RegimeDetector(...)`는 탐지 파라미터만 담은 config 객체로, 그 자체를
`regime=`에 넘기면 *탐지를 적합·백테스트 시점까지 미룬다* — 백테스트에서는 각
fold의 *마스킹된* 학습 데이터로 다시 탐지되어, 변화 시점이 hold-out 셀을 엿보지
않는다(leakage-safe). 즉시 탐지가 필요하면 `RegimeDetector(...).detect(tri)`를
부른다.
```

```python
lr.Regime(change="2024-07-01", groups={"coverage": ["SURGERY"]})  # 수동 고정
lr.RegimeDetector(target="ratio", window=12)                      # 지연 탐지기
```

효과는 변화 이후의 최근 코호트에서 가장 크다. regime을 무시하면 가장 최근
코호트(2025-4분기)의 예측 손해율이 **1.41**로 잡히지만, 이는 이미 끝난 옛
regime의 높은 손해율이 섞여 부풀려진 값이다. regime을 반영하면 새 regime의
코호트만으로 추정해 **0.92**로 내려간다 — 달라진 상품의 실제 수준에 더
가깝다.

왜 섞으면 안 되는지는 아래 도식이 한눈에 보여 준다. 전체 코호트를 섞으면 옛
regime의 높은 손해율이 섞여 최근 코호트가 **1.41**(점선)로 부풀고, 변화 이후
새 regime 코호트만으로 추정하면 **0.92**(`o`)로 실제 수준에 가깝다:

```text
 손해율
  1.5 | *  *                regime 1 (옛 상품): 실제로 높음
  1.4 | - - - - - - - - -   섞은 추정 -> 1.41  (옛 수준이 섞여 부풀음)
      |
  1.0 |
  0.9 |          o  o  o    새 regime만 추정 -> 0.92  (실제 수준)
      +---------|-------> 코호트 (uy)
            2024-07 변화
```

```{admonition} regime과 recent는 다른 축
:class: note

4장까지 본 `recent`는 최근 N개의 *달력 대각선*만 보는 필터(시간 축)이고,
regime은 *코호트*를 기준으로 변화 이전을 잘라 내는 필터다. 둘은 직교해 함께
쓸 수 있다 — recent로 최근 흐름에 집중하면서, regime으로 옛 규칙을 배제하는
식이다.
```

## 5.5 옛 regime을 버리지 않는 두 방식 — `treatment`

§5.4의 `latest_only`는 가장 단순한 선택 — 옛 regime을 통째로 버린다. 하지만
옛 코호트는 **깊은 경과**까지 관측된 유일한 자료이기도 하다. 새 regime 코호트는
아직 어려서 깊은 경과의 진전을 모르는데, 그 정보를 옛 regime에서 *빌려* 오면
버리지 않고도 변화를 반영할 수 있다. 추정기(estimator)의 `treatment=` 인자가
그 반영 방식을 정한다.

- **`"latest_only"`**(기본) — 변화 이전 코호트를 *전부 드롭*하고 새 regime의
  코호트만으로 인자를 추정한다. 가장 단순하고 보수적.
- **`"segment_wise"`** — *모든* regime을 유지한다. 각 regime은 자기 코호트로
  자기 수준·자기 모양을 자기 관측 깊이까지 적합하고, **관측 불가능한 깊은
  꼬리(tail)**만 더 깊은(옛) regime의 *수준 불변* link 인자에서 빌려 온다.
  3장에서 본 `borrowed` 셀 유형이 바로 이 방식에서 나온다.
- **`"covariate"`** — 역시 모든 regime을 유지하되, regime을 *공변량*으로 넣는다.
  하나의 **공유 경과 모양**($g_k$)에 **regime별 수준**을 얹어, 관측 셀이 적은 regime의
  수준을 그 코호트들에 걸쳐 풀링(pooling)한다. 신뢰도 추정기에 맞물려 동작하므로
  `CredibleLoss`/`SmoothLoss`와 함께 쓴다.

세 방식이 변화 이후(새 regime) 코호트의 예측 손해율을 어떻게 다르게 내는지
직접 비교한다 — `latest_only`는 옛 코호트를 버려 추정 표본이 가장 적고,
나머지 둘은 옛 regime의 깊은 정보를 끌어 와 약간 다른 수준을 낸다.

```python
out = []
r = lr.RegimeDetector().detect(tri)  # 탐지는 treatment과 무관 — 한 번만
for tr in ["latest_only", "segment_wise", "covariate"]:
    est = lr.CredibleLoss if tr == "covariate" else lr.PooledLoss
    s = (lr.Ratio(loss=est(regime=r, treatment=tr), premium=lr.PooledPremium()).fit(tri)
         .summary().sort("cohort").tail(3)
         .select(["cohort", pl.col("ratio_proj").alias(tr)]))
    out.append(s)

from functools import reduce
reduce(lambda a, b: a.join(b, on="cohort"), out)
#> ┌────────────┬─────────────┬──────────────┬───────────┐
#> │ cohort     ┆ latest_only ┆ segment_wise ┆ covariate │
#> ╞════════════╪═════════════╪══════════════╪═══════════╡
#> │ 2025-04-01 ┆ 0.898270    ┆ 0.918689     ┆ 0.919509  │
#> │ 2025-07-01 ┆ 0.890296    ┆ 0.910535     ┆ 0.929589  │
#> │ 2025-10-01 ┆ 0.919844    ┆ 0.940754     ┆ 0.928665  │
#> └────────────┴─────────────┴──────────────┴───────────┘
```

세 방식 모두 옛 regime의 높은 손해율에 오염되지 않아 0.9 안팎으로 모인다
(`latest_only`가 가장 낮고, 깊은 꼬리를 빌려 오는 두 방식이 조금 높다). 어느
방식이 나은지는 데이터마다 다르므로 7장의 백테스트로 가린다.

## 5.6 손해와 보험료는 따로 본다

구조 변화는 손해 쪽과 보험료 쪽에서 **따로** 일어날 수 있다. 손해 쪽
변화는 약관 개정·손해사정 변경에서, 보험료 쪽 변화는 요율 개정·판매 채널
이동에서 온다. 두 사건이 같은 시점일 이유는 없으므로, 손해율
합성(`Ratio`)에서는 손해쪽 추정기와 보험료쪽 추정기에 `regime`을 **따로**
지정해 서로 다른 변화 시점을 줄 수 있다.

```python
from datetime import date

lr.Ratio(
    loss=lr.PooledLoss(regime=date(2024, 7, 1)),        # 손해 쪽 변화
    premium=lr.PooledPremium(regime=date(2024, 1, 1)),  # 보험료 쪽 변화
).fit(tri)
```

수동으로 변화 시점을 알고 있으면 이렇게 손해쪽·보험료쪽에 각각 `date`로
직접 지정한다.

## 5.7 함께 보기

- {doc}`3장 — 코호트 보정 <04-projection>`: regime이 가장 크게 작용하는
  최근 코호트의 예측이 방법(PooledLoss vs CredibleLoss)마다 갈리는 양상.
- {doc}`7장 — 예측 검증 <07-backtest>`: regime 반영(과 세 `treatment` 방식)이
  실제로 예측을 개선하는지 과거 시점에서 되짚어 확인한다.
- {doc}`API 레퍼런스 <../api>`의 `RegimeDetector`, `Regime`,
  `Regime.evaluate`
