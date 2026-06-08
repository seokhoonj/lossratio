# 6장. 구조 변화 탐지

지금까지의 예측은 조용한 가정 하나에 기대고 있었다 — **모든 코호트가 같은
손해 진전 패턴을 공유한다**는 것. ATA 인자도 강도도 여러 코호트를 합쳐
구했으니, 코호트들이 한목소리를 낸다고 믿은 셈이다.

그러나 현실에서는 어느 시점에 **구조가 바뀐다**. 약관이 개정되고, 요율이
조정되고, 손해사정 방식이 달라진다. 그 전후로 인수된 코호트는 손해 추이가
다른데, 이를 한데 섞어 인자를 구하면 예측이 양쪽의 어중간한 평균으로
뭉개진다. 이 장은 그런 변화를 **찾아내고 예측에 반영하는** 법을 다룬다.

## 6.1 regime이란

**regime**(유사한 손해 추이를 공유하는 인수 코호트들의 묶음)은 "같은 규칙
아래 인수된 코호트들"을 가리킨다. 규칙이 바뀌면 새 regime이 시작된다.

예를 들어 어느 수술 담보가 2024년 7월에 약관을 개정해 보장 범위를 좁혔다고
하자. 그 이후 코호트는 구조적으로 손해율이 낮아진다 — 위험이 좋아져서가
아니라 **상품 자체가 달라졌기** 때문이다. 2024년 6월 코호트와 8월 코호트는
이름만 이웃일 뿐, 서로 다른 모집단이다.

```{admonition} 왜 섞으면 안 되는가
:class: important

chain ladder의 숨은 전제는 "하나의 일관된 진전 패턴"이다. regime 변화는 이
전제를 깬다. 변화 전(높은 손해율)과 후(낮은 손해율) 코호트를 합쳐 인자를
구하면, 그 인자는 어느 쪽도 아닌 평균이 된다. 그 평균으로 최근 코호트를
예측하면 **실제보다 높게** 잡힌다 — 이미 끝난 옛 규칙의 손해를 새 코호트에
덮어씌우는 셈이다.
```

## 6.2 변화 지점 찾기 — E-Divisive

변화가 *언제* 일어났는지 눈대중하지 않고 데이터가 말하게 한다.
`detect_regime()`은 코호트별 손해율 흐름에서 **분포가 달라지는 지점**을
찾는다. 기본 알고리즘은 **E-Divisive**(분포 변화점을 비모수적으로 찾는
방법 — 정규성 같은 가정 없이 통계적으로 유의한 단절만 짚어 낸다)다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df, groups="coverage", grain="Q")

reg = tri.detect_regime()
reg.changes
#> ┌──────────┬────────────┬───────────┐
#> │ coverage ┆ change     ┆ regime_id │
#> ╞══════════╪════════════╪═══════════╡
#> │ SURGERY  ┆ 2024-07-01 ┆ 2         │
#> └──────────┴────────────┴───────────┘
```

내장 수술담보 데이터에는 2024년 7월에 손해율이 한 단계 낮아지는 구조 변화가
심어져 있는데, `detect_regime()`이 정확히 그 지점을 짚어 낸다.

```{admonition} 자동 윈도우는 ATA 인자 안정점에서 온다
:class: note

`detect_regime(window="auto")`(기본값)는 코호트별 손해율 흐름을 얼마나 긴
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
   reg = tri.detect_regime()

.. plot::
   :context: close-figs
   :caption: 수술담보 코호트의 regime 분할. 2024년 7월(24.07)을 경계로 이전(regime 1)과 이후(regime 2)가 나뉜다.

   reg.plot()
```

## 6.3 예측에 반영하기

찾은 regime을 적합에 넘기면, 패키지가 **regime별로 인자를 따로 추정**해
변화 전후를 섞지 않는다. 손해율 합성에는 `loss_regime`, 단일 역할 적합에는
`regime` 인자로 전달한다.

```python
reg = tri.detect_regime()

base    = lr.Ratio(method="ed").fit(tri)                  # regime 무시
trimmed = lr.Ratio(method="ed", loss_regime=reg).fit(tri) # regime 반영
```

효과는 변화 이후의 최근 코호트에서 가장 크다. regime을 무시하면 가장 최근
코호트(2025-4분기)의 예측 손해율이 **1.41**로 잡히지만, 이는 이미 끝난 옛
regime의 높은 손해율이 섞여 부풀려진 값이다. regime을 반영하면 새 regime의
코호트만으로 추정해 **0.92**로 내려간다 — 달라진 상품의 실제 수준에 더
가깝다.

왜 섞으면 안 되는지 도식으로 보면 — 전체를 섞으면(점선) 옛 regime의 높은
손해율이 섞여 1.41로 부풀고, 새 regime만으로 추정하면(`o`) 0.92로 실제
수준에 가깝다:

```text
 loss ratio
  1.5 |********                regime 1 (old product, high)
      |        ****
      |            - - - - -   blended estimate -> 1.41 (old level mixed in)
  1.0 |              oooooo    new-regime-only estimate -> 0.92 (true level)
  0.9 |
      +-------------|--------> cohort (uy)
              2024-07 (detected change)
```

```{admonition} regime과 recent는 다른 축
:class: note

5장까지 본 `recent`는 최근 N개의 *달력 대각선*만 보는 필터(시간 축)이고,
regime은 *코호트*를 기준으로 변화 이전을 잘라 내는 필터다. 둘은 직교해 함께
쓸 수 있다 — recent로 최근 흐름에 집중하면서, regime으로 옛 규칙을 배제하는
식이다.
```

## 6.4 손해와 보험료는 따로 본다

구조 변화는 손해 쪽과 보험료 쪽에서 **따로** 일어날 수 있다. 손해 쪽
변화는 약관 개정·손해사정 변경에서, 보험료 쪽 변화는 요율 개정·판매 채널
이동에서 온다. 두 사건이 같은 시점일 이유는 없으므로, 손해율
합성(`Ratio`)에서는 `loss_regime`과 `premium_regime`을 따로 받는다(기본은
손해 쪽 설정을 보험료 쪽에도 적용).

```python
lr.Ratio(method="ed",
         loss_regime=lr.Regime.at(change="2024-07-01"),     # 손해 쪽 변화
         premium_regime=lr.Regime.at(change="2024-01-01")   # 보험료 쪽 변화
         ).fit(tri)
```

수동으로 변화 시점을 알고 있으면 `Regime.at()`으로 직접 지정할 수도 있다.

## 6.5 함께 보기

- {doc}`4장 — 손해율 예측 <04-projection>`: regime이 가장 크게 작용하는
  최근 코호트의 예측이 방법마다 갈리는 양상.
- {doc}`7장 — 예측 검증 <07-backtest>`: regime 반영이 실제로 예측을
  개선하는지 과거 시점에서 되짚어 확인한다.
- {doc}`API 레퍼런스 <../api>`의 `Triangle.detect_regime`, `Regime`,
  `Regime.at`
