# 7장. 예측 검증

5장은 예측의 불확실성을 *공식*으로 추정했다. 그런데 그 추정이 실전에서
정말 맞을까? 그리고 4장의 여러 방법 중 *내 데이터*에는 어느 것이 맞을까?
이런 물음에 답하는 가장 정직한 길은 **과거로 돌아가 직접 맞춰 보는** 것이다.
그것이 백테스트다.

## 7.1 시간을 되돌린다 — 달력 대각선 hold-out

아이디어는 단순하다. **최근 관측 일부를 가린 뒤, 가린 데이터 없이 예측하고,
가렸던 실제값과 비교**한다.

무엇을 가릴까? 1장에서 본 것처럼, 한 달력 시점에 새로 들어오는 관측은
삼각형의 **대각선** 하나를 채운다. 그래서 최근 `holdout`개의 달력 대각선을
가리는 것은 곧 **시계를 그만큼 과거로 되돌리는** 것과 같다 — 그 시점의
우리가 아직 몰랐던, 그래서 예측해야 했던 바로 그 칸들이다.

```{admonition} 왜 대각선인가 — 코호트가 아니라
:class: note

코호트(가로줄)를 통째로 가리면 "한 번도 본 적 없는 코호트"를 예측하는 셈이라
실전과 다르다. 실제로 우리는 *모든* 코호트를 조금씩은 관측한 상태에서,
*최근 시점 이후*를 내다본다. 대각선 hold-out이 바로 그 상황을 재현한다.
```

## 7.2 두 가지 오차 지표 — AEG와 A/E Error

가린 칸마다 **실제값**(actual)과 **예측값**(expected)을 비교한다. 차이를 재는
방식은 둘이다.

```{list-table}
:header-rows: 1
:widths: 18 30 52

* - 지표
  - 정의
  - 성격
* - AEG
  - 실제 − 예측
  - 절대 격차(원 단위). 큰 코호트의 절대 오차 비교에 적합
* - A/E Error
  - 실제 / 예측 − 1
  - 상대 오차율(무차원). 코호트 크기를 정규화한 비교에 적합
```

둘은 대체가 아니라 보완이다 — 절대 금액이 궁금하면 AEG, 크기를 떼고 비율로
비교하려면 A/E Error를 본다. 5장의 SE(절대)와 CV(상대) 관계와 같은 짝이다.

## 7.3 백테스트 실행

`Backtest`는 예측에 쓸 **추정기**와 가릴 대각선 수(`holdout`)를 받는다.
무엇을 검증할지는 `target`으로 고른다(`"ratio"` / `"loss"` / `"premium"`).

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SUR")
tri = lr.Triangle(df, groups="coverage", grain="Q")

bt = lr.Backtest(lr.Ratio(method="ed"), holdout=3, target="ratio").fit(tri)
```

결과에는 가린 칸별 오차(`ae_err`)와, 그것을 경과별로 모은 `col_summary`,
달력 대각선별로 모은 `diag_summary`가 들어 있다.

## 7.4 A/E Error 히트맵 읽기

가린 칸들의 A/E Error를 삼각형 그대로 색칠하면 어디서 잘 맞고 어디서
빗나갔는지 한눈에 들어온다.

```{eval-rst}
.. plot::
   :context:
   :nofigs:
   :include-source: false

   import polars as pl
   import lossratio as lr

   df = lr.load_experience().filter(pl.col("coverage") == "SUR")
   tri = lr.Triangle(df, groups="coverage", grain="Q")
   bt = lr.Backtest(lr.Ratio(method="ed"), holdout=3, target="ratio").fit(tri)

.. plot::
   :context: close-figs
   :caption: ED 예측의 A/E Error 히트맵(가린 3개 분기 대각선). 빨강은 모델이 과소 예측한 칸(실제 > 예측), 파랑은 과대 예측한 칸이다. 데이터가 가장 적은 어린 코호트(맨 위)에서 오차가 두드러진다.

   bt.plot_triangle()
```

색이 옅을수록 잘 맞은 것이다. 오차가 두드러지는 곳은 **데이터가 가장 적은
어린 코호트**(맨 윗줄) — 5장에서 본 "어릴수록 불확실하다"가 검증에서도
그대로 나타난다. 빨강(과소 예측)과 파랑(과대 예측)이 특정 경과에 쏠려 있지
않고 흩어져 있다면, 체계적 편향 없이 무작위 오차만 남았다는 좋은 신호다.

## 7.5 방법을 데이터로 고르기

백테스트의 진짜 쓸모는 "어느 방법이 좋은가"를 **취향이 아니라 숫자로** 가리는
데 있다. 같은 hold-out에서 세 방법의 평균 절대 A/E Error를 비교해 본다.

```python
for m in ("ed", "cl", "sa"):
    bt = lr.Backtest(lr.Ratio(method=m), holdout=3, target="ratio").fit(tri)
    err = float(bt.ae_err["ae_err"].abs().mean()) * 100
    print(f"{m:>3}: 평균 |A/E Error| = {err:.1f}%")
#> ed: 평균 |A/E Error| = 11.7%
#> cl: 평균 |A/E Error| = 4.5%
#> sa: 평균 |A/E Error| = 11.7%
```

```{list-table}
:header-rows: 1
:widths: 14 22 64

* - 방법
  - 평균 |A/E Error|
  - 
* - ED
  - 11.7%
  - 안전한 기본값이나, 발전한 칸에선 노출 앵커가 편향을 부른다
* - CL
  - 4.5%
  - 발전이 신뢰할 만한 구간을 3분기 내다보니 가장 정확
* - SA
  - 11.7%
  - 전환을 지정하지 않아 순수 ED로 동작 — 그래서 ED와 같다
```

흥미로운 반전이다. 4장에서 ED를 "안전한 기본값"이라 했지만, 이 검증에서는
CL이 ED보다 훨씬 정확하다. 가린 칸들이 이미 어느 정도 발전한 코호트의
3분기 앞이라, 진전 패턴이 신뢰할 만해 CL이 잘 맞기 때문이다. **보편적으로
최선인 방법은 없다** — 데이터의 성숙도와 내다보는 기간에 따라 달라지고,
그것을 가려 주는 것이 백테스트다.

SA가 ED와 같은 값을 낸 데 주목하자. `lr.Ratio(method="sa")`는 전환을
지정하지 않으면 순수 ED로 동작한다(4장). 전환을 명시하면 SA가 CL의 정확성을
끌어온다.

```python
# 전환을 경과 3에 고정 -> 후반 CL 구간이 정확도를 끌어올린다
bt = lr.Backtest(lr.Ratio(method="sa", switch=3), holdout=3, target="ratio").fit(tri)
float(bt.ae_err["ae_err"].abs().mean()) * 100
#> 4.4%   (ED의 11.7%보다 한참 낮다)
```

```{admonition} 전환 지점도 누출 없이 고를 수 있다
:class: tip

`switch=3`처럼 손으로 짚는 대신 `switch=lr.SwitchPoint.detect()`를 넘기면,
백테스트가 **매 fold마다** 가린 데이터를 빼고 전환 지점을 다시 고른다 —
가린 칸을 미리 엿보지 않으므로 누출이 없다(4.6절). 다만 SwitchPoint는
보수적이라, 분기 단위 단일 담보처럼 데이터가 얇으면 흔히 순수 ED로 비킨다.
백테스트의 usage 뷰에서 전환 경계를 겹쳐 보려면 `switch=` 인자를 쓴다 —
`tri.plot_triangle(kind="usage", holdout=3, switch=3)`.
```

```{admonition} hold-out 길이가 결론을 바꾼다
:class: important

`holdout=3`은 "3분기 앞"을 검증한다. 이 짧은 지평에서는 발전한 칸이 많아
CL이 유리하다. 그러나 `holdout`을 크게 잡아 *갓 인수된 코호트의 먼 미래*까지
예측하게 하면, 얇은 데이터에 외삽하는 CL이 흔들리고 노출에 닻을 내린 ED가
다시 안전해진다. 검증 결과는 늘 hold-out 길이와 함께 읽어야 한다.
```

같은 방식으로 regime 반영 여부(6장)도 백테스트로 견줄 수 있다 —
`lr.Backtest(lr.Ratio(method="ed", loss_regime=reg), ...)`처럼 추정기에
선택지를 담아 비교하면 된다.

## 7.6 자연스러운 한계

가장 오래된 코호트의 아주 깊은 경과 칸은 검증에서 빠진다. 대각선을 가린 뒤
다시 적합하면, 그 칸은 가린 삼각형의 예측 범위 밖에 놓여 비교할 짝이 없기
때문이다. `ae_err` 표는 이렇게 **비교 가능한(reachable) 칸만** 담는다 —
빠진 칸이 있다고 오류가 아니다.

## 7.7 마무리

이렇게 일곱 장을 지나왔다. 손해율이 왜 어려운지(1장)에서 출발해, 손해의
진전을 ATA 인자(2장)와 강도(3장)로 읽고, 그 재료로 빈 칸을 채우고(4장), 그
예측의 불확실성을 정량화하고(5장), 구조 변화를 가려내고(6장), 마지막으로 그
모든 선택을 데이터로 검증했다(7장).

핵심은 한 줄로 모인다 — **손해율은 하나의 숫자가 아니라, 경과에 따라
발전하는 과정이며, 그 과정을 읽고·예측하고·의심하고·검증하는 것이 분석**
이다. lossratio는 그 네 가지를 한 흐름으로 잇는 도구다.

## 7.8 함께 보기

- {doc}`튜토리얼 차례 <index>`: 일곱 장 전체를 되짚어 본다.
- {doc}`API 레퍼런스 <../api>`의 `Backtest`, `BacktestFit`.
