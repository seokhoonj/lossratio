# 7장. 예측 검증

4장은 예측의 불확실성을 *공식*으로 추정했다. 그런데 그 추정이 실전에서
정말 맞을까? 그리고 2장의 여러 방법 중 *내 데이터*에는 어느 것이 맞을까?
이런 물음에 답하는 가장 정직한 길은 **과거로 돌아가 직접 맞춰 보는** 것이다.
그것이 백테스트다.

## 7.1 백테스트 — 달력 대각선 hold-out

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

가린 칸마다 **실제값**(actual)과 **예측값**(expected)을 비교한다. 차이를 측정하는
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

둘은 대체가 아니라 보완이다 — 절대 금액이 궁금하면 **AEG**(Actual-Expected
Gap, 실제 − 예측), 크기를 떼고 비율로 비교하려면 **A/E Error**(Actual / Expected
− 1, 실제를 예측으로 나눠 1을 뺀 값)를 본다. 4장의 SE(절대)와 CV(상대) 관계와
같은 짝이다.

한 칸을 직접 짚어 두 지표를 같은 자리에 놓아 보자. 손해액(`target="loss"`)을
검증해, 2024년 1분기 코호트가 8경과에 도달한 칸 하나를 꺼낸다(`Backtest`
호출법은 7.3에서 자세히 다룬다 — 여기서는 한 칸의 두 지표만 본다).

```python
import polars as pl
import lossratio as lr
from datetime import date

df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df, groups="coverage", grain="Q")

est = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium())
bt = lr.Backtest(estimator=est, holdouts=3, target="loss").fit(tri)
bt.ae_err.filter(
    (pl.col("cohort") == date(2024, 1, 1)) & (pl.col("duration") == 8)
).select(["cohort", "duration", "actual", "expected", "aeg", "ae_err"])
#> shape: (1, 6)
#> ┌────────────┬──────────┬──────────┬──────────┬───────────┬───────────┐
#> │ cohort     ┆ duration ┆ actual   ┆ expected ┆ aeg       ┆ ae_err    │
#> │ ---        ┆ ---      ┆ ---      ┆ ---      ┆ ---       ┆ ---       │
#> │ date       ┆ i64      ┆ f64      ┆ f64      ┆ f64       ┆ f64       │
#> ╞════════════╪══════════╪══════════╪══════════╪═══════════╪═══════════╡
#> │ 2024-01-01 ┆ 8        ┆ 2.8976e9 ┆ 2.9110e9 ┆ -1.3348e7 ┆ -0.004585 │
#> └────────────┴──────────┴──────────┴──────────┴───────────┴───────────┘
```

이 칸에서 실제 누적 손해는 2,897,639,271원, 가린 데이터 없이 예측한 값은
2,910,986,939원이다. 같은 칸을 두 지표가 이렇게 다르게 요약한다.

- **AEG** = 2,897,639,271 − 2,910,986,939 = **−13,347,668원**. 모델이 약
  1,335만원 과대 예측했다는 *절대 금액*이다.
- **A/E Error** = 2,897,639,271 / 2,910,986,939 − 1 = **−0.46%**. 같은 격차를
  예측값 대비 *비율*로 떼어 낸 값이다.

약 29억원짜리 칸에서 1,335만원 격차는 −0.46%에 지나지 않는다 — 절대 금액으로는
큰 숫자가 상대 오차로는 작다. 그래서 원 단위 영향이 궁금하면 AEG를, 큰 칸과
작은 칸을 한 표에서 견주려면 A/E Error를 본다.

## 7.3 백테스트 실행

`Backtest`는 예측에 쓸 **추정기**와 가릴 대각선 수(`holdouts`)를 받는다.
무엇을 검증할지는 `target`으로 고른다(`"ratio"` / `"loss"` / `"premium"`).
손해율 추정기는 손해쪽 사다리(여기서는 `PooledLoss` — 완전 풀링)와
분모쪽 `PooledPremium`을 묶은 `Ratio`로 만든다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df, groups="coverage", grain="Q")

est = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium())
bt = lr.Backtest(estimator=est, holdouts=3, target="ratio").fit(tri)
```

`holdouts`에 정수 하나를 주면 as-of 시점 하나만 보는 단일 시점 백테스트다
(7.6에서는 여러 깊이를 한 번에 돌린다). 결과에는 가린 칸별 오차(`ae_err`)와,
그것을 경과별로 모은 `col_summary`, 달력 대각선별로 모은 `diag_summary`가
들어 있다.

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

   df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
   tri = lr.Triangle(df, groups="coverage", grain="Q")
   est = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium())
   bt = lr.Backtest(estimator=est, holdouts=3, target="ratio").fit(tri)

.. plot::
   :context: close-figs
   :caption: PooledLoss 예측의 A/E Error 히트맵(가린 3개 분기 대각선). 빨강은 모델이 과소 예측한 칸(실제 > 예측), 파랑은 과대 예측한 칸이다. 데이터가 가장 적은 최근 코호트(맨 위)에서 오차가 두드러진다.

   bt.plot_triangle()
```

색이 옅을수록 잘 맞은 것이다. 오차가 두드러지는 곳은 **데이터가 가장 적은
최근 코호트**(맨 윗줄) — 4장에서 본 "어릴수록 불확실하다"가 검증에서도
그대로 나타난다. 빨강(과소 예측)과 파랑(과대 예측)이 특정 경과에 쏠려 있지
않고 흩어져 있다면, 체계적 편향 없이 무작위 오차만 남았다는 좋은 신호다.

## 7.5 방법을 데이터로 고르기

백테스트의 진짜 쓸모는 "어느 방법이 좋은가"를 **취향이 아니라 숫자로** 가리는
데 있다. 같은 hold-out에서 세 방법의 평균 절대 A/E Error를 비교해 본다.

```python
losses = {"pooled": lr.PooledLoss, "linkratio": lr.ChainLadder, "credible": lr.CredibleLoss}
for name, loss in losses.items():
    est = lr.Ratio(loss=loss(), premium=lr.PooledPremium())
    bt = lr.Backtest(estimator=est, holdouts=3, target="ratio").fit(tri)
    err = float(bt.ae_err["ae_err"].abs().mean()) * 100
    print(f"{name:>9}: 평균 |A/E Error| = {err:.1f}%")
#>    pooled: 평균 |A/E Error| = 11.7%
#> linkratio: 평균 |A/E Error| = 4.5%
#>  credible: 평균 |A/E Error| = 11.4%
```

```{list-table}
:header-rows: 1
:widths: 14 22 64

* - 방법
  - 평균 |A/E Error|
  - 
* - PooledLoss
  - 11.7%
  - 안전한 기본값이나, 경과한 칸에선 노출 앵커가 편향을 부른다
* - ChainLadder
  - 4.5%
  - 진전이 신뢰할 만한 구간을 3분기 내다보니 이 hold-out에선 가장 정확
* - CredibleLoss
  - 11.4%
  - 코호트 수준 보정이 풀링 형상의 편향을 부분적으로 다룬다 — 이 담보에선 소폭
```

흥미로운 반전이다. 2장에서 `PooledLoss`를 "안전한 기본값"이라 했지만, 이
검증에서는 `ChainLadder`가 `PooledLoss`보다 훨씬 정확하다. 가린 칸들이 이미
어느 정도 경과한 코호트의 3분기 앞이라, 진전 패턴이 신뢰할 만해 `ChainLadder`가
잘 맞기 때문이다. **보편적으로
최선인 방법은 없다** — 코호트가 얼마나 경과했는지와 내다보는 기간에 따라
달라지고, 그것을 가려 주는 것이 백테스트다.

```{admonition} hold-out 길이가 결론을 바꾼다
:class: important

`holdouts=3`은 "3분기 앞"을 검증한다. 이 짧은 지평에서는 경과한 칸이 많아
`ChainLadder`가 유리하다. 그러나 `holdouts`를 크게 잡아 *갓 인수된 코호트의 먼
미래*까지 예측하게 하면, 얇은 데이터에 외삽하는 `ChainLadder`가 흔들리고 노출에
고정한 `PooledLoss`가 다시 안전해진다. 검증 결과는 늘 hold-out 길이와 함께 읽어야 한다.
```

같은 방식으로 regime 반영 여부(5장)도 백테스트로 비교할 수 있다 —
`lr.Backtest(estimator=lr.Ratio(loss=lr.PooledLoss(regime=date(2024, 7, 1)),
premium=lr.PooledPremium()), ...)`처럼 추정기에 선택지를 담아 비교하면 된다.

이 손계산에는 두 가지 빈틈이 있다. 첫째, **지표가 하나뿐**이다 — 평균
|A/E Error|만 보면 편향의 방향도, 예측 구간이 정직한지도 놓친다. 둘째, 더
미묘하게, **방법마다 도달하는 held-out 칸이 다를 수 있어** 서로 다른 칸 집합
위의 평균을 견주는 셈이 된다(사과와 오렌지). 두 빈틈은 뒤(7.7~7.8)에서
`EstimatorComparison`이 한꺼번에 메운다.

## 7.6 여러 시점에서 검증하기 — rolling-origin

7.3의 `Backtest`는 as-of 시점 하나(hold-out 깊이 하나)를 보았다. 그런데
"이 예측을 **몇 기간 앞까지** 믿어도 되나", "코호트 이력이 **얼마나
쌓여야** 예측이 안정되나" 같은 물음에는 시점 하나로 답하기 어렵다. 같은
`Backtest`에 `holdouts`로 여러 깊이를 한 번에 주면 시점을 옮겨 가며
반복하는 **rolling-origin** 백테스트가 되고, 가린 칸마다 두 좌표가 붙는다.

- `horizon` — 그 칸이 as-of 시점에서 몇 기간 *앞*인가 (내다본 거리)
- `anchor_duration` — 그 as-of 시점에 코호트가 몇 경과까지 관측돼
  있었나 (예측이 딛고 선 이력의 길이)

horizon별로 모은 `horizon_summary`가 이 패키지의 **신뢰도 곡선**이다 —
멀리 내다볼수록 오차가 어떻게 커지는지를 보여 준다.

```python
est = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium())
bt = lr.Backtest(estimator=est, holdouts=(2, 4, 6), target="ratio").fit(tri)
bt.horizon_summary.select(["coverage", "horizon", "n", "ae_err_wt", "abs_err_mean"]).head(4)
#> shape: (4, 5)
#> ┌──────────┬─────────┬─────┬───────────┬──────────────┐
#> │ coverage ┆ horizon ┆ n   ┆ ae_err_wt ┆ abs_err_mean │
#> │ ---      ┆ ---     ┆ --- ┆ ---       ┆ ---          │
#> │ str      ┆ i64     ┆ u32 ┆ f64       ┆ f64          │
#> ╞══════════╪═════════╪═════╪═══════════╪══════════════╡
#> │ SURGERY  ┆ 1       ┆ 21  ┆ -0.046662 ┆ 0.067953     │
#> │ SURGERY  ┆ 2       ┆ 18  ┆ -0.077085 ┆ 0.109897     │
#> │ SURGERY  ┆ 3       ┆ 8   ┆ -0.081962 ┆ 0.120106     │
#> │ SURGERY  ┆ 4       ┆ 6   ┆ -0.117574 ┆ 0.166891     │
#> └──────────┴─────────┴─────┴───────────┴──────────────┘
```

`ae_err_wt`가 마법이 아님은 horizon 하나에서 직접 확인된다. horizon=1 칸만 모아
격차의 합을 예측값의 합으로 나누면 위 표의 첫 행이 그대로 나온다.

```python
h1 = bt.ae_err.filter(pl.col("horizon") == 1)
print("n            =", h1.height)
print("sum(aeg)     =", round(h1["aeg"].sum(), 6))
print("sum(expected)=", round(h1["expected"].sum(), 6))
print("ae_err_wt    =", round(h1["aeg"].sum() / h1["expected"].sum(), 6))
#> n            = 21
#> sum(aeg)     = -1.300774
#> sum(expected)= 27.876427
#> ae_err_wt    = -0.046662
```

즉 `ae_err_wt`는 칸별 비율을 단순 평균한 것이 아니라 **예측값으로 가중한 풀링
편향**이다 — 큰 칸이 더 큰 목소리를 낸다. horizon=1 칸 21개의 격차를 합쳐
−1.30, 예측값 합 27.88로 나눠 −4.67%다.

`is_single_origin`이 `False`가 되면 `col_summary` / `diag_summary` /
`plot_triangle`처럼 시점 하나를 전제로 한 단일 시점 뷰는 막힌다(여러
깊이에 걸쳐 칸을 이중 집계하기 때문). 그 칸별 그림이 필요하면
`bt.fits[h]`로 깊이를 하나 골라 본다.

두 reader가 두 축의 물음에 바로 답한다. 둘 다 기본 허용 범위는 빡빡하게
`tol=0.03`(편향 3%)이다 — 누적 손해율의 분모는 경과에 따라 계속 자라므로 누적
곡선이 평평해 *보이는* 것은 관성(inertia, 분모가 자라며 신호를 감쇠시키는
현상)의 착시일 뿐이고, 믿을 근거는 표본 외 편향이기 때문이다.

```python
bt.reliable_horizon(tol=0.10)        # 몇 기간 앞까지 |풀링 편향| <= 10%인가
#> shape: (1, 3)
#> ┌──────────┬──────────────────┬─────────────┐
#> │ coverage ┆ reliable_horizon ┆ max_horizon │
#> ╞══════════╪══════════════════╪═════════════╡
#> │ SURGERY  ┆ 3                ┆ 5           │
#> └──────────┴──────────────────┴─────────────┘
```

`reliable_horizon`은 가장 가까운 horizon부터 걸어가다 편향이 처음 허용 범위를
벗어나는 곳에서 멈춘다 — `tol=0.10`이면 3분기 앞까지다. 기본값 `tol=0.03`에서는
horizon=1 편향이 이미 −4.67%(위에서 직접 재현했다)라 한 기간 앞도 3% 안에 들지
못해 0을 답한다.

이력 축은 `anchor_summary`가 근거를 그대로 보여 준다.

```python
bt.anchor_summary.select(["coverage", "anchor_duration", "n", "ae_err_wt", "abs_err_mean"])
#> shape: (9, 5)
#> ┌──────────┬─────────────────┬─────┬───────────┬──────────────┐
#> │ coverage ┆ anchor_duration ┆ n   ┆ ae_err_wt ┆ abs_err_mean │
#> ╞══════════╪═════════════════╪═════╪═══════════╪══════════════╡
#> │ SURGERY  ┆ 1               ┆ 11  ┆ -0.169081 ┆ 0.223111     │
#> │ SURGERY  ┆ 2               ┆ 10  ┆ -0.155243 ┆ 0.195684     │
#> │ SURGERY  ┆ 3               ┆ 9   ┆ -0.032846 ┆ 0.047932     │
#> │ SURGERY  ┆ 4               ┆ 8   ┆ -0.035368 ┆ 0.050083     │
#> │ SURGERY  ┆ 5               ┆ 6   ┆ 0.000543  ┆ 0.005407     │
#> │ SURGERY  ┆ 6               ┆ 4   ┆ 0.001107  ┆ 0.012868     │
#> │ SURGERY  ┆ 7               ┆ 3   ┆ 0.006006  ┆ 0.008968     │
#> │ SURGERY  ┆ 8               ┆ 2   ┆ 0.000837  ┆ 0.002512     │
#> │ SURGERY  ┆ 9               ┆ 1   ┆ -0.008474 ┆ 0.012791     │
#> └──────────┴─────────────────┴─────┴───────────┴──────────────┘
```

`anchor_duration`이 작은(어린 코호트) 칸은 편향이 −15~−17%로 크다가, 5경과부터
±1% 안으로 잦아든다. `convergence`는 바로 이 anchor 축을 걸어 "이 경과 이력부터는
끝까지 편향이 허용 범위 안"인 첫 경과를 찾는다.

```python
bt.convergence(tol=0.03, min_run=3)  # 몇 경과 이력부터 표본 외 편향이 가라앉나
#> shape: (1, 3)
#> ┌──────────┬──────────────┬────────────┐
#> │ coverage ┆ converged_at ┆ max_anchor │
#> ╞══════════╪══════════════╪════════════╡
#> │ SURGERY  ┆ 5            ┆ 9          │
#> └──────────┴──────────────┴────────────┘
```

위 표에서 5·6·7·8·9경과의 편향은 각각 0.0005, 0.0011, 0.0060, 0.0008, −0.0085로
모두 3% 안이다 — in-band 구간이 5개 경과를 지속한다. 그래서 `min_run=3`이면
`converged_at`은 5다. 그런데 기본값 `min_run=6`으로 돌리면:

```python
bt.convergence()                     # 기본 tol=0.03, min_run=6
#> shape: (1, 3)
#> ┌──────────┬──────────────┬────────────┐
#> │ coverage ┆ converged_at ┆ max_anchor │
#> ╞══════════╪══════════════╪════════════╡
#> │ SURGERY  ┆ null         ┆ 9          │
#> └──────────┴──────────────┴────────────┘
```

`null`이다 — in-band 구간이 5개뿐이라 6의 문턱을 넘지 못한다. `min_run`은 그
in-band 구간이 최소 몇 개 경과를 지속해야 인정할지의 보호 장치다. 데이터
가장자리의 얇은 칸 몇 개(여기서 9경과는 `n`이 1뿐이다)가 우연히 범위 안에 들어와
잘못 판정하는 것을 막는다. 이 작은 분기 삼각형에서는 근거가 모자라 `null`(보류)로
답하는데, 8장의 안정성과 같은 **정직한 보류**다.

## 7.7 여러 방법을 같은 잣대로 — EstimatorComparison

`EstimatorComparison`은 **같은 삼각형·같은 hold-out**에서 여러 추정기의
백테스트를 한꺼번에 돌린다. label과 추정기의 딕셔너리, 공유 `holdouts`, 비교
기준이 되는 `baseline`(기본값은 첫 번째 label)을 받는다. 7.5의 손계산과 같은
단일 시점(`holdouts=(3,)`)에 세 방법을 얹어 보자.

```python
cmp = lr.EstimatorComparison(
    {
        "pooled":    lr.Ratio(loss=lr.PooledLoss(),   premium=lr.PooledPremium()),
        "linkratio": lr.Ratio(loss=lr.ChainLadder(),  premium=lr.PooledPremium()),
        "credible":  lr.Ratio(loss=lr.CredibleLoss(), premium=lr.PooledPremium()),
    },
    holdouts=(3,),
).fit(tri)
```

가장 먼저 볼 것은 `scorecard()` — 각 방법의 검증 성적을 한 표로 쌓는, 직접 읽는
점수표다.

```python
sc = cmp.scorecard()
sc.filter((pl.col("population") == "all") & (pl.col("lane") == "cumulative")).select(
    ["estimator", "n", "bias", "mae", "rmse", "coverage_80", "coverage_95"]
)
#> shape: (3, 7)
#> ┌───────────┬─────┬───────────┬──────────┬──────────┬─────────────┬─────────────┐
#> │ estimator ┆ n   ┆ bias      ┆ mae      ┆ rmse     ┆ coverage_80 ┆ coverage_95 │
#> ╞═══════════╪═════╪═══════════╪══════════╪══════════╪═════════════╪═════════════╡
#> │ pooled    ┆ 21  ┆ -0.115242 ┆ 0.117269 ┆ 0.178675 ┆ 0.428571    ┆ 0.571429    │
#> │ linkratio ┆ 21  ┆ 0.039402  ┆ 0.04524  ┆ 0.106018 ┆ 0.952381    ┆ 0.952381    │
#> │ credible  ┆ 21  ┆ -0.111574 ┆ 0.113686 ┆ 0.173219 ┆ null        ┆ null        │
#> └───────────┴─────┴───────────┴──────────┴──────────┴─────────────┴─────────────┘
```

한 표에 결정에 필요한 세 갈래가 담긴다.

- **편향**: `bias`(부호 있는 평균 A/E Error). `pooled`/`credible`는 −0.115/
  −0.112로 과소 예측, `linkratio`는 +0.039로 살짝 과대 예측이다.
- **분산**: `mae`(평균 |A/E Error|)와 `rmse`. `linkratio`가 0.045로 가장 작다
  (증분 lane에서는 `deviance`도 함께 산출되나 누적 lane에선 `null`이다).
- **구간 정직성**: `coverage_80`/`coverage_95`(예측 구간이 실제값을 덮은 비율).
  명목 80% 구간을 `pooled`는 43%만 덮어 과신하고, `linkratio`는 95%로 넉넉하다.
  `credible`은 점추정 전용이라 분석적 SE가 없어 `null`이다(4장).

세 방법이 *서로 다른 칸*에 닿으면 위 평균은 공정하지 않다.
`EstimatorComparison`의 baseline-상대 비교 프레임(`horizon_comparison` 등)과
뒤의 crossover는 **모든 방법이 채점한 매칭 칸만**으로 계산하고, 그 손실을
`match_summary`가 기록한다.

```python
cmp.match_summary
#> shape: (3, 6)
#> ┌──────────┬───────────┬─────────┬─────────┬───────────┬───────────┐
#> │ coverage ┆ estimator ┆ holdout ┆ n_cells ┆ n_matched ┆ n_dropped │
#> ╞══════════╪═══════════╪═════════╪═════════╪═══════════╪═══════════╡
#> │ SURGERY  ┆ pooled    ┆ 3       ┆ 21      ┆ 21        ┆ 0         │
#> │ SURGERY  ┆ linkratio ┆ 3       ┆ 21      ┆ 21        ┆ 0         │
#> │ SURGERY  ┆ credible  ┆ 3       ┆ 21      ┆ 21        ┆ 0         │
#> └──────────┴───────────┴─────────┴─────────┴───────────┴───────────┘
```

이 수술담보에서는 세 방법 모두 같은 21개 칸에 닿아 `n_dropped`가 모두 0이다 —
매칭이 아무것도 떨어뜨리지 않은 깨끗한 경우다. 재적합이 관측 앵커를 더 요구하는
방법이 섞이면 `n_dropped`가 0보다 커지고, 그때 이 열이 어느 비교가 사과-오렌지
인지를 드러낸다.

세 reader(`scorecard`/`rank`/`best`)는 두 축을 공유한다. `lane`은
`"cumulative"`(기본)·`"incremental"`(기간별)·`"anchored"`(앵커 이후 발생분) 중
어느 척도로 채점할지를, `population`은 전체(`"all"`)인지 **결정 구간**
(`"terminal"`, 예측이 실제로 딛고 가는 최근 경과 몇 개)인지를 고른다.
`population="terminal"`은 `terminal=` 정수와 함께 쓴다.

```python
cmp.rank("mae", population="terminal", terminal=3)
#> shape: (3, 6)
#> ┌──────────┬─────────┬───────────┬─────┬──────────┬──────┐
#> │ coverage ┆ holdout ┆ estimator ┆ n   ┆ mae      ┆ rank │
#> ╞══════════╪═════════╪═══════════╪═════╪══════════╪══════╡
#> │ SURGERY  ┆ 3       ┆ linkratio ┆ 9   ┆ 0.005676 ┆ 1    │
#> │ SURGERY  ┆ 3       ┆ pooled    ┆ 9   ┆ 0.005717 ┆ 2    │
#> │ SURGERY  ┆ 3       ┆ credible  ┆ 9   ┆ 0.007072 ┆ 3    │
#> └──────────┴─────────┴───────────┴─────┴──────────┴──────┘
```

전체로 보면 `linkratio`가 `pooled`를 크게 앞섰지만(mae 0.045 대 0.117), 결정
구간(최근 3경과)만 떼어 보면 셋이 0.0057~0.0071로 사실상 붙는다 — 이미 충분히
경과한 칸에서는 어느 방법이든 비슷하게 맞는다는 뜻이다.

## 7.8 줄세우기와 기계적 선택 — rank와 best

`rank()`는 **지표 하나**로 방법을 줄 세운다(낮을수록 좋음; `bias`와
`coverage_*`는 각각 0과 명목값으로부터의 거리로 환산한다).

```python
cmp.rank("mae")
#> shape: (3, 6)
#> ┌──────────┬─────────┬───────────┬─────┬──────────┬──────┐
#> │ coverage ┆ holdout ┆ estimator ┆ n   ┆ mae      ┆ rank │
#> ╞══════════╪═════════╪═══════════╪═════╪══════════╪══════╡
#> │ SURGERY  ┆ 3       ┆ linkratio ┆ 21  ┆ 0.04524  ┆ 1    │
#> │ SURGERY  ┆ 3       ┆ credible  ┆ 21  ┆ 0.113686 ┆ 2    │
#> │ SURGERY  ┆ 3       ┆ pooled    ┆ 21  ┆ 0.117269 ┆ 3    │
#> └──────────┴─────────┴───────────┴─────┴──────────┴──────┘

cmp.rank("bias")
#> shape: (3, 6)
#> ┌──────────┬─────────┬───────────┬─────┬───────────┬──────┐
#> │ coverage ┆ holdout ┆ estimator ┆ n   ┆ bias      ┆ rank │
#> ╞══════════╪═════════╪═══════════╪═════╪═══════════╪══════╡
#> │ SURGERY  ┆ 3       ┆ linkratio ┆ 21  ┆ 0.039402  ┆ 1    │
#> │ SURGERY  ┆ 3       ┆ credible  ┆ 21  ┆ -0.111574 ┆ 2    │
#> │ SURGERY  ┆ 3       ┆ pooled    ┆ 21  ┆ -0.115242 ┆ 3    │
#> └──────────┴─────────┴───────────┴─────┴───────────┴──────┘

cmp.rank("coverage_80")
#> shape: (3, 6)
#> ┌──────────┬─────────┬───────────┬─────┬─────────────┬──────┐
#> │ coverage ┆ holdout ┆ estimator ┆ n   ┆ coverage_80 ┆ rank │
#> ╞══════════╪═════════╪═══════════╪═════╪═════════════╪══════╡
#> │ SURGERY  ┆ 3       ┆ credible  ┆ 21  ┆ null        ┆ null │
#> │ SURGERY  ┆ 3       ┆ linkratio ┆ 21  ┆ 0.952381    ┆ 1    │
#> │ SURGERY  ┆ 3       ┆ pooled    ┆ 21  ┆ 0.428571    ┆ 2    │
#> └──────────┴─────────┴───────────┴─────┴─────────────┴──────┘
```

`mae`로도 `bias`로도 `linkratio`가 1등이지만, `coverage_80`에서는 점추정인
`credible`이 `null`로 빠지고 `linkratio`가 1등이다(`pooled`의 좁은 구간은 2등).
**지표를 바꿀 때 순위가 흔들린다면 그것이 진짜 trade-off다.**

여러 지표를 한꺼번에 종합하려면 `best()`다. 7.5에서 손으로 돌린
`ae_err.abs().mean()` 루프를 이것이 대체한다 — 지표마다 순위를 매겨 그 합(Borda
count, 순위를 합산하는 투표법)이 가장 낮은 방법을 고른다. 기본 지표는 편향·
정확도·구간 정직성 세 가지(`("bias", "mae", "coverage_80")`)다.

```python
cmp.best()
#> UserWarning: dropped ['coverage_80'] from the Borda panel (not a scorecard
#> column, or null for some estimators -- e.g. coverage on point-only fits);
#> ranking on ['bias', 'mae'].
#> shape: (3, 6)
#> ┌──────────┬─────────┬───────────┬───────────┬──────────┬──────────┐
#> │ coverage ┆ holdout ┆ estimator ┆ bias_rank ┆ mae_rank ┆ rank_sum │
#> ╞══════════╪═════════╪═══════════╪═══════════╪══════════╪══════════╡
#> │ SURGERY  ┆ 3       ┆ linkratio ┆ 1.0       ┆ 1.0      ┆ 2.0      │
#> │ SURGERY  ┆ 3       ┆ credible  ┆ 2.0       ┆ 2.0      ┆ 4.0      │
#> │ SURGERY  ┆ 3       ┆ pooled    ┆ 3.0       ┆ 3.0      ┆ 6.0      │
#> └──────────┴─────────┴───────────┴───────────┴──────────┴──────────┘
```

`credible`이 점추정이라 `coverage_80`을 채우지 못하므로, `best()`는 그 지표를
패널에서 빼고(경고로 알린다) `bias`와 `mae`만으로 줄 세운다 — 한 방법만 가진
지표로는 나머지를 공정히 매길 수 없기 때문이다. `rank_sum`이 가장 낮은
`linkratio`가 승자다. 7.5의 손계산이 "linkratio가 평균 오차 최소"라는 한 가지
사실만 주었다면, `best()`는 **같은 칸 위에서, 여러 지표를 함께, 감사 가능한 순위
분해(`*_rank` 열)와 함께** 그 선택을 내린다 — 7.5가 경고한 사과-오렌지의 긴장이
여기서 풀린다. 모든 추정기에 같은 불확실성 원천을 주면
(`uncertainty=ResidualBootstrap(...)`) `coverage_80`도 패널에 살아남아 세 지표
전부로 고를 수 있다.

## 7.9 horizon을 따라 승자가 바뀌는가 — crossover

`scorecard`/`best`가 *한 시점*에서 방법을 가렸다면, `crossover()`는 여러
시점(rolling)에 걸쳐 horizon 축을 따라 "이기는 방법이 바뀌는 지점"을 찾는다.
승자는 `baseline`(아래에서는 첫 label `pooled`) 대비로 판정한다.

```python
cmp = lr.EstimatorComparison(
    {
        "pooled":    lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()),
        "linkratio": lr.Ratio(loss=lr.ChainLadder(),  premium=lr.PooledPremium()),
    },
    holdouts=(2, 4, 6),
).fit(tri)

cmp.crossover(min_run=3)
#> shape: (1, 8)
#> ┌──────────┬───────────┬──────────────┬──────────────┬─────────────┬────────────────┬─────────┬─────────────┐
#> │ coverage ┆ estimator ┆ crossover_at ┆ early_winner ┆ late_winner ┆ overall_winner ┆ n_flips ┆ max_horizon │
#> ╞══════════╪═══════════╪══════════════╪══════════════╪═════════════╪════════════════╪═════════╪═════════════╡
#> │ SURGERY  ┆ linkratio ┆ null         ┆ linkratio    ┆ null        ┆ linkratio      ┆ 1       ┆ 5           │
#> └──────────┴───────────┴──────────────┴──────────────┴─────────────┴────────────────┴─────────┴─────────────┘
```

이 예에서 `crossover_at`은 `null` — 관측 범위 안에 *지속되는* 역전이 없다는
뜻이다(가장 깊은 horizon 한 칸에서 순위가 뒤집히긴 했지만, 셀이 한 줌뿐인 단발
역전이라 `min_run`이 잡음으로 거른다). **"crossover 없음 + `overall_winner`"가
가장 정직한 결과다** — 한 방법이 관측 범위 전체에서 우세하고, 다른 방법은 표본
외에서 보태는 것이 없다는 읽기다. 이 합성 수술담보의 짧은 hold-out에서는
`ChainLadder`가 그 승자인데, 위의 "hold-out 길이가 결론을 바꾼다"가 여기에도
그대로 적용된다 — 결론은 늘 *내 데이터, 내 hold-out*에서 다시 가린다.

## 7.10 자연스러운 한계

가장 오래된 코호트의 아주 깊은 경과 칸은 검증에서 빠진다. 대각선을 가린 뒤
다시 적합하면, 그 칸은 가린 삼각형의 예측 범위 밖에 놓여 비교할 짝이 없기
때문이다. `ae_err` 표는 이렇게 **비교 가능한(reachable) 칸만** 담는다 —
빠진 칸이 있다고 오류가 아니다.

그리고 더 근본적으로, 백테스트는 **관측된 대각선 안**에서만 예측을 검증한다 —
아직 아무도 보지 못한 깊은 경과의 미래 칸은 가릴 actual 자체가 없으므로 어떤
방법도 거기서는 채점되지 않는다. 그러니 위의 모든 방법 선택은 *관측 범위
안에서의* 결론이며, 관측 너머 먼 경과까지 그대로 외삽해 주는 보증은 아니다.

## 7.11 마무리

이렇게 일곱 장을 지나왔다. 손해율이 왜 어려운지(1장)에서 출발해, 그
재료로 빈 칸을 채우고(2장), 코호트 수준 보정으로 형상을 다듬고(3장), 그
예측의 불확실성을 정량화하고(4장), 구조 변화를 가려내고(5장), 셀 수준
공변량으로 나눠 보고(6장), 마지막으로 그 모든 선택을 데이터로 검증했다(7장).
손해의 진전을 ATA 인자와 강도로 읽는 법은 부록에 두었다.

핵심은 한 줄로 모인다 — **손해율은 하나의 숫자가 아니라, 경과에 따라
움직이는 과정이며, 그 과정을 읽고·예측하고·의심하고·검증하는 것이 분석**
이다. lossratio는 그 네 가지를 한 흐름으로 잇는 도구다.

## 7.12 함께 보기

- {doc}`튜토리얼 차례 <index>`: 일곱 장 전체를 되짚어 본다.
- {doc}`API 레퍼런스 <../api>`의 `Backtest`, `BacktestFit`,
  `EstimatorComparison`, `EstimatorComparisonFit`.
