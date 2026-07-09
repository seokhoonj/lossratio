# 시작하기

이 문서는 설치부터 손해율 예측·검증까지 전체 흐름을 한 번에 훑어보는
것을 목표로 합니다. 각 단계가 왜 필요하고 결과를 어떻게 읽는지는
{doc}`튜토리얼 <tutorial/index>`에서 차근차근 다룹니다.

## 설치

```{warning}
**개발 중(WIP)** — 이론적 토대를 대폭 개정하는 중이라 API와 산출값이 바뀔 수
있습니다. PyPI 버전은 이미 폐기된 옛 방법론이므로, 최신은 아래처럼 GitHub에서
설치하세요.
```

```bash
pip install "git+https://github.com/seokhoonj/lossratio.git"                       # polars only
pip install "lossratio[pandas] @ git+https://github.com/seokhoonj/lossratio.git"   # add pandas / pyarrow support
```

## 데이터 불러오기

lossratio에는 시연용 합성 경험 데이터가 들어 있습니다. 네 개의
담보(CI / CANCER / INPATIENT / SURGERY)에 대해 인수월 코호트별로 손해와 보험료가
경과월(`duration_m`)별로 기록된 long-format 표입니다. 이 가운데
수술담보에는 2024년 7월에 한 번의 구조 변화(regime)가 심어져 있어,
여기서는 수술담보에 집중합니다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience()
df.select(["coverage", "uy_m", "cy_m", "duration_m", "incr_loss", "incr_premium"]).head(3)
#> shape: (3, 6)
#> ┌──────────┬────────────┬────────────┬────────────┬───────────┬──────────────┐
#> │ coverage ┆ uy_m       ┆ cy_m       ┆ duration_m ┆ incr_loss ┆ incr_premium │
#> │ ---      ┆ ---        ┆ ---        ┆ ---        ┆ ---       ┆ ---          │
#> │ str      ┆ date       ┆ date       ┆ i64        ┆ i64       ┆ i64          │
#> ╞══════════╪════════════╪════════════╪════════════╪═══════════╪══════════════╡
#> │ CI       ┆ 2023-01-01 ┆ 2023-01-01 ┆ 1          ┆ 1418956   ┆ 1356200      │
#> │ CI       ┆ 2023-01-01 ┆ 2023-02-01 ┆ 2          ┆ 73602     ┆ 1274360      │
#> │ CI       ┆ 2023-01-01 ┆ 2023-03-01 ┆ 3          ┆ 420092    ┆ 1384735      │
#> └──────────┴────────────┴────────────┴────────────┴───────────┴──────────────┘
```

## 삼각형 만들기

`Triangle`은 long-format 경험 데이터를 받아 스키마를 검증하고 코호트 x
경과 기간 삼각형으로 집계합니다. 누적값(`loss`, `premium`, `ratio`)이
기본이고, 기간별 증분값은 `incr_` 접두사를 붙입니다.

```python
df_sur = df.filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df_sur, groups="coverage")
```

## 손해율 예측하기

손해율 예측은 분자(손해)와 분모(보험료)를 각각 예측해 합성합니다.
손해쪽은 사다리(ladder) 형태의 추정기를 고르고, 분모쪽은
`PooledPremium`을 쓴 뒤, `Ratio`가 둘을 묶어
`ratio_proj = loss_proj / premium_proj`를 셀 단위로 계산합니다.

손해쪽 사다리는 아래 네 가지입니다.

- `PooledLoss` — 완전 풀링(complete pooling): 모든 코호트가 하나의 공통
  경과별 진전 형상을 공유한다고 보고, 위험보험료를 기준으로 한
  가법 예측입니다. 갓 인수되어 관측 기간이 짧은
  코호트에서도 별도의 사전 판단 없이 안전하게 작동하는 기본 기준선이라
  기본으로 채택했습니다.
- `CredibleLoss` — `PooledLoss`의 풀링 형상은 그대로 두고, 각 코호트의
  수준만 신뢰도(credibility) 가중으로 보정합니다(부분 풀링).
- `SmoothLoss` — `CredibleLoss`의 형상을 매끄러운(penalized P-spline)
  곡선으로 적합합니다.
- `ChainLadder` — 손해 자체의 곱셈 link ratio(자기손해 링크비)를 쓰는
  고전적 참조 비교군입니다.

분모쪽 보험료는 위험보험료(요율 x 보유계약)로 이미 **알려진 노출**입니다 —
손해처럼 추정해야 할 확률 과정이 아니라 장부에 할당된 값이라, 보험료 사다리
(`PooledPremium` / `CrediblePremium` / `SmoothPremium`)는 모두 점추정만
산출하고 표준오차·신뢰구간 컬럼은 항상 `null`입니다. 셋 다 쓸 수 있지만
기본은 `PooledPremium`이며, 보험료에 별도 모델이 굳이 필요하지 않은 보통의
경우에 적합합니다.

손해율 밴드는 이렇게 분모를 알려진 노출로 두고 손해쪽 밴드를 그 분모로 나눠
만듭니다(`ratio_se = loss_total_se / premium`). 분모에 분산을 얹지 않으며,
장래 보험료의 실제 불확실성(해지)은 비율에서 대부분 상쇄됩니다.

```python
fit = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(tri)
fit.summary().head(3)
#> shape: (3, 6)
#> ┌──────────┬────────────┬───────────┬──────────────┬────────────┬──────────┐
#> │ coverage ┆ cohort     ┆ loss_proj ┆ premium_proj ┆ ratio_proj ┆ ratio_se │
#> │ ---      ┆ ---        ┆ ---       ┆ ---          ┆ ---        ┆ ---      │
#> │ str      ┆ date       ┆ f64       ┆ f64          ┆ f64        ┆ f64      │
#> ╞══════════╪════════════╪═══════════╪══════════════╪════════════╪══════════╡
#> │ SURGERY  ┆ 2023-01-01 ┆ 1.6746e9  ┆ 1.1093e9     ┆ 1.509562   ┆ null     │
#> │ SURGERY  ┆ 2023-02-01 ┆ 4.1196e9  ┆ 2.7300e9     ┆ 1.508992   ┆ 0.004207 │
#> │ SURGERY  ┆ 2023-03-01 ┆ 8.8364e8  ┆ 5.8066e8     ┆ 1.521789   ┆ 0.007969 │
#> └──────────┴────────────┴───────────┴──────────────┴────────────┴──────────┘
```

완전히 관측된 첫 코호트는 예측할 미래 셀이 없어 `ratio_se`가 `null`입니다.

`ratio_se`가 정말 손해쪽 표준오차를 알려진 분모로 나눈 값인지는 셀 단위로
직접 확인됩니다. 셀별 산출 컬럼을 펼쳐 `loss_total_se`와 `premium_proj`를
읽으면, 그 둘의 비가 곧 `ratio_se`입니다.

```python
full = fit.to_polars()
chk = full.filter(pl.col("loss_total_se").is_not_null()).select(
    ["cohort", "duration", "loss_total_se", "premium_proj", "ratio_se"]
).head(2)
chk.with_columns((pl.col("loss_total_se") / pl.col("premium_proj")).alias("se_check"))
#> shape: (2, 6)
#> ┌────────────┬──────────┬───────────────┬──────────────┬──────────┬──────────┐
#> │ cohort     ┆ duration ┆ loss_total_se ┆ premium_proj ┆ ratio_se ┆ se_check │
#> │ ---        ┆ ---      ┆ ---           ┆ ---          ┆ ---      ┆ ---      │
#> │ date       ┆ i64      ┆ f64           ┆ f64          ┆ f64      ┆ f64      │
#> ╞════════════╪══════════╪═══════════════╪══════════════╪══════════╪══════════╡
#> │ 2023-02-01 ┆ 36       ┆ 1.1485e7      ┆ 2.7300e9     ┆ 0.004207 ┆ 0.004207 │
#> │ 2023-03-01 ┆ 35       ┆ 3.0104e6      ┆ 5.6439e8     ┆ 0.005334 ┆ 0.005334 │
#> └────────────┴──────────┴───────────────┴──────────────┴──────────┴──────────┘
```

첫 행은 `11,484,605 / 2,730,023,494 = 0.004207`로 `ratio_se`와 정확히
일치합니다 — 분모는 고정된 채 손해쪽 불확실성만 비율로 환산됩니다.

사다리의 위 칸도 같은 방식으로 묶습니다. 손해쪽만 `SmoothLoss`나
`ChainLadder`로 바꾸면 나머지 흐름은 그대로입니다.

```python
cl = lr.Ratio(loss=lr.ChainLadder(), premium=lr.PooledPremium()).fit(tri)
cl.summary().head(2)
#> shape: (2, 6)
#> ┌──────────┬────────────┬───────────┬──────────────┬────────────┬──────────┐
#> │ coverage ┆ cohort     ┆ loss_proj ┆ premium_proj ┆ ratio_proj ┆ ratio_se │
#> │ ---      ┆ ---        ┆ ---       ┆ ---          ┆ ---        ┆ ---      │
#> │ str      ┆ date       ┆ f64       ┆ f64          ┆ f64        ┆ f64      │
#> ╞══════════╪════════════╪═══════════╪══════════════╪════════════╪══════════╡
#> │ SURGERY  ┆ 2023-01-01 ┆ 1.6746e9  ┆ 1.1093e9     ┆ 1.509562   ┆ null     │
#> │ SURGERY  ┆ 2023-02-01 ┆ 4.1195e9  ┆ 2.7300e9     ┆ 1.508976   ┆ 0.004335 │
#> └──────────┴────────────┴───────────┴──────────────┴────────────┴──────────┘
```

## 코호트 수준 반영과 불확실성

코호트별 손해율 수준 차이까지 반영하고 싶으면 `CredibleLoss`를 씁니다.
풀링된 형상은 그대로 두고 각 코호트의 수준만 신뢰도 가중으로 보정합니다 —
관측이 쌓인 코호트일수록 자기 경험을 더 믿고, 갓 인수된 코호트는
포트폴리오 평균에 가깝게 묶어 둡니다.

불확실성 밴드는 추정기에 `uncertainty=ResidualBootstrap(...)`를 넘겨
켭니다. 잔차 부트스트랩(residual bootstrap)으로 전체 파이프라인을 매
복제마다 다시 적합하고, 달력 드리프트(drift) 밴드까지 포함해
`ratio_se` / `ratio_ci_lo` / `ratio_ci_hi`를 채웁니다(기본값
`uncertainty=None`은 점추정만).

```python
fit = lr.Ratio(
    loss=lr.CredibleLoss(uncertainty=lr.ResidualBootstrap(n_replicates=500, seed=42)),
    premium=lr.PooledPremium(),
).fit(tri)
fit.to_polars().filter(pl.col("source") == "own").select(
    ["cohort", "duration", "ratio_proj", "ratio_se", "ratio_ci_lo", "ratio_ci_hi"]
).head(3)
#> shape: (3, 6)
#> ┌────────────┬──────────┬────────────┬──────────┬─────────────┬─────────────┐
#> │ cohort     ┆ duration ┆ ratio_proj ┆ ratio_se ┆ ratio_ci_lo ┆ ratio_ci_hi │
#> │ ---        ┆ ---      ┆ ---        ┆ ---      ┆ ---         ┆ ---         │
#> │ date       ┆ i64      ┆ f64        ┆ f64      ┆ f64         ┆ f64         │
#> ╞════════════╪══════════╪════════════╪══════════╪═════════════╪═════════════╡
#> │ 2023-02-01 ┆ 36       ┆ 1.509848   ┆ 0.004654 ┆ 1.500727    ┆ 1.518969    │
#> │ 2023-03-01 ┆ 35       ┆ 1.524031   ┆ 0.005755 ┆ 1.51275     ┆ 1.535311    │
#> │ 2023-03-01 ┆ 36       ┆ 1.524907   ┆ 0.008948 ┆ 1.507369    ┆ 1.542445    │
#> └────────────┴──────────┴────────────┴──────────┴─────────────┴─────────────┘
```

손해쪽 적합의 코호트별 신뢰도(credibility)는 `.credibility`로 직접 볼 수
있습니다. `u`는 코호트의 자기 수준, `Z`는 그 수준을 얼마나 믿을지 정하는
신뢰도 가중치, `psi`는 코호트 간 분산입니다. `Z`가 1에 가까우면 코호트의
자기 경험을 거의 그대로 쓰고, 0에 가까우면 포트폴리오 평균 쪽으로 끌어당겨
수축(shrinkage)합니다 — 최종 수준은 `Z*u + (1-Z)*평균` 꼴로 섞입니다.

```python
cred = lr.CredibleLoss(uncertainty=lr.ResidualBootstrap(n_replicates=500, seed=42)).fit(tri)
cred.credibility.head(3)
#> shape: (3, 5)
#> ┌──────────┬────────────┬──────────┬──────────┬──────────┐
#> │ coverage ┆ cohort     ┆ u        ┆ Z        ┆ psi      │
#> │ ---      ┆ ---        ┆ ---      ┆ ---      ┆ ---      │
#> │ str      ┆ date       ┆ f64      ┆ f64      ┆ f64      │
#> ╞══════════╪════════════╪══════════╪══════════╪══════════╡
#> │ SURGERY  ┆ 2023-01-01 ┆ 1.015829 ┆ 0.982995 ┆ 0.014626 │
#> │ SURGERY  ┆ 2023-02-01 ┆ 1.020378 ┆ 0.992758 ┆ 0.014626 │
#> │ SURGERY  ┆ 2023-03-01 ┆ 1.037056 ┆ 0.965518 ┆ 0.014626 │
#> └──────────┴────────────┴──────────┴──────────┴──────────┘
```

여기서는 `Z`가 0.98 안팎으로 1에 매우 가깝습니다 — 수술담보의 각 코호트는
관측이 충분히 쌓여 자기 경험을 거의 그대로 신뢰하고, 포트폴리오 평균으로의
수축이 거의 일어나지 않는다는 뜻입니다. 관측 기간이 짧은 갓 인수 코호트일수록
`Z`는 더 작아지고 평균 쪽으로 더 강하게 끌려갑니다.

신뢰도 보정이 셀 단위 정확도까지 항상 개선하는 것은 아닙니다 — 어느
모델이 맞는지는 데이터로 가립니다({doc}`튜토리얼 3장
<tutorial/04-projection>`).

## 구조 변화 탐지하기

`RegimeDetector`는 코호트 손해율 경로에서 구조 변화 시점을 찾습니다.
수술담보에 심어 둔 2024년 7월 변화가 검출됩니다.

```python
reg = lr.RegimeDetector(target="ratio", window=12).detect(tri)
reg.change_points
#> [datetime.date(2024, 7, 1)]
```

## 백테스트로 검증하기

`Backtest`는 가장 최근 몇 개의 달력 대각선을 가린 뒤 추정기를 다시
적합하고, 그 예측을 실제값과 비교합니다. `holdouts`에 정수 하나를 주면
단일 시점 백테스트이고, 여러 개를 주면 시점을 옮겨 가며 반복하는
rolling-origin 백테스트가 됩니다. `reliable_horizon()`은 예측이 허용
오차 안에 머무는 경과 길이를 알려 줍니다.

```python
bt = lr.Backtest(
    estimator=lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()),
    holdouts=(6, 12),
    target="ratio",
).fit(tri)
bt.reliable_horizon()
#> shape: (1, 3)
#> ┌──────────┬──────────────────┬─────────────┐
#> │ coverage ┆ reliable_horizon ┆ max_horizon │
#> │ ---      ┆ ---              ┆ ---         │
#> │ str      ┆ i64              ┆ i64         │
#> ╞══════════╪══════════════════╪═════════════╡
#> │ SURGERY  ┆ 1                ┆ 12          │
#> └──────────┴──────────────────┴─────────────┘
```

## 추정기 비교하기

여러 추정기를 한 기준선과 맞대어 비교하려면 `EstimatorComparison`을 씁니다.
추정기들을 이름과 함께 넘기고 `baseline`으로 비교 기준을 정한 뒤, 각 hold-out
시점에서 백테스트한 성적을 한 표로 모읍니다. `scorecard()`는 추정기별 오차
지표(`bias`·`mae`·`rmse` 등)를 펼쳐 주고, `best()`는 그 지표들을 Borda 방식으로
합산해 시점별 우승 추정기를 골라 줍니다.

```python
ec = lr.EstimatorComparison(
    estimators={
        "pooled":   lr.Ratio(loss=lr.PooledLoss(),   premium=lr.PooledPremium()),
        "credible": lr.Ratio(loss=lr.CredibleLoss(), premium=lr.PooledPremium()),
        "chain":    lr.Ratio(loss=lr.ChainLadder(),  premium=lr.PooledPremium()),
    },
    holdouts=(6, 12),
    target="ratio",
    baseline="pooled",
).fit(tri)

ec.scorecard().filter(
    (pl.col("holdout") == 6) & (pl.col("lane") == "cumulative")
).select(["estimator", "holdout", "n", "bias", "mae", "rmse"])
#> shape: (3, 6)
#> ┌───────────┬─────────┬─────┬───────────┬──────────┬──────────┐
#> │ estimator ┆ holdout ┆ n   ┆ bias      ┆ mae      ┆ rmse     │
#> │ ---       ┆ ---     ┆ --- ┆ ---       ┆ ---      ┆ ---      │
#> │ str       ┆ i64     ┆ u32 ┆ f64       ┆ f64      ┆ f64      │
#> ╞═══════════╪═════════╪═════╪═══════════╪══════════╪══════════╡
#> │ pooled    ┆ 6       ┆ 159 ┆ -0.082077 ┆ 0.084646 ┆ 0.131572 │
#> │ credible  ┆ 6       ┆ 159 ┆ -0.061056 ┆ 0.062737 ┆ 0.105374 │
#> │ chain     ┆ 6       ┆ 159 ┆ -0.000747 ┆ 0.01795  ┆ 0.032529 │
#> └───────────┴─────────┴─────┴───────────┴──────────┴──────────┘
```

```python
ec.best()
#> shape: (6, 6)
#> ┌──────────┬─────────┬───────────┬───────────┬──────────┬──────────┐
#> │ coverage ┆ holdout ┆ estimator ┆ bias_rank ┆ mae_rank ┆ rank_sum │
#> │ ---      ┆ ---     ┆ ---       ┆ ---       ┆ ---      ┆ ---      │
#> │ str      ┆ i64     ┆ str       ┆ f64       ┆ f64      ┆ f64      │
#> ╞══════════╪═════════╪═══════════╪═══════════╪══════════╪══════════╡
#> │ SURGERY  ┆ 6       ┆ chain     ┆ 1.0       ┆ 1.0      ┆ 2.0      │
#> │ SURGERY  ┆ 6       ┆ credible  ┆ 2.0       ┆ 2.0      ┆ 4.0      │
#> │ SURGERY  ┆ 6       ┆ pooled    ┆ 3.0       ┆ 3.0      ┆ 6.0      │
#> │ SURGERY  ┆ 12      ┆ chain     ┆ 1.0       ┆ 1.0      ┆ 2.0      │
#> │ SURGERY  ┆ 12      ┆ credible  ┆ 2.0       ┆ 2.0      ┆ 4.0      │
#> │ SURGERY  ┆ 12      ┆ pooled    ┆ 3.0       ┆ 3.0      ┆ 6.0      │
#> └──────────┴─────────┴───────────┴───────────┴──────────┴──────────┘
```

이 hold-out 구간에서는 `ChainLadder`가 두 시점 모두 가장 낮은 오차로
앞섭니다. 다만 이 우열은 백테스트가 닿는 셀에 한정된 결과이므로, 결정은
데이터·맥락과 함께 읽어야 합니다.

## 다음 단계

여기까지가 전체 흐름의 압축본입니다. 각 단계의 개념과 결과 해석은
튜토리얼에서 한 장씩 다룹니다.

- {doc}`튜토리얼 <tutorial/index>` — 손해율 삼각형부터 백테스트까지
  순서대로
- {doc}`API 레퍼런스 <api>` — 모든 클래스와 함수의 상세 설명
```
