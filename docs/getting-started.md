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

`Ratio`는 손해율을 추정하는 추정기입니다. 기본값은 `method="ed"`로,
노출 기반(exposure-driven, ED) 예측입니다. 갓 인수되어 관측이 얇은
코호트에서도 별도의 사전 판단 없이 안전하게 작동하는
기준선이라 기본으로 채택했습니다.

```python
fit = lr.Ratio(method="ed").fit(tri)
fit.summary().select(["coverage", "cohort", "ratio_proj", "ratio_se", "ratio_cv"]).head(3)
#> shape: (3, 5)
#> ┌──────────┬────────────┬────────────┬──────────┬──────────┐
#> │ coverage ┆ cohort     ┆ ratio_proj ┆ ratio_se ┆ ratio_cv │
#> │ ---      ┆ ---        ┆ ---        ┆ ---      ┆ ---      │
#> │ str      ┆ date       ┆ f64        ┆ f64      ┆ f64      │
#> ╞══════════╪════════════╪════════════╪══════════╪══════════╡
#> │ SURGERY  ┆ 2023-01-01 ┆ 1.509562   ┆ null     ┆ null     │
#> │ SURGERY  ┆ 2023-02-01 ┆ 1.508992   ┆ 0.004207 ┆ 0.002788 │
#> │ SURGERY  ┆ 2023-03-01 ┆ 1.521789   ┆ 0.007969 ┆ 0.005236 │
#> └──────────┴────────────┴────────────┴──────────┴──────────┘

fit.plot()
```

코호트별 손해율 수준 차이까지 반영하고 싶으면 cohort-scaled(CS,
`method="cs"`)를 씁니다. CS는 ED의 풀링된 형상은 그대로 두고, 각
코호트의 수준만 신뢰도(credibility) 가중으로 보정합니다 — 관측이 쌓인
코호트일수록 자기 경험을 더 믿고, 갓 인수된 코호트는 포트폴리오 평균에
가깝게 묶어 둡니다. CS의 불확실성은 닫힌형 공식이 성립하지 않아
부트스트랩 전용이며, `n_bootstrap=`으로 켭니다.

```python
fit = lr.Ratio(method="cs", n_bootstrap=500, seed=42).fit(tri)
fit.summary().select(
    ["coverage", "cohort", "ratio_proj", "ratio_se", "ratio_ci_lo", "ratio_ci_hi"]
).head(3)
#> shape: (3, 6)
#> ┌──────────┬────────────┬────────────┬──────────┬─────────────┬─────────────┐
#> │ coverage ┆ cohort     ┆ ratio_proj ┆ ratio_se ┆ ratio_ci_lo ┆ ratio_ci_hi │
#> │ ---      ┆ ---        ┆ ---        ┆ ---      ┆ ---         ┆ ---         │
#> │ str      ┆ date       ┆ f64        ┆ f64      ┆ f64         ┆ f64         │
#> ╞══════════╪════════════╪════════════╪══════════╪═════════════╪═════════════╡
#> │ SURGERY  ┆ 2023-01-01 ┆ 1.509562   ┆ null     ┆ null        ┆ null        │
#> │ SURGERY  ┆ 2023-02-01 ┆ 1.511121   ┆ 0.033652 ┆ 1.457595    ┆ 1.595545    │
#> │ SURGERY  ┆ 2023-03-01 ┆ 1.527346   ┆ 0.067159 ┆ 1.419338    ┆ 1.675426    │
#> └──────────┴────────────┴────────────┴──────────┴─────────────┴─────────────┘
```

CS는 포트폴리오 집계 수준의 체계적 편향을 줄이는 보정이고, 셀 단위
정확도는 대체로 ED가 낫습니다 — 언제 어느 쪽이 맞는지는 데이터로
가립니다 ({doc}`튜토리얼 4장 <tutorial/04-projection>`).

## 구조 변화 탐지하기

`detect_regime`은 코호트 손해율 경로에서 구조 변화 시점을 찾습니다.
수술담보에 심어 둔 2024년 7월 변화가 검출됩니다.

```python
reg = tri.detect_regime(target="ratio", window=12)
reg.change_points
#> [datetime.date(2024, 7, 1)]
```

## 백테스트로 검증하기

`Backtest`는 가장 최근 몇 개의 달력 대각선을 가린 뒤 추정기를 다시
적합하고, 그 예측을 실제값과 비교합니다. `ae_err = 실제 / 예측 - 1`은
부호 있는 상대 오차입니다.

```python
bt = lr.Backtest(estimator=lr.Ratio(method="ed"), holdout=6).fit(tri)
bt.diag_summary.select(
    ["coverage", "cal_idx", "n", "ae_err_mean", "ae_err_med", "ae_err_wt"]
).head(3)
#> shape: (3, 6)
#> ┌──────────┬─────────┬─────┬─────────────┬────────────┬───────────┐
#> │ coverage ┆ cal_idx ┆ n   ┆ ae_err_mean ┆ ae_err_med ┆ ae_err_wt │
#> │ ---      ┆ ---     ┆ --- ┆ ---         ┆ ---        ┆ ---       │
#> │ str      ┆ i64     ┆ u32 ┆ f64         ┆ f64        ┆ f64       │
#> ╞══════════╪═════════╪═════╪═════════════╪════════════╪═══════════╡
#> │ SURGERY  ┆ 31      ┆ 29  ┆ -0.038762   ┆ -0.004586  ┆ -0.026285 │
#> │ SURGERY  ┆ 32      ┆ 28  ┆ -0.060688   ┆ -0.011492  ┆ -0.04627  │
#> │ SURGERY  ┆ 33      ┆ 27  ┆ -0.08139    ┆ -0.010749  ┆ -0.065249 │
#> └──────────┴─────────┴─────┴─────────────┴────────────┴───────────┘
```

## 다음 단계

여기까지가 전체 흐름의 압축본입니다. 각 단계의 개념과 결과 해석은
튜토리얼에서 한 장씩 다룹니다.

- {doc}`튜토리얼 <tutorial/index>` — 손해율 삼각형부터 백테스트까지
  순서대로
- {doc}`API 레퍼런스 <api>` — 모든 클래스와 함수의 상세 설명
