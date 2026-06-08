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
담보(CI / CAN / HOS / SUR)에 대해 인수월 코호트별로 손해와 보험료가
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
df_sur = df.filter(pl.col("coverage") == "SUR")
tri = lr.Triangle(df_sur, groups="coverage")
```

## 손해율 예측하기

`Ratio`는 손해율을 추정하는 추정기입니다. 기본값은 `method="ed"`로,
노출 기반(exposure-driven, ED) 예측입니다. 갓 인수되어 관측이 얇은
코호트에서도 전환점 검출 같은 사전 단계 없이 안전하게 작동하는
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
#> │ SUR      ┆ 2023-01-01 ┆ 1.509562   ┆ null     ┆ null     │
#> │ SUR      ┆ 2023-02-01 ┆ 1.508992   ┆ 0.004207 ┆ 0.002788 │
#> │ SUR      ┆ 2023-03-01 ┆ 1.521789   ┆ 0.007969 ┆ 0.005236 │
#> └──────────┴────────────┴────────────┴──────────┴──────────┘

fit.plot()
```

경과 후반부를 체인래더(CL)로 넘기고 싶으면 단계 적응형(stage-adaptive,
SA)을 씁니다. SA는 전환점 이전에는 ED, 이후에는 CL로 예측하며, 전환점은
`switch=`로 지정합니다 — 정수를 주면 그 경과 기간에 고정되고,
`SwitchPoint.detect()`를 주면 백테스트로 표본 외 손해 예측 오차가 가장
작아지는 경계를 골라 줍니다 (백테스트 fold 안에서도 누출 없이 안전하게
재해석됩니다).

```python
fit = lr.Ratio(method="sa", switch=lr.SwitchPoint.detect()).fit(tri)
fit.switch_point
#> {'SUR': 1}
```

`SwitchPoint`는 보수적입니다 — 표본이 얇거나 분기 단위로 거칠면 흔히
순수 ED로 물러납니다 (`switch_point`가 `None`이고 `switch_from` 열이
null). 그 경우 예측은 안전한 ED 베이스라인과 같으며, 이는 의도된
동작입니다. 전환점을 직접 고정하려면 `switch=12`처럼 정수를 주면
됩니다.

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
#> │ SUR      ┆ 31      ┆ 29  ┆ -0.038762   ┆ -0.004586  ┆ -0.026285 │
#> │ SUR      ┆ 32      ┆ 28  ┆ -0.060688   ┆ -0.011492  ┆ -0.04627  │
#> │ SUR      ┆ 33      ┆ 27  ┆ -0.08139    ┆ -0.010749  ┆ -0.065249 │
#> └──────────┴─────────┴─────┴─────────────┴────────────┴───────────┘
```

## 다음 단계

여기까지가 전체 흐름의 압축본입니다. 각 단계의 개념과 결과 해석은
튜토리얼에서 한 장씩 다룹니다.

- {doc}`튜토리얼 <tutorial/index>` — 손해율 삼각형부터 백테스트까지
  순서대로
- {doc}`API 레퍼런스 <api>` — 모든 클래스와 함수의 상세 설명
