# 빠른 시작

이 문서는 설치부터 손해율 예측·검증까지 전체 흐름을 5분 안에 한 번
훑어보는 것을 목표로 합니다. 각 단계가 왜 필요하고 결과를 어떻게
읽는지는 {doc}`튜토리얼 <tutorial/index>`에서 차근차근 다룹니다.

## 설치

```bash
pip install lossratio              # polars only
pip install lossratio[pandas]      # add pandas / pyarrow support
```

## 데이터 불러오기

lossratio에는 시연용 합성 경험 데이터가 들어 있습니다. 네 개의
담보(CI / CAN / HOS / SUR)에 대해 인수월 코호트별 손해·보험료가
경과월(`dev_m`)을 따라 기록된 long-format 표입니다. 이 가운데
SUR 담보에는 2024년 7월에 한 번의 구조 변화(regime)가 심어져 있어,
이 빠른 시작에서는 SUR에 집중합니다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience()
df.select(["coverage", "uy_m", "cy_m", "dev_m", "incr_loss", "incr_premium"]).head(3)
#> shape: (3, 6)
#> ┌──────────┬────────────┬────────────┬───────┬───────────┬──────────────┐
#> │ coverage ┆ uy_m       ┆ cy_m       ┆ dev_m ┆ incr_loss ┆ incr_premium │
#> │ ---      ┆ ---        ┆ ---        ┆ ---   ┆ ---       ┆ ---          │
#> │ str      ┆ date       ┆ date       ┆ i64   ┆ i64       ┆ i64          │
#> ╞══════════╪════════════╪════════════╪═══════╪═══════════╪══════════════╡
#> │ CI       ┆ 2023-01-01 ┆ 2023-01-01 ┆ 1     ┆ 12562144  ┆ 18836105     │
#> │ CI       ┆ 2023-01-01 ┆ 2023-02-01 ┆ 2     ┆ 651603    ┆ 17699438     │
#> │ CI       ┆ 2023-01-01 ┆ 2023-03-01 ┆ 3     ┆ 3719107   ┆ 19232426     │
#> └──────────┴────────────┴────────────┴───────┴───────────┴──────────────┘
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

`Ratio`는 손해율을 예측하는 추정기입니다. `method="sa"`는 단계
적응형으로, 성숙점 이전에는 노출 기반(ED), 이후에는 체인래더(CL)로
예측합니다.

```python
fit = lr.Ratio(method="sa").fit(tri)
fit.summary().select(["coverage", "cohort", "ratio_ult", "ratio_se", "ratio_cv"]).head(3)
#> shape: (3, 5)
#> ┌──────────┬────────────┬───────────┬──────────┬──────────┐
#> │ coverage ┆ cohort     ┆ ratio_ult ┆ ratio_se ┆ ratio_cv │
#> │ ---      ┆ ---        ┆ ---       ┆ ---      ┆ ---      │
#> │ str      ┆ date       ┆ f64       ┆ f64      ┆ f64      │
#> ╞══════════╪════════════╪═══════════╪══════════╪══════════╡
#> │ SUR      ┆ 2023-01-01 ┆ 1.509562  ┆ null     ┆ null     │
#> │ SUR      ┆ 2023-02-01 ┆ 1.508976  ┆ 0.004335 ┆ 0.002873 │
#> │ SUR      ┆ 2023-03-01 ┆ 1.522523  ┆ 0.00836  ┆ 0.005491 │
#> └──────────┴────────────┴───────────┴──────────┴──────────┘

fit.plot()
```

## 구조 변화 탐지하기

`detect_regime`은 코호트 손해율 경로에서 구조 변화 시점을 찾습니다.
SUR에 심어 둔 2024년 7월 변화가 검출됩니다.

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
bt = lr.Backtest(estimator=lr.Ratio(method="sa"), holdout=6).fit(tri)
bt.diag_summary.select(
    ["coverage", "cal_idx", "n", "ae_err_mean", "ae_err_med", "ae_err_wt"]
).head(3)
#> shape: (3, 6)
#> ┌──────────┬─────────┬─────┬─────────────┬────────────┬───────────┐
#> │ coverage ┆ cal_idx ┆ n   ┆ ae_err_mean ┆ ae_err_med ┆ ae_err_wt │
#> │ ---      ┆ ---     ┆ --- ┆ ---         ┆ ---        ┆ ---       │
#> │ str      ┆ i64     ┆ u32 ┆ f64         ┆ f64        ┆ f64       │
#> ╞══════════╪═════════╪═════╪═════════════╪════════════╪═══════════╡
#> │ SUR      ┆ 31      ┆ 29  ┆ 0.000615    ┆ 0.002411   ┆ 0.001107  │
#> │ SUR      ┆ 32      ┆ 28  ┆ 0.000807    ┆ -0.000776  ┆ 0.000218  │
#> │ SUR      ┆ 33      ┆ 27  ┆ -0.00358    ┆ -0.001933  ┆ -0.002875 │
#> └──────────┴─────────┴─────┴─────────────┴────────────┴───────────┘
```

## 다음 단계

여기까지가 전체 흐름의 압축본입니다. 각 단계의 개념과 결과 해석은
튜토리얼에서 한 장씩 다룹니다.

- {doc}`튜토리얼 <tutorial/index>` — 손해율 삼각형부터 백테스트까지
  순서대로
- {doc}`API 레퍼런스 <api>` — 모든 클래스와 함수의 상세 설명
