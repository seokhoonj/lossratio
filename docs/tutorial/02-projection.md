# 2장. 손해율 예측 — 손해·보험료 모델과 Ratio 합성

```{admonition} 이 장에서 배우는 것
:class: tip

- 손해율 예측은 손해와 보험료를 각각 미래로 뻗어 합성하는 것
- 손해 모델 `PooledLoss` — 강도 x 누적 보험료
- 보험료 모델 `PooledPremium` — 알려진 노출이라 자기-발전, 점추정
- `Ratio(loss=, premium=)` 합성과 예측 손해율 읽기
- 예측 손해율의 밴드는 분자(손해)에서 온다
```

1장의 직각삼각형은 오른쪽 아래 — 아직 경과하지 않은 칸 — 가 비어 있었습니다.
코호트가 최근일수록 관측된 경과가 짧아 빈 칸이 많습니다. 이 장은 그 빈 칸을
채웁니다. 손해율은 손해를 보험료로 나눈 값이니, **손해와 보험료를 각각 미래
경과로 예측한 뒤 나눕니다**.

이 장의 예시는 **2025년 6월 암(CANCER) 코호트**입니다. 인수된 지 얼마 안 돼
경과 7까지만 관측됐습니다.

## 2.1 무엇을 예측하는가

관측이 끝난 칸에서 손해도 보험료도 멈춰 있습니다. 2025-06 코호트가 마지막으로
관측된 경과 7을 봅니다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience()
tri = lr.Triangle(df.filter(pl.col("coverage") == "CANCER"), groups="coverage")

(tri.df
    .filter(pl.col("cohort") == pl.date(2025, 6, 1))
    .filter(pl.col("duration") == 7)
    .select(
        "duration",
        pl.col("loss").cast(pl.Int64),
        pl.col("premium").cast(pl.Int64),
        pl.col("ratio").round(3),
    ))
#> shape: (1, 4)
#> ┌──────────┬────────┬─────────┬───────┐
#> │ duration ┆ loss   ┆ premium ┆ ratio │
#> │ ---      ┆ ---    ┆ ---     ┆ ---   │
#> │ i64      ┆ i64    ┆ i64     ┆ f64   │
#> ╞══════════╪════════╪═════════╪═══════╡
#> │ 7        ┆ 653086 ┆ 840381  ┆ 0.777 │
#> └──────────┴────────┴─────────┴───────┘
```

`loss`·`premium`은 누적 손해·누적 보험료(정수 달러), `ratio`는 누적 손해율입니다.

경과 7의 누적 손해율은 0.777입니다. 그런데 1장에서 봤듯, 이 값은 이 코호트의
**최종 손해율이 아닙니다** — 분모효과가 아직 다 풀리지 않았고, 앞으로 경과가
더 쌓이면서 손해와 보험료가 각자 계속 흐릅니다. 지금 이 코호트의 손해율이 앞으로
어디로 갈지 알려면, 손해와 보험료를 각각 남은 경과로 뻗어야 합니다.

## 2.2 손해 예측 — PooledLoss

`PooledLoss`는 손해를 **강도(intensity)**로 예측합니다. 강도 `g_k`는 경과 `k`에서
*누적 보험료 한 단위당 그 기간에 늘어난 손해*입니다. 이 강도를 모든 코호트에서
함께(완전 풀링) 추정한 뒤, 각 미래 경과에서 증분 손해를 이렇게 채웁니다.

$$
\text{증분 손해}_k = g_k \times \text{누적 보험료}_k
$$

암담보의 풀링 강도는 경과가 갈수록 작아집니다 (분모인 누적 보험료가 커지므로) —
경과 1에서 0.837, 경과 8에서 0.109입니다. 강도를 읽고 다스리는 자세한 이야기는
부록(강도)에서 다룹니다. 여기서는 예측에 바로 씁니다.

```python
lf = lr.PooledLoss().fit(tri)

(lf.df
    .filter(pl.col("cohort") == pl.date(2025, 6, 1))
    .filter(pl.col("duration").is_in([7, 8, 12, 36]))
    .select(
        "duration",
        pl.col("loss_proj").round(0).cast(pl.Int64),
        pl.col("incr_loss_proj").round(0).cast(pl.Int64),
    ))
#> shape: (4, 3)
#> ┌──────────┬───────────┬────────────────┐
#> │ duration ┆ loss_proj ┆ incr_loss_proj │
#> │ ---      ┆ ---       ┆ ---            │
#> │ i64      ┆ i64       ┆ i64            │
#> ╞══════════╪═══════════╪════════════════╡
#> │ 7        ┆ 653086    ┆ 120009         │
#> │ 8        ┆ 757347    ┆ 104261         │
#> │ 12       ┆ 1146172   ┆ 87326          │
#> │ 36       ┆ 3659281   ┆ 107008         │
#> └──────────┴───────────┴────────────────┘
```

경과 8의 증분 손해 104261은 강도 `g_8 = 0.109`에 그 경과의 누적 보험료 960064를
곱한 값입니다 (`0.109 x 960064 ≈ 104261`). 관측이 끝난 경과 7의 누적 손해 653086에서
출발해, 미래 경과마다 이 증분을 더해 나가면 경과 36에서 누적 손해 3659281에
닿습니다.

## 2.3 보험료 예측 — PooledPremium

보험료는 손해와 성격이 다릅니다. 위험보험료는 요율에 유지계약을 곱해 *이미
배정된 알려진 노출*입니다. 그래서 미래 경과의 보험료는 손해에 기대지 않고,
**자기 자신의 발전 패턴**(코호트가 경과하며 보험료가 쌓이는 링크비)으로 뻗습니다.
`PooledPremium`이 이 발전을 완전 풀링으로 추정합니다.

```python
pf = lr.PooledPremium().fit(tri)

(pf.df
    .filter(pl.col("cohort") == pl.date(2025, 6, 1))
    .filter(pl.col("duration").is_in([7, 8, 12, 36]))
    .select(
        "duration",
        pl.col("premium_proj").round(0).cast(pl.Int64),
    ))
#> shape: (4, 2)
#> ┌──────────┬──────────────┐
#> │ duration ┆ premium_proj │
#> │ ---      ┆ ---          │
#> │ i64      ┆ i64          │
#> ╞══════════╪══════════════╡
#> │ 7        ┆ 840381       │
#> │ 8        ┆ 960064       │
#> │ 12       ┆ 1440745      │
#> │ 36       ┆ 4315536      │
#> └──────────┴──────────────┘
```

보험료는 *알려진 노출*이라 점추정만 냅니다 — 발전계수에 표준오차를 붙이지
않습니다. 이유는 2.6에서 밴드를 다룰 때 봅니다.

## 2.4 합성 — Ratio

손해와 보험료 두 예측을 나누면 예측 손해율입니다. `Ratio`에 손해 모델과 보험료
모델을 넘겨 합성합니다. 각 셀의 `ratio_proj`는 `loss_proj / premium_proj`입니다.

```python
rf = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(tri)

(rf.df
    .filter(pl.col("cohort") == pl.date(2025, 6, 1))
    .filter(pl.col("duration").is_in([7, 8, 12, 24, 36]))
    .select(
        "duration",
        pl.col("loss_proj").round(0).cast(pl.Int64),
        pl.col("premium_proj").round(0).cast(pl.Int64),
        pl.col("ratio_proj").round(3),
    ))
#> shape: (5, 4)
#> ┌──────────┬───────────┬──────────────┬────────────┐
#> │ duration ┆ loss_proj ┆ premium_proj ┆ ratio_proj │
#> │ ---      ┆ ---       ┆ ---          ┆ ---        │
#> │ i64      ┆ i64       ┆ i64          ┆ f64        │
#> ╞══════════╪═══════════╪══════════════╪════════════╡
#> │ 7        ┆ 653086    ┆ 840381       ┆ 0.777      │
#> │ 8        ┆ 757347    ┆ 960064       ┆ 0.789      │
#> │ 12       ┆ 1146172   ┆ 1440745      ┆ 0.796      │
#> │ 24       ┆ 2349131   ┆ 2878908      ┆ 0.816      │
#> │ 36       ┆ 3659281   ┆ 4315536      ┆ 0.848      │
#> └──────────┴───────────┴──────────────┴────────────┘
```

관측이 끝난 경과 7의 손해율 0.777에서 출발해, 예측 구간에서 손해율이 서서히
올라 경과 36에서 0.848에 닿습니다. **관측 끝의 값이 최종이 아니라**, 분모효과가
마저 풀리며 더 오른다는 것을 예측이 보여 줍니다.

궤적을 그림으로 보려면 손으로 matplotlib을 짤 필요 없이 결과 객체의
`.plot()` 한 줄이면 됩니다 — 그룹의 **모든 코호트**를 관측(실선)·예측(점선)으로
한 번에 그립니다.

```{eval-rst}
.. plot::
   :context: close-figs
   :caption: CANCER 담보의 코호트별 누적 손해율 예측. 관측 구간은 실선, 예측 구간은 점선(끝점은 프런티어)이다. 최근 코호트일수록 관측이 짧아 예측 구간이 길다.

   import polars as pl
   import lossratio as lr

   tri = lr.Triangle(
       lr.load_experience().filter(pl.col("coverage") == "CANCER"),
       groups="coverage")
   lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(tri).plot(metric="ratio")
```

## 2.5 결과 읽기 — summary · predict · plot

적합 결과 `RatioFit`은 셀별 진단이 모두 담긴 `.df` 외에, 바로 쓰기 위한 세
메서드를 제공합니다.

`.summary()`는 **코호트별 최종 예측**(마지막 경과)을 한 줄씩 냅니다.

```python
from datetime import date

(rf.summary()
    .filter(pl.col("cohort").is_in([
        date(2023, 1, 1), date(2024, 6, 1),
        date(2025, 6, 1), date(2025, 12, 1)]))
    .select(
        "cohort",
        pl.col("loss_proj").round(0).cast(pl.Int64),
        pl.col("premium_proj").round(0).cast(pl.Int64),
        pl.col("ratio_proj").round(3),
        pl.col("ratio_se").round(4),
    ))
#> shape: (4, 5)
#> ┌────────────┬───────────┬──────────────┬────────────┬──────────┐
#> │ cohort     ┆ loss_proj ┆ premium_proj ┆ ratio_proj ┆ ratio_se │
#> │ ---        ┆ ---       ┆ ---          ┆ ---        ┆ ---      │
#> │ date       ┆ i64       ┆ i64          ┆ f64        ┆ f64      │
#> ╞════════════╪═══════════╪══════════════╪════════════╪══════════╡
#> │ 2023-01-01 ┆ 1467709   ┆ 1694550      ┆ 0.866      ┆ null     │
#> │ 2024-06-01 ┆ 2489107   ┆ 2821507      ┆ 0.882      ┆ 0.0365   │
#> │ 2025-06-01 ┆ 3659281   ┆ 4315536      ┆ 0.848      ┆ 0.0414   │
#> │ 2025-12-01 ┆ 2965694   ┆ 3523852      ┆ 0.842      ┆ 0.049    │
#> └────────────┴───────────┴──────────────┴────────────┴──────────┘
```

2023-01 코호트는 이미 경과 36까지 전부 관측돼 예측할 칸이 없으니 표준오차가
`null`입니다. 최근 코호트일수록 채울 칸이 많아 표준오차가 큽니다.

`.predict()`는 **예측 표면**만 추립니다 — 셀마다 예측값과 출처(`source`:
관측 `observed` / 자기 데이터로 채운 `own`)를 붙이고, 표준오차 열은 덜어 낸
가벼운 뷰입니다.

```python
rf.predict().columns
#> ['coverage', 'cohort', 'duration', 'loss_proj', 'premium_proj', 'ratio_proj', 'source']
```

`.plot()`은 코호트별 궤적을 한 줄로 그립니다 — 관측 구간은 실선, 예측 구간은
점선입니다. `metric`으로 곡선을 고릅니다 (`"ratio"` 기본 · `"loss"` · `"premium"`).

```python
rf.plot(metric="ratio")     # 위 그림을 그룹의 모든 코호트에 대해 한 번에
```

## 2.6 예측 손해율의 밴드 — 분자에서만

예측 손해율에는 표준오차 `ratio_se`와 신뢰구간이 붙습니다. 관측된 칸은 예측이
아니니 `null`이고, 예측 칸에만 값이 있습니다.

```python
(rf.df
    .filter(pl.col("cohort") == pl.date(2025, 6, 1))
    .filter(pl.col("duration").is_in([7, 8, 24, 36]))
    .select(
        "duration",
        pl.col("ratio_proj").round(3),
        pl.col("ratio_se").round(4),
        pl.col("ratio_ci_lo").round(3),
        pl.col("ratio_ci_hi").round(3),
    ))
#> shape: (4, 5)
#> ┌──────────┬────────────┬──────────┬─────────────┬─────────────┐
#> │ duration ┆ ratio_proj ┆ ratio_se ┆ ratio_ci_lo ┆ ratio_ci_hi │
#> │ ---      ┆ ---        ┆ ---      ┆ ---         ┆ ---         │
#> │ i64      ┆ f64        ┆ f64      ┆ f64         ┆ f64         │
#> ╞══════════╪════════════╪══════════╪═════════════╪═════════════╡
#> │ 7        ┆ 0.777      ┆ null     ┆ null        ┆ null        │
#> │ 8        ┆ 0.789      ┆ 0.0268   ┆ 0.736       ┆ 0.841       │
#> │ 24       ┆ 0.816      ┆ 0.0472   ┆ 0.724       ┆ 0.908       │
#> │ 36       ┆ 0.848      ┆ 0.0414   ┆ 0.767       ┆ 0.929       │
#> └──────────┴────────────┴──────────┴─────────────┴─────────────┘
```

이 밴드는 **분자(손해)의 불확실성에서만** 옵니다.

$$
\text{ratio\_se} = \frac{\text{손해 표준오차}}{\text{예측 보험료}}
$$

분모인 보험료는 *알려진 노출*이라 표준오차가 없습니다 (2.3). 위험보험료는
요율에 유지계약을 곱해 배정된 값이라, 그 발전계수에 분산을 매기는 것은 실체
없는 인공 오차가 됩니다. 그래서 밴드는 오직 손해 예측의 불확실성만 담습니다.
예측 불확실성을 과정오차·모수오차로 분해하고 부트스트랩으로 검증하는 이야기는
{doc}`예측 불확실성 <05-uncertainty>`에서 다룹니다.

## 2.7 함께 보기

- {doc}`코호트 보정 — Credible·Smooth <04-projection>`: 완전 풀링을 넘어, 각
  코호트의 수준을 신뢰도로 보정(`CredibleLoss`)하거나 형상을 평활(`SmoothLoss`)한다.
- {doc}`예측 불확실성 <05-uncertainty>`: 이 장의 밴드를 과정오차·모수오차로
  분해하고 부트스트랩으로 검증한다.
- {doc}`예측 검증 <07-backtest>`: 방법 선택을 데이터로 가리는 백테스트.
- 부록 — {doc}`ATA 인자 <02-ata>`·{doc}`강도 <03-intensity>`: `PooledLoss`가
  손해를 채우는 데 쓰는 강도 `g_k`의 정의와 진단.
- {doc}`API 레퍼런스 <../api>`의 `PooledLoss`, `PooledPremium`, `Ratio`.
