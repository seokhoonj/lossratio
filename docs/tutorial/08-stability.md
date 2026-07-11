# 8장. 안정성 — 예측 손해율을 언제부터 믿을까

```{admonition} 이 장에서 배우는 것
:class: tip

- 관측 경과 너머로 손해율을 연장할 때 "언제부터 믿을 만한가"라는 물음과 **안정성(stability)**
- 손해율의 한 칸 진전 `rho_k = f^L_k / f^P_k`로 정착을 직접 재는 게이트
- `Stability().assess()`로 `stable` / `developing` / `insufficient_depth` 판정 읽기
- `RatioFit.extend(horizon=)`로 안정되면 동결 연장, 아니면 정직하게 보류
- 문턱 `tol`과 그 조절
```

2장은 빈 칸을 채워 예측 손해율을 냈고, 4장은 그 불확실성을, 7장은 검증을
다뤘습니다. 그런데 관측 경과의 *끝* 너머로 손해율을 더 연장하려 할 때 실무의
물음이 하나 남습니다 —

> "이 손해율, 관측이 끝난 다음에도 이 값으로 쭉 간다고 봐도 되나? 아니면
> 아직 움직이는 중이라 못 믿나?"

## 8.1 안정성이란

장기건강보험은 손해도 보험료도 계속 쌓입니다 — *도달할 최종값*이 없습니다.
분모(보험료)가 고정된 방식이라면 분자만 유한한 값으로 모여 손해율이 한 점에
수렴하겠지만, 여기선 **분모도 함께 쌓여** 어느 쪽도 닫히지 않습니다.
그래서 관측이 끝난 경과 너머의 손해율은 *정교한 꼬리(tail) 곡선*으로 외삽하는 게
아니라, **진전이 이미 정착했다면 마지막 손해율을 그대로 평탄 연장**하는 것이
정직합니다(실데이터 표본 외 검증의 결론). 단 **정착했을 때만** 그렇습니다.

정착했는지는 눈으로 가리기 어렵습니다. 1장에서 본 **관성(inertia)** — 누적
손해율이 분모효과에 눌려 굼뜨게 움직이는 성질 — 때문에, 누적값은 *아직 진전
중인데도* 평평해 보일 수 있기 때문입니다. 안정성 판정은 그 관성에 속지 않게,
손해율의 *한 칸 진전*을 직접 봅니다.

## 8.2 어떻게 판정하나 — 손해율의 한 칸 진전

손해율 $R_k = L_k / P_k$가 평평하다는 것은, **분자(손해)와 분모(보험료)가 같은
속도로 진전한다**는 뜻입니다. 그래서 한 칸 진전을 손해 link 인자와 보험료
link 인자의 비로 봅니다.

$$\rho_k = \frac{f^L_k}{f^P_k},\qquad f^L_k = \frac{\sum_i L_{i,k+1}}{\sum_i L_{i,k}},\quad
f^P_k = \frac{\sum_i P_{i,k+1}}{\sum_i P_{i,k}}$$

손해와 보험료가 발맞춰 늘면 $\rho_k \to 1$ — 손해율이 평평합니다. 최근
`window`개 경과의 $|\rho_k - 1|$이 모두 문턱 `tol` 아래면 **안정(stable)**,
아니면 **진전 중(developing)**, 경과가 너무 얕으면 **`insufficient_depth`**
입니다.

```{admonition} 왜 손해율이 손해·보험료보다 안정한가
:class: note

손해율은 손해·보험료 *각각*보다 훨씬 안정합니다. 둘이 함께 진전하므로
**분자와 분모의 변동이 비에서 대부분 상쇄**되기 때문입니다. 보험료의 불확실성이
이렇게 상쇄되므로, 4장에서 손해율 밴드가 보험료를 *알려진 값*으로 두는 것이
정당해집니다. 그래서 관측 너머 go-forward도, 손해·보험료를 따로
외삽해 나누기보다 **손해율을 통째로 동결**하는 편이 실데이터에서 더
정확합니다.
```

옛 누적값을 다시 적합해 가며 보는 대신, 이렇게 **관측된 진전을 직접** 재므로
판정이 가볍고, "관측은 평평해 보여도 진전은 안 멈췄다"를 정직하게 가립니다.

실제 숫자로 보겠습니다. 수술담보의 cohort x duration 누적 행렬에서, 각 link
$k \to k+1$의 $f^L_k$ / $f^P_k$를 *그 두 경과에 모두 관측된 코호트*만 모아
($\geq 3$개) 구한 뒤 $\rho_k = f^L_k / f^P_k$를 봅니다. 가장 최근 6개 link이
판정 창입니다.

```python
import numpy as np
import polars as pl
import lossratio as lr

tri = lr.Triangle(lr.load_experience(), groups="coverage", grain="M")
sur = tri.to_polars().filter(pl.col("coverage") == "SURGERY")
Lm = sur.pivot(values="loss", index="cohort", on="duration", aggregate_function="first").sort("cohort")
Pm = sur.pivot(values="premium", index="cohort", on="duration", aggregate_function="first").sort("cohort")
durs = sorted(int(c) for c in Lm.columns if c != "cohort")
L = Lm.select([str(d) for d in durs]).to_numpy()
P = Pm.select([str(d) for d in durs]).to_numpy()

rows = []
for k in range(len(durs) - 1):
    both = np.isfinite(L[:, k]) & np.isfinite(L[:, k + 1]) & np.isfinite(P[:, k]) & np.isfinite(P[:, k + 1])
    if int(both.sum()) < 3:                       # min_cohorts=3 미만 link 제외
        continue
    fL = L[both, k + 1].sum() / L[both, k].sum()
    fP = P[both, k + 1].sum() / P[both, k].sum()
    rows.append((f"{durs[k]}->{durs[k+1]}", fL, fP, fL / fP, abs(fL / fP - 1)))

usable = pl.DataFrame(rows, schema=["link", "f_L", "f_P", "rho", "abs_dev"], orient="row")
recent = usable.tail(6)                            # window=6
print(recent.with_columns(pl.col(["f_L", "f_P", "rho", "abs_dev"]).round(5)))
print("recent_drift =", round(recent["abs_dev"].max(), 6))
#> ┌────────┬─────────┬─────────┬─────────┬─────────┐
#> │ link   ┆ f_L     ┆ f_P     ┆ rho     ┆ abs_dev │
#> ╞════════╪═════════╪═════════╪═════════╪═════════╡
#> │ 28->29 ┆ 1.03706 ┆ 1.03566 ┆ 1.00135 ┆ 0.00135 │
#> │ 29->30 ┆ 1.03477 ┆ 1.03436 ┆ 1.00041 ┆ 0.00041 │
#> │ 30->31 ┆ 1.03234 ┆ 1.03303 ┆ 0.99934 ┆ 0.00066 │
#> │ 31->32 ┆ 1.03306 ┆ 1.03221 ┆ 1.00082 ┆ 0.00082 │
#> │ 32->33 ┆ 1.03415 ┆ 1.03135 ┆ 1.00272 ┆ 0.00272 │
#> │ 33->34 ┆ 1.02779 ┆ 1.03007 ┆ 0.99778 ┆ 0.00222 │
#> └────────┴─────────┴─────────┴─────────┴─────────┘
#> recent_drift = 0.002715
```

깊은 경과에서 손해와 보험료가 거의 같은 속도로 진전합니다 — 예컨대 마지막
link `33->34`은 $f^L = 1.0278$, $f^P = 1.0301$로 손해율을 0.2%밖에 못
움직입니다($\rho = 0.998$). 6개 창의 최댓값 $\max|\rho_k - 1| = 0.0027$이 바로
`recent_drift`이고, 문턱 0.01 아래라 수술담보는 `stable`이 됩니다.

## 8.3 패키지로 확인하기

`Stability().assess(triangle)`가 담보별 판정을 냅니다.

```python
import polars as pl
import lossratio as lr

tri = lr.Triangle(lr.load_experience(), groups="coverage", grain="M")
rep = lr.Stability().assess(tri)
rep.to_polars().select(
    ["coverage", "frontier_duration", "frontier_ratio",
     "stable", "recent_drift", "stable_from_duration", "status"]
)
#> shape: (4, 7)
#> ┌───────────┬───────────────────┬────────────────┬────────┬──────────────┬──────────────────────┬────────────┐
#> │ coverage  ┆ frontier_duration ┆ frontier_ratio ┆ stable ┆ recent_drift ┆ stable_from_duration ┆ status     │
#> ╞═══════════╪═══════════════════╪════════════════╪════════╪══════════════╪══════════════════════╪════════════╡
#> │ CANCER    ┆ 36                ┆ 0.866135       ┆ false  ┆ 0.012678     ┆ null                 ┆ developing │
#> │ CI        ┆ 36                ┆ 0.90022        ┆ false  ┆ 0.010744     ┆ null                 ┆ developing │
#> │ INPATIENT ┆ 36                ┆ 0.416592       ┆ true   ┆ 0.002715     ┆ 9                    ┆ stable     │
#> │ SURGERY   ┆ 36                ┆ 1.509565       ┆ true   ┆ 0.002715     ┆ 9                    ┆ stable     │
#> └───────────┴───────────────────┴────────────────┴────────┴──────────────┴──────────────────────┴────────────┘
```

수술·입원담보는 `recent_drift`(최근 창의 $\max|\rho_k-1|$)가 0.003으로 문턱
0.01 아래라 **`stable`** — 경과 9부터 정착(`stable_from_duration`)했고,
관측 끝(경과 36)의 손해율 `frontier_ratio`를 그 너머로 동결해도 됩니다. 암·CI
담보는 drift가 0.011~0.013으로 문턱을 넘어 **`developing`** — 아직 손해율이
움직이는 중이라 동결할 자격이 없습니다.

`frontier_ratio`는 관측 끝(프런티어)의 누적 손해율 그 자체 — 정착했다면 그
너머로 *동결해 연장할* 값입니다. 담보마다 수준이 다른 게 자연스럽습니다:
수술담보는 1.51(손해가 보험료를 넘는 적자 담보), 입원담보는 0.42(흑자 담보).
둘 다 정착했으므로 이 값들이 각각의 go-forward 동결 수준이 됩니다. 동결할
값만 골라 보려면 `frozen_ratio()`가 — `stable`인 담보는 `frontier_ratio`를,
아닌 담보는 `null`을 — 돌려줍니다.

```python
rep.frozen_ratio()
#> ┌───────────┬───────────────────┬──────────────────┬────────────┐
#> │ coverage  ┆ frontier_duration ┆ go_forward_ratio ┆ status     │
#> ╞═══════════╪═══════════════════╪══════════════════╪════════════╡
#> │ CANCER    ┆ 36                ┆ null             ┆ developing │
#> │ CI        ┆ 36                ┆ null             ┆ developing │
#> │ INPATIENT ┆ 36                ┆ 0.416592         ┆ stable     │
#> │ SURGERY   ┆ 36                ┆ 1.509565         ┆ stable     │
#> └───────────┴───────────────────┴──────────────────┴────────────┘
```

판정에는 충분한 link 깊이가 필요합니다. `min_links`(기본 8)보다 얕은 담보는
*판정 자체를 보류*하고 `insufficient_depth`로 답합니다 — 근거가 모자라면
`stable`도 `developing`도 단언하지 않습니다. 관측보다 깊은 깊이를 요구하면
모두 보류로 떨어집니다.

```python
lr.Stability(min_links=40).assess(tri).to_polars().select(
    ["coverage", "stable", "recent_drift", "status"]
)
#> ┌───────────┬────────┬──────────────┬────────────────────┐
#> │ coverage  ┆ stable ┆ recent_drift ┆ status             │
#> ╞═══════════╪════════╪══════════════╪════════════════════╡
#> │ CANCER    ┆ null   ┆ null         ┆ insufficient_depth │
#> │ CI        ┆ null   ┆ null         ┆ insufficient_depth │
#> │ INPATIENT ┆ null   ┆ null         ┆ insufficient_depth │
#> │ SURGERY   ┆ null   ┆ null         ┆ insufficient_depth │
#> └───────────┴────────┴──────────────┴────────────────────┘
```

`min_cohorts`(기본 3)도 같은 방향입니다 — 한 link에 코호트가 그만큼 모여야
그 link을 셉니다. 너무 많이 요구하면(`min_cohorts=200`) 셀 link이 없어 역시
`insufficient_depth`가 됩니다.

## 8.4 진전이 정착했는지 보기

누적 손해율 $R_k$의 경과별 궤적을 보면, 안정 담보는 끝에서 평평해지고 진전
중인 담보는 아직 오릅니다.

```{eval-rst}
.. plot::
   :context: close-figs
   :caption: 담보별 누적 손해율의 경과 궤적(월 단위). 수술·입원은 끝에서 평평(stable), 암·CI는 아직 상승(developing). 안정성 게이트는 이 끝부분의 한 칸 변화율을 본다.

   import polars as pl
   import lossratio as lr
   import matplotlib.pyplot as plt

   tri = lr.Triangle(lr.load_experience(), groups="coverage", grain="M")
   traj = (tri.to_polars()
             .group_by(["coverage", "duration"])
             .agg((pl.col("loss").sum() / pl.col("premium").sum()).alias("R"))
             .sort(["coverage", "duration"]))
   rep = lr.Stability().assess(tri).to_polars()
   stable = dict(zip(rep["coverage"].to_list(), rep["stable"].to_list()))

   fig, ax = plt.subplots(figsize=(6.4, 3.8))
   for cov in ["SURGERY", "INPATIENT", "CANCER", "CI"]:
       sub = traj.filter(pl.col("coverage") == cov)
       ls = "-" if stable[cov] else "--"
       lab = f"{cov} ({'stable' if stable[cov] else 'developing'})"
       ax.plot(sub["duration"].to_list(), sub["R"].to_list(), ls, label=lab)
   ax.set_xlabel("duration (months)")
   ax.set_ylabel("cumulative loss ratio")
   ax.legend(fontsize=8)
```

실선(stable)은 끝에서 잔잔하고, 점선(developing)은 끝까지 기울어 있습니다.
게이트가 보는 건 *전체 모양*이 아니라 이 **끝부분의 한 칸 변화율**입니다 —
거기가 잔잔해야 동결을 신뢰합니다.

## 8.5 안정되면 동결 연장 — `extend`

판정이 나면 `RatioFit.extend(horizon=)`로 손해율을 목표 경과까지 내밉니다.
안정 담보는 마지막 손해율을 평탄 동결(`frozen`), 진전 중인 담보는 값을 비우고
`uncertain`으로 표시합니다 — *아직 안 정해진* 값을 지어내지 않습니다.

```python
fit = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(tri)
ext = fit.extend(horizon=48)
ext.group_by(["coverage", "status"]).len().sort(["coverage", "status"])
#> CANCER    / projected 1296,  uncertain 432    (developing -> 값 비움)
#> CI        / projected 1296,  uncertain 432
#> INPATIENT / frozen 432,      projected 1296   (stable -> 마지막 값 동결)
#> SURGERY   / frozen 432,      projected 1296
```

관측 프런티어(경과 36)까지는 `projected`(모델 예측), 그 너머는 — 수술·입원은
`frozen`(마지막 손해율 평탄 연장), 암·CI는 `uncertain`(값 `null`)입니다.
`amounts=True`를 주면 손해율은 동결하되 보험료는 계속 납입되므로 금액
(`loss` / `premium`)도 함께 내밉니다 — 손해율은 평탄, 보험료는 최근 성장률로
이어가고, `loss = ratio x premium`로 맞춥니다.

```python
ext_amt = fit.extend(horizon=48, amounts=True)
ext_amt.filter((pl.col("coverage") == "SURGERY") & (pl.col("status") == "frozen")).select(
    "duration",
    pl.col("loss").round(0).cast(pl.Int64),
    pl.col("premium").round(0).cast(pl.Int64),
    "ratio",
).head(3)
#> ┌──────────┬──────────┬──────────┬──────────┐
#> │ duration ┆ loss     ┆ premium  ┆ ratio    │
#> ╞══════════╪══════════╪══════════╪══════════╡
#> │ 37       ┆ 17262763 ┆ 11435591 ┆ 1.509565 │
#> │ 38       ┆ 17795580 ┆ 11788551 ┆ 1.509565 │
#> │ 39       ┆ 18344842 ┆ 12152406 ┆ 1.509565 │
#> └──────────┴──────────┴──────────┴──────────┘
```

`extend`는 같은 `window` / `tol`로 안정성 게이트를 *자기 안에서 다시 돌립니다*.
그래서 `tol`을 `recent_drift`(0.0027) 아래로 조이면 수술·입원도 동결 자격을
잃고, 모든 담보의 연장 구간이 `uncertain`이 됩니다.

```python
fit.extend(horizon=48, tol=0.002).group_by(["coverage", "status"]).len().sort(
    ["coverage", "status"]
)
#> CANCER    / projected 1296,  uncertain 432
#> CI        / projected 1296,  uncertain 432
#> INPATIENT / projected 1296,  uncertain 432   (tol=0.002 < drift -> 동결 철회)
#> SURGERY   / projected 1296,  uncertain 432
```

## 8.6 기준 조절 — 무엇을 "안정"이라 부를까

"얼마나 안 움직여야 믿겠는가"는 분석자가 정합니다. 기본 `tol=0.01`(기간당
1%)은 실데이터로 보정한 값입니다 — 4개 포트폴리오, 약 2,000개의 segment-by-cut
표본에서 판정을 *12개월 표본 외(out-of-sample) 동결 오차*(허용 < 5%)에 맞춰
보정했을 때, 이 문턱이 정밀도(precision) 약 95%를 냅니다. 즉 게이트가 `stable`
이라 부른 담보를 실제로 동결해 보면 약 95%가 잘 맞습니다. 대신 동결 가능한
경우의 절반쯤만 잡아내는(recall ~50%) 보수적 설정입니다. 0.012까지 늦추면
정밀도 90% 이상을 유지하며 더 많이 잡지만, 0.015를 넘기면 정밀도가 떨어집니다
(0.02 -> 약 74%). 더 조이면 더 보수적이 됩니다.

```python
# 더 엄격하게 (drift 문턱을 절반으로)
lr.Stability(tol=0.005).assess(tri).to_polars().select(["coverage", "stable", "status"])
#> 입원담보는 여전히 stable, 수술담보도 stable(drift 0.003 < 0.005);
#> 문턱을 더 조이면 그만큼 stable 담보가 줄어든다
```

문턱을 조이면 "더 확실해질 때까지 기다리는" 셈이라, 그만큼 정착하지 않은
담보는 `developing`으로 **보류**됩니다 — 근거가 부족하면 판정하지 않고 물러나는,
이 패키지 진단들의 일관된 보수적 태도입니다.

한 가지 분명히 해 둘 점: 안정성은 *관측된* 진전이 멈췄는지만 잴 수 있을 뿐,
관측 너머의 깊은 꼬리(관측이 끝난 경과 이후의 미관측 진전) 자체를
측정하지는 못합니다 — 정착이 미래에 뒤집힐지(트렌드·선택 효과·regime 변화)는
데이터 밖의 문제이며, 그래서 `stable`도 "최종값에 도달했다"가 아니라 "지금까지의
관측이 멈췄다"는 경험적 진술입니다.

## 8.7 실무에서 — 진전 중이 흔하고, 그게 정직함

위 합성 데이터는 수술·입원이 깨끗하게 안정됐지만, **실무 데이터에선
`developing`이 훨씬 흔합니다.** 특히 암·2대진단처럼 클레임이 드물고
크며(lumpy) 손해가 오랜 경과에 걸쳐 이어지는 담보일수록 그렇습니다. 이는
결함이 아니라 **설계된 보수성**입니다.

```{admonition} 관성에 속지 않는다 — 안정성의 본래 가치
:class: important

1장에서 **분모효과·관성** 때문에 누적 손해율이 굼떠 보인다고 했습니다.
lumpy 담보도 *관측* 누적 손해율은 평평해 보이죠. 그런데 그건 분모가 커져
움직임을 가린 것뿐, 손해율은 아직 진전 중일 수 있습니다. 안정성은 누적값이
아니라 **한 칸 진전 $\rho_k$**를 보므로 이 관성에 속지 않습니다 — "관측은
안정처럼 보여도 진전은 안 멈췄다"를 정직하게 `developing`으로 답합니다.
```

`developing`이라고 점추정이 *안 나오는* 게 아닙니다. 손해율 예측값은 늘
나옵니다(관측 프런티어까지). 다만 그 너머로 *동결해 연장할* 자격이 아직 없을
뿐입니다. 이럴 때는 안정성(*언제* 믿나) 대신 **4장의 불확실성(CI·CV,
*얼마나* 믿나)** 으로 판단합니다 — 진전 중인 담보일수록 CI가 넓게 나와 그
불확실성을 그대로 드러냅니다. 안정성(언제)과 불확실성(얼마나)은 짝이고,
안정성이 침묵할 때 불확실성이 답합니다.

## 8.8 함께 보기

- {doc}`2장 — 손해율 예측 <02-projection>`: 안정성이 신뢰하는 그 예측을 세우는
  강도 사다리(Pooled·Credible·Smooth)와 벤치마크 ChainLadder.
- {doc}`4장 — 불확실성 <05-uncertainty>`: 안정성이 침묵(developing)할 때 답하는
  CI·CV.
- {doc}`API 레퍼런스 <../api>`의 `Stability`, `StabilityReport`(`frozen_ratio`),
  `RatioFit.extend`.
