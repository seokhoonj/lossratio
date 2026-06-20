# 8장. 안정성 — 예측 손해율을 언제부터 믿을까

```{admonition} 이 장에서 배우는 것
:class: tip

- 관측 경과 너머로 손해율을 내밀 때 "언제부터 믿을 만한가"라는 물음과 **안정성(stability)**
- 손해율의 한 칸 진전 `rho_k = f^L_k / f^P_k`로 정착을 직접 재는 게이트
- `Stability().assess()`로 `stable` / `developing` / `insufficient_depth` 판정 읽기
- `RatioFit.extend(horizon=)`로 안정되면 동결 연장, 아니면 정직하게 보류
- 문턱 `tol`과 그 조절
```

이 장은 전체 흐름의 **마지막 진단** — 세운 예측을 *관측 너머로 얼마나 믿고
내밀어도 되는지*를 가리는 자리입니다.

```{mermaid}
flowchart LR
  A["경험 데이터"] --> B["Triangle<br/>삼각형 (1장)"]
  B --> C["link 진단 (2·3장)"]
  C --> D["예측 (4장)"]
  D --> E["불확실성 (5장)"]
  E --> F["regime · 백테스트 (6·7장)"]
  F --> G["안정성<br/>(이번 장)"]
  classDef data fill:#dceaf6,stroke:#4a7ba6,color:#16344e
  classDef estimate fill:#eaf1f8,stroke:#6f8ca3,color:#22313c
  classDef validate fill:#e3f0e9,stroke:#5a9b86,color:#1c3a2e
  classDef current fill:#ffe3a0,stroke:#cf9b00,color:#4a3800,stroke-width:2.5px
  class A data
  class B,C,D estimate
  class E,F validate
  class G current
```

4장은 빈 칸을 채워 예측 손해율을 냈고, 5장은 그 불확실성을, 7장은 검증을
다뤘습니다. 그런데 관측 경과의 *끝* 너머로 손해율을 더 내밀려 할 때 실무의
물음이 하나 남습니다 —

> "이 손해율, 관측이 끝난 다음에도 이 값으로 쭉 간다고 봐도 되나? 아니면
> 아직 움직이는 중이라 못 믿나?"

## 8.1 안정성이란

장기건강보험은 손해도 보험료도 계속 쌓입니다 — *도달할 최종값*이 없습니다.
분모(보험료)가 고정된 방식이라면 분자만 유한한 값으로 모여 손해율이 한 점에
수렴하겠지만, 여기선 **분모도 함께 쌓여** 어느 쪽도 닫히지 않습니다.
그래서 관측이 끝난 경과 너머의 손해율은 *정교한 꼬리 곡선*으로 외삽하는 게
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
이렇게 상쇄되므로, 5장의 `se_method="fixed"`가 보험료를 *known*으로 두는 것이
정당해집니다. 그래서 관측 너머 go-forward도, 손해·보험료를 따로
외삽해 나누기보다 **손해율을 통째로 동결**하는 편이 실데이터에서 더
정확합니다.
```

옛 누적값을 다시 적합해 가며 보는 대신, 이렇게 **관측된 진전을 직접** 재므로
판정이 가볍고, "관측은 평평해 보여도 진전은 안 멈췄다"를 정직하게 가립니다.

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
#> │ CANCER    ┆ 36                ┆ 0.866128       ┆ false  ┆ 0.012679     ┆ null                 ┆ developing │
#> │ CI        ┆ 36                ┆ 0.900219       ┆ false  ┆ 0.010744     ┆ null                 ┆ developing │
#> │ INPATIENT ┆ 36                ┆ 0.416596       ┆ true   ┆ 0.002715     ┆ 9                    ┆ stable     │
#> │ SURGERY   ┆ 36                ┆ 1.509562       ┆ true   ┆ 0.002716     ┆ 9                    ┆ stable     │
#> └───────────┴───────────────────┴────────────────┴────────┴──────────────┴──────────────────────┴────────────┘
```

수술·입원담보는 `recent_drift`(최근 창의 $\max|\rho_k-1|$)가 0.003으로 문턱
0.01 아래라 **`stable`** — 경과 9부터 정착(`stable_from_duration`)했고,
관측 끝(경과 36)의 손해율 `frontier_ratio`를 그 너머로 동결해도 됩니다. 암·CI
담보는 drift가 0.011~0.013으로 문턱을 넘어 **`developing`** — 아직 손해율이
움직이는 중이라 동결할 자격이 없습니다.

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
   df = lr.PooledLoss().fit(tri).to_polars().filter(pl.col("source") == "observed")
   traj = (df.group_by(["coverage", "duration"])
             .agg((pl.col("loss_obs").sum() / pl.col("premium_obs").sum()).alias("R"))
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
(`loss` / `premium`)도 함께 내밉니다.

## 8.6 기준 조절 — 무엇을 "안정"이라 부를까

"얼마나 안 움직여야 믿겠는가"는 분석자가 정합니다. 기본 `tol=0.01`(기간당
1%)은 실데이터로 보정한 값입니다 — 그 문턱에서 `stable` 판정의 약 95%가 실제로
동결이 잘 맞습니다(보수적: 동결 가능한 담보의 절반만 잡음). 더 조이면 더
보수적이 됩니다.

```python
# 더 엄격하게 (drift 문턱을 절반으로)
lr.Stability(tol=0.005).assess(tri).to_polars().select(["coverage", "stable", "status"])
#> 입원담보는 여전히 stable, 수술담보도 stable(drift 0.003 < 0.005);
#> 문턱을 더 조이면 그만큼 stable 담보가 줄어든다
```

문턱을 조이면 "더 확실해질 때까지 기다리는" 셈이라, 그만큼 정착하지 않은
담보는 `developing`으로 **보류**됩니다 — 근거가 얇으면 판정하지 않고 물러나는,
이 패키지 진단들의 일관된 보수적 태도입니다.

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
나옵니다(관측 프런티어까지). 다만 그 너머로 *동결해 내밀* 자격이 아직 없을
뿐입니다. 이럴 때는 안정성(*언제* 믿나) 대신 **5장의 불확실성(CI·CV,
*얼마나* 믿나)** 으로 판단합니다 — 진전 중인 담보일수록 CI가 넓게 나와 그
불확실성을 그대로 드러냅니다. 안정성(언제)과 불확실성(얼마나)은 짝이고,
안정성이 침묵할 때 불확실성이 답합니다.

## 8.8 함께 보기

- {doc}`4장 — 손해율 예측 <04-projection>`: 안정성이 신뢰하는 그 예측을 세우는
  강도 사다리(Pooled·Credible·Smooth)와 벤치마크 ChainLadder.
- {doc}`5장 — 불확실성 <05-uncertainty>`: 안정성이 침묵(developing)할 때 답하는
  CI·CV.
- {doc}`API 레퍼런스 <../api>`의 `Stability`, `StabilityReport`, `RatioFit.extend`.
