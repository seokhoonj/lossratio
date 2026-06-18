---
orphan: true
---

# 불확실성을 어떻게 계산하나 — 해석적 밴드 vs ResidualBootstrap

5장은 불확실성의 두 축(과정오차·모수오차)과 그 출력을 *읽는* 법을
다뤘습니다. 이 레시피는 그 한 단계 아래 — lossratio가 그 불확실성을
**어떻게 계산하는가**를 3x3 삼각형 손계산으로 따라갑니다. 방법을 고르는 실무
기준도 마지막에 정리합니다.

## 1. 두 오차 — 무엇을 잡아야 하나

모든 불확실성 계산은 두 오차를 함께 잡아야 합니다.

- **모수오차**(parameter error) — 인자(`f_k`·`g_k`)를 데이터로 *추정*했으니
  그 추정이 틀릴 수 있다.
- **과정오차**(process error) — 미래 셀 자체가 *확률적*이라 평균 주위로
  흩어진다.

## 2. 두 갈래 — 해석적(기본) vs ResidualBootstrap

lossratio는 두 가지로 이 둘을 계산합니다.

```{list-table}
:header-rows: 1
:widths: 26 18 56

* - 방법
  - 재적합?
  - 무엇
* - 해석적(기본)
  - 아니오 (닫힌형)
  - 링크비·완전 풀링 강도의 해석식으로 `loss_proc_se` / `loss_param_se` /
    `loss_total_se`를 바로 채움. 인자를 추가로 줄 필요가 없는 기본 출력.
* - `ResidualBootstrap`
  - 예
  - 적합 잔차를 재표집해 pseudo-삼각형을 만들고 **전체 파이프라인을 재적합** —
    복제본의 퍼짐이 곧 밴드. 같은 SE/CI 컬럼을 재표집 스프레드로 덮어쓰고,
    경험적 분위수 예측밴드를 함께 냄.
```

`PooledLoss`(완전 풀링)·`ChainLadder`(링크비)는 둘 다 됩니다 — 기본이
해석적, `uncertainty=lr.ResidualBootstrap(...)`로 재표집 대안. 반면
`CredibleLoss`·`SmoothLoss`는 **해석적 SE가 없습니다**(신뢰도 레벨·평활 형상의
추정 분산이 해석적 재귀를 깨므로) — 그 둘은 ResidualBootstrap이 *유일한* 밴드입니다.

## 3. 손계산 예제 — 3x3 삼각형

두 방법을 같은 토대 위에서 비교하기 위해 작은 누적손해 삼각형을 씁니다.

```text
누적손해      duration1   duration2   duration3
cohort1       100    150    165      <- 끝까지 관측
cohort2       150    240     ?
cohort3       200     ?      ?
```

링크비(link-ratio `f_k`) 적합 — `ChainLadder`:

- **인자**: $f_1 = (150+240)/(100+150) = 1.56$, $f_2 = 165/150 = 1.10$
- **예측**: c2.duration3 $= 240 \times 1.10 = 264$, c3.duration2 $= 200 \times 1.56 = 312$,
  c3.duration3 $= 312 \times 1.10 = 343.2$
- **관측 끝까지의 예측**: $[165,\ 264,\ 343.2]$, 합 $= 772.2$

772.2는 *점추정 하나*입니다. 두 방법은 "이 값이 얼마나 흔들리나"를 서로 다르게
계산합니다.

### 3.1 해석적 — 분산 분해(alpha = 1)

데이터를 건드리지 않고 닫힌형으로 분해합니다.

- $\sigma^2_1 = 0.60$, $\operatorname{Var}(\hat f_1) = \sigma^2_1 / \sum_i C_{i,1}
  = 0.60/250 = 0.0024$, $\operatorname{SE}(f_1) = 0.049$
- duration2는 링크가 하나뿐이라 $\sigma^2_2$를 직접 못 구함 -> LOCF로
  $\sigma^2_2 = 0.60$, $\operatorname{Var}(\hat f_2) = 0.60/150 = 0.004$
- 이 인자 분산을 해석적 재귀로 미래 셀까지 전파하면 모수오차 SE가, 셀 분산
  $\sigma^2_k C_k$를 더하면 과정오차 SE가, 둘의 제곱합 제곱근이 총 SE가 됩니다.

이게 `uncertainty=` 없이 `lr.ChainLadder().fit(tri)`가 채우는 `loss_proc_se`
/ `loss_param_se` / `loss_total_se`입니다.

### 3.2 `ResidualBootstrap` — 잔차를 재표집해 재적합

같은 두 오차를, 인자 분산식 대신 *재표집*으로 잡습니다. 표준화 잔차 풀
(duration1): cohort1 $= -0.7746$, cohort2 $= +0.6325$.

- **Stage 1 (모수오차)**: 각 관측 링크마다 풀에서 잔차를 복원추출해 pseudo
  누적값을 재구성하고, 그 pseudo 과거삼각형에 `ChainLadder`를 **재적합**:

  $$C^\ast_{i,k+1} = f_k C_{i,k} + r^\ast \cdot \sigma_k\sqrt{C_{i,k}}$$

  예: cohort1 duration1->2에서 $r^\ast=+0.6325$ -> $1.56\times100 +
  0.6325\times0.7746\times10 = 160.9$. 복제본마다 다른 $f^\ast_k$가 나옴.
- **Stage 2 (과정오차)**: 재적합 인자로 미래 평균을 예측한 뒤, 각 미래 셀에
  과대분산 process 노이즈를 더해 예측 draw를 만듦.

복제본별 예측의 퍼짐(스프레드)이 곧 SE이고, 예측 draw의 경험적 분위수가 CI
밴드입니다(점추정에 재중심). 난수 원천이 가정분포가 아니라 *실제 잔차의
경험적 재표집*이라 분포에 무가정입니다.

## 4. 언제 무엇을 — 실무 가이드

```python
import lossratio as lr

tri = lr.Triangle(df, groups="coverage")

# 닫힌형 SE/CI (기본, 가장 싸고 안정적)
fit = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(tri)

# 재표집 밴드 (CredibleLoss/SmoothLoss는 이게 유일한 밴드)
fit = lr.Ratio(
    loss=lr.CredibleLoss(uncertainty=lr.ResidualBootstrap(n_replicates=999, seed=1)),
    premium=lr.PooledPremium(),
).fit(tri)

fit.to_polars().select(["cohort", "duration", "ratio_proj", "ratio_se",
                        "ratio_ci_lo", "ratio_ci_hi"])
```

방법 선택:

- **밴드(SE/CI)만 필요하고 모델이 `PooledLoss`/`ChainLadder`** -> 기본(해석적).
  가장 싸고 안정적이며 점추정과 정합.
- **`CredibleLoss`/`SmoothLoss`를 쓰거나, 예측분포 밴드가 필요** ->
  `uncertainty=lr.ResidualBootstrap(...)`. 신뢰도 레벨·평활 형상의 추정 분산은
  닫힌형이 성립하지 않아 부트스트랩이 유일한 밴드입니다.

```{important}
부트스트랩의 밴드는 손해 측 estimator를 그대로 따라갑니다 — `PooledLoss`면
완전 풀링 강도(`g_k`) 부트스트랩, `ChainLadder`면 링크비(England-Verrall ODP)
부트스트랩. 그래서 분포의
중심이 점추정과 같은 예측에서 나옵니다(점추정에 밴드가 어긋나는 mismatch 없음).
포트폴리오 손해율 불확실성에는 셀 단위 해석적 SE를 단순 합산하지 말고
부트스트랩을 쓰십시오 — 코호트들이 같은 인자를 공유하는 상관(parameter
correlation)을 부트스트랩만이 제대로 잡습니다.
```

## 5. 함께 보기

- {doc}`5장 — 예측의 불확실성 <../tutorial/05-uncertainty>` — 과정오차·모수오차의
  정의와 SE/CV/CI 읽는 법.
- {doc}`API 레퍼런스 <../api>`의 `ResidualBootstrap`.
