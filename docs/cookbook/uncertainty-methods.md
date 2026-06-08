---
orphan: true
---

# 불확실성 추정의 세 갈래 — Analytical · ParametricBootstrap · ResidualBootstrap

5장은 불확실성의 두 축(과정오차·모수오차)과 그 출력을 *읽는* 법을
다뤘습니다. 이 레시피는 그 한 단계 아래 — lossratio가 그 불확실성을
**어떻게 계산하는가**, 그리고 세 가지 방법이 무엇이 같고 무엇이 다른가를
3x3 삼각형 손계산으로 끝까지 따라갑니다. 방법을 고르는 실무 기준도 마지막에
정리합니다.

## 1. 두 갈래의 질문 — 무엇을 뽑나 vs 어떻게 뽑나

모든 불확실성 방법은 두 오차를 함께 잡아야 합니다.

- **모수오차**(parameter error) — 인자(`f_k`·`g_k`)를 데이터로 *추정*했으니
  그 추정이 틀릴 수 있다.
- **과정오차**(process error) — 미래 셀 자체가 *확률적*이라 평균 주위로
  흩어진다.

방법을 가르는 축은 **"그 불확실성을 어떻게 만들어내나"** 입니다. 전체 지형은
하나의 트리로 정리됩니다.

```text
불확실성
├── Analytical (닫힌형)              <- Monte Carlo 아님
└── Monte Carlo (시뮬레이션)
    ├── 모수 시뮬레이션 (인자 draw)    <- "시뮬레이션된 Analytical"
    │                                  부트스트랩 아님 (재적합 없이 인자 직접 draw)
    └── Bootstrap (pseudo-data 재적합)
        ├── Parametric   (gamma 셀 draw)  -> ParametricBootstrap
        └── Nonparametric(잔차 재표집)     -> ResidualBootstrap
```

즉 **Monte Carlo(시뮬레이션) = 모수 시뮬레이션 + 두 부트스트랩**이고,
**Bootstrap = parametric + nonparametric**(재적합하는 둘)으로 그 부분집합입니다.
닫힌형인 Analytical만 시뮬레이션이 아닙니다.

## 2. 세 공개 클래스

lossratio는 이 지형을 세 전략 클래스로 노출합니다 (모델의 `uncertainty=`
인자에 넘김).

```{list-table}
:header-rows: 1
:widths: 26 18 56

* - 클래스
  - Monte Carlo?
  - 무엇
* - `Analytical`
  - 아니오 (닫힌형)
  - Mack/ED 해석식 SE. 기본값. `simulate=True`면 시뮬레이션된 Mack 인자 draw(니치, CL 전용).
* - `ParametricBootstrap`
  - 예 (모수)
  - 셀을 gamma/ODP에서 draw + 재적합. SA의 정합 불확실성.
* - `ResidualBootstrap`
  - 예 (비모수)
  - 적합 잔차를 재표집 + 재적합. 단일 모델 CL에 정합.
```

`ParametricBootstrap`과 `ResidualBootstrap`은 공통 접미사 그대로 **모수/비모수
부트스트랩 형제**입니다. 인자-draw(트리의 "모수 시뮬레이션")는 별도 모델이
아니라 *Analytical을 시뮬레이션한 것*이라 `Analytical(simulate=True)`로
흡수됩니다 (4절 참조).

## 3. 손계산 예제 — 3x3 삼각형

세 방법을 같은 토대 위에서 비교하기 위해 작은 누적손해 삼각형을 씁니다.

```text
누적손해      duration1   duration2   duration3
cohort1       100    150    165      <- 완전 발달
cohort2       150    240     ?
cohort3       200     ?      ?
```

체인 사다리(CL) 적합:

- **인자**: $f_1 = (150+240)/(100+150) = 1.56$, $f_2 = 165/150 = 1.10$
- **예측**: c2.duration3 $= 240 \times 1.10 = 264$, c3.duration2 $= 200 \times 1.56 = 312$,
  c3.duration3 $= 312 \times 1.10 = 343.2$
- **궁극치**: $[165,\ 264,\ 343.2]$, 합 $= 772.2$

Mack 분산(alpha = 1):

- $\sigma^2_1 = 0.60$, $\operatorname{Var}(\hat f_1) = \sigma^2_1 / \sum_i C_{i,1}
  = 0.60/250 = 0.0024$, $\operatorname{SE}(f_1) = 0.049$
- duration2는 링크가 하나뿐이라 $\sigma^2_2$를 직접 못 구함 -> LOCF로
  $\sigma^2_2 = 0.60$, $\operatorname{Var}(\hat f_2) = 0.60/150 = 0.004$
- **표준화 잔차 풀**(duration1): cohort1 $= -0.7746$, cohort2 $= +0.6325$

772.2는 *점추정 하나*입니다. 세 방법은 "이 값이 얼마나 흔들리나"를 서로 다르게
시뮬레이션합니다. (아래 draw 값은 모두 *예시*입니다.)

### 3.1 `Analytical(simulate=True)` — 인자를 직접 draw

데이터를 건드리지 않고 인자를 점근정규에서 바로 뽑습니다.

- $f_1^\ast \sim N(1.56,\ 0.049^2) \to 1.60$
- $f_2^\ast \sim N(1.10,\ 0.063^2) \to 1.08$

뽑은 인자로 관측 최신 대각선을 전개 (재적합 없음):

- c2.duration3$^\ast = 240 \times 1.08 = 259.2$
- c3.duration2$^\ast = 200 \times 1.60 = 320 \to$ c3.duration3$^\ast = 320 \times 1.08 = 345.6$
- 이 replicate의 궁극 합 $= 165 + 259.2 + 345.6 = 769.8$

궁극치가 뽑은 인자들의 *곱*이라 분포가 오른쪽으로 치우칩니다(skew). "추정값을
점근분포에서 뽑아 전개"라 곧 **해석적 Mack을 시뮬레이션한 것** — 엔진은 이를
`type="analytical"`로 둡니다.

### 3.2 `ParametricBootstrap` — 셀을 모수적 분포에서 draw

2단계입니다.

- **Stage 1 (모수오차)**: 관측 증분(cohort1: 100,50,15 / cohort2: 150,90 /
  cohort3: 200)을 각각 gamma(mean = 적합 증분)에서 다시 뽑아 pseudo
  과거삼각형을 만들고 거기에 CL을 **재적합** -> $f_1^\ast=1.54$, $f_2^\ast=1.12$
  (예시).
- **Stage 2 (과정오차)**: 재적합 인자로 미래 평균 예측 후, 각 미래 셀을
  gamma(mean = 그 평균)에서 draw.

핵심: 모수오차를 *인자 직접 draw가 아니라 교란된 pseudo-삼각형에 재적합*해서
얻습니다. 난수 원천이 모수적 분포(gamma)라 `type="parametric"`.

### 3.3 `ResidualBootstrap` — 잔차를 재표집

구조는 3.2와 같고 과거 교란만 *경험적 잔차 재표집*으로 합니다. 잔차 풀은
$\{-0.7746,\ +0.6325\}$.

- **Stage 1 (모수오차)**: 각 관측 링크마다 풀에서 잔차를 복원추출해 pseudo
  누적값 재구성:

  $$C^\ast_{i,k+1} = f_k C_{i,k} + r^\ast \cdot \sigma_k\sqrt{C_{i,k}}$$

  예: cohort1 duration1->2에서 $r^\ast=+0.6325$ -> $1.56\times100 +
  0.6325\times0.7746\times10 = 160.9$. 이 pseudo 과거삼각형에 CL 재적합.
- **Stage 2 (과정오차)**: 3.2와 동일(gamma 미래 노이즈).

난수 원천이 가정분포가 아니라 *실제 잔차의 경험적 재표집*이라 분포에 무가정 ->
`type="nonparametric"`.

## 4. 3x3 비교표

```{list-table}
:header-rows: 1
:widths: 30 24 24 22

* -
  - `Analytical(simulate=True)`
  - `ParametricBootstrap`
  - `ResidualBootstrap`
* - 과거 데이터
  - 안 건드림
  - gamma로 재생성
  - 잔차 재표집으로 재생성
* - 모수오차 얻는 법
  - 인자 직접 draw
  - pseudo-삼각형 재적합
  - pseudo-삼각형 재적합
* - 과정오차 얻는 법
  - 정규(Mack)
  - 모수적 gamma draw
  - 모수적 gamma draw
* - 분포 가정
  - 인자 ~ 정규
  - process ~ gamma
  - 잔차에 무가정
* - 재적합?
  - 아니오 (직접 draw)
  - 예
  - 예
* - 엔진 type
  - analytical
  - parametric
  - nonparametric
```

세 방법 모두 모수오차와 과정오차를 *둘 다* 잡습니다. 차이는 그 난수를 만드는
방식뿐입니다 — (1) 점근분포 직접 draw, (2) 모수분포 재생성 + 재적합, (3)
경험적 잔차 재표집 + 재적합. "재표집"이라는 단어가 붙는 건 마지막 하나뿐이고,
앞의 둘(Monte Carlo)은 가정분포에서 뽑습니다.

## 5. 왜 `simulate=True`(인자 draw)는 니치인가 — 선형 vs 비선형

`Analytical`은 이미 모수오차와 과정오차를 *닫힌형으로* 분해해 줍니다
(`loss_proc_se` / `loss_param_se` / `loss_total_se`). 그렇다면 그걸
시뮬레이션한 `simulate=True`는 무엇을 더 주는가? 답은 **분포의 모양** 하나뿐이고,
그조차 모델이 *선형이냐 비선형이냐*에 달려 있습니다.

- **ED (덧셈)**: $\text{loss\_ult} = \text{latest} + \sum_k g_k \cdot P$. 인자
  $g_k$에 **선형**이라, $g_k$가 정규면 궁극치도 *정확히 정규*. 평균과 분산이
  분포를 완전히 규정하므로 시뮬레이션은 그 Gaussian을 재구성할 뿐 — **새 정보
  0**. 그래서 ED에는 인자-draw가 아예 없습니다(닫힌형과 동치).
- **CL (곱셈)**: $\text{loss\_ult} = C_\text{latest} \cdot \prod_k f_k$. 인자에
  **비선형(곱)**이라 분포가 치우칩니다. 평균+SE로는 못 담는 상향 꼬리를
  시뮬레이션이 드러냅니다 -> 이때만 의미가 있습니다.

```{note}
그래서 `Analytical(simulate=True)`는 **CL 전용**입니다. ED/SA 점추정과 짝지어도
밴드는 CL로 나옵니다 (Mack 인자 곱셈 closed-form의 시뮬레이션이라). 분포가
필요하면 보통 과정오차까지 함께 잡는 부트스트랩을 쓰지, "모수만의 skew"를 따로
원하는 경우는 드뭅니다. 그래서 별도 클래스가 아니라 `Analytical`의 플래그로
둡니다.
```

같은 사실이 부트스트랩 평균에서 보입니다 — CL 인자-draw는 곱의 볼록성 때문에
시뮬레이션 평균이 plug-in 점추정보다 살짝 위로 drift하지만, ED 부트스트랩(덧셈)은
점추정과 거의 일치합니다.

## 6. 실무 가이드

```python
import lossratio as lr

tri = lr.Triangle(df, groups="coverage")

# 닫힌형 SE/CI (기본, 가장 싸고 안정적)
fit = lr.Ratio(method="ed", uncertainty=lr.Analytical()).fit(tri)

# 예측분포 히스토그램이 필요하면 부트스트랩
fit = lr.Ratio(method="ed",
               uncertainty=lr.ResidualBootstrap(n_replicates=999, seed=1)).fit(tri)

samples = fit.ultimate_ratio_samples()      # (n_replicates,) 포트폴리오 궁극 손해율
```

방법 선택:

- **밴드(SE/CI)만 필요** -> `Analytical()`. 가장 싸고 안정적이며 점추정과 정합.
- **예측분포(히스토그램)가 필요** -> `ResidualBootstrap`(단일 CL/ED) 또는
  `ParametricBootstrap`(SA 포함 어디나). `ultimate_ratio_samples()`로
  replicate별 포트폴리오 궁극 손해율 벡터를 받아 분위수(p50/p75/p95)를 그립니다.
- **단계 적응형(SA)** -> `ParametricBootstrap`. SA의 2단계 ED+CL 구조는 단일 잔차
  풀이 없어 `ResidualBootstrap`이 정합하지 않습니다.

```{important}
부트스트랩의 밴드는 손해 측 `method`를 따라갑니다 — ED 헤드라인이면 ED
부트스트랩, CL이면 CL 부트스트랩. 그래서 분포의 중심이 점추정과 같은 예측에서
나옵니다(ED 점추정에 CL 밴드가 어긋나는 mismatch 없음). 포트폴리오 궁극
손해율의 불확실성에는 셀 단위 해석적 SE를 단순 합산하지 말고 부트스트랩을
쓰십시오 — 코호트들이 같은 인자를 공유하는 상관(parameter correlation)을
부트스트랩만이 제대로 잡습니다.
```

## 7. 함께 보기

- 5장 "예측의 불확실성" — 과정오차·모수오차의 정의와 SE/CV/CI 읽는 법.
- `Analytical` / `ParametricBootstrap` / `ResidualBootstrap` API 문서.
- `RatioFit.ultimate_ratio_samples()` — 포트폴리오 궁극 손해율의 예측분포 표본.
