# 투영 메커니즘 — 손해·보험료 사다리

손해율 예측의 두 사다리(손해·보험료)가 각 셀을 *어떻게* 투영하는지 수식으로
정리한다. 표기: $C_k$ = 누적 손해, $P_k$ = 누적 위험보험료, $k$ = 경과,
$i$ = 코호트. 장기 건강보험의 코호트-경험이므로 분자·분모가 모두 계속
흐르고, "최종값(ultimate)"이나 run-off 꼬리는 정의되지 않는다 — 투영은 관측된
최대 경과까지만 한다.

## 1. 두 측면, 두 메커니즘 — 측면이 메커니즘을 정한다

| 측면 | 외부 노출 | 메커니즘 | 발전 인자 |
|---|---|---|---|
| **손해(loss)** | 있음 (분모 = 위험보험료 $P$) | **강도(intensity)** | $g_k$ |
| **보험료(premium)** | 없음 (보험료가 곧 노출 기반) | **링크비(link ratio)** | $f^P_k$ |

손해는 노출 $P$가 있어, 증분을 노출로 정규화한 **강도**(intensity)로 발전한다:

$$g_k = \frac{\sum_i \Delta L_{i,k}}{\sum_i P_{i,k}}, \qquad
  C_{i,k+1} = C_{i,k} + u_i\, g_k\, P_{i,k}.$$

보험료는 외부 노출이 없어 **자기 링크비**(self-developing link ratio)로 발전한다.
성장률을 $h_k = f^P_k - 1$로 두면:

$$f^P_k = \frac{\sum_i P_{i,k+1}}{\sum_i P_{i,k}}, \qquad
  P_{i,k+1} = P_{i,k}\,\bigl(1 + u_i\, h_k\bigr).$$

$u_i = 1$이면 $1 + (f^P_k - 1) = f^P_k$이므로 $P_{i,k+1} = P_{i,k}\,f^P_k$, 곧
보험료의 순수 링크비 발전이다.

```{admonition} 메커니즘은 선택이 아니다
:class: note

손해엔 노출(보험료)이 있어 강도, 보험료엔 없어 링크비 — 이는 **측면의 성질**
이지 사용자가 고르는 옵션이 아니다. 그래서 추정기 이름에 메커니즘을 적지 않는다
(중복이므로).
```

## 2. 세 단계 — 형상 x 반응 (양쪽 동일)

각 측면은 같은 3단 사다리를 가지며, 단계는 **코호트별 레벨이 어떻게 들어가는가**로
갈린다.

| 단계 | 경과 형상(shape) | 코호트 레벨(level) |
|---|---|---|
| **Pooled** | saturated (경과마다 자유 인자) | 완전 풀링, 코호트 공통 ($u_i = 1$) |
| **Credible** | saturated | 코호트별 **신뢰도**(credibility) $u_i$ |
| **Smooth** | **P-spline 평활** ($g_k$ 또는 $h_k$가 매끄러운 곡선) | 코호트별 $u_i$ |

- **Pooled는 Credible/Smooth의 $u_i = 1$ 퇴화**다 — 코호트 구분이 없다.
- Credible은 형상은 saturated로 두고 **레벨**을 코호트별 신뢰도로 가중한다.
- Smooth는 **형상**을 P-spline(벌점 B-spline)으로 평활해 이웃 경과끼리 강도를
  빌려 안정화하고, 레벨은 Credible과 같다.

## 3. 신뢰도 레벨 $u_i$

$u_i$는 **코호트 $i$가 풀링 평균 대비 얼마나 높게/낮게 발전하는가**를 신뢰도
(credibility)로 추정한 배수다. $1$이 기준선(풀링 레벨)이다:

$$u_i = Z_i\,\hat\theta_i + (1 - Z_i)\cdot 1, \qquad
  Z_i = \frac{A_i}{A_i + 1/\psi}.$$

- $\hat\theta_i$ = 코호트 자기 추정, $Z_i \in [0,1]$ = 신뢰도 가중(노출량 기반).
  데이터가 많은 코호트는 $Z_i \to 1$로 자기 추정에, 얇은 코호트는 $Z_i \to 0$으로
  풀링($u_i \to 1$)에 가까워진다.
- $\psi$ = 코호트 간 분산(between-cohort variance). $\psi \to 0$이면 $u_i = 1$이
  강제되어 **Pooled 단계**가 된다.

**예시** (보험료, 경과 $k$, 두 코호트 A·B, 둘 다 $P_k$, $h_k = 0.05$):
$u_A = 1.2$, $u_B = 0.7$이면 A는 $P_k(1 + 1.2\cdot0.05) = 1.06\,P_k$, B는
$P_k(1 + 0.7\cdot0.05) = 1.035\,P_k$로 **코호트마다 다르게** 발전한다. Pooled는
둘 다 $u = 1$이라 $1.05\,P_k$로 같다.

## 4. 전체 분류표

| 측면 | 단계 | 메커니즘 | 투영식 |
|---|---|---|---|
| 손해 | Pooled | 강도 $g_k$ | $C_{k+1} = C_k + g_k\,P_k$ |
| 손해 | Credible | 강도 $g_k$ | $C_{k+1} = C_k + u_i\,g_k\,P_k$ |
| 손해 | Smooth | 강도 $g_k$ (평활) | $C_{k+1} = C_k + u_i\,g_k\,P_k$ |
| 보험료 | Pooled | 링크비 $f^P_k$ | $P_{k+1} = P_k\,f^P_k$ |
| 보험료 | Credible | 링크비 $f^P_k$ | $P_{k+1} = P_k\,(1 + u_i\,h_k)$ |
| 보험료 | Smooth | 링크비 $f^P_k$ (평활 $h_k$) | $P_{k+1} = P_k\,(1 + u_i\,h_k)$ |

세 투영 형태로 갈린다: 강도-가법(손해 사다리 전부), 단일 링크비 곱(보험료
Pooled, 코호트 공통), 코호트별 링크비 곱(보험료 Credible/Smooth, $1 + u_i h_k$).
마지막은 $u_i = 1$에서 단일 링크비 곱과 일치하는 일반화다.

## 5. ChainLadder — 손해의 링크비 벤치마크

손해는 보통 강도 $g_k$로 발전하지만, `ChainLadder`는 **강도 사다리 밖**의 유일한
손해 모델이다: 노출을 쓰지 않고 자기 누적손해의 **링크비** $f_k = \sum_i C_{i,k+1}
/ \sum_i C_{i,k}$로 발전하는 벤치마크다.

$$C_{i,k+1} = C_{i,k}\, f_k.$$

즉 손해엔 메커니즘이 둘(강도 사다리 + ChainLadder 링크비), 보험료엔 하나(링크비)
다 — 그래서 *손해에서만* 링크비 모델에 메커니즘 이름을 붙여 구분한다.
`PooledPremium`은 **보험료의 ChainLadder**로, $f^P_k$에 같은 곱셈 링크비
메커니즘을 적용한 것 — 같은 메커니즘, 다른 타깃이다.

## 6. segment_wise — regime 간 borrow

구조 변화(regime change)가 있으면 코호트를 regime별로 나눠 각자 적합한다
([6장 — 구조 변화 탐지](../tutorial/06-regime)). 각 regime은 자기 코호트로
**own depth**(자기 관측 최대 경과)까지 자기 레벨·형상으로 적합한다.

최신(얇은) regime이 아직 도달하지 못한 후기 경과를 **borrow 구간**이라 한다 —
$[\,d_r + 1,\ K\,]$, 여기서 $d_r$은 그 regime의 own depth, $K$는 **global depth**
(어느 코호트든 도달한 최대 경과). 이 구간은 *미관측*이 아니라, **더 깊은(옛)
regime들이 이미 관측한** 경과다. 최신 regime엔 자기 데이터가 없을 뿐이다.

borrow 구간은 그 옛 regime들(donor)의 **수준 불변(level-invariant) 링크비**
(손해 $f_k$ / 보험료 $f^P_k$)로 채운다. 링크비는 누적의 비율이라 레벨이 상쇄되어,
*형상만* 빌리고 최신 regime의 자기 레벨은 유지된다:

$$C_{i,k+1} = C_{i,k}\, f^{\text{donor}}_k \quad (k \ge d_r,\ \text{borrow 구간}).$$

투영은 $K$(global depth)에서 멈춘다 — 관측 최대 경과 너머로 외삽하지 않는다.
즉 borrow는 코호트 축을 가로지르는 *형상 전이*로, **빌려오는 정보는 donor
regime에서 관측된 것**이고 채워지는 곳은 최신 regime이 자기 관측이 없는 후기
경과다.

보험료 사다리에서도 같은 borrow가 적용된다: Pooled는 단일 링크비 $f^P_k$로
borrow 구간을 채우고, Credible/Smooth는 own 구간을 코호트별 $1 + u_i h_k$로
적합한 뒤 borrow 구간을 donor $f^P_k$로 채운다(거기선 링크비가 레벨을 상쇄하므로
$u$ 무관).

## 7. 함께 보기

- {doc}`3장 — 강도 <../tutorial/03-intensity>`: 강도 $g_k$의 정의와 진단.
- {doc}`4장 — 손해율 예측 <../tutorial/04-projection>`: 사다리 사용법과 예측 표.
- {doc}`6장 — 구조 변화 탐지 <../tutorial/06-regime>`: regime과 세 treatment.
- {doc}`9장 — 공변량 <../tutorial/09-covariates>`: 공변량으로 속성별 분해.
