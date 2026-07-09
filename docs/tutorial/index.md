# 튜토리얼

이 튜토리얼은 lossratio로 **현업에서 장기 건강보험 손해율을 관리하는 흐름**을
따라갑니다 — 경험을 삼각형으로 정리해 지금 위치를 보고, 앞으로의 손해율을
예측하고, 관측 기간이 짧은 최근 코호트에 휘둘리지 않도록 보정하고, 구조 변화를 잡아내고,
얼마나 믿을지 검증하는 실무 작업들입니다.

```{admonition} 이 패키지가 전제하는 것
:class: note

lossratio는 모든 보험 종목을 아우르는 범용 이론 도구가 아닙니다. 무엇을
대상으로, 무슨 목적으로, 어떻게 범위를 좁혀 만들었는지를 먼저 밝혀 둡니다.

- **대상** — 한국 시장에서 많이 팔리는 **보장성 장기 건강보험**(암·수술·진단 등).
- **목적** — 학술적 완결성이 아니라 **현업의 손해율 관리**.
- **범위** — 가능한 이론을 넓게 코드화하기보다, 실제 데이터에서 **가장 실무적으로
  비교적 정확했던 경험**을 토대로 방법을 골라 좁혔습니다.
```

손해율을 **빠르게 산출하는 최소 코드**는 저장소 README의 Quick Start에
있습니다. 이 튜토리얼은 그 각 단계가 *무엇을·왜* 하는지 이해시키는 데 목적이
있습니다.

모든 예제는 내장 `lr.load_experience()`(4개 담보) 데이터로 그대로 실행되며,
표에 등장하는 숫자는 전부 코드 실행 결과입니다. 개념과 산술은 한 담보(CANCER)를
주인공으로 삼각형 하나에서 짚고, 담보가 여럿일 때의 이야기(구조 변화·세그먼트)는
필요한 장에서 나머지 담보로 확장합니다.

## 전체 지도 — 파이프라인이 아니라 객체와 옵션

lossratio는 세 가지로 이뤄집니다 — **데이터**(`Triangle`), **모델**(생성한 뒤
데이터에 적합), **결과**(`RatioFit`). 아래는 그 관계입니다. 왼쪽에서 오른쪽으로
*반드시 거치는 단계*가 아니라, **필요한 것을 골라 끼우는 구조**입니다.

```{mermaid}
flowchart TB
  A["경험 데이터"] --> T["<b>Triangle</b><br/>코호트 × 경과 · 1장"]
  T --> LM["<b>손해 모델</b><br/>Pooled/Credible/Smooth<br/>2·3장"]
  T --> PM["<b>보험료 모델</b><br/>PooledPremium · 2장"]
  LM --> R["<b>Ratio 합성</b><br/>손해율 = 손해 / 보험료 · 2장"]
  PM --> R
  R --> RF["<b>RatioFit</b><br/>예측 손해율"]
  OPT["<b>옵션 (추정기 인자)</b><br/>불확실성 4장 · 구조 변화 5장 · 공변량 6장"] -. 끼움 .-> LM
  T -. 진단 (선택) .-> DIAG["ATA · 강도 · ChainLadder<br/>부록"]
  RF --> V["<b>검증 (직교)</b><br/>백테스트 7장 · 안정성 8장"]
  classDef data fill:#dceaf6,stroke:#4a7ba6,color:#16344e
  classDef model fill:#eaf1f8,stroke:#6f8ca3,color:#22313c
  classDef result fill:#ffe3a0,stroke:#cf9b00,color:#4a3800
  classDef opt fill:#f3e8f6,stroke:#9b6fa6,color:#3c223c
  classDef validate fill:#e3f0e9,stroke:#5a9b86,color:#1c3a2e
  classDef aux fill:#f0f0f0,stroke:#999,color:#333
  class A,T data
  class LM,PM,R model
  class RF result
  class OPT opt
  class V validate
  class DIAG aux
```

읽는 법:

- **실행만 하려면 1-2장이면 충분**합니다 (삼각형 → 손해·보험료 모델 → 합성 →
  예측 손해율).
- **3-6장은 예측을 더 낫게·강건하게** 만드는 것들입니다 — 코호트 보정, 불확실성
  밴드, 구조 변화, 세그먼트. regime·불확실성·공변량은 *별도 단계가 아니라 추정기를
  만들 때 끼우는 인자*입니다.
- **7-8장은 그 결과를 검증**합니다 (백테스트·안정성) — 앞 흐름의 "다음 단계"가
  아니라 완성된 추정기를 통째로 평가하는 별도 축입니다.
- **인자(ATA·강도)와 벤치마크(ChainLadder)** 는 예측이 이상할 때 속을 들여다보는
  **부록**입니다 — 예측의 전제가 아니라 진단 도구입니다.

## 차례

```{list-table}
:header-rows: 1
:widths: 6 30 64

* - 장
  - 제목
  - 내용
* - 1
  - 손해율 삼각형 — 경험 데이터 구성
  - 장기 건강보험 손해율이 왜 어려운가(닫히지 않는 흐름), 경험 데이터의 구조, 코호트 × 경과 삼각형, 누적과 증분, 4담보를 `groups`로 올리기
* - 2
  - 손해율 예측 — 손해·보험료 모델과 Ratio 합성
  - 완전 풀링 `PooledLoss`(강도 × 보험료), 보험료는 알려진 노출로 자기-발전 `PooledPremium`, `Ratio(loss=, premium=)` 합성으로 예측 손해율 산출과 읽기
* - 3
  - 코호트 보정 — 신뢰도(Credible)와 평활(Smooth)
  - 관측 셀이 적은 최근 코호트를 얼마나 믿을지: `CredibleLoss`(코호트 수준 부분 풀링, `.credibility`), `SmoothLoss`(형상 평활)
* - 4
  - 예측 불확실성 — 밴드와 부트스트랩
  - 과정오차와 모수오차, 표준오차·변동계수·신뢰구간, `ResidualBootstrap`(Credible/Smooth는 부트스트랩 전용)
* - 5
  - 구조 변화 (Regime)
  - 요율·약관·언더라이팅·의료환경 변화로 전후가 갈리는 시점을 손해율에서 추정, E-Divisive, step/drift 게이트, 예측 반영(SURGERY)
* - 6
  - 세그먼트별 손해율 — 분리(groups)와 공변량(covariates)
  - 세그먼트를 쪼개 따로 적합(`groups`, 완전 분리) vs 형상 공유 + 수준 회귀(`covariates`, 부분 풀링), `.coefficients`와 `predict(by=)`
* - 7
  - 표본 외 검증 — 백테스트
  - 달력 대각선 hold-out, A/E Error, rolling-origin 신뢰도 곡선과 방법 비교(`EstimatorComparison`)
* - 8
  - 예측 안정성 — 언제부터 믿을까
  - go-forward 안정성 진단(`Stability`), freeze 게이트, `extend`로 관측 최대 경과 너머 이어 보기
* - 부록
  - 발전 인자 진단 — ATA·강도와 ChainLadder 벤치마크
  - 예측이 이상할 때 속 들여다보기: ATA `f_k`(곱셈, 차용 도너·벤치마크 엔진), 강도 `g_k`(덧셈, 본류 재료), 고전 `ChainLadder`
```

```{toctree}
:maxdepth: 1
:caption: 차례

01-triangle
02-projection
04-projection
05-uncertainty
06-regime
09-covariates
07-backtest
08-stability
02-ata
03-intensity
```
