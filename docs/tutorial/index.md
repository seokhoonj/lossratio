# 튜토리얼

이 튜토리얼은 코호트별 손해가 시간이 지나면서 어떻게 발생하고 누적
손해율이 어떻게 움직이는지부터 시작해, lossratio가 그 손해율을 어떻게
예측하고 검증하는지까지를 차근차근 설명합니다.

수식과 코드가 등장하지만, 핵심은 **개념과 분석의 흐름**입니다.
파이썬에 익숙하지 않아도 따라올 수 있도록, 코드는 "패키지에 무엇을
시키는가"를 보여주는 최소한의 호출 예시로만 씁니다. 모든 예제는
패키지에 내장된 `lr.load_experience()` 데이터로 그대로 실행됩니다.

## 전체 흐름 한눈에 보기

분석은 경험 데이터에서 시작해 삼각형으로 정리하고, 손해율이 경과 기간에
따라 어떻게 움직이는지 진단한 뒤, 아직 관측되지 않은 칸을 예측하고, 그
예측을 검증하는 순서로 흐릅니다. 각 장이 이 흐름의 어디에 놓이는지 먼저
보겠습니다.

```{mermaid}
flowchart LR
  A["경험 데이터<br/>(long-format)"] --> B["Triangle<br/>코호트 x 경과 기간 삼각형<br/>(1장)"]
  B --> C["link 진단<br/>ATA 인자 / 강도<br/>(2-3장)"]
  C --> D["예측<br/>완전 풀링 / 부분 풀링<br/>(4장)"]
  D --> E["불확실성<br/>표준오차 · 신뢰구간<br/>(5장)"]
  E --> F["regime 탐지 · 백테스트<br/>구조 변화 · 검증<br/>(6-7장)"]
  classDef data fill:#dceaf6,stroke:#4a7ba6,color:#16344e
  classDef estimate fill:#eaf1f8,stroke:#6f8ca3,color:#22313c
  classDef validate fill:#e3f0e9,stroke:#5a9b86,color:#1c3a2e
  classDef current fill:#ffe3a0,stroke:#cf9b00,color:#4a3800,stroke-width:2.5px
  class A data
  class B,C,D estimate
  class E,F validate
```

왼쪽 세 단계(삼각형 -> 진단 -> 예측)가 손해율 수준을 *읽어 내는* 본
과정이고, 오른쪽 두 단계(불확실성 -> 검증)가 그 결과를 *믿을 만한지
따지는* 과정입니다. 이 튜토리얼은 이 순서를 그대로 따라갑니다.

## 이 튜토리얼이 다루는 것

```{list-table}
:header-rows: 1
:widths: 8 32 60

* - 장
  - 제목
  - 내용
* - 1
  - 손해율과 코호트 삼각형
  - 장기 건강보험 손해율이 왜 어려운가, 경험 데이터의 구조, 코호트 x 경과 기간 삼각형, 누적과 증분
* - 2
  - 손해율의 경과별 변화 — Link와 ATA 인자
  - 이웃한 경과 기간을 잇는 링크, ATA 인자를 손으로 구하기, 경과별 인자 읽기, ATA 인자가 안정되는 구간
* - 3
  - 강도의 직관
  - 위험보험료로 나눈 손해(강도), 완전 풀링 강도(`PooledLoss`)의 직관, 강도가 0으로 잦아드는 까닭
* - 4
  - 손해율 예측 — 구조 사다리
  - 완전 풀링(`PooledLoss`) 베이스라인, 코호트 수준을 신뢰도 가중으로 보정하는 `CredibleLoss`, 형상을 매끄럽게 다듬는 `SmoothLoss`, 벤치마크 링크비(`ChainLadder`), 예측 결과 읽기
* - 5
  - 예측의 불확실성
  - 과정오차와 모수오차, 표준오차·변동계수·신뢰구간, 부트스트랩
* - 6
  - 구조 변화 탐지
  - 인수 코호트의 regime 변화란, E-Divisive 탐지, step/drift 게이트, 예측에 반영하기
* - 7
  - 예측 검증
  - 달력 대각선 hold-out 백테스트, A/E Error로 읽는 예측 정확도, rolling-origin 신뢰도 곡선과 방법 간 비교
```

```{toctree}
:maxdepth: 1
:caption: 차례

01-triangle
02-ata
03-intensity
04-projection
05-uncertainty
06-regime
07-backtest
08-stability
```
