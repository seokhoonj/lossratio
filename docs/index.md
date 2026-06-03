# lossratio

장기 건강보험 손해율 분석 — 코호트 발전 분석부터 예측·검증까지

lossratio는 장기 건강보험의 **손해율(loss ratio)**, 곧
손해액을 위험보험료로 나눈 값을 코호트별로 분석하고 예측하는
오픈소스 파이썬 패키지입니다. 경과에 따라 손해가 어떻게 쌓이는지를
삼각형으로 정리하고, 미관측 구간의 손해율을 예측하며, 구조 변화를
탐지하고, 예측의 정확도를 백테스트로 검증합니다.

핵심 가치는 하나로 모입니다 — 갓 인수되어 관측이 얇은 코호트에서도
**손해율 수준(level)을 가장 이르게, 그러나 믿을 수 있게 읽어내는 것**.
가격 가정이 앞으로의 손해를 감당할 수준인지를, 몇 년 치 데이터가 다
쌓이기 전에 판단할 수 있어야 하기 때문입니다.

```{button-link} https://demo.lossratio.org
:color: primary
:shadow:
라이브 데모 열기
```

```{button-ref} start
:color: secondary
:shadow:
빠른 시작 보기
```

::::{grid} 1 2 2 4
:gutter: 3

:::{grid-item-card} 코호트 삼각형
인수 코호트 x 경과 기간 삼각형을 long-format 경험 데이터에서
바로 구축. 누적·증분, 다양한 집계 주기를 일관되게 처리.
:::

:::{grid-item-card} 단계 적응형 예측
성숙점 이전에는 노출 기반(ED), 이후에는 체인래더(CL)로 예측하는
단계 적응형(SA) 방법. 초기 변동성에 강건한 손해율 예측.
:::

:::{grid-item-card} 진단과 탐지
ATA 인자·성숙점·수렴점 진단, 그리고 인수 코호트의 구조 변화
(regime) 탐지까지 한 인터페이스로.
:::

:::{grid-item-card} 검증과 불확실성
달력 대각선 hold-out 백테스트로 예측을 검증하고,
부트스트랩과 해석적 분해로 표준오차를 정량화.
:::

::::

## 설치

```bash
pip install lossratio              # polars only
pip install lossratio[pandas]      # add pandas / pyarrow support
```

lossratio는 내부적으로 polars를 사용하지만 입력은 polars와 pandas를
모두 받으며, 출력은 입력 타입을 그대로 따라갑니다 (pandas in ->
pandas out, polars in -> polars out).

```{toctree}
:hidden:

빠른 시작 <start>
튜토리얼 <tutorial/index>
API 레퍼런스 <api>
```
