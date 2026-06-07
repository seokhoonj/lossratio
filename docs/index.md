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

데모 사이트(demo.lossratio.org)는 lossratio 라이브러리를 활용해 만든 별도의
웹 애플리케이션입니다. lossratio 자체는 데모가 아니라, 그 계산을 맡는 Python
라이브러리입니다.

```{button-ref} getting-started
:color: secondary
:shadow:
시작하기
```

::::{grid} 1 2 2 4
:gutter: 3

:::{grid-item-card} 코호트 삼각형
인수 코호트 x 경과 기간 삼각형을 long-format 경험 데이터에서
바로 구축. 누적·증분, 다양한 집계 주기를 일관되게 처리.
:::

:::{grid-item-card} 노출 기반 예측
기본은 노출 기반(exposure-driven, ED)의 안전한 베이스라인.
필요하면 `SwitchPoint`로 ED->CL 전환점을 골라 단계 적응형(SA)으로
전환. 초기 변동성에 강건한 손해율 예측.
:::

:::{grid-item-card} 진단과 탐지
ATA 인자(age-to-age factor)·수렴점(convergence point) 진단, 그리고
인수 코호트의 구조 변화(regime) 탐지까지 한 인터페이스로.
:::

:::{grid-item-card} 검증과 불확실성
달력 대각선 hold-out 백테스트로 예측을 검증하고,
부트스트랩과 해석적 분해로 표준오차를 정량화.
:::

::::

## 설치

```{warning}
**개발 중(WIP)** — 이론적 토대를 대폭 개정하는 중이라 API와 산출값이 바뀔 수
있습니다. PyPI 버전은 이미 폐기된 옛 방법론이므로, 최신은 아래처럼 GitHub에서
설치하세요.
```

```bash
pip install "git+https://github.com/seokhoonj/lossratio.git"              # polars only
pip install "lossratio[pandas] @ git+https://github.com/seokhoonj/lossratio.git"      # add pandas / pyarrow support
```

lossratio는 내부적으로 polars를 사용하지만 입력은 polars와 pandas를
모두 받으며, 출력은 입력 타입을 그대로 따라갑니다 (pandas in ->
pandas out, polars in -> polars out).

```{toctree}
:hidden:

시작하기 <getting-started>
튜토리얼 <tutorial/index>
API 레퍼런스 <api>
```
