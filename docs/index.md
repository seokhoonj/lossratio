# lossratio

장기 건강보험 손해율 분석 — 코호트 진전 분석부터 예측·검증까지

lossratio는 장기 건강보험의 **손해율(loss ratio)**, 곧
손해액을 위험보험료로 나눈 값을 코호트별로 분석하고 예측하는
오픈소스 파이썬 패키지입니다. 경과에 따라 손해가 어떻게 쌓이는지를
삼각형으로 정리하고, 미관측 구간의 손해율을 예측하며, 구조 변화를
탐지하고, 예측의 정확도를 백테스트로 검증합니다.

핵심 가치는 하나로 모입니다 — 갓 인수되어 관측 기간이 짧은 코호트에서도
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

::::{grid} 1 2 2 4
:gutter: 3

:::{grid-item-card} 코호트 삼각형
인수 코호트 x 경과 기간 삼각형을 long-format 경험 데이터에서
바로 구축. 누적·증분, 다양한 집계 주기를 일관되게 처리.
같은 시점에 인수된 계약을 한 행으로 묶어 경과에 따른 손해 누적을
나란히 비교할 수 있게 정렬해 두는 것이 모든 분석의 출발점입니다.
:::

:::{grid-item-card} 손해 예측 사다리
손해율은 경과에 따른 공통 **형상(shape)**과 코호트마다 다른
**수준(level)**으로 나눠 볼 수 있습니다. 완전 풀링(`PooledLoss`)의 안전한
베이스라인에서, 코호트별 수준만 신뢰도 가중으로 보정하는 `CredibleLoss`,
형상까지 평활하는 `SmoothLoss`로 한 단계씩 올라갑니다. 관측 기간이 짧은 갓 인수
코호트는 제 형상을 스스로 믿기 어려워 풀링된 형상을 빌려 오고 수준만
조절하므로, 사다리를 오를수록 데이터에서 더 많은 구조를 빌리게 됩니다.
`ChainLadder`는 자기손해 링크비를 쓰는 고전적 참조 비교군입니다.
:::

:::{grid-item-card} 진단과 탐지
ATA 인자(age-to-age factor)·안정성(stability) 진단, 그리고
인수 코호트의 구조 변화(regime) 탐지까지 한 인터페이스로.
요율 개정·약관 변경처럼 손해율 수준 자체가 달라진 시점을 데이터에서
찾아내, 더 이상 같은 흐름으로 볼 수 없는 코호트를 가려냅니다.
:::

:::{grid-item-card} 검증과 불확실성
달력 대각선 hold-out 백테스트와 rolling-origin 신뢰도 곡선으로
예측을 검증하고, 부트스트랩과 해석적 분해로 표준오차를 정량화.
예측을 한 숫자가 아니라 신뢰할 수 있는 경과 길이와 밴드로 제시해,
가격 가정 판단의 근거가 어디까지 유효한지 함께 보여 줍니다.
:::

::::

## 30초 미리보기

설치 직후의 전체 흐름은 한 화면에 담깁니다 — 경험 데이터를 불러와
삼각형으로 집계하고, 손해(분자)와 보험료(분모) 추정기를 `LossRatio`로 묶어
코호트별 예측 손해율과 표준오차를 얻습니다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience()                                   # 시연용 합성 경험 데이터
tri = lr.Triangle(df.filter(pl.col("coverage") == "SURGERY"), groups="coverage")

fit = lr.LossRatio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(tri)
fit.summary().select(["cohort", "ratio_proj", "ratio_se"]).head(3)
#> shape: (3, 3)
#> ┌────────────┬────────────┬──────────┐
#> │ cohort     ┆ ratio_proj ┆ ratio_se │
#> │ ---        ┆ ---        ┆ ---      │
#> │ date       ┆ f64        ┆ f64      │
#> ╞════════════╪════════════╪══════════╡
#> │ 2023-01-01 ┆ 1.509562   ┆ null     │
#> │ 2023-02-01 ┆ 1.508992   ┆ 0.004207 │
#> │ 2023-03-01 ┆ 1.521789   ┆ 0.007969 │
#> └────────────┴────────────┴──────────┘
```

완전히 관측된 첫 코호트는 예측할 미래 셀이 없어 `ratio_se`가 `null`입니다.
단계별 설명은 {doc}`시작하기 <getting-started>`에서 이어집니다.

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
