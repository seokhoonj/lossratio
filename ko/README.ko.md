# lossratio

보험 경험 데이터의 손해율 분석과 추정.

## 개요

`lossratio`는 long-format 경험 데이터로부터 **장기 건강보험** 손해율을
분석하기 위한 도구 모음이다 — 한 행이 (코호트 × 경과 기간 × 인구통계) 셀
하나에 대응하며, 손해액과 위험보험료 컬럼을 가진다. 다년 보장 건강보험은
손해가 천천히 누적된다. 초기 경과 기간에서 연속 발달비(age-to-age)는
불안정하며, 익스포저(≈ 위험보험료)가 추정의 가장 신뢰할 만한 기준이
된다. 또한 상품 개정, 인수 기준 변경, 규제 개혁에서 비롯된 구조적
변화(structural break)가 코호트에 걸쳐 누적된다. 이 패키지의 기본값 —
단계 적응적(stage-adaptive, SA) 추정, 초기 경과 기간의 노출
기반(exposure-driven, ED) 모형, 코호트 regime(체제) 탐지 — 은 이러한
환경에 맞게 조정되어 있다. 동일한 도구는 누적 손해 / 익스포저 구조라면
어떤 영역에도 적용 가능하다 (사망률, 발병률, 일반 손해).

제공 기능:

- 경험 데이터의 세 가지 집계 프레임: 코호트 × 경과 기간 (`triangle`),
  달력 기간 (`calendar`), 포트폴리오 전체 (`total`)
- age-to-age (`ata`) 와 노출 기반 (`ed`) 의 경과 기간 모형화
- chain ladder 추정 (`fit_cl`) 과 손해율 추정 (`fit_lr`), 세 가지 method
  지원:
  - `"sa"` — **단계 적응적** (기본값): 성숙점 이전은 노출 기반, 이후는
    chain ladder
  - `"ed"` — 모든 경과 기간에 대해 노출 기반
  - `"cl"` — 고전적 chain ladder (Mack 모형)
- 구조적 변화에 대한 코호트 regime 탐지 (`detect_cohort_regime`)
- 진단 및 triangle 시각화

## 입력 형식

최소한 다음 컬럼을 포함하는 long-format `data.frame` / `data.table`:

| 컬럼   | 의미                                 | 예시               |
|--------|--------------------------------------|--------------------|
| cohort | 인수 / 사고 시점 (granularity 무관)  | `uym`, `uy`        |
| dev    | 코호트 시작 시점 이후 경과 기간      | `elap_m`, `elap_y` |
| `loss` | 셀 내 증가분 클레임 금액             | numeric            |
| `rp`   | 셀 내 증가분 위험보험료 (기대손해액) | numeric            |
| group  | 선택 — 보장, 상품, 연령대, 성별 등   | character / factor |

[`as_experience()`](https://seokhoonj.github.io/lossratio/ko/reference/as_experience.md)
는 스키마를 검증하고 날짜 컬럼을 코어션한다. 이어서
[`build_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/build_triangle.md)
은 표준 코호트 × 경과 기간 구조로 집계하며, 누적 컬럼과 파생 비율을 함께
산출한다.

## 설치

``` r

# devtools
devtools::install_github("seokhoonj/lossratio")

# remotes
remotes::install_github("seokhoonj/lossratio")
```

이 패키지는 `seokhoonj/instead` 와 `seokhoonj/ggshort` 에 의존한다
(`Remotes:` 를 통해 자동 설치).

## Quick Start

``` r

library(lossratio)

# 번들 calibrated 합성 experience data
# (종목별 dev curve 형상은 실 포트폴리오의 broad shape 에 calibrate;
# cell-level 값과 cohort 패턴은 무작위 생성)
data(experience)
exp <- as_experience(experience)

# Build the canonical cohort × dev structure
tri <- build_triangle(exp, group_var = cv_nm)

plot(tri)              # cohort trajectories
plot_triangle(tri)     # cell heatmap

# Age-to-age and exposure-driven development
ata <- build_ata(tri, value_var = "closs"); fit_ata(ata)
ed  <- build_ed(tri);                       fit_ed(ed)

# Chain ladder fit
cl <- fit_cl(tri, value_var = "closs", method = "mack")
plot(cl, type = "projection")

# Loss ratio fit (stage-adaptive by default)
lr <- fit_lr(tri, method = "sa")
plot(lr, type = "clr")
summary(lr)

# Structural change across cohorts
detect_cohort_regime(tri[cv_nm == "SUR"], K = 12, method = "ecp")
```

## 집계 프레임

동일한 long-format 경험 데이터를 세 가지 관점으로 본다:

| Builder | 출력 객체 | 차원 | 활용 |
|----|----|----|----|
| [`build_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/build_triangle.md) | `triangle` | 코호트 × 경과 기간 (2D) | Chain ladder, ED, SA 추정 |
| [`build_calendar()`](https://seokhoonj.github.io/lossratio/ko/reference/build_calendar.md) | `calendar` | 달력 기간 (1D) | 달력연도 추세 / 대각선 효과 |
| [`build_total()`](https://seokhoonj.github.io/lossratio/ko/reference/build_total.md) | `total` | 포트폴리오 전체 (0D, 그룹별) | 그룹 간 고수준 비교 |

`build_triangle` 이후의 하위 컬럼은 입력 granularity (`uym` / `uyq` /
`uy` 등) 와 무관하게 `cohort` 와 `dev` 로 표준화된다. 원본 컬럼명과
granularity 는 attribute (`cohort_var`, `cohort_type`, `dev_var`,
`dev_type`) 로 보존된다.

## Methods

### 단계 적응적 (`fit_lr(method = "sa")`, 기본값)

노출 기반과 chain ladder 의 결합으로, 그룹별 성숙점에서 전환된다:

- 성숙점 이전: 노출 기반 추정 $`\Delta C^L = g_k \cdot C^P_k`$ — 연속
  발달비가 변동성이 큰 구간에서 추정값을 보험료 규모에 고정한다.
- 성숙점 이후: chain ladder 추정 $`C^L_{k+1} = f_k \cdot C^L_k`$ — 연속
  발달비가 안정된 이후 코호트의 관측 수준을 보존한다.

### 노출 기반 (`fit_lr(method = "ed")`)

모든 미래 손해 증가분이 익스포저(위험보험료)를 분모로 사용한다. 연속
발달비가 정보량이 부족하거나 전 구간에 걸쳐 불안정할 때 적합하다.

### Chain Ladder (`fit_lr(method = "cl")` / `fit_cl()`)

고전적 Mack chain ladder. 선택적 log-linear tail factor 와 해석적 Mack
표준오차를 지원한다.

## 시각화

두 S3 generic 모두 객체 클래스에 따라 dispatch 된다:

``` r

plot(x)              # base plot generic — line / panel diagnostics
plot_triangle(x)     # lossratio generic — cell heatmap layout
```

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) 과
[`plot_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.md)
은 `triangle`, `calendar`, `ata`, `ata_fit`, `ed`, `ed_fit`, `cl_fit`,
`lr_fit`, `cohort_regime` 객체 전반에 일관되게 작동한다.

## 문서

``` r

?build_triangle
?fit_lr
?detect_cohort_regime
vignette("regime-detection", package = "lossratio")
```

## 라이선스

GPL (\>= 2).
[LICENSE.md](https://seokhoonj.github.io/lossratio/ko/LICENSE.md) 참조.

## Author

Seokhoon Joo (<seokhoonj@gmail.com>)
