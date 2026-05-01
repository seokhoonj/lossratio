# lossratio 시작하기

> 영어 원본 보기: [Getting started with
> lossratio](https://seokhoonj.github.io/lossratio/getting-started.md)

이 vignette 은 `lossratio` 의 전체 파이프라인을 내장 합성 experience
데이터 위에서 따라간다. raw long-format 행에서 시작하여 적합된 손해율
추정까지 이어진다.

## 입력 형태

`lossratio` 는 long-format experience 데이터를 입력으로 사용한다 — 한
행은 (코호트 × 경과 기간 × 인구통계) 셀 하나에 대응한다. 내장 데이터셋
`experience` 는 33,381 행 테이블로, 여러 단위의 calendar / underwriting
기간 컬럼, 인구통계 차원 (`cv_nm`, `age_band`, `gender`), 금액 컬럼
(`loss`, `rp`) 을 포함한다.

``` r

library(lossratio)

data(experience)
str(experience)
#> Classes 'data.table' and 'data.frame':  33381 obs. of  17 variables:
#>  $ cy, cyh, cyq, cym  : Date  (calendar period)
#>  $ uy, uyh, uyq, uym  : Date  (underwriting period)
#>  $ elap_y, elap_h, elap_q, elap_m : int  (elapsed period)
#>  $ cv_nm    : chr   (coverage)
#>  $ age_band : Ord.factor
#>  $ gender   : Factor
#>  $ loss, rp : num
```

## 1단계 — 검증 및 코어션

``` r

exp <- as_experience(experience)
class(exp)
#> [1] "experience" "data.table" "data.frame"
```

[`as_experience()`](https://seokhoonj.github.io/lossratio/reference/as_experience.md)
는 필수 컬럼 (`cym`, `uym`, `loss`, `rp`) 의 존재를 확인하고, 날짜
컬럼을 코어션하며, 클래스를 부여한다. 변형 없이 검증만 필요하면
`check_experience(df)` 를 사용한다.

## 2단계 — 코호트 × 경과 기간 구조 구축

``` r

tri <- build_triangle(exp, group_var = cv_nm)
class(tri)
#> [1] "triangle" "data.table" "data.frame"
names(tri)
#> [1] "cv_nm" "n_obs" "cohort" "dev" "loss" "rp" "closs" "crp"
#> [9] "margin" "cmargin" "profit" "cprofit" "lr" "clr"
#> [...] proportions
```

[`build_triangle()`](https://seokhoonj.github.io/lossratio/reference/build_triangle.md)
의 동작:

- 인구통계 차원을 집계하여 제거한다 (여기서는 `age_band`, `gender`),
- 누적 컬럼 (`closs`, `crp`) 을 추가한다,
- 파생 지표 (`margin`, `lr`, `clr`, 비율) 를 추가한다,
- 코호트 / 경과 기간 컬럼을 표준명 `cohort` 와 `dev` 로 rename 한다,
- 원본 컬럼명은 attribute (`cohort_var`, `dev_var`) 로 보존하여 하위
  plot 라벨에서 활용 가능하게 한다.

## 3단계 — 진단

``` r

plot(tri)              # 코호트별 clr 궤적
plot_triangle(tri)     # clr 셀의 heatmap
summary(tri)           # 경과 기간별 그룹 통계량
```

## 4단계 — 경과 기간 모형화

코호트 발달을 보는 두 가지 상호 보완적 관점.

``` r

# 연속 발달비(age-to-age) 인자
ata <- build_ata(tri, value_var = "closs")
fit_ata(ata)

# 노출 기반(exposure-driven) 강도
ed <- build_ed(tri, loss_var = "closs", exposure_var = "crp")
fit_ed(ed)
```

[`fit_ata()`](https://seokhoonj.github.io/lossratio/reference/fit_ata.md)
는 경과 기간 링크별로 선택된 age-to-age 인자를 반환한다.
[`fit_ed()`](https://seokhoonj.github.io/lossratio/reference/fit_ed.md)
는 강도 인자 $`g_k = \Delta C^L_k / C^P_k`$ 를 반환한다. 두 출력 모두
아래 추정 방법의 입력으로 사용된다.

## 5단계 — 추정

[`fit_cl()`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md)
은 chain ladder 추정을 수행한다.

``` r

cl <- fit_cl(tri, value_var = "closs", method = "mack")
plot(cl, type = "projection")
summary(cl)
```

[`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md)
은 손해율 추정을 수행한다. 기본값 `method = "sa"` (단계
적응적(stage-adaptive)) 는 성숙점(maturity point) 이전에는 노출 기반
방식, 성숙점 이후에는 chain ladder 를 적용한다. 전환 지점은 ata 인자에서
그룹별로 탐지된 성숙점이다.

``` r

lr <- fit_lr(tri, method = "sa")
plot(lr, type = "clr")
summary(lr)
```

## 6단계 — 구조 변화 진단

[`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
은 최근 코호트가 이전 코호트와 다르게 행동하는지 검사한다. 동질적
부분집합에 한하여 손해율 적합을 수행하기 위한 전처리 단계로 활용된다.

``` r

sub <- build_triangle(exp[cv_nm == "SUR"], group_var = cv_nm)
detect_cohort_regime(sub, K = 12, method = "ecp")
```

상세 설명은
[`vignette("regime-detection")`](https://seokhoonj.github.io/lossratio/articles/regime-detection.md)
을 참고한다.

## 다음 단계

- [`vignette("aggregation-frameworks")`](https://seokhoonj.github.io/lossratio/articles/aggregation-frameworks.md)
  — `build_triangle`, `build_calendar`, `build_total` 의 사용 시점 비교.
- [`vignette("loss-ratio-methods")`](https://seokhoonj.github.io/lossratio/articles/loss-ratio-methods.md)
  —
  [`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md)
  에서 `"sa"` / `"ed"` / `"cl"` 중 선택 기준.
- [`vignette("chain-ladder")`](https://seokhoonj.github.io/lossratio/articles/chain-ladder.md)
  —
  [`fit_cl()`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md)
  심층 해설 (Mack 분산, tail factor).
- [`vignette("triangle-diagnostics")`](https://seokhoonj.github.io/lossratio/articles/triangle-diagnostics.md)
  —
  [`summary_ata()`](https://seokhoonj.github.io/lossratio/reference/summary_ata.md),
  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md),
  triangle 형식의 시각화.
- [`vignette("regime-detection")`](https://seokhoonj.github.io/lossratio/articles/regime-detection.md)
  —
  [`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  을 통한 코호트 간 구조 변화 진단.
- [`vignette("backtest")`](https://seokhoonj.github.io/lossratio/articles/backtest.md)
  — `fit_cl` 또는 `fit_lr` 을 사용한 대각선 홀드아웃(holdout) 검증.
