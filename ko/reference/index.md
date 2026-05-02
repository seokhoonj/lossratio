# Package index

## 입력 계층

원시 experience 데이터의 검증·코어션 헬퍼.

- [`check_experience()`](https://seokhoonj.github.io/lossratio/ko/reference/check_experience.md)
  : Check an experience dataset

- [`is_experience()`](https://seokhoonj.github.io/lossratio/ko/reference/is_experience.md)
  :

  Check whether an object is an `Experience`

- [`as_experience()`](https://seokhoonj.github.io/lossratio/ko/reference/as_experience.md)
  :

  Coerce a dataset to an `Experience` object

- [`add_experience_period()`](https://seokhoonj.github.io/lossratio/ko/reference/add_experience_period.md)
  : Add standard period variables to an experience dataset

- [`validate_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/validate_triangle.md)
  : Validate triangle structure before building a development

## 집계 빌더

같은 long-format experience 데이터를 보는 세 가지 프레임워크 — cohort ×
dev (`triangle`), 달력 기간 (`calendar`), 포트폴리오 전체 (`total`).

- [`build_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/build_triangle.md)
  : Build a development structure from experience data
- [`build_calendar()`](https://seokhoonj.github.io/lossratio/ko/reference/build_calendar.md)
  : Build a calendar-based development structure from experience data
- [`build_total()`](https://seokhoonj.github.io/lossratio/ko/reference/build_total.md)
  : Build a total development summary from experience data

## Age-to-age (ATA) 인자

chain ladder 방법의 기본 단위.

- [`build_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ata.md)
  :

  Build age-to-age (ata) factors from `Triangle` data

- [`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md)
  : Fit age-to-age development factors

- [`summary(`*`<ATA>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary.ATA.md)
  : Summarise age-to-age factor statistics

- [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md)
  : Find ata maturity by group

## 노출 기반 (ED) 강도

노출 기반 방법의 기본 단위.

- [`build_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ed.md)
  : Build exposure-driven development data
- [`fit_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ed.md)
  : Fit ED intensity factors
- [`summary(`*`<ED>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary.ED.md)
  : Summarise ED intensity statistics

## 추정

chain ladder 와 손해율 추정. `fit_lr` 은 세 가지 method 지원 — `"sa"`
(단계 적응적, default), `"ed"`, `"cl"`.

- [`fit_cl()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_cl.md)
  :

  Fit chain ladder projection from a `Triangle` object

- [`fit_lr()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_lr.md)
  : Fit loss ratio projection model

## Regime 탐지

인수 코호트 간 구조적 변화 탐지.

- [`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  [`print(`*`<CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  [`summary(`*`<CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  [`print(`*`<summary.CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  : Detect structural regime shifts across underwriting cohorts

## Backtest

Triangle 의 최근 대각선을 보류한 뒤 재적합·예측을 보류된 실제값과 비교.

- [`backtest()`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  [`print(`*`<Backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  [`summary(`*`<Backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  [`print(`*`<summary.Backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  : Backtest a loss-ratio / chain ladder fit on existing data

## 시각화

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) (base generic)
과
[`plot_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.md)
(lossratio generic) 이 객체 클래스에 따라 dispatch.

- [`plot_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.md)
  : Triangle plot generic
- [`plot(`*`<ATA>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.ATA.md)
  : Plot age-to-age factor diagnostics
- [`plot(`*`<ATAFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.ATAFit.md)
  : Plot an ata fit
- [`plot(`*`<Backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.Backtest.md)
  : Plot a backtest object
- [`plot(`*`<CLFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.CLFit.md)
  : Plot a chain ladder fit
- [`plot(`*`<Calendar>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.Calendar.md)
  : Plot calendar-based development statistics
- [`plot(`*`<CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.CohortRegime.md)
  : Plot a cohort regime detection result
- [`plot(`*`<ED>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.ED.md)
  : Plot ED intensity diagnostics
- [`plot(`*`<EDFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.EDFit.md)
  : Plot an ED fit
- [`plot(`*`<LRFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.LRFit.md)
  : Plot a loss ratio fit
- [`plot(`*`<Triangle>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.Triangle.md)
  : Plot development trajectories with optional summary overlay
- [`plot_triangle(`*`<ATA>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ATA.md)
  : Plot ata factors as a triangle heatmap table
- [`plot_triangle(`*`<ATAFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ATAFit.md)
  : Triangle heatmap for an ata fit
- [`plot_triangle(`*`<Backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.Backtest.md)
  : Triangle heatmap of backtest AEG
- [`plot_triangle(`*`<CLFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.CLFit.md)
  : Plot chain ladder results as a triangle table
- [`plot_triangle(`*`<ED>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ED.md)
  : Plot ED intensities as a triangle heatmap table
- [`plot_triangle(`*`<EDFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.EDFit.md)
  : Triangle heatmap for an ED fit
- [`plot_triangle(`*`<LRFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.LRFit.md)
  : Plot loss ratio projection as a triangle heatmap
- [`plot_triangle(`*`<Triangle>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.Triangle.md)
  : Plot development values as a triangle table

## 기타 S3 메서드

패키지 클래스에 등록된 print / summary / longer 메서드.

- [`backtest()`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  [`print(`*`<Backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  [`summary(`*`<Backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  [`print(`*`<summary.Backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  : Backtest a loss-ratio / chain ladder fit on existing data

- [`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  [`print(`*`<CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  [`summary(`*`<CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  [`print(`*`<summary.CohortRegime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  : Detect structural regime shifts across underwriting cohorts

- [`print(`*`<ATAFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/print.ATAFit.md)
  :

  Print an `ATAFit` object

- [`print(`*`<CLFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/print.CLFit.md)
  :

  Print a `CLFit` object

- [`print(`*`<EDFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/print.EDFit.md)
  :

  Print an `EDFit` object

- [`print(`*`<LRFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/print.LRFit.md)
  :

  Print an `LRFit` object

- [`summary(`*`<ATA>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary.ATA.md)
  : Summarise age-to-age factor statistics

- [`summary(`*`<CLFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary.CLFit.md)
  :

  Summary method for `CLFit`

- [`summary(`*`<ED>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary.ED.md)
  : Summarise ED intensity statistics

- [`summary(`*`<EDFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary.EDFit.md)
  :

  Summary method for `EDFit`

- [`summary(`*`<LRFit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary.LRFit.md)
  :

  Summary method for `LRFit`

- [`summary(`*`<Triangle>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary.Triangle.md)
  : Summarise development statistics (Mean, Median, Weighted)

## 헬퍼

- [`get_recent_weights()`](https://seokhoonj.github.io/lossratio/ko/reference/get_recent_weights.md)
  : Recent-diagonal weights for a development triangle
- [`longer()`](https://seokhoonj.github.io/lossratio/ko/reference/longer.md)
  : Reshape an object to long form (S3 generic)

## 데이터셋

- [`experience`](https://seokhoonj.github.io/lossratio/ko/reference/experience.md)
  : Sample loss experience data
