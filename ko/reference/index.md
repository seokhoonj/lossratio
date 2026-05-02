# Package index

## 입력 계층

원시 experience 데이터의 검증·코어션 헬퍼.

- [`check_experience()`](https://seokhoonj.github.io/lossratio/ko/reference/check_experience.md)
  : Check an experience dataset

- [`is_experience()`](https://seokhoonj.github.io/lossratio/ko/reference/is_experience.md)
  :

  Check whether an object is an `experience`

- [`as_experience()`](https://seokhoonj.github.io/lossratio/ko/reference/as_experience.md)
  :

  Coerce a dataset to an `experience` object

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

  Build age-to-age (ata) factors from `triangle` data

- [`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md)
  : Fit age-to-age development factors

- [`summary_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ata.md)
  [`summary(`*`<ata>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ata.md)
  : Summarise age-to-age factor statistics

- [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md)
  : Find ata maturity by group

## 노출 기반 (ED) 강도

노출 기반 방법의 기본 단위.

- [`build_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ed.md)
  : Build exposure-driven development data
- [`fit_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ed.md)
  : Fit ED intensity factors
- [`summary_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ed.md)
  [`summary(`*`<ed>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ed.md)
  : Summarise ED intensity statistics

## 추정

chain ladder 와 손해율 추정. `fit_lr` 은 세 가지 method 지원 — `"sa"`
(단계 적응적, default), `"ed"`, `"cl"`.

- [`fit_cl()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_cl.md)
  :

  Fit chain ladder projection from a `triangle` object

- [`fit_lr()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_lr.md)
  : Fit loss ratio projection model

## Regime 탐지

인수 코호트 간 구조적 변화 탐지.

- [`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  [`print(`*`<cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  [`summary(`*`<cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  [`print(`*`<summary.cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  : Detect structural regime shifts across underwriting cohorts

## Backtest

Triangle 의 최근 대각선을 보류한 뒤 재적합·예측을 보류된 실제값과 비교.

- [`backtest()`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  [`print(`*`<backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  [`summary(`*`<backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  [`print(`*`<summary.backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  : Backtest a loss-ratio / chain ladder fit on existing data

## 시각화

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) (base generic)
과
[`plot_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.md)
(lossratio generic) 이 객체 클래스에 따라 dispatch.

- [`plot_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.md)
  : Triangle plot generic
- [`plot(`*`<ata>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.ata.md)
  : Plot age-to-age factor diagnostics
- [`plot(`*`<ata_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.ata_fit.md)
  : Plot an ata fit
- [`plot(`*`<backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.backtest.md)
  : Plot a backtest object
- [`plot(`*`<calendar>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.calendar.md)
  : Plot calendar-based development statistics
- [`plot(`*`<cl_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.cl_fit.md)
  : Plot a chain ladder fit
- [`plot(`*`<cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.cohort_regime.md)
  : Plot a cohort regime detection result
- [`plot(`*`<ed>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.ed.md)
  : Plot ED intensity diagnostics
- [`plot(`*`<ed_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.ed_fit.md)
  : Plot an ED fit
- [`plot(`*`<lr_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.lr_fit.md)
  : Plot a loss ratio fit
- [`plot(`*`<triangle>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot.triangle.md)
  : Plot development trajectories with optional summary overlay
- [`plot_triangle(`*`<ata>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ata.md)
  : Plot ata factors as a triangle heatmap table
- [`plot_triangle(`*`<ata_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ata_fit.md)
  : Triangle heatmap for an ata fit
- [`plot_triangle(`*`<backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.backtest.md)
  : Triangle heatmap of backtest AEG
- [`plot_triangle(`*`<cl_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.cl_fit.md)
  : Plot chain ladder results as a triangle table
- [`plot_triangle(`*`<ed>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ed.md)
  : Plot ED intensities as a triangle heatmap table
- [`plot_triangle(`*`<ed_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.ed_fit.md)
  : Triangle heatmap for an ED fit
- [`plot_triangle(`*`<lr_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.lr_fit.md)
  : Plot loss ratio projection as a triangle heatmap
- [`plot_triangle(`*`<triangle>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.triangle.md)
  : Plot development values as a triangle table

## 기타 S3 메서드

패키지 클래스에 등록된 print / summary / longer 메서드.

- [`backtest()`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  [`print(`*`<backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  [`summary(`*`<backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  [`print(`*`<summary.backtest>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
  : Backtest a loss-ratio / chain ladder fit on existing data

- [`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  [`print(`*`<cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  [`summary(`*`<cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  [`print(`*`<summary.cohort_regime>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  : Detect structural regime shifts across underwriting cohorts

- [`print(`*`<ata_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/print.ata_fit.md)
  :

  Print an `ata_fit` object

- [`print(`*`<cl_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/print.cl_fit.md)
  :

  Print a `cl_fit` object

- [`print(`*`<ed_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/print.ed_fit.md)
  :

  Print an `ed_fit` object

- [`print(`*`<lr_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/print.lr_fit.md)
  :

  Print an `lr_fit` object

- [`summary(`*`<cl_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary.cl_fit.md)
  :

  Summary method for `cl_fit`

- [`summary(`*`<ed_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary.ed_fit.md)
  :

  Summary method for `ed_fit`

- [`summary(`*`<lr_fit>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary.lr_fit.md)
  :

  Summary method for `lr_fit`

- [`summary(`*`<triangle>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary.triangle.md)
  : Summarise development statistics (Mean, Median, Weighted)

- [`summary_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ata.md)
  [`summary(`*`<ata>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ata.md)
  : Summarise age-to-age factor statistics

- [`summary_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ed.md)
  [`summary(`*`<ed>`*`)`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ed.md)
  : Summarise ED intensity statistics

## 헬퍼

- [`get_recent_weights()`](https://seokhoonj.github.io/lossratio/ko/reference/get_recent_weights.md)
  : Recent-diagonal weights for a development triangle
- [`longer()`](https://seokhoonj.github.io/lossratio/ko/reference/longer.md)
  : Reshape an object to long form (S3 generic)

## 데이터셋

- [`experience`](https://seokhoonj.github.io/lossratio/ko/reference/experience.md)
  : Sample loss experience data
