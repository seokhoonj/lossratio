# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.0.1.dev6] — 2026-05-10

### Added
- `Regime` result class and `Triangle.detect_regime()` method.
  Multivariate non-parametric divisive change-point detection
  via the E-Divisive procedure of Matteson & James (2014, JASA),
  plus a Ward hierarchical clustering fallback. The implementation
  follows the (tau, kappa) double-loop best-split of Algorithm 2
  in James & Matteson (2014, JSS) and is vectorised to O(n^2) via
  a 2D prefix-sum table.
- `lr.load_experience()` built-in synthetic dataset: 36 monthly
  cohorts × up to 36 dev months × 4 coverages (`CI`, `CAN`, `HOS`,
  `SUR`), generated deterministically. Per-coverage target LR,
  cohort premium volume mean / CV, and cell-level loss noise CV
  are calibrated to a real long-term Korean health portfolio (no
  real-data file is shipped). `SUR` carries a planted regime shift
  at cohort 2025-07.
- `scipy >= 1.10` runtime dependency (Ward hierarchical clustering
  and pairwise distances).

### Changed
- LR / Triangle.maturity / Maturity argument rename to mirror the
  R sibling and to read as "the maximum X tolerated":
  `theta_cv` → `max_cv`, `theta_rse` → `max_rse`, `m` → `min_run`.
  Default `max_cv` raised 0.10 → 0.15 to match R, so SA fits on
  cohort-scale portfolios pick the same `k_star` as the R sibling.
- Column rename: `cv_nm` → `coverage` everywhere (data,
  `Triangle.group_var`, examples). `2CI` value → `CI` so it is a
  letter-first identifier; documented as the two major non-cancer
  critical illnesses (cerebrovascular + ischemic heart disease;
  cancer is the separate `CAN` coverage).
- Summary column rename to suffix style for parity with R:
  `ultimate_loss` → `loss_ult`, `ultimate_exposure` →
  `premium_ult`, `ultimate_lr` → `lr_ult`,
  `latest_observed_dev` → `latest`. Matches the existing
  `loss_*` / `premium_*` / `lr_*` column families.
- README QuickStart now opens with `df = lr.load_experience()` and
  walks Experience → Triangle → LR fit → detect_regime → Backtest
  in one block. Adds an Install section with the polars-only and
  `[pandas]` extras.

### Fixed
- `_result_to_long_df` now passes `infer_schema_length=None` to
  `pl.DataFrame`, so backtest refits whose summary frame mixes
  `None` and `float` in the same column no longer fail polars'
  schema inference.

## [0.0.1.dev5] — 2026-05-09

### Changed
- **Column naming convention swept to mirror R `lossratio`**:
  cumulative is now the unmarked default (`loss`, `premium`, `lr`);
  per-period values carry an `_incr` (incremental) suffix (`loss_incr`,
  `premium_incr`, `lr_incr`). Old c-prefix forms (`closs`, `crp`,
  `clr`) are gone, including compound identifiers like `closs_obs`,
  `closs_proj`, `crp_obs`, `crp_proj`, `_build_closs_matrix`,
  `_build_crp_matrix` — all renamed to `loss_*` / `premium_*`.
- Raw `Experience` required columns: `loss` → `loss_incr`, `rp` →
  `premium_incr`. `REQUIRED_COLS = ("cym", "uym", "loss_incr",
  "premium_incr")`.
- README tagline aligned to R: "Loss ratio analytics for long-term
  health insurance — cohort development analysis, stage-adaptive
  projection, regime detection, and backtest validation."

### Added
- `Backtest` meta-estimator + `BacktestFit` result class —
  calendar-diagonal hold-out backtest of any fit estimator.

  ```python
  bt = lr.Backtest(estimator=lr.LR(method="sa"), holdout=6).fit(tri)
  bt.aeg              # per-cell: cohort, dev, calendar_idx, actual, predicted, aeg
  bt.col_summary      # aggregated by dev
  bt.diag_summary     # aggregated by calendar diagonal
  bt.fit              # the refitted estimator's result class instance
  ```

  Holds out the most recent ``holdout`` calendar diagonals (cells
  where ``cohort_idx + (dev - 1) > max_cal_idx - holdout``), refits
  the supplied estimator on the masked Triangle, and compares the
  projection to the original observed `loss` on the held-out cells.
  Supports `lr.CL`, `lr.ED`, and `lr.LR` (all three now produce a
  unified `loss_proj` column on `.df`). Per-group fitting when
  `Triangle.group_var` is set.

## [0.0.1.dev4] — 2026-05-07

### Added
- `Maturity` result class and `Triangle.maturity()` method.
  Detects the maturity point `k*` — the first development period at
  which the age-to-age factors are jointly stable (CV below
  `theta_cv` and RSE below `theta_rse`) for `m` consecutive links.
  Returns per-link diagnostics (`f`, `sigma2`, `cv`, `rse`,
  `stable`) and a `k_star` summary (single value when no group_var
  is set, or a `dict[group, k_star]` otherwise).
- `LR` estimator + `LRFit` result class — sklearn-style
  loss-ratio projection with three methods:
  - `method="sa"` (default): stage-adaptive — exposure-driven (ED)
    before the maturity point `k*`, chain ladder (CL) after.
    Maturity is detected internally via the same CV/RSE thresholds
    used by `Triangle.maturity()`. Falls back to ED throughout when
    `k*` is not detected.
  - `method="ed"`: ED projection only.
  - `method="cl"`: Mack chain ladder projection only.
  - The premium triangle is always projected forward via chain ladder
    on cumulative risk premium.
  - Output columns: `[group_var?, cohort, dev, closs, crp, loss_proj,
    exposure_proj, lr_proj, se_loss, se_lr, cv_lr]`.
  - `LRFit.summary()` returns per-cohort `ultimate_loss`,
    `ultimate_exposure`, `ultimate_lr`, `se_lr`, `cv_lr`.

## [0.0.1.dev3] — 2026-05-07

### Added
- `ED` estimator (exposure-driven, sklearn-style fit/predict pattern)
  and `EDFit` result class. The ED model anchors incremental loss to
  cumulative risk premium with an additive mean structure
  `E[Δloss | F] = g_k · crp`; cumulative loss is obtained by summing
  the projected increments, and loss ratio is computed downstream
  as projected cumulative loss divided by projected cumulative
  premium.
  - Pooled intensity per link (`g_k = Σ Δloss / Σ crp`, alpha = 1).
  - Per-link variance parameter (`sigma^2_g_k`) with the same
    Mack-style tail rule as CL when only one observation is available
    for the last link.
  - Cumulative premium projection via a separate chain ladder fit on
    the crp triangle (`f^P_k`); future incremental loss is then
    projected as `Δ̂loss = ĝ_k · Ĉ^P_k`.
  - Standard error on projected cumulative loss combining parameter
    risk and process risk additively (matching the ED phase variance
    recursion in the R sibling's paper).
  - `EDFit.summary()` returning per-cohort `ultimate`, `se_ultimate`,
    and `cv_ultimate`.
  - Per-group fitting when `Triangle.group_var` is set.
  - Output frame columns: `[group_var?, cohort, dev, closs, closs_proj,
    crp, crp_proj, se_proj]`.

## [0.0.1.dev2] — 2026-05-07

### Added
- `CL` estimator (Mack chain ladder, sklearn-style fit/predict pattern)
  and `CLFit` result class.
  - Volume-weighted ATA factor estimation (alpha = 1).
  - Per-link variance parameter (`sigma^2_k`) following Mack (1993),
    with the standard tail recommendation when only one observation
    is available for the last link.
  - Point projection of unobserved cells (`closs_proj`).
  - Mack standard error on projected ultimate combining parameter
    risk and process risk (`se_proj`).
  - `CLFit.summary()` returns per-cohort `ultimate`, `se_ultimate`,
    and `cv_ultimate`.
  - Per-group fitting when `Triangle.group_var` is set (each group
    fitted independently).
- `numpy >= 1.24` as a required runtime dependency (used for the
  Mack matrix recursion).

## [0.0.1.dev1] — 2026-05-07

### Added
- `Experience` class — entry point for loss-ratio analysis. Validates
  experience data (required columns: `cym`, `uym`, `loss`, `rp`),
  coerces dates and numeric columns, and serves as the source for
  downstream classes.
- `Triangle` class — cohort × dev aggregated experience. Built via
  `experience.triangle(group_var=...)` (method chain) or directly
  `lossratio.Triangle(experience)`. Computes incremental loss/rp,
  cumulative `closs`/`crp`, and ratios `lr`/`clr` per
  (group, cohort) lane. Supports `dev_unit` of `"month"`,
  `"quarter"`, `"half"`, or `"year"`.
- Both classes accept polars or pandas DataFrames as input. Internal
  storage is polars for performance; the `df` property mirrors the
  original input format (pandas in → pandas out, polars in → polars
  out). Explicit `.to_polars()` and `.to_pandas()` methods are
  available; `_output_type` propagates through method chains.
- `polars >= 1.0` as a required runtime dependency.
- `pandas >= 2.0` and `pyarrow >= 10` as optional dependencies for
  users who prefer pandas inputs/outputs (`pyarrow` is required by
  `polars.to_pandas()`).

### Changed
- Development Status classifier moved from "1 - Planning" to
  "2 - Pre-Alpha" — first working code is in the package.
- Project description updated to "Python sibling of the R lossratio
  package" framing.

## [0.0.1.dev0] — 2026-05-06

### Added
- Initial PyPI placeholder reserving the `lossratio` name.
- `__version__` attribute.
- README pointing to the R package and the upcoming Python port.
