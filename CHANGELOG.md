# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Breaking

- Renamed `add_experience_period(df)` to `derive_grain_columns(df)`
  and moved it from `lossratio.experience` to `lossratio._period`.
  The new name is honest about the function's role: it *derives*
  the M / Q / S / A grain sibling columns (`uy_a` / `uy_s` / `uy_q`,
  `cy_a` / `cy_s` / `cy_q`, `dev_a` / `dev_s` / `dev_q`) from the
  monthly source columns (`uy_m` / `cy_m`), rather than just adding
  arbitrary "period" variables. Public API remains a single
  top-level import: `from lossratio import derive_grain_columns`.

  Migration: `lr.add_experience_period(df)` → `lr.derive_grain_columns(df)`.
  The function signature and behavior are unchanged.

### Internal

- The `lossratio.experience` module now only hosts
  `validate_experience`. Grain derivation moved next to its
  domain-neutral peers in `lossratio._period`.

## [0.0.1.dev9] — 2026-05-11

### Breaking

- Renamed raw experience column letter family from M/Q/H/Y (Month /
  Quarter / Half / Year) to **M/Q/S/A** (Month / Quarter / Semi-annual
  / Annual). The latter is a `-ly` adverb word family that avoids
  conflict with pandas' `H = hour` convention. New shipped column
  names: `coverage, uy_m, cy_m, dev_m, loss_incr, premium_incr`
  (previously `uym, cym`). `add_experience_period(df)` now derives
  the full 12-column enrichment: `uy_a/uy_s/uy_q/uy_m`,
  `cy_a/cy_s/cy_q/cy_m`, `dev_a/dev_s/dev_q/dev_m`. All `uy_*` and
  `cy_*` are Date; all `dev_*` are integer counts.

- New `grain` argument on `Triangle()` (replaces `granularity`):
  values `"auto"` (default — auto-detect from data spacing), `"M"`,
  `"Q"`, `"S"`, `"A"`. Single-letter codes mirror the column suffix
  (`grain="Q"` ↔ `dev_q` / `cy_q` / `uy_q`). `tri.grain` property
  returns the active code.

- Triangle now auto-coerces `cohort_var` / `cym_var` to Date,
  accepting integer (`yyyy` / `yyyymm` / `yyyymmdd`), ISO string
  (`"YYYY-MM-DD"`, `"YYYY/MM/DD"`, `"YYYYMMDD"`, with optional
  `HH:MM:SS` time suffix), or existing Date / Datetime. Auto-detects
  input grain from value spacing; `grain` arg can request a coarser
  view (validated — cannot decompose).

  Migration:
    Before (dev8): lr.Triangle(df, granularity="quarter")
    After  (dev9): lr.Triangle(df, grain="Q")

  Date-coercion + grain inference are exposed as reusable helpers
  in `lossratio._period` (`coerce_cols_to_date`, `infer_grain`,
  `validate_grain`, `resolve_grain`, `floor_to_period`,
  `floor_cols_to_period`, `count_periods`, `GRAIN_ORDER`).

## [0.0.1.dev8] — 2026-05-10

### Breaking

- Renamed raw experience data columns from `elap_y/h/q/m` to
  `dev_y/h/q/m` to match R `lossratio`'s 2026-05-10 sweep. The
  "elap" prefix was an etymology word ("elapsed") rather than the
  concept word; `dev_*` aligns raw column names with the abstract
  `dev` axis used by Triangle and matches ChainLadder-style framing.
  Migration:
    Before: lr.Triangle(df, dev_var="elap_m")
    After:  lr.Triangle(df, dev_var="dev_m")
  Mapping: `"elap_m"` → `"dev_m"`, `"elap_q"` → `"dev_q"`,
  `"elap_h"` → `"dev_h"`, `"elap_y"` → `"dev_y"`. The default
  `dev_var` is now `"dev_m"`. `add_experience_period(df)` derives
  `dev_y/h/q/m` columns. R↔Python parity restored.

## [0.0.1.dev7] — 2026-05-10

### Added
- **`Triangle.link()`** returning :class:`Link`: long-format link
  table — one row per (cohort, adjacent dev pair) — with per-cell
  ``ata`` and (in dual mode) per-cell ``intensity``. Mirrors R's
  ``Link`` data class.
- :class:`Link` has methods :meth:`Link.ata` and
  :meth:`Link.intensity` for paired factor-level diagnostics, plus
  ``.df`` for the raw long-format table.
- :class:`ATA` (per-link Mack-pooled ``f_k``, cross-cohort CV, RSE,
  sigma^2, n_obs) and :class:`Intensity` (per-link WLS ``g_k``,
  ``g_se``, sigma^2, n_obs) result classes, exposed via
  ``Link.ata()`` / ``Link.intensity()``. ED has no maturity-point
  analogue (g_k decays to zero, breaking CV/RSE).

The single canonical chain for factor-level diagnostics:

```python
tri.link()                          # → Link
   .ata()                           # → ATA       (multiplicative)
   .intensity()                     # → Intensity (additive)
   .ata().maturity(max_cv=0.15)     # → Maturity  (stability detection)
```

### Changed (R-parity, breaking)
- **Removed** `Triangle.ata()`, `Triangle.intensity()`, and
  `Triangle.maturity()` shortcut methods. Use the explicit chain
  via ``triangle.link()`` (see above). Rationale: a single
  canonical path makes the dependency structure explicit (Maturity
  is a post-processing step *on top of* the ATA factor diagnostic;
  ATA and Intensity are paired factor diagnostics derived from the
  shared :class:`Link` intermediate). "Build once, summarise twice"
  is now naturally available — call ``link.ata()`` and
  ``link.intensity()`` on the same ``Link`` without recomputing.

  *Migration*: replace
  - ``tri.ata(...)`` → ``tri.link().ata(...)``
  - ``tri.intensity(...)`` → ``tri.link().intensity(...)``
  - ``tri.maturity(...)`` → ``tri.link().ata().maturity(...)``

- `BacktestFit.aeg` property renamed to **`BacktestFit.ae_err`** and
  the underlying formula switched from a literal `actual - predicted`
  gap to the standard A/E convention
  **`ae_err = actual / predicted - 1`** (signed relative error).
  Positive values now flag under-projection (model under-estimated;
  actual exceeded the projection); negative values flag
  over-projection. Aggregations in `col_summary` / `diag_summary`
  follow R lossratio:
  - `n` (count)
  - `ae_err_mean` (mean of cell-level A/E - 1)
  - `ae_err_med` (median)
  - `ae_err_wt = sum(actual - predicted) / sum(predicted)`
    (exposure-weighted pooled A/E - 1)
- The previous `sum_actual` / `sum_predicted` / `sum_aeg` aggregation
  columns are removed (recoverable from cell-level `bt.ae_err` if
  needed).

This is a breaking change for callers that read `bt.aeg`,
`bt.col_summary["sum_aeg"]`, etc. The new column / property names
match the R sibling's `ae_err` family.

- Removed the `Experience` class. Schema validation and derived-column
  enrichment (cy / cyh / cyq / uy / uyh / uyq / elap_y / elap_h / elap_q /
  elap_m) now happen inside `Triangle.__init__`. Migration:
    Before: `lr.Experience(df).triangle(group_var=...)`
    After:  `lr.Triangle(df, group_var=...)`
  `add_experience_period(df)` and `validate_experience(df)` remain as
  module-level helpers for users who want an explicit pre-validation
  step. The class wrapper added friction for ad-hoc filter/transform
  workflows in polars/pandas without offsetting value.

- Renamed `Triangle()`'s `dev_unit` argument to `dev_var` (R parity —
  R's `build_triangle()` uses `dev_var = "elap_m"`). The new argument
  takes a column name (e.g., `"elap_m"` / `"elap_q"` / `"elap_h"` /
  `"elap_y"`) instead of a granularity string. Migration:
    Before: `lr.Triangle(df, dev_unit="month")`
    After:  `lr.Triangle(df, dev_var="elap_m")`
  Mapping: `"month"` → `"elap_m"`, `"quarter"` → `"elap_q"`,
  `"half"` → `"elap_h"`, `"year"` → `"elap_y"`. Triangle exposes
  `dev_var` (column name) and `dev_type` (granularity) attributes,
  mirroring R's two-attribute pattern.

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
