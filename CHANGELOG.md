# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
