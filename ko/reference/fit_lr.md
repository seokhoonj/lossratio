# Fit loss ratio projection model

Unified interface for loss ratio projection from a `"Triangle"` object.
Three projection methods are available:

- `"sa"` (default):

  Uses exposure-driven (ED) estimation before maturity and chain
  ladder (CL) after maturity.

  - Before maturity: age-to-age factors are volatile, so exposure-driven
    projection \\\Delta C^L = g_k \cdot C^P_k\\ anchors the estimate to
    premium volume.

  - After maturity: age-to-age factors are stable, so chain ladder
    projection \\C^L\_{k+1} = f_k \cdot C^L_k\\ preserves the cohort's
    observed level.

- `"ed"`:

  Exposure-driven for all development periods. All future increments are
  \\g_k \cdot C^P_k\\.

- `"cl"`:

  Chain ladder for all development periods. Equivalent to classical Mack
  model.

In all cases, exposure is projected forward using chain ladder:
\$\$\hat{C}^P\_{i,k+1} = f^P_k \cdot \hat{C}^P\_{i,k}\$\$

## Usage

``` r
fit_lr(
  x,
  method = c("sa", "ed", "cl"),
  loss_var = "closs",
  exposure_var = "crp",
  loss_alpha = 1,
  exposure_alpha = 1,
  delta_method = c("simple", "full"),
  rho = 0,
  conf_level = 0.95,
  sigma_method = c("min_last2", "locf", "loglinear"),
  recent = NULL,
  maturity_args = NULL,
  bootstrap = FALSE,
  B = 1000,
  seed = NULL
)
```

## Arguments

- x:

  An object of class `"Triangle"`.

- method:

  One of `"sa"`, `"ed"`, or `"cl"`. Default is `"sa"`.

- loss_var:

  Cumulative loss variable. Default is `"closs"`.

- exposure_var:

  Cumulative exposure variable. Default is `"crp"`.

- loss_alpha:

  Numeric scalar controlling the variance structure for loss estimation.
  Default is `1`.

- exposure_alpha:

  Numeric scalar for exposure chain ladder. Default is `1`.

- delta_method:

  Method for computing `se_clr = SE(L/E)`. One of:

  `"simple"` (default)

  :   `se_clr = se_proj / exposure_proj`, treats exposure as fixed.

  `"full"`

  :   Full delta method with exposure uncertainty and loss-exposure
      correlation: \$\$\mathrm{Var}(L/E) \approx
      \frac{\mathrm{Var}(L)}{E^2} + \frac{L^2 \mathrm{Var}(E)}{E^4} -
      \frac{2 \rho L \mathrm{SE}(L) \mathrm{SE}(E)}{E^3}\$\$

- rho:

  Numeric scalar in `(-1, 1)`; assumed correlation between ultimate loss
  and ultimate exposure. Only used when `delta_method = "full"`. Default
  is `0`.

- conf_level:

  Confidence level used for `ci_lower`/`ci_upper` in the cohort summary.
  Default is `0.95`.

- sigma_method:

  Sigma extrapolation method. One of `"min_last2"` (default), `"locf"`,
  or `"loglinear"`.

- recent:

  Optional positive integer for estimation window. Default is `NULL`.

- maturity_args:

  A named list forwarded to
  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md),
  or `NULL` (default) to skip maturity filtering. When `method = "sa"`,
  this also determines the switch point between ED and CL. Pass
  [`list()`](https://rdrr.io/r/base/list.html) to use all defaults.

- bootstrap:

  Logical; if `TRUE`, parameter and process variance are derived via
  residual bootstrap rather than the analytical delta method. Default is
  `FALSE`.

- B:

  Integer number of bootstrap replications. Used only when
  `bootstrap = TRUE`. Default is `1000`.

- seed:

  Optional integer seed for reproducible bootstrap. Default is `NULL`.

## Value

An object of class `"LRFit"`.

## See also

[`build_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/build_triangle.md),
[`build_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ata.md),
[`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md),
[`build_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ed.md),
[`fit_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ed.md),
[`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md)
