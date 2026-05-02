# Fit chain ladder projection from a `Triangle` object

Fit a chain ladder projection from an object of class `"Triangle"`. The
function works on long-form cumulative data and does not require a
complete triangle.

Two methods are supported via the `method` argument:

- `"basic"` (default):

  Classical chain ladder point projection. Age-to-age factors are
  estimated through
  [`build_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ata.md)
  and
  [`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md),
  then applied recursively.

- `"mack"`:

  Mack (1993) chain ladder. Point forecast follows the standard
  recursion, and prediction uncertainty is decomposed into process
  variance and parameter variance.

When `weight_var` is supplied (e.g. `"crp"`), age-to-age factors and
their variance are estimated using the supplied WLS weights.

## Usage

``` r
fit_cl(
  x,
  method = c("basic", "mack"),
  value_var = "closs",
  weight_var = NULL,
  alpha = 1,
  sigma_method = c("min_last2", "locf", "loglinear"),
  recent = NULL,
  maturity_args = NULL,
  tail = FALSE
)
```

## Arguments

- x:

  An object of class `"Triangle"`.

- method:

  One of `"basic"` or `"mack"`. Default is `"basic"`.

- value_var:

  A single cumulative variable to project. Typical choices are
  `"closs"`, `"crp"`, or `"clr"`.

- weight_var:

  An optional column name passed to
  [`build_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ata.md)
  as the WLS weight variable. Typically `"crp"` when
  `value_var = "clr"`. Default is `NULL`.

- alpha:

  Numeric scalar controlling the variance structure in
  [`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md).
  Default is `1`.

- sigma_method:

  Sigma extrapolation method passed to
  [`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md).
  One of `"min_last2"` (default), `"locf"`, or `"loglinear"`. Only
  relevant when `method = "mack"`.

- recent:

  Optional positive integer. When supplied, only the most recent
  `recent` periods are used for factor estimation. Default is `NULL`
  (use all periods).

- maturity_args:

  A named list of arguments forwarded to
  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md)
  via
  [`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md),
  or `NULL` (default) to skip maturity filtering. Pass
  [`list()`](https://rdrr.io/r/base/list.html) to use all defaults with
  maturity filtering enabled.

- tail:

  Logical or numeric. If `FALSE`, no tail factor is applied. If `TRUE`,
  a log-linear tail factor is estimated from selected factors. If
  numeric, the supplied value is used as the tail factor.

## Value

An object of class `"CLFit"` containing:

- `call`:

  The matched call.

- `data`:

  The input `"Triangle"` object.

- `method`:

  The method used (`"basic"` or `"mack"`).

- `group_var`:

  Character vector of grouping variable names.

- `cohort_var`:

  Character scalar of period variable name.

- `dev_var`:

  Character scalar of development variable name.

- `value_var`:

  Character scalar of value variable name.

- `full`:

  `data.table` with observed and projected values. For `"mack"`, also
  includes process/parameter SE and CV columns.

- `pred`:

  `data.table` identical to `full` with observed cells set to `NA`.

- `ata`:

  The `"ATA"` object produced by
  [`build_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ata.md).

- `summary`:

  For `"basic"`: `data.table` of fitted factors from
  [`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md).
  For `"mack"`: cohort-level summary with latest, ultimate, reserve, and
  Mack standard errors.

- `selected`:

  `data.table` of selected factors used for projection.

- `factor`:

  For `"mack"` only: `data.table` of fitted factors from
  [`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md).

- `maturity`:

  Maturity diagnostics from
  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md),
  or `NULL` when maturity filtering was not applied.

- `alpha`:

  Value of `alpha` used.

- `sigma_method`:

  For `"mack"` only: sigma extrapolation method.

- `weight_var`:

  Weight variable name used, or `NULL`.

- `recent`:

  Number of recent periods used, or `NULL`.

- `use_maturity`:

  Logical; whether maturity filtering was applied.

- `maturity_args`:

  Resolved maturity arguments, or `NULL`.

- `tail`:

  Tail factor argument supplied by the user.

- `tail_factor`:

  Numeric tail factor applied.

## See also

[`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md),
[`fit_lr()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_lr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(experience)
exp <- as_experience(experience)
tri <- build_triangle(exp[cv_nm == "SUR"], group_var = cv_nm)

# Basic chain ladder (point projection only)
cl <- fit_cl(tri, value_var = "closs", method = "basic")
print(cl)

# Mack chain ladder with process / parameter standard errors
cl_mack <- fit_cl(tri, value_var = "closs", method = "mack")
summary(cl_mack)
plot(cl_mack)

# WLS factors for clr (loss ratio) using crp as the weight
cl_clr <- fit_cl(tri, value_var = "clr", weight_var = "crp")
} # }
```
