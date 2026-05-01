# Backtest a chain ladder / loss ratio fit on existing data

Hold out the latest `holdout` calendar diagonals from the input
`triangle`, refit the model on the earlier portion, project the held-out
cells, and compare the projection to the actual values that were
withheld.

The Actual-Expected Gap (AEG) is computed cell-wise as \$\$aeg =
\frac{value\_{proj}}{value\_{actual}} - 1\$\$ and aggregated by
development period (`col_summary`) and by calendar diagonal
(`diag_summary`).

## Usage

``` r
backtest(x, holdout = 6L, fit_fn = fit_cl, value_var = "closs", ...)

# S3 method for class 'backtest'
print(x, ...)

# S3 method for class 'backtest'
summary(object, ...)

# S3 method for class 'summary.backtest'
print(x, ...)
```

## Arguments

- x:

  A `"triangle"` object (or a `"backtest"` object for the S3
  [`print()`](https://rdrr.io/r/base/print.html) method).

- holdout:

  Integer. Number of latest calendar diagonals to mask before refitting.
  Default `6L`.

- fit_fn:

  Fitting function. Supported: `fit_cl` (default) and `fit_lr`. If
  `fit_fn` does not have a `value_var` formal (as is the case for
  `fit_lr`), `value_var` is used only to select the comparison column on
  the fit's `$full` table; arguments for the fitter itself (e.g.,
  `loss_var`, `exposure_var`, `method`) are passed through `...`.

- value_var:

  Character scalar. Column to project and compare. For `fit_cl`, any
  column present in `x` (default `"closs"`). For `fit_lr`, must be one
  of `"closs"`, `"crp"`, or `"clr"`, which map to `loss_proj`,
  `exposure_proj`, and `clr_proj` respectively on `fit_lr$full`.

- ...:

  Additional arguments passed to `fit_fn` (e.g., `method`, `alpha`,
  `recent`, `tail`).

- object:

  A `"backtest"` object. Used by the S3
  [`summary()`](https://rdrr.io/r/base/summary.html) method.

## Value

An object of class `"backtest"` with components:

- `call`:

  Matched call.

- `data`:

  Original `triangle`.

- `masked`:

  Triangle used for fitting (with held-out cells removed).

- `fit`:

  The fit object returned by `fit_fn`.

- `aeg`:

  `data.table` of held-out cells with columns
  `(group_var, cohort, dev, value_actual, value_pred, aeg, calendar_idx)`.

- `col_summary`:

  Per-`dev` aggregate AEG (mean / median / weighted / n).

- `diag_summary`:

  Per-calendar-diagonal aggregate AEG.

- `value_var`, `holdout`, `fit_fn_name`:

  Call metadata.

- `group_var`, `cohort_var`, `dev_var`:

  Variable name relays from `x`.

## See also

[`fit_cl()`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md),
[`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md),
[`plot.backtest()`](https://seokhoonj.github.io/lossratio/reference/plot.backtest.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(experience)
exp <- as_experience(experience)
tri <- build_triangle(exp, group_var = cv_nm)
bt <- backtest(tri, holdout = 6L, value_var = "closs", method = "mack")
print(bt)
summary(bt)
plot(bt)
} # }
```
