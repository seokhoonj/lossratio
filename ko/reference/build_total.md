# Build a total development summary from experience data

Aggregate `loss` and `rp` by group and compute the corresponding total
loss ratio over a selected period window.

This function is intended for high-level portfolio comparison across
groups such as products, coverages, or channels. It summarises:

- the number of observed periods (`n_obs`)

- the first and last observed periods (`sales_start`, `sales_end`)

- total `loss` and total `rp`

- total loss ratio (`lr = loss / rp`)

- each group's share of total loss and risk premium

If `period_from` and/or `period_to` are supplied, the input data are
first restricted to that period window before aggregation. This is
useful when comparing groups on a common period basis.

## Usage

``` r
build_total(
  df,
  group_var,
  cohort_var = "uym",
  dev_var = "elap_m",
  value_var = c("loss", "rp"),
  period_from = NULL,
  period_to = NULL,
  fill_gaps = FALSE
)
```

## Arguments

- df:

  A data.frame containing experience data.

- group_var:

  Grouping variable(s).

- cohort_var:

  A single period variable. This may be an underwriting period (`uym`,
  `uyq`, `uyh`, `uy`) or a calendar period (`cym`, `cyq`, `cyh`, `cy`).
  Default `"uym"`.

- dev_var:

  A single development variable used to count observed periods. Default
  `"elap_m"`.

- value_var:

  Value variables to aggregate. Must include both `"loss"` and `"rp"`.
  Default `c("loss", "rp")`.

- period_from:

  Optional lower bound for `cohort_var`. Only rows with
  `cohort_var >= period_from` are kept. May be supplied as `Date`,
  character, or any value coercible to `Date`. Default `NULL`.

- period_to:

  Optional upper bound for `cohort_var`. Only rows with
  `cohort_var <= period_to` are kept. May be supplied as `Date`,
  character, or any value coercible to `Date`. Default `NULL`.

- fill_gaps:

  Logical; if `TRUE`, zero-fill missing
  `(group_var, cohort_var, dev_var)` cells before aggregation so that
  every cohort has a consecutive `dev_var` sequence. Default `FALSE`.
  Note that filling inflates `n_obs` (counts filled rows as observed
  periods); use
  [`validate_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/validate_triangle.md)
  to inspect first.

## Value

A data.frame with class `"Total"` containing:

- n_obs:

  Number of observed development periods

- sales_start:

  First observed period

- sales_end:

  Last observed period

- loss:

  Total loss

- rp:

  Total risk premium

- lr:

  Total loss ratio (`loss / rp`)

- loss_prop:

  Share of total loss

- rp_prop:

  Share of total risk premium

## Examples

``` r
if (FALSE) { # \dontrun{
build_total(df, cv_nm)

build_total(
  df,
  cv_nm,
  period_from = "2023-01-01",
  period_to   = "2023-12-01"
)
} # }
```
