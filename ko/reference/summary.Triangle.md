# Summarise development statistics (Mean, Median, Weighted)

S3 method for [`summary()`](https://rdrr.io/r/base/summary.html) on
`Triangle` objects. Computes group-wise summary statistics for loss
ratios (`lr`) and cumulative loss ratios (`clr`).

The function aggregates data by the grouping variables stored in
`attr(x, "group_var")` and the development variable stored in
`attr(x, "dev_var")`.

The following statistics are computed:

- arithmetic mean,

- median,

- weighted mean (portfolio-level ratio based on sums).

## Usage

``` r
# S3 method for class 'Triangle'
summary(object, ...)
```

## Arguments

- object:

  An object of class `Triangle`.

- ...:

  Unused; included for S3 compatibility.

## Value

A `data.table` grouped by `group_var` and `dev_var`, containing:

- n_obs:

  Number of observations in the cell

- lr_mean:

  Mean of loss ratios

- lr_median:

  Median of loss ratios

- lr_wt:

  Weighted loss ratio (`sum(loss) / sum(rp)`)

- clr_mean:

  Mean of cumulative loss ratios

- clr_median:

  Median of cumulative loss ratios

- clr_wt:

  Weighted cumulative loss ratio (`sum(closs) / sum(crp)`)

The returned object keeps the attributes `group_var` and `dev_var`, and
its class is updated to `"TriangleSummary"`.

## Details

The weighted mean is computed as:

- `lr_wt = sum(loss) / sum(rp)`

- `clr_wt = sum(closs) / sum(crp)`

These correspond to portfolio-level loss ratios based on risk premium
and are typically more stable than simple averages when exposure sizes
differ across cohorts.

It is assumed that the input `Triangle` object does not contain missing
values.

## Examples

``` r
if (FALSE) { # \dontrun{
d <- build_triangle(df, group_var = cv_nm)
sm <- summary(d)
head(sm)
attr(sm, "longer")
} # }
```
