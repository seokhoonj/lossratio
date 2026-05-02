# Summarise calendar-development statistics (Mean, Median, Weighted)

S3 method for [`summary()`](https://rdrr.io/r/base/summary.html) on
`Calendar` objects. Computes calendar-period summary statistics for loss
ratios (`lr`) and cumulative loss ratios (`clr`).

Where
[`summary.Triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/summary.Triangle.md)
aggregates by `(group_var, dev)` (cohort × development), this method
aggregates by `(group_var, calendar)` (calendar period) so the resulting
table is indexed by calendar diagonals rather than development periods.

## Usage

``` r
# S3 method for class 'Calendar'
summary(object, ...)
```

## Arguments

- object:

  An object of class `Calendar`.

- ...:

  Unused; included for S3 compatibility.

## Value

A `data.table` of class `"CalendarSummary"` with one row per
`(group_var, calendar)` combination, containing:

- n_obs:

  Number of observations in the cell.

- lr_mean:

  Mean of loss ratios.

- lr_median:

  Median of loss ratios.

- lr_wt:

  Weighted loss ratio (`sum(loss) / sum(rp)`).

- clr_mean:

  Mean of cumulative loss ratios.

- clr_median:

  Median of cumulative loss ratios.

- clr_wt:

  Weighted cumulative loss ratio (`sum(closs) / sum(crp)`).

The returned object preserves the attributes `group_var`,
`calendar_var`, and `calendar_type`.

## Examples

``` r
if (FALSE) { # \dontrun{
cal <- build_calendar(df, group_var = cv_nm)
sm  <- summary(cal)
head(sm)
} # }
```
