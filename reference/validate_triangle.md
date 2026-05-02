# Validate triangle structure before building a development

Check that each `(group_var, cohort_var)` cohort has a consecutive
`dev_var` sequence within its observed range. Non-consecutive cohorts
produce non-consecutive age-to-age links downstream (e.g., `14 -> 17`
instead of `14 -> 15`), which breaks
[`summary.ATA()`](https://seokhoonj.github.io/lossratio/reference/summary.ATA.md)
/
[`summary.ED()`](https://seokhoonj.github.io/lossratio/reference/summary.ED.md)
key uniqueness and causes cartesian joins in
[`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md).

This function inspects the raw data without modifying it. Use it before
[`build_triangle()`](https://seokhoonj.github.io/lossratio/reference/build_triangle.md)
to decide whether to fix the data source, drop offending cohorts, or
pass `fill_gaps = TRUE` to
[`build_triangle()`](https://seokhoonj.github.io/lossratio/reference/build_triangle.md).

## Usage

``` r
validate_triangle(df, group_var, cohort_var = "uym", dev_var = "elap_m")
```

## Arguments

- df:

  A data.frame.

- group_var:

  Grouping variable(s).

- cohort_var:

  A single cohort variable. Default `"uym"`.

- dev_var:

  A single development variable. Default `"elap_m"`.

## Value

A `data.table` of class `"TriangleValidation"` with one row per cohort
containing gaps. Columns:

- group_var(s), cohort_var:

  Cohort identifier.

- `n_observed`:

  Number of distinct observed `dev_var` values.

- `n_expected`:

  `max(elap_m) - min(elap_m) + 1` for that cohort.

- `missing`:

  List column of missing `dev_var` values.

Returns a zero-row data.table when no gaps are found.

## See also

[`build_triangle()`](https://seokhoonj.github.io/lossratio/reference/build_triangle.md)
