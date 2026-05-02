# Granularity of a cohort or development variable

Like
[`.get_period_type()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-get_period_type.md)
but also recognises the integer elapsed-period columns (`elap_m` /
`elap_q` / `elap_h` / `elap_y`). Used by
[`build_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/build_triangle.md)
to verify that `cohort_var` and `dev_var` share the same granularity.
Not used for date formatting (these elap columns are integers, not
Date).

## Usage

``` r
.get_granularity(var)
```
