# Fill ED intensities for projection

Internal helper that fills `NA` values in `g_selected` using
`na_method`: `"zero"` (default, no further development), `"locf"`, or
`"none"`.

## Usage

``` r
.filter_ed(
  ed_summary,
  grp_var = character(0),
  na_method = c("zero", "locf", "none")
)
```
