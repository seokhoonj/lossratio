# Format one column of facet labels

Internal helper that formats a single column of facet label values.
Period-like variables (`uym`, `cym`, `uyq`, ...) are formatted via
[`.format_period()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-format_period.md)
in abbreviated form (e.g. `"23.01"`); all other variables are coerced
with [`as.character()`](https://rdrr.io/r/base/character.html).

## Usage

``` r
.format_facet_col(var, x)
```

## Arguments

- var:

  Single column name.

- x:

  Values in that column.

## Value

Character vector.
