# Get the period type string for a period variable name

Internal helper that maps a period variable name (e.g. `"uym"`, `"cyq"`)
to the corresponding type string accepted by
[`.format_period()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-format_period.md).

Returns `NA_character_` for unrecognised variable names, which callers
can use to fall back to
[`as.character()`](https://rdrr.io/r/base/character.html) formatting.

## Usage

``` r
.get_period_type(var)
```

## Arguments

- var:

  A single character string naming a period variable.

## Value

One of `"month"`, `"quarter"`, `"half"`, `"year"`, or `NA_character_`.
