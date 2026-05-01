# Human-readable label for a period / development variable name

Internal helper that maps a package convention variable name (e.g.
`"uym"`, `"elap_m"`) to a human-readable axis label (e.g.
`"underwriting ym"`, `"development months"`). Falls back to the input
string when the variable is not recognised.

## Usage

``` r
.pretty_var_label(var)
```

## Arguments

- var:

  A single character string.

## Value

A single character string.
