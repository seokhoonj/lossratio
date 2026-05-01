# Get a human-readable label for an amount divisor

Internal helper that converts a numeric scaling divisor into a
human-readable unit label used in plot captions.

## Usage

``` r
.get_amount_unit(divisor)
```

## Arguments

- divisor:

  A single numeric scalar.

## Value

A character string such as `"100 million"`, `"billion"`, or a fallback
`"scaled (/1e+08)"` for unrecognised values. Returns `""` when `divisor`
is `1`.
