# Safely convert to data.table

Internal helper that converts any `data.frame`-like object to a
`data.table`. If the input is already a `data.table`, a copy is returned
to prevent unintended modification by reference. Otherwise,
[`data.table::as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html)
is called, which always creates a new object.

## Usage

``` r
.ensure_dt(x)
```

## Arguments

- x:

  A `data.frame`, `tibble`, or `data.table`.

## Value

A `data.table`.
