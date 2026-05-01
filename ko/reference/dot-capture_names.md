# Capture and normalize column specifications into names

Resolves a flexible column specification (character vector, numeric
indices, a variable, or a bare symbol / `c(a, b)` / `.(a, b)` call) into
a character vector of column names guaranteed to exist in `data`.

Use inside another function by capturing and unquoting with
`!!rlang::enquo(x)`:

    f <- function(data, cols) {
      .capture_names(data, !!rlang::enquo(cols))
    }

## Usage

``` r
.capture_names(data, cols)
```

## Arguments

- data:

  A data.frame (or tibble/data.table).

- cols:

  A column specification (see description).

## Value

Character vector of column names; `character(0)` if absent.
