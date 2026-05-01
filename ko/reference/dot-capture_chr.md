# Convert an expression input into a character vector

Captures a user-supplied argument via
[`rlang::enquo()`](https://rlang.r-lib.org/reference/enquo.html) and
normalizes it into a character vector of names.

## Usage

``` r
.capture_chr(x)
```

## Arguments

- x:

  An expression typed at the call site, e.g. `c(a, b)` or `c("a", "b")`.
  Single names like `a` are also supported.

## Value

A character vector of names.
