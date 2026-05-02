# Reshape an object to long form (S3 generic)

Mirrors the S3 generic originally provided by the `instead` package.
Concrete methods (`longer.Triangle`, `longer.TriangleSummary`) dispatch
on domain-specific classes and typically return the pre-computed
long-form data stored in `attr(x, "longer")`.

## Usage

``` r
longer(x, ...)
```

## Arguments

- x:

  An object to reshape.

- ...:

  Passed to methods.

## Value

A long-form object as defined by the dispatched method.
