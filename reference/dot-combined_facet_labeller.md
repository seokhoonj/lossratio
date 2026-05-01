# Combined single-line facet labeller

Internal helper that returns a labeller suitable for
`facet_wrap(..., labeller = ...)`, producing single-line strip labels
that combine multiple facet variables. Period-like columns are formatted
via
[`.format_facet_col()`](https://seokhoonj.github.io/lossratio/reference/dot-format_facet_col.md).

With one variable, labels are returned as-is (formatted). With multiple
variables, labels are combined as `"first (rest1, rest2, ...)"` — e.g.
`"SUR (23.01)"`.

## Usage

``` r
.combined_facet_labeller(vars, sep = ", ")
```

## Arguments

- vars:

  Character vector of facet column names.

- sep:

  Separator used to join the non-first variables. Default `", "`.

## Value

A labeller callable suitable for `facet_wrap(..., labeller = ...)`.
