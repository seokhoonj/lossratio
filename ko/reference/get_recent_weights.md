# Recent-diagonal weights for a development triangle

Returns a weight matrix that restricts a development triangle to its
most recent `recent` calendar diagonals. Cells on or after the last
`recent` diagonals retain their input values; earlier observed cells are
set to `0`. Cells that are `NA` in the input (not yet observed) are left
as `NA`.

This is a standard construct for restricting chain-ladder estimation to
recent calendar periods when older experience is considered less
representative of current conditions (e.g. after a rate change or a
claim-handling reform).

## Usage

``` r
get_recent_weights(weights, recent)
```

## Arguments

- weights:

  A triangle-shaped numeric matrix, with origin periods as rows and
  development periods as columns. Unobserved future cells should be
  `NA`.

- recent:

  Optional positive integer: the number of most recent calendar
  diagonals to keep. When missing or `NULL`, `weights` is returned
  unchanged.

## Value

A numeric matrix of the same shape as `weights`.

## Handling of NA cells

`NA` cells in the input (not yet observed) remain `NA` in the output.
They are semantically distinct from the `0` cells produced by the
recency filter, which represent observed values explicitly excluded from
the current weighting scheme. Callers who want both to behave
identically can post-process with `w[is.na(w)] <- 0`.

## Examples

``` r
if (FALSE) { # \dontrun{
m <- ChainLadder::RAA
get_recent_weights(m)       # unchanged (no `recent` supplied)
get_recent_weights(m, 3)    # keep only the last 3 calendar diagonals
} # }
```
