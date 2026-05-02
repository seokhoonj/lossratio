# Filter a long-format table to recent calendar diagonals

Internal long-format analogue of
[`get_recent_weights()`](https://seokhoonj.github.io/lossratio/reference/get_recent_weights.md).
Returns a subset of the input `data.table` containing only rows whose
calendar position falls within the last `recent` calendar diagonals of
its group.

The matrix-form condition `row + col >= m - recent + 2` is translated to
the group-wise long-form condition
`rank(cohort) + dev - 1 > max(rank(cohort) + dev - 1) - recent`.

## Usage

``` r
.apply_recent_filter(dt, recent, grp_var = character(0), coh_var, dev_var)
```

## Arguments

- dt:

  A long-format development `data.table`.

- recent:

  Positive integer or `NULL`. When `NULL` or missing, `dt` is returned
  unchanged.

- grp_var:

  Character vector of group columns (may be empty).

- coh_var:

  Single column name for the cohort variable (e.g. `cohort`).

- dev_var:

  Single column name for the development variable (e.g. `dev` for
  `Triangle` objects, or `ata_from` for `ATA`/`ED` objects).

## Value

A filtered copy of `dt` (class preserved), keeping only rows within the
recent-diagonal window.
