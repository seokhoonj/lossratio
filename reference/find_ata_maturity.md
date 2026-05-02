# Find ata maturity by group

Identify the first mature age-to-age (ata) link from an object of class
`"ATASummary"`, typically produced by
[`summary.ATA()`](https://seokhoonj.github.io/lossratio/reference/summary.ATA.md).

Maturity is determined using a combination of:

- `cv < cv_threshold`

- `rse < rse_threshold`

- `valid_ratio >= min_valid_ratio`

- `n_valid >= min_n_valid`

- optional consecutive maturity over `min_run` ata links

Both `cv` and `rse` must be satisfied simultaneously. `cv` captures the
raw variability of observed ata factors across cohorts, while `rse`
reflects the precision of the WLS-estimated factor. Using both criteria
together provides a more robust maturity assessment than either alone.

## Usage

``` r
find_ata_maturity(
  x,
  cv_threshold = 0.1,
  rse_threshold = 0.05,
  min_valid_ratio = 0.5,
  min_n_valid = 3L,
  min_run = 1L
)
```

## Arguments

- x:

  An object of class `"ATASummary"`, typically produced by
  [`summary.ATA()`](https://seokhoonj.github.io/lossratio/reference/summary.ATA.md).

- cv_threshold:

  Maximum allowed coefficient of variation. Default is `0.10`.

- rse_threshold:

  Maximum allowed relative standard error. Default is `0.05`.

- min_valid_ratio:

  Minimum proportion of finite ata values required. Default is `0.5`.

- min_n_valid:

  Minimum number of finite ata factors required. Default is `3L`.

- min_run:

  Minimum number of consecutive ata links satisfying the maturity
  criteria. Default is `1L`.

## Value

A `data.table` with class `"ATAMaturity"` containing one row per group.
If no mature link is found, all values for that group are `NA`.
