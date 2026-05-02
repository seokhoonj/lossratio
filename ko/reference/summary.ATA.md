# Summarise age-to-age factor statistics

Compute group-wise summary statistics for age-to-age factors from an
object of class `"ATA"`. This function serves two purposes:

1.  **Diagnostics**: provides descriptive statistics (`mean`, `median`,
    `wt`, `cv`) that help the user assess the stability and consistency
    of observed ata factors across cohorts.

2.  **Estimation**: fits a no-intercept weighted least squares model per
    ata link to produce the WLS-estimated factor (`f`), its standard
    error (`f_se`), relative standard error (`rse`), and Mack sigma
    (`sigma`). These are used downstream by
    [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md)
    and
    [`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md).

## Usage

``` r
# S3 method for class 'ATA'
summary(object, alpha = 1, digits = 3, ...)
```

## Arguments

- object:

  An object of class `"ATA"`, typically produced by
  [`build_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ata.md).

- alpha:

  Numeric scalar controlling the variance structure in the WLS fit.
  Default is `1`.

- digits:

  Number of decimal places to round numeric columns. Default is `3`.
  Pass `NULL` to skip rounding.

- ...:

  Additional arguments passed to the internal WLS estimation.

## Value

A `data.table` with class `"ATASummary"` containing one row per ata
link:

- `ata_from`, `ata_to`, `ata_link`:

  Link identifiers.

- `mean`:

  Arithmetic mean of observed ata factors.

- `median`:

  Median of observed ata factors.

- `wt`:

  Volume-weighted mean: \\\sum C\_{i,k+1} / \sum C\_{i,k}\\, independent
  of `alpha`.

- `cv`:

  Coefficient of variation of observed ata factors (\\SD / mean\\). Used
  by
  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md)
  to assess stability.

- `f`:

  WLS-estimated factor. Equals `wt` when `alpha = 2` and no zero
  `value_from` rows are present.

- `f_se`:

  Standard error of the WLS-estimated factor.

- `rse`:

  Relative standard error of the WLS-estimated factor (\\f\\se / f\\).

- `sigma`:

  Mack sigma (residual standard deviation from the WLS fit). Used in
  Mack variance estimation.

- `n_obs`:

  Total number of observations for the link.

- `n_valid`:

  Number of finite ata values.

- `n_inf`:

  Number of infinite ata values.

- `n_nan`:

  Number of NaN ata values.

- `valid_ratio`:

  Proportion of finite ata values (\\n\\valid / n\\obs\\).

## Relationship between `wt` and `f`

Both `wt` and `f` are weighted averages of the observed ata factors, but
they differ in how weights are assigned and which observations are
included:

- `wt`:

  Volume-weighted mean: \\wt = \sum C\_{i,k+1} / \sum C\_{i,k}\\.
  Computed from all rows where `value_from` and `value_to` are finite,
  including rows where either value is zero. Independent of `alpha`.

- `f`:

  WLS-estimated factor. Only rows where `value_from > 0` are used, since
  `value_from = 0` causes numerical issues in the WLS weights (\\w =
  value\\from^{\alpha}\\). When `alpha = 2`, `f` and `wt` are
  numerically equivalent (assuming no zero `value_from` rows). When
  `alpha \ne 2`, they diverge.

Therefore `wt` and `f` can differ for two reasons:

1.  **Zero exclusion**: rows with `value_from = 0` are included in `wt`
    but excluded from `f`. This typically affects early development
    periods where some cohorts have not yet accumulated any claims.

2.  **Alpha effect**: when `alpha \ne 2`, the WLS weights differ from
    the volume weights used in `wt`, leading to different estimates.
    Comparing `wt` and `f` can help diagnose whether the choice of
    `alpha` materially affects the estimated factor.

## Weights

When the input `"ATA"` object contains a `weight` column (added by
[`build_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ata.md)
when `weight_var` is supplied), that column is automatically used as the
WLS weight in place of `value_from`. This is useful when
`value_var = "clr"`, where `value_from` carries no exposure information
and an external exposure variable such as `crp` should be used instead.

## Coefficient of variation (`cv`)

The coefficient of variation is defined as: \$\$cv =
\frac{SD(f_k)}{\bar{f}\_k}\$\$ where \\f_k\\ are the individual observed
ata values for link \\k\\ and \\\bar{f}\_k\\ is their arithmetic mean.
The `cv` reflects the relative spread of observed factors across
cohorts, regardless of the exposure scale. It is used by
[`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md)
as one of the criteria for determining the maturity point.

## Relative standard error (`rse`)

The relative standard error is defined as: \$\$rse =
\frac{SE(\hat{f}\_k)}{\hat{f}\_k}\$\$ where \\SE(\hat{f}\_k)\\ is the
standard error of the WLS-estimated factor. Unlike `cv`, which treats
all cohorts equally, `rse` gives more weight to cohorts with larger
exposures (via the WLS weights). A small `rse` indicates that the WLS
estimate is precise, which tends to occur when: (1) there are many
cohorts, (2) exposures are large, and (3) the observed ata values are
consistent across cohorts.

## See also

[`build_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ata.md),
[`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md),
[`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md)
