# Estimate age-to-age factors via weighted least squares

Internal helper that fits one no-intercept weighted linear model per
age-to-age link:

\$\$C\_{i,k+1} = f_k \cdot C\_{i,k} + \varepsilon\_{i,k}\$\$

Weights are proportional to \\w\_{i,k} / C\_{i,k}^{2 - \alpha}\\, where
\\w\_{i,k}\\ is either a constant or a column supplied via `weights`.
This corresponds to Mack's variance assumption \\\mathrm{Var}(C\_{i,k+1}
\mid C\_{i,k}) \propto C\_{i,k}^{\alpha}\\.

When only one observation is available for a link, the factor is
computed directly as `value_to / value_from` and standard errors are set
to `NA`.

Near-zero values of `f_se` and `sigma` (below `tol`) are set to zero to
avoid numerical noise from essentially perfect fits.

## Usage

``` r
.lm_ata(x, weights = 1, alpha = 1, na_rm = TRUE, tol = 1e-12)
```

## Arguments

- x:

  An object of class `"ATA"`.

- weights:

  Either a length-one numeric scalar (default `1`) or a single column
  name present in the `ata` data that provides per-row weights.

- alpha:

  Numeric scalar controlling the variance structure. Default is `1`.

- na_rm:

  Logical; if `TRUE` (default), rows with non-finite or non-positive
  `value_from` are dropped before fitting. Note that `value_to = 0` is
  permitted, as zero cumulative values are valid observations (e.g. no
  claims yet developed in early development periods).

- tol:

  Non-negative numeric scalar. Values below `tol` are set to zero.
  Default is `1e-12`.

## Value

A `data.table` with one row per ata link containing `f`, `f_se`,
`sigma`, `rse`, and `n_obs`. `rse` is defined as \\f\\se / f\\ and
represents the relative standard error of the WLS-estimated factor.
`rse` is `NA` when `f_se` is `NA` (single observation links) or when `f`
is zero.
