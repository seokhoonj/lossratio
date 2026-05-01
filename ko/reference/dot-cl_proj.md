# Compute chain ladder point projection for a single cohort

Internal helper that fills in the unobserved development path for a
single cohort by applying the selected age-to-age factors recursively:

\$\$\hat{C}\_{i,k+1} = \hat{f}\_k \cdot \hat{C}\_{i,k}\$\$

Only cells beyond the last observed value are projected. Observed cells
are returned unchanged.

## Usage

``` r
.cl_proj(value_obs, f_selected)
```

## Arguments

- value_obs:

  Numeric vector of cumulative observed values for a single cohort,
  ordered by development period.

- f_selected:

  Numeric vector of selected development factors.

## Value

A numeric vector of the same length as `value_obs` with unobserved cells
filled by recursive chain ladder projection.
