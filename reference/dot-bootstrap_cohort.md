# Parametric bootstrap for one cohort's loss and loss-ratio CI

Internal helper for bootstrap CI calculation used by
[`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md)
when `bootstrap = TRUE`. For a single cohort, simulates `B` replicates
of the projected loss path using Mack-style variance estimates as
sampling inputs, and returns percentile-based CI bounds aligned with the
input rows.

Observed rows (index \<= `last_obs`) are returned with CI equal to the
observed value (no uncertainty). Projected rows get bootstrap
percentiles.

## Usage

``` r
.bootstrap_cohort(
  loss_obs,
  loss_proj,
  exposure_proj,
  g_selected,
  f_selected,
  ed_sigma2,
  cl_sigma2,
  ed_g_var,
  cl_f_var,
  last_obs,
  maturity_from,
  B,
  loss_alpha,
  method,
  probs
)
```

## Value

A list with four vectors of length `length(exposure_proj)`: `ci_lower`,
`ci_upper` (for CLR), and `ci_lower_loss`, `ci_upper_loss` (for
cumulative loss).
