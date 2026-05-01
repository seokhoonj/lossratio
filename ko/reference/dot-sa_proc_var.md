# Hybrid process variance for a single cohort

Internal helper for process variance:

- ED phase (additive): \\\text{proc}\_{k+1} = \text{proc}\_k +
  \sigma^2\_{\text{ed},k} \cdot (C^P_k)^\alpha\\

- CL phase (multiplicative, Mack): \\\text{proc}\_{k+1} = f_k^2 \cdot
  \text{proc}\_k + \sigma^2\_{\text{cl},k} \cdot (C^L_k)^\alpha\\

## Usage

``` r
.sa_proc_var(
  loss_proj,
  exposure_proj,
  ed_sigma2,
  cl_sigma2,
  f_selected,
  last_obs,
  maturity_from,
  alpha = 1,
  method = "sa"
)
```
