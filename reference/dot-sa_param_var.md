# Hybrid parameter variance for a single cohort

Internal helper for parameter variance:

- ED phase: \\\text{param}\_{k+1} = \text{param}\_k + (C^P_k)^2 \cdot
  \mathrm{Var}(\hat{g}\_k)\\

- CL phase: \\\text{param}\_{k+1} = f_k^2 \cdot \text{param}\_k +
  (C^L_k)^2 \cdot \mathrm{Var}(\hat{f}\_k)\\

## Usage

``` r
.sa_param_var(
  loss_proj,
  exposure_proj,
  ed_g_var,
  cl_f_var,
  f_selected,
  last_obs,
  maturity_from,
  method = "sa"
)
```
