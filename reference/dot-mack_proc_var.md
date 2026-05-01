# Compute Mack process variance for a single cohort

Internal helper computing:

\$\$ \mathrm{proc}\_{i,k+1} = f_k^2 \cdot \mathrm{proc}\_{i,k} +
\sigma^2_k \cdot \hat{C}\_{i,k}^{\alpha} \$\$

When `scale` is supplied, the increment is divided by `scale`.

## Usage

``` r
.mack_proc_var(
  value_proj,
  f_selected,
  sigma2,
  last_obs,
  alpha = 1,
  scale = NULL
)
```
