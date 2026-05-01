# Estimate ED intensity via weighted least squares

Internal helper that fits one no-intercept weighted linear model per
development link:

\$\$\Delta C^L\_{i,k+1} = g_k \cdot C^P\_{i,k} + \varepsilon\_{i,k}\$\$

Weights are proportional to \\1 / (C^P\_{i,k})^{2 - \alpha}\\,
corresponding to the variance assumption \\\mathrm{Var}(\Delta
C^L\_{i,k+1}) \propto (C^P\_{i,k})^{\alpha}\\.

## Usage

``` r
.lm_ed(x, alpha = 1, na_rm = TRUE, tol = 1e-12)
```
