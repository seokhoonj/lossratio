# Compute Mack's factor variance for each development link

Internal helper computing:

\$\$\mathrm{Var}(\hat{f}\_k) = \frac{\sigma^2_k}{W_k}\$\$

where \\W_k = \sum_i w\_{i,k} \cdot C\_{i,k}^\alpha\\. This is
consistent with the WLS weight \\w\_{i,k} / C\_{i,k}^{2-\alpha}\\ used
in
[`.lm_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-lm_ata.md).

Also used by
[`fit_lr()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_lr.md)
for the CL component.

## Usage

``` r
.mack_f_var(ata_fit, alpha = 1)
```
