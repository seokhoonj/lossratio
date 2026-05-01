# Compute ED intensity variance for each development link

Internal helper computing \\\mathrm{Var}(\hat{g}\_k) = \sigma^2_k /
W_k\\ where \\W_k = \sum_i C^P\_{i,k}^{2 - \alpha}\\.

Used by
[`fit_ed()`](https://seokhoonj.github.io/lossratio/reference/fit_ed.md)
when `method = "mack"` and by
[`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md)
for the ED component.

## Usage

``` r
.mack_g_var(ed_fit, alpha = 1)
```

## Arguments

- ed_fit:

  An object of class `"ed_fit"`.

- alpha:

  Numeric scalar. Default is `1`.

## Value

The `$selected` `data.table` with `g_var` column.
