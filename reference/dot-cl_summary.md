# Summarise a `CLFit` object by cohort

Internal helper producing a one-row-per-cohort summary from the full
development grid. Contains latest observed, ultimate projection, and
reserve. When `method = "mack"`, also includes process/parameter
standard errors and coefficient of variation.

## Usage

``` r
.cl_summary(x)
```
