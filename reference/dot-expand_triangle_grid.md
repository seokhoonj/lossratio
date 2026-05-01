# Expand a `triangle` object to a full development grid

Internal helper that constructs a complete cohort-by-development-period
grid from an object of class `"triangle"`, analogous to
[`base::expand.grid()`](https://rdrr.io/r/base/expand.grid.html).

## Usage

``` r
.expand_triangle_grid(triangle, ata_fit, value_var)
```
