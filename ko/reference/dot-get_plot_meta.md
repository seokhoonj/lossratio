# Get plot display metadata for a value variable

Internal helper that returns display metadata for a given value
variable, including the plot title, y-axis caption, reference line
value, and variable type classification. Used across plot functions to
avoid repeating `switch` and `if` blocks for each variable type.

## Usage

``` r
.get_plot_meta(value_var, amount_divisor = 1e+08)
```

## Arguments

- value_var:

  A single character string naming the variable to plot. Must be one of
  the recognised variable names in the `lossratio` package.

- amount_divisor:

  Numeric scaling factor for amount variables. Default is `1e8`.

## Value

A named list with elements:

- `type`:

  One of `"ratio"`, `"amount"`, or `"prop"`.

- `title`:

  Plot title string.

- `caption`:

  Y-axis caption string, or `NULL`.

- `hline`:

  Y-intercept for a reference line, or `NULL`.
