# Format Date values as period labels

Convert a `Date` vector into formatted labels by month, quarter,
half-year, year, or day.

Output formats:

- `"month"`: `"2024.02"` or `"24.02"`

- `"quarter"`: `"2024.Q1"` or `"24.Q1"`

- `"half"`: `"2024.H1"` or `"24.H1"`

- `"year"`: `"2024"` or `"24"`

- `"day"`: `"2024.02.01"` or `"24.02.01"`

## Usage

``` r
.format_period(
  x,
  type = c("month", "quarter", "half", "year", "day"),
  sep = ".",
  abb = TRUE
)
```

## Arguments

- x:

  A `Date` vector (or an object coercible to `Date`).

- type:

  Period unit. One of `"month"`, `"quarter"`, `"half"`, `"year"`,
  `"day"`.

- sep:

  Separator placed between year and period components. Default `"."`.

- abb:

  Logical; if `TRUE`, use 2-digit year. Default `TRUE`.

## Value

Character vector of the same length as `x`.
