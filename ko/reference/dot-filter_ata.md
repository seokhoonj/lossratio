# Filter and fill age-to-age factors for projection

Internal helper that produces a `f_selected` column by applying two
steps:

1.  **Filter** — when `use_maturity = TRUE`, development links that
    precede the maturity point are excluded (`f_selected` set to `NA`).

2.  **Fill** — `NA` values in `f_selected` are forward-filled using
    LOCF, so that every link used in projection has a finite factor.

## Usage

``` r
.filter_ata(
  ata_summary,
  maturity = NULL,
  use_maturity = FALSE,
  grp_var = character(0),
  na_method = c("locf", "none")
)
```

## Arguments

- ata_summary:

  A `data.table` of class `"ata_summary"` from
  [`summary_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ata.md).

- maturity:

  A `data.table` from
  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md),
  or `NULL` when `use_maturity = FALSE`.

- use_maturity:

  Logical; if `TRUE`, apply the maturity filter. When `FALSE`,
  `maturity` is ignored entirely.

- grp_var:

  Character vector of grouping variable names.

- na_method:

  One of `"locf"` or `"none"`.

## Value

A `data.table` with `selected` and `f_selected` columns added.
