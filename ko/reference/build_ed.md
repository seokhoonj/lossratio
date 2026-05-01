# Build exposure-driven development data

Construct exposure-driven incremental development data from an object of
class `"triangle"`, typically produced by
[`build_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/build_triangle.md).
This is the foundational data structure for the exposure-driven (ED)
model, where incremental loss is modelled as a function of cumulative
exposure (risk premium) rather than cumulative loss.

The incremental loss intensity is defined as:

\$\$g\_{i,k} = \frac{\Delta C^L\_{i,k+1}}{C^P\_{i,k}}\$\$

where \\\Delta C^L\_{i,k+1} = C^L\_{i,k+1} - C^L\_{i,k}\\ is the
incremental loss and \\C^P\_{i,k}\\ is the cumulative exposure (risk
premium) at development period \\k\\.

## Usage

``` r
build_ed(
  x,
  loss_var = "closs",
  exposure_var = "crp",
  min_exposure = 0,
  drop_invalid = FALSE
)
```

## Arguments

- x:

  An object of class `"triangle"`.

- loss_var:

  A single cumulative loss variable. Default is `"closs"`.

- exposure_var:

  A single cumulative exposure variable. Default is `"crp"`.

- min_exposure:

  Minimum exposure required to compute `g`. If
  `exposure_from <= min_exposure`, `g` is set to `NA`. Default is `0`.

- drop_invalid:

  Logical; if `TRUE`, rows with invalid (non-finite) `g` values are
  dropped. Default is `FALSE`.

## Value

A `data.table` with class `"ed"` containing:

- `ata_from`:

  Current development period.

- `ata_to`:

  Next development period.

- `ata_link`:

  Concatenated label `"ata_from-ata_to"`.

- `loss_from`:

  Cumulative loss \\C^L\_{i,k}\\.

- `loss_to`:

  Cumulative loss \\C^L\_{i,k+1}\\.

- `delta_loss`:

  Incremental loss \\\Delta C^L\_{i,k+1}\\.

- `exposure_from`:

  Cumulative exposure \\C^P\_{i,k}\\.

- `exposure_to`:

  Cumulative exposure \\C^P\_{i,k+1}\\.

- `g`:

  Incremental loss intensity \\\Delta C^L\_{i,k+1} / C^P\_{i,k}\\, or
  `NA` when `exposure_from <= min_exposure`.

The returned object carries the following attributes: `group_var`,
`cohort_var`, `dev_var`, `loss_var`, and `exposure_var`.

## See also

[`build_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/build_triangle.md),
[`summary_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/summary_ed.md),
[`fit_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ed.md)

## Examples

``` r
if (FALSE) { # \dontrun{
d <- build_triangle(df, group_var = cv_nm)
ed <- build_ed(d)
head(ed)
attr(ed, "loss_var")
attr(ed, "exposure_var")
} # }
```
