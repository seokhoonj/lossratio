# Coerce a dataset to an `Experience` object

Coerce a data.frame to a minimal `Experience` object for loss ratio
analysis.

This function checks that the input contains the minimum required
columns, attempts to coerce them to the expected classes, optionally
derives standard period variables via
[`add_experience_period()`](https://seokhoonj.github.io/lossratio/ko/reference/add_experience_period.md),
and prepends class `"Experience"`.

The function intentionally performs only minimal coercion. Other columns
such as grouping variables or presentation variables are left unchanged
and should be cleaned by the user in advance.

## Usage

``` r
as_experience(df, add_period = TRUE)
```

## Arguments

- df:

  A data.frame containing experience data.

- add_period:

  Logical; if `TRUE`, derive additional period variables using
  [`add_experience_period()`](https://seokhoonj.github.io/lossratio/ko/reference/add_experience_period.md).
  Default is `TRUE`.

## Value

A data.frame with class `"Experience"` prepended.

## Details

Minimum required columns are:

- `cym` : Calendar year-month (`Date` or coercible to `Date`)

- `uym` : Underwriting year-month (`Date` or coercible to `Date`)

- `loss` : Incurred loss amount (`numeric` or coercible)

- `rp` : Risk premium (`numeric` or coercible)

If `add_period = TRUE`, additional period variables such as `uy`, `uyh`,
`uyq`, `cy`, `cyh`, `cyq`, `elap_y`, `elap_h`, and `elap_q` may be
added, depending on the available source columns.

## See also

[`check_experience()`](https://seokhoonj.github.io/lossratio/ko/reference/check_experience.md),
[`add_experience_period()`](https://seokhoonj.github.io/lossratio/ko/reference/add_experience_period.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- as_experience(df)
class(x)
} # }
```
