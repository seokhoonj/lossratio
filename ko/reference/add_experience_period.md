# Add standard period variables to an experience dataset

Add underwriting, calendar, and development period variables to an
experience dataset using standard column conventions for loss ratio
analysis.

The function detects the presence of key source columns such as `uym`,
`cym`, and `elap_m`, and derives additional period variables when
possible.

## Usage

``` r
add_experience_period(df)
```

## Arguments

- df:

  A data.frame containing period variables such as `uym`, `cym`, and
  `elap_m`.

## Value

A data.frame (or tibble/data.table depending on input) with additional
period variables.

## Details

The following variables are added when the required source columns
exist:

**Underwriting period (from `uym`):**

- `uy` : underwriting year

- `uyh` : underwriting half-year

- `uyq` : underwriting quarter

**Calendar period (from `cym`):**

- `cy` : calendar year

- `cyh` : calendar half-year

- `cyq` : calendar quarter

**Elapsed period:**

- `elap_y` is derived from `elap_m` as yearly development index, where
  months 1 to 12 map to 1, 13 to 24 map to 2, and so on.

- `elap_h` is derived from `uym` and `cym` using calendar half-year
  boundaries. For example, contracts issued in January to June are
  aligned to the same first development half-year block, and the next
  calendar half-year becomes development half-year 2.

- `elap_q` is derived from `uym` and `cym` using calendar quarter
  boundaries. For example, contracts issued in January to March are
  aligned to the same first development quarter block, and the next
  calendar quarter becomes development quarter 2.

Therefore, `elap_h` and `elap_q` are not simple grouped versions of
`elap_m`; they are aligned to calendar half-year and quarter boundaries
so that underwriting cohorts such as Q1, Q2, H1, and H2 are compared
consistently on the same cumulative development basis.

Newly created columns are inserted before their corresponding base
columns.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  uym  = as.Date("2023-01-01") + 0:5 * 30,
  cym  = as.Date("2023-01-01") + 0:5 * 30,
  elap_m = 1:6
)

df2 <- add_experience_period(df)
head(df2)
} # }
```
