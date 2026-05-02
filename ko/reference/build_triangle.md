# Build a development structure from experience data

Aggregate experience data into a development structure by grouping,
period, and development-period variables. The result contains:

- cumulative loss and cumulative risk premium,

- period and cumulative proportions,

- margin and cumulative margin,

- profit indicators,

- loss ratio (`lr = loss / rp`) and cumulative loss ratio
  (`clr = closs / crp`).

The loss ratio is defined as: \$\$lr = loss / rp\$\$

where `rp` represents risk premium (expected loss), not written premium.

Proportion variables are computed within each `cohort_var + dev_var`
cell:

- `loss_prop = loss / sum(loss)`

- `rp_prop = rp / sum(rp)`

- `closs_prop = closs / sum(closs)`

- `crp_prop = crp / sum(crp)`

Therefore, for a fixed `(cohort_var, dev_var)` cell, the proportions sum
to 1 across groups. These are useful for examining the composition of
each development cell across products or other grouping variables.

## Usage

``` r
build_triangle(
  df,
  group_var,
  cohort_var = "uym",
  dev_var = "elap_m",
  value_var = c("loss", "rp"),
  fill_gaps = FALSE
)
```

## Arguments

- df:

  A data.frame containing experience data with loss and risk premium.

- group_var:

  Column(s) used for grouping (e.g., product, gender).

- cohort_var:

  Column(s) defining the exposure period (e.g., underwriting year-month,
  quarter, half-year, or year such as `uym`, `uyq`, `uyh`, `uy`).

- dev_var:

  Column(s) defining development periods (e.g., months since issue such
  as `elap_m`).

- value_var:

  Character vector specifying the value columns, typically
  `c("loss", "rp")` where:

  - `loss` = actual claims (observed loss),

  - `rp` = risk premium (expected loss).

- fill_gaps:

  Logical; if `TRUE`, zero-fill missing
  `(group_var, cohort_var, dev_var)` cells so that every cohort has a
  consecutive `dev_var` sequence. Default `FALSE`, which raises an error
  when gaps are detected. Use
  [`validate_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/validate_triangle.md)
  to inspect gaps before deciding.

## Value

A data.frame with class `"Triangle"`, containing the following derived
columns:

- n_obs:

  Number of distinct periods observed

- closs, crp:

  Cumulative loss and cumulative risk premium

- loss_prop, rp_prop:

  Period proportions within each `(period, dev)` cell

- closs_prop, crp_prop:

  Cumulative proportions within each `(period, dev)` cell

- margin, cmargin:

  Period and cumulative margin (`rp - loss`)

- profit, cprofit:

  Profit indicator (factor `"pos"` / `"neg"`)

- lr:

  Loss ratio (`loss / rp`)

- clr:

  Cumulative loss ratio (`closs / crp`)

The returned object also has an attribute `"longer"` containing a melted
long-format version (`class = "TriangleLonger"`).

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  pd_cd    = rep(c("P001", "P002"), each = 6),
  pd_nm    = rep(c("cancer", "health"), each = 6),
  uym      = rep(as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")), 4),
  elap_m     = rep(1:2, 6),
  loss     = runif(12, 80, 120),
  rp       = runif(12, 90, 110),
  n_policy = sample(50:100, 12, replace = TRUE)
)

res <- build_triangle(
  df,
  group_var   = pd_cd,
  value_var   = c("loss", "rp", "n_policy"),
  cohort_var = "uym",
  dev_var = "elap_m"
)

head(res)
attr(res, "longer")
} # }
```
