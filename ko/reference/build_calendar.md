# Build a calendar-based development structure from experience data

Aggregate experience data into a development structure along a single
calendar-style period axis, including:

- cumulative loss and cumulative risk premium,

- period and cumulative proportions,

- margin and cumulative margin,

- profit indicators,

- loss ratio (`lr = loss / rp`) and cumulative loss ratio
  (`clr = closs / crp`).

In contrast to
[`build_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/build_triangle.md),
which builds a development structure using `calendar_var × dev_var`,
this function aggregates values over a one-dimensional calendar axis.

The loss ratio is defined as: \$\$lr = loss / rp\$\$

where `rp` represents risk premium (expected loss), not written premium.

Proportion variables are computed within each `calendar_var` cell:

- `loss_prop = loss / sum(loss)`

- `rp_prop = rp / sum(rp)`

- `closs_prop = closs / sum(closs)`

- `crp_prop = crp / sum(crp)`

Therefore, for a fixed `calendar_var` cell, the proportions sum to 1
across groups. These are useful for examining the composition of each
calendar period across products or other grouping variables.

## Usage

``` r
build_calendar(
  df,
  group_var,
  calendar_var = "cym",
  value_var = c("loss", "rp"),
  period_from = NULL,
  period_to = NULL,
  fill_gaps = FALSE
)
```

## Arguments

- df:

  A data.frame containing experience data with loss and risk premium.

- group_var:

  Column(s) used for grouping (e.g., product, gender).

- calendar_var:

  A single calendar-like period variable defining the summary axis.
  Typical examples include:

  - `cym` (calendar year-month),

  - `cyq` (calendar year-quarter),

  - `cyh` (calendar year-half),

  - `cy` (calendar year),

  - `uym`, `uyq`, `uyh`, `uy` when a single underwriting-period axis is
    to be summarised as a time series rather than as a development
    structure.

- value_var:

  Character vector specifying the value columns, typically
  `c("loss", "rp")` where:

  - `loss` = actual claims (observed loss),

  - `rp` = risk premium (expected loss).

- period_from:

  Optional lower bound for `calendar_var`. Only rows with
  `calendar_var >= period_from` are kept.

- period_to:

  Optional upper bound for `calendar_var`. Only rows with
  `calendar_var <= period_to` are kept.

- fill_gaps:

  Logical; if `TRUE`, zero-fill missing `(group_var, calendar_var)`
  cells so every group has a consecutive calendar sequence (monthly,
  quarterly, etc. based on `calendar_var`). Default `FALSE`, which
  raises an error when gaps are detected.

## Value

A data.frame with class `"calendar"`, containing the following derived
columns:

- index:

  Calendar index within each group, defined as the sequential order of
  `calendar_var` after sorting in ascending order. This represents the
  progression of calendar periods for each group (e.g., 1 = first
  observed period, 2 = second, ...), and can be used to align groups
  with different starting periods on a common index scale.

- closs, crp:

  Cumulative loss and cumulative risk premium

- loss_prop, rp_prop:

  Period proportions within each `calendar_var` cell

- closs_prop, crp_prop:

  Cumulative proportions within each `calendar_var` cell

- margin, cmargin:

  Period and cumulative margin (`rp - loss`)

- profit, cprofit:

  Profit indicator (factor `"pos"` / `"neg"`)

- lr:

  Loss ratio (`loss / rp`)

- clr:

  Cumulative loss ratio (`closs / crp`)

The returned object also has an attribute `"longer"` containing a melted
long-format version (`class = "calendar_longer"`).

## Examples

``` r
if (FALSE) { # \dontrun{
res1 <- build_calendar(
  df,
  group_var  = pd_cd,
  calendar_var = "cym"
)

res2 <- build_calendar(
  df,
  group_var   = pd_cd,
  calendar_var = "cyq",
  period_from = "2023-01-01"
)

head(res1)
attr(res1, "longer")
} # }
```
