# Aggregation frameworks: triangle, calendar, total

The same long-format experience data can be aggregated three ways
depending on the question being asked. `lossratio` exposes one builder
per framework. This vignette compares them.

## At a glance

| Builder | Output object | Dimension | When to use |
|----|----|----|----|
| [`build_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/build_triangle.md) | `triangle` | cohort × dev (2D) | Chain ladder, ED, SA projection |
| [`build_calendar()`](https://seokhoonj.github.io/lossratio/ko/reference/build_calendar.md) | `calendar` | calendar period (1D) | Calendar-year trend, diagonal effect |
| [`build_total()`](https://seokhoonj.github.io/lossratio/ko/reference/build_total.md) | `total` | portfolio total (per group) | High-level loss-ratio comparison |

Conceptually:

- `triangle` preserves both the cohort axis (when policies were
  underwritten) and the development axis (how loss accrues over
  development time). This is the canonical chain-ladder data structure.
- `calendar` collapses cohorts onto the diagonal — each row is one
  calendar period across all underwriting cohorts. Equivalent to the
  diagonal sum of the triangle.
- `total` collapses both dimensions to one value per group. Useful for
  portfolio-level comparison (which product had the worst loss ratio
  over the window?).

## Triangle (cohort × dev)

``` r

library(lossratio)
data(experience)
exp <- as_experience(experience)

tri <- build_triangle(exp, group_var = cv_nm)
head(tri)
#>    cv_nm n_obs     cohort dev   loss     rp  closs    crp ...
#> 1:   SUR    30 2023-04-01       1      0  ...       0  ...
#> 2:   2CI    30 2023-04-01       1      0  ...       0  ...
#> ...
```

Each row is one (cohort, dev) cell with cumulative loss / risk premium.
Visualise as line plot or heatmap:

``` r

plot(tri)              # one trajectory per cohort, faceted by group
plot_triangle(tri)     # cohort × dev heatmap of clr
```

Use `triangle` as input to: -
[`build_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ata.md),
[`build_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/build_ed.md)
— development factors -
[`fit_cl()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_cl.md),
[`fit_lr()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_lr.md)
— projection -
[`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
— structural change detection

## Calendar (calendar period only)

``` r

cal <- build_calendar(exp, group_var = cv_nm, calendar_var = "cym")
head(cal)
#>    cv_nm   calendar dev   loss     rp  closs    crp ...
#> 1:   SUR 2023-04-01       1      0  ...       0  ...
#> 2:   SUR 2023-05-01       2      0  ...       0  ...
#> ...
```

Each row is one calendar period (per group). The `dev` column here is a
sequential index (1, 2, 3, …) within group, not “development period
since cohort start”.

Calendar aggregation is mathematically the **diagonal sum** of the
triangle: cells with the same `cym` (regardless of `uym`/`elap_m`) are
combined.

Use cases: - Trend analysis (“loss ratio is rising over calendar
time”) - Calendar-year effect detection (e.g., regulatory shock, premium
on-leveling event) - Portfolio monitoring dashboards

``` r

plot(cal)                           # x = calendar
plot(cal, x_by = "dev")         # x = sequential index
```

## Total (portfolio summary)

``` r

tot <- build_total(
  exp,
  group_var = cv_nm,
  cohort_var = "uym",
  period_from = "2023-04-01",
  period_to   = "2024-03-01"
)
head(tot)
#>    cv_nm n_obs sales_start sales_end       loss          rp        lr ...
#> 1:   SUR    12  2023-04-01 2024-03-01  ...        ...       ...
```

One row per group, summarising loss / risk premium / loss ratio over the
window. The `period_from` / `period_to` arguments restrict to a fixed
window so groups are comparable.

Use cases: - Compare overall loss ratio across coverages - Rank groups
by reserve / share of portfolio - Build executive summary tables

## Aggregation as data flow

                         experience (long, with demographics)
                                  │
             ┌────────────────────┼─────────────────────┐
             │                    │                     │
       build_triangle      build_calendar         build_total
       (cohort × dev)      (calendar series)     (portfolio total)
             │                    │                     │
             ▼                    ▼                     ▼
         triangle             calendar               total
       (2D, projection)     (1D, trend)         (0D, comparison)

All three start from the same `experience` and aggregate demographic
dimensions away. Choose the framework based on the analytical question.

## Attribute schema

After aggregation, each object stores its source-column metadata as
attributes (used for plot labels and granularity-aware date formatting):

``` r

attr(tri, "cohort_var")      # "uym"
attr(tri, "cohort_type")     # "month"
attr(tri, "dev_var")     # "elap_m"
attr(tri, "dev_type")    # "month"

attr(cal, "calendar_var")    # "cym"
attr(cal, "calendar_type")   # "month"
```

The data columns themselves are standardised to `cohort` / `dev` /
`calendar`, so downstream code is granularity-agnostic.
