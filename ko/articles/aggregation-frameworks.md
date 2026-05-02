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
#>     cv_nm n_obs     cohort   dev       loss       rp      closs      crp
#>    <char> <int>     <Date> <int>      <num>    <num>      <num>    <num>
#> 1:    SUR    30 2023-04-01     1      0.000 11191625      0.000 11191625
#> 2:    CAN    30 2023-04-01     1   6445.022 12879189   6445.022 12879189
#> 3:    2CI    30 2023-04-01     1 468845.299  7567722 468845.299  7567722
#> 4:    HOS    30 2023-04-01     1      0.000 15273270      0.000 15273270
#> 5:    SUR    29 2023-04-01     2      0.000 14025887      0.000 25217512
#> 6:    CAN    29 2023-04-01     2      0.000 30821343   6445.022 43700532
#>      margin  cmargin profit cprofit           lr          clr  loss_prop
#>       <num>    <num> <fctr>  <fctr>        <num>        <num>      <num>
#> 1: 11191625 11191625    pos     pos 0.0000000000 0.0000000000 0.00000000
#> 2: 12872744 12872744    pos     pos 0.0005004214 0.0005004214 0.01356018
#> 3:  7098876  7098876    pos     pos 0.0619532953 0.0619532953 0.98643982
#> 4: 15273270 15273270    pos     pos 0.0000000000 0.0000000000 0.00000000
#> 5: 14025887 25217512    pos     pos 0.0000000000 0.0000000000 0.00000000
#> 6: 30821343 43694087    pos     pos 0.0000000000 0.0001474816 0.00000000
#>      rp_prop  closs_prop  crp_prop
#>        <num>       <num>     <num>
#> 1: 0.2385673 0.000000000 0.2385673
#> 2: 0.2745405 0.013560180 0.2745405
#> 3: 0.1613181 0.986439820 0.1613181
#> 4: 0.3255741 0.000000000 0.3255741
#> 5: 0.1890296 0.000000000 0.2082178
#> 6: 0.4153853 0.008953224 0.3608298
```

Each row is one (cohort, dev) cell with cumulative loss / risk premium.
Visualise as line plot or heatmap:

``` r

plot(tri)              # one trajectory per cohort, faceted by group
```

![](aggregation-frameworks_files/figure-html/unnamed-chunk-2-1.png)

``` r


# Quarterly cohort and dev for the multi-group heatmap so each panel's
# 10 x 10 cells stay readable when 4 panels share a row.
tri_q <- build_triangle(exp, group_var = cv_nm,
                        cohort_var = "uyq", dev_var = "elap_q")
plot_triangle(tri_q)   # cohort × dev heatmap of clr
```

![](aggregation-frameworks_files/figure-html/unnamed-chunk-2-2.png)

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
#>     cv_nm   calendar   dev       loss        rp       closs       crp   margin
#>    <char>     <Date> <int>      <num>     <num>       <num>     <num>    <num>
#> 1:    2CI 2023-04-01     1   468845.3   7567722    468845.3   7567722  7098876
#> 2:    2CI 2023-05-01     2   788083.2  27286691   1256928.5  34854413 26498608
#> 3:    2CI 2023-06-01     3 18122448.5  42665531  19379377.0  77519944 24543083
#> 4:    2CI 2023-07-01     4 70259226.3  68265635  89638603.2 145785580 -1993591
#> 5:    2CI 2023-08-01     5 32739938.3 110351069 122378541.5 256136649 77611131
#> 6:    2CI 2023-09-01     6 61587155.5 135154730 183965697.0 391291379 73567574
#>      cmargin profit cprofit        lr        clr loss_prop   rp_prop closs_prop
#>        <num> <fctr>  <fctr>     <num>      <num>     <num>     <num>      <num>
#> 1:   7098876    pos     pos 0.0619533 0.06195330 0.9864398 0.1613181  0.9864398
#> 2:  33597485    pos     pos 0.0288816 0.03606225 0.9626042 0.2248381  0.9713592
#> 3:  58140567    pos     pos 0.4247562 0.24999214 0.3480569 0.1688936  0.3631716
#> 4:  56146977    neg     pos 1.0292034 0.61486605 0.4423512 0.2060648  0.4224394
#> 5: 133758108    pos     pos 0.2966889 0.47778614 0.2173681 0.2219566  0.3373051
#> 6: 207325682    pos     pos 0.4556789 0.47015014 0.2061898 0.2113124  0.2781021
#>     crp_prop
#>        <num>
#> 1: 0.1613181
#> 2: 0.2071298
#> 3: 0.1841805
#> 4: 0.1938191
#> 5: 0.2050163
#> 6: 0.2071482
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
```

![](aggregation-frameworks_files/figure-html/unnamed-chunk-4-1.png)

``` r

plot(cal, x_by = "dev")         # x = sequential index
```

![](aggregation-frameworks_files/figure-html/unnamed-chunk-4-2.png)

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
#>     cv_nm n_obs sales_start  sales_end        loss          rp        lr
#>    <char> <int>      <Date>     <Date>       <num>       <num>     <num>
#> 1:    SUR    30  2023-04-01 2024-03-01 26195800275 23817090349 1.0998741
#> 2:    CAN    30  2023-04-01 2024-03-01 15036650641 24008537115 0.6263043
#> 3:    2CI    30  2023-04-01 2024-03-01 12482828943 18720199622 0.6668107
#> 4:    HOS    30  2023-04-01 2024-03-01  9737095655 25111393761 0.3877561
#>    loss_prop   rp_prop
#>        <num>     <num>
#> 1: 0.4128419 0.2598496
#> 2: 0.2369754 0.2619383
#> 3: 0.1967275 0.2042414
#> 4: 0.1534552 0.2739707
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
#> [1] "uym"
attr(tri, "cohort_type")     # "month"
#> [1] "month"
attr(tri, "dev_var")     # "elap_m"
#> [1] "elap_m"
attr(tri, "dev_type")    # "month"
#> [1] NA

attr(cal, "calendar_var")    # "cym"
#> [1] "cym"
attr(cal, "calendar_type")   # "month"
#> [1] "month"
```

The data columns themselves are standardised to `cohort` / `dev` /
`calendar`, so downstream code is granularity-agnostic.
