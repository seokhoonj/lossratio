# Triangle and ata diagnostics

Before fitting a chain ladder or loss-ratio model, it pays to inspect
the underlying triangle. This vignette covers the diagnostic tools in
`lossratio` for understanding cohort behaviour, age-to-age factor
stability, and maturity detection.

## Triangle-level diagnostics

``` r

library(lossratio)
data(experience)
exp <- as_experience(experience)
tri <- build_triangle(exp, group_var = cv_nm)
```

### Cohort trajectories

``` r

plot(tri)                              # raw clr trajectories per cohort
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-2-1.png)

``` r

plot(tri, value_var = "loss")          # cumulative loss instead of clr
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-2-2.png)

``` r

plot(tri, summary = TRUE)              # raw + overlay (mean / median / weighted)
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-2-3.png)

The `summary = TRUE` overlay computes mean, median, and weighted clr at
each dev and overlays them on the cohort lines. Useful for spotting
cohorts that deviate from the central tendency.

### Cell heatmap

``` r

plot_triangle(tri)                          # clr in each cell
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-3-1.png)

``` r

plot_triangle(tri, value_var = "loss")      # cumulative loss
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-3-2.png)

``` r

plot_triangle(tri, label_style = "detail")  # ratio + (loss / rp) amounts
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-3-3.png)

### Group statistics by dev

``` r

sm <- summary(tri)
head(sm)
#> Key: <cv_nm, dev>
#>     cv_nm   dev n_obs    lr_mean  lr_median      lr_wt   clr_mean clr_median
#>    <char> <int> <int>      <num>      <num>      <num>      <num>      <num>
#> 1:    2CI     1    30 0.01682339 0.00184005 0.01644478 0.01682339 0.00184005
#> 2:    2CI     2    29 0.25741347 0.01811353 0.27088773 0.15773318 0.03301080
#> 3:    2CI     3    28 0.62369370 0.42156041 0.61629298 0.35691535 0.24171186
#> 4:    2CI     4    27 0.77462795 0.71944584 0.75991294 0.50358374 0.47161170
#> 5:    2CI     5    26 0.53125865 0.31309431 0.51708799 0.51125195 0.49812486
#> 6:    2CI     6    25 0.59424632 0.61509818 0.57229192 0.53105955 0.52356666
#>        clr_wt
#>         <num>
#> 1: 0.01644478
#> 2: 0.15551405
#> 3: 0.36029953
#> 4: 0.49175016
#> 5: 0.49843641
#> 6: 0.52060534
```

Returns a `triangle_summary` object with mean / median / weighted loss
ratios per (group, dev) cell.

## Age-to-age factor diagnostics

``` r

ata <- build_ata(tri, value_var = "closs")
sm  <- summary_ata(ata, alpha = 1)
head(sm)
#> Key: <cv_nm>
#>     cv_nm ata_from ata_to ata_link    mean median     wt    cv      f   f_se
#>    <char>    <num>  <num>   <fctr>   <num>  <num>  <num> <num>  <num>  <num>
#> 1:    2CI        1      2      1-2 144.524  1.909 20.117 2.779 11.642 20.904
#> 2:    2CI        2      3      2-3  60.420  3.604  3.920 3.643  3.649  4.689
#> 3:    2CI        3      4      3-4  10.596  1.773  2.061 2.816  1.996  0.897
#> 4:    2CI        4      5      4-5   2.183  1.520  1.450 1.058  1.450  0.185
#> 5:    2CI        5      6      5-6   1.585  1.384  1.414 0.372  1.414  0.096
#> 6:    2CI        6      7      6-7   1.458  1.306  1.352 0.349  1.352  0.075
#>      rse     sigma n_obs n_valid n_inf n_nan valid_ratio
#>    <num>     <num> <num>   <num> <num> <num>       <num>
#> 1: 1.796 51673.677    29      16     0     0       0.552
#> 2: 1.285 51990.699    28      22     0     0       0.786
#> 3: 0.449 19350.742    27      26     0     0       0.963
#> 4: 0.128  5616.591    26      26     0     0       1.000
#> 5: 0.068  3445.872    25      25     0     0       1.000
#> 6: 0.056  3158.200    24      24     0     0       1.000
```

[`summary_ata()`](https://seokhoonj.github.io/lossratio/reference/summary_ata.md)
computes per-link statistics that drive maturity detection:

- `mean`, `median`, `wt` — descriptive averages of observed ata factors
  at each link (excluding cohorts where the link is not observed).
- `cv` — coefficient of variation of the observed factors (relative
  spread, alpha-independent).
- `f` — WLS-estimated factor (volume-weighted by `value_from^alpha`).
- `f_se`, `rse` — WLS standard error and relative standard error.
- `sigma` — Mack residual sigma per link.
- `n_obs`, `n_valid`, `n_inf`, `n_nan`, `valid_ratio` — observation
  counts and the share of finite ata factors per link.

### Diagnostic plots for `ata`

``` r

plot(ata, type = "cv")            # CV vs ata link with maturity overlay
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-6-1.png)

``` r

plot(ata, type = "rse")           # RSE vs ata link
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-6-2.png)

``` r

plot(ata, type = "summary")       # mean / median / wt overlay per link
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-6-3.png)

``` r

plot(ata, type = "box")           # boxplot of observed ata per link
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-6-4.png)

``` r

plot(ata, type = "point")         # scatter of observed ata per link
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-6-5.png)

### Triangle of ata factors

``` r

plot_triangle(ata)                                # heatmap of observed factors
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-7-1.png)

``` r

plot_triangle(ata, label_style = "detail")        # factor + (loss / rp) amounts
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-7-2.png)

``` r

plot_triangle(ata, show_maturity = TRUE)          # overlay maturity line
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-7-3.png)

The heatmap colours each cell by `log(ata / median(ata))` within its
link, so column-wise colour distinguishes cohorts that deviate from the
link’s median.

## Maturity detection

The maturity point is the development link beyond which age-to-age
factors are stable enough to trust for chain-ladder projection. Used
internally by `fit_lr(method = "sa")` to switch from ED to CL.

[`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md)
operates on a
[`summary_ata()`](https://seokhoonj.github.io/lossratio/reference/summary_ata.md)
object — first build the descriptive / WLS summary, then probe it for
the first mature link:

``` r

sm  <- summary_ata(ata, alpha = 1)
mat <- find_ata_maturity(
  sm,
  cv_threshold    = 0.10,    # CV must be below this
  rse_threshold   = 0.05,    # RSE must be below this
  min_valid_ratio = 0.5,     # at least 50% finite cohorts at the link
  min_n_valid     = 3L,      # at least 3 finite cohorts
  min_run         = 1L       # at least 1 consecutive mature link
)

print(mat)
#> Key: <cv_nm>
#>     cv_nm ata_from ata_to ata_link  mean median    wt    cv     f  f_se   rse
#>    <char>    <num>  <num>   <char> <num>  <num> <num> <num> <num> <num> <num>
#> 1:    2CI       11     12    11-12 1.184  1.174 1.182 0.070 1.182 0.019 0.016
#> 2:    CAN       12     13    12-13 1.168  1.128 1.154 0.097 1.154 0.025 0.022
#> 3:    HOS       11     12    11-12 1.154  1.148 1.153 0.087 1.153 0.023 0.020
#> 4:    SUR        9     10     9-10 1.188  1.172 1.165 0.097 1.165 0.022 0.019
#>       sigma n_obs n_valid n_inf n_nan valid_ratio
#>       <num> <num>   <num> <num> <num>       <num>
#> 1: 1226.842    19      19     0     0           1
#> 2: 1751.230    18      18     0     0           1
#> 3: 1286.484    19      19     0     0           1
#> 4: 1774.277    21      21     0     0           1
```

A row per group with the first development link satisfying all
thresholds, carrying the link’s full statistics. The threshold arguments
are also stored as attributes on the returned object.
[`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md)
is also called internally by
[`fit_ata()`](https://seokhoonj.github.io/lossratio/reference/fit_ata.md)
and
[`fit_cl()`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md)
when `maturity_args` is supplied (the `alpha` of the internal
[`summary_ata()`](https://seokhoonj.github.io/lossratio/reference/summary_ata.md)
step is taken from those callers).

Tune the thresholds to your portfolio’s volatility profile. Tight
thresholds (e.g. `cv_threshold = 0.05`) push maturity later; loose
thresholds push it earlier.

## ED diagnostics

``` r

ed <- build_ed(tri, loss_var = "closs", exposure_var = "crp")
sm <- summary_ed(ed, alpha = 1)
head(sm)
#> Key: <cv_nm>
#>     cv_nm ata_from ata_to ata_link    mean  median      wt      cv       g
#>    <char>    <num>  <num>   <fctr>   <num>   <num>   <num>   <num>   <num>
#> 1:    2CI        1      2      1-2 0.39727 0.03232 0.32519 1.59348 0.32519
#> 2:    2CI        2      3      2-3 0.47485 0.30772 0.47545 1.32737 0.47545
#> 3:    2CI        3      4      3-4 0.41184 0.29551 0.37962 1.06244 0.37962
#> 4:    2CI        4      5      4-5 0.23862 0.13582 0.22054 1.14338 0.22054
#> 5:    2CI        5      6      5-6 0.20460 0.14878 0.20793 0.75605 0.20793
#> 6:    2CI        6      7      6-7 0.18152 0.16867 0.18172 0.71140 0.18172
#>       g_se     rse    sigma n_obs n_valid n_inf n_nan valid_ratio
#>      <num>   <num>    <num> <num>   <num> <num> <num>       <num>
#> 1: 0.10625 0.32673 2013.727    29      29     0     0           1
#> 2: 0.10999 0.23134 3022.150    28      28     0     0           1
#> 3: 0.08056 0.21222 2907.043    27      27     0     0           1
#> 4: 0.05169 0.23440 2241.822    26      26     0     0           1
#> 5: 0.03124 0.15025 1590.279    25      25     0     0           1
#> 6: 0.02633 0.14490 1535.385    24      24     0     0           1

plot(ed, type = "summary")
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-9-1.png)

``` r

plot(ed, type = "box")
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-9-2.png)

``` r

plot_triangle(ed)
```

![](triangle-diagnostics_files/figure-html/unnamed-chunk-9-3.png)

[`summary_ed()`](https://seokhoonj.github.io/lossratio/reference/summary_ed.md)
is the ED-side analogue of
[`summary_ata()`](https://seokhoonj.github.io/lossratio/reference/summary_ata.md),
computing per-link statistics for the intensity
$`g_k = \Delta C^L_k / C^P_k`$.

## Validation before building

If gaps in the development sequence are suspected, inspect them before
[`build_triangle()`](https://seokhoonj.github.io/lossratio/reference/build_triangle.md):

``` r

gaps <- validate_triangle(exp, group_var = cv_nm,
                          cohort_var = "uym", dev_var = "elap_m")
head(gaps)
#> Empty data.table (0 rows and 5 cols): cv_nm,uym,n_observed,n_expected,missing
```

Returns a `triangle_validation` object with one row per cohort that has
non-consecutive development periods. An empty result means the triangle
is clean.

If gaps exist, options:

- Fix the data source (preferred).
- Drop offending cohorts.
- Pass `fill_gaps = TRUE` to
  [`build_triangle()`](https://seokhoonj.github.io/lossratio/reference/build_triangle.md)
  to zero-fill missing cells (use with care — inflates `n_obs`).

## Recent-diagonal subset

When older cohorts are no longer representative (rate change, reserving
regime shift), restrict estimation to the recent calendar diagonals:

``` r

fit_ata(ata, alpha = 1, recent = 12)        # last 12 calendar diagonals
#> <ata_fit>
#> alpha       : 1 
#> sigma_method: min_last2 
#> recent      : 12 
#> use_maturity: FALSE 
#> groups      : cv_nm 
#> n_groups    : 4 
#> ata links   : 116
fit_cl(tri, value_var = "closs", recent = 12)
#> <cl_fit>
#> method      : basic 
#> value_var   : closs 
#> weight_var  : none 
#> alpha       : 1 
#> recent      : 12 
#> use_maturity: FALSE 
#> tail_factor : 1 
#> groups      : cv_nm 
#> periods     : 30
fit_lr(tri, recent = 12)
#> <lr_fit>
#> method        : sa 
#> loss_var      : closs 
#> exposure_var  : crp 
#> loss_alpha    : 1 
#> exposure_alpha: 1 
#> delta_method  : simple 
#> conf_level    : 0.95 
#> ci_type       : analytical  
#> sigma_method  : min_last2 
#> recent        : 12 
#> maturity[2CI] : 18
#> maturity[CAN] : 18
#> maturity[HOS] : 18
#> maturity[SUR] : 18
#> groups        : cv_nm 
#> periods       : 120
```

`recent = K` keeps only rows whose calendar position
(`rank(cohort) + dev - 1`) is among the latest `K` per group.

## Workflow checklist

Before fitting:

1.  [`validate_triangle()`](https://seokhoonj.github.io/lossratio/reference/validate_triangle.md)
    — schema and gap check.
2.  [`build_triangle()`](https://seokhoonj.github.io/lossratio/reference/build_triangle.md)
    — canonical shape with derived columns.
3.  `plot(tri)` / `plot_triangle(tri)` — visual inspection.
4.  `summary(tri)` — group-level central tendency.
5.  [`build_ata()`](https://seokhoonj.github.io/lossratio/reference/build_ata.md) +
    `plot(ata, type = "cv")` — link stability.
6.  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md)
    — verify maturity detection produces a sensible point per group.
7.  [`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
    (optional) — structural change diagnosis.

Then fit
[`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md)
/
[`fit_cl()`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md)
with confidence in the input data.

## See also

- [`vignette("getting-started")`](https://seokhoonj.github.io/lossratio/articles/getting-started.md)
  — full pipeline overview.
- [`vignette("regime-detection")`](https://seokhoonj.github.io/lossratio/articles/regime-detection.md)
  —
  [`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/reference/detect_cohort_regime.md)
  deep dive.
- [`vignette("loss-ratio-methods")`](https://seokhoonj.github.io/lossratio/articles/loss-ratio-methods.md)
  — projection method choice.
