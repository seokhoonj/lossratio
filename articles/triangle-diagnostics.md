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
plot(tri, value_var = "loss")          # cumulative loss instead of clr
plot(tri, summary = TRUE)              # raw + overlay (mean / median / weighted)
```

The `summary = TRUE` overlay computes mean, median, and weighted clr at
each dev and overlays them on the cohort lines. Useful for spotting
cohorts that deviate from the central tendency.

### Cell heatmap

``` r

plot_triangle(tri)                          # clr in each cell
plot_triangle(tri, value_var = "loss")      # cumulative loss
plot_triangle(tri, label_style = "detail")  # ratio + (loss / rp) amounts
```

### Group statistics by dev

``` r

sm <- summary(tri)
head(sm)
#>    cv_nm dev n_obs lr_mean lr_median   lr_wt clr_mean clr_median  clr_wt
#> 1:   ...
```

Returns a `triangle_summary` object with mean / median / weighted loss
ratios per (group, dev) cell.

## Age-to-age factor diagnostics

``` r

ata <- build_ata(tri, value_var = "closs")
sm  <- summary_ata(ata, alpha = 1)
head(sm)
#>    cv_nm ata_from ata_to ata_link  mean median    wt    cv     f  f_se   rse  sigma  n_obs  n_valid  n_inf  n_nan  valid_ratio
#> 1:   SUR        1      2      1-2  2.85   2.71  2.78  0.31  2.81  0.18  0.06   0.42     12       12      0      0         1.00
#> 2:   SUR        2      3      2-3  1.42   1.40  1.41  0.08  1.41  0.04  0.03   0.06     11       11      0      0         1.00
#> 3:   SUR        3      4      3-4  1.18   1.17  1.18  0.05  1.18  0.02  0.02   0.03     10       10      0      0         1.00
#> 4:   SUR        4      5      4-5  1.09   1.09  1.09  0.03  1.09  0.01  0.01   0.02      9        9      0      0         1.00
#> ...
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
plot(ata, type = "rse")           # RSE vs ata link
plot(ata, type = "summary")       # mean / median / wt overlay per link
plot(ata, type = "box")           # boxplot of observed ata per link
plot(ata, type = "point")         # scatter of observed ata per link
```

### Triangle of ata factors

``` r

plot_triangle(ata)                                # heatmap of observed factors
plot_triangle(ata, label_style = "detail")        # factor + (loss / rp) amounts
plot_triangle(ata, show_maturity = TRUE)          # overlay maturity line
```

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
#>    cv_nm ata_from ata_to ata_link  mean median    wt    cv     f  f_se   rse  sigma  n_obs  n_valid  n_inf  n_nan  valid_ratio
#> 1:   SUR        4      5      4-5  1.09   1.09  1.09  0.03  1.09  0.01  0.01   0.02      9        9      0      0         1.00
#> 2:   2CI        3      4      3-4  1.12   1.11  1.12  0.04  1.12  0.02  0.02   0.04     10       10      0      0         1.00
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

plot(ed, type = "summary")
plot(ed, type = "box")
plot_triangle(ed)
```

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
#>    cv_nm        uym n_observed n_expected missing
#> 1:                                       <list>
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
fit_cl(tri, value_var = "closs", recent = 12)
fit_lr(tri, recent = 12)
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
