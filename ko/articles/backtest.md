# Backtesting projections against held-out diagonals

## Motivation

Reserving and projection methods are fitted on observed data, but their
practical value lies in how they would have performed at past valuation
dates.
[`backtest()`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
answers that question by hiding the latest `holdout` calendar diagonals
from a triangle, refitting the model on the earlier portion, and
comparing its projection to the actuals that were withheld. This is
calendar-diagonal hold-out (rather than dev-period hold-out), because it
simulates “what would the model have said *K* months ago at the
valuation date?”. The cell-level metric is the Actual-Expected Gap,
$`\mathrm{aeg} = v_{\mathrm{pred}} / v_{\mathrm{actual}} - 1`$, where
positive values flag over-projection and negative values flag
under-projection.

## Basic usage

``` r

library(lossratio)
data(experience)
exp     <- as_experience(experience)
tri_sur <- build_triangle(exp[cv_nm == "SUR"], cv_nm)

bt <- backtest(tri_sur, holdout = 6L, value_var = "closs", method = "mack")
print(bt)
#> <backtest>
#>   fit_fn      : fit_cl
#>   value_var   : closs
#>   holdout     : 6 calendar diagonals
#>   held-out    : 123 cells
#>   AEG         : mean 3.67% / median 2.37%
```

The returned object is a `"backtest"` list with these key slots:

- `aeg` — per-cell `data.table` (cohort, dev, actual, pred, aeg,
  calendar_idx).
- `col_summary` — AEG aggregated by `dev`.
- `diag_summary` — AEG aggregated by calendar diagonal.
- `masked` — the triangle the fit was trained on (latest diagonals
  removed).
- `fit` — the fit object returned by `fit_fn` (a `cl_fit` or `lr_fit`).

`summary(bt)` prints the two summary tables alongside the call metadata.

## Calendar-diagonal masking limitation

Removing the latest `holdout` diagonals shortens the lower-right edge of
the triangle. A chain ladder fit on the masked triangle can only project
as far as its longest cohort × dev support; cells beyond that support,
which would belong to the very oldest cohorts at the largest development
periods, simply have no projection to compare against. The function
silently filters those unreachable cells, so `bt$aeg` always contains
only cells where both an actual and a finite projection exist. The
practical takeaway: `holdout` larger than a few diagonals reduces the
validation set fastest in the oldest cohorts, where the chain ladder
would otherwise rely on its own extrapolated tail.

## Output interpretation

**`col_summary` — systematic bias by development period.** A
consistently signed AEG at a given dev signals a structural mismatch
between the model and that maturity. Early-dev positive values usually
reflect inflated link factors; late-dev values flag tail miscalibration.

``` r

head(bt$col_summary, 8)
#>     cv_nm   dev     n    aeg_mean      aeg_med       aeg_wt
#>    <char> <int> <int>       <num>        <num>        <num>
#> 1:    SUR     2     1  0.35265816  0.352658163  0.352658163
#> 2:    SUR     3     2  1.09194930  1.091949300  1.238708134
#> 3:    SUR     4     3  0.26297174  0.149141399  0.002701091
#> 4:    SUR     5     4 -0.03488373 -0.005683932 -0.003633722
#> 5:    SUR     6     5  0.11717321  0.170952712  0.158239539
#> 6:    SUR     7     6  0.02372767  0.088320648 -0.064009176
#> 7:    SUR     8     6 -0.12194303 -0.209316200 -0.173876247
#> 8:    SUR     9     6 -0.05656802 -0.153320463 -0.120480803
```

`aeg_mean` averages cell-level AEG, `aeg_med` is the median, and
`aeg_wt = sum(pred - actual) / sum(actual)` is exposure-weighted.
Comparing the three columns flags whether a few large cells dominate
(`aeg_wt` very different from `aeg_med`) or the bias is uniform.

**`diag_summary` — calendar-year effect.** A single bad diagonal in
otherwise unbiased output points at a calendar event (a rate change,
claim handling shift, or one-off shock) that a static chain ladder
cannot see by construction.

``` r

bt$diag_summary
#>     cv_nm calendar_idx     n     aeg_mean    aeg_med     aeg_wt
#>    <char>        <int> <int>        <num>      <num>      <num>
#> 1:    SUR           25    23  0.057628989 0.02040208 0.03453846
#> 2:    SUR           26    22  0.138882688 0.04596682 0.02951810
#> 3:    SUR           27    21 -0.001821114 0.01078351 0.03417155
#> 4:    SUR           28    20 -0.006580479 0.01864699 0.02640584
#> 5:    SUR           29    19  0.025964844 0.02624162 0.05352598
#> 6:    SUR           30    18 -0.010504749 0.03836698 0.06807051
```

A monotone drift across calendar diagonals (as in the SUR example above,
where AEG becomes increasingly negative across `25, ..., 30`) typically
indicates that the latest period is running better than the
earlier-cohort link factors imply.

**`aeg` — cell-level outliers.** For diagnosing specific cohort × dev
cells, inspect `bt$aeg` directly:

``` r

head(bt$aeg, 5)
#>     cv_nm     cohort   dev value_actual value_pred          aeg calendar_idx
#>    <char>     <Date> <int>        <num>      <num>        <num>        <int>
#> 1:    SUR 2023-05-01    24   1751200630 2101185808  0.199854415           25
#> 2:    SUR 2023-06-01    23   2137306940 2180912455  0.020402084           25
#> 3:    SUR 2023-06-01    24   2353384510 2684703232  0.140783931           26
#> 4:    SUR 2023-07-01    22   1892578390 1885245251 -0.003874681           25
#> 5:    SUR 2023-07-01    23   2021258324 2064343550  0.021316042           26
```

## Plot demos

Four plot views are registered on `"backtest"`:

``` r

plot(bt, type = "col")    # AEG by dev (point + dashed zero line)
```

![](backtest_files/figure-html/unnamed-chunk-5-1.png)

``` r

plot(bt, type = "diag")   # AEG by calendar diagonal
```

![](backtest_files/figure-html/unnamed-chunk-5-2.png)

``` r

plot(bt, type = "cell")   # per-cohort AEG trajectories over dev
```

![](backtest_files/figure-html/unnamed-chunk-5-3.png)

``` r

plot_triangle(bt)         # diverging-color heatmap on the held-out wedge
```

![](backtest_files/figure-html/unnamed-chunk-5-4.png)

`type = "col"` is the right place to look for systematic dev-period
bias; `type = "diag"` reveals calendar-year drift; `type = "cell"`
exposes which cohorts contribute the bias;
[`plot_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.md)
puts the cell-level AEG values on the same triangular layout as
[`plot_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.md)
for the underlying fit, with a red/blue diverging palette where red
marks over-projection.

## Holdout selection

Choose `holdout` to balance two opposing effects:

- Too large: the masked triangle loses its latest experience, so the
  oldest cohorts have few or no reachable cells in their later dev
  periods. The validation set shrinks unevenly, biased toward early dev.
- Too small: the held-out wedge is just a thin diagonal band, which may
  not capture enough cells to reveal systematic patterns.

Typical choices are `holdout = 6L` (half-year) for monthly triangles, or
`holdout = 12L` (full year) for stronger validation when the triangle
has at least 24–30 diagonals of history.

## Choosing the fit function

[`backtest()`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md)
supports both `fit_cl` and `fit_lr`. The fitter is passed through
`fit_fn`, and `value_var` selects which projection column on `fit$full`
to compare against the held-out actuals. For `fit_cl`, `value_var` is
also forwarded to the fit itself. For `fit_lr` — which projects loss and
exposure jointly — `value_var` only chooses the comparison column, with
the mapping:

| `value_var` | Compared column on `fit_lr$full` |
|-------------|----------------------------------|
| `"closs"`   | `loss_proj`                      |
| `"crp"`     | `exposure_proj`                  |
| `"clr"`     | `clr_proj`                       |

``` r

bt_cl  <- backtest(tri_sur, holdout = 6L, fit_fn = fit_cl,
                   value_var = "closs", method = "mack")
bt_lr  <- backtest(tri_sur, holdout = 6L, fit_fn = fit_lr,
                   method = "sa", value_var = "closs")
bt_clr <- backtest(tri_sur, holdout = 6L, fit_fn = fit_lr,
                   method = "sa", value_var = "clr")

print(bt_clr)
#> <backtest>
#>   fit_fn      : fit_lr
#>   value_var   : clr
#>   holdout     : 6 calendar diagonals
#>   held-out    : 123 cells
#>   AEG         : mean 31.28% / median 7.85%
```

Backtesting `clr` is often the more informative diagnostic: the loss
ratio is unitless and dimension-free across cohorts of very different
volume, so `aeg_mean` and `aeg_med` carry a consistent meaning across
the triangle. Backtesting `closs` weights the result toward whichever
cohorts happen to be the largest at the held-out diagonals.

## See also

- [`vignette("chain-ladder")`](https://seokhoonj.github.io/lossratio/ko/articles/chain-ladder.md)
  —
  [`fit_cl()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_cl.md)
  reference.
- [`vignette("loss-ratio-methods")`](https://seokhoonj.github.io/lossratio/ko/articles/loss-ratio-methods.md)
  —
  [`fit_lr()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_lr.md)
  and the `"sa"`, `"ed"`, `"cl"` methods.
- [`?backtest`](https://seokhoonj.github.io/lossratio/ko/reference/backtest.md),
  [`?plot.backtest`](https://seokhoonj.github.io/lossratio/ko/reference/plot.backtest.md),
  [`?plot_triangle.backtest`](https://seokhoonj.github.io/lossratio/ko/reference/plot_triangle.backtest.md).
