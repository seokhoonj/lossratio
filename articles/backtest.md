# Backtesting projections against held-out diagonals

## Motivation

Reserving and projection methods are fitted on observed data, but their
practical value lies in how they would have performed at past valuation
dates.
[`backtest()`](https://seokhoonj.github.io/lossratio/reference/backtest.md)
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

bt <- backtest(tri_sur, holdout = 6L)
print(bt)
#> <backtest>
#>   fit_fn      : fit_lr
#>   value_var   : clr
#>   holdout     : 6 calendar diagonals
#>   held-out    : 123 cells
#>   AEG         : mean 31.28% / median 7.85%
```

The returned object is a `"backtest"` list with these key slots:

- `aeg` — per-cell `data.table` (cohort, dev, actual, pred, aeg,
  calendar_idx).
- `col_summary` — AEG aggregated by `dev`.
- `diag_summary` — AEG aggregated by calendar diagonal.
- `masked` — the triangle the fit was trained on (latest diagonals
  removed).
- `fit` — the fit object returned by `fit_fn` (an `lr_fit` or `cl_fit`).

`summary(bt)` prints the two summary tables alongside the call metadata.

## Validation coverage after masking

Masking the latest `holdout` diagonals shortens the triangle’s
lower-right edge. Chain ladder can only project as far as the largest
dev still observed in the masked data, so cells beyond that range — the
oldest cohorts at their latest dev — have no projection to compare
against. These unreachable cells are silently dropped, so `bt$aeg`
contains only cells where both an actual and a finite projection exist.

Practical takeaway: as `holdout` grows, the validation set shrinks
fastest in the oldest cohorts’ late-dev region — exactly where chain
ladder relies on extrapolation (projection beyond the observed dev
range), so it is the area most in need of validation yet the first to
disappear.

## Output interpretation

**`col_summary` — systematic bias by development period.** A
consistently signed AEG at a given dev signals a structural mismatch
between the model and that maturity. Early-dev positive values usually
reflect inflated link factors; late-dev values flag tail miscalibration.

``` r

head(bt$col_summary, 8)
#>     cv_nm   dev     n  aeg_mean   aeg_med    aeg_wt
#>    <char> <int> <int>     <num>     <num>     <num>
#> 1:    SUR     2     1 0.2837315 0.2837315 0.2837315
#> 2:    SUR     3     2 2.1989647 2.1989647 1.6068271
#> 3:    SUR     4     3 1.6455467 0.1315263 0.5380333
#> 4:    SUR     5     4 0.6585572 0.2763337 0.4646954
#> 5:    SUR     6     5 0.9926610 0.6708877 0.8535695
#> 6:    SUR     7     6 0.8134587 0.5405731 0.4912874
#> 7:    SUR     8     6 0.7926697 0.7754185 0.6533346
#> 8:    SUR     9     6 0.5582173 0.5978053 0.4457645
```

`aeg_mean` averages cell-level AEG, `aeg_med` is the median, and
`aeg_wt = sum(pred - actual) / sum(actual)` is exposure-weighted.
Comparing the three columns flags whether a few large cells dominate
(`aeg_wt` very different from `aeg_med`) or the bias is uniform.

**`diag_summary` — calendar-year effect.** A single bad diagonal in
otherwise unbiased output points at a calendar event (a rate change,
claim handling shift, or one-off shock) that a static fitter cannot see
by construction.

``` r

bt$diag_summary
#>     cv_nm calendar_idx     n  aeg_mean    aeg_med    aeg_wt
#>    <char>        <int> <int>     <num>      <num>     <num>
#> 1:    SUR           25    23 0.2613952 0.03817686 0.0754984
#> 2:    SUR           26    22 0.4210424 0.05473891 0.1278466
#> 3:    SUR           27    21 0.2465127 0.06217712 0.1162987
#> 4:    SUR           28    20 0.2926339 0.08366472 0.1460770
#> 5:    SUR           29    19 0.3595587 0.18991295 0.2010825
#> 6:    SUR           30    18 0.2968898 0.11898697 0.1976781
```

A monotone drift across calendar diagonals (as in the SUR example above,
where AEG becomes increasingly negative across `25, ..., 30`) typically
indicates that the latest period is running better than the
earlier-cohort link factors imply.

**`aeg` — cell-level outliers.** For diagnosing specific cohort × dev
cells, inspect `bt$aeg` directly:

``` r

head(bt$aeg, 5)
#> Key: <cv_nm>
#>     cv_nm     cohort   dev value_actual value_pred         aeg calendar_idx
#>    <char>     <Date> <int>        <num>      <num>       <num>        <int>
#> 1:    SUR 2023-05-01    24     1.030446   1.156866 0.122684167           25
#> 2:    SUR 2023-06-01    23     1.175862   1.182519 0.005661816           25
#> 3:    SUR 2023-06-01    24     1.198728   1.292790 0.078467444           26
#> 4:    SUR 2023-07-01    22     1.105530   1.113031 0.006784601           25
#> 5:    SUR 2023-07-01    23     1.106120   1.118137 0.010863743           26
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
[`plot_triangle()`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.md)
puts the cell-level AEG values on the same triangular layout as
[`plot_triangle()`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.md)
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

The default fitter is `fit_lr` with `method = "sa"` and
`value_var = "clr"`. The loss ratio is unitless and dimension-free
across cohorts of very different volume, so `aeg_mean` and `aeg_med`
carry a consistent meaning across the triangle. `value_var` selects
which projection column on `fit$full` is compared against the held-out
actuals; for `fit_lr` — which projects loss and exposure jointly — the
mapping is:

| `value_var` | Compared column on `fit_lr$full` |
|-------------|----------------------------------|
| `"closs"`   | `loss_proj`                      |
| `"crp"`     | `exposure_proj`                  |
| `"clr"`     | `clr_proj`                       |

The `method` argument selects the underlying loss-ratio projection
strategy: `"sa"` (stage-adaptive, the default) blends exposure-driven
projections before the maturity point with chain ladder afterwards;
`"ed"` is purely exposure-driven; `"cl"` is the classical chain ladder
applied to `clr`.

``` r

bt_sa  <- backtest(tri_sur, holdout = 6L, method = "sa")   # default
bt_ed  <- backtest(tri_sur, holdout = 6L, method = "ed")
bt_cl  <- backtest(tri_sur, holdout = 6L, method = "cl")

bt_loss <- backtest(tri_sur, holdout = 6L, value_var = "closs")
bt_rp   <- backtest(tri_sur, holdout = 6L, value_var = "crp")

print(bt_sa)
#> <backtest>
#>   fit_fn      : fit_lr
#>   value_var   : clr
#>   holdout     : 6 calendar diagonals
#>   held-out    : 123 cells
#>   AEG         : mean 31.28% / median 7.85%
```

Backtesting `closs` weights the result toward whichever cohorts happen
to be the largest at the held-out diagonals, which is useful when
monetary impact matters more than a normalized comparison.

If you only need to project a single triangle column (e.g., raw
cumulative loss without forming a ratio), `fit_fn = fit_cl` is also
supported.

## See also

- [`vignette("chain-ladder")`](https://seokhoonj.github.io/lossratio/articles/chain-ladder.md)
  —
  [`fit_cl()`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md)
  reference.
- [`vignette("loss-ratio-methods")`](https://seokhoonj.github.io/lossratio/articles/loss-ratio-methods.md)
  —
  [`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md)
  and the `"sa"`, `"ed"`, `"cl"` methods.
- [`?backtest`](https://seokhoonj.github.io/lossratio/reference/backtest.md),
  [`?plot.backtest`](https://seokhoonj.github.io/lossratio/reference/plot.backtest.md),
  [`?plot_triangle.backtest`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.backtest.md).
