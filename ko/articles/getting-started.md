# Getting started with lossratio

This vignette walks the full `lossratio` pipeline on the bundled
synthetic experience data, from raw long-format rows to a fitted
loss-ratio projection.

## Input shape

`lossratio` consumes long-format experience data — one row per (cohort ×
dev × demographic) cell. The bundled dataset `experience` is a
33,381-row table with calendar / underwriting period columns at multiple
granularities, demographic dimensions (`cv_nm`, `age_band`, `gender`),
and amounts (`loss`, `rp`).

``` r

library(lossratio)

data(experience)
str(experience)
#> Classes 'data.table' and 'data.frame':   33480 obs. of  17 variables:
#>  $ cy      : Date, format: "2023-01-01" "2023-01-01" ...
#>  $ cyh     : Date, format: "2023-01-01" "2023-01-01" ...
#>  $ cyq     : Date, format: "2023-04-01" "2023-04-01" ...
#>  $ cym     : Date, format: "2023-04-01" "2023-04-01" ...
#>  $ uy      : Date, format: "2023-01-01" "2023-01-01" ...
#>  $ uyh     : Date, format: "2023-01-01" "2023-01-01" ...
#>  $ uyq     : Date, format: "2023-04-01" "2023-04-01" ...
#>  $ uym     : Date, format: "2023-04-01" "2023-04-01" ...
#>  $ elap_y  : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ elap_h  : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ elap_q  : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ elap_m  : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ cv_nm   : chr  "SUR" "SUR" "SUR" "SUR" ...
#>  $ age_band: Ord.factor w/ 9 levels "30-34"<"35-39"<..: 1 1 2 2 3 3 4 4 5 5 ...
#>  $ gender  : Factor w/ 2 levels "M","F": 1 2 1 2 1 2 1 2 1 2 ...
#>  $ loss    : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ rp      : num  3555865 258448 255420 464313 92744 ...
#>  - attr(*, ".internal.selfref")=<pointer: (nil)>
```

## Step 1 — Validate and coerce

``` r

exp <- as_experience(experience)
class(exp)
#> [1] "Experience" "data.table" "data.frame"
```

[`as_experience()`](https://seokhoonj.github.io/lossratio/ko/reference/as_experience.md)
checks required columns (`cym`, `uym`, `loss`, `rp`), coerces date
columns, and tags the class. Use `check_experience(df)` for a
non-mutating check.

## Step 2 — Build the cohort × dev structure

``` r

tri <- build_triangle(exp[cv_nm == "SUR"], group_var = cv_nm)
class(tri)
#> [1] "Triangle"   "data.table" "data.frame"
names(tri)
#>  [1] "cv_nm"      "n_obs"      "cohort"     "dev"        "loss"      
#>  [6] "rp"         "closs"      "crp"        "margin"     "cmargin"   
#> [11] "profit"     "cprofit"    "lr"         "clr"        "loss_prop" 
#> [16] "rp_prop"    "closs_prop" "crp_prop"
```

[`build_triangle()`](https://seokhoonj.github.io/lossratio/ko/reference/build_triangle.md):

- aggregates demographic dimensions away (here: `age_band`, `gender`),
- adds cumulative columns (`closs`, `crp`),
- adds derived metrics (`margin`, `lr`, `clr`, proportions),
- standardises the cohort / development columns to the names `cohort`
  and `dev`,
- preserves original column names as attributes (`cohort_var`,
  `dev_var`) for downstream plot labels.

## Step 3 — Diagnostics

``` r

plot(tri)              # cohort trajectories of clr
```

![](getting-started_files/figure-html/unnamed-chunk-4-1.png)

``` r

plot_triangle(tri)     # heatmap of clr cells
```

![](getting-started_files/figure-html/unnamed-chunk-4-2.png)

``` r

summary(tri)           # group-wise statistics by dev
#> Key: <cv_nm, dev>
#>      cv_nm   dev n_obs   lr_mean  lr_median      lr_wt  clr_mean clr_median
#>     <char> <int> <int>     <num>      <num>      <num>     <num>      <num>
#>  1:    SUR     1    30 0.0738546 0.00000000 0.07343113 0.0738546  0.0000000
#>  2:    SUR     2    29 0.5365888 0.09928479 0.54126364 0.3512535  0.1120446
#>  3:    SUR     3    28 0.6201189 0.24720698 0.56590121 0.4521326  0.2618097
#>  4:    SUR     4    27 0.8852658 0.63871641 0.92060613 0.6327243  0.4798531
#>  5:    SUR     5    26 0.5767557 0.48288984 0.59880667 0.6369308  0.5641166
#>  6:    SUR     6    25 0.9314593 0.54313974 0.95085712 0.7264308  0.6191132
#>  7:    SUR     7    24 0.8240033 0.62522852 0.77687368 0.7555932  0.7992795
#>  8:    SUR     8    23 1.1675190 0.72482123 1.27544718 0.8676102  0.7931338
#>  9:    SUR     9    22 0.7525589 0.61557975 0.80158896 0.8755390  0.7890790
#> 10:    SUR    10    21 0.8335086 0.69794425 0.76977519 0.8760001  0.7896553
#> 11:    SUR    11    20 0.9673902 0.88007049 1.00022913 0.8967165  0.8355443
#> 12:    SUR    12    19 0.8517931 0.69471560 0.85401464 0.9051208  0.8833728
#> 13:    SUR    13    18 1.0020060 0.89679640 0.98253068 0.9362016  0.9420986
#> 14:    SUR    14    17 1.0962361 1.07387992 1.16483245 0.9869708  1.0624927
#> 15:    SUR    15    16 0.7760479 0.82301145 0.80108577 0.9870392  1.0459131
#> 16:    SUR    16    15 0.8254440 0.69628828 0.85204193 0.9992647  1.0388452
#> 17:    SUR    17    14 0.9058332 0.80986792 0.90979966 1.0121916  1.0649114
#> 18:    SUR    18    13 1.0184650 1.08485538 1.00362430 1.0518882  1.0717561
#> 19:    SUR    19    12 1.1760267 1.09649870 1.16189899 1.0931503  1.1135768
#> 20:    SUR    20    11 1.0221131 1.02247205 1.00298630 1.1006314  1.1221288
#> 21:    SUR    21    10 1.2370057 1.15594775 1.24931320 1.1032718  1.1256455
#> 22:    SUR    22     9 1.1949424 1.18073045 1.17762170 1.1065793  1.1255582
#> 23:    SUR    23     8 1.2851680 1.35774960 1.31179285 1.1060777  1.1285479
#> 24:    SUR    24     7 1.0598906 0.83750353 1.13727405 1.1000157  1.0774681
#> 25:    SUR    25     6 0.9385597 0.97980177 0.95054947 1.0833124  1.0638356
#> 26:    SUR    26     5 0.8196121 0.96653873 0.85145987 1.0923577  1.0737966
#> 27:    SUR    27     4 1.0466055 1.06727772 1.03231129 1.0684910  1.0866012
#> 28:    SUR    28     3 0.9876176 0.53566859 1.20396265 1.0678995  1.0663302
#> 29:    SUR    29     2 0.9678421 0.96784214 0.96314466 1.0004101  1.0004101
#> 30:    SUR    30     1 0.8877097 0.88770973 0.88770973 0.9318395  0.9318395
#>      cv_nm   dev n_obs   lr_mean  lr_median      lr_wt  clr_mean clr_median
#>     <char> <int> <int>     <num>      <num>      <num>     <num>      <num>
#>         clr_wt
#>          <num>
#>  1: 0.07343113
#>  2: 0.35150130
#>  3: 0.44744111
#>  4: 0.63467050
#>  5: 0.63999293
#>  6: 0.72781357
#>  7: 0.75210594
#>  8: 0.86554029
#>  9: 0.87613233
#> 10: 0.87381695
#> 11: 0.89806836
#> 12: 0.90531725
#> 13: 0.93753010
#> 14: 0.98899894
#> 15: 0.99133865
#> 16: 1.00482936
#> 17: 1.01782260
#> 18: 1.05697014
#> 19: 1.09700314
#> 20: 1.10409516
#> 21: 1.10555289
#> 22: 1.10949325
#> 23: 1.10906728
#> 24: 1.10067296
#> 25: 1.08291578
#> 26: 1.09382874
#> 27: 1.07050979
#> 28: 1.07487952
#> 29: 0.99818417
#> 30: 0.93183950
#>         clr_wt
#>          <num>
```

## Step 4 — Development modeling

Two complementary views of cohort development:

``` r

# Age-to-age factors
ata <- build_ata(tri, value_var = "closs")
fit_ata(ata)
#> <ATAFit>
#> alpha       : 1 
#> sigma_method: min_last2 
#> recent      : all 
#> use_maturity: FALSE 
#> groups      : cv_nm 
#> n_groups    : 1 
#> ata links   : 29

# Exposure-driven intensities
ed <- build_ed(tri, loss_var = "closs", exposure_var = "crp")
fit_ed(ed)
#> <EDFit>
#> method      : basic 
#> loss_var    : closs 
#> exposure_var: crp 
#> alpha       : 1 
#> sigma_method: min_last2 
#> recent      : all 
#> groups      : cv_nm 
#> n_groups    : 1 
#> links       : 29
```

[`fit_ata()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ata.md)
returns selected age-to-age factors per development link.
[`fit_ed()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_ed.md)
returns intensity factors $`g_k = \Delta C^L_k / C^P_k`$. Both are
inputs to the projection methods below.

## Step 5 — Projection

[`fit_cl()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_cl.md)
runs a chain ladder projection:

``` r

cl <- fit_cl(tri, value_var = "closs", method = "mack")
plot(cl, type = "projection")
```

![](getting-started_files/figure-html/unnamed-chunk-6-1.png)

``` r

summary(cl)
#>      cv_nm     cohort     latest   ultimate    reserve      proc_se    param_se
#>     <char>     <Date>      <num>      <num>      <num>        <num>       <num>
#>  1:    SUR 2023-04-01 2442597071 2442597071          0          0.0         0.0
#>  2:    SUR 2023-05-01 2423543637 2600462323  176918686     270023.8    278555.7
#>  3:    SUR 2023-06-01 3211045456 3634951622  423906166     461673.6    481436.4
#>  4:    SUR 2023-07-01 2552396717 3106052722  553656005  217960839.9 130390124.8
#>  5:    SUR 2023-08-01 2472997731 3159902356  686904626  235800279.1 139803601.1
#>  6:    SUR 2023-09-01 2014222422 2712676355  698453933  230925350.4 124174149.4
#>  7:    SUR 2023-10-01 2422172261 3464336734 1042164472  276909538.5 163800531.4
#>  8:    SUR 2023-11-01 2157147627 3350616828 1193469201  347646647.3 180286628.2
#>  9:    SUR 2023-12-01 2062030049 3510350175 1448320126  379204806.3 195291841.3
#> 10:    SUR 2024-01-01 1803809923 3316423464 1512613541  371903392.6 185291187.4
#> 11:    SUR 2024-02-01 1627213163 3293904285 1666691122  406210630.0 191768889.0
#> 12:    SUR 2024-03-01 1006624217 2212909870 1206285654  348109440.4 131371151.8
#> 13:    SUR 2024-04-01  707083238 1712964997 1005881758  316686849.1 103164315.5
#> 14:    SUR 2024-05-01  398857315 1069653530  670796215  262671175.6  65778221.0
#> 15:    SUR 2024-06-01  558855275 1654603715 1095748440  342800640.1 103939643.6
#> 16:    SUR 2024-07-01  423131366 1378042291  954910925  336548944.8  89486749.5
#> 17:    SUR 2024-08-01  457705986 1642689619 1184983633  387322899.6 109347248.0
#> 18:    SUR 2024-09-01  278007657 1166380335  888372678  360265108.1  81491441.5
#> 19:    SUR 2024-10-01  214811383 1027414225  812602841  358796882.8  74015042.6
#> 20:    SUR 2024-11-01  251273978 1400108599 1148834621  451728996.7 105050621.5
#> 21:    SUR 2024-12-01  322678180 2168358641 1845680461  619598523.6 171876903.2
#> 22:    SUR 2025-01-01  179253480 1403314580 1224061099  523388231.9 114399770.2
#> 23:    SUR 2025-02-01  100816663  954214608  853397945  497168434.1  84734257.6
#> 24:    SUR 2025-03-01  111279088 1488227973 1376948885  843027250.9 163376032.7
#> 25:    SUR 2025-04-01   55914458  958667601  902753142  751897155.3 113958290.7
#> 26:    SUR 2025-05-01   41578392 1041506132  999927740  978637571.0 147793335.1
#> 27:    SUR 2025-06-01   14997311  484991120  469993808  813441066.4  81273400.9
#> 28:    SUR 2025-07-01    6232031  436725873  430493841 5630793730.4 495929947.1
#> 29:    SUR 2025-08-01          0          0          0          0.0         0.0
#> 30:    SUR 2025-09-01          0          0          0          0.0         0.0
#>      cv_nm     cohort     latest   ultimate    reserve      proc_se    param_se
#>     <char>     <Date>      <num>      <num>      <num>        <num>       <num>
#>               se           cv
#>            <num>        <num>
#>  1:          0.0 0.000000e+00
#>  2:     387951.2 1.491855e-04
#>  3:     667025.8 1.835034e-04
#>  4:  253985260.2 8.177107e-02
#>  5:  274129200.4 8.675243e-02
#>  6:  262194082.4 9.665513e-02
#>  7:  321728933.4 9.286884e-02
#>  8:  391613916.6 1.168782e-01
#>  9:  426538613.0 1.215089e-01
#> 10:  415505664.9 1.252873e-01
#> 11:  449201939.7 1.363737e-01
#> 12:  372073328.8 1.681376e-01
#> 13:  333066714.6 1.944387e-01
#> 14:  270782054.1 2.531493e-01
#> 15:  358211848.4 2.164940e-01
#> 16:  348242832.8 2.527084e-01
#> 17:  402462233.3 2.450020e-01
#> 18:  369366759.6 3.166778e-01
#> 19:  366351511.0 3.565762e-01
#> 20:  463783052.2 3.312479e-01
#> 21:  642996112.2 2.965359e-01
#> 22:  535744854.1 3.817710e-01
#> 23:  504337532.1 5.285368e-01
#> 24:  858712218.3 5.770031e-01
#> 25:  760483940.8 7.932718e-01
#> 26:  989734492.3 9.502916e-01
#> 27:  817491121.8 1.685580e+00
#> 28: 5652590958.7 1.294311e+01
#> 29:          0.0           NA
#> 30:          0.0           NA
#>               se           cv
#>            <num>        <num>
```

[`fit_lr()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_lr.md)
runs a loss-ratio projection. The default `method = "sa"`
(stage-adaptive) uses exposure-driven before maturity and chain ladder
after, switching at the per-group maturity point detected from the ata
factors.

``` r

lr <- fit_lr(tri, method = "sa")
plot(lr, type = "clr")
```

![](getting-started_files/figure-html/unnamed-chunk-7-1.png)

``` r

summary(lr)
#>      cv_nm     cohort     latest   ultimate    reserve exposure_ult clr_latest
#>     <char>     <Date>      <num>      <num>      <num>        <num>      <num>
#>  1:    SUR 2023-04-01 2442597071 2442597071          0   2621263717 0.93183950
#>  2:    SUR 2023-05-01 2423543637 2600462323  176918686   2447179852 1.06560740
#>  3:    SUR 2023-06-01 3211045456 3634951622  423906166   3045660786 1.19909110
#>  4:    SUR 2023-07-01 2552396717 3106052722  553656005   2859002912 1.08237572
#>  5:    SUR 2023-08-01 2472997731 3159902356  686904626   2669412392 1.18159392
#>  6:    SUR 2023-09-01 2014222422 2712676355  698453933   2893769142 0.95080886
#>  7:    SUR 2023-10-01 2422172261 3464336734 1042164472   3094707500 1.14505030
#>  8:    SUR 2023-11-01 2157147627 3350616828 1193469201   2879458987 1.18696133
#>  9:    SUR 2023-12-01 2062030049 3510350175 1448320126   2843155663 1.24103980
#> 10:    SUR 2024-01-01 1803809923 3316423464 1512613541   2974323113 1.11558444
#> 11:    SUR 2024-02-01 1627213163 3293904285 1666691122   2661467247 1.22363542
#> 12:    SUR 2024-03-01 1006624217 2212909870 1206285654   2483209935 0.88837591
#> 13:    SUR 2024-04-01  707083238 1712964997 1005881758   2676093355 0.63405399
#> 14:    SUR 2024-05-01  398857315 1069653530  670796215   2650182997 0.40197016
#> 15:    SUR 2024-06-01  558855275 1654603715 1095748440   2653104694 0.62898297
#> 16:    SUR 2024-07-01  423131366 1378042291  954910925   2758730497 0.51295337
#> 17:    SUR 2024-08-01  457705986 1642689619 1184983633   2949332103 0.58596112
#> 18:    SUR 2024-09-01  278007657 1166380335  888372678   2628969704 0.45517868
#> 19:    SUR 2024-10-01  214811383 1027414225  812602841   2600171987 0.40289870
#> 20:    SUR 2024-11-01  251273978 1400108599 1148834621   2558267991 0.56379158
#> 21:    SUR 2024-12-01  322678180 2168358641 1845680461   2865484237 0.76337615
#> 22:    SUR 2025-01-01  179253480 1403314580 1224061099   2811568955 0.51475151
#> 23:    SUR 2025-02-01  100816663 1246593084 1145776421   3116396355 0.32204865
#> 24:    SUR 2025-03-01  111279088 1859420124 1748141036   2859035712 0.48317258
#> 25:    SUR 2025-04-01   55914458 1706798413 1650883954   2820900137 0.31249886
#> 26:    SUR 2025-05-01   41578392 2113613584 2072035192   3170694512 0.27248382
#> 27:    SUR 2025-06-01   14997311 1815490908 1800493597   2746665433 0.14942417
#> 28:    SUR 2025-07-01    6232031 2701876527 2695644496   3705335918 0.07318336
#> 29:    SUR 2025-08-01          0 2250325480 2250325480   2969197221 0.00000000
#> 30:    SUR 2025-09-01          0 2371912952 2371912952   2995415278 0.00000000
#>      cv_nm     cohort     latest   ultimate    reserve exposure_ult clr_latest
#>     <char>     <Date>      <num>      <num>      <num>        <num>      <num>
#>       clr_ult maturity_from      proc_se    param_se           se           cv
#>         <num>         <num>        <num>       <num>        <num>        <num>
#>  1: 0.9318395             9          0.0         0.0          0.0 0.0000000000
#>  2: 1.0626364             9     270023.8    278555.7     387951.2 0.0001491855
#>  3: 1.1934854             9     461673.6    481436.4     667025.8 0.0001835034
#>  4: 1.0864112             9  217960839.9 130390124.8  253985260.2 0.0817710718
#>  5: 1.1837445             9  235800279.1 139803601.1  274129200.4 0.0867524276
#>  6: 0.9374198             9  230925350.4 124174149.4  262194082.4 0.0966551288
#>  7: 1.1194392             9  276909538.5 163800531.4  321728933.4 0.0928688399
#>  8: 1.1636272             9  347646647.3 180286628.2  391613916.6 0.1168781561
#>  9: 1.2346669             9  379204806.3 195291841.3  426538613.0 0.1215088500
#> 10: 1.1150179             9  371903392.6 185291187.4  415505664.9 0.1252872769
#> 11: 1.2376272             9  406210630.0 191768889.0  449201939.7 0.1363737076
#> 12: 0.8911489             9  348109440.4 131371151.8  372073328.8 0.1681375883
#> 13: 0.6400991             9  316686849.1 103164315.5  333066714.6 0.1944387161
#> 14: 0.4036150             9  262671175.6  65778221.0  270782054.1 0.2531493111
#> 15: 0.6236481             9  342800640.1 103939643.6  358211848.4 0.2164940433
#> 16: 0.4995204             9  336548944.8  89486749.5  348242832.8 0.2527083784
#> 17: 0.5569700             9  387322899.6 109347248.0  402462233.3 0.2450019947
#> 18: 0.4436644             9  360265108.1  81491441.5  369366759.6 0.3166778009
#> 19: 0.3951332             9  358796882.8  74015042.6  366351511.0 0.3565762496
#> 20: 0.5472877             9  451728996.7 105050621.5  463783052.2 0.3312479136
#> 21: 0.7567163             9  619598523.6 171876903.2  642996112.2 0.2965358682
#> 22: 0.4991215             9  523388231.9 114399770.2  535744854.1 0.3817710312
#> 23: 0.4000111             9  701160977.1 151966057.1  717440170.6 0.5755207372
#> 24: 0.6503662             9 1008915815.3 229980334.0 1034795669.0 0.5565152574
#> 25: 0.6050545             9 1022367657.8 226433235.7 1047142606.3 0.6135127608
#> 26: 0.6666090             9 1151829261.7 274237724.0 1184025750.3 0.5601902634
#> 27: 0.6609800             9 1079112730.9 238225268.5 1105095274.0 0.6087032818
#> 28: 0.7291853             9 1349660320.4 349060386.5 1394068195.6 0.5159629545
#> 29: 0.7578902             9 1225790725.6 285878838.8 1258685669.0 0.5593349407
#> 30: 0.7918478             9 1250626835.9 295553919.7 1285075718.4 0.5417887353
#>       clr_ult maturity_from      proc_se    param_se           se           cv
#>         <num>         <num>        <num>       <num>        <num>        <num>
#>           se_clr       cv_clr  ci_lower  ci_upper
#>            <num>        <num>     <num>     <num>
#>  1: 0.0000000000 0.0000000000 0.9318395 0.9318395
#>  2: 0.0001585299 0.0001491855 1.0623257 1.0629471
#>  3: 0.0002190086 0.0001835034 1.1930561 1.1939146
#>  4: 0.0888370065 0.0817710718 0.9122938 1.2605285
#>  5: 0.1026927129 0.0867524276 0.9824705 1.3850186
#>  6: 0.0906064270 0.0966551288 0.7598344 1.1150051
#>  7: 0.1039610152 0.0928688399 0.9156793 1.3231990
#>  8: 0.1360026027 0.1168781561 0.8970670 1.4301874
#>  9: 0.1500229546 0.1215088500 0.9406273 1.5287065
#> 10: 0.1396975544 0.1252872769 0.8412157 1.3888201
#> 11: 0.1687798113 0.1363737076 0.9068249 1.5684296
#> 12: 0.1498356315 0.1681375883 0.5974765 1.1848214
#> 13: 0.1244600507 0.1944387161 0.3961619 0.8840363
#> 14: 0.1021748515 0.2531493111 0.2033559 0.6038740
#> 15: 0.1350160999 0.2164940433 0.3590214 0.8882748
#> 16: 0.1262330022 0.2527083784 0.2521083 0.7469326
#> 17: 0.1364587708 0.2450019947 0.2895158 0.8244243
#> 18: 0.1404986749 0.3166778009 0.1682921 0.7190368
#> 19: 0.1408951073 0.3565762496 0.1189838 0.6712825
#> 20: 0.1812879080 0.3312479136 0.1919699 0.9026055
#> 21: 0.2243935262 0.2965358682 0.3169131 1.1965195
#> 22: 0.1905501386 0.3817710312 0.1256501 0.8725929
#> 23: 0.2302146739 0.5755207372 0.0000000 0.8512236
#> 24: 0.3619387000 0.5565152574 0.0000000 1.3597530
#> 25: 0.3712086765 0.6135127608 0.0000000 1.3326102
#> 26: 0.3734278864 0.5601902634 0.0000000 1.3985142
#> 27: 0.4023406930 0.6087032818 0.0000000 1.4495533
#> 28: 0.3762326079 0.5159629545 0.0000000 1.4665877
#> 29: 0.4239144709 0.5593349407 0.0000000 1.5887473
#> 30: 0.4290142098 0.5417887353 0.0000000 1.6327002
#>           se_clr       cv_clr  ci_lower  ci_upper
#>            <num>        <num>     <num>     <num>
```

## Step 6 — Structural change diagnostics

[`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
checks whether recent cohorts behave differently from earlier ones.
Useful as a preprocessing step before loss-ratio fitting on a
homogeneous subset:

``` r

sub <- build_triangle(exp[cv_nm == "SUR"], group_var = cv_nm)
detect_cohort_regime(sub, K = 12, method = "ecp")
#> <CohortRegime>
#>   method      : ecp
#>   value_var   : clr
#>   window (K)  : elap_m 1, ..., 12
#>   cohorts     : 19 analysed (11 dropped)
#>   regimes     : 2
#>   breakpoints : 24.03
#>   PC1 / PC2   : 63.6% / 15.1%
```

See
[`vignette("regime-detection")`](https://seokhoonj.github.io/lossratio/ko/articles/regime-detection.md)
for the dedicated walkthrough.

## Where to next

- [`vignette("aggregation-frameworks")`](https://seokhoonj.github.io/lossratio/ko/articles/aggregation-frameworks.md)
  — when to use `build_triangle` vs `build_calendar` vs `build_total`.
- [`vignette("loss-ratio-methods")`](https://seokhoonj.github.io/lossratio/ko/articles/loss-ratio-methods.md)
  — choosing among `"sa"` / `"ed"` / `"cl"` in
  [`fit_lr()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_lr.md).
- [`vignette("chain-ladder")`](https://seokhoonj.github.io/lossratio/ko/articles/chain-ladder.md)
  —
  [`fit_cl()`](https://seokhoonj.github.io/lossratio/ko/reference/fit_cl.md)
  deep dive (Mack variance, tail factor).
- [`vignette("triangle-diagnostics")`](https://seokhoonj.github.io/lossratio/ko/articles/triangle-diagnostics.md)
  — [`summary()`](https://rdrr.io/r/base/summary.html),
  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/ko/reference/find_ata_maturity.md),
  and triangle-style visualisations.
- [`vignette("regime-detection")`](https://seokhoonj.github.io/lossratio/ko/articles/regime-detection.md)
  —
  [`detect_cohort_regime()`](https://seokhoonj.github.io/lossratio/ko/reference/detect_cohort_regime.md)
  for structural-change diagnosis across cohorts.
- [`vignette("backtest")`](https://seokhoonj.github.io/lossratio/ko/articles/backtest.md)
  — calendar-diagonal hold-out validation with `fit_cl` or `fit_lr`.
