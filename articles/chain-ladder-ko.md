# fit_cl 을 이용한 chain ladder 준비금 산출

> 영어 원본 보기: [Chain ladder reserving with
> fit_cl](https://seokhoonj.github.io/lossratio/chain-ladder.md)

[`fit_cl()`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md)
은 단일 값 컬럼에 대한 전용 chain ladder 적합 함수이다. 손해와
익스포저를 동시에 추정해 손해율을 산출하는
[`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md)
과 달리,
[`fit_cl()`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md)
은 하나의 누적 지표를 전방으로 추정하고 코호트별 Mack 방식 표준오차를
함께 계산한다.

## 기본 사용법

이 vignette 은 간결성을 위해 `SUR` 그룹만 사용한다 — 모든 절차는 다중
그룹 입력에도 그대로 일반화된다.

``` r

library(lossratio)
data(experience)
exp <- as_experience(experience)
tri <- build_triangle(exp[cv_nm == "SUR"], group_var = cv_nm)

cl <- fit_cl(tri, value_var = "closs", method = "mack")
print(cl)
#> <CLFit>
#> method      : mack 
#> value_var   : closs 
#> weight_var  : none 
#> alpha       : 1 
#> sigma_method: min_last2 
#> recent      : all 
#> use_maturity: FALSE 
#> tail_factor : 1 
#> groups      : cv_nm 
#> periods     : 30
```

`value_var` 은 추정 대상 누적 컬럼을 선택한다 — 준비금 산출에는 보통
`"closs"` (누적 손해), 익스포저 추정에는 `"crp"` (누적 위험보험료) 를
쓴다.

## 방법: basic vs Mack

두 가지 추정 방법이 제공된다.

| `method`  | 계산 내용                                       |
|-----------|-------------------------------------------------|
| `"basic"` | 점 추정만 (선택된 연속 발달비(age-to-age) 인자) |
| `"mack"`  | 점 추정 + 인자 / 프로세스 / 모수 SE             |

``` r

cl_basic <- fit_cl(tri, value_var = "closs", method = "basic")
cl_mack  <- fit_cl(tri, value_var = "closs", method = "mack")

names(cl_basic)
#>  [1] "call"          "data"          "method"        "group_var"    
#>  [5] "cohort_var"    "dev_var"       "value_var"     "full"         
#>  [9] "pred"          "ata"           "summary"       "factor"       
#> [13] "selected"      "maturity"      "alpha"         "sigma_method" 
#> [17] "weight_var"    "recent"        "use_maturity"  "maturity_args"
#> [21] "tail"          "tail_factor"

# Mack 은 $full 과 $summary 에 분산 추정값을 추가한다
head(cl_mack$summary)
#>     cv_nm     cohort     latest   ultimate   reserve     proc_se    param_se
#>    <char>     <Date>      <num>      <num>     <num>       <num>       <num>
#> 1:    SUR 2023-04-01 2442597071 2442597071         0         0.0         0.0
#> 2:    SUR 2023-05-01 2423543637 2600462323 176918686    270023.8    278555.7
#> 3:    SUR 2023-06-01 3211045456 3634951622 423906166    461673.6    481436.4
#> 4:    SUR 2023-07-01 2552396717 3106052722 553656005 217960839.9 130390124.8
#> 5:    SUR 2023-08-01 2472997731 3159902356 686904626 235800279.1 139803601.1
#> 6:    SUR 2023-09-01 2014222422 2712676355 698453933 230925350.4 124174149.4
#>             se           cv
#>          <num>        <num>
#> 1:         0.0 0.0000000000
#> 2:    387951.2 0.0001491855
#> 3:    667025.8 0.0001835034
#> 4: 253985260.2 0.0817710718
#> 5: 274129200.4 0.0867524276
#> 6: 262194082.4 0.0966551288
```

`method = "mack"` 으로 적합하면 추정 플롯의 신뢰 구간
(`show_interval = TRUE`) 을 사용할 수 있다.

``` r

plot(cl_mack, type = "projection", show_interval = TRUE)
```

![](chain-ladder-ko_files/figure-html/unnamed-chunk-3-1.png)

## Tail 인자

마지막 관측 경과 기간에서도 손해가 여전히 발달 중인 triangle 의 경우,
외삽한 tail 인자(tail factor) 로 ultimate 를 추정한다.

``` r

# 선택된 ata 인자로부터 로그 선형 외삽
cl_tail <- fit_cl(tri, value_var = "closs", method = "mack", tail = TRUE)

# 또는 명시적인 tail 인자 값 지정
cl_tail <- fit_cl(tri, value_var = "closs", method = "mack", tail = 1.025)
```

외삽은 추정된 ata 인자에 대해 $`\log(f_k - 1) \sim k`$ 회귀를 적합한 뒤,
외삽된 $`f_k`$ 의 누적 곱만큼 추정 범위를 연장한다. 기본값은 비활성
(`tail = FALSE`) 이다.

## Maturity 필터링

선택된 ata 인자가 변동성이 크다면, 추정을 성숙(mature) 영역으로 제한할
수 있다.

``` r

cl_mat <- fit_cl(
  tri,
  value_var     = "closs",
  method        = "mack",
  maturity_args = list(cv_threshold = 0.10, rse_threshold = 0.05)
)

cl_mat$maturity
#> Key: <cv_nm>
#>     cv_nm ata_from ata_to ata_link  mean median    wt    cv     f  f_se   rse
#>    <char>    <num>  <num>   <char> <num>  <num> <num> <num> <num> <num> <num>
#> 1:    SUR        9     10     9-10 1.188  1.172 1.165 0.097 1.165 0.022 0.019
#>       sigma n_obs n_valid n_inf n_nan valid_ratio
#>       <num> <num>   <num> <num> <num>       <num>
#> 1: 1774.277    21      21     0     0           1
```

`maturity_args` 는
[`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md)
로 그대로 전달된다.

## 분산 성분 (Mack)

`fit_cl(method = "mack")` 은 추정 분산을 다음과 같이 분해한다.

- `proc_se` — 프로세스 분산. $`\sigma^2_k`$ (경과 기간별 잔차 링크 분산)
  으로부터 도출.
- `param_se` — 모수 분산. 선택된 연속 발달비 인자 $`\hat{f}_k`$ 의
  불확실성으로부터 도출.
- `se` — 총 표준오차,
  $`\sqrt{\mathrm{proc\_se}^2 + \mathrm{param\_se}^2}`$.
- `cv` — 변동계수, `se / value_proj`.

``` r

summary(cl_mack)
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

## 준비금 플롯

`type = "reserve"` 는 코호트별 준비금을 (Mack 일 경우 선택적 오차 막대와
함께) 표시한다.

``` r

plot(cl_mack, type = "reserve", conf_level = 0.95)
```

![](chain-ladder-ko_files/figure-html/unnamed-chunk-7-1.png)

## Triangle 시각화

[`plot_triangle()`](https://seokhoonj.github.io/lossratio/reference/plot_triangle.md)
은 코호트 × dev 셀을 히트맵으로 표시하며, 관측된 셀과 추정된 셀을
구분한다.

``` r

plot_triangle(cl_mack, what = "full")    # 관측 + 추정
```

![](chain-ladder-ko_files/figure-html/unnamed-chunk-8-1.png)

``` r

plot_triangle(cl_mack, what = "pred")    # 추정만
```

![](chain-ladder-ko_files/figure-html/unnamed-chunk-8-2.png)

``` r

plot_triangle(cl_mack, what = "data")    # 관측만
```

![](chain-ladder-ko_files/figure-html/unnamed-chunk-8-3.png)

`label_style = "cv"` 모드는 셀별 변동계수를 표시하며, 신뢰성이 낮은 셀을
식별하는 데 유용하다.

``` r

plot_triangle(cl_mack, label_style = "cv")
```

![](chain-ladder-ko_files/figure-html/unnamed-chunk-9-1.png)

``` r

plot_triangle(cl_mack, label_style = "se")
```

![](chain-ladder-ko_files/figure-html/unnamed-chunk-9-2.png)

``` r

plot_triangle(cl_mack, label_style = "ci")
```

![](chain-ladder-ko_files/figure-html/unnamed-chunk-9-3.png)

## Sigma 외삽 방법

Mack 분산은 모든 발달 링크에서 $`\sigma_k`$ 가 필요한데, 마지막
링크에서는 직접 추정이 불가능하다. `sigma_method` 가 외삽 방식을
결정한다.

| `sigma_method` | 동작                                                       |
|----------------|------------------------------------------------------------|
| `"min_last2"`  | (기본) 추정 가능한 마지막 두 $`\sigma`$ 의 최솟값 — 보수적 |
| `"locf"`       | 마지막 관측값 carried forward                              |
| `"loglinear"`  | 관측된 $`\sigma_k`$ 시퀀스에 대한 로그 선형 외삽           |

``` r

fit_cl(tri, value_var = "closs", method = "mack", sigma_method = "loglinear")
#> <CLFit>
#> method      : mack 
#> value_var   : closs 
#> weight_var  : none 
#> alpha       : 1 
#> sigma_method: loglinear 
#> recent      : all 
#> use_maturity: FALSE 
#> tail_factor : 1 
#> groups      : cv_nm 
#> periods     : 30
```

## 함께 보기

- [`vignette("loss-ratio-methods")`](https://seokhoonj.github.io/lossratio/articles/loss-ratio-methods.md)
  —
  [`fit_lr()`](https://seokhoonj.github.io/lossratio/reference/fit_lr.md)
  을 사용해야 할 때.
- [`vignette("triangle-diagnostics")`](https://seokhoonj.github.io/lossratio/articles/triangle-diagnostics.md)
  — [`summary()`](https://rdrr.io/r/base/summary.html),
  [`find_ata_maturity()`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md),
  ata 진단 플롯.
- [`?fit_cl`](https://seokhoonj.github.io/lossratio/reference/fit_cl.md),
  [`?find_ata_maturity`](https://seokhoonj.github.io/lossratio/reference/find_ata_maturity.md),
  [`?fit_ata`](https://seokhoonj.github.io/lossratio/reference/fit_ata.md).
