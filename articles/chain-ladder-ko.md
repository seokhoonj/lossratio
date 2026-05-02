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

``` r

library(lossratio)
data(experience)
exp <- as_experience(experience)
tri <- build_triangle(exp, group_var = cv_nm)

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
#> periods     : 120
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
#>     cv_nm     cohort     latest   ultimate   reserve   proc_se   param_se
#>    <char>     <Date>      <num>      <num>     <num>     <num>      <num>
#> 1:    2CI 2023-04-01 1328747444 1328747444         0         0        0.0
#> 2:    2CI 2023-05-01 1291252929 1439747016 148494087    197098   205179.4
#> 3:    2CI 2023-06-01 1376369414 1678909175 302539761  98147051 76418777.8
#> 4:    2CI 2023-07-01 1014392879 1276962309 262569429  86025353 58305083.3
#> 5:    2CI 2023-08-01  977430606 1315332423 337901817 107739939 67248203.1
#> 6:    2CI 2023-09-01 1288259684 1849768703 561509019 132231461 96170407.8
#>             se           cv
#>          <num>        <num>
#> 1:         0.0 0.0000000000
#> 2:    284510.5 0.0001976114
#> 3: 124389200.8 0.0740892972
#> 4: 103922298.1 0.0813824319
#> 5: 127004784.2 0.0965571759
#> 6: 163505066.0 0.0883921680
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
#>       cv_nm     cohort     latest   ultimate   reserve    proc_se    param_se
#>      <char>     <Date>      <num>      <num>     <num>      <num>       <num>
#>   1:    2CI 2023-04-01 1328747444 1328747444         0          0         0.0
#>   2:    2CI 2023-05-01 1291252929 1439747016 148494087     197098    205179.4
#>   3:    2CI 2023-06-01 1376369414 1678909175 302539761   98147051  76418777.8
#>   4:    2CI 2023-07-01 1014392879 1276962309 262569429   86025353  58305083.3
#>   5:    2CI 2023-08-01  977430606 1315332423 337901817  107739939  67248203.1
#>  ---                                                                         
#> 116:    SUR 2025-05-01   41578392 1041506132 999927740  978637571 147793335.1
#> 117:    SUR 2025-06-01   14997311  484991120 469993808  813441066  81273400.9
#> 118:    SUR 2025-07-01    6232031  436725873 430493841 5630793730 495929947.1
#> 119:    SUR 2025-08-01          0          0         0          0         0.0
#> 120:    SUR 2025-09-01          0          0         0          0         0.0
#>                se           cv
#>             <num>        <num>
#>   1:          0.0 0.000000e+00
#>   2:     284510.5 1.976114e-04
#>   3:  124389200.8 7.408930e-02
#>   4:  103922298.1 8.138243e-02
#>   5:  127004784.2 9.655718e-02
#>  ---                          
#> 116:  989734492.3 9.502916e-01
#> 117:  817491121.8 1.685580e+00
#> 118: 5652590958.7 1.294311e+01
#> 119:          0.0           NA
#> 120:          0.0           NA
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
#> periods     : 120
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
