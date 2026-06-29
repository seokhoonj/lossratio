---
orphan: true
---

# 공변량은 어떻게 전체 예측과 맞나 — 유효 강도 붕괴와 속성별 예측

9장은 공변량(covariate)을 *쓰는* 법을 다뤘다. 이 레시피는 한 단계 아래 —
공변량을 넣어도 **전체 예측**(공변량으로 쪼개지 않은 코호트 x 경과 손해율
예측, 곧 `predict()`)이 그대로이고, 속성별 예측(`predict(by=)`)을 합치면 전체 예측과
*정확히* 일치하는 까닭을 본다. 두 가지 장치 덕분이다: (1) 적합에서 3차원을
2차원으로 **붕괴**시키는 유효 강도, (2) 예측에서 전체 예측을 셀별로
**배분**하는 속성별 예측.

## 1. 적합 — 3차원을 2차원으로 붕괴

공변량 모델은 셀(코호트 x 경과 x 공변량 레벨)마다 강도가 다르다.

```
E[dLoss_cell] = u_i * g_d(x) * P_cell
```

`g_d(x) = exp(s_d + X'beta)` 는 공변량 레벨 `x` 의 강도(공유 형상 `s_d` +
레벨 상대도 `beta`), `u_i` 는 코호트 신뢰도 수준, `P_cell` 은 셀의 누적
위험보험료다.

이걸 코호트 x 경과 x 레벨 3차원으로 들고 가지 않는다. 코호트의 보험료
믹스로 **주변화**(공변량에 대해 가중평균으로 합치기)해, 코호트별 경과별
**유효 강도**(공변량을 합친 하나의 강도) 한 장으로 붕괴시킨다.

```
g_eff[i, k] = sum_x  g_d(x) * w[i, k, x]      (w = 레벨 x 의 from-보험료 점유율)
```

이 `g_eff` 는 기존 코호트 x 경과 신뢰도·투영 기계가 **그대로** 소비한다 —
세 번째 축이 사라지므로 코드 경로가 바뀌지 않는다. `g_eff` 가 보험료
점유율의 가중평균이라, 공변량을 안 쓴 풀링 강도와 같은 자리에 산다.

### 손계산 — 2-레벨 슬라이스

실데이터로 한 셀의 붕괴를 끝까지 따라가 보자. 암(CANCER) 담보를 연령대
두 레벨(`20s`·`70+`)로만 자르고 그 위에 공변량을 적합한다.

```python
sl  = df.filter((pl.col("coverage") == "CANCER")
                & pl.col("age_band").is_in(["20s", "70+"]))
tri = lr.Triangle(sl, groups=["coverage", "age_band"])
fit = lr.CredibleLoss(covariates=["age_band"]).fit(tri)

fit.coefficients
#> coverage  level  beta     exp_beta
#> CANCER    20s    0.000    1.000     <- 기준
#> CANCER    70+   -1.611    0.1998
```

코호트 `2023-01-01`, 경과 1 셀의 누적 위험보험료(`from`-보험료)는 레벨별로
`20s` = 872131, `70+` = 331555 이다. 두 레벨의 강도 상대도 `g_d(x)` = `exp_beta`
와 보험료 점유율 `w` 로 `g_eff` 를 만든다:

```text
보험료 점유율   w(20s) = 872131 / (872131 + 331555) = 0.7246
                w(70+) = 331555 / (872131 + 331555) = 0.2754

유효 상대도     g_eff = exp_beta(20s)*w(20s) + exp_beta(70+)*w(70+)
                      = 1.000*0.7246 + 0.1998*0.2754
                      = 0.7796
```

이 `g_eff` = 0.7796 한 값이 세 번째 축(연령대)을 흡수한 *코호트 x 경과 한
장*의 강도다. 적합·투영 기계는 이 0.7796 만 본다 — 공변량이 없던 때와 같은
코드 경로다.

## 2. 예측 — 전체 예측을 셀별로 배분

속성별 예측(`predict(by=...)`)은 각 코호트 x 경과에서 **전체 예측 누적
손해를 공변량 셀들에 배분**한다. 배분 비중은 레벨의 상대도 x 보험료
점유율이다.

```
loss_x[i, d]  =  loss_proj[i, d] * ( g_d(x) cp_x / sum_x g_d(x) cp_x )
```

비중이 매 경과에서 1로 합산되므로, 표를 공변량에 대해 합치면
`loss_proj`(와 `premium_proj`)가 **셀 단위로 정확히 재현**된다 — 믹스가
경과에 따라 변해도, 손해만 있고 보험료 없는 셀이 있어도, 좌측 절단
코호트가 있어도. 셀별 손해율은 `전체 예측 손해율 x g_d(x)/g_eff` 라, 상대도가
큰 레벨이 더 높은 손해율로 갈린다.

### 손계산 — 배분이 합으로 돌아온다

위 슬라이스의 같은 셀(`2023-01-01`, 경과 1)의 전체 예측 누적손해는
`loss_proj` = 237447 이다. 배분 비중은 §1 의 `g_d(x)*cp_x` 를 정규화한 값으로,
그 분모는 정확히 `g_eff x 전체보험료` (= 938367) 다.

```text
g_d*cp   20s = 1.000 * 872131 = 872131.0
         70+ = 0.1998 * 331555 =  66236.1
         합                    = 938367.1   ( = g_eff 0.7796 x 1203686 )

배분 비중 20s = 872131.0 / 938367.1 = 0.9294
          70+ =  66236.1 / 938367.1 = 0.0706

손해 배분 20s = 237447 * 0.9294 = 220686.44
          70+ = 237447 * 0.0706 =  16760.56
          합                    = 237447.00   <- 전체 예측과 셀 단위로 일치
```

`predict(by=)` 가 바로 이 표를 낸다:

```python
from datetime import date

fit.predict(by="age_band").filter(
    pl.col("cohort") == date(2023, 1, 1), pl.col("duration") == 1)
#> coverage cohort     duration age_band loss_proj    premium_proj ratio_proj
#> CANCER   2023-01-01 1        20s      220686.44    872131       0.2530
#> CANCER   2023-01-01 1        70+       16760.56    331555       0.0506
# loss_proj 합 = 237447.0 = predict() 의 같은 셀
```

셀별 손해율은 `loss 배분 / premium`: `20s` = 220686.44/872131 = 0.2530,
`70+` = 16760.56/331555 = 0.0506. 상대도가 큰 `20s` 가 더 높은 손해율로
갈린다.

## 3. 그래서 — 속성별 예측은 모델이 아니라 *뷰*

핵심은 속성별 예측이 별도 예측이 *아니라* 전체 예측의 **분배**라는 점이다.
전체 예측은 공변량과 무관하게 정해지고(주변화로 같은 값), 표는 그
전체 예측을 셀별로 나눠 보여줄 뿐이다. 그래서:

- `predict(by=...)` 를 합치면 항상 `predict()` 와 일치한다.
- 공변량을 넣고 빼도 전체 예측 OOS 정확도는 사실상 그대로다.
- 얻는 것은 *분배의 모양* — 어느 셀이 손해율을 끌어올리는가(`.coefficients`)
  와 셀별 예측 손해율(`predict(by=)`).

## 4. 상대도·수축·보고 단위

### 상대도 읽기 — .coefficients

`g_d(x)` 의 레벨별 상대도는 `fit.coefficients` 로 직접 본다 — `beta` 는 로그
척도, `exp_beta` 는 곱셈 상대도다. 위 2-레벨 적합에서:

```python
fit.coefficients
#> coverage  level  beta     exp_beta
#> CANCER    20s    0.000    1.000     <- 기준 (exp(0)=1)
#> CANCER    70+   -1.611    0.1998    <- exp(-1.611) = 0.20
```

`70+` 암 손해율은 기준(`20s`)의 0.20 배다. 이 한 줄이 §1·§2 의 `g_d(x)` =
`exp_beta` 로 그대로 들어가 붕괴와 배분을 만든다.

### 수축 — lam_cov

`beta` 를 얼마나 0으로 당길지가 `lam_cov` 다. `0`(기본)은 순수 추정,
`"auto"` 는 그 공변량의 효과 분산을 데이터에서 추정하는 **신뢰도
(credibility)** 수축이다 — 레벨별 데이터가 두꺼우면 거의 안 당기고, 얇으면
기준 쪽으로 강하게 당긴다. 구현은 Schall(1991)의 분산성분 추정을 차용한다
(empirical Bayes 계열). 레벨이 많고 셀이 드문 공변량(채널 등)에서, 데이터가
얇은 레벨의 상대도가 부풀어 오르는 것을 막는다.

```python
cohs = sorted(df["uy_m"].unique().to_list())
thin = df.filter((pl.col("coverage") == "CI") & pl.col("uy_m").is_in(cohs[-4:]))
tri  = lr.Triangle(thin, groups=["coverage", "channel"])

lr.CredibleLoss(covariates=["channel"], lam_cov=0.0).fit(tri).coefficients
#> CI  ON  beta 0.714  exp_beta 2.04   <- 얇은 데이터에서 부풀어 오름
lr.CredibleLoss(covariates=["channel"], lam_cov="auto").fit(tri).coefficients
#> CI  ON  beta 0.359  exp_beta 1.43   <- 신뢰도가 기준 쪽으로 당김
```

수축은 `g_d(x)` 만 바꾸므로 위 두 장치(붕괴·배분)는 그대로고, 전체 예측
일치도 깨지지 않는다.

### 보고 단위 — covariates ⊂ groups, 보고는 groups − covariates

공변량 컬럼은 **반드시 삼각형 `groups` 의 부분집합** 이어야 한다 — 그래야
적합이 회귀할 셀이 삼각형 안에 존재한다. 적합은 그 공변량을 도로 합쳐
`groups - covariates` 단위로 보고한다. 합치는 일은 null 을 보존하는
`Triangle.collapse()` 가 맡는다.

```python
tri = lr.Triangle(df, groups=["coverage", "age_band"])
fit = lr.CredibleLoss(covariates=["age_band"]).fit(tri)

fit.groups            # 보고 단위 = groups - covariates
#> 'coverage'
fit.predict()         # coverage 단위 (age_band 는 수준 보정으로만)
fit.predict(by="age_band")   # 다시 age_band 로 쪼갠 뷰
```

group 이 아닌 컬럼을 `covariates=` 에 주면 적합이 읽을 셀이 없어 즉시
에러로 멈춘다.

## 5. 함께 보기

- {doc}`9장 — 셀 수준 공변량 <../tutorial/09-covariates>`: 공변량을 쓰는 법.
- {doc}`불확실성 계산 <uncertainty-methods>`: 같은 "한 단계 아래" 결의 레시피.
