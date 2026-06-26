# 9장. 셀 수준 공변량 — covariates

앞 장들은 손해율을 **코호트 x 경과 기간** 한 장으로 예측했다. 그런데 한
코호트 안에는 성별·연령대·채널처럼 셀을 더 잘게 가르는 속성이 있다. 이
장은 그런 속성을 **공변량(covariate)** 으로 모델에 넣어 (1) 그 속성이
손해율 수준을 얼마나 올리고 내리는지 *해석*하고, (2) 속성별 손해율을
*나눠서* 보는 법을 다룬다.

## 9.1 쪼갤 것인가, 회귀할 것인가 — groups vs covariate

먼저 삼각형을 그 속성까지 잘게 묶어 만든다. 그다음 그 속성을 두 가지로
다룰 수 있다.

- **그대로 group 으로 둔다(partition).** 속성 값마다 독립된 삼각형을 따로
  적합한다. 각 조각은 서로를 보지 않는다 — 조각이 얇으면 불안정하다.
- **`covariates=` 로 회귀한다(regress).** *하나의 적합 안에서* 그 속성을 셀
  수준 고정효과(fixed effect)로 추정한다. 경과 형상(shape)은 모든 셀이
  공유(풀링)하고, 손해율 수준(level)만 속성별로 다르게 둔다.

핵심 규칙은 **`covariates` 는 삼각형 `groups` 의 부분집합** 이라는 것이다.
공변량으로 쓰려면 먼저 그 컬럼으로 삼각형을 묶어 두어야 한다(그래야 적합이
읽을 셀이 존재한다). 그러면 적합은 그 공변량을 도로 합쳐 **`groups -
covariates` 단위로 보고** 하고, 공변량은 형상을 공유한 채 수준만 옮긴다.

```{list-table}
:header-rows: 1
:widths: 40 60

* - 삼각형 / 적합
  - 보고 단위 (출력 grain)
* - `Triangle(groups=["coverage","age_band"])` + `covariates=["age_band"]`
  - `coverage` (age_band 는 수준 보정으로만)
* - `Triangle(groups=["coverage","age_band"])`, covariate 없음
  - `coverage x age_band` (독립 적합)
```

왜 쪼개지 않고 공변량으로 두나. 담보 x 성별 x 연령을 전부 독립 group 으로
쪼개면 조각이 얇아져 불안정하다. 공변량은 **경과 형상을 모든 셀이 공유** 하고
**수준 차이만** 속성별로 두므로, 얇은 조각도 공유 형상을 빌려 버틴다.

## 9.2 어떻게 넣나 — groups 에 묶고 covariates 로 회귀

공변량으로 쓸 컬럼을 삼각형 `groups` 에 포함시킨 뒤, `covariates=` 에 그
컬럼을 준다. 적합은 삼각형 자신의 (보고 단위 x 공변량) 셀에서 회귀 데이터를
읽으므로 원본 데이터를 따로 넘길 필요가 없다.

```python
import lossratio as lr

df = lr.load_experience()                              # coverage, age_band, ...
tri = lr.Triangle(df, groups=["coverage", "age_band"])

fit = lr.CredibleLoss(covariates=["age_band"]).fit(tri)
```

- `covariates=` 는 셀 수준 공변량 컬럼 목록이며, **반드시 `groups` 의
  부분집합** 이다. group 이 아닌 컬럼을 주면 즉시 에러로 멈춘다.
- 적합은 age_band 를 도로 합쳐 **coverage 단위로 예측** 하고(`fit.groups` 가
  `"coverage"`), age_band 는 공유 경과 형상 위의 수준 보정으로 들어간다.
- 공변량은 `PooledLoss` · `CredibleLoss` · `SmoothLoss` 어디서나 쓴다.

## 9.3 상대도 읽기 — .coefficients

적합은 공변량 레벨별 로그 상대도(기준 레벨 = 0, 즉 `exp_beta` = 1)를 낸다.

```python
fit.coefficients
#> coverage  covariate  level  beta    exp_beta
#> CANCER    age_band   20s    0.00    1.00     <- 기준 레벨
#> CANCER    age_band   30s   -0.29    0.75
#> CANCER    age_band   40s   -0.68    0.51
#> CANCER    age_band   50s   -0.98    0.37
#> CANCER    age_band   60s   -1.32    0.27
#> CANCER    age_band   70+   -1.61    0.20
```

`exp_beta` 는 손해율의 곱셈 상대도다. 위(합성 데이터)에서는 20s 대비 30s 가
0.75 배, 70+ 는 0.20 배로 달린다. **이것이 공변량의 1차 가치 — 계리적
해석이다.**

## 9.4 속성별 손해율 — predict(by=)

4장의 `predict()` 는 공변량으로 쪼개지 않은 **전체 예측** — 코호트 x 경과 한
장의 손해율 예측 — 을 준다. 공변량을 넣었으면 `predict(by="age_band")` 로 그
전체 예측을 속성별로 *나눠* 볼 수 있다.

```python
fit.predict()               # 전체 예측: 코호트 x 경과 (coverage 단위)
fit.predict(by="age_band")  # 속성별: 코호트 x 경과 x 연령대
#> coverage cohort     duration age_band loss_proj   ratio_proj source
#> CANCER   2023-01-01 1        20s      248097.05   0.284      observed
#> CANCER   2023-01-01 1        30s      231168.33   0.212      observed
#> CANCER   2023-01-01 1        40s      160962.81   0.144      observed
#> CANCER   2023-01-01 1        50s       83324.38   0.106      observed
#> CANCER   2023-01-01 1        60s       49269.00   0.076      observed
#> CANCER   2023-01-01 1        70+       18843.43   0.057      observed
```

각 (코호트, 경과)의 전체 예측을 연령대 셀로 나눠 담은 표다 — 연령대별
예측 손해율을 따로 본다(위에서 `ratio_proj` 가 9.3 의 상대도 순서를 그대로
따른다). 이 표를 다시 연령대에 대해 합치면 **전체 예측과 정확히 같아진다**:
나눠 본 것일 뿐 다른 모델이 아니다(한 판을 조각냈다 다시 합치면 같은 한 판).

## 9.5 수축 — lam_cov

공변량 효과를 얼마나 믿을지(0 쪽으로 얼마나 당길지)를 정한다.

```{list-table}
:header-rows: 1
:widths: 22 78

* - `lam_cov`
  - 무엇
* - `0` (기본)
  - 순수 고정효과(MLE), 수축 없음. 성별처럼 레벨이 적은 공변량에 적합 —
    교과서 기본형.
* - `"auto"`
  - **데이터가 수축량을 정한다 = 신뢰도(credibility)** (통계 용어로는
    empirical Bayes). 레벨들이 정말 다르면 거의 안 수축(신호 유지), 잡음
    같으면 기준 쪽으로 강하게 수축. 값이 여러 개고 데이터가 드문 공변량
    (채널·연령대처럼 레벨이 많은 것)에 권장.
* - `{컬럼: 값}`
  - 공변량별로 따로 지정(`"auto"` 또는 고정 값).
```

`lam_cov` 은 ridge(L2) 페널티의 강도다. 세 모드는 다른 메커니즘이 아니라
*같은 ridge* 에서 강도를 고르는 세 방법일 뿐이다 — `0` 은 페널티 없음(MLE),
`"auto"` 는 그 강도를 데이터에서 신뢰도식으로 추정, 숫자는 사용자가 직접
고정. `lam_cov="auto"` 가 중요한 까닭: 레벨이 많고 데이터가 드문 공변량을
수축 없이 적합하면, 데이터가 거의 없는 레벨에서 상대도가 분리(separation --
그 레벨만 손해/무손해로 갈려 추정이 무한대로 튐)로 폭발한다. `"auto"` 는 그
공변량의 효과 분산을 데이터에서 추정해(레벨 간 변동이 작으면 강하게 수축)
폭발을 막으면서도 진짜 신호는 살린다.

```python
tri = lr.Triangle(df, groups=["coverage", "channel"])
lr.CredibleLoss(covariates=["channel"], lam_cov="auto").fit(tri)
```

## 9.6 전체 예측은 (거의) 안 바뀐다

정직하게 짚을 점. 담보별로 쪼개 손해율만 예측한다면, **공변량은 전체 예측을
거의 바꾸지 않는다.** 속성별로 나눈 뒤 합치면(9.4) 같은 값으로 돌아오기 때문이다
(실데이터 검증: 공변량 유 vs 무의 전체 예측 차이 ~0.1%).

그러므로 공변량의 가치는 *전체 예측의 정확도* 가 아니라 **`.coefficients`
(해석, 9.3)와 `predict(by=)`(속성별 예측, 9.4)** 에 있다. 전체 손해율만 필요하면
공변량은 넣지 않아도 된다.

## 9.7 제약

- 공변량 컬럼은 **반드시 삼각형 `groups` 의 부분집합** 이다 (group 이 아닌
  컬럼은 적합이 읽을 셀이 없어 에러).
- 공변량은 `balance=` 와 동시에 쓸 수 없다(서로 배타적).
- 진단·예측의 grain 도 공변량을 따라간다 — 백테스트는 공변량 적합의 보고
  단위(`groups - covariates`)에서 채점하고, 가려진(held-out) 셀은 삼각형
  마스킹을 통해 공변량 적합에도 그대로 가려져 새어 들지 않는다.

## 9.8 함께 보기

- {doc}`4장 — 손해율 예측 <04-projection>`: 공변량이 다듬는 그 예측.
- {doc}`공변량 레시피 <../cookbook/covariates>`: 속성별 예측이 전체 예측과
  정확히 맞는 까닭을 한 단계 아래에서.
- {doc}`API 레퍼런스 <../api>` 의 `PooledLoss` · `CredibleLoss` ·
  `SmoothLoss` 의 `covariates` · `lam_cov`.
```
