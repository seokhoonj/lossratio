# 9장. 셀 수준 공변량 — covariates

앞 장들은 손해율을 **코호트 x 경과 기간** 한 장으로 예측했다. 그런데 한
코호트 안에는 성별·연령대·채널처럼 셀을 더 잘게 가르는 속성이 있다. 이
장은 그런 속성을 **공변량(covariate)** 으로 모델에 넣어 (1) 그 속성이
손해율 수준을 얼마나 올리고 내리는지 *해석*하고, (2) 속성별 손해율을
*분해*해서 보는 법을 다룬다.

## 9.1 쪼갤 것인가, 회귀할 것인가 — groups vs covariate

한 속성을 다루는 길은 둘이다.

- **`groups=` 로 쪼갠다(partition).** 그 속성의 값마다 독립된 삼각형을
  따로 적합한다. 각 조각은 서로를 보지 않는다.
- **`covariates=` 로 회귀한다(regress).** *하나의 적합 안에서* 그 속성을
  셀 수준 고정효과(fixed effect)로 추정한다. 경과 형상(shape)은 모든 셀이
  공유하고, 손해율 수준(level)만 속성별로 다르게 둔다.

규칙은 **"그 속성으로 쪼개거나(groups), 그 속성에 회귀하거나(covariate),
둘 다는 하지 않는다"** 이다. 같은 컬럼을 양쪽에 주면 에러다 — 회귀는 그
속성이 *코호트 안에서 변해야* 식별되는데, `groups` 로 쪼개면 코호트 안에서
상수가 되어 코호트 신뢰도 수준에 흡수되어 아무것도 추정하지 못한다.

왜 쪼개지 않고 공변량으로 두나. 담보 x 성별 x 연령을 전부 `groups` 로
쪼개면 조각이 얇아져 불안정하다. 공변량은 **경과 형상을 모든 셀이
공유(풀링)** 하고 **수준 차이만** 속성별로 두므로, 얇은 조각도 공유 형상을
빌려 버틴다.

## 9.2 어떻게 넣나 — covariates= 와 source=

삼각형은 이미 집계되어 공변량 컬럼을 잃었으므로, 적합 시점에 원본
(분해된) frame 을 따로 읽어 셀을 복원한다.

```python
import lossratio as lr

df = lr.load_experience()                 # coverage, age_band, channel, ...
tri = lr.Triangle(df, groups="coverage")

fit = lr.CredibleLoss(covariates=["age_band"], source=df).fit(tri)
```

- `covariates=` 는 셀 수준 공변량 컬럼 목록이다.
- `source=` 는 그 공변량을 읽어올 **원본 frame** 이다. `source` 를 공변량에
  대해 합산하면 삼각형의 셀과 정확히 일치해야 하며, 어긋나면 즉시 에러로
  멈춘다(조용히 틀린 값을 내지 않는다).
- 공변량은 `PooledLoss` · `CredibleLoss` · `SmoothLoss` 어디서나 쓴다.

## 9.3 상대도 읽기 — .coefficients

적합은 공변량 레벨별 로그 상대도(기준 레벨 = 0, 즉 `exp_beta` = 1)를 낸다.

```python
fit.coefficients
#> coverage  covariate  level  beta    exp_beta
#> CANCER    age_band   20s    0.00    1.00     <- 기준 레벨
#> CANCER    age_band   30s   -0.29    0.75
#> CANCER    age_band   40s   -0.68    0.51
#> ...
```

`exp_beta` 는 손해율의 곱셈 상대도다. 위(합성 데이터)에서는 20s 대비 30s 가
0.75 배로 달린다. **이것이 공변량의 1차 가치 — 계리적 해석이다.**

## 9.4 속성별 손해율 — predict(by=)

```python
fit.predict(by="age_band")
#> coverage cohort duration age_band loss_proj incr_loss_proj premium_proj ratio_proj source
```

**헤드라인 예측**(headline — 공변량으로 쪼개기 전의 대표 코호트 x 경과
예측, 곧 `predict()` 한 장)을 공변량별로 *분해*한 표면이다. 연령대별 예측
손해율을 따로 본다. 그리고 이 표면을 공변량에 대해 합치면 **헤드라인과
정확히 일치한다** — 분해일 뿐 다른 모델이 아니다(조각을 합치면 다시 한 판).

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
  - 데이터가 수축량을 정한다 = **신뢰도(credibility) = 경험적 베이즈
    (empirical Bayes)**. 레벨들이 정말 다르면 거의 안 수축(신호 유지),
    잡음 같으면 기준 쪽으로 강하게 수축. 레벨이 많고 희소한
    고cardinality 공변량(채널·연령대)에 권장.
* - `{컬럼: 값}`
  - 공변량별로 따로 지정(`"auto"` 또는 고정 값).
```

`lam_cov="auto"` 가 중요한 까닭. 레벨이 많고 희소한 공변량을 수축 없이
적합하면 데이터가 거의 없는 레벨에서 상대도가 분리(separation)로 폭발한다.
`"auto"` 는 그 공변량의 효과 분산을 데이터에서 추정해(레벨 간 변동이 작으면
강하게 수축) 폭발을 막으면서도 진짜 신호는 살린다.

```python
lr.CredibleLoss(covariates=["channel"], source=df, lam_cov="auto").fit(tri)
```

## 9.6 헤드라인은 (거의) 안 바뀐다

정직하게 짚을 점. 담보별로 쪼개 손해율만 예측한다면, **공변량은 헤드라인
예측을 거의 바꾸지 않는다.** 분해한 뒤 합치면 같은 값으로 돌아오기
때문이다(실데이터 검증: 공변량 유 vs 무의 헤드라인 차이 ~0.1%).

그러므로 공변량의 가치는 *헤드라인 정확도* 가 아니라 **`.coefficients`
(해석)와 `predict(by=)`(분해)** 에 있다. 헤드라인 손해율만 필요하면 공변량은
넣지 않아도 된다.

## 9.7 제약

- 공변량은 `borrow=` · `balance=` 와 동시에 쓸 수 없다(서로 배타적).
- 같은 컬럼을 `groups` 와 `covariates` 에 동시에 주면 에러다.
- 백테스트는 fold 마다 `source` 도 삼각형과 같은 기준으로 가려, 가려진
  기간이 공변량 적합에 새어 들지 않게 한다.

## 9.8 함께 보기

- {doc}`4장 — 손해율 예측 <04-projection>`: 공변량이 다듬는 그 예측.
- {doc}`공변량 레시피 <../cookbook/covariates>`: 분해 표면이 헤드라인과
  정확히 맞는 까닭을 한 단계 아래에서.
- {doc}`API 레퍼런스 <../api>` 의 `PooledLoss` · `CredibleLoss` ·
  `SmoothLoss` 의 `covariates` · `source` · `lam_cov`.
