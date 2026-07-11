# 6장. 셀 수준 공변량 — covariates

앞 장들은 손해율을 **코호트 x 경과 기간** 한 장으로 예측했다. 그런데 한
코호트 안에는 성별·연령대·채널처럼 셀을 더 잘게 가르는 속성이 있다. 이
장은 그런 속성을 **공변량(covariate)** 으로 모델에 넣어 (1) 그 속성이
손해율 수준을 얼마나 올리고 내리는지 *해석*하고, (2) 속성별 손해율을
*나눠서* 보는 법을 다룬다.

## 6.1 쪼갤 것인가, 회귀할 것인가 — groups vs covariate

먼저 삼각형을 그 속성까지 잘게 묶어 만든다. 그다음 그 속성을 두 가지로
다룰 수 있다.

- **그대로 group 으로 둔다(partition).** 속성 값마다 독립된 삼각형을 따로
  적합한다. 각 조각은 서로를 보지 않는다 — 조각의 관측 셀이 적으면 불안정하다.
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
쪼개면 조각이 작아져 불안정하다. 공변량은 **경과 형상을 모든 셀이 공유** 하고
**수준 차이만** 속성별로 두므로, 관측 셀이 적은 조각도 공유 형상을 빌려 버틴다.

## 6.2 어떻게 넣나 — groups 에 묶고 covariates 로 회귀

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

## 6.3 상대도 읽기 — .coefficients

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

`beta` 는 로그 척도라 그대로는 읽기 어렵다. `exp_beta` 가 그것을 손해율의
곱셈 상대도로 되돌린 값이다 — 한 줄씩 지수를 취하면 된다:

```text
exp(-0.29) = 0.75      30s 손해율은 20s 의 0.75 배
exp(-0.68) = 0.51      40s 는 0.51 배
exp(-0.98) = 0.37      50s 는 0.37 배
exp(-1.32) = 0.27      60s 는 0.27 배
exp(-1.61) = 0.20      70+ 는 0.20 배
```

(`beta` = 0 인 기준 레벨 20s 는 `exp(0)` = 1.00.) 위(합성 데이터)에서는
나이가 높을수록 암 담보 손해율이 단조롭게 낮아진다. **이것이 공변량의 1차
가치 — 계리적 해석이다.**

## 6.4 속성별 손해율 — predict(by=)

2장의 `predict()` 는 공변량으로 쪼개지 않은 **전체 예측** — 코호트 x 경과 한
장 — 을 준다. 공변량을 넣었으면 `predict(by="age_band")` 로 그 전체 예측을
속성별로 *나눠* 볼 수 있다. 손해율은 합성값이라 loss 모델을 `LossRatio` 로 감싸
분모(보험료)를 붙여야 나온다 — `LossFit` 자체는 `loss_proj` 만 낸다.

```python
rf = lr.LossRatio(loss=lr.CredibleLoss(covariates=["age_band"]),
              premium=lr.PooledPremium()).fit(tri)

rf.predict()               # 전체 예측: 코호트 x 경과 (coverage 단위)
rf.predict(by="age_band")  # 속성별: 코호트 x 경과 x 연령대
#> coverage cohort     duration age_band loss_proj   ratio_proj source
#> CANCER   2023-01-01 1        20s        2481.55   0.285      observed
#> CANCER   2023-01-01 1        30s        2311.91   0.212      observed
#> CANCER   2023-01-01 1        40s        1609.85   0.144      observed
#> CANCER   2023-01-01 1        50s         833.35   0.106      observed
#> CANCER   2023-01-01 1        60s         492.84   0.076      observed
#> CANCER   2023-01-01 1        70+         188.49   0.057      observed
```

각 (코호트, 경과)의 전체 예측을 연령대 셀로 나눠 담은 표다 — 연령대별
예측 손해율을 따로 본다(위에서 `ratio_proj` 가 6.3 의 상대도 순서를 그대로
따른다). 이 표를 다시 연령대에 대해 합치면 **전체 예측과 정확히 같아진다**:
나눠 본 것일 뿐 다른 모델이 아니다(한 판을 조각냈다 다시 합치면 같은 한 판).

"조각냈다 합치면 같다"를 위 셀에서 직접 확인할 수 있다. 6개 연령대
`loss_proj` 를 더하면 전체 예측의 한 셀 값과 셀 단위로 정확히 일치한다:

```text
2481.55 + 2311.91 + 1609.85 + 833.35 + 492.84 + 188.49
  = 7918.00     # = predict() 의 CANCER 2023-01-01 경과 1 loss_proj
```

```python
import polars as pl
from datetime import date

full = rf.predict()
by   = rf.predict(by="age_band")
cell = dict(coverage="CANCER", cohort=date(2023, 1, 1), duration=1)
# CANCER 2023-01-01, 경과 1 한 셀
full.filter(**{k: pl.lit(v) for k, v in cell.items()})["loss_proj"][0]
#> 7918.0
round(by.filter(**{k: pl.lit(v) for k, v in cell.items()})["loss_proj"].sum(), 2)
#> 7918.0     # 부동소수점 잔차(~1e-9)만 빼면 셀 단위로 일치
```

같은 항등식이 좌측 절단 코호트·미관측 셀·경과에 따라 변하는 보험료 믹스가
있어도 셀마다 성립한다. 한 단계 아래의 배분 산식은 {doc}`공변량 레시피
<../cookbook/covariates>` 에서 손계산으로 따라간다.

## 6.5 수축 — lam_cov

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
`"auto"` 는 그 강도를 **신뢰도(credibility)** 로 데이터에서 추정, 숫자는
사용자가 직접 고정. `"auto"` 의 수축은 Bühlmann 신뢰도와 같은 발상이다 —
레벨별 데이터가 많으면(신뢰도 높음) 거의 안 당기고, 적으면(신뢰도 낮음)
기준 쪽으로 강하게 당긴다. 구현은 Schall(1991)의 분산성분 추정을 차용한다
(empirical Bayes 계열).

`lam_cov="auto"` 가 중요한 까닭: 레벨이 많고 데이터가 드문 공변량을 수축
없이 적합하면, 데이터가 거의 없는 레벨에서 상대도가 분리(separation -- 그
레벨만 손해/무손해로 갈려 추정이 폭발)로 부풀어 오른다. `"auto"` 는 그
공변량의 효과 분산을 데이터에서 추정해(레벨 간 변동이 작으면 강하게 수축)
부풀음을 막으면서도 진짜 신호는 살린다.

레벨이 4개인 채널(channel)을 *관측 셀이 적은 조각*(CI 담보, 최근 코호트
4개)에 넣어 두 모드를 비교하면 차이가 드러난다:

```python
df = lr.load_experience()
cohs = sorted(df["uy_m"].unique().to_list())
thin = df.filter((pl.col("coverage") == "CI") & pl.col("uy_m").is_in(cohs[-4:]))
tri = lr.Triangle(thin, groups=["coverage", "channel"])

lr.CredibleLoss(covariates=["channel"], lam_cov=0.0).fit(tri).coefficients
#> coverage  level  beta     exp_beta
#> CI        FC     0.000    1.00     <- 기준
#> CI        GA     0.067    1.07
#> CI        ON     0.714    2.04     <- 관측 셀이 적은 곳에서 부풀어 오름
#> CI        TM    -0.180    0.84

lr.CredibleLoss(covariates=["channel"], lam_cov="auto").fit(tri).coefficients
#> coverage  level  beta     exp_beta
#> CI        FC     0.000    1.00
#> CI        GA    -0.003    1.00
#> CI        ON     0.359    1.43     <- 기준 쪽으로 당겨짐
#> CI        TM    -0.141    0.87
```

`lam_cov=0` 에서는 ON 채널이 손해율 2.04 배로 솟지만(표본이 적은 셀의 잡음을 그대로
믿음), `"auto"` 는 그 효과를 1.43 배로 끌어내린다 — 신뢰도가 낮은 레벨일수록
기준으로 더 많이 수축한다. 차이가 진짜 신호면 덜 당기고, 잡음이면 강하게
당기는 것이 신뢰도 수축이다.

`lam_cov` 은 공변량별로 따로 줄 수도 있다(dict 형). 단 한 dict 안에서
`"auto"` 와 고정 숫자를 섞을 수는 없다 — `"auto"` 면 그 공변량 블록의 효과
분산을 함께 추정하므로 전부 `"auto"` 이거나 전부 숫자여야 한다.

```python
tri = lr.Triangle(df, groups=["coverage", "age_band", "channel"])
# 연령대는 고정효과, 채널은 신뢰도 수축
lr.CredibleLoss(covariates=["age_band", "channel"],
                lam_cov={"age_band": "auto", "channel": "auto"}).fit(tri)
# 또는 공변량별 고정 ridge
lr.CredibleLoss(covariates=["age_band", "channel"],
                lam_cov={"age_band": 0.0, "channel": 5.0}).fit(tri)
```

공변량을 둘 이상 넣었으면 `predict(by=[...])` 로 한꺼번에 쪼갠다 — 각
(코호트, 경과) 셀이 연령대 x 채널 격자로 갈리고, 합치면 여전히 전체 예측과
정확히 일치한다:

```python
rf = lr.LossRatio(loss=lr.CredibleLoss(covariates=["age_band", "channel"]),
              premium=lr.PooledPremium()).fit(tri)
rf.predict(by=["age_band", "channel"])
#> coverage cohort     duration age_band channel loss_proj    ratio_proj
#> CANCER   2023-01-01 1        20s      FC        923.86     0.252
#> CANCER   2023-01-01 1        20s      GA        779.52     0.316
#> CANCER   2023-01-01 1        20s      ON        223.65     0.257
#> CANCER   2023-01-01 1        20s      TM        546.83     0.317
#> ...                                                          (24행 = 6 x 4)
# 한 셀의 24행을 더하면 -> 7918.0 (= predict() 의 같은 셀)
```

## 6.6 전체 예측은 (거의) 안 바뀐다

정직하게 짚을 점. 담보별로 쪼개 손해율만 예측한다면, **공변량은 전체 예측을
거의 바꾸지 않는다.** 속성별로 나눈 뒤 합치면(6.4) 같은 값으로 돌아오기 때문이다
(실데이터 검증: 공변량 유 vs 무의 전체 예측 차이 ~0.1%).

그러므로 공변량의 가치는 *전체 예측의 정확도* 가 아니라 **`.coefficients`
(해석, 6.3)와 `predict(by=)`(속성별 예측, 6.4)** 에 있다. 전체 손해율만 필요하면
공변량은 넣지 않아도 된다.

## 6.7 합계 보정 — balance

`balance=True` 는 별개의 보정 스위치다(공변량과 무관하게 모든 손해 추정기에
있다). 적합한 인자로 다시 깐 *관측 구간* 증분 합이 실제 관측 증분 합과
어긋날 때, 세그먼트마다 비례 상수 `alpha` 하나로 미래 예측을 다시 맞춘다
(Ohlsson 2008 의 balance property). 보정 상수는 `fit.balance_factor` 로 본다.

```python
df  = lr.load_experience()
tri = lr.Triangle(df, groups="coverage")

lr.PooledLoss(balance=True).fit(tri).balance_factor
#> coverage   alpha
#> CANCER     1.000     <- 완전 풀링은 경과 단계마다 이미 합이 맞음(구조적 1.0)
#> CI         1.000
#> INPATIENT  1.000
#> SURGERY    1.000

lr.CredibleLoss(balance=True).fit(tri).balance_factor
#> coverage   alpha
#> CANCER     1.0028    <- 신뢰도 재가중이 합계를 약간 흔들어 보정 발생
#> CI         1.000
#> INPATIENT  1.000
#> SURGERY    1.0265
```

`PooledLoss` 의 완전 풀링 강도는 경과 단계마다 이미 합이 맞아 `alpha` 가 모두
1.0(구조적 무동작)이고, 신뢰도·평활은 코호트별 재가중이 합계를 흔들어 `alpha`
가 1에서 벗어난다.

## 6.8 제약

- 공변량 컬럼은 **반드시 삼각형 `groups` 의 부분집합** 이다 (group 이 아닌
  컬럼은 적합이 읽을 셀이 없어 에러).
- 공변량은 `balance=` 와 동시에 쓸 수 없다(서로 배타적) — 함께 주면
  `NotImplementedError` 로 멈춘다. 둘 다 미래 예측의 수준을 옮기는 보정이라
  적용 순서가 모호해 아직 결합 경로를 두지 않았다.
- 진단·예측의 grain 도 공변량을 따라간다 — 백테스트는 공변량 적합의 보고
  단위(`groups - covariates`)에서 채점하고, 가려진(held-out) 셀은 삼각형
  마스킹을 통해 공변량 적합에도 그대로 가려져 새어 들지 않는다.

## 6.9 함께 보기

- {doc}`2장 — 손해율 예측 <02-projection>`: 공변량이 다듬는 그 예측.
- {doc}`공변량 레시피 <../cookbook/covariates>`: 속성별 예측이 전체 예측과
  정확히 맞는 까닭을 한 단계 아래에서.
- {doc}`API 레퍼런스 <../api>` 의 `PooledLoss` · `CredibleLoss` ·
  `SmoothLoss` 의 `covariates` · `lam_cov`.
```
