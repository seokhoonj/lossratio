# 4장. 손해율 예측 — ED · CL · SA

1장의 직각삼각형은 오른쪽 아래가 비어 있었다. 2장은 그 빈 칸을 채울 한
가지 재료를 줬다 — 누적 손해를 몇 배로 키우는 ATA 인자(age-to-age factor,
곱셈). 3장은 또 하나를 줬다 — 보험료에 견준 강도(노출 기반, 덧셈). 이제 이
재료들로 **실제로 빈 칸을 채운다**.

이 장은 "그 칸이 *얼마가 될까*"(점 예측)만 다룬다. "그 값을 *얼마나 믿을
수 있나*"(표준오차·신뢰구간)는 5장에서 따로 본다 — 예측의 두 축은 서로
독립이기 때문이다.

## 4.1 기본 흐름 — 안전한 기본값에서 출발한다

빈 칸을 채우는 길은 셋이다. 셋은 대립하는 별개 방법이 아니라 *관측된 발전
패턴을 얼마나 믿느냐*의 스펙트럼 위에 놓인다.

- **노출 기반(exposure-driven, ED)** — 미래 손해를 *보험료라는 바깥 기준*에
  닻 내린다. 강도를 보험료에 곱한 증분을 더해 나간다. 성숙도를 따로 판단할
  필요가 없어 **무조건 안전한 기본값**이다. 그래서 method 기본값이 `"ed"`다.
- **체인래더(CL)** — 관측된 손해의 *진전 패턴*만으로 외삽한다. 누적 손해에
  ATA 인자를 곱해 나가는 고전적 대안(Mack 1993)이다. 충분히 발전한 구간에선
  정확하지만, 얇은 초기 관측에 곱셈을 거듭하면 출렁인다.
- **단계 적응형(stage-adaptive, SA)** — ED와 CL을 경과에 따라 **갈아타는**
  합성이다. 초기 구간은 ED로 안전하게, 발전이 신뢰할 만해진 뒤엔 CL로
  정확하게 — 전환 지점은 선택이다(아래 4.5).

```{mermaid}
flowchart TD
  Q{"어느 방법으로<br/>빈 칸을 채울까"}
  Q -->|"기본값 · 무조건 안전"| ED["ED (노출 기반)<br/>강도 x 보험료"]
  Q -->|"고전적 대안 · 발전이 신뢰할 만할 때"| CL["CL (체인래더)<br/>누적 손해 x ATA 인자"]
  Q -->|"둘의 합성"| SA["SA (단계 적응형)<br/>전환 전 ED + 전환 후 CL"]
  SA -.->|"전환 지점은 선택<br/>SwitchPoint"| SP["int (고정)<br/>SwitchPoint.detect() (백테스트 선택)<br/>None (전환 없음 = 순수 ED)"]
  style ED fill:#cfe8ff,stroke:#1f6fb2,stroke-width:2px
  style SA fill:#ffe08a,stroke:#d39e00,stroke-width:2px
```

method 이름은 `"ed"`(기본), `"cl"`, `"sa"` 순서로, **안전한 기본값 -> 고전
대안 -> 합성**의 학습 순서를 따른다.

```{list-table}
:header-rows: 1
:widths: 10 24 66

* - 방법
  - 한 줄 정의
  - 성격
* - ED
  - 강도 x 보험료. 노출에 닻을 내린 증분 예측
  - 안전한 기본값. 성숙도 판단 불필요, 초기 ATA 변동에 강건
* - CL
  - 누적 손해 x ATA 인자
  - 고전적 대안. 발전이 일관될 때 정확, 어린 코호트엔 위험
* - SA
  - 전환 전 ED + 전환 후 CL
  - 합성. 전환을 지정하지 않으면 순수 ED로 동작(안전한 기본값)
```

## 4.2 노출 앵커 — exposure-driven

빈 칸을 ED로 채워 본다. 강도는 3장에서 `g₁ = 0.055`, `g₂ = 0.030`이었다.
ED는 직전 손해에 곱하는 대신, **강도를 보험료에 곱한 증분**을 더한다(예제
보험료는 1,000 고정).

```{list-table}
:header-rows: 1
:widths: 14 20 26 26

* - 코호트
  - 경과 1
  - 경과 2
  - 경과 3
* - B
  - 120
  - 180
  - 180 + 0.030 x 1000 = **210**
* - C
  - 110
  - 110 + 0.055 x 1000 = **165**
  - 165 + 0.030 x 1000 = **195**
```

패키지로 확인한다. `fit` 진입점은 sklearn 스타일이다 — `lr.ExposureDriven()`
으로 모형을 만들고 `.fit(tri)`로 적합한다.

```python
import polars as pl
import lossratio as lr

toy = pl.DataFrame({
    "uy_m":    ["2024-01-01", "2024-01-01", "2024-01-01",
                "2024-02-01", "2024-02-01", "2024-03-01"],
    "cy_m":    ["2024-01-01", "2024-02-01", "2024-03-01",
                "2024-02-01", "2024-03-01", "2024-03-01"],
    "loss":    [100, 150, 180, 120, 180, 110],
    "premium": [1000, 1000, 1000, 1000, 1000, 1000],
}).with_columns(pl.col(["uy_m", "cy_m"]).str.to_date())

tri = lr.Triangle(toy, cohort="uy_m", calendar="cy_m",
                  loss="loss", premium="premium", cell_type="cumulative")

lr.ExposureDriven().fit(tri).df.select(["cohort", "dev", "loss_proj"]).sort(
    ["cohort", "dev"])
#> ┌────────────┬─────┬───────────┐
#> │ cohort     ┆ dev ┆ loss_proj │
#> ╞════════════╪═════╪═══════════╡
#> │ 2024-02-01 ┆ 3   ┆ 210.0     │   <- B 경과 3
#> │ 2024-03-01 ┆ 2   ┆ 165.0     │   <- C 경과 2
#> │ 2024-03-01 ┆ 3   ┆ 195.0     │   <- C 경과 3
#> └────────────┴─────┴───────────┘
```

## 4.3 발전 기반 — chain ladder

같은 빈 칸을 CL로 채워 본다. ATA 인자는 2장에서 `f₁ = 1.5`, `f₂ = 1.2`였다.
CL은 **직전 누적 손해에 인자를 곱하면** 채워진다.

```{list-table}
:header-rows: 1
:widths: 14 20 20 20

* - 코호트
  - 경과 1
  - 경과 2
  - 경과 3
* - B
  - 120
  - 180
  - 180 x 1.2 = **216**
* - C
  - 110
  - 110 x 1.5 = **165**
  - 165 x 1.2 = **198**
```

C는 두 번 곱한다 — 경과 1에서 2로 `x1.5`, 다시 2에서 3으로 `x1.2`. 곧 2장에서
본 누적 ATA 인자 곱(LDF) `1.5 x 1.2 = 1.8`을 경과 1의 110에 곱한 것과 같다
(`110 x 1.8 = 198`).

```python
lr.ChainLadder().fit(tri).df.select(["cohort", "dev", "loss_proj"]).sort(
    ["cohort", "dev"])
#> ┌────────────┬─────┬───────────┐
#> │ cohort     ┆ dev ┆ loss_proj │
#> ╞════════════╪═════╪═══════════╡
#> │ 2024-02-01 ┆ 3   ┆ 216.0     │   <- B 경과 3
#> │ 2024-03-01 ┆ 2   ┆ 165.0     │   <- C 경과 2
#> │ 2024-03-01 ┆ 3   ┆ 198.0     │   <- C 경과 3
#> └────────────┴─────┴───────────┘
```

CL과 ED를 나란히 두면 흥미로운 일이 보인다.

```{list-table}
:header-rows: 1
:widths: 24 16 16 16

* - 빈 칸
  - ED
  - CL
  - 일치?
* - C 경과 2
  - 165
  - 165
  - 일치
* - C 경과 3
  - 195
  - 198
  - 갈림
* - B 경과 3
  - 210
  - 216
  - 갈림
```

왜 어떤 칸은 같고 어떤 칸은 다를까? **B는 초기에 뜨겁게 달렸다** — 경과 2에서
B의 누적 손해(180)가 A(150)보다 크다. CL은 그 뜨거움을 미래로 *그대로 가져
간다*(증분 `0.2 x 180 = 36`). ED는 보험료라는 같은 기준에 닻을 내리므로
*식혀서* 본다(증분 `0.030 x 1000 = 30`). 반대로 C는 경과 1에서 포트폴리오
평균에 가까워, 두 방법이 같은 값을 낸다.

```{admonition} 덧셈식 닻 vs 곱셈식 모멘텀
:class: note

- **ED**의 증분은 *보험료(노출)*에 비례한다 — 코호트가 평균에서 벗어나도
  노출이 함의하는 수준으로 당겨진다(닻).
- **CL**의 증분은 코호트 *자신이 쌓은 누적 손해*에 비례한다 — 뜨겁게 달린
  코호트는 계속 뜨겁게 외삽된다(모멘텀).

분모를 무엇으로 잡았느냐(보험료냐, 직전 손해냐)가 예측의 성격을 가른다.
어느 하나가 옳은 게 아니라, 다음 절에서 볼 *반응성과 안정성의 맞교환*이다.
```

## 4.4 어린 코호트에서 갈린다

이 차이는 데이터가 적을수록 커진다. 내장 SUR 담보를 분기(Quarter) 단위로
집계해 보면, 충분히 발전한 오래된 코호트는 어느 방법으로 예측해도 최종
손해율이 거의 일치한다. 그러나 **갓 인수되어 경과 몇 분기치밖에 없는 어린
코호트**에서는 방법마다 크게 갈린다.

```{eval-rst}
.. plot::
   :context:
   :nofigs:
   :include-source: false

   import polars as pl
   import lossratio as lr
   import matplotlib.pyplot as plt

   df = lr.load_experience().filter(pl.col("coverage") == "SUR")
   tri = lr.Triangle(df, groups="coverage", grain="Q")
   COH = pl.lit("2025-01-01").str.to_date()

   _MODELS = {"ed": lr.ExposureDriven, "cl": lr.ChainLadder}

   def _traj(method):
       return (_MODELS[method]().fit(tri).df
               .filter(pl.col("cohort") == COH).sort("dev")
               .with_columns(
                   (pl.col("loss_obs") / pl.col("premium_obs")).alias("r_obs"),
                   (pl.col("loss_proj") / pl.col("premium_proj")).alias("r_proj")))

   ed, cl = _traj("ed"), _traj("cl")
   dev = ed["dev"].to_list()
   r_obs = ed["r_obs"].to_list()
   last = max(d for d, v in zip(dev, r_obs) if v is not None)

.. plot::
   :context: close-figs
   :caption: 어린 SUR 코호트(2025-1분기, 경과 1-4만 관측)의 누적 손해율 예측. 점선 오른쪽이 예측 구간이다. ED는 보험료에 닻을 내려 포트폴리오 수준(약 1.30)으로 올라가고, CL은 얇은 관측만으로 외삽해 약 0.88에 머문다.

   fig, ax = plt.subplots(figsize=(6.2, 3.6))
   ax.plot([d for d, v in zip(dev, r_obs) if v is not None],
           [v for v in r_obs if v is not None], "-o", color="black", ms=4,
           label="observed")
   ax.plot(dev, ed["r_proj"].to_list(), "--", color="#1f77b4", label="ED")
   ax.plot(dev, cl["r_proj"].to_list(), "--", color="#d62728", label="CL")
   ax.axvline(last + 0.5, color="gray", ls=":", lw=1)
   ax.set_xlabel("development quarter")
   ax.set_ylabel("cumulative loss ratio")
   ax.legend()
```

같은 관측(검은 점, 경과 1-4)에서 출발하지만 예측이 갈라진다. ED는 약 1.30,
CL은 약 0.88 — 한 코호트의 최종 손해율 추정이 방법에 따라 크게 차이 난다.
얇은 관측에 곱셈을 거듭하는 CL은 **반응성**이 높은 만큼 출렁이고(분산이
큼), 보험료에 닻을 내린 ED는 **안정적**인 대신 노출 패턴이 유지된다고
가정한다(편향 위험). 이것이 예측 방법 선택의 본질 — 반응성과 안정성의
맞교환이다.

## 4.5 둘을 갈아타기 — 단계 적응형(SA)

ED는 미성숙 구간에서 안정적이고, CL은 발전이 일관된 구간에서 손해의 진전을
직접 따라가 좋다. **단계 적응형(SA)**은 둘의 장점만 취한다 — 어떤 경과
지점을 경계로 **전환 전은 ED, 전환 후는 CL**로 예측한다.

세 방법의 한 칸 전진 규칙과 SA의 전환을 도식으로 보면 — ED는 덧셈형(외부
보험료=노출에 앵커), CL은 곱셈형(자기 누적 손해에 앵커), SA는 전환 지점 k
이전엔 ED, 이후엔 CL로 갈아탄다:

```text
  ED (additive):        C[k+1] = C[k] + g_k x P[k]  <- anchor: external premium
  CL (multiplicative):  C[k+1] = C[k] x f_k         <- anchor: own cumulative loss

  SA: ED before the switch dev k, CL at/after
       dev:  1    2    3   [k]    4    5    6
             |---- ED ----|  |---- CL ----|
              additive          multiplicative
```

`lr.StageAdaptive()`는 **전환을 지정하지 않으면 순수 ED와 동등**하다. 전환은
`switch=` 인자로 **선택적으로** 켠다 — 안전한 ED 기본선에 명확한 근거가
있을 때만 CL 구간을 들인다.

```python
lr.StageAdaptive()            # 전환 없음 = 순수 ED (안전한 기본값)
lr.StageAdaptive(switch=3)    # 경과 3에서 ED -> CL 전환 (고정)
```

```{eval-rst}
.. plot::
   :context: close-figs
   :caption: 같은 어린 SUR 코호트(2025-1분기)에 SA(switch=3)를 적용. 경과 3까지는 ED로 안전하게 채우고, 경과 3부터 CL로 갈아타 예측이 CL 쪽(약 0.88)으로 수렴한다. ED와 CL 사이의 절충이다.

   sa = (lr.StageAdaptive(switch=3).fit(tri).df
         .filter(pl.col("cohort") == COH).sort("dev")
         .with_columns((pl.col("loss_proj") / pl.col("premium_proj")).alias("r_proj")))

   fig, ax = plt.subplots(figsize=(6.2, 3.6))
   ax.plot([d for d, v in zip(dev, r_obs) if v is not None],
           [v for v in r_obs if v is not None], "-o", color="black", ms=4,
           label="observed")
   ax.plot(dev, ed["r_proj"].to_list(), "--", color="#1f77b4", alpha=0.4, label="ED")
   ax.plot(dev, cl["r_proj"].to_list(), "--", color="#d62728", alpha=0.4, label="CL")
   ax.plot(dev, sa["r_proj"].to_list(), "-", color="#2ca02c", lw=2, label="SA(switch=3)")
   ax.axvline(3 - 0.5, color="#2ca02c", ls=":", lw=1.2)
   ax.set_xlabel("development quarter")
   ax.set_ylabel("cumulative loss ratio")
   ax.legend()
```

## 4.6 전환 지점은 어디인가 — SwitchPoint

`switch=3`처럼 손으로 짚는 대신, 전환 지점을 데이터가 고르게 할 수 있다.
`lr.SwitchPoint`는 ED->CL 핸드오프를 **백테스트로 찾는다** — 여러 후보
경계마다 SA 모형을 만들어 hold-out 손해 예측 오차를 견주고, 그 오차를 가장
줄이는 경계를 고른다(7장의 검증 원리를 전환 지점 선택에 그대로 쓴다).

```python
df = lr.load_experience().filter(pl.col("coverage") == "SUR")
tri = lr.Triangle(df, groups="coverage", grain="Q")

sp = lr.SwitchPoint.detect()(tri)   # 백테스트로 전환 지점 선택
sp.summary()
#> ┌──────────┬───────┬──────────┐
#> │ coverage ┆ point ┆ status   │
#> ╞══════════╪═══════╪══════════╡
#> │ SUR      ┆ null  ┆ deferred │
#> └──────────┴───────┴──────────┘
```

```{admonition} SwitchPoint는 정직하게 보수적이다
:class: important

위 출력의 `point`가 `null`이고 `status`가 `deferred`인 데에 주목하자.
SwitchPoint는 **전환을 근거가 분명할 때만** 택한다 — hold-out 셀이 너무
적으면 자동 전환을 보류(`deferred`)하고 `None`(순수 ED, 안전한 기본선)으로
물러난다. 분기 단위 단일 담보처럼 데이터가 얇으면 흔히 이렇게 ED로 비킨다.

데이터가 두꺼워지면 실제 전환이 잡힌다. 같은 SUR을 **월 단위 전체
포트폴리오**로 보면 `point`가 잡힌다(아래). 즉 SwitchPoint의 자동 선택은
*데이터가 충분히 말해 줄 때만* CL을 들이고, 그렇지 않으면 ED로 비키는
보수적 게이트다.
```

```python
df_all = lr.load_experience()
tri_all = lr.Triangle(df_all, groups="coverage")   # 월 단위 전체
lr.SwitchPoint.detect()(tri_all).summary()
#> ┌──────────┬───────┬────────┐
#> │ coverage ┆ point ┆ status │
#> ╞══════════╪═══════╪════════╡
#> │ CAN      ┆ null  ┆ ed     │   <- 검증된 순수 ED
#> │ CI       ┆ null  ┆ ed     │
#> │ HOS      ┆ null  ┆ ed     │
#> │ SUR      ┆ 1     ┆ cl     │   <- 데이터가 두꺼워지자 CL이 잡힘
#> └──────────┴───────┴────────┘
```

`point`의 의미는 셋이다 — `None`(전환 없음, 순수 ED) / `1`(첫 링크에서
전환 = 사실상 순수 CL) / `k >= 2`(경과 `k` 전은 ED, `k`부터 CL). `status`는
같은 `None`이라도 검증된 ED(`"ed"`)와 데이터 부족으로 보류한 경우
(`"deferred"`)를 구분해 준다.

SA에 넘기는 방법은 셋이다. 백테스트 안에서 매 fold마다 다시 고르고 싶으면
누출 안전한 lazy 명세 `SwitchPoint.detect()`를 그대로 넘긴다(7장).

```python
lr.StageAdaptive(switch=lr.SwitchPoint.detect())   # 백테스트 선택 (lazy)
lr.StageAdaptive(switch=lr.SwitchPoint.at(3))       # 명시 전환
lr.StageAdaptive(switch=3)                          # int 도 허용 (= at(3))
```

적합 결과에는 코호트별 실효 전환 경계가 `switch_from` 열로 남고
(`None`이면 그 코호트는 순수 ED), `LossFit.switch_point` /
`RatioFit.switch_point` 속성으로 한눈에 볼 수 있다.

```python
sa = lr.StageAdaptive(switch=3).fit(tri)
sa.switch_point          #> {'SUR': 3}
sa.df["switch_from"]     #> 코호트별 전환 경계 (없으면 null = 순수 ED)
```

## 4.7 신뢰의 스펙트럼으로 정리

ED와 CL은 대립하는 두 방법이 아니라 **하나의 축 위 양 끝**이다 — *관측된
발전 패턴을 얼마나 믿느냐*의 스펙트럼이다.

- **ED** — 발전 대신 **노출(보험료)에 닻**을 내린다. 게다가 강도 `g_k`를
  *코호트 전체에서 풀링*해 추정하므로, 한 어린 코호트의 들쭉날쭉한 관측이
  아니라 포트폴리오 평균 노출 패턴이 예측을 끈다(안정적인 대신 노출 패턴
  유지를 가정).
- **CL** — 발전을 전적으로 믿는다. 코호트가 쌓은 누적 손해에 ATA 인자를
  곱해, 관측된 진전을 그대로 미래로 연장한다. 성숙한 코호트엔 정확하지만,
  얇은 초기 관측에 곱셈을 거듭하면 출렁인다.

이 "노출 앵커 + 코호트 풀링"이 핵심이다. 손해보험 reserving에서는 외부
prior 손해율을 손수 넣거나(Bornhuetter-Ferguson), 데이터에서 풀링해(Cape
Cod) 어린 코호트를 안정시킨다. 장기 건강보험에서는 **ED가 이미 그 역할을
한다** — 보험료라는 외부 기준에 닻을 내리고 강도를 코호트 간 풀링하므로,
별도의 prior 층을 얹지 않아도 어린 코호트가 노출 수준에 머문다.

SA는 이 스펙트럼 위에서 **갈아타는 길**이다. 발전을 아직 못 믿을 초기엔
ED로 안전하게, 발전이 코호트 간 일관되게 안정된 뒤엔 CL로 정확하게 — 그
전환 지점을 데이터로 고르는 것이 SwitchPoint이고, 데이터가 부족하면
SwitchPoint는 ED로 비켜 안전을 택한다.

## 4.8 어느 방법을 언제

```{list-table}
:header-rows: 1
:widths: 12 88

* - 방법
  - 어울리는 상황
* - ED
  - 초기 구간이 많거나 ATA 인자가 불안정할 때의 안전한 기본값. 의심스러우면 여기서 시작
* - CL
  - 충분히 발전해 진전 패턴이 코호트 간 일관될 때. 어린 코호트엔 위험
* - SA
  - 초기 ED 안정 + 후기 CL 진전을 한 번에. 전환 근거가 데이터로 분명할 때 (SwitchPoint)
```

기본값 `"ed"`로 시작해, 발전이 신뢰할 만하다는 근거가 백테스트로 확인되면
CL이나 SA로 갈아타는 것이 실무의 출발점이다. 어느 방법이 *내 데이터*에 맞는
지는 취향이 아니라 검증으로 가린다(7장).

## 4.9 함께 보기

- {doc}`2장 — 손해 진전 속도 <02-ata>`와 {doc}`3장 — 노출 기반의 직관
  <03-intensity>`: 이 장이 빈 칸을 채우는 데 쓴 ATA 인자와 강도.
- {doc}`5장 — 예측의 불확실성 <05-uncertainty>`: 이 장의 점 예측에 표준오차와
  신뢰구간을 입혀 "얼마나 믿을 수 있나"를 정량화한다.
- {doc}`7장 — 예측 검증 <07-backtest>`: SwitchPoint가 전환 지점을 고르는
  데 쓰는 백테스트를, 방법 선택 자체에도 그대로 쓴다.
- {doc}`API 레퍼런스 <../api>`의 `ExposureDriven`, `ChainLadder`,
  `StageAdaptive`, `SwitchPoint`, `Ratio`
