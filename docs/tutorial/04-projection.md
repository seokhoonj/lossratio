# 4장. 손해율 예측 — CL · ED · SA · BF · CC

1장의 직각삼각형은 오른쪽 아래가 비어 있었습니다. 2장은 그 빈 칸을 채울
한 가지 재료를 줬습니다 — 누적 손해를 몇 배로 키우는 **ATA 인자**(곱셈).
3장은 또 하나를 줬습니다 — 보험료에 견준 **강도**(노출 기반, 덧셈). 이제
이 재료들로 **실제로 빈 칸을 채웁니다**.

이 장은 "그 칸이 *얼마가 될까*"(점 예측)만 다룹니다. "그 값을 *얼마나 믿을
수 있나*"(표준오차·신뢰구간)는 5장에서 따로 봅니다 — 예측의 두 축은 서로
독립이기 때문입니다.

## 4.1 두 갈래 — 발전 기반과 노출 앵커

빈 칸을 채우는 길은 크게 둘입니다.

- **발전 기반**(development-based) — 관측된 손해의 *진전 패턴*만으로 미래를
  외삽합니다. 코호트가 지금까지 쌓은 손해에 ATA 인자를 곱해 나가는 **chain
  ladder**(CL)가 대표입니다.
- **노출 앵커**(exposure-anchored) — 미래 손해를 *보험료라는 바깥 기준*에
  닻 내립니다. 강도를 보험료에 곱하는 **노출 기반**(ED), 그리고 외부·내부
  정보를 섞는 **BF**·**CC**가 여기 속합니다.

그리고 이 둘을 경과에 따라 **갈아타는** 절충이 단계 적응형(SA)입니다.

```{list-table}
:header-rows: 1
:widths: 10 22 68

* - 방법
  - 갈래
  - 한 줄 정의
* - CL
  - 발전 기반
  - 누적 손해 x ATA 인자. 관측된 진전 패턴만 사용
* - ED
  - 노출 앵커
  - 강도 x 보험료. 노출에 닻을 내린 증분 예측
* - SA
  - 합성
  - 성숙점 전 ED + 성숙점 후 CL
* - BF
  - 노출 앵커
  - 미발현분을 외부 prior ELR x 보험료로 채움
* - CC
  - 노출 앵커
  - BF의 prior를 데이터 풀링으로 추정
```

method 이름은 `"ed"`(기본), `"cl"`, `"sa"`, `"bf"`, `"cc"` 순서로, **단순 ->
고전 -> 합성**의 학습 순서를 따릅니다.

## 4.2 발전 기반 — chain ladder

2장에서 손계산한 삼각형을 그대로 마무리합니다. ATA 인자는 `f₁ = 1.5`,
`f₂ = 1.2`였습니다. 빈 칸은 **직전 누적 손해에 인자를 곱하면** 채워집니다.

```{list-table}
:header-rows: 1
:widths: 14 20 20 20

* - 코호트
  - 경과 1
  - 경과 2
  - 경과 3
* - A (관측)
  - 100
  - 150
  - 180
* - B
  - 120
  - 180
  - 180 x 1.2 = **216**
* - C
  - 110
  - 110 x 1.5 = **165**
  - 165 x 1.2 = **198**
```

C는 두 번 곱합니다 — 경과 1에서 2로 `x1.5`, 다시 2에서 3으로 `x1.2`. 곧
2장에서 본 누적 ATA 인자 곱(LDF) `1.5 x 1.2 = 1.8`을 경과 1의 110에 곱한
것과 같습니다(`110 x 1.8 = 198`). 패키지로 확인합니다.

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

lr.Loss(method="cl").fit(tri).df.select(["cohort", "dev", "loss_proj"]).sort(
    ["cohort", "dev"])
#> ┌────────────┬─────┬───────────┐
#> │ cohort     ┆ dev ┆ loss_proj │
#> ╞════════════╪═════╪═══════════╡
#> │ 2024-02-01 ┆ 3   ┆ 216.0     │   <- B 경과 3
#> │ 2024-03-01 ┆ 2   ┆ 165.0     │   <- C 경과 2
#> │ 2024-03-01 ┆ 3   ┆ 198.0     │   <- C 경과 3
#> └────────────┴─────┴───────────┘
```

`fit` 진입점은 sklearn 스타일입니다 — `lr.Loss(method="cl")`로 방법을
설정하고 `.fit(tri)`로 적합합니다.

## 4.3 노출 앵커 — exposure-driven

같은 빈 칸을 ED로 채워 봅니다. 강도는 3장에서 `g₁ = 0.055`, `g₂ = 0.030`
이었습니다. ED는 직전 손해에 곱하는 대신, **강도를 보험료에 곱한 증분**을
더합니다(예제 보험료는 1,000 고정).

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

CL과 나란히 두면 흥미로운 일이 보입니다.

```{list-table}
:header-rows: 1
:widths: 24 16 16 16

* - 빈 칸
  - CL
  - ED
  - 일치?
* - C 경과 2
  - 165
  - 165
  - 일치
* - C 경과 3
  - 198
  - 195
  - 갈림
* - B 경과 3
  - 216
  - 210
  - 갈림
```

왜 어떤 칸은 같고 어떤 칸은 다를까요? **B는 초기에 뜨겁게 달렸습니다** —
경과 2에서 B의 누적 손해(180)가 A(150)보다 큽니다. CL은 그 뜨거움을 미래로
*그대로 가져갑니다*(증분 `0.2 x 180 = 36`). ED는 보험료라는 같은 기준에
닻을 내리므로 *식혀서* 봅니다(증분 `0.030 x 1000 = 30`). 반대로 C는 경과
1에서 포트폴리오 평균에 가까워, 두 방법이 같은 값을 냅니다.

```{admonition} 곱셈식 모멘텀 vs 덧셈식 닻
:class: note

- **CL**의 증분은 코호트 *자신이 쌓은 누적 손해*에 비례합니다 — 뜨겁게
  달린 코호트는 계속 뜨겁게 외삽됩니다(모멘텀).
- **ED**의 증분은 *보험료(노출)*에 비례합니다 — 코호트가 평균에서 벗어나도
  노출이 함의하는 수준으로 당겨집니다(닻).

분모를 무엇으로 잡았느냐(직전 손해냐, 보험료냐)가 예측의 성격을 가릅니다.
어느 하나가 옳은 게 아니라, 다음 절에서 볼 *반응성과 안정성의 맞교환*입니다.
```

## 4.4 어린 코호트에서 갈린다

이 차이는 데이터가 적을수록 커집니다. 내장 SUR 담보에서, 충분히 발전한
오래된 코호트는 어느 방법으로 예측해도 최종 손해율이 약 1.5로 거의
일치합니다. 그러나 **갓 인수되어 경과 몇 달치밖에 없는 어린 코호트**에서는
방법마다 크게 갈립니다.

```{eval-rst}
.. plot::
   :context:
   :nofigs:
   :include-source: false

   import polars as pl
   import lossratio as lr
   import matplotlib.pyplot as plt

   df = lr.load_experience().filter(pl.col("coverage") == "SUR")
   tri = lr.Triangle(df, groups="coverage")
   COH = pl.lit("2025-08-01").str.to_date()

   def _traj(method):
       return (lr.Loss(method=method).fit(tri).df
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
   :caption: 어린 SUR 코호트(2025-08, 경과 1-5만 관측)의 누적 손해율 예측. 점선 오른쪽이 예측 구간이다. ED는 보험료에 닻을 내려 포트폴리오 수준(약 1.4)으로 올라가고, CL은 얇은 관측만으로 외삽해 약 0.87에 머문다.

   fig, ax = plt.subplots(figsize=(6.2, 3.6))
   ax.plot([d for d, v in zip(dev, r_obs) if v is not None],
           [v for v in r_obs if v is not None], "-o", color="black", ms=3,
           label="observed")
   ax.plot(dev, ed["r_proj"].to_list(), "--", color="#1f77b4", label="ED")
   ax.plot(dev, cl["r_proj"].to_list(), "--", color="#d62728", label="CL")
   ax.axvline(last + 0.5, color="gray", ls=":", lw=1)
   ax.set_xlabel("development month")
   ax.set_ylabel("cumulative loss ratio")
   ax.legend()
```

같은 관측(검은 점, 경과 1-5)에서 출발하지만 예측이 갈라집니다. ED는 약
1.38, CL은 약 0.87 — 한 코호트의 최종 손해율 추정이 방법에 따라 1.5배
넘게 차이 납니다. 얇은 관측에 곱셈을 거듭하는 CL은 **반응성**이 높은 만큼
출렁이고(분산이 큼), 보험료에 닻을 내린 ED는 **안정적**인 대신 노출 패턴이
유지된다고 가정합니다(편향 위험). 이것이 예측 방법 선택의 본질 —
반응성과 안정성의 맞교환입니다.

## 4.5 둘을 갈아타기 — 단계 적응형(SA)

CL은 성숙한 구간에서 손해의 진전을 직접 따라가 좋고, ED는 미성숙 구간에서
안정적입니다. **단계 적응형**(stage-adaptive, SA)은 둘의 장점만 취합니다 —
3장의 성숙점을 경계로, **성숙점 이전은 ED, 이후는 CL**로 예측합니다.

```python
lr.Ratio(method="sa").fit(tri)   # 성숙점 전 ED + 후 CL
```

성숙점은 SA가 내부에서 자동으로 찾습니다(3장의 CV·RSE 기준). 다만 주의할
점 하나 — SUR처럼 성숙점이 이르게(경과 2) 잡히는 데이터에서는 ED 구간이
좁아 SA가 사실상 CL에 가깝게 동작합니다. SA의 진가는 **성숙점이 늦어 ED로
안전하게 메울 초기 구간이 넓을 때** 드러납니다.

## 4.6 외부 정보로 닻 내리기 — BF · CC

CL은 데이터만 믿고(어리면 출렁), ED는 노출만 믿습니다. 그 사이에서 **이미
발현된 부분은 관측을 쓰고, 아직 안 나온 부분만 외부 기준으로 채우는** 절충이
BF·CC입니다.

**Bornhuetter-Ferguson(BF)** — 분석자가 **외부 prior 손해율(ELR)**을 줍니다.
코호트가 경과상 `q`만큼 발현됐다면(누적 ATA 인자 곱으로 계산), 최종 손해는

$$
\text{ult} = \underbrace{L_{\text{관측}}}_{\text{발현분}} + \underbrace{(1 - q)\,\times\,\text{ELR}\,\times\,\text{보험료}}_{\text{미발현분}}
$$

로 둡니다. 어린 코호트는 `q`가 작아 prior가 지배하고, 성숙한 코호트는 `q`가
1에 가까워 관측이 지배합니다 — 자동으로 데이터가 쌓일수록 prior에서
관측으로 무게가 옮겨갑니다.

```python
lr.Ratio(method="bf", prior=1.5).fit(tri)   # ELR prior = 1.5
```

**Cape Cod(CC)** — BF와 똑같은 구조이되, prior ELR을 외부에서 받지 않고
**데이터 전체에서 풀링해 추정**합니다(`ELR = Σ관측손해 / Σ(보험료 x q)`).
prior를 손수 정하기 어려울 때, 포트폴리오 스스로 말하게 하는 방법입니다.

두 방법 모두 **노출 앵커 hybrid**입니다 — 발전 패턴(`q`, 몇 % 나왔나)은
CL에서 빌리고, 미발현분의 *크기*는 보험료 x ELR로 닻을 내립니다. 그래서
어린 코호트에서 CL처럼 무너지지 않고 ED처럼 노출 수준에 머뭅니다.

## 4.7 어느 방법을 언제

```{list-table}
:header-rows: 1
:widths: 12 88

* - 방법
  - 어울리는 상황
* - CL
  - 충분히 성숙해 진전 패턴이 코호트 간 일관될 때. 어린 코호트엔 위험
* - ED
  - 초기 구간이 많거나 ATA 인자가 불안정할 때의 안전한 기본값
* - SA
  - 성숙점이 뚜렷하고 미성숙 구간이 넓을 때 — ED 안정 + CL 진전을 한 번에
* - BF
  - 신뢰할 외부 손해율 기준(요율 가정 등)이 있을 때
* - CC
  - 외부 기준이 없어 포트폴리오에서 ELR을 끌어내고 싶을 때
```

기본값 `"ed"`로 시작해 데이터의 성숙도와 가진 정보에 따라 갈아타는 것이
실무의 출발점입니다.

## 4.8 함께 보기

- {doc}`2장 — 손해 진전 속도 <02-ata>`와 {doc}`3장 — 노출 기반의 직관
  <03-intensity>`: 이 장이 빈 칸을 채우는 데 쓴 ATA 인자와 강도·성숙점.
- {doc}`5장 — 예측의 불확실성 <05-uncertainty>`: 이 장의 점 예측에 표준오차와
  신뢰구간을 입혀 "얼마나 믿을 수 있나"를 정량화합니다.
- {doc}`API 레퍼런스 <../api>`의 `Loss`, `Ratio`, `CL`, `ED`, `BF`, `CC`
