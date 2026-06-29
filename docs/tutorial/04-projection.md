# 4장. 손해율 예측 — Pooled · Credible · Smooth

1장의 직각삼각형은 오른쪽 아래가 비어 있었다. 2장은 그 빈 칸을 채울 한
가지 재료를 줬다 — 한 시점을 경과할 때 누적 손해가 몇 배가 되는지를
나타내는 ATA 인자(age-to-age factor, 곱셈). 3장은 또 하나를 줬다 — 보험료
대비 강도(완전 풀링, 덧셈). 이제 이 재료들로 **실제로 빈 칸을 채운다**.

이 장은 "그 칸이 *얼마가 될까*"(점추정)만 다룬다. "그 값을 *얼마나 믿을
수 있나*"(표준오차·신뢰구간)는 5장에서 따로 본다 — 예측의 두 축은 서로
독립이기 때문이다.

## 4.1 기본 흐름 — 안전한 기본값에서 출발한다

이 장의 주인공은 강도 사다리의 세 단이다 — 베이스라인 **PooledLoss**, 그 위에
코호트별 수준을 보정하는 **CredibleLoss**, 그리고 풀링 형상을 매끄럽게 다듬는
**SmoothLoss**. 거기에 고전적 참조점으로 **ChainLadder**가 함께 등장한다.

- **PooledLoss(완전 풀링)** — 미래 손해를 *위험보험료*에
  고정한다. 강도를 보험료에 곱한 증분을 더해 나간다. 별도의 사전 판단
  없이 어디서나 작동하는 **무조건 안전한 기본값**이라, 손해쪽 추정기의
  기본 기준선으로 채택했다.
- **CredibleLoss(부분 풀링, 코호트 수준 보정)** — PooledLoss의 풀링된
  형상은 그대로 두고, **각 코호트의 수준만** 신뢰도 가중으로 보정한다.
  포트폴리오 평균과 다르게 달리는 코호트의 수준 차이를, 그 코호트의
  관측이 쌓인 만큼만 반영한다(아래 4.5).
- **SmoothLoss(형상 평활)** — CredibleLoss가 코호트의 *수준*을 보정한다면,
  SmoothLoss는 풀링 *형상*을 다듬는다. PooledLoss의 강도 `g_k`를 P-spline으로
  매끄럽게 한 형상을 쓴다. 경과별 강도가 들쭉날쭉한 세그먼트(얇은 late-tail·
  thin)에서 값이 나고, 강도가 이미 매끄러운 큰 담보에선 PooledLoss와
  사실상 같다.
- **ChainLadder(자기손해 링크비)** — 관측된 손해의 *진전 패턴*만으로 외삽한다.
  누적 손해에 ATA 인자를 곱해 나가는 곱셈 link ratio 계열의 **고전적
  참조점**이다. 진전이 일관된 구간에선 정확할 수 있지만, 얇은 초기 관측에
  곱셈을 거듭하면 출렁인다.

```{mermaid}
flowchart TD
  Q{"어느 방법으로<br/>빈 칸을 채울까"}
  Q -->|"기본값 · 무조건 안전"| pooled["PooledLoss (완전 풀링)<br/>강도 x 보험료"]
  pooled -->|"+ 코호트 수준 보정"| credible["CredibleLoss (부분 풀링)<br/>코호트 스케일 x 강도 x 보험료"]
  pooled -->|"+ 형상 평활"| smooth["SmoothLoss (형상 평활)<br/>평활 강도 x 보험료"]
  Q -->|"고전적 참조점 · 곱셈"| linkratio["ChainLadder (자기손해 링크비)<br/>누적 손해 x ATA 인자"]
  style Q fill:#f3f4f6,stroke:#9aa0a6,stroke-width:2px,color:#1f2937
  style pooled fill:#cfe8ff,stroke:#1f6fb2,stroke-width:2px,color:#0d3b66
  style credible fill:#ffe08a,stroke:#d39e00,stroke-width:2px,color:#5a4500
  style smooth fill:#d5f0d5,stroke:#3a9b5c,stroke-width:2px,color:#1c3a2e
  style linkratio fill:#ececec,stroke:#777777,stroke-width:2px,color:#2a2a2a
```

```{list-table}
:header-rows: 1
:widths: 12 28 60

* - 방법
  - 한 줄 정의
  - 성격
* - PooledLoss
  - 강도 x 보험료. 노출에 고정한 증분 예측
  - 안전한 기본값. 사전 판단 불필요, 초기 ATA 변동에 강건
* - CredibleLoss
  - 코호트 스케일 x 강도 x 보험료
  - PooledLoss의 확장. 집계 수준의 체계적 편향을 줄이는 보정 — 셀 단위 정확도는 대체로 PooledLoss
* - SmoothLoss
  - P-spline으로 평활한 강도 x 보험료
  - PooledLoss의 형상 평활 확장. 거친 강도 형상에서 값이 남, 매끄러운 담보에선 PooledLoss와 사실상 동일
* - ChainLadder
  - 누적 손해 x ATA 인자
  - 고전적 참조점. 업계 관행과의 소통·검증, 방법 비교의 비교 상대. 최근 코호트엔 위험
```

보험료 쪽에도 같은 3단 사다리(`PooledPremium` / `CrediblePremium` /
`SmoothPremium`)가 대칭으로 있다. 다만 위험보험료는 *알려진 노출*이라 점추정만
내고 표준오차는 내지 않는다(5장 5.4에서 다룬다).

## 4.2 완전 풀링 — PooledLoss

빈 칸을 PooledLoss로 채워 본다. 강도는 3장에서 `g₁ = 0.055`, `g₂ = 0.015`였다.
PooledLoss는 직전 손해에 곱하는 대신, **강도를 누적 보험료에 곱한 증분**을
더한다(예제 누적 보험료는 1,000, 2,000, 3,000으로 늘어난다).

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
  - 180 + 0.015 x 2000 = **210**
* - C
  - 110
  - 110 + 0.055 x 1000 = **165**
  - 165 + 0.015 x 2000 = **195**
```

패키지로 확인한다. `fit` 진입점은 sklearn 스타일이다 — `lr.PooledLoss()`
로 모형을 만들고 `.fit(tri)`로 적합한다.

```python
import polars as pl
import lossratio as lr

toy = pl.DataFrame({
    "uy_m":    ["2024-01-01", "2024-01-01", "2024-01-01",
                "2024-02-01", "2024-02-01", "2024-03-01"],
    "cy_m":    ["2024-01-01", "2024-02-01", "2024-03-01",
                "2024-02-01", "2024-03-01", "2024-03-01"],
    "loss":    [100, 150, 180, 120, 180, 110],
    "premium": [1000, 2000, 3000, 1000, 2000, 1000],
}).with_columns(pl.col(["uy_m", "cy_m"]).str.to_date())

tri = lr.Triangle(toy, cohort="uy_m", calendar="cy_m",
                  loss="loss", premium="premium", cell_type="cumulative")

lr.PooledLoss().fit(tri).df.select(["cohort", "duration", "loss_proj"]).sort(
    ["cohort", "duration"])
#> ┌────────────┬──────────┬───────────┐
#> │ cohort     ┆ duration ┆ loss_proj │
#> ╞════════════╪══════════╪═══════════╡
#> │ 2024-02-01 ┆ 3        ┆ 210.0     │   <- B 경과 3
#> │ 2024-03-01 ┆ 2        ┆ 165.0     │   <- C 경과 2
#> │ 2024-03-01 ┆ 3        ┆ 195.0     │   <- C 경과 3
#> └────────────┴──────────┴───────────┘
```

## 4.3 자기손해 링크비 — ChainLadder

같은 빈 칸을 ChainLadder로 채워 본다. ATA 인자는 2장에서 `f₁ = 1.5`, `f₂ = 1.2`였다.
ChainLadder는 **직전 누적 손해에 인자를 곱하면** 채워진다.

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
lr.ChainLadder().fit(tri).df.select(["cohort", "duration", "loss_proj"]).sort(
    ["cohort", "duration"])
#> ┌────────────┬──────────┬───────────┐
#> │ cohort     ┆ duration ┆ loss_proj │
#> ╞════════════╪══════════╪═══════════╡
#> │ 2024-02-01 ┆ 3        ┆ 216.0     │   <- B 경과 3
#> │ 2024-03-01 ┆ 2        ┆ 165.0     │   <- C 경과 2
#> │ 2024-03-01 ┆ 3        ┆ 198.0     │   <- C 경과 3
#> └────────────┴──────────┴───────────┘
```

ChainLadder와 PooledLoss를 나란히 두면 흥미로운 일이 보인다.

```{list-table}
:header-rows: 1
:widths: 24 16 16 16

* - 빈 칸
  - PooledLoss
  - ChainLadder
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

왜 어떤 칸은 같고 어떤 칸은 다를까? **B는 초기 손해가 평균보다 높았다** —
경과 2에서 B의 누적 손해(180)가 A(150)보다 크다. ChainLadder는 그 높은 수준을
미래로 *그대로 끌고 간다*(증분 `0.2 x 180 = 36`). 반면 PooledLoss는 보험료라는
같은 기준에 고정해 *포트폴리오 평균 쪽으로 당겨* 본다(증분 `0.015 x 2000 =
30`). 반대로 C는 경과 1에서 포트폴리오 평균에 가까워, 두 방법이 같은 값을
낸다.

```{admonition} 덧셈식 고정 vs 곱셈식 모멘텀
:class: note

- **PooledLoss**의 증분은 *보험료(노출)*에 비례한다 — 코호트가 평균에서
  벗어나도 노출이 함의하는 수준으로 당겨진다(고정).
- **ChainLadder**의 증분은 코호트 *자신이 쌓은 누적 손해*에 비례한다 — 초기
  손해가 높았던 코호트는 계속 높게 외삽된다(모멘텀).

분모를 무엇으로 잡았느냐(보험료냐, 직전 손해냐)가 예측의 성격을 가른다.
어느 하나가 옳은 게 아니라, 다음 절에서 볼 *반응성과 안정성의 맞교환*이다.
```

ChainLadder는 손해보험 reserving의 표준 어휘라, 업계 관행과 결과를 **소통하고
검증하는 참조점**으로서의 가치가 크다. 다만 장기 건강보험 실데이터의
표본 외(out-of-sample) 비교에서는 완전 풀링 베이스라인이 내다보는 거리
전반에서 더 잘 맞았다 — ChainLadder가 *내 데이터*에서 실제로 보탬이 되는지는
가정하지 말고 7장의 방법 비교(`lr.EstimatorComparison`)로 확인한다.

## 4.4 최근 코호트에서 갈린다

이 차이는 데이터가 적을수록 커진다. 내장 수술담보를 분기(Quarter) 단위로
집계해 보면, 충분히 경과한 코호트는 어느 방법으로 채워도 예측
손해율이 거의 일치한다. 그러나 **갓 인수되어 경과 몇 분기치밖에 없는 최근
코호트**에서는 방법마다 크게 갈린다.

```{eval-rst}
.. plot::
   :context:
   :nofigs:
   :include-source: false

   import polars as pl
   import lossratio as lr
   import matplotlib.pyplot as plt

   df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
   tri = lr.Triangle(df, groups="coverage", grain="Q")
   COH = pl.lit("2025-01-01").str.to_date()

   _MODELS = {"pooled": lr.PooledLoss, "linkratio": lr.ChainLadder}

   def _traj(method):
       return (_MODELS[method]().fit(tri).df
               .filter(pl.col("cohort") == COH).sort("duration"))

   pooled, linkratio = _traj("pooled"), _traj("linkratio")
   duration = pooled["duration"].to_list()
   src = pooled["source"].to_list()
   ratio_all = pooled["ratio_proj"].to_list()
   # 관측된 손해율 = source == "observed" 인 셀의 ratio_proj
   obs_pairs = [(d, r) for d, r, s in zip(duration, ratio_all, src) if s == "observed"]
   last = max(d for d, r in obs_pairs)

.. plot::
   :context: close-figs
   :caption: 최근 수술담보 코호트(2025-1분기, 경과 1-4만 관측)의 누적 손해율 예측. 점선 오른쪽이 예측 구간이다. PooledLoss는 보험료에 고정해 포트폴리오 수준(약 1.30)으로 올라가고, ChainLadder는 얇은 관측만으로 외삽해 약 0.88에 머문다.

   fig, ax = plt.subplots(figsize=(6.2, 3.6))
   ax.plot([d for d, r in obs_pairs], [r for d, r in obs_pairs],
           "-o", color="black", ms=4, label="observed")
   ax.plot(duration, pooled["ratio_proj"].to_list(), "--", color="#1f77b4",
           label="PooledLoss")
   ax.plot(duration, linkratio["ratio_proj"].to_list(), "--", color="#d62728",
           label="ChainLadder")
   ax.axvline(last + 0.5, color="gray", ls=":", lw=1)
   ax.set_xlabel("duration (quarters)")
   ax.set_ylabel("cumulative loss ratio")
   ax.legend()
```

같은 관측(검은 점, 경과 1-4)에서 출발하지만 예측이 갈라진다. PooledLoss는
약 1.30, ChainLadder는 약 0.88 — 한 코호트의 예측 손해율 추정이 방법에 따라
크게 차이 난다. 얇은 관측에 곱셈을 거듭하는 ChainLadder는 **반응성**이 높은
만큼 출렁이고(분산이 큼), 보험료에 고정한 PooledLoss는 **안정적**인 대신
노출 패턴이 유지된다고 가정한다(편향 위험). 이것이 예측 방법 선택의 본질 —
반응성과 안정성의 맞교환이다.

이 갈림은 4.2-4.3의 장난감 삼각형이 보여 준 두 증분 규칙을 실제
수술담보 코호트에 그대로 옮겨 손으로 따라갈 수 있다. 이 코호트가 마지막으로
*관측된* 칸은 경과 4다 — 누적 손해 582,440,300, 누적 보험료 695,383,078, 곧
관측 손해율 0.838. 여기서 경과 5로 가는 첫 예측 증분만 두 방법으로 계산하면:

- **PooledLoss(덧셈식 `g x P`)** — 경과 4->5 강도 `g = 0.382`(= 풀링 증분 /
  직전 누적 보험료)를 *누적 보험료*에 곱한다: `0.382 x 695,383,078 ≈
  265,537,874`. 누적 손해는 847,978,174, 손해율은 0.958로 *오른다*.
- **ChainLadder(곱셈식 `f x C`)** — 경과 4->5 ATA 인자 `f = 1.293`의 증가분
  `(f - 1) = 0.293`을 *직전 누적 손해*에 곱한다: `0.293 x 582,440,300 ≈
  170,364,123`. 누적 손해는 752,804,423, 손해율은 0.850으로 거의 제자리다.

같은 관측 칸에서 출발했는데 첫 한 걸음부터 갈린다 — 덧셈식은 *계속 자라는
보험료*에 강도를 곱해 손해율을 끌어올리고, 곱셈식은 *얇게 쌓인 자기 손해*에만
비례해 거의 머문다. 이 한 걸음을 경과 12까지 반복하면 PooledLoss는 1.30,
ChainLadder는 0.88에 닿는다 — 위 그림의 두 점선이다.

```python
import polars as pl
import lossratio as lr

df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df, groups="coverage", grain="Q")
COH = pl.lit("2025-01-01").str.to_date()

pooled = (lr.PooledLoss().fit(tri).df
          .filter(pl.col("cohort") == COH).sort("duration"))
link = (lr.ChainLadder().fit(tri).df
        .filter(pl.col("cohort") == COH).sort("duration"))

C4 = link.filter(pl.col("duration") == 4)["loss_proj"][0]       # 마지막 관측 누적 손해
P4 = pooled.filter(pl.col("duration") == 4)["premium_proj"][0]  # 마지막 관측 누적 보험료
g = pooled.filter(pl.col("duration") == 5)["incr_loss_proj"][0] / P4   # 경과 4->5 강도
f = link.filter(pl.col("duration") == 5)["loss_proj"][0] / C4          # 경과 4->5 ATA 인자

print(f"g x P4  (덧셈식 첫 증분)   = {g * P4:>13,.0f}")        #> g x P4  (덧셈식 첫 증분)   =   265,537,874
print(f"(f-1) x C4 (곱셈식 첫 증분) = {(f - 1) * C4:>13,.0f}") #> (f-1) x C4 (곱셈식 첫 증분) =   170,364,124
print("PooledLoss  ratio @ dur12 =",
      round(pooled.filter(pl.col("duration") == 12)["ratio_proj"][0], 2))  #> 1.3
print("ChainLadder ratio @ dur12 =",
      round(link.filter(pl.col("duration") == 12)["ratio_proj"][0], 2))    #> 0.88
```

## 4.5 코호트 수준 보정 — CredibleLoss

PooledLoss의 강도 `g_k`는 **코호트 전체에서 풀링**해 추정한다. 풀링이
PooledLoss를 안정적으로 만들지만, 대가가 하나 있다 — 어떤 코호트가
포트폴리오 평균보다 일관되게 높게(또는 낮게) 달려와도, PooledLoss의 예측
증분은 포트폴리오 공통의 강도를 쓰므로 그 **수준 차이가 지워진다**.
4.3에서 본 "평균 쪽으로 당겨 보는" 성질의 뒷면이다.

**CredibleLoss(부분 풀링)**는 그 수준만 돌려준다. 형상(`g_k`)은
PooledLoss의 풀링을 그대로 쓰고, 코호트별 스케일 하나를 증분에 곱한다.

$$
\text{(예측) 증분 손해}_{c,k} = s_c \times g_k \times \text{누적 보험료}_{c,k-1}
$$

스케일 $s_c$는 그 코호트의 관측 강도가 풀링 강도의 몇 배였는지의
중앙값을, **신뢰도**(credibility) 가중으로 1(포트폴리오 평균)을 향해
줄인 값이다.

$$
s_c = \frac{n \times \text{관측 배율의 중앙값} + K \times 1}{n + K}
$$

$n$은 그 코호트의 관측이 갖는 무게(노출을 분산으로 가중한 *유효표본*이지,
관측 경과의 단순 *개수*가 아니다 — 분기마다 큰 보험료가 실릴수록 같은 개수라도
$n$이 커진다), $K$는 신뢰도 상수다. `CredibleLoss`는 이 $K$를 직접 노출하지
않고, 코호트 간 분산 `psi`(기본 `"auto"` — Buhlmann-Straub 적률로 추정)에서
$K = 1/\psi$로 내부적으로 정한다. `psi = 0`이면 모든 코호트가 1에 묶여
PooledLoss와 같아진다.
갓 인수되어 $n$이 작은 코호트는 1에 묶여 사실상 PooledLoss와 같고, 관측이
쌓일수록 자기 수준을 더 믿는다 — "자기 경험을 말할 자격이 생긴 만큼만
반영"하는 구조다.

실제 한 코호트로 채워 보면 작동이 또렷하다. 위 최근 수술담보
코호트(2025-1분기)는 관측된 경과가 3개(경과 2,3,4의 증분)뿐이고, 각 경과의
관측 강도가 풀링 강도의 약 [0.87, 0.77, 0.74]배 — 중앙값 약 0.78로 포트폴리오
평균(= 1)보다 낮게 달려왔다. `psi="auto"`가 코호트 간 분산을 $\hat\psi \approx
0.0101$로 추정하니 신뢰도 상수는 $K = 1/\hat\psi \approx 99$다. 여기서 신뢰도
가중의 "표본 크기"는 관측 경과의 개수 3이 아니라, 그 3개 링크를 받쳐 주는
보험료 노출을 분산으로 가중한 유효표본 $n \approx 48$이다. 그래서 신뢰도는

$$
Z = \frac{n}{n + K} = \frac{48}{48 + 99} \approx 0.33,
$$

곧 자기 수준을 1/3만 믿고 2/3는 평균으로 당긴다. 스케일은

$$
s_c = \frac{n \times 0.78 + K \times 1}{n + K}
    = Z \times 0.78 + (1 - Z) \times 1
    \approx 0.33 \times 0.78 + 0.67 \times 1 \approx 0.93.
$$

예측 증분이 0.93배로 눌리니, 이 코호트의 CredibleLoss 예측 손해율(아래 표의
1.23)이 PooledLoss(1.30) 아래로 내려앉는다.

```{eval-rst}
.. plot::
   :context: close-figs
   :caption: 같은 최근 수술담보 코호트(2025-1분기)에 CredibleLoss를 적용. 형상은 PooledLoss와 같지만, 이 코호트가 포트폴리오 평균보다 낮게 달려온 만큼 수준이 아래로 보정되어 약 1.23에 닿는다(PooledLoss는 약 1.30).

   cs = (lr.CredibleLoss().fit(tri).df
         .filter(pl.col("cohort") == COH).sort("duration"))

   fig, ax = plt.subplots(figsize=(6.2, 3.6))
   ax.plot([d for d, r in obs_pairs], [r for d, r in obs_pairs],
           "-o", color="black", ms=4, label="observed")
   ax.plot(duration, pooled["ratio_proj"].to_list(), "--", color="#1f77b4",
           alpha=0.4, label="PooledLoss")
   ax.plot(duration, cs["ratio_proj"].to_list(), "-", color="#2ca02c", lw=2,
           label="CredibleLoss")
   ax.set_xlabel("duration (quarters)")
   ax.set_ylabel("cumulative loss ratio")
   ax.legend()
```

코호트별 예측 손해율을 PooledLoss와 나란히 두면 보정의 방향이 보인다.

```python
df = lr.load_experience().filter(pl.col("coverage") == "SURGERY")
tri = lr.Triangle(df, groups="coverage", grain="Q")

pooled = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(tri).summary()
cred = lr.Ratio(loss=lr.CredibleLoss(), premium=lr.PooledPremium()).fit(tri).summary()
#> 코호트        PooledLoss ratio_proj   CredibleLoss ratio_proj
#> 2025-01-01    1.299                   1.227    <- 평균보다 낮게 달려온 코호트: 아래로
#> 2025-04-01    1.345                   1.237
#> 2025-07-01    1.375                   1.346
#> 2025-10-01    1.406                   1.406    <- 관측 1개뿐: 스케일이 1에 묶여 PooledLoss와 같음
```

```{admonition} CredibleLoss는 어디에 좋고 어디에 좋지 않나
:class: important

CredibleLoss가 고치는 것은 **집계 수준의 체계적 편향**이다 — 코호트 수준
차이를 지우는 PooledLoss의 풀링이 포트폴리오 합계를 한쪽으로 치우치게 할
때, 특히 관측이 얇은 최근 코호트가 많은 크고 매끄러운 담보에서 효과가
있다. 반면 **셀 단위 정확도는 대체로 PooledLoss가 낫다** — 코호트별
스케일은 수준 편향을 줄이는 대신 셀 단위의 잡음을 더한다. 그리고 담보에
따라서는 보정이 집계 편향을 오히려 키우기도 하므로, 일괄 적용하지 말고
**담보별 백테스트로 확인한 뒤** 채택한다(7장).

또 하나 — CredibleLoss의 표준오차·신뢰구간은 닫힌형이 없어 **부트스트랩
전용**이다. `uncertainty=lr.ResidualBootstrap(...)`로 켜며, 5장에서 다시
본다.
```

## 4.6 형상 평활 — SmoothLoss

CredibleLoss가 코호트의 *수준*을 보정했다면, **SmoothLoss**는 풀링 *형상*을
다듬는다. PooledLoss의 강도 $g_k$는 경과마다 자유모수로(경과별로 따로) 추정하므로,
관측이 두꺼운 큰 담보에선 그 형상이 이미 매끄럽다 — 그런 곳에선 SmoothLoss와
PooledLoss가 사실상 겹친다(앞 절들이 SURGERY 같은 큰 담보를 쓴 이유다).

값이 갈리는 곳은 **얇은 세그먼트의 후기 경과**다. 거기선 한 경과에 모인
코호트가 몇 개뿐이라 풀링 강도 자체가 들쭉날쭉해진다. SmoothLoss는 그 형상에
P-spline(2차 차분 벌점)을 씌워 경과에 따라 매끄럽게 변하도록 강제한다 —
$g_k = \exp(s(k))$, $s(k) = B(k)\cdot\beta$.

암(CANCER)담보의 한 채널(TM) 세그먼트로 보면 분명하다(이 세그먼트는 한 경과에
모인 코호트가 30개 안팎에서 시작해 후기 경과로 갈수록 한 자릿수까지 준다).
경과별 강도 $g_k$를 풀링(raw)과 평활로 나란히 두면, 풀링 강도는 경과마다 위아래로
튀는데 SmoothLoss는 그 추세만 남긴다.

```{list-table} 얇은 세그먼트(암담보·TM)의 후기 경과별 강도 $g_k$ — 풀링(raw) vs 평활
:header-rows: 1
:widths: 18 28 28

* - 경과(월)
  - PooledLoss $g_k$ (풀링 raw)
  - SmoothLoss $g_k$ (평활)
* - 12
  - 0.066
  - 0.081
* - 13
  - 0.091  ↑ 튐
  - 0.073
* - 14
  - 0.057  ↓
  - 0.066
* - 15
  - 0.054
  - 0.061
* - 16
  - 0.072  ↑ 튐
  - 0.057
* - 17
  - 0.039  ↓
  - 0.053
* - 18
  - 0.063  ↑ 튐
  - 0.050
* - 19
  - 0.041  ↓
  - 0.047
* - 20
  - 0.047
  - 0.045
```

풀링 강도(가운데 열)는 0.066→0.091→0.057→…처럼 코호트가 적어 생긴 잡음으로
들쭉날쭉하지만, SmoothLoss(오른쪽 열)는 0.081→0.073→0.066→…으로 매끄럽게
내려간다. 들쭉날쭉함은 단언 대신 숫자로 잴 수 있다 — 곡률(curvature)을 경과축
*2차 차분의 RMS*로 정의하면(작을수록 매끄럽다), 위 표의 두 열에서 곧장 나온다.

```python
import numpy as np

# 위 표의 풀링(raw) vs 평활(smooth) g_k (경과 12-20)
raw    = np.array([0.066, 0.091, 0.057, 0.054, 0.072, 0.039, 0.063, 0.041, 0.047])
smooth = np.array([0.081, 0.073, 0.066, 0.061, 0.057, 0.053, 0.050, 0.047, 0.045])

curv = lambda g: float(np.sqrt(np.mean(np.diff(g, 2) ** 2)))   # 2차 차분의 RMS
print(round(curv(raw), 4), round(curv(smooth), 4),
      round(curv(raw) / curv(smooth), 1))
#> 0.0441 0.0011 41.3
```

곡률이 0.044에서 0.0011로 **약 41배** 줄어든다 — P-spline이 추세만 남기고
표본 잡음을 걷어낸 결과다.

```{admonition} 정직하게 — 이 합성 데이터엔 보탬이 적다
:class: note

위 대비는 *얇은 세그먼트의 후기 경과*에서만 두드러진다. 큰 매끄러운
담보(앞 절의 SURGERY 등)에서는 풀링 강도가 이미 매끄러워 SmoothLoss와
PooledLoss의 예측 손해율이 셀 단위로 거의 같다(중앙값 0%). 그러니 SmoothLoss를
일괄 채택하지 말고, *형상이 거친 세그먼트*에서만 — 7장 백테스트로 보탬을 확인한 뒤
— 쓴다. CredibleLoss처럼 표준오차·신뢰구간은 닫힌형이 없어 **부트스트랩
전용**이다(5장).
```

```{admonition} 보정·평활을 더 다루는 인자들 (covariates / lam_cov / balance)
:class: seealso

`CredibleLoss`·`SmoothLoss`에는 이 장이 쓰지 않은 인자가 더 있다.

- **`covariates=[...]`** — 코호트 *수준*을 코호트마다 따로 추정하는 대신,
  `age_band`·`channel` 같은 세그먼트 키를 공유해 *세그먼트 안에서 풀링*한다.
  얇은 세그먼트의 수준이 그 코호트 하나가 아니라 같은 키를 가진 코호트들에서
  추정되어 더 안정된다. `lam_cov`(기본 `0.0`)는 그 세그먼트 수준을 평균으로
  당기는 능선(ridge) 벌점 — `"auto"`면 데이터에서 고른다. 이 covariate 경로가
  바로 **6장의 covariate regime 처리**가 올라타는 구조다.
- **`balance=False`** — 켜면 적합된 증분의 경과별 합이 관측 합과 맞도록
  강도를 미세 조정한다(집계 정합용). 기본은 끔.

세 인자 모두 *기본값에선 이 장의 결과를 바꾸지 않는다* — 필요할 때만 켜는
확장 손잡이다. 자세한 사용은 6장과 {doc}`API 레퍼런스 <../api>`를 본다.
```

## 4.7 신뢰의 스펙트럼으로 정리

PooledLoss와 ChainLadder는 대립하는 두 방법이 아니라 **하나의 축 위
양 끝**이다 — *관측된 진전 패턴을 얼마나 믿느냐*의 스펙트럼이다.

- **PooledLoss** — 진전 대신 **노출(보험료)에 고정**한다. 게다가 강도
  `g_k`를 *코호트 전체에서 풀링*해 추정하므로, 한 최근 코호트의 들쭉날쭉한
  관측이 아니라 포트폴리오 평균 노출 패턴이 예측을 끈다(안정적인 대신
  노출 패턴 유지를 가정).
- **ChainLadder** — 진전을 전적으로 믿는다. 코호트가 쌓은 누적 손해에 ATA
  인자를 곱해, 관측된 진전을 그대로 미래로 연장한다. 충분히 경과한
  코호트엔 정확하지만, 얇은 초기 관측에 곱셈을 거듭하면 출렁인다.

이 "노출 앵커 + 코호트 풀링"이 핵심이다. 손해보험 reserving에서는 외부
prior 손해율을 손수 넣거나(Bornhuetter-Ferguson), 데이터에서 풀링해(Cape
Cod) 최근 코호트를 안정시킨다. 장기 건강보험에서는 **PooledLoss가 이미 그
역할을 한다** — 보험료라는 외부 기준에 고정하고 강도를 코호트 간 풀링하므로,
별도의 prior 층을 얹지 않아도 최근 코호트가 노출 수준에 머문다.

CredibleLoss는 이 스펙트럼과 **직교하는 보정**이다. 진전 패턴(형상)에 대한
믿음은 PooledLoss의 풀링 그대로 두고, 코호트가 자기 *수준*을 말할 자격 —
곧 관측이 쌓인 정도 — 만큼만 그 수준을 반영한다. 형상의 축(PooledLoss vs
ChainLadder)과 수준의 축(풀링 vs 코호트별)이 별개라는 것이, 이 장 방법들의
가장 간결한 지도다. SmoothLoss는 이 지도에서 *형상의 축*에 놓인다 — PooledLoss의
풀링 형상을 매끄러운 쪽으로 다듬은 변형이라, 수준의 축에선 PooledLoss와 같은
자리다(그래서 매끄러운 담보에선 둘이 사실상 겹친다).

## 4.8 어느 방법을 언제

```{list-table}
:header-rows: 1
:widths: 12 88

* - 방법
  - 어울리는 상황
* - PooledLoss
  - 초기 구간이 많거나 ATA 인자가 불안정할 때의 안전한 기본값. 의심스러우면 여기서 시작
* - CredibleLoss
  - 포트폴리오 집계 수준의 체계적 편향이 보일 때. 담보별 백테스트로 보정 효과를 확인한 뒤 채택
* - SmoothLoss
  - 경과별 강도 형상이 들쭉날쭉할 때(얇은 late-tail·thin 세그먼트). 매끄러운 큰 담보에선 PooledLoss와 사실상 같아 추가 이득이 없음
* - ChainLadder
  - 업계 관행과의 소통·검증, 방법 비교의 참조점. 최근 코호트엔 위험
```

베이스라인 `PooledLoss`로 시작해, 수준 보정이 보탬이 된다는 근거가
백테스트로 확인되면 CredibleLoss를 채택하는 것이 실무의 출발점이다. 어느
방법이 *내 데이터*에 맞는지는 취향이 아니라 검증으로 가린다 — 같은
hold-out에서 두 방법의 held-out 셀을 매칭 비교하는 `lr.EstimatorComparison`이
그 도구다(7장).

## 4.9 결과 객체에서 — `.predict()` 와 `.plot()`

적합 결과(`LossFit` / `PremiumFit` / `RatioFit`)는 셀별 진단이 모두 담긴
`.df` 외에, 결과를 바로 쓰기 위한 두 메서드를 제공한다.

`.predict()`는 **예측 표면**만 추린다 — 코호트 x 경과 셀에 예측값과 각 셀의
출처(`source`)를 붙이고, 표준오차·신뢰구간 열은 덜어 낸 가벼운 뷰다.
`source`는 셋 중 하나다.

- **`observed`** — 삼각형에 실제로 관측된 칸.
- **`own`** — 그 코호트가 *자기 데이터*로 채운 예측 칸(이 장의 기본 경로).
- **`borrowed`** — 그 코호트의 관측이 닿지 못하는 깊은 경과를, *다른 regime의
  발전 형상*으로 채운 칸. 6장에서 `regime`을 `segment_wise`로 두면, 최근
  regime의 코호트가 아직 관측하지 못한 후기 경과를 더 오래된(깊은) regime의
  *수준 불변* 링크비(level-invariant ATA 인자)로 잇는다 — 손해율 *수준*은
  자기 것을 쓰되 *형상*만 빌린다. 기본(`latest_only`) 경로에서는 나타나지
  않고, 빌려 온 깊은 꼬리를 표시하는 6장 전용 표식이다.

```python
fit = lr.Ratio(loss=lr.PooledLoss(), premium=lr.PooledPremium()).fit(tri)
fit.predict().columns
#> ['coverage', 'cohort', 'duration', 'loss_proj', 'premium_proj', 'ratio_proj', 'source']
```

`.plot()`은 4.4에서 손으로 그린 코호트별 궤적 그림을 **한 줄로** 그린다 —
관측 구간은 실선, 예측 구간은 점선으로 코호트마다 색을 입혀 그룹별로
패싯한다(위 4.4 그림이 바로 이 그림이다). `metric`으로 곡선을 고른다 —
`RatioFit`은 `"ratio"`(기본)·`"loss"`·`"premium"`, `LossFit`은
`"loss"`(기본)·`"ratio"`, `PremiumFit`은 `"premium"`이다.

```python
fit.plot(metric="ratio")            # 예측 손해율 궤적 (RatioFit)
lr.PooledLoss().fit(tri).plot()     # 누적 손해 궤적 (LossFit, 기본 metric="loss")
```

## 4.10 함께 보기

- {doc}`2장 — 손해의 진전 <02-ata>`와 {doc}`3장 — 강도의 직관
  <03-intensity>`: 이 장이 빈 칸을 채우는 데 쓴 ATA 인자와 강도.
- {doc}`5장 — 예측의 불확실성 <05-uncertainty>`: 이 장의 점추정에 표준오차와
  신뢰구간을 입혀 "얼마나 믿을 수 있나"를 정량화한다.
- {doc}`7장 — 예측 검증 <07-backtest>`: 방법 선택을 데이터로 가리는
  백테스트, rolling-origin 신뢰도 곡선, 방법 간 매칭 비교.
- {doc}`집계 주기 안정성 <../cookbook/grain-stability>`: 월·분기 예측이 갈리는
  이유와, 월에서 한 번 적합하고 `LossFit.at_grain()` 으로 거친 주기를 *집계
  뷰*로 보는 grain-안정 패턴.
- {doc}`API 레퍼런스 <../api>`의 `PooledLoss`, `CredibleLoss`, `SmoothLoss`, `ChainLadder`, `Ratio`
