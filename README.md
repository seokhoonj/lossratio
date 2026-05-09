# lossratio (Python)

Python sibling of the R `lossratio` package: loss ratio analytics
for long-term health insurance — cohort development analysis,
stage-adaptive projection, regime detection, and backtest validation
on long-format experience data. Stage-adaptive (SA) projection uses
an exposure-driven (ED) model before the maturity point and chain
ladder (CL) after, supported by maturity point detection, cohort
regime detection, and a calendar-diagonal backtest framework.

This Python implementation is in active development.

## Current status

Working components so far:

- `Experience` — validates loss ratio experience data (`cym`, `uym`,
  `loss_incr`, `premium_incr`), accepts polars or pandas input.
- `Triangle` — cohort × dev aggregation. Cumulative is the unmarked
  default (`loss`, `premium`, `lr`); per-period values carry an
  `_incr` (incremental) suffix (`loss_incr`, `premium_incr`,
  `lr_incr`).

Additional components (loss-ratio projection with stage-adaptive
method, maturity point and cohort regime detection, calendar-diagonal
backtest) are being added incrementally. The full working reference
implementation is the R `lossratio` package; see the section below
for a link.

## Quick Start

```python
import polars as pl
import lossratio as lr

# Three cohorts (2024-01, 02, 03) each observed over up to three months
df = pl.DataFrame({
    "cym": [
        "2024-01-01", "2024-02-01", "2024-03-01",   # cohort 2024-01, dev 1-3
        "2024-02-01", "2024-03-01",                  # cohort 2024-02, dev 1-2
        "2024-03-01",                                # cohort 2024-03, dev 1
    ],
    "uym": [
        "2024-01-01", "2024-01-01", "2024-01-01",
        "2024-02-01", "2024-02-01",
        "2024-03-01",
    ],
    "loss": [12.0, 18.0, 25.0, 15.0, 22.0,  9.0],
    "rp":   [100.0, 100.0, 100.0, 110.0, 110.0, 120.0],
})

exp = lr.Experience(df)
tri = exp.triangle()
print(tri.df)
#> shape: (6, 8)
#> ┌────────────┬─────┬──────┬───────┬───────┬───────┬──────────┬──────────┐
#> │ cohort     ┆ dev ┆ loss ┆ rp    ┆ closs ┆ crp   ┆ lr       ┆ clr      │
#> │ date       ┆ i64 ┆ f64  ┆ f64   ┆ f64   ┆ f64   ┆ f64      ┆ f64      │
#> ╞════════════╪═════╪══════╪═══════╪═══════╪═══════╪══════════╪══════════╡
#> │ 2024-01-01 ┆ 1   ┆ 12.0 ┆ 100.0 ┆ 12.0  ┆ 100.0 ┆ 0.12     ┆ 0.12     │
#> │ 2024-01-01 ┆ 2   ┆ 18.0 ┆ 100.0 ┆ 30.0  ┆ 200.0 ┆ 0.18     ┆ 0.15     │
#> │ 2024-01-01 ┆ 3   ┆ 25.0 ┆ 100.0 ┆ 55.0  ┆ 300.0 ┆ 0.25     ┆ 0.183333 │
#> │ 2024-02-01 ┆ 1   ┆ 15.0 ┆ 110.0 ┆ 15.0  ┆ 110.0 ┆ 0.136364 ┆ 0.136364 │
#> │ 2024-02-01 ┆ 2   ┆ 22.0 ┆ 110.0 ┆ 37.0  ┆ 220.0 ┆ 0.2      ┆ 0.168182 │
#> │ 2024-03-01 ┆ 1   ┆  9.0 ┆ 120.0 ┆  9.0  ┆ 120.0 ┆ 0.075    ┆ 0.075    │
#> └────────────┴─────┴──────┴───────┴───────┴───────┴──────────┴──────────┘
```

The same triangle can be sliced by an optional grouping variable
(coverage, product, age band, sum insured, ...):

```python
df_grouped = df.with_columns(pl.lit("SUR").alias("cv_nm"))
tri = lr.Experience(df_grouped).triangle(group_var="cv_nm")
```

Pandas inputs are accepted too; outputs mirror the input type
(pandas in → pandas out, polars in → polars out). Install with the
optional `pandas` extra:

```bash
pip install lossratio[pandas]
```

## R package

- Source: <https://github.com/seokhoonj/lossratio>
- Documentation: <https://seokhoonj.github.io/lossratio/>
- 한국어 문서: <https://seokhoonj.github.io/lossratio/ko/>

```r
remotes::install_github("seokhoonj/lossratio")
library(lossratio)
```

## Author

Seokhoon Joo (<seokhoonj@gmail.com>) — also maintains the R
`lossratio` package.

## License

MPL-2.0 (Mozilla Public License 2.0).
