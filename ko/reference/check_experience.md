# Check an experience dataset

Check that an experience dataset contains the required columns with the
expected classes, and validate the classes of optional columns when
present.

## Usage

``` r
check_experience(df)
```

## Arguments

- df:

  A data.frame containing experience data.

## Value

Invisibly returns the result of
[`.check_col_spec()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-check_col_spec.md).

## Required columns

These columns must be present:

- `cym` : Calendar year-month (`Date`)

- `uym` : Underwriting year-month (`Date`)

- `loss` : Incurred loss amount (`numeric`)

- `rp` : Risk premium (`numeric`)

## Optional columns

These columns are validated only when present:

- `elap_m` : Elapsed month (`integer`)

- `pd_tp_cd`, `pd_tp_nm`, `pd_cd`, `pd_nm`: Product type/product codes
  and names (`character`)

- `cv_tp_cd`, `cv_tp_nm`, `cv_cd`, `cv_nm`: Coverage type/coverage codes
  and names (`character`)

- `rd_tp_cd`, `rd_tp_nm`, `rd_cd`, `rd_nm`: Rider type/rider codes and
  names (`character`)

- `age_band` : Age band (`ordered`)

- `gender` : Gender (`factor`)

- `ch_cd`, `ch_nm` : Channel code and name (`character`)

- `n_policy` : Number of unique policies in the cell (`integer`)

## Derived columns

The following columns may be derived later by
[`add_experience_period()`](https://seokhoonj.github.io/lossratio/ko/reference/add_experience_period.md)
and are not validated here:

- `uy`, `uyh`, `uyq` : Underwriting year, half-year, quarter

- `cy`, `cyh`, `cyq` : Calendar year, half-year, quarter

- `elap_y`, `elap_h`, `elap_q` : Elapsed year, half-year, quarter

## See also

[`as_experience()`](https://seokhoonj.github.io/lossratio/ko/reference/as_experience.md),
[`add_experience_period()`](https://seokhoonj.github.io/lossratio/ko/reference/add_experience_period.md),
[`.check_col_spec()`](https://seokhoonj.github.io/lossratio/ko/reference/dot-check_col_spec.md)
