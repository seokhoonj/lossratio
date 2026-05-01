# Hybrid point projection for a single cohort

Internal helper that projects cumulative loss:

- **sa (stage-adaptive)**: ED before maturity, CL after.

- **ed**: ED for all periods.

- **cl**: CL for all periods.

## Usage

``` r
.sa_proj(
  loss_obs,
  exposure_proj,
  g_selected,
  f_selected,
  maturity_from,
  method = "sa"
)
```

## Arguments

- loss_obs:

  Numeric vector of observed cumulative loss.

- exposure_proj:

  Numeric vector of projected cumulative exposure.

- g_selected:

  Numeric vector of ED intensities.

- f_selected:

  Numeric vector of CL factors.

- maturity_from:

  Numeric scalar; switch point. `NA` = no switch.

- method:

  One of `"sa"`, `"ed"`, or `"cl"`.

## Value

A numeric vector with projected cumulative loss.
