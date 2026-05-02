# Detect structural regime shifts across underwriting cohorts

Detect structural change points in the sequence of cohort-level
development trajectories. Each underwriting cohort (indexed by the
`cohort_var` of a `"Triangle"` object) is treated as a feature vector
whose entries are the selected `value_var` observed at development
periods `1, ..., K`. Cohorts are then ordered by underwriting period and
tested for structural shifts in the multivariate sequence.

Three detection strategies are supported:

- `"ecp"`:

  Multivariate non-parametric divisive change-point detection via
  [`ecp::e.divisive()`](https://rdrr.io/pkg/ecp/man/e.divisive.html).
  The number of regimes is determined by the data; only significant
  breakpoints at `sig_level` are retained. Preferred when the number of
  regimes is not known in advance.

- `"pelt"`:

  Univariate mean change-point detection via
  [`changepoint::cpt.mean()`](https://rdrr.io/pkg/changepoint/man/cpt.mean.html)
  with the PELT algorithm applied to the first principal component of
  the cohort feature matrix. Fast and may return multiple breakpoints.

- `"hclust"`:

  Ward hierarchical clustering on the scaled cohort feature matrix, cut
  to `n_regimes` clusters. Ignores time ordering — useful as a sanity
  check since non-adjacent cohorts may cluster together if the
  trajectory pattern is not strictly chronological.

## Usage

``` r
detect_cohort_regime(
  x,
  value_var = "clr",
  K = 12L,
  method = c("ecp", "pelt", "hclust"),
  n_regimes = NULL,
  sig_level = 0.05,
  min_size = 3L,
  ...
)

# S3 method for class 'CohortRegime'
print(x, ...)

# S3 method for class 'CohortRegime'
summary(object, ...)

# S3 method for class 'summary.CohortRegime'
print(x, ...)
```

## Arguments

- x:

  An object of class `"Triangle"`. Must correspond to a single group (no
  `group_var` or a single-value `group_var` subset). Also used by S3
  [`print()`](https://rdrr.io/r/base/print.html) method on
  `CohortRegime` objects.

- value_var:

  Column name of the trajectory variable. Default is `"clr"` (cumulative
  loss ratio).

- K:

  Integer. Common development-period window used to build the cohort
  feature matrix. Cohorts with fewer than `K` observed periods are
  dropped. Default is `12`.

- method:

  One of `"ecp"`, `"pelt"`, `"hclust"`.

- n_regimes:

  Integer. Number of regimes to force. `NULL` means auto-detect for
  `"ecp"` and `"pelt"`; ignored (required to equal the requested value)
  for `"hclust"`, where the default is `2`.

- sig_level:

  Significance level for `"ecp"`. Default `0.05`.

- min_size:

  Minimum segment size for `"ecp"`. Default `3`.

- ...:

  Reserved for future use.

- object:

  An object of class `"CohortRegime"`. Used by the S3
  [`summary()`](https://rdrr.io/r/base/summary.html) method.

## Value

An object of class `"CohortRegime"` with components:

- `call`:

  Matched call.

- `method`:

  Detection method used.

- `value_var`, `K`:

  Trajectory variable and window.

- `cohort_var`:

  Period variable from `x`.

- `labels`:

  `data.table` with one row per analysed cohort: period, regime id,
  regime label.

- `breakpoints`:

  `Date` vector of breakpoint dates (each is the first cohort of a new
  regime; excludes the initial regime start).

- `n_regimes`:

  Number of regimes detected.

- `trajectory`:

  Cohort feature matrix (rows = cohorts, columns = development periods
  `1, ..., K`).

- `pca`:

  `prcomp` object fitted to the feature matrix.

- `dropped`:

  Cohorts excluded due to the `K` window constraint.

## See also

[`plot.CohortRegime()`](https://seokhoonj.github.io/lossratio/reference/plot.CohortRegime.md),
[`build_triangle()`](https://seokhoonj.github.io/lossratio/reference/build_triangle.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(experience)
exp <- as_experience(experience)
tri_sur <- build_triangle(exp[cv_nm == "SUR"], group_var = cv_nm)

# Hierarchical clustering (no extra package dependency)
r <- detect_cohort_regime(tri_sur, K = 12, method = "hclust",
                          n_regimes = 2L)
print(r)
summary(r)
plot(r)

# ecp divisive change-point detection (requires the ecp package)
r_ecp <- detect_cohort_regime(tri_sur, K = 12, method = "ecp")
} # }
```
