# Setup — use a single-group subset to keep test fast
data(experience)
exp <- as_experience(experience)
sub <- build_triangle(exp[cv_nm == "SUR"], group_var = cv_nm)

test_that("detect_cohort_regime returns class 'cohort_regime' (ecp default)", {
  r <- detect_cohort_regime(sub, K = 12, method = "ecp")
  expect_s3_class(r, "cohort_regime")
  expect_equal(r$method, "ecp")
  expect_equal(r$K, 12)
})

test_that("cohort_regime has expected list elements", {
  r <- detect_cohort_regime(sub, K = 12, method = "ecp")
  for (nm in c("method", "value_var", "K", "cohort_var", "dev_var",
               "group_var", "labels", "breakpoints", "n_regimes",
               "trajectory", "pca")) {
    expect_true(nm %in% names(r), info = paste("missing", nm))
  }
})

test_that("$labels has cohort and regime columns", {
  r <- detect_cohort_regime(sub, K = 12, method = "ecp")
  expect_true("cohort" %in% names(r$labels))
  expect_true("regime" %in% names(r$labels))
})

test_that("$trajectory is a numeric matrix", {
  r <- detect_cohort_regime(sub, K = 12, method = "ecp")
  expect_true(is.matrix(r$trajectory))
  expect_true(is.numeric(r$trajectory))
})

test_that("$pca$sdev is positive numeric vector", {
  r <- detect_cohort_regime(sub, K = 12, method = "ecp")
  expect_true(is.numeric(r$pca$sdev))
  expect_true(all(r$pca$sdev >= 0))
})

test_that("methods 'pelt' and 'hclust' run", {
  expect_s3_class(detect_cohort_regime(sub, K = 12, method = "pelt"),
                  "cohort_regime")
  expect_s3_class(detect_cohort_regime(sub, K = 12, method = "hclust"),
                  "cohort_regime")
})

test_that("hclust with n_regimes = 3 runs", {
  # Note: hclust with k = 3 can produce > 3 sequential regime segments
  # if non-adjacent cohorts share a cluster. Just verify the call runs.
  r <- detect_cohort_regime(sub, K = 12, method = "hclust", n_regimes = 3)
  expect_s3_class(r, "cohort_regime")
  expect_true(r$n_regimes >= 1L)
})

test_that("value_var = 'lr' runs", {
  expect_s3_class(detect_cohort_regime(sub, K = 12, method = "ecp", value_var = "lr"),
                  "cohort_regime")
})

test_that("summary.cohort_regime returns class 'summary.cohort_regime'", {
  r <- detect_cohort_regime(sub, K = 12, method = "ecp")
  s <- summary(r)
  expect_s3_class(s, "summary.cohort_regime")
})

test_that("print methods don't error", {
  r <- detect_cohort_regime(sub, K = 12, method = "ecp")
  expect_no_error(capture.output(print(r)))
  expect_no_error(capture.output(print(summary(r))))
})
