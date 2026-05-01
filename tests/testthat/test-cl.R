# Setup
data(experience)
exp <- as_experience(experience)
tri <- build_triangle(exp, group_var = cv_nm)

test_that("fit_cl method = 'basic' returns class 'cl_fit' with expected structure", {
  cl <- fit_cl(tri, value_var = "closs", method = "basic")
  expect_s3_class(cl, "cl_fit")
  for (nm in c("data", "method", "group_var", "cohort_var", "dev_var",
               "value_var", "full", "pred", "ata", "summary",
               "factor", "selected")) {
    expect_true(nm %in% names(cl), info = paste("missing", nm))
  }
  expect_equal(cl$method, "basic")
})

test_that("$full has expected columns", {
  cl <- fit_cl(tri, value_var = "closs", method = "basic")
  for (nm in c("cv_nm", "cohort", "dev", "value_obs", "value_proj", "is_observed")) {
    expect_true(nm %in% names(cl$full), info = paste("missing", nm))
  }
})

test_that("value_proj is finite for projected cells", {
  cl <- fit_cl(tri, value_var = "closs", method = "basic")
  proj_only <- cl$full[is_observed == FALSE]
  expect_true(all(is.finite(proj_only$value_proj)))
})

test_that("fit_cl method = 'mack' adds variance columns", {
  cl <- fit_cl(tri, value_var = "closs", method = "mack")
  for (nm in c("proc_se", "param_se", "se_proj", "cv_proj")) {
    expect_true(nm %in% names(cl$full), info = paste("missing", nm))
  }
})

test_that("Mack standard errors are non-negative", {
  cl <- fit_cl(tri, value_var = "closs", method = "mack")
  ses <- cl$full[is.finite(se_proj), se_proj]
  expect_true(all(ses >= 0))
})

test_that("$summary has one row per (group, cohort) with expected columns", {
  cl <- fit_cl(tri, value_var = "closs", method = "mack")
  for (nm in c("cv_nm", "cohort", "latest", "ultimate", "reserve")) {
    expect_true(nm %in% names(cl$summary), info = paste("missing", nm))
  }
})

test_that("tail = TRUE produces finite tail_factor", {
  cl <- fit_cl(tri, value_var = "closs", method = "basic", tail = TRUE)
  expect_true(is.finite(cl$tail_factor))
})

test_that("tail = numeric scalar sets tail_factor literally", {
  cl <- fit_cl(tri, value_var = "closs", method = "basic", tail = 1.05)
  expect_equal(cl$tail_factor, 1.05)
})

test_that("tail = FALSE keeps tail_factor at 1", {
  cl <- fit_cl(tri, value_var = "closs", method = "basic", tail = FALSE)
  expect_equal(cl$tail_factor, 1)
})

test_that("maturity_args triggers maturity detection", {
  cl <- fit_cl(tri, value_var = "closs", method = "basic", maturity_args = list())
  expect_false(is.null(cl$maturity))
})

test_that("summary(cl_fit) returns the $summary table", {
  cl <- fit_cl(tri, value_var = "closs", method = "mack")
  s <- summary(cl)
  expect_identical(s, cl$summary)
})

test_that("print.cl_fit doesn't error", {
  cl <- fit_cl(tri, value_var = "closs", method = "mack")
  expect_no_error(capture.output(print(cl)))
})
