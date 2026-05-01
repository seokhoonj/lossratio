# Setup
data(experience)
exp <- as_experience(experience)
tri <- build_triangle(exp, group_var = cv_nm)

test_that("fit_lr default (method = 'sa') returns class 'lr_fit'", {
  lr <- fit_lr(tri)
  expect_s3_class(lr, "lr_fit")
  expect_equal(lr$method, "sa")
})

test_that("lr_fit has expected list elements", {
  lr <- fit_lr(tri, method = "sa")
  for (nm in c("data", "method", "group_var", "cohort_var", "dev_var",
               "loss_var", "exposure_var", "full", "pred", "summary",
               "ed", "loss_ata_fit", "exposure_ata_fit", "maturity",
               "delta_method", "rho", "conf_level")) {
    expect_true(nm %in% names(lr), info = paste("missing", nm))
  }
})

test_that("$full has expected columns", {
  lr <- fit_lr(tri, method = "sa")
  for (nm in c("cv_nm", "cohort", "dev", "loss_obs", "exposure_obs",
               "is_observed", "loss_proj", "exposure_proj")) {
    expect_true(nm %in% names(lr$full), info = paste("missing", nm))
  }
})

test_that("$summary has cohort-level entries with expected columns", {
  lr <- fit_lr(tri, method = "sa")
  for (nm in c("cv_nm", "cohort", "latest", "ultimate", "reserve")) {
    expect_true(nm %in% names(lr$summary), info = paste("missing", nm))
  }
})

test_that("methods 'sa', 'ed', 'cl' all run", {
  for (m in c("sa", "ed", "cl")) {
    lr <- fit_lr(tri, method = m)
    expect_s3_class(lr, "lr_fit")
    expect_equal(lr$method, m)
  }
})

test_that("delta_method 'simple' and 'full' both run", {
  expect_s3_class(fit_lr(tri, delta_method = "simple"), "lr_fit")
  expect_s3_class(fit_lr(tri, delta_method = "full", rho = 0.3), "lr_fit")
})

test_that("bootstrap = TRUE runs and returns class 'lr_fit'", {
  lr_b <- fit_lr(tri, method = "sa", bootstrap = TRUE, B = 25, seed = 1)
  expect_s3_class(lr_b, "lr_fit")
  expect_false(is.null(lr_b$bootstrap))
})

test_that("bootstrap reproducibility via seed", {
  lr_a <- fit_lr(tri, method = "sa", bootstrap = TRUE, B = 25, seed = 42)
  lr_b <- fit_lr(tri, method = "sa", bootstrap = TRUE, B = 25, seed = 42)
  expect_equal(lr_a$summary$ci_lower, lr_b$summary$ci_lower)
})

test_that("summary(lr_fit) returns the $summary table", {
  lr <- fit_lr(tri, method = "sa")
  expect_identical(summary(lr), lr$summary)
})

test_that("print.lr_fit doesn't error", {
  lr <- fit_lr(tri, method = "sa")
  expect_no_error(capture.output(print(lr)))
})
