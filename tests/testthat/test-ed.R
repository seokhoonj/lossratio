# Setup
data(experience)
exp <- as_experience(experience)
tri <- build_triangle(exp, group_var = cv_nm)
ed  <- build_ed(tri)

test_that("build_ed returns class 'ed' with expected columns", {
  expect_s3_class(ed, "ed")
  for (nm in c("cv_nm", "cohort", "ata_from", "ata_to", "ata_link",
               "loss_from", "loss_to", "delta_loss",
               "exposure_from", "exposure_to", "g")) {
    expect_true(nm %in% names(ed), info = paste("missing", nm))
  }
})

test_that("build_ed attributes set correctly", {
  for (a in c("group_var", "cohort_var", "dev_var", "loss_var", "exposure_var")) {
    expect_false(is.null(attr(ed, a)), info = paste("missing attr", a))
  }
})

test_that("g == delta_loss / exposure_from when exposure_from > 0", {
  ok <- is.finite(ed$g) & ed$exposure_from > 0
  expect_equal(ed$g[ok], ed$delta_loss[ok] / ed$exposure_from[ok], tolerance = 1e-6)
})

test_that("delta_loss == loss_to - loss_from", {
  ok <- is.finite(ed$delta_loss)
  expect_equal(ed$delta_loss[ok],
               ed$loss_to[ok] - ed$loss_from[ok],
               tolerance = 1e-6)
})

test_that("build_ed errors when loss_var == exposure_var", {
  expect_error(build_ed(tri, loss_var = "closs", exposure_var = "closs"))
})

# fit_ed -----------------------------------------------------------------

test_that("fit_ed returns class 'ed_fit' with expected components", {
  ef <- fit_ed(ed)
  expect_s3_class(ef, "ed_fit")
  for (nm in c("factor", "selected")) {
    expect_true(nm %in% names(ef), info = paste("missing", nm))
  }
})

test_that("fit_ed method = 'basic' and 'mack' both work", {
  expect_no_error(fit_ed(ed, method = "basic"))
  ef_mack <- fit_ed(ed, method = "mack")
  expect_s3_class(ef_mack, "ed_fit")
})

test_that("fit_ed sigma_method variants run", {
  for (sm in c("min_last2", "locf", "loglinear")) {
    expect_no_error(fit_ed(ed, sigma_method = sm))
  }
})

test_that("recent reduces selected rows count", {
  ef_full   <- fit_ed(ed)
  ef_recent <- fit_ed(ed, recent = 6)
  expect_true(nrow(ef_recent$selected) <= nrow(ef_full$selected))
})

test_that("print.ed_fit doesn't error", {
  ef <- fit_ed(ed)
  expect_no_error(capture.output(print(ef)))
})

# summary.ed -------------------------------------------------------------

test_that("summary.ed returns ed_summary with expected columns", {
  sm <- summary(ed, alpha = 1)
  expect_s3_class(sm, "ed_summary")
  for (nm in c("ata_from", "ata_to", "mean", "median", "wt", "g")) {
    expect_true(nm %in% names(sm), info = paste("missing", nm))
  }
})
