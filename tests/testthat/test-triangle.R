data(experience, package = "lossratio")
exp <- as_experience(experience)

test_that("build_triangle returns object inheriting class 'triangle'", {
  tri <- build_triangle(exp, group_var = cv_nm)
  expect_s3_class(tri, "triangle")
})

test_that("build_triangle output has expected columns", {
  tri <- build_triangle(exp, group_var = cv_nm)
  expected <- c("cohort", "dev", "loss", "rp",
                "closs", "crp", "lr", "clr")
  expect_true(all(expected %in% names(tri)))
})

test_that("build_triangle sets attributes correctly", {
  tri <- build_triangle(exp, group_var = cv_nm)
  expect_equal(attr(tri, "cohort_var"),  "uym")
  expect_equal(attr(tri, "cohort_type"), "month")
  expect_equal(attr(tri, "dev_var"), "elap_m")
  expect_equal(attr(tri, "group_var"),   "cv_nm")
})

test_that("closs equals cumulative sum of loss within (group, cohort)", {
  tri <- build_triangle(exp, group_var = cv_nm)
  data.table::setorder(tri, cv_nm, cohort, dev)
  chk <- tri[, .(max_abs_err = max(abs(closs - cumsum(loss)))),
             by = .(cv_nm, cohort)]
  tol <- 1e-6
  expect_true(all(chk$max_abs_err <= tol))
})

test_that("lr equals loss/rp within each row when rp > 0", {
  tri <- build_triangle(exp, group_var = cv_nm)
  pos <- tri[rp > 0]
  expect_equal(pos$lr, pos$loss / pos$rp)
})

test_that("summary.triangle returns a triangle_summary with expected columns", {
  tri <- build_triangle(exp, group_var = cv_nm)
  sm <- summary(tri)
  expect_s3_class(sm, "triangle_summary")
  expected <- c("lr_mean", "lr_median", "lr_wt",
                "clr_mean", "clr_median", "clr_wt")
  expect_true(all(expected %in% names(sm)))
})

test_that("longer.triangle returns triangle_longer with variable/value", {
  tri <- build_triangle(exp, group_var = cv_nm)
  lng <- longer(tri)
  expect_s3_class(lng, "triangle_longer")
  expect_true(all(c("variable", "value") %in% names(lng)))
})

test_that("build_calendar returns class 'calendar' with expected columns", {
  cal <- build_calendar(exp, group_var = cv_nm)
  expect_s3_class(cal, "calendar")
  expect_true(all(c("calendar", "dev") %in% names(cal)))
  expect_equal(attr(cal, "calendar_var"), "cym")
})

test_that("build_total returns class 'total' with one row per group", {
  tot <- build_total(exp, group_var = cv_nm)
  expect_s3_class(tot, "total")
  expected <- c("n_obs", "sales_start", "sales_end",
                "loss", "rp", "lr", "loss_prop", "rp_prop")
  expect_true(all(expected %in% names(tot)))
  expect_equal(nrow(tot), data.table::uniqueN(exp$cv_nm))
})

test_that("validate_triangle returns class 'triangle_validation' with no gaps", {
  res <- validate_triangle(experience, group_var = cv_nm)
  expect_s3_class(res, "triangle_validation")
  expect_equal(nrow(res), 0L)
})

test_that("build_triangle errors when group_var is invalid", {
  expect_error(build_triangle(exp, group_var = nonexistent_col))
})
