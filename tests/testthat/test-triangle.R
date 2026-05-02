data(experience, package = "lossratio")
exp <- as_experience(experience)

test_that("build_triangle returns object inheriting class 'Triangle'", {
  tri <- build_triangle(exp, group_var = cv_nm)
  expect_s3_class(tri, "Triangle")
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

test_that("summary.Triangle returns a TriangleSummary with expected columns", {
  tri <- build_triangle(exp, group_var = cv_nm)
  sm <- summary(tri)
  expect_s3_class(sm, "TriangleSummary")
  expected <- c("lr_mean", "lr_median", "lr_wt",
                "clr_mean", "clr_median", "clr_wt")
  expect_true(all(expected %in% names(sm)))
})

test_that("longer.Triangle returns TriangleLonger with variable/value", {
  tri <- build_triangle(exp, group_var = cv_nm)
  lng <- longer(tri)
  expect_s3_class(lng, "TriangleLonger")
  expect_true(all(c("variable", "value") %in% names(lng)))
})

test_that("build_calendar returns class 'Calendar' with expected columns", {
  cal <- build_calendar(exp, group_var = cv_nm)
  expect_s3_class(cal, "Calendar")
  expect_true(all(c("calendar", "dev") %in% names(cal)))
  expect_equal(attr(cal, "calendar_var"), "cym")
})

test_that("build_total returns class 'Total' with one row per group", {
  tot <- build_total(exp, group_var = cv_nm)
  expect_s3_class(tot, "Total")
  expected <- c("n_obs", "sales_start", "sales_end",
                "loss", "rp", "lr", "loss_prop", "rp_prop")
  expect_true(all(expected %in% names(tot)))
  expect_equal(nrow(tot), data.table::uniqueN(exp$cv_nm))
})

test_that("validate_triangle returns class 'TriangleValidation' with no gaps", {
  res <- validate_triangle(experience, group_var = cv_nm)
  expect_s3_class(res, "TriangleValidation")
  expect_equal(nrow(res), 0L)
})

test_that("build_triangle errors when group_var is invalid", {
  expect_error(build_triangle(exp, group_var = nonexistent_col),
               regexp = "Unknown column")
})

test_that("summary.Calendar returns CalendarSummary with expected columns", {
  cal <- build_calendar(exp, group_var = cv_nm)
  s   <- summary(cal)
  expect_s3_class(s, "CalendarSummary")
  expected <- c("calendar", "n_obs",
                "lr_mean", "lr_median", "lr_wt",
                "clr_mean", "clr_median", "clr_wt")
  expect_true(all(expected %in% names(s)))
  expect_equal(attr(s, "group_var"),    "cv_nm")
  expect_equal(attr(s, "calendar_var"), "cym")
  expect_false(inherits(s, "Calendar"))
})

test_that("summary.Calendar row count matches (group, calendar) cells", {
  cal <- build_calendar(exp, group_var = cv_nm)
  s   <- summary(cal)
  expect_equal(nrow(s), nrow(unique(cal[, .(cv_nm, calendar)])))
})

test_that("summary.Total returns TotalSummary ordered by descending lr", {
  tot <- build_total(exp, group_var = cv_nm)
  s   <- summary(tot)
  expect_s3_class(s, "TotalSummary")
  expect_false(inherits(s, "Total"))
  expect_equal(nrow(s), nrow(tot))
  expect_true(all(diff(s$lr) <= 0))
  expect_equal(attr(s, "group_var"), "cv_nm")
})

test_that("summary.Total honors digits = NULL (no rounding)", {
  tot <- build_total(exp, group_var = cv_nm)
  s_round <- summary(tot, digits = 2L)
  s_raw   <- summary(tot, digits = NULL)
  expect_true(all(s_round$lr == round(s_round$lr, 2L)))
  expect_true(any(s_raw$lr != round(s_raw$lr, 2L)) ||
              all(s_raw$lr == round(s_raw$lr, 6L)))
})

test_that("plot.Total returns a ggplot for default value_var = 'lr'", {
  tot <- build_total(exp, group_var = cv_nm)
  expect_no_error(p <- plot(tot))
  expect_s3_class(p, "ggplot")
})

test_that("plot.Total errors on unknown value_var", {
  tot <- build_total(exp, group_var = cv_nm)
  expect_error(plot(tot, value_var = "nope"))
})
