# Edge-case coverage: empty inputs, single cohort/group, NA loss,
# alternative granularities. The goal is to lock in current behaviour
# (whether that is silent zero-row pass-through or hard error).

# Empty input ---------------------------------------------------------------

test_that("as_experience accepts a zero-row data.frame and returns Experience", {
  data(experience)
  empty <- experience[0L, ]
  expect_no_error(out <- as_experience(empty))
  expect_s3_class(out, "Experience")
  expect_equal(nrow(out), 0L)
})

test_that("build_triangle accepts a zero-row Experience and returns empty Triangle", {
  exp_empty <- as_experience(make_exp()[0L, ])
  expect_no_error(tri <- build_triangle(exp_empty, group_var = cv_nm))
  expect_s3_class(tri, "Triangle")
  expect_equal(nrow(tri), 0L)
  expect_true(all(c("cohort", "dev", "loss", "rp") %in% names(tri)))
})

# Single cohort -------------------------------------------------------------

test_that("build_triangle on a single cohort succeeds", {
  exp <- make_exp()
  single <- exp[uym == as.Date("2023-04-01")]
  tri <- build_triangle(single, group_var = cv_nm)
  expect_s3_class(tri, "Triangle")
  expect_equal(data.table::uniqueN(tri$cohort), 1L)
  expect_gt(nrow(tri), 0L)
})

test_that("build_ata on a single cohort returns ATA with valid links", {
  exp <- make_exp()
  single <- exp[uym == as.Date("2023-04-01")]
  tri <- build_triangle(single, group_var = cv_nm)
  ata <- build_ata(tri, value_var = "closs")
  expect_s3_class(ata, "ATA")
  expect_true(all(ata$ata_to == ata$ata_from + 1L))
})

# Single group --------------------------------------------------------------

test_that("build_triangle on a single group succeeds", {
  exp <- make_exp()
  one_grp <- exp[cv_nm == "SUR"]
  tri <- build_triangle(one_grp, group_var = cv_nm)
  expect_s3_class(tri, "Triangle")
  expect_equal(unique(tri$cv_nm), "SUR")
})

test_that("summary.Triangle on a single group returns one row per dev", {
  tri <- make_sub_tri("SUR")
  sm  <- summary(tri)
  expect_s3_class(sm, "TriangleSummary")
  expect_equal(nrow(sm), data.table::uniqueN(tri$dev))
})

test_that("fit_cl runs on a single-group triangle", {
  tri <- make_sub_tri("SUR")
  expect_no_error(cl <- fit_cl(tri, value_var = "closs", method = "mack"))
  expect_s3_class(cl, "CLFit")
})

# NA loss propagation -------------------------------------------------------

test_that("build_triangle propagates NA loss without erroring", {
  exp <- make_exp()
  exp_na <- data.table::copy(exp)
  exp_na[1:50, loss := NA_real_]
  expect_no_error(tri <- build_triangle(exp_na, group_var = cv_nm))
  expect_s3_class(tri, "Triangle")
  # at least some NAs survive aggregation
  expect_true(anyNA(tri$loss))
})

test_that("as_experience refuses NA in required columns after coercion", {
  raw <- as.data.frame(make_exp())
  raw$loss[1:5] <- NA
  expect_error(as_experience(raw),
               regexp = "loss.*coerced.*numeric")
})

# Granularity (quarter / half / year) --------------------------------------

test_that("build_triangle with cohort_var = 'uyq' / dev_var = 'elap_q' succeeds", {
  exp <- make_exp()
  skip_if_not("elap_q" %in% names(exp), "elap_q not present in experience")
  tri_q <- build_triangle(exp, group_var = cv_nm,
                          cohort_var = "uyq", dev_var = "elap_q")
  expect_s3_class(tri_q, "Triangle")
  expect_identical(attr(tri_q, "cohort_var"), "uyq")
  expect_identical(attr(tri_q, "cohort_type"), "quarter")
  expect_identical(attr(tri_q, "dev_var"), "elap_q")
  # dev_type from raw elap_* is NA (.get_period_type only knows uy*/cy*).
  expect_true(is.na(attr(tri_q, "dev_type")))
  expect_gt(nrow(tri_q), 0L)
})

test_that("build_triangle with cohort_var = 'uyh' / dev_var = 'elap_h' succeeds", {
  exp <- make_exp()
  skip_if_not("elap_h" %in% names(exp), "elap_h not present in experience")
  tri_h <- build_triangle(exp, group_var = cv_nm,
                          cohort_var = "uyh", dev_var = "elap_h")
  expect_s3_class(tri_h, "Triangle")
  expect_identical(attr(tri_h, "cohort_type"), "half")
  expect_true(is.na(attr(tri_h, "dev_type")))
  expect_gt(nrow(tri_h), 0L)
})

test_that("build_triangle with cohort_var = 'uy' / dev_var = 'elap_y' succeeds", {
  exp <- make_exp()
  skip_if_not("elap_y" %in% names(exp), "elap_y not present in experience")
  tri_y <- build_triangle(exp, group_var = cv_nm,
                          cohort_var = "uy", dev_var = "elap_y")
  expect_s3_class(tri_y, "Triangle")
  expect_identical(attr(tri_y, "cohort_type"), "year")
  expect_true(is.na(attr(tri_y, "dev_type")))
  expect_gt(nrow(tri_y), 0L)
})

test_that("build_triangle errors on mismatched granularity (uym + elap_q)", {
  exp <- make_exp()
  skip_if_not("elap_q" %in% names(exp), "elap_q not present in experience")
  expect_error(
    build_triangle(exp, group_var = cv_nm,
                   cohort_var = "uym", dev_var = "elap_q"),
    regexp = "granularity"
  )
})

test_that("build_calendar with calendar_var = 'cyq' returns Calendar quarter", {
  exp <- make_exp()
  skip_if_not("cyq" %in% names(exp), "cyq not present in experience")
  cal_q <- build_calendar(exp, group_var = cv_nm, calendar_var = "cyq")
  expect_s3_class(cal_q, "Calendar")
  expect_identical(attr(cal_q, "calendar_type"), "quarter")
  expect_gt(nrow(cal_q), 0L)
})
