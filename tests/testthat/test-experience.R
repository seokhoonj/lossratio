data(experience, package = "lossratio")

test_that("check_experience accepts valid input", {
  expect_no_error(check_experience(experience))
})

test_that("as_experience errors when required columns are missing", {
  broken <- as.data.frame(experience)[, setdiff(names(experience), "cym")]
  expect_error(as_experience(broken),
               regexp = "Missing columns.*'cym'")

  broken2 <- as.data.frame(experience)[, setdiff(names(experience),
                                                 c("uym", "loss", "rp"))]
  expect_error(as_experience(broken2),
               regexp = "Missing columns.*'uym'.*'loss'.*'rp'")
})

test_that("as_experience returns an object inheriting class 'Experience'", {
  exp <- as_experience(experience)
  expect_s3_class(exp, "Experience")
})

test_that("is_experience distinguishes coerced from raw input", {
  expect_true(is_experience(as_experience(experience)))
  expect_false(is_experience(experience))
})

test_that("as_experience coerces date columns to Date class", {
  raw <- as.data.frame(experience)
  raw$cym  <- as.character(raw$cym)
  raw$uym  <- as.character(raw$uym)
  exp <- as_experience(raw)
  expect_s3_class(exp$cym, "Date")
  expect_s3_class(exp$uym, "Date")
})

test_that("add_experience_period adds expected period columns", {
  base <- data.frame(
    uym  = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01")),
    cym  = as.Date(c("2023-03-01", "2023-06-01", "2023-09-01")),
    elap_m = c(3L, 3L, 3L)
  )
  out <- add_experience_period(base)
  expected <- c("uy", "uyh", "uyq", "cy", "cyh", "cyq",
                "elap_y", "elap_h", "elap_q")
  expect_true(all(expected %in% names(out)))
  expect_s3_class(out$uy,  "Date")
  expect_s3_class(out$cy,  "Date")
  expect_type(out$elap_y, "integer")
})

test_that("add_experience_period derives elap_m when missing", {
  base <- data.frame(
    uym = as.Date("2023-01-01"),
    cym = as.Date("2023-04-01")
  )
  out <- add_experience_period(base)
  expect_true("elap_m" %in% names(out))
  expect_equal(out$elap_m, 4L)
})
