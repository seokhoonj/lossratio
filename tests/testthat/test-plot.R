# Setup — full pipeline objects for plot dispatch tests
data(experience)
exp  <- as_experience(experience)
tri  <- build_triangle(exp, group_var = cv_nm)
cal  <- build_calendar(exp, group_var = cv_nm)
ata  <- build_ata(tri, value_var = "closs")
af   <- fit_ata(ata)
ed   <- build_ed(tri)
ef   <- fit_ed(ed)
cl_b <- fit_cl(tri, value_var = "closs", method = "basic")
cl_m <- fit_cl(tri, value_var = "closs", method = "mack")
lr   <- fit_lr(tri, method = "sa")
sub  <- build_triangle(exp[cv_nm == "SUR"], group_var = cv_nm)
reg  <- detect_cohort_regime(sub, K = 12, method = "ecp")

is_plot <- function(x) inherits(x, "ggplot") || inherits(x, "gtable")

# plot.<class> -----------------------------------------------------------

test_that("plot.triangle dispatches", {
  expect_true(is_plot(suppressWarnings(plot(tri))))
  expect_true(is_plot(suppressWarnings(plot(tri, value_var = "loss"))))
  expect_true(is_plot(suppressWarnings(plot(tri, summary = TRUE))))
})

test_that("plot.calendar dispatches", {
  expect_true(is_plot(suppressWarnings(plot(cal))))
  expect_true(is_plot(suppressWarnings(plot(cal, x_by = "dev"))))
})

test_that("plot.ata dispatches across types", {
  for (tp in c("cv", "rse", "summary", "box", "point")) {
    p <- suppressWarnings(plot(ata, type = tp))
    expect_true(is_plot(p), info = paste("type =", tp))
  }
})

test_that("plot.ata_fit dispatches", {
  expect_true(is_plot(suppressWarnings(plot(af))))
})

test_that("plot.ed dispatches across types", {
  for (tp in c("summary", "box", "point")) {
    p <- suppressWarnings(plot(ed, type = tp))
    expect_true(is_plot(p), info = paste("type =", tp))
  }
})

test_that("plot.ed_fit dispatches", {
  expect_true(is_plot(suppressWarnings(plot(ef))))
})

test_that("plot.cl_fit dispatches (basic, projection only)", {
  expect_true(is_plot(suppressWarnings(plot(cl_b, type = "projection"))))
})

test_that("plot.cl_fit dispatches (mack, both types)", {
  expect_true(is_plot(suppressWarnings(plot(cl_m, type = "projection"))))
  expect_true(is_plot(suppressWarnings(plot(cl_m, type = "reserve"))))
})

test_that("plot.lr_fit dispatches across types", {
  expect_true(is_plot(suppressWarnings(plot(lr, type = "clr"))))
  expect_true(is_plot(suppressWarnings(plot(lr, type = "closs"))))
})

test_that("plot.cohort_regime dispatches", {
  expect_true(is_plot(suppressWarnings(plot(reg))))
})

# plot_triangle.<class> --------------------------------------------------

test_that("plot_triangle.triangle dispatches", {
  expect_true(is_plot(suppressWarnings(plot_triangle(tri))))
  expect_true(is_plot(suppressWarnings(plot_triangle(tri, label_style = "detail"))))
})

test_that("plot_triangle.ata dispatches", {
  expect_true(is_plot(suppressWarnings(plot_triangle(ata))))
  expect_true(is_plot(suppressWarnings(plot_triangle(ata, show_maturity = TRUE))))
})

test_that("plot_triangle.ata_fit dispatches", {
  expect_true(is_plot(suppressWarnings(plot_triangle(af))))
})

test_that("plot_triangle.ed dispatches", {
  expect_true(is_plot(suppressWarnings(plot_triangle(ed))))
})

test_that("plot_triangle.ed_fit dispatches", {
  expect_true(is_plot(suppressWarnings(plot_triangle(ef))))
})

test_that("plot_triangle.cl_fit dispatches across what variants", {
  for (w in c("pred", "full", "data")) {
    p <- suppressWarnings(plot_triangle(cl_m, what = w))
    expect_true(is_plot(p), info = paste("what =", w))
  }
})

test_that("plot_triangle.lr_fit dispatches", {
  expect_true(is_plot(suppressWarnings(plot_triangle(lr, what = "pred"))))
  expect_true(is_plot(suppressWarnings(plot_triangle(lr, what = "full"))))
})
