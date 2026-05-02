# Triangle validation -----------------------------------------------------

#' Validate triangle structure before building a development
#'
#' @description
#' Check that each `(group_var, cohort_var)` cohort has a consecutive
#' `dev_var` sequence within its observed range. Non-consecutive
#' cohorts produce non-consecutive age-to-age links downstream (e.g.,
#' `14 -> 17` instead of `14 -> 15`), which breaks
#' [summary_ata()] / [summary_ed()] key uniqueness and causes cartesian
#' joins in [fit_lr()].
#'
#' This function inspects the raw data without modifying it. Use it
#' before [build_triangle()] to decide whether to fix the data source, drop
#' offending cohorts, or pass `fill_gaps = TRUE` to [build_triangle()].
#'
#' @param df A data.frame.
#' @param group_var Grouping variable(s).
#' @param cohort_var A single cohort variable. Default `"uym"`.
#' @param dev_var A single development variable. Default `"elap_m"`.
#'
#' @return A `data.table` of class `"triangle_validation"` with one row
#'   per cohort containing gaps. Columns:
#'   \describe{
#'     \item{group_var(s), cohort_var}{Cohort identifier.}
#'     \item{`n_observed`}{Number of distinct observed `dev_var`
#'       values.}
#'     \item{`n_expected`}{`max(elap_m) - min(elap_m) + 1` for that cohort.}
#'     \item{`missing`}{List column of missing `dev_var` values.}
#'   }
#'   Returns a zero-row data.table when no gaps are found.
#'
#' @seealso [build_triangle()]
#'
#' @export
validate_triangle <- function(df,
                              group_var,
                              cohort_var  = "uym",
                              dev_var = "elap_m") {
  .assert_class(df, "data.frame")

  dt <- .ensure_dt(df)

  grp_var <- .capture_names(dt, !!rlang::enquo(group_var))
  coh_var <- .capture_names(dt, !!rlang::enquo(cohort_var))
  dev_var <- .capture_names(dt, !!rlang::enquo(dev_var))

  .assert_length(coh_var)
  .assert_length(dev_var)

  .validate_triangle_impl(dt, grp_var, coh_var, dev_var)
}


.validate_calendar_impl <- function(dt, grp_var, cal_var) {
  period_type <- .get_period_type(cal_var)
  step <- switch(period_type,
                 month    = "month",
                 quarter  = "3 months",
                 half     = "6 months",
                 year     = "year",
                 NA_character_)

  if (is.na(step)) {
    z <- data.table::data.table()
    return(.prepend_class(z, "calendar_validation"))
  }

  .row <- function(p) {
    p <- sort(unique(p[!is.na(p)]))
    if (length(p) == 0L) {
      return(list(n_observed = 0L, n_expected = 0L,
                  missing = list(as.Date(integer(0)))))
    }
    exp_seq <- seq(min(p), max(p), by = step)
    miss    <- setdiff(exp_seq, p)
    list(
      n_observed = length(p),
      n_expected = length(exp_seq),
      missing    = list(as.Date(miss, origin = "1970-01-01"))
    )
  }

  if (length(grp_var)) {
    gaps <- dt[, .row(.SD[[1L]]), by = grp_var, .SDcols = cal_var]
  } else {
    r <- .row(dt[[cal_var]])
    gaps <- data.table::data.table(
      n_observed = r$n_observed,
      n_expected = r$n_expected,
      missing    = r$missing
    )
  }

  gaps <- gaps[n_observed != n_expected]

  data.table::setattr(gaps, "group_var" , grp_var)
  data.table::setattr(gaps, "cohort_var", cal_var)

  .prepend_class(gaps, "calendar_validation")
}


.validate_triangle_impl <- function(dt, grp_var, coh_var, dev_var) {
  grp_coh <- c(grp_var, coh_var)

  gaps <- dt[, {
    e <- .SD[[1L]]
    e <- e[!is.na(e)]
    if (length(e) == 0L) {
      list(n_observed = 0L, n_expected = 0L, missing = list(integer(0)))
    } else {
      rng  <- seq.int(min(e), max(e))
      miss <- setdiff(rng, e)
      list(
        n_observed = length(unique(e)),
        n_expected = length(rng),
        missing    = list(miss)
      )
    }
  }, by = grp_coh, .SDcols = dev_var]

  gaps <- gaps[n_observed != n_expected]

  data.table::setattr(gaps, "group_var"  , grp_var)
  data.table::setattr(gaps, "cohort_var" , coh_var)
  data.table::setattr(gaps, "dev_var", dev_var)

  .prepend_class(gaps, "triangle_validation")
}


# Development -------------------------------------------------------------

#' Build a development structure from experience data
#'
#' @description
#' Aggregate experience data into a development structure by grouping,
#' period, and development-period variables. The result contains:
#' - cumulative loss and cumulative risk premium,
#' - period and cumulative proportions,
#' - margin and cumulative margin,
#' - profit indicators,
#' - loss ratio (`lr = loss / rp`) and cumulative loss ratio (`clr = closs / crp`).
#'
#' The loss ratio is defined as:
#' \deqn{lr = loss / rp}
#'
#' where `rp` represents risk premium (expected loss), not written premium.
#'
#' Proportion variables are computed within each `cohort_var + dev_var` cell:
#' \itemize{
#'   \item `loss_prop  = loss  / sum(loss)`
#'   \item `rp_prop    = rp    / sum(rp)`
#'   \item `closs_prop = closs / sum(closs)`
#'   \item `crp_prop   = crp   / sum(crp)`
#' }
#'
#' Therefore, for a fixed `(cohort_var, dev_var)` cell, the proportions
#' sum to 1 across groups. These are useful for examining the composition of
#' each development cell across products or other grouping variables.
#'
#' @param df A data.frame containing experience data with loss and risk premium.
#' @param group_var Column(s) used for grouping (e.g., product, gender).
#' @param value_var Character vector specifying the value columns, typically
#'   `c("loss", "rp")` where:
#'   \itemize{
#'     \item `loss` = actual claims (observed loss),
#'     \item `rp`   = risk premium (expected loss).
#'   }
#' @param cohort_var Column(s) defining the exposure period
#'   (e.g., underwriting year-month, quarter, half-year, or year such as
#'   `uym`, `uyq`, `uyh`, `uy`).
#' @param dev_var Column(s) defining development periods
#'   (e.g., months since issue such as `elap_m`).
#' @param fill_gaps Logical; if `TRUE`, zero-fill missing
#'   `(group_var, cohort_var, dev_var)` cells so that every cohort
#'   has a consecutive `dev_var` sequence. Default `FALSE`, which
#'   raises an error when gaps are detected. Use
#'   [validate_triangle()] to inspect gaps before deciding.
#'
#' @return A data.frame with class `"triangle"`, containing the following
#'   derived columns:
#'   \describe{
#'     \item{n_obs}{Number of distinct periods observed}
#'     \item{closs, crp}{Cumulative loss and cumulative risk premium}
#'     \item{loss_prop, rp_prop}{Period proportions within each `(period, dev)` cell}
#'     \item{closs_prop, crp_prop}{Cumulative proportions within each `(period, dev)` cell}
#'     \item{margin, cmargin}{Period and cumulative margin (`rp - loss`)}
#'     \item{profit, cprofit}{Profit indicator (factor `"pos"` / `"neg"`)}
#'     \item{lr}{Loss ratio (`loss / rp`)}
#'     \item{clr}{Cumulative loss ratio (`closs / crp`)}
#'   }
#'
#' The returned object also has an attribute `"longer"` containing
#' a melted long-format version (`class = "triangle_longer"`).
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   pd_cd    = rep(c("P001", "P002"), each = 6),
#'   pd_nm    = rep(c("cancer", "health"), each = 6),
#'   uym      = rep(as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")), 4),
#'   elap_m     = rep(1:2, 6),
#'   loss     = runif(12, 80, 120),
#'   rp       = runif(12, 90, 110),
#'   n_policy = sample(50:100, 12, replace = TRUE)
#' )
#'
#' res <- build_triangle(
#'   df,
#'   group_var   = pd_cd,
#'   value_var   = c("loss", "rp", "n_policy"),
#'   cohort_var = "uym",
#'   dev_var = "elap_m"
#' )
#'
#' head(res)
#' attr(res, "longer")
#' }
#'
#' @export
build_triangle <- function(df,
                           group_var,
                           cohort_var  = "uym",
                           dev_var = "elap_m",
                           value_var   = c("loss", "rp"),
                           fill_gaps   = FALSE) {
  .assert_class(df, "data.frame")

  if (!is.logical(fill_gaps) || length(fill_gaps) != 1L || is.na(fill_gaps))
    stop("`fill_gaps` must be a single non-missing logical value.",
         call. = FALSE)

  dt <- .ensure_dt(df)

  grp_var <- .capture_names(dt, !!rlang::enquo(group_var))
  coh_var <- .capture_names(dt, !!rlang::enquo(cohort_var))
  dev_var <- .capture_names(dt, !!rlang::enquo(dev_var))
  val_var <- .capture_names(dt, !!rlang::enquo(value_var))

  if (!all(c("loss", "rp") %in% val_var))
    stop("`value_var` must include both 'loss' and 'rp'.", call. = FALSE)

  .assert_length(coh_var)
  .assert_length(dev_var)
  .assert_length(val_var)

  coh_gran <- .get_granularity(coh_var)
  dev_gran <- .get_granularity(dev_var)
  if (!is.na(coh_gran) && !is.na(dev_gran) && coh_gran != dev_gran)
    stop(sprintf(
      "`cohort_var` and `dev_var` must share the same granularity; got `%s` (%s) and `%s` (%s).",
      coh_var, coh_gran, dev_var, dev_gran
    ), call. = FALSE)

  # standardize column names early
  data.table::setnames(dt, c(coh_var, dev_var), c("cohort", "dev"))

  grp_coh_var     <- c(grp_var, "cohort")
  grp_dev_var     <- c(grp_var, "dev")
  grp_coh_dev_var <- c(grp_var, "cohort", "dev")
  coh_dev_var     <- c("cohort", "dev")

  # count observed periods
  dn <- dt[, .(n_obs = data.table::uniqueN(cohort)),
           by = grp_dev_var]

  # aggregate values
  ds <- dt[, lapply(.SD, sum),
           by = grp_coh_dev_var, .SDcols = val_var]

  # validate / fill dev gaps per (grp, cohort). Downstream
  # build_ata / build_ed require consecutive (k, k+1) transitions per
  # cohort; non-consecutive dev produces duplicate (grp, ata_from)
  # keys in summary tables and cartesian joins in fit_lr.
  gaps <- .validate_triangle_impl(ds, grp_var, "cohort", "dev")
  if (nrow(gaps)) {
    if (fill_gaps) {
      grid <- ds[, .(dev = seq.int(min(dev, na.rm = TRUE),
                                       max(dev, na.rm = TRUE))),
                 by = grp_coh_var]
      ds <- ds[grid, on = c(grp_coh_var, "dev")]
      for (v in val_var) {
        data.table::set(ds, i = which(is.na(ds[[v]])), j = v, value = 0)
      }
      data.table::setorderv(ds, c(grp_coh_var, "dev"))
    } else {
      stop(
        sprintf(
          "Non-consecutive `dev` (source `%s`) detected in %d cohort(s). %s\n%s",
          dev_var,
          nrow(gaps),
          "Call `validate_triangle()` to inspect, or pass `fill_gaps = TRUE` to zero-fill.",
          paste(utils::capture.output(print(head(gaps, 5L))), collapse = "\n")
        ),
        call. = FALSE
      )
    }
  }

  # join n_obs
  n_obs <- i.n_obs <- NULL
  ds[dn, on = grp_dev_var, n_obs := i.n_obs]
  data.table::setcolorder(ds, "n_obs", before = "cohort")

  # cumulative values
  c_val_var <- paste0("c", val_var)
  ds[, (c_val_var) := lapply(.SD, cumsum),
     by = grp_coh_var, .SDcols = val_var]

  # margin
  data.table::set(ds, j = "margin" , value = ds[["rp"]]  - ds[["loss"]])
  data.table::set(ds, j = "cmargin", value = ds[["crp"]] - ds[["closs"]])

  # profit indicators
  data.table::set(
    ds,
    j     = "profit",
    value = factor(
      ifelse(ds[["margin"]] >= 0, "pos", "neg"),
      levels = c("pos", "neg")
    )
  )

  data.table::set(
    ds,
    j     = "cprofit",
    value = factor(
      ifelse(ds[["cmargin"]] >= 0, "pos", "neg"),
      levels = c("pos", "neg")
    )
  )

  # loss ratios
  data.table::set(ds, j = "lr" , value = ds[["loss"]]  / ds[["rp"]])
  data.table::set(ds, j = "clr", value = ds[["closs"]] / ds[["crp"]])

  # proportions within each cohort + dev cell
  ds[, loss_prop  := loss  / sum(loss),  by = coh_dev_var]
  ds[, rp_prop    := rp    / sum(rp),    by = coh_dev_var]
  ds[, closs_prop := closs / sum(closs), by = coh_dev_var]
  ds[, crp_prop   := crp   / sum(crp),   by = coh_dev_var]

  # long format
  dm <- data.table::melt(
    data         = ds,
    id.vars      = grp_coh_dev_var,
    measure.vars = c("closs", "crp")
  )
  dm <- .prepend_class(dm, "triangle_longer")

  data.table::setattr(ds, "group_var"   , grp_var)
  data.table::setattr(ds, "cohort_var"  , coh_var)
  data.table::setattr(ds, "cohort_type" , .get_period_type(coh_var))
  data.table::setattr(ds, "dev_var" , dev_var)
  data.table::setattr(ds, "dev_type", .get_period_type(dev_var))
  data.table::setattr(ds, "longer"      , dm)

  .update_class(ds, "experience", "triangle")
}

#' Build a calendar-based development structure from experience data
#'
#' @description
#' Aggregate experience data into a development structure along a single
#' calendar-style period axis, including:
#' - cumulative loss and cumulative risk premium,
#' - period and cumulative proportions,
#' - margin and cumulative margin,
#' - profit indicators,
#' - loss ratio (`lr = loss / rp`) and cumulative loss ratio (`clr = closs / crp`).
#'
#' In contrast to [build_triangle()], which builds a development structure using
#' `calendar_var × dev_var`, this function aggregates values over
#' a one-dimensional calendar axis.
#'
#' The loss ratio is defined as:
#' \deqn{lr = loss / rp}
#'
#' where `rp` represents risk premium (expected loss), not written premium.
#'
#' Proportion variables are computed within each `calendar_var` cell:
#' \itemize{
#'   \item `loss_prop  = loss  / sum(loss)`
#'   \item `rp_prop    = rp    / sum(rp)`
#'   \item `closs_prop = closs / sum(closs)`
#'   \item `crp_prop   = crp   / sum(crp)`
#' }
#'
#' Therefore, for a fixed `calendar_var` cell, the proportions
#' sum to 1 across groups. These are useful for examining the composition of
#' each calendar period across products or other grouping variables.
#'
#' @param df A data.frame containing experience data with loss and risk premium.
#' @param group_var Column(s) used for grouping (e.g., product, gender).
#' @param value_var Character vector specifying the value columns, typically
#'   `c("loss", "rp")` where:
#'   \itemize{
#'     \item `loss` = actual claims (observed loss),
#'     \item `rp`   = risk premium (expected loss).
#'   }
#' @param calendar_var A single calendar-like period variable defining
#'   the summary axis. Typical examples include:
#'   \itemize{
#'     \item `cym` (calendar year-month),
#'     \item `cyq` (calendar year-quarter),
#'     \item `cyh` (calendar year-half),
#'     \item `cy`  (calendar year),
#'     \item `uym`, `uyq`, `uyh`, `uy` when a single underwriting-period axis
#'       is to be summarised as a time series rather than as a development
#'       structure.
#'   }
#' @param period_from Optional lower bound for `calendar_var`. Only rows with
#'   `calendar_var >= period_from` are kept.
#' @param period_to Optional upper bound for `calendar_var`. Only rows with
#'   `calendar_var <= period_to` are kept.
#' @param fill_gaps Logical; if `TRUE`, zero-fill missing
#'   `(group_var, calendar_var)` cells so every group has a consecutive
#'   calendar sequence (monthly, quarterly, etc. based on `calendar_var`).
#'   Default `FALSE`, which raises an error when gaps are detected.
#'
#' @return A data.frame with class `"calendar"`, containing the following
#'   derived columns:
#'   \describe{
#'     \item{index}{Calendar index within each group, defined as the sequential
#'       order of `calendar_var` after sorting in ascending order. This represents
#'       the progression of calendar periods for each group (e.g., 1 = first
#'       observed period, 2 = second, ...), and can be used to align groups with
#'       different starting periods on a common index scale.}
#'     \item{closs, crp}{Cumulative loss and cumulative risk premium}
#'     \item{loss_prop, rp_prop}{Period proportions within each `calendar_var` cell}
#'     \item{closs_prop, crp_prop}{Cumulative proportions within each `calendar_var` cell}
#'     \item{margin, cmargin}{Period and cumulative margin (`rp - loss`)}
#'     \item{profit, cprofit}{Profit indicator (factor `"pos"` / `"neg"`)}
#'     \item{lr}{Loss ratio (`loss / rp`)}
#'     \item{clr}{Cumulative loss ratio (`closs / crp`)}
#'   }
#'
#' The returned object also has an attribute `"longer"` containing
#' a melted long-format version (`class = "calendar_longer"`).
#'
#' @examples
#' \dontrun{
#' res1 <- build_calendar(
#'   df,
#'   group_var  = pd_cd,
#'   calendar_var = "cym"
#' )
#'
#' res2 <- build_calendar(
#'   df,
#'   group_var   = pd_cd,
#'   calendar_var = "cyq",
#'   period_from = "2023-01-01"
#' )
#'
#' head(res1)
#' attr(res1, "longer")
#' }
#'
#' @export
build_calendar <- function(df,
                           group_var,
                           calendar_var = "cym",
                           value_var    = c("loss", "rp"),
                           period_from  = NULL,
                           period_to    = NULL,
                           fill_gaps    = FALSE) {
  .assert_class(df, "data.frame")

  if (!is.logical(fill_gaps) || length(fill_gaps) != 1L || is.na(fill_gaps))
    stop("`fill_gaps` must be a single non-missing logical value.",
         call. = FALSE)

  dt <- .ensure_dt(df)

  grp_var <- .capture_names(dt, !!rlang::enquo(group_var))
  cal_var <- .capture_names(dt, !!rlang::enquo(calendar_var))
  val_var <- .capture_names(dt, !!rlang::enquo(value_var))

  if (!all(c("loss", "rp") %in% val_var))
    stop("`value_var` must include both 'loss' and 'rp'.", call. = FALSE)

  .assert_length(cal_var)
  .assert_length(val_var)

  cal_type <- .get_period_type(cal_var)

  if (!is.null(period_from)) {
    period_from <- as.Date(period_from)
    dt <- dt[dt[[cal_var]] >= period_from]
  }

  if (!is.null(period_to)) {
    period_to <- as.Date(period_to)
    dt <- dt[dt[[cal_var]] <= period_to]
  }

  # standardize column name early
  data.table::setnames(dt, cal_var, "calendar")

  grp_cal_var <- c(grp_var, "calendar")

  # aggregate values
  ds <- dt[, lapply(.SD, sum),
           by = grp_cal_var, .SDcols = val_var]

  # validate / fill calendar period consecutiveness per group
  gaps <- .validate_calendar_impl(ds, grp_var, "calendar")
  if (nrow(gaps)) {
    if (fill_gaps) {
      step <- switch(cal_type,
                     month    = "month",
                     quarter  = "3 months",
                     half     = "6 months",
                     year     = "year")
      if (length(grp_var)) {
        grid <- ds[, .(calendar = seq(min(calendar, na.rm = TRUE),
                                      max(calendar, na.rm = TRUE),
                                      by = step)),
                   by = grp_var]
      } else {
        grid <- data.table::data.table(
          calendar = seq(min(ds$calendar, na.rm = TRUE),
                         max(ds$calendar, na.rm = TRUE),
                         by = step)
        )
      }
      ds <- ds[grid, on = grp_cal_var]
      for (v in val_var) {
        data.table::set(ds, i = which(is.na(ds[[v]])), j = v, value = 0)
      }
    } else {
      stop(
        sprintf(
          "Non-consecutive `calendar` (source `%s`) detected in %d group(s). %s\n%s",
          cal_var,
          nrow(gaps),
          "Inspect gaps manually or pass `fill_gaps = TRUE` to zero-fill.",
          paste(utils::capture.output(print(head(gaps, 5L))), collapse = "\n")
        ),
        call. = FALSE
      )
    }
  }

  data.table::setorderv(ds, c(grp_var, "calendar"))

  # sequential dev index per group
  if (length(grp_var)) {
    ds[, dev := seq_len(.N), by = grp_var]
  } else {
    ds[, dev := seq_len(.N)]
  }

  data.table::setcolorder(ds, "dev", after = "calendar")

  c_val_var <- paste0("c", val_var)
  if (length(grp_var)) {
    ds[, (c_val_var) := lapply(.SD, cumsum),
       by = grp_var, .SDcols = val_var]
  } else {
    ds[, (c_val_var) := lapply(.SD, cumsum), .SDcols = val_var]
  }

  # margin
  data.table::set(ds, j = "margin" , value = ds[["rp"]]  - ds[["loss"]])
  data.table::set(ds, j = "cmargin", value = ds[["crp"]] - ds[["closs"]])

  # profit indicators
  data.table::set(
    ds,
    j     = "profit",
    value = factor(
      ifelse(ds[["margin"]] >= 0, "pos", "neg"),
      levels = c("pos", "neg")
    )
  )

  data.table::set(
    ds,
    j     = "cprofit",
    value = factor(
      ifelse(ds[["cmargin"]] >= 0, "pos", "neg"),
      levels = c("pos", "neg")
    )
  )

  # loss ratios
  data.table::set(ds, j = "lr" , value = ds[["loss"]]  / ds[["rp"]])
  data.table::set(ds, j = "clr", value = ds[["closs"]] / ds[["crp"]])

  # proportions within each calendar cell
  ds[, loss_prop  := loss  / sum(loss),  by = "calendar"]
  ds[, rp_prop    := rp    / sum(rp),    by = "calendar"]
  ds[, closs_prop := closs / sum(closs), by = "calendar"]
  ds[, crp_prop   := crp   / sum(crp),   by = "calendar"]

  # long format
  dm <- data.table::melt(
    data         = ds,
    id.vars      = c(grp_cal_var, "dev"),
    measure.vars = c("closs", "crp")
  )
  dm <- .prepend_class(dm, "calendar_longer")

  data.table::setattr(ds, "group_var"    , grp_var)
  data.table::setattr(ds, "calendar_var" , cal_var)
  data.table::setattr(ds, "calendar_type", cal_type)
  data.table::setattr(ds, "longer"       , dm)

  .prepend_class(ds, "calendar")
}

#' Build a total development summary from experience data
#'
#' @description
#' Aggregate `loss` and `rp` by group and compute the corresponding total
#' loss ratio over a selected period window.
#'
#' This function is intended for high-level portfolio comparison across
#' groups such as products, coverages, or channels. It summarises:
#' \itemize{
#'   \item the number of observed periods (`n_obs`)
#'   \item the first and last observed periods (`sales_start`, `sales_end`)
#'   \item total `loss` and total `rp`
#'   \item total loss ratio (`lr = loss / rp`)
#'   \item each group's share of total loss and risk premium
#' }
#'
#' If `period_from` and/or `period_to` are supplied, the input data are first
#' restricted to that period window before aggregation. This is useful when
#' comparing groups on a common period basis.
#'
#' @param df A data.frame containing experience data.
#' @param group_var Grouping variable(s).
#' @param cohort_var A single period variable. This may be an underwriting
#'   period (`uym`, `uyq`, `uyh`, `uy`) or a calendar period
#'   (`cym`, `cyq`, `cyh`, `cy`). Default `"uym"`.
#' @param dev_var A single development variable used to count observed periods.
#'   Default `"elap_m"`.
#' @param value_var Value variables to aggregate. Must include both `"loss"`
#'   and `"rp"`. Default `c("loss", "rp")`.
#' @param period_from Optional lower bound for `cohort_var`. Only rows with
#'   `cohort_var >= period_from` are kept. May be supplied as `Date`,
#'   character, or any value coercible to `Date`. Default `NULL`.
#' @param period_to Optional upper bound for `cohort_var`. Only rows with
#'   `cohort_var <= period_to` are kept. May be supplied as `Date`,
#'   character, or any value coercible to `Date`. Default `NULL`.
#' @param fill_gaps Logical; if `TRUE`, zero-fill missing
#'   `(group_var, cohort_var, dev_var)` cells before aggregation so
#'   that every cohort has a consecutive `dev_var` sequence. Default
#'   `FALSE`. Note that filling inflates `n_obs` (counts filled rows as
#'   observed periods); use [validate_triangle()] to inspect first.
#'
#' @return A data.frame with class `"total"` containing:
#'   \describe{
#'     \item{n_obs}{Number of observed development periods}
#'     \item{sales_start}{First observed period}
#'     \item{sales_end}{Last observed period}
#'     \item{loss}{Total loss}
#'     \item{rp}{Total risk premium}
#'     \item{lr}{Total loss ratio (`loss / rp`)}
#'     \item{loss_prop}{Share of total loss}
#'     \item{rp_prop}{Share of total risk premium}
#'   }
#'
#' @examples
#' \dontrun{
#' build_total(df, cv_nm)
#'
#' build_total(
#'   df,
#'   cv_nm,
#'   period_from = "2023-01-01",
#'   period_to   = "2023-12-01"
#' )
#' }
#'
#' @export
build_total <- function(df,
                        group_var,
                        cohort_var  = "uym",
                        dev_var = "elap_m",
                        value_var   = c("loss", "rp"),
                        period_from = NULL,
                        period_to   = NULL,
                        fill_gaps   = FALSE) {
  .assert_class(df, "data.frame")

  if (!is.logical(fill_gaps) || length(fill_gaps) != 1L || is.na(fill_gaps))
    stop("`fill_gaps` must be a single non-missing logical value.",
         call. = FALSE)

  dt <- .ensure_dt(df)

  grp_var <- .capture_names(dt, !!rlang::enquo(group_var))
  coh_var <- .capture_names(dt, !!rlang::enquo(cohort_var))
  dev_var <- .capture_names(dt, !!rlang::enquo(dev_var))
  val_var <- .capture_names(dt, !!rlang::enquo(value_var))

  if (length(coh_var) != 1L)
    stop("`cohort_var` must resolve to exactly one column.", call. = FALSE)

  if (length(dev_var) != 1L)
    stop("`dev_var` must resolve to exactly one column.", call. = FALSE)

  if (!all(c("loss", "rp") %in% val_var))
    stop("`value_var` must include both 'loss' and 'rp'.", call. = FALSE)

  # filter by cohort range
  if (!is.null(period_from)) {
    period_from <- as.Date(period_from)
    if (is.na(period_from))
      stop("`period_from` must be coercible to `Date`.", call. = FALSE)
    dt <- dt[dt[[coh_var]] >= period_from]
  }

  if (!is.null(period_to)) {
    period_to <- as.Date(period_to)
    if (is.na(period_to))
      stop("`period_to` must be coercible to `Date`.", call. = FALSE)
    dt <- dt[dt[[coh_var]] <= period_to]
  }

  # validate / fill dev gaps per (grp, cohort)
  gaps <- .validate_triangle_impl(dt, grp_var, coh_var, dev_var)
  if (nrow(gaps)) {
    if (fill_gaps) {
      grp_coh_dev <- c(grp_var, coh_var, dev_var)
      agg <- dt[, lapply(.SD, sum),
                by = grp_coh_dev, .SDcols = val_var]
      grid <- agg[, .(.e = seq.int(min(.SD[[1L]], na.rm = TRUE),
                                   max(.SD[[1L]], na.rm = TRUE))),
                  by = c(grp_var, coh_var), .SDcols = dev_var]
      data.table::setnames(grid, ".e", dev_var)
      dt <- agg[grid, on = grp_coh_dev]
      for (v in val_var) {
        data.table::set(dt, i = which(is.na(dt[[v]])), j = v, value = 0)
      }
    } else {
      stop(
        sprintf(
          "Non-consecutive `%s` detected in %d cohort(s). %s\n%s",
          dev_var,
          nrow(gaps),
          "Call `validate_triangle()` to inspect, or pass `fill_gaps = TRUE` to zero-fill.",
          paste(utils::capture.output(print(head(gaps, 5L))), collapse = "\n")
        ),
        call. = FALSE
      )
    }
  }

  # aggregate values
  ds <- dt[, .(
    n_obs       = data.table::uniqueN(.SD[[1L]]),
    sales_start = min(.SD[[2L]]),
    sales_end   = max(.SD[[2L]]),
    loss        = sum(.SD[[3L]]),
    rp          = sum(.SD[[4L]])
  ), by = grp_var, .SDcols = c(dev_var, coh_var, "loss", "rp")]

  # compute total loss ratio and shares
  data.table::set(ds, j = "lr"       , value = ds[["loss"]] / ds[["rp"]])
  data.table::set(ds, j = "loss_prop", value = ds[["loss"]] / sum(ds[["loss"]]))
  data.table::set(ds, j = "rp_prop"  , value = ds[["rp"]]   / sum(ds[["rp"]]))

  .prepend_class(ds, "total")
}

# Development Summary -----------------------------------------------------

#' Summarise development statistics (Mean, Median, Weighted)
#'
#' @description
#' S3 method for `summary()` on `triangle` objects. Computes group-wise summary
#' statistics for loss ratios (`lr`) and cumulative loss ratios (`clr`).
#'
#' The function aggregates data by the grouping variables stored in
#' `attr(x, "group_var")` and the development variable stored in
#' `attr(x, "dev_var")`.
#'
#' The following statistics are computed:
#' - arithmetic mean,
#' - median,
#' - weighted mean (portfolio-level ratio based on sums).
#'
#' @param object An object of class `triangle`.
#' @param ... Unused; included for S3 compatibility.
#'
#' @details
#' The weighted mean is computed as:
#' \itemize{
#'   \item `lr_wt  = sum(loss)  / sum(rp)`
#'   \item `clr_wt = sum(closs) / sum(crp)`
#' }
#'
#' These correspond to portfolio-level loss ratios based on risk premium and
#' are typically more stable than simple averages when exposure sizes differ
#' across cohorts.
#'
#' It is assumed that the input `triangle` object does not contain missing values.
#'
#' @return
#' A `data.table` grouped by `group_var` and `dev_var`, containing:
#' \describe{
#'   \item{n_obs}{Number of observations in the cell}
#'   \item{lr_mean}{Mean of loss ratios}
#'   \item{lr_median}{Median of loss ratios}
#'   \item{lr_wt}{Weighted loss ratio (`sum(loss) / sum(rp)`)}
#'   \item{clr_mean}{Mean of cumulative loss ratios}
#'   \item{clr_median}{Median of cumulative loss ratios}
#'   \item{clr_wt}{Weighted cumulative loss ratio (`sum(closs) / sum(crp)`)}
#' }
#'
#' The returned object keeps the attributes `group_var` and `dev_var`,
#' and its class is updated to `"triangle_summary"`.
#'
#' @examples
#' \dontrun{
#' d <- build_triangle(df, group_var = cv_nm)
#' sm <- summary(d)
#' head(sm)
#' attr(sm, "longer")
#' }
#'
#' @method summary triangle
#' @export
summary.triangle <- function(object, ...) {
  .assert_class(object, "triangle")

  dt <- .ensure_dt(object)

  grp_var       <- attr(dt, "group_var")
  dev_var       <- attr(dt, "dev_var")
  dev_type      <- attr(dt, "dev_type")
  grp_dev_var   <- c(grp_var, "dev")

  ds <- dt[, .(
    n_obs      = .N,
    lr_mean    = mean(lr),
    lr_median  = median(lr),
    lr_wt      = sum(loss)  / sum(rp),
    clr_mean   = mean(clr),
    clr_median = median(clr),
    clr_wt     = sum(closs) / sum(crp)
  ), keyby = grp_dev_var]

  dm <- data.table::melt(
    data          = ds,
    id.vars       = grp_dev_var,
    measure.vars  = c(
      "lr_mean" , "lr_median" , "lr_wt",
      "clr_mean", "clr_median", "clr_wt"
    ),
    variable.name = "type",
    value.name    = "value"
  )
  dm <- .prepend_class(dm, "triangle_summary_longer")

  data.table::setattr(ds, "group_var"   , grp_var)
  data.table::setattr(ds, "dev_var" , dev_var)
  data.table::setattr(ds, "dev_type", dev_type)
  data.table::setattr(ds, "longer"      , dm)

  .update_class(ds, "triangle", "triangle_summary")
}

# Development Longer ------------------------------------------------------

#' @method longer triangle
#' @export
longer.triangle <- function(x, ...) {
  .assert_class(x, "triangle")
  attr(x, "longer")
}

#' @method longer triangle_summary
#' @export
longer.triangle_summary <- function(x, ...) {
  .assert_class(x, "triangle_summary")
  attr(x, "longer")
}
