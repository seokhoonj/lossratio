#' Check an experience dataset
#'
#' @description
#' Check that an experience dataset contains the required columns with
#' the expected classes, and validate the classes of optional columns
#' when present.
#'
#' @param df A data.frame containing experience data.
#'
#' @section Required columns:
#' These columns must be present:
#' \itemize{
#'   \item `cym`  : Calendar year-month (`Date`)
#'   \item `uym`  : Underwriting year-month (`Date`)
#'   \item `loss` : Incurred loss amount (`numeric`)
#'   \item `rp`   : Risk premium (`numeric`)
#' }
#'
#' @section Optional columns:
#' These columns are validated only when present:
#' \itemize{
#'   \item `elap_m`     : Elapsed month (`integer`)
#'   \item `pd_tp_cd`, `pd_tp_nm`, `pd_cd`, `pd_nm`:
#'     Product type/product codes and names (`character`)
#'   \item `cv_tp_cd`, `cv_tp_nm`, `cv_cd`, `cv_nm`:
#'     Coverage type/coverage codes and names (`character`)
#'   \item `rd_tp_cd`, `rd_tp_nm`, `rd_cd`, `rd_nm`:
#'     Rider type/rider codes and names (`character`)
#'   \item `age_band` : Age band (`ordered`)
#'   \item `gender`   : Gender (`factor`)
#'   \item `ch_cd`, `ch_nm` : Channel code and name (`character`)
#'   \item `n_policy` : Number of unique policies in the cell (`integer`)
#' }
#'
#' @section Derived columns:
#' The following columns may be derived later by
#' [add_experience_period()] and are not validated here:
#' \itemize{
#'   \item `uy`, `uyh`, `uyq` : Underwriting year, half-year, quarter
#'   \item `cy`, `cyh`, `cyq` : Calendar year, half-year, quarter
#'   \item `elap_y`, `elap_h`, `elap_q` : Elapsed year, half-year, quarter
#' }
#'
#' @return Invisibly returns the result of [.check_col_spec()].
#'
#' @seealso [as_experience()], [add_experience_period()],
#'   [.check_col_spec()]
#'
#' @export
check_experience <- function(df) {
  .assert_class(df, "data.frame")

  required_spec <- list(
    cym  = "Date",
    uym  = "Date",
    loss = "numeric",
    rp   = "numeric"
  )

  optional_spec <- list(
    elap_m     = "integer",
    pd_tp_cd = "character",
    pd_tp_nm = "character",
    pd_cd    = "character",
    pd_nm    = "character",
    cv_tp_cd = "character",
    cv_tp_nm = "character",
    cv_cd    = "character",
    cv_nm    = "character",
    rd_tp_cd = "character",
    rd_tp_nm = "character",
    rd_cd    = "character",
    rd_nm    = "character",
    age_band = "ordered",
    gender   = "factor",
    ch_cd    = "character",
    ch_nm    = "character",
    n_policy = "integer"
  )

  # check required columns (strict)
  .check_col_spec(df, required_spec)

  # check optional columns (only those present)
  present_optional <- optional_spec[names(optional_spec) %in% names(df)]
  if (length(present_optional)) {
    .check_col_spec(df, present_optional)
  }

  invisible(NULL)
}

#' Add standard period variables to an experience dataset
#'
#' @description
#' Add underwriting, calendar, and development period variables to an
#' experience dataset using standard column conventions for loss ratio
#' analysis.
#'
#' The function detects the presence of key source columns such as `uym`,
#' `cym`, and `elap_m`, and derives additional period variables when
#' possible.
#'
#' @details
#' The following variables are added when the required source columns
#' exist:
#'
#' \strong{Underwriting period (from `uym`):}
#' \itemize{
#'   \item `uy`  : underwriting year
#'   \item `uyh` : underwriting half-year
#'   \item `uyq` : underwriting quarter
#' }
#'
#' \strong{Calendar period (from `cym`):}
#' \itemize{
#'   \item `cy`  : calendar year
#'   \item `cyh` : calendar half-year
#'   \item `cyq` : calendar quarter
#' }
#'
#' \strong{Elapsed period:}
#' \itemize{
#'   \item `elap_y` is derived from `elap_m` as yearly development
#'     index, where months 1 to 12 map to 1, 13 to 24 map to 2, and so on.
#'   \item `elap_h` is derived from `uym` and `cym` using calendar half-year
#'     boundaries. For example, contracts issued in January to June are
#'     aligned to the same first development half-year block, and the next
#'     calendar half-year becomes development half-year 2.
#'   \item `elap_q` is derived from `uym` and `cym` using calendar quarter
#'     boundaries. For example, contracts issued in January to March are
#'     aligned to the same first development quarter block, and the next
#'     calendar quarter becomes development quarter 2.
#' }
#'
#' Therefore, `elap_h` and `elap_q` are not simple grouped versions of
#' `elap_m`; they are aligned to calendar half-year and quarter boundaries
#' so that underwriting cohorts such as Q1, Q2, H1, and H2 are compared
#' consistently on the same cumulative development basis.
#'
#' Newly created columns are inserted before their corresponding base
#' columns.
#'
#' @param df A data.frame containing period variables such as `uym`,
#'   `cym`, and `elap_m`.
#'
#' @return A data.frame (or tibble/data.table depending on input) with
#'   additional period variables.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   uym  = as.Date("2023-01-01") + 0:5 * 30,
#'   cym  = as.Date("2023-01-01") + 0:5 * 30,
#'   elap_m = 1:6
#' )
#'
#' df2 <- add_experience_period(df)
#' head(df2)
#' }
#'
#' @export
add_experience_period <- function(df) {
  .assert_class(df, "data.frame")

  dt <- .ensure_dt(df)

  has_uym  <- .has_cols(dt, "uym")
  has_cym  <- .has_cols(dt, "cym")
  has_elap_m <- .has_cols(dt, "elap_m")

  # Extract year / month once per source column (C-level via data.table).
  if (has_uym) {
    uym_year <- data.table::year(dt[["uym"]])
    uym_mon  <- data.table::month(dt[["uym"]])
  }
  if (has_cym) {
    cym_year <- data.table::year(dt[["cym"]])
    cym_mon  <- data.table::month(dt[["cym"]])
  }

  # underwriting period (uy, uyh, uyq) — calendar-anchored
  # H1: Jan-Jun, H2: Jul-Dec; Q1: Jan-Mar, ..., Q4: Oct-Dec
  if (has_uym) {
    uy_h_mon <- data.table::fifelse(uym_mon <= 6L, 1L, 7L)
    uy_q_mon <- ((uym_mon - 1L) %/% 3L) * 3L + 1L
    dt[, `:=`(
      uy  = .first_of_month(uym_year, 1L),
      uyh = .first_of_month(uym_year, uy_h_mon),
      uyq = .first_of_month(uym_year, uy_q_mon)
    )]
    data.table::setcolorder(dt, c("uy", "uyh", "uyq"), before = "uym")
  }

  # calendar period (cy, cyh, cyq)
  if (has_cym) {
    cy_h_mon <- data.table::fifelse(cym_mon <= 6L, 1L, 7L)
    cy_q_mon <- ((cym_mon - 1L) %/% 3L) * 3L + 1L
    dt[, `:=`(
      cy  = .first_of_month(cym_year, 1L),
      cyh = .first_of_month(cym_year, cy_h_mon),
      cyq = .first_of_month(cym_year, cy_q_mon)
    )]
    data.table::setcolorder(dt, c("cy", "cyh", "cyq"), before = "cym")
  }

  # development month (elap_m)
  if (!has_elap_m && has_uym && has_cym) {
    dt[, elap_m := (cym_year - uym_year) * 12L + (cym_mon - uym_mon) + 1L]
    data.table::setcolorder(dt, "elap_m", after = "cym")
    has_elap_m <- TRUE
  }

  # development period (elap_y, elap_h, elap_q) — calendar-anchored boundaries
  if (has_uym && has_cym && has_elap_m) {
    elap_m_local <- dt[["elap_m"]]
    elap_y <- (elap_m_local - 1L) %/% 12L + 1L

    uy_half <- (uym_mon - 1L) %/% 6L   # 0 = H1, 1 = H2
    cy_half <- (cym_mon - 1L) %/% 6L
    elap_h <- (cym_year - uym_year) * 2L + (cy_half - uy_half) + 1L

    uy_q <- (uym_mon - 1L) %/% 3L      # 0 = Q1, ..., 3 = Q4
    cy_q <- (cym_mon - 1L) %/% 3L
    elap_q <- (cym_year - uym_year) * 4L + (cy_q - uy_q) + 1L

    dt[, `:=`(elap_y = elap_y, elap_h = elap_h, elap_q = elap_q)]
    data.table::setcolorder(dt, c("elap_y", "elap_h", "elap_q"), before = "elap_m")
  }

  dt[]
}

# Vectorised first-day-of-(year, month) construction with deduplication.
# Experience data typically has only a few dozen unique (year, month)
# pairs, so we evaluate ISOdate() once per unique key (~30 calls) instead
# of once per row (~1M calls).
.first_of_month <- function(year, month) {
  if (length(month) == 1L) month <- rep_len(month, length(year))
  key   <- year * 100L + month
  ukey  <- unique(key)
  cache <- as.Date(ISOdate(ukey %/% 100L, ukey %% 100L, 1L, tz = "UTC"))
  cache[match(key, ukey)]
}

#' Coerce a dataset to an `Experience` object
#'
#' @description
#' Coerce a data.frame to a minimal `Experience` object for loss ratio
#' analysis.
#'
#' This function checks that the input contains the minimum required
#' columns, attempts to coerce them to the expected classes, optionally
#' derives standard period variables via [add_experience_period()], and
#' prepends class `"Experience"`.
#'
#' The function intentionally performs only minimal coercion. Other
#' columns such as grouping variables or presentation variables are left
#' unchanged and should be cleaned by the user in advance.
#'
#' @param df A data.frame containing experience data.
#' @param add_period Logical; if `TRUE`, derive additional period
#'   variables using [add_experience_period()]. Default is `TRUE`.
#'
#' @details
#' Minimum required columns are:
#' \itemize{
#'   \item `cym`  : Calendar year-month (`Date` or coercible to `Date`)
#'   \item `uym`  : Underwriting year-month (`Date` or coercible to `Date`)
#'   \item `loss` : Incurred loss amount (`numeric` or coercible)
#'   \item `rp`   : Risk premium (`numeric` or coercible)
#' }
#'
#' If `add_period = TRUE`, additional period variables such as `uy`,
#' `uyh`, `uyq`, `cy`, `cyh`, `cyq`, `elap_y`, `elap_h`, and `elap_q` may be
#' added, depending on the available source columns.
#'
#' @return A data.frame with class `"Experience"` prepended.
#'
#' @seealso [check_experience()], [add_experience_period()]
#'
#' @examples
#' \dontrun{
#' x <- as_experience(df)
#' class(x)
#' }
#'
#' @export
as_experience <- function(df, add_period = TRUE) {
  .assert_class(df, "data.frame")

  if (!is.logical(add_period) || length(add_period) != 1L ||
      is.na(add_period)) {
    stop("`add_period` must be a single non-missing logical value.",
         call. = FALSE)
  }

  required_cols <- c("cym", "uym", "loss", "rp")
  missing_cols  <- setdiff(required_cols, names(df))

  if (length(missing_cols)) {
    stop(
      paste0(
        "`df` must contain the required columns: ",
        paste(sprintf("'%s'", required_cols), collapse = ", "),
        ". Missing columns: ",
        paste(sprintf("'%s'", missing_cols), collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  dt <- .ensure_dt(df)

  # coerce required period columns
  if (!inherits(dt[["cym"]], "Date")) {
    data.table::set(dt, j = "cym", value = as.Date(dt[["cym"]]))
  }

  if (!inherits(dt[["uym"]], "Date")) {
    data.table::set(dt, j = "uym", value = as.Date(dt[["uym"]]))
  }

  # coerce required numeric columns
  data.table::set(dt, j = "loss", value = as.numeric(dt[["loss"]]))
  data.table::set(dt, j = "rp"  , value = as.numeric(dt[["rp"]]))

  # validate required columns after coercion
  if (anyNA(dt[["cym"]])) {
    stop("`cym` could not be safely coerced to `Date`.", call. = FALSE)
  }

  if (anyNA(dt[["uym"]])) {
    stop("`uym` could not be safely coerced to `Date`.", call. = FALSE)
  }

  if (anyNA(dt[["loss"]])) {
    stop("`loss` could not be safely coerced to `numeric`.", call. = FALSE)
  }

  if (anyNA(dt[["rp"]])) {
    stop("`rp` could not be safely coerced to `numeric`.", call. = FALSE)
  }

  # add derived period variables
  if (add_period) {
    dt <- data.table::as.data.table(add_experience_period(dt))
  }

  .prepend_class(dt, "Experience")
}

#' Check whether an object is an `Experience`
#'
#' @param x An object.
#'
#' @return Logical scalar.
#'
#' @export
is_experience <- function(x) {
  inherits(x, "Experience")
}

