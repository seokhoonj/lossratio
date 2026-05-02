# Synthetic experience-study generator for the lossratio package.
# Produces a fully synthetic `experience` data.table that mirrors the
# package's documented schema (column order, classes, factor levels) and
# lands in broadly plausible marginal ranges. Deterministic via
# set.seed(20260501L). Run from the package root:
#   Rscript -e 'source("data-raw/make_experience.R")'

set.seed(20260501L)

library(data.table)
library(lubridate)

# ---- Grid -------------------------------------------------------------------

cohorts   <- seq.Date(as.Date("2023-04-01"), as.Date("2025-09-01"), by = "month")
n_cohorts <- length(cohorts)  # 30

cv_levels       <- c("SUR", "CAN", "2CI", "HOS")
age_band_levels <- c("30-34", "35-39", "40-44", "45-49", "50-54",
                     "55-59", "60-64", "65-69", "70-")
gender_levels   <- c("M", "F")

# Build full triangle: cohort c (1..30) gets elap_m 1..(31 - c).
tri_grid <- rbindlist(lapply(seq_len(n_cohorts), function(c) {
  data.table(uym = cohorts[c], elap_m = seq.int(1L, n_cohorts - c + 1L))
}))

# Cross-join triangle cells with demographic strata. Each (uym, elap_m) cell
# is replicated 4 * 9 * 2 = 72 times.
dt <- CJ(idx      = seq_len(nrow(tri_grid)),
         cv_nm    = cv_levels,
         age_band = age_band_levels,
         gender   = gender_levels,
         sorted   = FALSE)
dt[, `:=`(uym = tri_grid$uym[idx], elap_m = tri_grid$elap_m[idx])]
dt[, idx := NULL]

# ---- Date derivations -------------------------------------------------------

dt[, uy  := lubridate::floor_date(uym, "year")]
dt[, uyq := lubridate::floor_date(uym, "quarter")]
dt[, uyh := as.Date(ifelse(month(uym) <= 6,
                           sprintf("%d-01-01", year(uym)),
                           sprintf("%d-07-01", year(uym))))]
dt[, cym := uym %m+% months(elap_m - 1L)]
dt[, cy  := lubridate::floor_date(cym, "year")]
dt[, cyq := lubridate::floor_date(cym, "quarter")]
dt[, cyh := as.Date(ifelse(month(cym) <= 6,
                           sprintf("%d-01-01", year(cym)),
                           sprintf("%d-07-01", year(cym))))]

dt[, elap_y := as.integer(ceiling(elap_m / 12))]
dt[, elap_h := as.integer(ceiling(elap_m / 6))]
dt[, elap_q := as.integer(ceiling(elap_m / 3))]
dt[, elap_m := as.integer(elap_m)]

# ---- Factor coercion --------------------------------------------------------

dt[, age_band := factor(age_band, levels = age_band_levels, ordered = TRUE)]
dt[, gender   := factor(gender,   levels = gender_levels)]

# ---- rp: cumulative risk premium (exposure proxy) ---------------------------
# Right-skewed Gamma scaled per cv_nm; rp grows with elap_m (cumulative).
# Allow tiny share negatives (refunds).

N <- nrow(dt)
rp_scale_by_cv <- c(SUR = 0.9e6, CAN = 0.9e6, `2CI` = 0.7e6, HOS = 0.9e6)
rp_base <- rgamma(N, shape = 0.6, scale = rp_scale_by_cv[dt$cv_nm])
# Cumulative growth with elap_m: roughly linear early then taper after 24m.
growth <- 1 + 0.6 * pmin(dt$elap_m, 24L)
dt[, rp := rp_base * growth]
# Refund flip on ~0.1% of cells.
dt[, rp := rp * ifelse(rbinom(.N, 1L, 0.999) == 1L, 1, -1)]

# ---- loss: cumulative loss --------------------------------------------------
# loss = (Bernoulli has-loss) * Gamma anchored to rp * conditional-LR scale.
# Targets ~65% zeros overall.
#
# Per-cv development LR curves are calibrated to the broad shape of a real
# long-term insurance portfolio's marginal LR-by-development pattern for the
# four coverage types. Only the smoothed first-10-month curve is taken from
# real aggregates; the elap_m 11-30 plateau is synthetic; cohort patterns,
# demographic mixes, cell-level loss / rp values are all randomly drawn.

dev_lr_by_cv <- list(
  SUR  = c(0.20, 0.80, 0.95, 1.05, 1.15, 1.20, 1.15, 1.20, 1.15, 1.10,
           rep(1.10, 20)),
  CAN  = c(0.01, 0.02, 0.05, 0.08, 0.15, 0.30, 0.40, 0.65, 0.70, 0.70,
           rep(0.70, 20)),
  `2CI` = c(0.05, 0.25, 0.55, 0.60, 0.70, 0.70, 0.80, 0.65, 0.75, 0.75,
            rep(0.70, 20)),
  HOS  = c(0.05, 0.15, 0.30, 0.40, 0.45, 0.40, 0.40, 0.40, 0.50, 0.40,
           rep(0.40, 20))
)
dev_lr_mat <- vapply(c("SUR", "CAN", "2CI", "HOS"),
                     function(cv) dev_lr_by_cv[[cv]],
                     numeric(30))
target_lr <- dev_lr_mat[cbind(dt$elap_m,
                              match(dt$cv_nm, c("SUR", "CAN", "2CI", "HOS")))]

# SUR cohort regime break at 2024-04: a synthetic scenario representing one
# of the four typical regime triggers in long-term insurance underwriting:
#   (1) drastic premium adjustment (up or down)
#   (2) product coverage content change
#   (3) sum insured limit change
#   (4) underwriting guideline change
# Post-2024-04 SUR cohorts get a 50% LR reduction. Strong enough that
# all of ecp / pelt / hclust detect the break in `detect_cohort_regime()`.
sur_post_break <- dt$cv_nm == "SUR" & dt$uym >= as.Date("2024-04-01")
target_lr[sur_post_break] <- target_lr[sur_post_break] * 0.50

p_has_loss <- pmin(0.55, dt$elap_m / 32)
has_loss   <- rbinom(N, 1L, p_has_loss)
shape_loss <- 5.0   # tight Gamma → cleaner cohort signal for regime demo
cond_scale <- pmax(abs(dt$rp), 1) * target_lr /
  pmax(p_has_loss, 0.05) / shape_loss
loss_raw <- has_loss * rgamma(N, shape = shape_loss, scale = cond_scale)

# Reversal sign flip on ~0.3% of cells.
dt[, loss := loss_raw * ifelse(rbinom(.N, 1L, 0.997) == 1L, 1, -1)]

# ---- Final column order (matches package schema) ----------------------------

col_order <- c("cy", "cyh", "cyq", "cym",
               "uy", "uyh", "uyq", "uym",
               "elap_y", "elap_h", "elap_q", "elap_m",
               "cv_nm", "age_band", "gender", "loss", "rp")
setcolorder(dt, col_order)
setattr(dt, "sorted", NULL)

experience <- dt[]

# ---- Save -------------------------------------------------------------------

usethis::use_data(experience, overwrite = TRUE)
