# Unit tests for kaplan(), nelson(), and bootstrap_survival()

## ---- Shared survival data --------------------------------------------------

data(pbc, package = "randomForestSRC")
pbc_dta <- pbc
pbc_dta$time <- pbc_dta$days / 364.25

## ---- kaplan() --------------------------------------------------------------

test_that("kaplan returns a gg_survival data frame", {
  gg_dta <- kaplan(interval = "time", censor = "status", data = pbc_dta)
  expect_s3_class(gg_dta, "gg_survival")
  expect_s3_class(gg_dta, "data.frame")
})

test_that("kaplan output has required columns", {
  gg_dta <- kaplan(interval = "time", censor = "status", data = pbc_dta)
  required_cols <- c("time", "n", "cens", "dead", "surv", "se",
                     "lower", "upper", "cum_haz",
                     "hazard", "density", "mid_int", "life", "proplife")
  expect_true(all(required_cols %in% colnames(gg_dta)))
})

test_that("kaplan retains only event rows (dead > 0)", {
  gg_dta <- kaplan(interval = "time", censor = "status", data = pbc_dta)
  expect_true(all(gg_dta$dead > 0))
})

test_that("kaplan survival is monotonically non-increasing", {
  gg_dta <- kaplan(interval = "time", censor = "status", data = pbc_dta)
  expect_true(all(diff(gg_dta$surv) <= 0))
})

test_that("kaplan survival is bounded in [0, 1]", {
  gg_dta <- kaplan(interval = "time", censor = "status", data = pbc_dta)
  expect_true(all(gg_dta$surv >= 0))
  expect_true(all(gg_dta$surv <= 1))
})

test_that("kaplan with stratification adds groups column", {
  pbc_strat <- pbc_dta
  pbc_strat$treatment <- factor(pbc_strat$treatment)
  gg_dta <- kaplan(interval = "time", censor = "status",
                   data = pbc_strat, by = "treatment")
  expect_true("groups" %in% colnames(gg_dta))
  # Both treatment levels should appear
  expect_true(length(unique(gg_dta$groups)) >= 2L)
})

test_that("kaplan plot returns a ggplot", {
  gg_dta <- kaplan(interval = "time", censor = "status", data = pbc_dta)
  expect_s3_class(plot(gg_dta), "ggplot")
})

test_that("kaplan plot with error = 'none' returns a ggplot", {
  gg_dta <- kaplan(interval = "time", censor = "status", data = pbc_dta)
  expect_s3_class(plot(gg_dta, error = "none"), "ggplot")
})

## ---- nelson() --------------------------------------------------------------

test_that("nelson returns a gg_survival data frame", {
  gg_dta <- nelson(interval = "time", censor = "status", data = pbc_dta)
  expect_s3_class(gg_dta, "gg_survival")
  expect_s3_class(gg_dta, "data.frame")
})

test_that("nelson output has required columns", {
  gg_dta <- nelson(interval = "time", censor = "status", data = pbc_dta)
  required_cols <- c("time", "n", "cens", "dead", "surv", "se",
                     "lower", "upper", "cum_haz",
                     "hazard", "density", "mid_int", "life", "proplife")
  expect_true(all(required_cols %in% colnames(gg_dta)))
})

test_that("nelson retains only event rows (dead > 0)", {
  gg_dta <- nelson(interval = "time", censor = "status", data = pbc_dta)
  expect_true(all(gg_dta$dead > 0))
})

test_that("nelson cum_haz is non-decreasing", {
  gg_dta <- nelson(interval = "time", censor = "status", data = pbc_dta)
  expect_true(all(diff(gg_dta$cum_haz) >= 0))
})

test_that("nelson with stratification adds groups column", {
  pbc_strat <- pbc_dta
  pbc_strat$treatment <- factor(pbc_strat$treatment)
  gg_dta <- nelson(interval = "time", censor = "status",
                   data = pbc_strat, by = "treatment")
  expect_true("groups" %in% colnames(gg_dta))
  expect_true(length(unique(gg_dta$groups)) >= 2L)
})

test_that("nelson and kaplan agree on survival estimates", {
  kap <- kaplan(interval = "time", censor = "status", data = pbc_dta)
  nel <- nelson(interval = "time", censor = "status", data = pbc_dta)
  # Both should produce estimates at the same time points
  common_times <- intersect(kap$time, nel$time)
  expect_true(length(common_times) > 0L)
  kap_surv <- kap$surv[kap$time %in% common_times]
  nel_surv <- nel$surv[nel$time %in% common_times]
  # KM and NA estimates are equivalent for the same data
  expect_equal(kap_surv, nel_surv, tolerance = 1e-6)
})

test_that("nelson plot returns a ggplot", {
  gg_dta <- nelson(interval = "time", censor = "status", data = pbc_dta)
  expect_s3_class(plot(gg_dta), "ggplot")
})

## ---- bootstrap_survival() --------------------------------------------------

# Helper: build wide-form survival data (obs x time-point columns) that
# bootstrap_survival() expects. gg_rfsrc() without conf.int pivots to long
# form, so we construct the wide matrix directly from the rfsrc internals.
.make_wide_surv <- function(rfsrc_obj) {
  wide <- data.frame(rfsrc_obj$survival.oob)
  colnames(wide) <- as.character(rfsrc_obj$time.interest)
  wide$obs_id <- seq_len(nrow(wide))
  wide$event  <- as.logical(rfsrc_obj$yvar[, 2L])
  wide
}

test_that("bootstrap_survival returns a data frame with correct columns (95% CI)", {
  data(pbc, package = "randomForestSRC")
  pbc_sub <- pbc[, c("days", "status", "treatment", "age", "bili", "albumin")]
  pbc_sub$time <- pbc_sub$days / 364.25

  set.seed(1L)
  rfsrc_pbc <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = pbc_sub, ntree = 50L)

  wide_dta   <- .make_wide_surv(rfsrc_pbc)
  n_timepts  <- length(rfsrc_pbc$time.interest)
  level_set  <- c(0.025, 0.975)

  set.seed(42L)
  result <- ggRandomForests:::bootstrap_survival(wide_dta, 50L, level_set)

  expect_s3_class(result, "data.frame")
  expect_equal(colnames(result), c("value", "lower", "upper", "median", "mean"))
  expect_equal(nrow(result), n_timepts)
  expect_true(all(result$lower <= result$mean + 1e-10))
  expect_true(all(result$mean  <= result$upper + 1e-10))
})

test_that("bootstrap_survival lower bound <= median <= upper bound", {
  data(pbc, package = "randomForestSRC")
  pbc_sub <- pbc[, c("days", "status", "treatment", "age", "bili", "albumin")]
  pbc_sub$time <- pbc_sub$days / 364.25

  set.seed(2L)
  rfsrc_pbc <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = pbc_sub, ntree = 50L)

  wide_dta  <- .make_wide_surv(rfsrc_pbc)
  level_set <- c(0.025, 0.975)

  set.seed(42L)
  result <- ggRandomForests:::bootstrap_survival(wide_dta, 50L, level_set)

  expect_true(all(result$lower  <= result$median + 1e-10))
  expect_true(all(result$median <= result$upper  + 1e-10))
})

test_that("bootstrap_survival time points match rfsrc$time.interest", {
  data(pbc, package = "randomForestSRC")
  pbc_sub <- pbc[, c("days", "status", "treatment", "age", "bili", "albumin")]
  pbc_sub$time <- pbc_sub$days / 364.25

  set.seed(3L)
  rfsrc_pbc <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = pbc_sub, ntree = 50L)

  wide_dta       <- .make_wide_surv(rfsrc_pbc)
  expected_times <- rfsrc_pbc$time.interest
  level_set      <- c(0.025, 0.975)

  set.seed(42L)
  result <- ggRandomForests:::bootstrap_survival(wide_dta, 50L, level_set)

  expect_equal(result$value, expected_times)
})
