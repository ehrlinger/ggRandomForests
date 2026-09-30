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

test_that("kaplan life column is non-decreasing and proplife is in [0, 1]", {
  # Regression test: the trapezoidal formula L(t_i) = L(t_{i-1}) + (S(t_{i-1}) + S(t_i))/2 * Δt
  # must produce a monotonically non-decreasing cumulative integral.
  # The old Adams-Bashforth formula could produce this too, but values were
  # numerically wrong (over-estimated).
  gg_dta <- kaplan(interval = "time", censor = "status", data = pbc_dta)
  expect_true(all(diff(gg_dta$life) >= 0),
              info = "life must be non-decreasing (cumulative area under S(t))")
  expect_true(all(gg_dta$proplife >= 0),
              info = "proplife must be >= 0")
  expect_true(all(gg_dta$proplife <= 1 + .Machine$double.eps^0.5),
              info = "proplife must be <= 1 (area under S(t) <= t * 1)")
})

test_that("kaplan with character (non-factor) by labels the groups", {
  # .label_strata() has two code paths: levels() for factors, sorted unique
  # values for character/numeric.  This test exercises the second.
  pbc_strat <- pbc_dta
  pbc_strat$trt_chr <- as.character(pbc_strat$treatment)
  pbc_strat <- pbc_strat[!is.na(pbc_strat$trt_chr), ]

  gg_dta <- kaplan(interval = "time", censor = "status",
                   data = pbc_strat, by = "trt_chr")
  expect_true("groups" %in% colnames(gg_dta))
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

test_that("kaplan and nelson restart the interval columns in every by= stratum", {
  # Issue #303: the lags were taken across the stacked strata, so the first row
  # of every later group lagged off the last row of the group before it, and
  # life carried on from the previous group's total.
  vet <- survival::veteran
  lagged <- c("hazard", "density", "mid_int", "life", "proplife")
  for (est in list(kaplan, nelson)) {
    strat <- est(interval = "time", censor = "status", data = vet, by = "trt")
    for (grp in unique(vet$trt)) {
      alone <- est(interval = "time", censor = "status",
                   data = vet[vet$trt == grp, ])
      expect_equal(as.list(strat[strat$groups == grp, lagged]),
                   as.list(alone[, lagged]))
    }
  }
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

test_that("nelson life column is non-decreasing and proplife is in [0, 1]", {
  gg_dta <- nelson(interval = "time", censor = "status", data = pbc_dta)
  expect_true(all(diff(gg_dta$life) >= 0),
              info = "life must be non-decreasing")
  expect_true(all(gg_dta$proplife >= 0))
  expect_true(all(gg_dta$proplife <= 1 + .Machine$double.eps^0.5))
})

test_that("nelson with character (non-factor) by uses unique() labels", {
  pbc_strat <- pbc_dta
  pbc_strat$trt_chr <- as.character(pbc_strat$treatment)
  pbc_strat <- pbc_strat[!is.na(pbc_strat$trt_chr), ]

  gg_dta <- nelson(interval = "time", censor = "status",
                   data = pbc_strat, by = "trt_chr")
  expect_true("groups" %in% colnames(gg_dta))
  expect_true(length(unique(gg_dta$groups)) >= 2L)
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

## ---- by= strata are read from the fit, not inferred from the times ---------

test_that("by= strata are separated when their event times do not overlap", {
  # The strata used to be told apart by a drop in the stacked times, so an arm
  # whose times all follow the previous arm's was folded into it.
  dta <- data.frame(time = c(1, 2, 3, 4), status = 1,
                    grp = c("a", "a", "b", "b"))
  lagged <- c("cum_haz", "hazard", "density", "mid_int", "life", "proplife")
  for (est in list(kaplan, nelson)) {
    strat <- est(interval = "time", censor = "status", data = dta, by = "grp")
    expect_equal(strat$groups, c("a", "a", "b", "b"))
    for (grp in c("a", "b")) {
      alone <- est(interval = "time", censor = "status",
                   data = dta[dta$grp == grp, ])
      expect_equal(as.list(strat[strat$groups == grp, lagged]),
                   as.list(alone[, lagged]))
    }
  }
})

test_that("by= labels follow the fit's stratum order, not the row order", {
  # survfit() sorts the strata; a character by= column whose first row is not
  # the first stratum used to have its labels swapped.
  dta <- data.frame(time = c(3, 4, 1, 2), status = 1,
                    grp = c("b", "b", "a", "a"))
  strat <- kaplan(interval = "time", censor = "status", data = dta, by = "grp")
  expect_equal(strat$time[strat$groups == "a"], c(1, 2))
  expect_equal(strat$time[strat$groups == "b"], c(3, 4))

  # Rows survfit() drops, and factor levels nobody has, do not shift a label.
  dta$grp <- factor(dta$grp, levels = c("z", "b", "a"))
  dta <- rbind(dta, data.frame(time = NA, status = 1, grp = "z"))
  strat <- kaplan(interval = "time", censor = "status", data = dta, by = "grp")
  expect_equal(strat$time[strat$groups == "a"], c(1, 2))
  expect_equal(strat$time[strat$groups == "b"], c(3, 4))

  # A numeric by= column keeps its type in groups.
  vet <- survival::veteran
  expect_type(
    kaplan(interval = "time", censor = "status", data = vet, by = "trt")$groups,
    "double"
  )
})
