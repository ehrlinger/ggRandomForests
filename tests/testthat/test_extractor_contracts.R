# Cross-checks: every gg_* value is compared against the field of the source
# forest it is supposed to carry.
#
# The rest of the suite asserts shape: right class, right column names, right
# number of rows. Shape assertions pass just as happily when an extractor reads
# the wrong field, transposes a matrix, or silently reorders rows, which is the
# failure mode that actually matters in a visualisation layer: the plot still
# renders, and it shows the wrong number.
#
# Every assertion here compares against the source object, never against a
# number pasted from a previous run. A stored constant bakes in whatever was
# wrong when it was recorded, and breaks on every upstream version bump for a
# reason that has nothing to do with this package.
#
# Each test names the specific wrong behaviour it would catch.

## ---- gg_vimp ---------------------------------------------------------------

test_that("gg_vimp carries rfsrc regression importance, sorted", {
  # Catches: reading the wrong importance column, dropping the name-to-value
  # pairing when sorting, or returning importance in fitted rather than
  # descending order (the plot layer depends on that order).
  skip_if_not_installed("randomForestSRC")
  set.seed(20260817L)
  rf <- randomForestSRC::rfsrc(mpg ~ ., mtcars, ntree = 50, importance = TRUE)

  gg <- gg_vimp(rf)

  expect_equal(gg$vimp, unname(rf$importance[as.character(gg$vars)]))
  expect_false(is.unsorted(rev(gg$vimp)))
})

test_that("gg_vimp 'all' set matches the rfsrc classification importance column", {
  # Catches: a multi-class pivot that pairs a variable with another class's
  # importance. The long frame has one row per (variable, class), so an
  # off-by-one in the pivot still yields a plausible plot.
  skip_if_not_installed("randomForestSRC")
  set.seed(20260817L)
  rf <- randomForestSRC::rfsrc(Species ~ ., iris, ntree = 50, importance = TRUE)

  gg  <- gg_vimp(rf)
  all <- gg[gg$set == "all", ]

  expect_equal(all$vimp, unname(rf$importance[as.character(all$vars), "all"]))
})

test_that("gg_vimp uses %IncMSE for randomForest, not IncNodePurity", {
  # Catches: taking the wrong column of randomForest's two-column importance
  # matrix. Both columns are numeric, positive and variable-named, so the
  # substitution is invisible to every shape-based test, and IncNodePurity is
  # a different quantity on a different scale.
  skip_if_not_installed("randomForest")
  set.seed(20260817L)
  rf <- randomForest::randomForest(mpg ~ ., mtcars, ntree = 50, importance = TRUE)

  gg <- gg_vimp(rf)

  expect_equal(gg$vimp, unname(rf$importance[as.character(gg$vars), "%IncMSE"]))
})

## ---- gg_error --------------------------------------------------------------

test_that("gg_error drops rfsrc's NA error rows and keeps the tree index", {
  # rfsrc only evaluates err.rate every block.size trees, so with ntree = 50
  # the vector is 50 long but mostly NA. gg_error keeps the evaluated points.
  # Catches: retaining the NAs (a broken curve), renumbering the x axis 1..k
  # instead of carrying the true tree index (a curve compressed to the left),
  # or an upstream block.size default change going unnoticed.
  skip_if_not_installed("randomForestSRC")
  set.seed(20260817L)
  rf <- randomForestSRC::rfsrc(mpg ~ ., mtcars, ntree = 50, tree.err = TRUE)

  gg <- gg_error(rf)

  expect_equal(gg$error, as.numeric(stats::na.omit(rf$err.rate)))
  expect_equal(as.numeric(gg$ntree), as.numeric(which(!is.na(rf$err.rate))))
  expect_false(anyNA(gg$error))
})

test_that("gg_error carries the full randomForest MSE trajectory", {
  # randomForest's $mse is dense, one value per tree, unlike rfsrc's. Catches
  # an extractor that applies rfsrc's NA-dropping logic to both packages and
  # silently truncates the randomForest curve.
  skip_if_not_installed("randomForest")
  set.seed(20260817L)
  rf <- randomForest::randomForest(mpg ~ ., mtcars, ntree = 50)

  gg <- gg_error(rf)

  expect_equal(gg$error, as.numeric(rf$mse))
  expect_equal(nrow(gg), rf$ntree)
})

## ---- gg_rfsrc --------------------------------------------------------------

test_that("gg_rfsrc regression carries OOB predictions, not in-bag", {
  # Catches the substitution of $predicted for $predicted.oob. Both are
  # numeric vectors of length n with the same names and a similar range, so
  # nothing shape-based can tell them apart, but in-bag predictions are
  # optimistically biased and would make every error plot look better than
  # the model is. as.numeric() on both sides because predicted.oob is a 1-d
  # array; the contract is about values, not the dim attribute.
  skip_if_not_installed("randomForestSRC")
  set.seed(20260817L)
  rf <- randomForestSRC::rfsrc(mpg ~ ., mtcars, ntree = 50)

  gg <- gg_rfsrc(rf)

  expect_equal(as.numeric(gg$yhat), as.numeric(rf$predicted.oob))
  expect_false(isTRUE(all.equal(as.numeric(gg$yhat), as.numeric(rf$predicted))))
  expect_equal(as.numeric(gg$mpg), as.numeric(rf$yvar))
})

test_that("gg_rfsrc classification carries the OOB probability matrix intact", {
  # Catches a transposed or column-rotated probability matrix. Every row still
  # sums to 1 under a column rotation, so a sum-to-one check would pass while
  # every class label was wrong.
  skip_if_not_installed("randomForestSRC")
  set.seed(20260817L)
  rf <- randomForestSRC::rfsrc(Species ~ ., iris, ntree = 50)
  lev <- levels(iris$Species)

  gg <- gg_rfsrc(rf)

  expect_equal(
    unname(as.matrix(gg[, lev])),
    unname(matrix(rf$predicted.oob, ncol = length(lev)))
  )
  expect_equal(gg$y, rf$yvar)
})

## ---- gg_brier --------------------------------------------------------------

test_that("gg_brier agrees with randomForestSRC's own Brier calculation", {
  # The integrated CRPS is the one scalar gg_brier and randomForestSRC both
  # compute, so it is the available cross-check on the whole Brier pipeline.
  # Catches a wrong time grid, a mis-weighted censoring correction, or an
  # integration over the wrong axis. Note that the per-time crps column is a
  # time-NORMALISED running score and is legitimately non-monotone; do not
  # "fix" it into a monotone curve.
  skip_if_not_installed("randomForestSRC")
  set.seed(20260817L)
  rf <- randomForestSRC::rfsrc(
    Surv(time, status) ~ ., survival::veteran, ntree = 50
  )

  gg  <- gg_brier(rf)
  src <- randomForestSRC::get.brier.survival(rf)

  expect_equal(gg$time, as.numeric(rf$time.interest))
  expect_equal(
    as.numeric(attr(gg, "crps_integrated")), as.numeric(src$crps)
  )
  expect_true(all(gg$brier >= 0 & gg$brier <= 1, na.rm = TRUE))
})

## ---- gg_roc / calc_auc -----------------------------------------------------

test_that("calc_auc matches the Mann-Whitney rank identity", {
  # AUC equals the probability that a random positive outscores a random
  # negative, which is computable from predicted.oob by ranks alone and shares
  # no code with the ROC path. Catches a trapezoid integrated in the wrong
  # direction, an off-by-one in the threshold grid, or sens/spec swapped:
  # all of those still produce a monotone curve inside [0, 1].
  skip_if_not_installed("randomForestSRC")
  set.seed(20260817L)
  rf  <- randomForestSRC::rfsrc(Species ~ ., iris, ntree = 50)
  lev <- levels(iris$Species)

  auc_rank <- function(score, positive) {
    r  <- rank(score)
    n1 <- sum(positive)
    n0 <- sum(!positive)
    (sum(r[positive]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }

  for (k in seq_along(lev)) {
    expect_equal(
      calc_auc(gg_roc(rf, which_outcome = k)),
      auc_rank(as.numeric(rf$predicted.oob[, k]), iris$Species == lev[k]),
      label = paste("AUC for class", lev[k])
    )
  }
})

## ---- known-truth simulation ------------------------------------------------

test_that("gg_vimp separates known signal from known noise", {
  # The only test here that can catch a plausible-but-wrong ranking. Every
  # other importance test compares gg_vimp against rfsrc's importance field,
  # so both would move together if the field were misread in a
  # order-preserving way. Here the truth is fixed by construction: x1 and x2
  # drive y, x3..x6 are independent noise, so every signal variable must
  # outrank every noise variable.
  skip_on_cran()
  skip_if_not_installed("randomForestSRC")
  set.seed(20260817L)
  n   <- 300L
  dta <- data.frame(
    x1 = stats::rnorm(n), x2 = stats::rnorm(n), x3 = stats::rnorm(n),
    x4 = stats::rnorm(n), x5 = stats::rnorm(n), x6 = stats::rnorm(n)
  )
  dta$y <- 5 * dta$x1 + 3 * dta$x2 + stats::rnorm(n, sd = 0.5)

  rf <- randomForestSRC::rfsrc(y ~ ., dta, ntree = 200, importance = TRUE)
  gg <- gg_vimp(rf)

  imp    <- stats::setNames(gg$vimp, as.character(gg$vars))
  signal <- imp[c("x1", "x2")]
  noise  <- imp[c("x3", "x4", "x5", "x6")]

  expect_true(min(signal) > max(noise))
  expect_equal(names(which.max(imp)), "x1")
})
