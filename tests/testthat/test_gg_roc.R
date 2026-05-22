# testthat for gg_roc function

test_that("gg_roc classifications", {
  ## Load the cached forest
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    forest = TRUE,
    importance = TRUE,
    save.memory = TRUE)

  # Test the cached forest type
  expect_s3_class(rfsrc_iris, "rfsrc")

  # Test the forest family
  expect_match(rfsrc_iris$family, "class")

  ## Create the correct gg_roc object
  which_outcome <- 1
  gg_dta <- gg_roc(rfsrc_iris, which_outcome)

  # Test object type
  expect_s3_class(gg_dta, "gg_roc")

  # Test classification dimensions
  expect_equal(ncol(gg_dta), 3)

  # Test data is correctly pulled from randomForest obect.
  unts <- sort(unique(rfsrc_iris$predicted.oob[, which_outcome]))

  ## Test plotting the gg_roc object
  gg_obj <- plot.gg_roc(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_obj, "ggplot")

  # Try test set prediction.
  gg_dta <- gg_roc(rfsrc_iris, which_outcome, oob = FALSE)
  # Try test set prediction.
  gg_plt <- plot.gg_roc(rfsrc_iris)

  # Test object type
  expect_s3_class(gg_dta, "gg_roc")

  # Test classification dimensions
  expect_equal(ncol(gg_dta), 3)

  # Test data is correctly pulled from randomForest obect.
  unts <- sort(unique(rfsrc_iris$predicted[, which_outcome]))

  ## Test plotting the gg_roc object
  gg_obj <- plot.gg_roc(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_obj, "ggplot")

  expect_s3_class(plot.gg_roc(rfsrc_iris), "ggplot")
  expect_error(gg_roc.randomForest(rfsrc_iris))
  expect_error(gg_roc.rfsrc(rf_iris))
})

test_that("gg_roc randomForest classifications", {
  ## Load the cached forest
  rf_iris <- randomForest(Species ~ ., data = iris)

  # Test the cached forest type
  expect_s3_class(rf_iris, "randomForest")

  # Test the forest family
  expect_match(rf_iris$type, "classification")

  ## Create the correct gg_roc object
  which_outcome <- 1
  gg_dta <- gg_roc(rf_iris, which_outcome)

  # Test object type
  expect_s3_class(gg_dta, "gg_roc")

  ## Test plotting the gg_roc object
  gg_obj <- plot.gg_roc(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_obj, "ggplot")

  # Try test set prediction.
  gg_dta <- gg_roc(rf_iris, which_outcome, oob = FALSE)

  # Test object type
  expect_s3_class(gg_dta, "gg_roc")
  # Test classification dimensions
  expect_equal(ncol(gg_dta), 3)

  ## Test plotting the gg_roc object
  gg_obj <- plot.gg_roc(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_obj, "ggplot")

  expect_s3_class(plot.gg_roc(rf_iris), "ggplot")

  expect_error(gg_roc.rfsrc(rf_iris))
})

test_that("gg_roc default oob=TRUE works without explicit argument", {
  # Regression test: gg_roc() was crashing with
  # "argument 'oob' is missing, with no default" when oob was not supplied.
  set.seed(42)
  rfsrc_iris <- randomForestSRC::rfsrc(Species ~ ., data = iris, ntree = 50)

  # All three outcomes must work without passing oob
  expect_s3_class(gg_roc(rfsrc_iris, which_outcome = 1), "gg_roc")
  expect_s3_class(gg_roc(rfsrc_iris, which_outcome = 2), "gg_roc")
  expect_s3_class(gg_roc(rfsrc_iris, which_outcome = 3), "gg_roc")

  # OOB default should equal oob = TRUE explicitly
  gg_default <- gg_roc(rfsrc_iris, which_outcome = 1)
  gg_explicit <- gg_roc(rfsrc_iris, which_outcome = 1, oob = TRUE)
  expect_equal(gg_default, gg_explicit)
})

test_that("gg_roc regression", {
  data(Boston, package = "MASS")
  boston <- Boston

  boston$chas <- as.logical(boston$chas)

  ## Load the cached forest
  rfsrc_boston <-
    randomForestSRC::rfsrc(
      medv ~ .,
      data = boston,
      forest = TRUE,
      importance = TRUE,
      tree.err = TRUE,
      save.memory = TRUE)

  # Test the cached forest type
  expect_s3_class(rfsrc_boston, "rfsrc")

  # Test the forest family
  expect_match(rfsrc_boston$family, "regr")

  ## Create the correct gg_roc object
  expect_error(gg_roc(rfsrc_boston))
  expect_error(plot.gg_roc(rfsrc_boston))

})

test_that("calc_roc", {
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    forest = TRUE,
    importance = TRUE,
    save.memory = TRUE)

  # Test the cached forest type
  expect_s3_class(rfsrc_iris, "rfsrc")

  # Test the forest family
  expect_match(rfsrc_iris$family, "class")

  gg_dta <- calc_roc.rfsrc(rfsrc_iris,
                           rfsrc_iris$yvar,
                           which_outcome = 1,
                           oob = TRUE)

  # Test the cached forest type
  expect_s3_class(gg_dta, "data.frame")

  expect_equal(ncol(gg_dta), 3)

  # Test oob=FALSE
  gg_dta <- calc_roc.rfsrc(rfsrc_iris,
                           rfsrc_iris$yvar,
                           which_outcome = 1,
                           oob = FALSE)

  # Test the cached forest type
  expect_s3_class(gg_dta, "data.frame")

  expect_equal(ncol(gg_dta), 3)

  # test the auc calculator
  auc <- calc_auc(gg_dta)
  expect_true(auc > .9)
  expect_true(auc <= 1)
  # The second outcome.
  gg_dta <- ggRandomForests::calc_roc(rfsrc_iris,
                           rfsrc_iris$yvar,
                           which_outcome = 2,
                           oob = TRUE)

  # Test the cached forest type
  expect_s3_class(gg_dta, "data.frame")

  expect_equal(ncol(gg_dta), 3)

  # test the auc calculator
  auc <- calc_auc(gg_dta)
  expect_true(auc > .9)
  expect_true(auc <= 1)
  # and the third...
  gg_dta <- calc_roc(rfsrc_iris,
                           rfsrc_iris$yvar,
                           which_outcome = 3,
                           oob = TRUE)

  # Test the cached forest type
  expect_s3_class(gg_dta, "data.frame")

  expect_equal(ncol(gg_dta), 3)

  # test the auc calculator
  auc <- calc_auc(gg_dta)
  expect_true(auc > .9)
  expect_true(auc <= 1)
})

# ---------------------------------------------------------------------------
# randomForest ROC correctness (#81) + rfsrc no-change characterization
# ---------------------------------------------------------------------------

test_that("gg_roc randomForest: separable class AUC ~ 1 and many thresholds (#81)", {
  set.seed(42)
  rf <- randomForest::randomForest(Species ~ ., data = iris)
  setosa <- which(levels(iris$Species) == "setosa")
  g <- gg_roc(rf, which_outcome = setosa)
  expect_s3_class(g, "gg_roc")
  expect_gt(nrow(g), nlevels(iris$Species))      # not the degenerate ~3-row curve
  expect_gt(calc_auc(g), 0.98)                    # setosa is separable
})

test_that("gg_roc randomForest default is macro-average, many points, no warning (#81)", {
  set.seed(42)
  rf <- randomForest::randomForest(Species ~ ., data = iris)
  expect_no_warning(g <- gg_roc(rf))              # was: 'Must specify which_outcome'
  expect_s3_class(g, "gg_roc")
  expect_gt(nrow(g), nlevels(iris$Species))
  expect_gt(calc_auc(g), 0.5)
})

test_that("calc_roc.rfsrc output is unchanged for an explicit which_outcome (guard)", {
  set.seed(42)
  rfsrc_iris <- randomForestSRC::rfsrc(Species ~ ., data = iris, ntree = 50)
  g <- gg_roc(rfsrc_iris, which_outcome = 1)
  expect_s3_class(g, "gg_roc")
  expect_equal(ncol(g), 3L)                       # sens, spec, pct (existing contract)
  expect_true(all(c("sens", "spec", "pct") %in% colnames(g)))
  expect_gte(calc_auc(g), 0.9)                    # rfsrc iris setosa-vs-rest stays strong
})

## ── per_class = TRUE (PR #88) ──────────────────────────────────────────────

test_that("gg_roc per_class=TRUE: long format with class column", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, per_class = TRUE)
  expect_true("class" %in% names(gg))
  expect_true(all(c("sens", "spec", "pct") %in% names(gg)))  # pct = threshold; same 3-col contract as calc_roc
  expect_s3_class(gg$class, "factor")
  expect_equal(nlevels(gg$class), 3L)
})

test_that("gg_roc per_class=TRUE: auc attr is named numeric vector length 3", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf  <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg  <- gg_roc(rf, per_class = TRUE)
  auc <- attr(gg, "auc")
  expect_length(auc, 3L)
  expect_named(auc)
  # setosa is linearly separable in iris — AUC should be near-perfect
  expect_gt(auc[["setosa"]], 0.99)
  # AUC values must be sorted descending
  expect_true(all(diff(auc) <= 0))
})

test_that("gg_roc per_class=TRUE: class factor levels ordered by descending AUC", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf  <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg  <- gg_roc(rf, per_class = TRUE)
  auc <- attr(gg, "auc")
  expect_equal(levels(gg$class), names(auc))
})

test_that("gg_roc per_class=TRUE on binary forest: no class column (no-op)", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  bin_data         <- iris[iris$Species != "virginica", ]
  bin_data$Species <- droplevels(bin_data$Species)
  rf  <- randomForest::randomForest(Species ~ ., data = bin_data, ntree = 100L)
  gg  <- gg_roc(rf, per_class = TRUE)
  # Binary forest: per_class is a no-op — no class column, scalar AUC
  expect_false("class" %in% names(gg))
  expect_length(attr(gg, "auc"), 1L)
})

test_that("gg_roc per_class=TRUE + which_outcome integer: message then per_class wins", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  expect_message(
    gg <- gg_roc(rf, per_class = TRUE, which_outcome = 1L),
    "which_outcome.*ignored.*per_class"
  )
  expect_true("class" %in% names(gg))
})

test_that("gg_roc which_outcome='all' still returns macro-average (no class column)", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, which_outcome = "all")
  expect_false("class" %in% names(gg))
  # Macro-average returns a single data frame, not a class-faceted one
  expect_true(all(c("sens", "spec", "pct") %in% names(gg)))  # pct = threshold; same 3-col contract as calc_roc
})
