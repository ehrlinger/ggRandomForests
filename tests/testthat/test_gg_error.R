# testthat for gg_error function

test_that("gg_error.rfsrc classifications", {
  ## Load the cached forest
  data(iris, package = "datasets")
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    importance = TRUE,
    tree.err = TRUE
  )
  # Test the cached forest type
  expect_s3_class(rfsrc_iris, "rfsrc")

  # Test the forest family
  expect_match(rfsrc_iris$family, "class")

  ## Create the correct gg_error object
  gg_dta <- gg_error(rfsrc_iris)

  # Test object type
  expect_s3_class(gg_dta, "gg_error")

  # Test classification dimensions
  expect_equal(dim(gg_dta)[1], dim(na.omit(rfsrc_iris$err.rate))[1])
  expect_equal(dim(gg_dta)[2], dim(rfsrc_iris$err.rate)[2] + 1)

  # Test data is correctly pulled from randomForest obect.
  # expect_equal(as.matrix(gg_dta[, -which(colnames(gg_dta) == "ntree")], ignore_attr = TRUE),
  #                   rfsrc_iris$err.rate)

  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  # "Incorrect object type: Expects a gg_error object"
  expect_error(gg_error(gg_plt))
  expect_error(gg_error.rfsrc(gg_plt))
  rfsrc_iris$err.rate <- NULL
  expect_error(gg_error(rfsrc_iris))

  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    importance = TRUE,
    tree.err = TRUE
  )
  ## Create the correct gg_error object
  gg_dta <- gg_error(rfsrc_iris, training = TRUE)

  # Test object type
  expect_s3_class(gg_dta, "gg_error")

  ## Test plotting the gg_error object (no warnings expected since pivot_longer migration)
  gg_plt <- plot(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")
})


test_that("gg_error.randomForest classifications", {
  ## Load the cached forest
  data(iris, package = "datasets")
  ## Load the cached forest
  rf_iris <- randomForest::randomForest(Species ~ .,
                                        data = iris,
                                        ntree = 75)

  # Test the cached forest type
  expect_s3_class(rf_iris, "randomForest")

  # Test the forest family
  expect_match(rf_iris$type, "classification")

  ## Create the correct gg_error object
  gg_dta <- gg_error(rf_iris)

  # Test object type
  expect_s3_class(gg_dta, "gg_error")

  # Test classification dimensions
  expect_equal(dim(gg_dta)[1], dim(rf_iris$err.rate)[1])
  expect_equal(dim(gg_dta)[2], dim(rf_iris$err.rate)[2] + 1)

  # Test data is correctly pulled from randomForest obect.
  expect_equal(as.matrix(gg_dta[, -which(colnames(gg_dta) == "ntree")], ignore_attr = TRUE),
                    rf_iris$err.rate)

  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  ## Ensure training curve can be requested
  gg_train <- gg_error(rf_iris, training = TRUE)
  expect_true("train" %in% names(gg_train))
  expect_length(gg_train$train, nrow(gg_train))
  expect_true(min(gg_train$train, na.rm = TRUE) >= 0)
  expect_true(max(gg_train$train, na.rm = TRUE) <= 1)

  # "Incorrect object type: Expects a gg_error object"
  expect_error(gg_error(gg_plt))
  expect_error(gg_error.randomForest(gg_plt))
  rf_iris$err.rate <- NULL
  expect_error(gg_error(rf_iris))


})



test_that("gg_error regression rfsrc", {
  ## Load the cached forest
  data(Boston, package = "MASS")

  Boston$chas <- as.logical(Boston$chas) # nolint: object_name_linter

  rfsrc_boston <- randomForestSRC::rfsrc(medv ~ ., data = Boston)
  # Test the cached forest type
  expect_s3_class(rfsrc_boston, "rfsrc")

  # Test the forest family
  expect_match(rfsrc_boston$family, "regr")

  ## Create the correct gg_error object
  gg_dta <- gg_error(rfsrc_boston)

  # Test object type
  expect_s3_class(gg_dta, "gg_error")

  # Test classification dimensions
  expect_equal(nrow(gg_dta), length(na.omit(rfsrc_boston$err.rate)))
  expect_equal(ncol(gg_dta), 2)

  # Test data is correctly pulled from randomForest object.
  expect_equal(as.numeric(gg_dta[[1]]), as.numeric(na.omit(rfsrc_boston$err.rate)))

  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  # Test the exception for input
  expect_error(gg_error(gg_plt))


  gg_dta <- gg_error(rfsrc_boston, training = TRUE)
  expect_s3_class(gg_dta, "gg_error")


})


test_that("gg_error regression randomForest", {
  ## Load the cached forest
  data(Boston, package = "MASS")

  Boston$chas <- as.logical(Boston$chas) # nolint: object_name_linter

  rf_boston <- randomForest::randomForest(medv ~ ., data = Boston, ntree = 100)
  # Test the cached forest type
  expect_s3_class(rf_boston, "randomForest")

  # Test the forest family
  expect_match(rf_boston$type, "regression")

  ## Create the correct gg_error object
  gg_dta <- gg_error(rf_boston)

  # Test object type
  expect_s3_class(gg_dta, "gg_error")

  # Test classification dimensions
  expect_equal(nrow(gg_dta), length(na.omit(rf_boston$mse)))
  expect_equal(ncol(gg_dta), 2)

  # Test data is correctly pulled from randomForest obect.
  expect_equal(c(gg_dta[, 1]), rf_boston$mse, ignore_attr = TRUE)

  ## Create the correct gg_error object
  gg_dta <- gg_error(rf_boston)

  # Test object type
  expect_s3_class(gg_dta, "gg_error")

  ## Training curve is available
  gg_train <- gg_error(rf_boston, training = TRUE)
  expect_true("train" %in% names(gg_train))
  expect_length(gg_train$train, nrow(gg_train))
  expect_true(all(is.finite(gg_train$train)))

})

## ---- Direct plot.gg_error() tests -----------------------------------------
# The tests above exercise plot() via S3 dispatch. The tests below call
# plot.gg_error() by name to ensure the function itself is exercised directly
# and all branches are covered explicitly.

test_that("plot.gg_error direct: regression rfsrc (single-outcome path, no legend)", {
  data(Boston, package = "MASS")
  Boston$chas <- as.logical(Boston$chas) # nolint: object_name_linter
  rfsrc_boston <- randomForestSRC::rfsrc(medv ~ ., data = Boston, ntree = 50L)
  gg_dta <- gg_error(rfsrc_boston)

  # Call by name — not via generic dispatch
  gg_plt <- plot.gg_error(gg_dta)

  expect_s3_class(gg_plt, "ggplot")
  # Single-outcome: gg_dta has exactly 2 columns (error, ntree)
  expect_equal(ncol(gg_dta), 2L)
  # Legend should be suppressed — theme has legend.position = "none"
  expect_equal(gg_plt$theme$legend.position, "none")
})

test_that("plot.gg_error direct: survival rfsrc (single-outcome path)", {
  data(pbc, package = "randomForestSRC")
  pbc$time <- pbc$days / 364.25
  pbc_sub <- pbc[, c("time", "status", "age", "bili")]
  Surv <- survival::Surv  # nolint: object_name_linter
  rfsrc_pbc <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = pbc_sub,
    ntree = 50L
  )
  gg_dta <- gg_error(rfsrc_pbc)
  gg_plt <- plot.gg_error(gg_dta)

  expect_s3_class(gg_plt, "ggplot")
  expect_equal(ncol(gg_dta), 2L)
})

test_that("plot.gg_error direct: classification rfsrc (multi-outcome path, legend shown)", {
  data(iris, package = "datasets")
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ ., data = iris, importance = TRUE, tree.err = TRUE
  )
  gg_dta <- gg_error(rfsrc_iris)
  gg_plt <- plot.gg_error(gg_dta)

  expect_s3_class(gg_plt, "ggplot")
  # Multi-outcome: more than 2 columns before pivot
  expect_true(ncol(gg_dta) > 2L)
  # Legend should NOT be suppressed — multiple outcomes need the colour key
  expect_false(identical(gg_plt$theme$legend.position, "none"))
})

test_that("plot.gg_error direct: accepts raw rfsrc object (auto-extract path)", {
  data(Boston, package = "MASS")
  Boston$chas <- as.logical(Boston$chas) # nolint: object_name_linter
  rfsrc_boston <- randomForestSRC::rfsrc(medv ~ ., data = Boston, ntree = 50L)

  # Pass the rfsrc object directly — plot.gg_error should call gg_error() internally
  gg_plt <- plot.gg_error(rfsrc_boston)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("plot.gg_error direct: errors on non-gg_error non-rfsrc input", {
  expect_error(plot.gg_error(list(a = 1)), "Incorrect object type")
  expect_error(plot.gg_error("not a forest"), "Incorrect object type")
})

test_that("plot.gg_error direct: point geometry used when only one valid row", {
  data(Boston, package = "MASS")
  Boston$chas <- as.logical(Boston$chas) # nolint: object_name_linter
  rfsrc_boston <- randomForestSRC::rfsrc(medv ~ ., data = Boston, ntree = 50L)
  gg_dta <- gg_error(rfsrc_boston)

  # Manufacture a single-row gg_error to trigger the point branch
  single_row <- gg_dta[1L, ]
  class(single_row) <- class(gg_dta)
  gg_plt <- plot.gg_error(single_row)

  expect_s3_class(gg_plt, "ggplot")
  # Confirm geom_point was used (not geom_line)
  geom_classes <- sapply(gg_plt$layers, function(l) class(l$geom)[1])
  expect_true(any(grepl("GeomPoint", geom_classes)))
})
