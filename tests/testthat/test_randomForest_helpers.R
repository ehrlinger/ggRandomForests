
skip_if_not_installed("randomForest")

data(Boston, package = "MASS")
data(iris, package = "datasets")

set.seed(100)

iris_subset <- droplevels(subset(iris, Species != "setosa"))


test_that(".rf_recover_model_frame rebuilds subsetted data", {
  rf_subset <- randomForest::randomForest(
    Species ~ .,
    data = iris_subset,
    ntree = 50,
    keep.forest = TRUE
  )
  info <- ggRandomForests:::`.rf_recover_model_frame`(rf_subset)
  expect_type(info, "list")
  expect_true(all(c("Sepal.Length", "Sepal.Width") %in% names(info$model_frame)))
  expect_equal(info$response_name, "Species")
})


test_that(".rf_training_curve returns trajectories for both families", {
  rf_cls <- randomForest::randomForest(
    Species ~ .,
    data = iris,
    ntree = 40,
    keep.forest = TRUE
  )
  rf_reg <- randomForest::randomForest(
    medv ~ .,
    data = Boston,
    ntree = 40,
    keep.forest = TRUE
  )

  cls_curve <- ggRandomForests:::`.rf_training_curve`(rf_cls)
  reg_curve <- ggRandomForests:::`.rf_training_curve`(rf_reg)

  expect_equal(length(cls_curve), rf_cls$ntree)
  expect_equal(length(reg_curve), rf_reg$ntree)
  expect_true(all(is.finite(reg_curve)))
  expect_true(all(cls_curve >= 0 & cls_curve <= 1))
})


test_that("training curves are skipped when forests are discarded", {
  rf_plain <- randomForest::randomForest(
    Species ~ .,
    data = iris,
    ntree = 25,
    keep.forest = FALSE
  )
  expect_warning(
    gg_plain <- gg_error(rf_plain, training = TRUE),
    "Training error curve is unavailable"
  )
  expect_false("train" %in% names(gg_plain))
})


test_that("gg_vimp falls back to placeholder when importance is unavailable", {
  rf_noimp <- randomForest::randomForest(
    medv ~ .,
    data = Boston,
    ntree = 60,
    keep.forest = FALSE,
    importance = FALSE
  )
  rf_noimp$importance <- NULL
  expect_warning(gg_na <- gg_vimp(rf_noimp), "Returning NA values")
  expect_true(all(is.na(gg_na$vimp)))
  expect_true(all(is.na(gg_na$rel_vimp)))
})
