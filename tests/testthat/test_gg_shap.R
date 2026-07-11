test_that("gg_shap.rfsrc returns a long tidy object for regression", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  dta <- na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = dta, ntree = 50)

  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  expect_s3_class(gg_dta, "gg_shap")
  expect_true(all(c("id", "vars", "shap", "value", "value_label") %in%
                    colnames(gg_dta)))

  n_obs  <- nrow(rf$xvar)
  n_vars <- ncol(rf$xvar)
  expect_equal(nrow(gg_dta), n_obs * n_vars)
  expect_type(gg_dta$shap, "double")
  expect_true(is.factor(gg_dta$vars))
})

test_that("gg_shap.default errors on a non-forest object", {
  expect_error(gg_shap(lm(mpg ~ wt, mtcars)), "rfsrc.*randomForest")
})

test_that("gg_shap.rfsrc errors on survival forests", {
  skip_if_not_installed("kernelshap")
  data(veteran, package = "randomForestSRC")
  rf <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = veteran,
                               ntree = 20)
  expect_error(gg_shap(rf), "regression and classification")
})

test_that("gg_shap.rfsrc handles classification via which.class", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  rf <- randomForestSRC::rfsrc(Species ~ ., data = iris, ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20, which.class = 2)

  expect_s3_class(gg_dta, "gg_shap")
  expect_equal(nrow(gg_dta), nrow(iris) * 4L)
  expect_equal(attr(gg_dta, "which.class"), 2)
})

test_that("gg_shap.randomForest works for regression", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  dta <- na.omit(airquality)
  rf <- randomForest::randomForest(Ozone ~ ., data = dta, ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  expect_s3_class(gg_dta, "gg_shap")
  expect_true(all(c("id", "vars", "shap") %in% colnames(gg_dta)))
})

test_that("gg_shap.randomForest handles classification via which.class", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20, which.class = 2)

  expect_s3_class(gg_dta, "gg_shap")
  expect_equal(nrow(gg_dta), nrow(iris) * 4L)
  expect_equal(attr(gg_dta, "which.class"), 2)
})
