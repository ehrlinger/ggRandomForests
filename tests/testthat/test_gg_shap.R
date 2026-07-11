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
