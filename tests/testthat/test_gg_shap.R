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

test_that("shap_importance and plot(type='importance') return ggplots", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  expect_s3_class(shap_importance(gg_dta), "ggplot")
  expect_s3_class(plot(gg_dta, type = "importance"), "ggplot")
})

test_that("shap_beeswarm and default plot() return ggplots", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  expect_s3_class(shap_beeswarm(gg_dta), "ggplot")
  expect_s3_class(plot(gg_dta), "ggplot")   # default type = "beeswarm"
})

test_that("shap_beeswarm scales feature values per-variable into [0,1]", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  gg_plt <- shap_beeswarm(gg_dta)
  scaled <- gg_plt$data$value_scaled

  expect_true(all(scaled >= 0 & scaled <= 1, na.rm = TRUE))
  # each variable's scaled range should span close to [0,1] unless constant
  ranges <- tapply(scaled, gg_plt$data$vars, function(v) diff(range(v, na.rm = TRUE)))
  expect_true(all(ranges > 0.9, na.rm = TRUE))
})

test_that("shap_dependence honors xvar and defaults to top variable", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  expect_s3_class(shap_dependence(gg_dta, xvar = "Temp"), "ggplot")
  expect_s3_class(shap_dependence(gg_dta), "ggplot")            # NULL -> top var
  expect_error(shap_dependence(gg_dta, xvar = "not_a_var"), "not_a_var")
})

test_that("shap_dependence uses geom_boxplot for a categorical feature", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  dta <- na.omit(airquality)
  dta$hot <- factor(ifelse(dta$Temp > 80, "hot", "cool"))
  dta$Temp <- NULL  # avoid near-duplicate signal with Ozone predictors

  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = dta, ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  gg_plt <- shap_dependence(gg_dta, xvar = "hot")
  expect_s3_class(gg_plt, "ggplot")
  expect_s3_class(gg_plt$layers[[2]]$geom, "GeomBoxplot")
})

test_that("autoplot.gg_shap delegates to plot", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  expect_s3_class(ggplot2::autoplot(gg_dta), "ggplot")
})

test_that("gg_shap.rfsrc errors on out-of-range which.class", {
  skip_if_not_installed("kernelshap")

  rf <- randomForestSRC::rfsrc(Species ~ ., data = iris, ntree = 20)
  expect_error(gg_shap(rf, which.class = 99), "which.class")
})

test_that("gg_shap.randomForest errors on out-of-range which.class", {
  skip_if_not_installed("kernelshap")

  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 20)
  expect_error(gg_shap(rf, which.class = 99), "which.class")
})

test_that("gg_shap.rfsrc validates bg_n", {
  skip_if_not_installed("kernelshap")
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality), ntree = 20)
  expect_error(gg_shap(rf, bg_n = 0), "bg_n")
  expect_error(gg_shap(rf, bg_n = NA), "bg_n")
  expect_error(gg_shap(rf, bg_n = c(10, 20)), "bg_n")
})

test_that("gg_shap.randomForest validates bg_n", {
  skip_if_not_installed("kernelshap")
  rf <- randomForest::randomForest(Ozone ~ ., data = na.omit(airquality), ntree = 20)
  expect_error(gg_shap(rf, bg_n = 0), "bg_n")
  expect_error(gg_shap(rf, bg_n = NA), "bg_n")
})

test_that("shap_beeswarm scales finite values even when a variable has Inf entries", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality), ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  # inject an Inf into one row of one variable's `value` column
  temp_rows <- which(as.character(gg_dta$vars) == "Temp")
  gg_dta$value[temp_rows[1]] <- Inf

  gg_plt <- shap_beeswarm(gg_dta)
  temp_scaled <- gg_plt$data$value_scaled[as.character(gg_plt$data$vars) == "Temp"]

  # not everything should be NA -- the finite Temp values should still scale
  expect_false(all(is.na(temp_scaled)))
  expect_true(any(!is.na(temp_scaled) & temp_scaled >= 0 & temp_scaled <= 1))
})

## ── vdiffr snapshots — see test_snapshots.R ──────────────────────────────────
## Visual regression tests for the three plot.gg_shap types are in
## test_snapshots.R (guarded by VDIFFR_RUN_TESTS=true), following the package
## convention.
