# testthat for gg_variable function

# Survival formula helper (rfsrc requires Surv to be in local scope)
Surv <- survival::Surv # nolint: object_name_linter

test_that("gg_variable classifications", {
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
  expect_equal(rfsrc_iris$family, "class")

  ## Create the correct gg_error object
  gg_dta <- gg_variable(rfsrc_iris)

  # Test object type
  expect_s3_class(gg_dta, "gg_variable")

  ## Ensure non-OOB predictions keep predictor columns
  gg_full <- gg_variable(rfsrc_iris, oob = FALSE)
  expect_true(all(rfsrc_iris$xvar.names %in% names(gg_full)))

  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta, xvar = "Petal.Width")

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta, xvar = rfsrc_iris$xvar.names)

  # Test return is a single plottable object (patchwork composite or ggplot)
  expect_true(inherits(gg_plt, "patchwork") || inherits(gg_plt, "ggplot"))
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta, xvar = rfsrc_iris$xvar.names,
                             panel = TRUE)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  rf_iris <- randomForest::randomForest(Species ~ .,
                                        data = iris)

  ## Create the correct gg_error object
  gg_dta <- gg_variable(rf_iris)

  # Test object type
  expect_s3_class(gg_dta, "gg_variable")

  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta, xvar = "Petal.Width")

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")
  gg_plt <- plot(gg_dta)

  ## Ensure we can rebuild training data for subset() calls
  iris_two <- droplevels(subset(iris, Species != "setosa"))
  rf_subset <- randomForest::randomForest(Species ~ .,
                                          data = iris_two,
                                          ntree = 60)
  gg_subset <- gg_variable(rf_subset)
  expect_s3_class(gg_subset, "gg_variable")
  expect_true(all(c("Sepal.Length", "Sepal.Width") %in% names(gg_subset)))

})


test_that("gg_variable regression", {
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

  ## Create the correct gg_error object
  gg_dta <- gg_variable(rfsrc_boston)

  # Test object type
  expect_s3_class(gg_dta, "gg_variable")

  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta)

  # Test return is a single plottable object (patchwork composite or ggplot)
  expect_true(inherits(gg_plt, "patchwork") || inherits(gg_plt, "ggplot"))


  ## Test plotting the gg_error object
  expect_warning(gg_plt <- plot.gg_variable(gg_dta, panel = TRUE))
  expect_s3_class(gg_plt, "ggplot")


  data(Boston, package = "MASS")
  rf_boston <- randomForest::randomForest(medv ~ ., data = Boston)
  gg_dta <- gg_variable(rf_boston)

  # Test object type
  expect_s3_class(gg_dta, "gg_variable")

  expect_warning(gg_plt <- plot(gg_dta, panel = TRUE))
  expect_s3_class(gg_plt, "ggplot")

})

test_that("gg_variable survival handles late time requests", {
  data(veteran, package = "randomForestSRC")
  Surv <- survival::Surv # nolint: object_name_linter
  rfsrc_veteran <- randomForestSRC::rfsrc(Surv(time, status) ~ .,
                                          data = veteran,
                                          ntree = 50,
                                          nsplit = 5)
  late_time <- max(rfsrc_veteran$time.interest) + 50
  expect_silent(gg_dta <- gg_variable(rfsrc_veteran, time = late_time))
  expect_s3_class(gg_dta, "gg_variable")
})

test_that("gg_variable survival: single time, single continuous variable plot", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  gg_dta <- gg_variable(rfsrc_veteran, time = 90)
  expect_s3_class(gg_dta, "gg_variable")

  gg_plt <- plot(gg_dta, xvar = "age")
  expect_s3_class(gg_plt, "ggplot")
})

test_that("gg_variable survival: single time, panel plot", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  gg_dta <- gg_variable(rfsrc_veteran, time = 90)
  gg_plt <- plot(gg_dta, xvar = c("age", "diagtime"), panel = TRUE)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("gg_variable survival: multiple times, single variable facets", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  gg_dta <- gg_variable(rfsrc_veteran, time = c(30, 90, 365))
  expect_s3_class(gg_dta, "gg_variable")

  # Single xvar → plot.gg_variable returns a single ggplot (not a list)
  gg_plt <- plot(gg_dta, xvar = "age")
  expect_s3_class(gg_plt, "ggplot")

  # Multiple xvars → returns a single plottable object (patchwork composite)
  gg_plt2 <- plot(gg_dta, xvar = c("age", "diagtime"))
  expect_true(inherits(gg_plt2, "patchwork") || inherits(gg_plt2, "ggplot"))
})

test_that("gg_variable survival: multiple times, panel plot", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  gg_dta <- gg_variable(rfsrc_veteran, time = c(30, 90))
  gg_plt <- plot(gg_dta, xvar = c("age", "diagtime"), panel = TRUE)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("gg_variable survival: points=FALSE smooth=TRUE options", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  gg_dta <- gg_variable(rfsrc_veteran, time = 90)

  gg_plt <- plot(gg_dta, xvar = "age", points = FALSE, smooth = TRUE)
  expect_s3_class(gg_plt, "ggplot")

  gg_plt2 <- plot(gg_dta, xvar = "age", points = TRUE, smooth = FALSE)
  expect_s3_class(gg_plt2, "ggplot")
})

test_that("gg_variable survival: oob=FALSE uses in-bag predictions", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  gg_dta <- gg_variable(rfsrc_veteran, time = 90, oob = FALSE)
  expect_s3_class(gg_dta, "gg_variable")
})

test_that("plot.gg_variable regression: points=FALSE smooth=TRUE", {
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  rfsrc_boston <- randomForestSRC::rfsrc(
    medv ~ .,
    data = boston,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_boston)

  gg_plt <- plot(gg_dta, xvar = "lstat", points = FALSE, smooth = TRUE)
  expect_s3_class(gg_plt, "ggplot")

  gg_plt2 <- plot(gg_dta, xvar = "lstat", points = FALSE, smooth = FALSE)
  expect_s3_class(gg_plt2, "ggplot")
})

test_that("plot.gg_variable regression: factor x variable triggers boxplot", {
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  rfsrc_boston <- randomForestSRC::rfsrc(
    medv ~ .,
    data = boston,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_boston)
  gg_dta$chas <- factor(gg_dta$chas)

  gg_plt <- plot(gg_dta, xvar = "chas")
  expect_s3_class(gg_plt, "ggplot")
})

test_that("plot.gg_variable regression: panel with two continuous variables", {
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  rfsrc_boston <- randomForestSRC::rfsrc(
    medv ~ .,
    data = boston,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_boston)
  gg_plt <- plot(gg_dta, xvar = c("lstat", "rm"), panel = TRUE)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("plot.gg_variable regression: panel points=FALSE smooth=TRUE", {
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  rfsrc_boston <- randomForestSRC::rfsrc(
    medv ~ .,
    data = boston,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_boston)
  gg_plt <- plot(gg_dta, xvar = c("lstat", "rm"),
                 panel = TRUE, points = FALSE, smooth = TRUE)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("plot.gg_variable classification: panel with multiple continuous vars", {
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_iris)
  gg_plt <- plot(gg_dta, xvar = c("Petal.Width", "Sepal.Width"), panel = TRUE)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("plot.gg_variable classification: smooth and no-points path", {
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_iris)
  gg_plt <- plot(gg_dta, xvar = "Petal.Width", points = FALSE, smooth = TRUE)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("plot.gg_variable: missing xvar returns list for all predictors", {
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  rfsrc_boston <- randomForestSRC::rfsrc(
    medv ~ .,
    data = boston,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_boston)
  gg_plt <- plot(gg_dta)
  # Returns a single plottable object (patchwork composite for multiple predictors)
  expect_true(inherits(gg_plt, "patchwork") || inherits(gg_plt, "ggplot"))
})

test_that("gg_variable.randomForest classification: class attr uses 'class' not 'classification'", {
  # randomForest stores the family in $type as "classification", but
  # plot.gg_variable and the rfsrc path both dispatch on "class".
  # Verify the mapping is applied so callers see a consistent class attribute.
  set.seed(42)
  rf_iris <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg_dta  <- gg_variable(rf_iris)

  expect_true("class"          %in% class(gg_dta))
  expect_false("classification" %in% class(gg_dta))
  expect_s3_class(gg_dta, "gg_variable")
})

## ── randomForest classification (PR #87) ─────────────────────────────────────

test_that("gg_variable.randomForest classification: produces yhat.* columns not yhat", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg <- gg_variable(rf)
  # Must have one column per class
  expect_true(all(c("yhat.setosa", "yhat.versicolor", "yhat.virginica")
                  %in% names(gg)))
  # Must NOT have a bare yhat column for multi-class
  expect_false("yhat" %in% names(gg))
  # Observed-class column must be present
  expect_true("yvar" %in% names(gg))
  # Vote fractions must be in [0, 1] and row-sum to ~1
  vote_cols <- c("yhat.setosa", "yhat.versicolor", "yhat.virginica")
  expect_true(all(gg[, vote_cols] >= 0))
  expect_true(all(gg[, vote_cols] <= 1))
  expect_true(all(abs(rowSums(gg[, vote_cols]) - 1) < 1e-6))
})

test_that("gg_variable.randomForest classification: plot returns patchwork for all xvar", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg <- gg_variable(rf)
  p  <- plot(gg)
  # iris has 4 predictors so the no-xvar default assembles a multi-panel
  # patchwork; assert patchwork specifically to catch regressions to a bare
  # list (#80).
  expect_s3_class(p, "patchwork")
})

test_that("gg_variable.randomForest classification: layer_data works on single-xvar plot", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg <- gg_variable(rf)
  p  <- plot(gg, xvar = "Sepal.Length")
  expect_no_error(ggplot2::layer_data(p, 1L))
})

test_that("gg_variable.randomForest classification: norm.votes=FALSE still gives [0,1] fractions", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L,
                                   norm.votes = FALSE)
  gg <- gg_variable(rf)
  vote_cols <- c("yhat.setosa", "yhat.versicolor", "yhat.virginica")
  expect_true(all(c("yhat.setosa", "yhat.versicolor", "yhat.virginica") %in% names(gg)))
  expect_true(all(gg[, vote_cols] >= 0))
  expect_true(all(gg[, vote_cols] <= 1))
  expect_true(all(abs(rowSums(gg[, vote_cols]) - 1) < 1e-6))
})

test_that("plot.gg_variable RF classification: smooth=TRUE layer_data smokeable (binary smooth aes bug)", {
  skip_if_not_installed("randomForest")
  # Two-class subset to exercise the *binary* classification path
  set.seed(42L)
  bin_data        <- iris[iris$Species != "virginica", ]
  bin_data$Species <- droplevels(bin_data$Species)
  rf  <- randomForest::randomForest(Species ~ ., data = bin_data, ntree = 50L)
  gg  <- gg_variable(rf)
  p   <- plot(gg, xvar = "Sepal.Length", smooth = TRUE)
  # Before the fix, geom_smooth(...)  has no aes and layer_data errors with
  # "stat_smooth() requires the following missing aesthetics: x and y"
  expect_no_error(ggplot2::layer_data(p, 2L))
})

test_that("plot.gg_variable RF classification: smooth=TRUE works for multi-class (missing block)", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg <- gg_variable(rf)
  # Before the fix the multi-class numeric path silently skips smooth=TRUE
  # but does not error; after the fix a smooth layer is present (layer 2).
  p  <- plot(gg, xvar = "Sepal.Length", smooth = TRUE)
  expect_s3_class(p, "ggplot")
  ld <- ggplot2::layer_data(p, 2L)   # layer 2 = geom_smooth
  expect_gt(nrow(ld), 0L)
})

test_that("plot.gg_variable RF classification multi-class: outcome column is class names not integers", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg <- gg_variable(rf)
  p  <- plot(gg, xvar = "Sepal.Length")
  expect_s3_class(p, "ggplot")
  # The 'outcome' column in the plot data drives facet labels.
  # It must contain class names, not integer indices.
  # ggplot2 >= 3.5 uses S7 slots; fall back to $ accessor for older versions.
  pd <- tryCatch(p@data, error = function(e) p$data)
  expect_false(is.numeric(pd$outcome))
  expect_true(all(c("setosa", "versicolor", "virginica") %in% as.character(pd$outcome)))
})

test_that("plot.gg_variable RF classification multi-class: outcome factor levels match column order", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg <- gg_variable(rf)
  p  <- plot(gg, xvar = "Sepal.Length")
  pd <- tryCatch(p@data, error = function(e) p$data)
  # Levels must follow the yhat.* column order in gg_variable output,
  # not alphabetical order (which factor() would impose by default).
  expected_levels <- sub("^yhat\\.", "", grep("^yhat\\.", names(gg), value = TRUE))
  expect_equal(levels(pd$outcome), expected_levels)
})

test_that("gg_variable.randomForest: oob=FALSE triggers a warning", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  # oob=FALSE is not supported for randomForest; a warning must be emitted
  # and OOB vote fractions are still returned.
  expect_warning(
    gg <- gg_variable(rf, oob = FALSE),
    regexp = "oob = FALSE is not supported"
  )
  expect_s3_class(gg, "gg_variable")
  expect_true("yhat.setosa" %in% names(gg))
})

## ── smooth=TRUE is a no-op for factor predictors (all families) ──────────────

# geom_smooth requires a continuous x-axis, so smooth=TRUE has no meaning for a
# factor predictor.  The contract these tests lock in: no GeomSmooth layer is
# added for a factor x.  Assert that directly (rather than a brittle layer
# count) so benign plot-composition changes do not break the suite.
has_smooth_layer <- function(p) {
  any(vapply(p$layers, function(l) inherits(l$geom, "GeomSmooth"), logical(1)))
}

test_that("plot.gg_variable binary classification + factor predictor: smooth=TRUE adds no smooth layer", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  bin_data         <- iris[iris$Species != "virginica", ]
  bin_data$Species <- droplevels(bin_data$Species)
  bin_data$size    <- cut(bin_data$Petal.Length, 2L, labels = c("small", "large"))
  rf  <- randomForest::randomForest(Species ~ size + Sepal.Width, data = bin_data,
                                    ntree = 25L)
  gg  <- gg_variable(rf)
  # smooth=TRUE with a factor predictor must not error
  expect_no_error(p <- plot(gg, xvar = "size", smooth = TRUE))
  expect_s3_class(p, "ggplot")
  expect_false(has_smooth_layer(p))
})

test_that("plot.gg_variable multi-class classification + factor predictor: smooth=TRUE adds no smooth layer", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  iris2      <- iris
  iris2$size <- cut(iris2$Petal.Length, 3L, labels = c("small", "medium", "large"))
  rf <- randomForest::randomForest(Species ~ size + Sepal.Width, data = iris2,
                                   ntree = 25L)
  gg <- gg_variable(rf)
  # smooth=TRUE with a factor predictor must not error
  expect_no_error(p <- plot(gg, xvar = "size", smooth = TRUE))
  expect_s3_class(p, "ggplot")
  expect_false(has_smooth_layer(p))
})

test_that("plot.gg_variable regression + factor predictor: smooth=TRUE adds no smooth layer", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  iris2      <- iris
  iris2$size <- cut(iris2$Petal.Length, 3L, labels = c("small", "medium", "large"))
  rf <- randomForest::randomForest(Sepal.Length ~ size + Sepal.Width, data = iris2,
                                   ntree = 25L)
  gg <- gg_variable(rf)
  # smooth=TRUE with a factor predictor must not error
  expect_no_error(p <- plot(gg, xvar = "size", smooth = TRUE))
  expect_s3_class(p, "ggplot")
  expect_false(has_smooth_layer(p))
})
