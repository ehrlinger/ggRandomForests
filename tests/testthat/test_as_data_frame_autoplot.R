# Tests for as.data.frame.gg_partial* and autoplot.gg_* methods.

setup_forests <- function() {
  data(airquality, package = "datasets")
  data(iris,       package = "datasets")
  data(pbc,        package = "randomForestSRC")
  set.seed(1)
  list(
    reg = randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                                 ntree = 50, importance = TRUE,
                                 tree.err = TRUE),
    cls = randomForestSRC::rfsrc(Species ~ ., data = iris,
                                 ntree = 50, importance = TRUE,
                                 tree.err = TRUE),
    srv = randomForestSRC::rfsrc(Surv(days, status) ~ ., data = pbc,
                                 ntree = 50, nsplit = 10,
                                 importance = TRUE, tree.err = TRUE)
  )
}

# ---------------------------------------------------------------------------
# as.data.frame — gg_partial family
# ---------------------------------------------------------------------------
test_that("as.data.frame.gg_partial returns a data.frame with type column", {
  f <- setup_forests()
  pv <- randomForestSRC::plot.variable(f$reg, partial = TRUE,
                                        show.plots = FALSE)
  gp <- gg_partial(pv)

  df <- as.data.frame(gp)

  expect_s3_class(df, "data.frame")
  expect_true("type" %in% names(df))
  expect_true(all(df$type %in% c("continuous", "categorical")))
  # Original object still intact
  expect_s3_class(gp, "gg_partial")
})

test_that("as.data.frame.gg_partial_rfsrc returns a data.frame with type column", {
  f <- setup_forests()
  gp <- gg_partial_rfsrc(f$reg, xvar.names = c("Temp", "Wind"))

  df <- as.data.frame(gp)

  expect_s3_class(df, "data.frame")
  expect_true("type" %in% names(df))
  expect_true(all(df$type %in% c("continuous", "categorical")))
})

test_that("as.data.frame preserves provenance attribute", {
  f <- setup_forests()
  gp <- gg_partial_rfsrc(f$reg, xvar.names = "Temp")

  df <- as.data.frame(gp)

  expect_false(is.null(attr(df, "provenance")))
  expect_equal(attr(df, "provenance")$source, "randomForestSRC")
})

test_that("as.data.frame result has nrow > 0", {
  f <- setup_forests()
  pv <- randomForestSRC::plot.variable(f$reg, partial = TRUE,
                                        show.plots = FALSE)
  df <- as.data.frame(gg_partial(pv))
  expect_gt(nrow(df), 0L)
})

# ---------------------------------------------------------------------------
# autoplot — all 10 classes
# ---------------------------------------------------------------------------
test_that("autoplot returns a ggplot for gg_error", {
  f <- setup_forests()
  p <- ggplot2::autoplot(gg_error(f$reg))
  expect_s3_class(p, "ggplot")
})

test_that("autoplot returns a ggplot for gg_vimp", {
  f <- setup_forests()
  p <- ggplot2::autoplot(gg_vimp(f$reg))
  expect_s3_class(p, "ggplot")
})

test_that("autoplot returns a ggplot for gg_rfsrc", {
  f <- setup_forests()
  p <- ggplot2::autoplot(gg_rfsrc(f$reg))
  expect_s3_class(p, "ggplot")
})

test_that("autoplot returns a ggplot for gg_variable", {
  f <- setup_forests()
  p <- ggplot2::autoplot(gg_variable(f$reg), xvar = "Temp")
  expect_s3_class(p, "ggplot")
})

test_that("autoplot returns a ggplot (or list of ggplots) for gg_partial", {
  f <- setup_forests()
  pv <- randomForestSRC::plot.variable(f$reg, partial = TRUE,
                                        show.plots = FALSE)
  p <- ggplot2::autoplot(gg_partial(pv))
  # plot.gg_partial returns a single ggplot when only one variable type is
  # present, or a named list(continuous=, categorical=) when both are.
  if (is.list(p) && !inherits(p, "ggplot")) {
    expect_true(all(vapply(p, inherits, logical(1), "ggplot")))
  } else {
    expect_s3_class(p, "ggplot")
  }
})

test_that("autoplot returns a ggplot for gg_partial_rfsrc", {
  f <- setup_forests()
  p <- ggplot2::autoplot(gg_partial_rfsrc(f$reg, xvar.names = "Temp"))
  expect_s3_class(p, "ggplot")
})

test_that("autoplot returns a ggplot for gg_roc", {
  f <- setup_forests()
  p <- ggplot2::autoplot(gg_roc(f$cls, which_outcome = 1))
  expect_s3_class(p, "ggplot")
})

test_that("autoplot returns a ggplot for gg_survival", {
  data(pbc, package = "randomForestSRC")
  p <- ggplot2::autoplot(
    gg_survival(interval = "days", censor = "status", data = pbc)
  )
  expect_s3_class(p, "ggplot")
})

test_that("autoplot returns a ggplot for gg_brier", {
  f <- setup_forests()
  p <- ggplot2::autoplot(gg_brier(f$srv))
  expect_s3_class(p, "ggplot")
})
