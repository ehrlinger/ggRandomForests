# Smoke tests for autoplot.gg_* methods.

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

test_that("autoplot returns a ggplot for gg_error", {
  f <- setup_forests()
  expect_s3_class(ggplot2::autoplot(gg_error(f$reg)), "ggplot")
})

test_that("autoplot returns a ggplot for gg_vimp", {
  f <- setup_forests()
  expect_s3_class(ggplot2::autoplot(gg_vimp(f$reg)), "ggplot")
})

test_that("autoplot returns a ggplot for gg_rfsrc", {
  f <- setup_forests()
  expect_s3_class(ggplot2::autoplot(gg_rfsrc(f$reg)), "ggplot")
})

test_that("autoplot returns a ggplot for gg_variable", {
  f <- setup_forests()
  expect_s3_class(
    ggplot2::autoplot(gg_variable(f$reg), xvar = "Temp"),
    "ggplot"
  )
})

test_that("autoplot returns a ggplot for gg_partial", {
  f <- setup_forests()
  pv <- randomForestSRC::plot.variable(f$reg, partial = TRUE,
                                        show.plots = FALSE)
  expect_s3_class(ggplot2::autoplot(gg_partial(pv)), "ggplot")
})

test_that("autoplot returns a ggplot for gg_partial_rfsrc", {
  f <- setup_forests()
  expect_s3_class(
    ggplot2::autoplot(gg_partial_rfsrc(f$reg, xvar.names = "Temp")),
    "ggplot"
  )
})

test_that("autoplot returns a ggplot for gg_roc", {
  f <- setup_forests()
  expect_s3_class(
    ggplot2::autoplot(gg_roc(f$cls, which_outcome = 1)),
    "ggplot"
  )
})

test_that("autoplot returns a ggplot for gg_survival", {
  data(pbc, package = "randomForestSRC")
  expect_s3_class(
    ggplot2::autoplot(
      gg_survival(interval = "days", censor = "status", data = pbc)
    ),
    "ggplot"
  )
})

test_that("autoplot returns a ggplot for gg_brier", {
  f <- setup_forests()
  expect_s3_class(ggplot2::autoplot(gg_brier(f$srv)), "ggplot")
})
