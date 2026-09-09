# Shape, factor handling and error paths for gg_ale_rfsrc().
#
# The value cross-checks -- that the curve is centered, and that a known
# imposed effect is recovered -- live in test_extractor_contracts.R with the
# other extractor cross-checks, because a shape assertion passes just as
# happily when the accumulation is wrong.

test_that("gg_ale_rfsrc returns the documented continuous shape", {
  skip_if_not_installed("randomForestSRC")
  set.seed(20260909L)
  air <- stats::na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = air, ntree = 50)

  g <- gg_ale_rfsrc(rf, xvar.names = c("Wind", "Temp"), n_eval = 10)

  expect_s3_class(g, "gg_ale_rfsrc")
  expect_named(g, c("continuous", "categorical"))
  expect_setequal(names(g$continuous), c("x", "yhat", "name"))
  expect_true(is.numeric(g$continuous$x))
  expect_setequal(unique(g$continuous$name), c("Wind", "Temp"))
  expect_equal(nrow(g$categorical), 0L)

  # One value per grid point, and the grid is the bin edges, so a k-bin
  # variable contributes k + 1 rows. unique() on the quantiles can collapse
  # ties, so this is an upper bound rather than an equality.
  expect_lte(sum(g$continuous$name == "Wind"), 11L)
  expect_gt(sum(g$continuous$name == "Wind"), 2L)
})

test_that("gg_ale_rfsrc keeps the model's factor level order, uncollapsed", {
  skip_if_not_installed("randomForestSRC")

  # Mirrors the gg_partial_rfsrc factor-collapse regression test. Levels are
  # deliberately NOT in alphabetical order, so a re-sort would be visible:
  # ground truth is C < A < B in level order but 0 < 5 < 10 in effect.
  set.seed(20260909L)
  n <- 200
  grp <- factor(sample(c("A", "B", "C"), n, TRUE), levels = c("C", "A", "B"))
  y <- c(A = 5, B = 10, C = 0)[as.character(grp)] + stats::rnorm(n, 0, 0.5)
  d <- data.frame(y = y, grp = grp, noise = stats::rnorm(n))
  rf <- randomForestSRC::rfsrc(y ~ ., data = d, ntree = 200)

  g <- gg_ale_rfsrc(rf, xvar.names = "grp")

  expect_equal(nrow(g$continuous), 0L)
  expect_s3_class(g$categorical$x, "factor")
  expect_equal(levels(g$categorical$x), c("C", "A", "B"))

  # Not collapsed to one value: the imposed effect spans 10 units.
  vals <- stats::setNames(g$categorical$yhat, as.character(g$categorical$x))
  expect_gt(diff(range(vals)), 3)

  # The imposed C < A < B ordering of the effect is recovered.
  expect_lt(vals[["C"]], vals[["A"]])
  expect_lt(vals[["A"]], vals[["B"]])
})

test_that("gg_ale_rfsrc handles a classification forest and which.class", {
  skip_if_not_installed("randomForestSRC")
  set.seed(20260909L)
  rf <- randomForestSRC::rfsrc(Species ~ ., data = iris, ntree = 50)

  g1 <- gg_ale_rfsrc(rf, xvar.names = "Petal.Length", n_eval = 8)
  g2 <- gg_ale_rfsrc(rf, xvar.names = "Petal.Length", n_eval = 8,
                     which.class = 2)

  expect_s3_class(g1, "gg_ale_rfsrc")
  # Different classes give different curves; a which.class that is ignored
  # would return the same numbers twice.
  expect_false(isTRUE(all.equal(g1$continuous$yhat, g2$continuous$yhat)))
})

test_that("gg_ale_rfsrc returns an interaction surface for a continuous pair", {
  skip_on_cran()
  skip_if_not_installed("randomForestSRC")
  set.seed(20260909L)
  air <- stats::na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = air, ntree = 50)

  g <- gg_ale_rfsrc(rf, xvar.names = "Wind", xvar2.name = "Temp", n_eval = 6)

  expect_s3_class(g, "gg_ale_interaction")
  expect_true(all(c("x", "y", "ale", "name1", "name2") %in% names(g)))
  expect_equal(unique(g$name1), "Wind")
  expect_equal(unique(g$name2), "Temp")
  # The surface is the full grid of edge pairs.
  expect_equal(nrow(g), length(unique(g$x)) * length(unique(g$y)))
})

test_that("gg_ale_rfsrc rejects unsupported input", {
  skip_if_not_installed("randomForestSRC")
  set.seed(20260909L)

  expect_error(
    gg_ale_rfsrc(list(), xvar.names = "x"),
    "expected an 'rfsrc' object"
  )

  rs <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = survival::veteran,
                               ntree = 30)
  expect_error(
    gg_ale_rfsrc(rs, xvar.names = "age"),
    "only regression and classification forests"
  )

  rf <- randomForestSRC::rfsrc(Sepal.Length ~ ., data = iris, ntree = 30)
  expect_error(
    gg_ale_rfsrc(rf, xvar.names = c("Petal.Width", "Sepal.Width"),
                 xvar2.name = "Petal.Length"),
    "must name exactly one predictor"
  )
  expect_error(
    gg_ale_rfsrc(rf, xvar.names = "Petal.Width", xvar2.name = "Species"),
    "two continuous predictors only"
  )
  expect_error(
    gg_ale_rfsrc(rf, xvar.names = "not_a_column"),
    "column names not found"
  )
})

test_that("plot.gg_ale_rfsrc and plot.gg_ale_interaction return, never print", {
  skip_if_not_installed("randomForestSRC")
  set.seed(20260909L)
  air <- stats::na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = air, ntree = 50)

  g <- gg_ale_rfsrc(rf, xvar.names = "Wind", n_eval = 6)
  expect_silent(p <- plot(g))
  expect_s3_class(p, "ggplot")

  gi <- gg_ale_rfsrc(rf, xvar.names = "Wind", xvar2.name = "Temp", n_eval = 5)
  expect_silent(pi <- plot(gi))
  expect_s3_class(pi, "ggplot")
})

test_that("print and summary methods report the ALE object without erroring", {
  skip_if_not_installed("randomForestSRC")
  set.seed(20260909L)
  air <- stats::na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = air, ntree = 50)

  g <- gg_ale_rfsrc(rf, xvar.names = "Wind", n_eval = 6)
  expect_output(print(g), "gg_ale_rfsrc")
  expect_output(print(summary(g)), "continuous: 1")

  gi <- gg_ale_rfsrc(rf, xvar.names = "Wind", xvar2.name = "Temp", n_eval = 5)
  expect_output(print(gi), "Wind x Temp")
  expect_output(print(summary(gi)), "interaction ALE range")
})
