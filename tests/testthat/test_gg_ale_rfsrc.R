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

test_that("gg_ale_rfsrc keeps a low-cardinality numeric predictor numeric", {
  # Catches: imposing the counterfactual level as a factor on a numeric column.
  # .ale_is_categorical() routes ANY predictor with fewer than cat_limit unique
  # values down the categorical path, so a 0/1 indicator lands there while
  # still being numeric. Substituting a factor changes the column's type, and
  # the forest then scores a variable it was not fit on -- silently, because
  # predict() still returns numbers. The symptom is a flat curve: the effect
  # measured 0 against a true 9.85 before this was fixed.
  skip_if_not_installed("randomForestSRC")
  set.seed(20260909L)
  n <- 300
  d <- data.frame(bin = stats::rbinom(n, 1, 0.5), noise = stats::rnorm(n))
  d$y <- 10 * d$bin + stats::rnorm(n, sd = 0.5)
  rf <- randomForestSRC::rfsrc(y ~ ., data = d, ntree = 200)

  g <- gg_ale_rfsrc(rf, xvar.names = "bin")

  # Ground truth from the forest itself, not from a stored number: score every
  # observation at bin = 0 and at bin = 1 and take the mean difference.
  d0 <- d
  d0$bin <- 0
  d1 <- d
  d1$bin <- 1
  truth <- mean(stats::predict(rf, d1)$predicted -
                  stats::predict(rf, d0)$predicted)

  expect_equal(diff(range(g$categorical$yhat)), truth, tolerance = 0.1)
  expect_gt(diff(range(g$categorical$yhat)), 5)
})

test_that("gg_ale_rfsrc uses the model's factor levels, not newx's", {
  # Catches: reading the grid from droplevels(newx[[x]]) instead of the fitted
  # model. A newx that omits one level, or carries a relevelled copy, would
  # then silently reorder the curve while every shape assertion still passed.
  skip_if_not_installed("randomForestSRC")
  set.seed(20260909L)
  n <- 240
  grp <- factor(sample(c("A", "B", "C"), n, TRUE), levels = c("C", "A", "B"))
  d <- data.frame(y = c(A = 5, B = 10, C = 0)[as.character(grp)] +
                    stats::rnorm(n, 0, 0.5),
                  grp = grp, noise = stats::rnorm(n))
  rf <- randomForestSRC::rfsrc(y ~ ., data = d, ntree = 200)

  # newx relevelled alphabetically; the model's order is still C, A, B.
  newx <- rf$xvar
  newx$grp <- factor(as.character(newx$grp), levels = c("A", "B", "C"))

  g <- gg_ale_rfsrc(rf, xvar.names = "grp", newx = newx)

  expect_equal(levels(g$categorical$x), c("C", "A", "B"))
})

test_that("gg_ale_rfsrc survives a quantile grid that collapses to one bin", {
  # Catches: nested apply() in the interaction accumulation. apply() drops the
  # dimension when an axis has a single bin, and the outer call then failed
  # with "dim(X) must have a positive length". Tied values collapse quantile
  # edges, so a predictor with most of its mass at one value reaches this for
  # any modest n_eval -- it is not an exotic input.
  skip_on_cran()
  skip_if_not_installed("randomForestSRC")
  set.seed(20260909L)
  n <- 200
  tied <- c(rep(0, round(0.91 * n)), sample(1:9, n - round(0.91 * n), TRUE))
  d <- data.frame(tied = tied, x2 = stats::runif(n, 0, 10))
  d$y <- d$tied + d$x2 + stats::rnorm(n, sd = 0.1)
  rf <- randomForestSRC::rfsrc(y ~ ., data = d, ntree = 100)

  expect_no_error(
    g <- gg_ale_rfsrc(rf, xvar.names = "tied", xvar2.name = "x2",
                      n_eval = 2, cat_limit = 3)
  )
  expect_s3_class(g, "gg_ale_interaction")
  expect_gt(nrow(g), 0L)
})

test_that("plot.gg_ale_interaction sizes cells to the irregular grid", {
  # Catches: geom_raster(), or a geom_tile() with no explicit width/height.
  # Both impose one constant cell size -- geom_tile() takes it from the
  # smallest gap -- so on a quantile grid the wide cells shrink to the
  # narrowest and the surface renders as scattered tiles with gaps. The
  # rendered extent is xmin/xmax, not the layer's `width` column.
  skip_on_cran()
  skip_if_not_installed("randomForestSRC")
  set.seed(20260909L)
  air <- stats::na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = air, ntree = 50)

  g <- gg_ale_rfsrc(rf, xvar.names = "Wind", xvar2.name = "Temp", n_eval = 6)
  built <- ggplot2::ggplot_build(plot(g))$data[[1]]

  # The grid is genuinely uneven, so the cells must be too.
  expect_gt(length(unique(round(diff(sort(unique(g$x))), 6))), 1L)
  expect_gt(length(unique(round(built$xmax - built$xmin, 6))), 1L)
  expect_gt(length(unique(round(built$ymax - built$ymin, 6))), 1L)

  # Cells abut: every gap between adjacent grid points is fully covered, so
  # the drawn surface spans at least the data range.
  expect_gte(diff(range(c(built$xmin, built$xmax))), diff(range(g$x)))
  expect_gte(diff(range(c(built$ymin, built$ymax))), diff(range(g$y)))
})

test_that("gg_ale_rfsrc rejects a newx missing training predictors", {
  # Catches: validating only that supplied names are known, which lets a
  # column subset through to fail inside predict.rfsrc() with a message that
  # names no column.
  skip_if_not_installed("randomForestSRC")
  set.seed(20260909L)
  air <- stats::na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = air, ntree = 30)

  expect_error(
    gg_ale_rfsrc(rf, xvar.names = "Wind", newx = air[, c("Wind", "Temp")]),
    "missing 3 predictor"
  )
  expect_error(
    gg_ale_rfsrc(rf, xvar.names = "Wind", newx = air[, c("Wind", "Temp")]),
    "Solar.R"
  )

  bad <- rf$xvar
  bad$not_a_predictor <- 1
  expect_error(
    gg_ale_rfsrc(rf, xvar.names = "Wind", newx = bad),
    "not trained on: not_a_predictor"
  )
  expect_error(
    gg_ale_rfsrc(rf, xvar.names = "Wind", newx = as.matrix(air)),
    "must be a data.frame"
  )
})

test_that("gg_ale_rfsrc treats an empty xvar.names as missing", {
  # Catches: character(0) falling through validation to fail inside
  # do.call("rbind", list()) with base R's "argument is of length zero", which
  # names neither the argument nor the function. Easy to hit from
  # programmatic use, where xvar.names comes from a filter that matched
  # nothing.
  skip_if_not_installed("randomForestSRC")
  set.seed(20260909L)
  air <- stats::na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = air, ntree = 30)

  expect_error(gg_ale_rfsrc(rf, xvar.names = character(0)),
               "'xvar.names' is required")
  expect_error(gg_ale_rfsrc(rf, xvar.names = NULL),
               "'xvar.names' is required")
})

test_that("gg_ale_rfsrc does not count NA toward the cat_limit cardinality", {
  # Catches: unique() including NA, so a predictor with cat_limit - 1 genuine
  # values plus some missingness reads as continuous and is binned by
  # quantile instead of being treated as categorical. make_eval_grid() drops
  # NA before the same check, so the two routes would classify the same
  # column differently -- a disagreement between gg_partial_rfsrc() and
  # gg_ale_rfsrc() on the same data.
  skip_if_not_installed("randomForestSRC")
  set.seed(20260909L)
  n <- 300
  v <- sample(1:9, n, TRUE)
  v[sample(n, 20)] <- NA
  d <- data.frame(v = v, noise = stats::rnorm(n))
  d$y <- d$v + stats::rnorm(n, sd = 0.2)

  # 9 genuine values, 10 counting NA: the boundary the bug sat on.
  expect_equal(length(unique(v)), 10L)
  expect_equal(length(unique(v[!is.na(v)])), 9L)

  expect_true(ggRandomForests:::.ale_is_categorical(v, 10))
  expect_false(ggRandomForests:::.ale_is_categorical(v, 9))

  rf <- randomForestSRC::rfsrc(y ~ ., data = stats::na.omit(d), ntree = 100)
  g <- gg_ale_rfsrc(rf, xvar.names = "v", cat_limit = 10)
  expect_gt(nrow(g$categorical), 0L)
  expect_equal(nrow(g$continuous), 0L)
})
