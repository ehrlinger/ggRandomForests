test_that("gg_partial_rfsrc imposes factor levels correctly (not collapsed)", {
  skip_if_not_installed("randomForestSRC")

  # A factor with a strong, monotone per-level effect. partial.rfsrc() imposes
  # a factor level by its integer code; passing the *labels* collapses every
  # level to one value (the bug this test guards). Ground truth: A < B < C.
  set.seed(101)
  n   <- 200
  grp <- factor(sample(c("A", "B", "C"), n, TRUE))
  y   <- c(A = 0, B = 5, C = 10)[as.character(grp)] + stats::rnorm(n, 0, 0.5)
  d   <- data.frame(y = y, grp = grp, noise = stats::rnorm(n))
  rf  <- randomForestSRC::rfsrc(y ~ ., data = d, ntree = 200)

  g     <- gg_partial_rfsrc(rf, xvar.names = "grp")
  means <- tapply(g$categorical$yhat, g$categorical$x, mean)

  # All three levels are present and labelled (not integer codes).
  expect_setequal(names(means), c("A", "B", "C"))

  # The levels must NOT collapse to a single value.
  expect_gt(diff(range(means)), 3)

  # The A < B < C ordering of the imposed effect is recovered.
  expect_lt(means[["A"]], means[["B"]])
  expect_lt(means[["B"]], means[["C"]])
})

test_that("gg_partial_rfsrc factor partial dependence matches ground truth", {
  skip_if_not_installed("randomForestSRC")

  set.seed(202)
  n   <- 200
  grp <- factor(sample(c("lo", "mid", "hi"), n, TRUE))
  y   <- c(lo = 1, mid = 4, hi = 7)[as.character(grp)] + stats::rnorm(n, 0, 0.4)
  d   <- data.frame(y = y, grp = grp, noise = stats::rnorm(n))
  rf  <- randomForestSRC::rfsrc(y ~ ., data = d, ntree = 300)

  lv    <- levels(grp)
  truth <- vapply(lv, function(lev) {
    dd <- d
    dd$grp <- factor(lev, levels = lv)
    mean(predict(rf, newdata = dd)$predicted)
  }, numeric(1))

  g     <- gg_partial_rfsrc(rf, xvar.names = "grp")
  means <- tapply(g$categorical$yhat, g$categorical$x, mean)[lv]

  # gg_partial_rfsrc should track the ground-truth partial dependence.
  expect_equal(as.numeric(means), as.numeric(truth), tolerance = 0.15)

  # x is returned as a factor in the model's level order (lo, mid, hi), not
  # alphabetical (hi, lo, mid), so downstream factor(x) does not re-sort it.
  expect_s3_class(g$categorical$x, "factor")
  expect_identical(levels(g$categorical$x), lv)
})
