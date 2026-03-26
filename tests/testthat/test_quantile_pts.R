# testthat for quantile_pts function

test_that("cutting a vector at evenly space points", {
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  rfsrc_boston <-
    randomForestSRC::rfsrc(
      medv ~ .,
      data = boston,
      forest = TRUE,
      importance = TRUE,
      tree.err = TRUE,
      save.memory = TRUE)
  # To create 6 intervals, we want 7 points.
  # quantile_pts will find balanced intervals
  rm_pts <-
    quantile_pts(rfsrc_boston$xvar$rm,
                 groups = 6,
                 intervals = TRUE)

  expect_true(is.numeric(rm_pts))
  expect_true(length(rm_pts) >= 2)
  expect_true(rm_pts[1] < min(rfsrc_boston$xvar$rm))
  expect_equal(max(rm_pts), max(rfsrc_boston$xvar$rm))
  expect_true(all(diff(rm_pts) > 0))

  # Use cut to create the intervals
  rm_grp <- cut(rfsrc_boston$xvar$rm, breaks = rm_pts)
  expect_s3_class(rm_grp, "factor")
  expect_equal(length(rm_grp), length(rfsrc_boston$xvar$rm))
  expect_equal(length(levels(rm_grp)), length(rm_pts) - 1)

  rm_pts <- quantile_pts(rfsrc_boston$xvar$rm, groups = 6)

  expect_true(is.numeric(rm_pts))
  expect_equal(length(rm_pts), 6)
  # First quantile coincides with the minimum value
  expect_equal(min(rfsrc_boston$xvar$rm), min(rm_pts), tolerance = 1.e-7)

  # Test the number of points for lots of groups.
  rm_pts <-
    quantile_pts(rfsrc_boston$xvar$rm, groups = nrow(rfsrc_boston$xvar) + 2)
  expect_equal(length(rm_pts), nrow(rfsrc_boston$xvar) + 2)
})
