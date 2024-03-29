# testthat for quantile_pts function
context("quantile_pts tests")

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
  
  expect_is(rm_pts, "numeric")
  expect_equal(length(rm_pts), 6 + 1)
  
  # When calculating intervals, we subtract 1.e-7 from the min value
  expect_true(abs(min(rfsrc_boston$xvar$rm) - min(rm_pts)) > 0)
  
  # Use cut to create the intervals
  rm_grp <- cut(rfsrc_boston$xvar$rm, breaks = rm_pts)
  expect_is(rm_grp, "factor")
  expect_equal(length(rm_grp), length(rfsrc_boston$xvar$rm))
  expect_equal(length(levels(rm_grp)), length(rm_pts) - 1)
  
  rm_pts <- quantile_pts(rfsrc_boston$xvar$rm, groups = 6)
  
  expect_is(rm_pts, "numeric")
  expect_equal(length(rm_pts), 6)
  # When calculating intervals, we subtract 1.e-7 from the min value
  expect_equal(min(rfsrc_boston$xvar$rm), min(rm_pts), tolerance = 1.e-7)
  
  # Test the number of points for lots of groups.
  rm_pts <-
    quantile_pts(rfsrc_boston$xvar$rm, groups = nrow(rfsrc_boston$xvar) + 2)
  expect_equal(length(rm_pts), length(unique(rfsrc_boston$xvar$rm)))
})
