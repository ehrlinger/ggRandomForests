# testthat for shift function
context("shift tests")

test_that("lead or lag a vector",{
  expect_that(shift(1:10, 2),is_identical_to(c(3:10, NA, NA)))
  expect_that(shift(1:10, -2), is_identical_to(c(NA, NA, 1:8)))
  expect_that(shift(1:10, 0), is_identical_to(1:10))
  expect_that(shift(1:10, 0), is_identical_to(1:10))
  expect_that(shift(1:10, 1:2), is_identical_to(cbind(c(2:10, NA),c(3:10, NA, NA))))
})