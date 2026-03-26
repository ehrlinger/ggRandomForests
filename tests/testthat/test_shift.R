# testthat for shift function

test_that("lead or lag a vector", {
  expect_identical(shift(1:10, 2),  c(3:10, NA, NA))
  expect_identical(shift(1:10, -2), c(NA, NA, 1:8))
  expect_identical(shift(1:10, 0),  1:10)
  expect_identical(
    shift(1:10, 1:2),
    cbind(c(2:10, NA), c(3:10, NA, NA))
  )
})
