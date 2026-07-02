# Wrong-class inputs to the rfsrc/randomForest wrappers must error via their
# default S3 methods with a clear message. Fit-free, so these run on CRAN.

test_that("rfsrc/randomForest wrappers error on non-model input", {
  bad <- data.frame(a = 1:3)

  expect_error(gg_error(bad),    "expected an 'rfsrc' or 'randomForest' object")
  expect_error(gg_vimp(bad),     "expected an 'rfsrc' or 'randomForest' object")
  expect_error(gg_variable(bad), "expected an 'rfsrc' or 'randomForest' object")
  expect_error(gg_rfsrc(bad),    "expected an 'rfsrc' or 'randomForest' object")
  expect_error(gg_brier(bad),    "expected an 'rfsrc' survival object")

  # The error names the class it actually got.
  expect_error(gg_error(1:3), "got an object of class integer")
})
