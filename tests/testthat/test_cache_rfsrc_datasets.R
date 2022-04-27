# testthat for gg_error function
context("cache_rfsrc_dataset tests")

test_that("cache_rfsrc_dataset", {
  skip_on_cran()
  # # Check the default set of data
  expect_invisible(cache_rfsrc_datasets(test = TRUE))
  
  # If we have a bad path...
  expect_error(cache_rfsrc_datasets(pth = "nothing"))
  
  # If we want the alternative sets
  expect_invisible(cache_rfsrc_datasets(set = c("airq"),
                                        test = TRUE))
  #
})
