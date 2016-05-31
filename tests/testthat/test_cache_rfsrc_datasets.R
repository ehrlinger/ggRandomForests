# testthat for gg_error function
context("cache_rfsrc_dataset tests")

test_that("cache_rfsrc_dataset",{
  
  # # Check the default set of data
  # expect_output(cache_rfsrc_datasets(test=TRUE),
  #               "iris: randomForest")
  # 
  # # If we have a bad path...
  # expect_error(cache_rfsrc_datasets(pth="nothing"))
  # 
  # # If we want the alternative sets
  # expect_output(cache_rfsrc_datasets(set=c("airq"),
  #                                    test=TRUE),
  #               "airq: randomForest")
  # # 
})
