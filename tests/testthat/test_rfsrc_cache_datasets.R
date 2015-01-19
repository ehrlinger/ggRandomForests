# testthat for gg_error function
context("rfsrc_cache_dataset tests")

test_that("rfsrc_cache_dataset",{
  
  # Check the default set of data
  expect_output(rfsrc_cache_datasets(test=TRUE),
                "Boston: randomForest")
  
  # If we have a bad path...
  expect_error(rfsrc_cache_datasets(pth="nothing"))
  
  # If we want the alternative sets
  expect_output(rfsrc_cache_datasets(set=c("airq", "mtcars", "veteran"), 
                                     test=TRUE),
                "airq: randomForest")
  
})
