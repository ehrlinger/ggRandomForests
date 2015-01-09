# testthat for gg_error function
context("rfsrc_cache_dataset tests")

test_that("rfsrc_cache_dataset",{
  
  expect_output(rfsrc_cache_datasets(test=TRUE),
                "airq: randomForest")
  
  expect_error(rfsrc_cache_datasets(pth="nothing"))
})
  