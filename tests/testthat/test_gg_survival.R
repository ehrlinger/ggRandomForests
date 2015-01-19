# testthat for gg_survival function
context("gg_survival tests")

test_that("gg_survival classifications",{
  expect_error(gg_survival(data=iris))
})


test_that("gg_survival survival",{
  #   ## Load the cached forest
  data(pbc, package="randomForestSRC")
  
  # Test the cached forest type
  expect_is(pbc, "data.frame")
  
  # Test object type
  gg_dta <- gg_survival(interval = "days",
                        censor = "status", 
                        by = "treatment", 
                        data = pbc, 
                        conf.int = .95)
  
  expect_is(gg_dta, "gg_survival")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_survival(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  expect_is(plot(gg_dta, error="bars"), "ggplot")
  expect_is(plot(gg_dta, error="none"), "ggplot")
  expect_is(plot(gg_dta, error="lines"), "ggplot")
  expect_is(plot(gg_dta, type="surv"), "ggplot")
  expect_is(plot(gg_dta, type="cum_haz"), "ggplot")
  expect_is(plot(gg_dta, type="density"), "ggplot")
  expect_is(plot(gg_dta, type="mid_int"), "ggplot")
  expect_is(plot(gg_dta, type="life"), "ggplot")
  expect_is(plot(gg_dta, type="hazard"), "ggplot")
  expect_is(plot(gg_dta, type="proplife"), "ggplot")
  # Test object type
  gg_dta <- gg_survival(interval = "days",
                        censor = "status", 
                        by = "treatment", 
                        data = pbc, 
                        conf.int = .95,
                        type="nelson")
  
  expect_is(gg_dta, "gg_survival")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_survival(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  
  # Test object type
  gg_dta <- gg_survival(interval = "days",
                        censor = "status", 
                        data = pbc, 
                        conf.int = .95)
  
  expect_is(gg_dta, "gg_survival")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_survival(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  expect_is(plot(gg_dta, error="bars"), "ggplot")
  expect_is(plot(gg_dta, error="none"), "ggplot")
  expect_is(plot(gg_dta, error="lines"), "ggplot")
  expect_is(plot(gg_dta, type="surv"), "ggplot")
  expect_is(plot(gg_dta, type="cum_haz"), "ggplot")
  expect_is(plot(gg_dta, type="density"), "ggplot")
  expect_is(plot(gg_dta, type="mid_int"), "ggplot")
  expect_is(plot(gg_dta, type="life"), "ggplot")
  expect_is(plot(gg_dta, type="hazard"), "ggplot")
  expect_is(plot(gg_dta, type="proplife"), "ggplot")
  
})

test_that("gg_survival regression",{ 
  ## Load the data
  data(Boston, package="MASS")
  
  ## Create the correct gg_error object
  expect_error(gg_survival(data=Boston))
})