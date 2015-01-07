# testthat for gg_variable function
context("gg_variable tests")

test_that("gg_variable classifications",{
  ## IF we want to build the forest every time...
  #   rfsrc_iris <- rfsrc(Species ~ ., data = iris)
  # rfsrc_iris <- var.select(rfsrc_iris)
  
  ## Load the cached forest
  data(rfsrc_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_equal(rfsrc_iris$family, "class")
  
  ## Create the correct gg_error object
  gg_dta<- gg_variable(rfsrc_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_variable")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta, xvar = "Petal.Width", )
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta, xvar = rfsrc_iris$xvar.names )
  
  # Test return is s ggplot object
  expect_is(gg_plt, "list")
  expect_equal(length(gg_plt), length(rfsrc_iris$xvar.names))
  for(ind in 1:length(rfsrc_iris$xvar.names))
    expect_is(gg_plt[[ind]], "ggplot")
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta, xvar = rfsrc_iris$xvar.names,
                             panel=TRUE)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
})


test_that("gg_variable survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  #   rfsrc_veteran <- var.select(rfsrc_veteran)
  ## Load the cached forest
  data(rfsrc_veteran, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_veteran, "rfsrc")
  
  ## Create the correct gg_error object
  gg_dta<- gg_variable(rfsrc_veteran, time=30)
  
  # Test object type
  expect_is(gg_dta, "gg_variable")
  
  ## Test plotting the gg_variable object
  gg_plt <- plot.gg_variable(gg_dta, xvar="age")
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  
  ## Test plotting the gg_variable object
  gg_plt <- plot.gg_variable(gg_dta, xvar=rfsrc_veteran$xvar.names)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "list")
  expect_equal(length(gg_plt), length(rfsrc_veteran$xvar.names))
  
  
  for(ind in 1:length(rfsrc_veteran$xvar.names))
    expect_is(gg_plt[[ind]], "ggplot")
  
  
  ## Test plotting the gg_error object
  expect_warning(gg_plt <- plot.gg_variable(gg_dta, xvar = rfsrc_veteran$xvar.names,
                                            panel=TRUE)
  )
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
})

test_that("gg_variable regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   rfsrc_airq <- var.select(rfsrc_airq)
  ## Load the cached forest
  data(rfsrc_airq, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_airq, "rfsrc")
  
  ## Create the correct gg_error object
  gg_dta<- gg_variable(rfsrc_airq)
  
  # Test object type
  expect_is(gg_dta, "gg_variable")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "list")
  expect_equal(length(gg_plt), length(rfsrc_airq$xvar.names))
  for(ind in 1:length(rfsrc_airq$xvar.names))
    expect_is(gg_plt[[ind]], "ggplot")
  
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta, panel=TRUE)
  expect_is(gg_plt, "ggplot")
  
})