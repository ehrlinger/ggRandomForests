# testthat for gg_partial function
context("gg_partial tests")

test_that("gg_partial classifications", {
  ## Load the cached forest
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    forest = TRUE,
    importance = TRUE,
    save.memory = TRUE)
  
  # Test the cached forest type
  testthat::expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  testthat::expect_equal(rfsrc_iris$family, "class")
  partial_iris <- randomForestSRC::plot.variable(rfsrc_iris,
                                                 partial = TRUE,
                                                 show.plots = FALSE)
  
  testthat::expect_equivalent(length(partial_iris$pData), length(rfsrc_iris$xvar.names))
  
  ## Create the correct gg_error object
  gg_dta <- gg_partial(partial_iris)
  
  # Test object type
  testthat::expect_is(gg_dta, "gg_partial_list")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_partial(gg_dta[[2]])
  
  # Test return is s ggplot object
  testthat::expect_is(gg_plt, "ggplot")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_partial_list(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt[[1]], "ggplot")
  
  expect_equivalent(length(gg_plt), length(partial_iris$pData))
})


test_that("gg_partial regression", {
  data(Boston, package = "MASS")
  boston <- Boston
  
  boston$chas <- as.logical(boston$chas)
  
  ## Load the cached forest
  rfsrc_boston <-
    randomForestSRC::rfsrc(
      medv ~ .,
      data = boston,
      forest = TRUE,
      importance = TRUE,
      tree.err = TRUE,
      save.memory = TRUE)
  
  # Test the cached forest type
  expect_is(rfsrc_boston, "rfsrc")
  
  nms <- rfsrc_boston$xvar.names
  nms <- nms[nms != "chas"]
  partial_boston <- randomForestSRC::plot.variable(
    rfsrc_boston,
    sorted = FALSE,
    partial = TRUE,
    show.plots = FALSE
  )
  
  ## Create the correct gg_error object
  gg_dta <- gg_partial(partial_boston)
  
  # Test object type
  expect_is(gg_dta, "gg_partial_list")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_partial(gg_dta[[1]])
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_partial_list(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "list")
  
  expect_equivalent(length(gg_plt), length(gg_dta))
  
  # gg_partial exceptions
  expect_error(gg_partial(gg_plt))
  
  # Remove all but one partial data.
  partial_boston$xvar.names <- "lstat"
  partial_boston$nvar <- 1
  for (ind in length(partial_boston$pData):2) {
    partial_boston$pData[[ind]] <- NULL
  }
  gg_dta <- gg_partial(partial_boston)
  
  # Test object type
  expect_is(gg_dta, "gg_partial")
  
  # generate a list of gg_partial objects, one per xvar.
  expect_error(gg_p <- gg_partial(gg_dta), "gg_partial")
  
  expect_is(plot(gg_dta, error = "bars"), "ggplot")
  expect_is(plot(gg_dta, error = "none"), "ggplot")
  expect_is(plot(gg_dta, error = "lines"), "ggplot")
  expect_is(plot(gg_dta, error = "shade"), "ggplot")
  # Test object type
  
  expect_is(plot(gg_dta), "ggplot")
  gg_plt <- plot(gg_dta, error = "shade") + 
    ggplot2::geom_smooth(se = .95)
  
})
