# testthat for gg_minimal_depth function
context("gg_minimal_depth tests")

test_that("gg_minimal_depth classifications", {
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    forest = TRUE,
    importance = TRUE,
    save.memory = TRUE
  )
  ## Load the cached forest
  varsel_iris <- randomForestSRC::var.select(rfsrc_iris)
  
  # Test the cached forest type
  expect_is(varsel_iris, "list")
  
  # Test the forest family
  expect_false(is.null(varsel_iris$md.obj))
  
  ## Create the correct gg_error object
  gg_dta <- gg_minimal_depth(varsel_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_minimal_depth")
  
  # Test varselect is the same
  expect_equivalent(gg_dta$varselect[, 
                                     -which(colnames(gg_dta$varselect) ==
                                              "names")],
                    varsel_iris$varselect)
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  expect_error(gg_minimal_depth(gg_plt))
  expect_error(gg_minimal_depth.rfsrc(gg_plt))
})

test_that("gg_minimal_depth regression", {
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
      save.memory = TRUE
    )
  
  varsel_boston <- randomForestSRC::var.select(rfsrc_boston)
  # Test the cached forest type
  expect_is(varsel_boston, "list")
  
  ## Create the correct gg_error object
  gg_dta <- gg_minimal_depth(varsel_boston)
  
  # Test object type
  expect_is(gg_dta, "gg_minimal_depth")
  
  # Test varselect is the same
  expect_equivalent(gg_dta$varselect[, 
                                     -which(colnames(gg_dta$varselect) == 
                                              "names")],
                    varsel_boston$varselect)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_minimal_depth(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## Create the correct gg_minimal_depth object
  gg_dta <- gg_minimal_depth(varsel_boston)
  # Test object type
  expect_is(gg_dta, "gg_minimal_depth")
  
  data(Boston, package = "MASS")
  
  cls <- sapply(Boston, class)
  #
  lbls <-
    #crim
    c(
      "Crime rate by town.",
      # zn
      "Proportion of residential land zoned for lots over 25,000 sq.ft.",
      # indus
      "Proportion of non-retail business acres per town.",
      # chas
      "Charles River (tract bounds river).",
      # nox
      "Nitrogen oxides concentration (10 ppm).",
      # rm
      "Number of rooms per dwelling.",
      # age
      "Proportion of units built prior to 1940.",
      # dis
      "Distances to Boston employment center.",
      # rad
      "Accessibility to highways.",
      # tax
      "Property-tax rate per $10,000.",
      # ptratio
      "Pupil-teacher ratio by town.",
      # black
      "Proportion of blacks by town.",
      # lstat
      "Lower status of the population (percent).",
      # medv
      "Median value of homes ($1000s)."
    )
  
  # Build a table for data description
  dta_labs <-
    data.frame(cbind(
      Variable = names(cls),
      Description = lbls,
      type = cls
    ))
  
  # Build a named vector for labeling figures later/
  st_labs <- as.character(dta_labs$Description)
  names(st_labs) <- names(cls)
  
  ## Test plotting the gg_error object
  gg_plt <-
    plot.gg_minimal_depth(gg_dta, lbls = st_labs, selection = TRUE)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
})

test_that("gg_minimal_depth exceptions", {
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
  varsel_boston <- randomForestSRC::var.select(rfsrc_boston)
  # Test the cached forest type
  expect_is(varsel_boston, "list")
  
  vsel <- varsel_boston
  vsel$varselect <- NULL
  
  expect_error(gg_minimal_depth(vsel))
  
  vsel$threshold <- NULL
  expect_error(gg_minimal_depth(vsel))
  
  expect_output(print(gg_minimal_depth(varsel_boston)),
                "gg_minimal_depth", ignore.case = TRUE)
  
  
  vsel <- varsel_boston
  vsel$varselect$vimp <- NULL
  gg_dta <- gg_minimal_depth(vsel)
  
  expect_is(gg_dta, "gg_minimal_depth")
  
  expect_is(plot(gg_dta, type = "rank"), "ggplot")
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
  
  expect_output(
    gg_dta <- gg_minimal_depth(rfsrc_boston, fast = TRUE),
    "minimal depth variable selection"
  )
  expect_is(gg_dta, "gg_minimal_depth")
  
  expect_output(
    gg_plt <- plot.gg_minimal_depth(rfsrc_boston, fast = TRUE),
    "minimal depth variable selection"
  )
  expect_error(gg_minimal_depth(gg_plt))
  
})
