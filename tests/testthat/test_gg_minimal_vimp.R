# testthat for gg_minimal_vimp function
context("gg_minimal_vimp tests")

test_that("gg_minimal_vimp classifications",{
  ## IF we want to build the forest every time...
  #   iris_rf <- rfsrc(Species ~ ., data = iris)
  # varsel_iris <- var.select(iris_rf)
  
  ## Load the cached forest
  data(varsel_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_iris, "list")
  
  # Test the forest family
  expect_false(is.null(varsel_iris$md.obj))
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_minimal_vimp(varsel_iris)
  
  # Test object type
  expect_is(ggrf.obj, "gg_minimal_vimp")
  
  # Test varselect is the same
  expect_equivalent(dim(ggrf.obj)[1], dim(varsel_iris$varselect)[1])
  expect_equivalent(dim(ggrf.obj)[2], 4)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_minimal_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})


test_that("gg_minimal_vimp survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  #   varsel_veteran <- var.select(rfsrc_veteran)
  ## Load the cached forest
  data(varsel_veteran, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_veteran, "list")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_minimal_vimp(varsel_veteran)
  
  # Test object type
  expect_is(ggrf.obj, "gg_minimal_vimp")
  
  
  # Test varselect is the same
  expect_equivalent(dim(ggrf.obj)[1], dim(varsel_veteran$varselect)[1])
  expect_equivalent(dim(ggrf.obj)[2], 4)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_minimal_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})

test_that("gg_minimal_vimp regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   varsel_airq <- var.select(rfsrc_airq)
  ## Load the cached forest
  data(varsel_airq, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_airq, "list")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_minimal_vimp(varsel_airq)
  
  # Test object type
  expect_is(ggrf.obj, "gg_minimal_vimp")
  
  
  # Test varselect is the same
  expect_equivalent(dim(ggrf.obj)[1], dim(varsel_airq$varselect)[1])
  expect_equivalent(dim(ggrf.obj)[2], 4)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_minimal_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  data(varsel_Boston, package="ggRandomForests")
  data(Boston, package="MASS")
  
  cls <- sapply(Boston, class) 
  # 
  lbls <- 
    #crim
    c("Crime rate by town.",
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
      "Median value of homes ($1000s).")
  
  # Build a table for data description
  dta.labs <- data.frame(cbind(Variable=names(cls), Description=lbls, type=cls))
  
  # Build a named vector for labeling figures later/
  st.labs <- as.character(dta.labs$Description)
  names(st.labs) <- names(cls)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_minimal_vimp(varsel_Boston, lbls=st.labs)
  expect_is(gg_plt, "ggplot")
  
})


test_that("gg_minimal_vimp exceptions",{
  data(varsel_airq, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_airq, "list")
  
  vsel <- varsel_airq
  vsel$varselect <- NULL
  
  expect_error(gg_minimal_vimp(vsel))
  
  vsel$threshold <- NULL
  expect_error(gg_minimal_vimp(vsel))
  
  vsel <- varsel_airq
  vsel$varselect$vimp <- NULL
  expect_error(gg_minimal_vimp(vsel))
  expect_error(plot.gg_minimal_vimp(vsel))
  
  data(rfsrc_airq, package="ggRandomForests")
  expect_output(gg_dta <- gg_minimal_vimp(rfsrc_airq, fast=TRUE),
                "minimal depth variable selection")
  expect_is(gg_dta, "gg_minimal_vimp")
  gg_plt <- plot.gg_minimal_vimp(gg_dta)
  expect_error(gg_minimal_depth(gg_plt))
  
})