# testthat for gg_vimp function
context("gg_vimp tests")

test_that("gg_vimp classifications",{
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
  gg_dta<- gg_vimp(rfsrc_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_vimp")
  
  # Test varselect is the same
 #expect_equivalent(select(gg_dta$varselect, -names), rfsrc_iris$importance)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_vimp(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
})


test_that("gg_vimp survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  #   rfsrc_veteran <- var.select(rfsrc_veteran)
  ## Load the cached forest
  data(rfsrc_veteran, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_veteran, "rfsrc")
  
  ## Create the correct gg_error object
  gg_dta<- gg_vimp(rfsrc_veteran)
  
  # Test object type
  expect_is(gg_dta, "gg_vimp")
  
  # Test varselect is the same
  expect_equal(gg_dta$vimp, as.vector(sort(rfsrc_veteran$importance, decreasing=TRUE)))
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_vimp(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  data(rfsrc_pbc, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_pbc, "rfsrc")
  
  ## Create the correct gg_error object
  expect_is(gg_dta<- gg_vimp(rfsrc_pbc),
            "gg_vimp")
  expect_is(plot(gg_dta), "ggplot")
  
})

test_that("gg_vimp regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   rfsrc_airq <- var.select(rfsrc_airq)
  ## Load the cached forest
  data(rfsrc_airq, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_airq, "rfsrc")
  
  ## Create the correct gg_error object
  gg_dta<- gg_vimp(rfsrc_airq)
  
  # Test object type
  expect_is(gg_dta, "gg_vimp")
  
  # Test varselect is the same
  expect_equal(gg_dta$vimp, as.vector(sort(rfsrc_airq$importance, decreasing=TRUE)))
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_vimp(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  data(rfsrc_Boston, package="ggRandomForests")
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
  gg_plt <- plot.gg_vimp(rfsrc_Boston, lbls=st.labs, relative=TRUE, 
                         bars=rfsrc_Boston$xvar.names)
  expect_is(gg_plt, "ggplot")
})