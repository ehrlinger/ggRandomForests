# testthat for gg_interaction function
context("gg_interaction tests")

test_that("gg_interaction classifications",{
  
  ## Load the cached forest
  data(interaction_iris, package="ggRandomForests")
  
  # Test the cached interaction structure
  expect_is(interaction_iris, "matrix")
  
  ## Create the correct gg_interaction object
  ggrf.obj <- gg_interaction(interaction_iris)
  
  # Test object type
  expect_is(ggrf.obj, "gg_interaction")
  
  # Test classification dimensions
  expect_equal(dim(ggrf.obj), dim(interaction_iris))
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(ggrf.obj), interaction_iris)
  
  ## Test plotting the gg_interaction object
  gg.obj <- plot.gg_interaction(ggrf.obj, xvar="Petal.Width")
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  # This one should fail with a variable not found message
  expect_error(plot.gg_interaction(ggrf.obj, xvar="Petal"))
  
  # "Incorrect object type: Expects a gg_interaction object"
  ## Test plotting the gg_interaction object
  gg.obj <- plot.gg_interaction(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
})


test_that("gg_interaction survival",{
  
  data(pbc, package = "randomForestSRC")
  #   pbc_rf <- rfsrc(Surv(days, status) ~ ., pbc,
  #                 nsplit = 10, na.action = "na.impute")
  #   interaction_pbc <- find.interaction(pbc.rf)
  #
  ## Load the cached forest
  data(interaction_pbc, package="ggRandomForests")
  
  # Test the cached interaction structure
  expect_is(interaction_pbc, "matrix")
  
  ## Create the correct gg_interaction object
  ggrf.obj <- gg_interaction(interaction_pbc)
  
  # Test object type
  expect_is(ggrf.obj, "gg_interaction")
  
  # Test classification dimensions
  expect_equal(dim(ggrf.obj), dim(interaction_pbc))
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(ggrf.obj), interaction_pbc)
  
  ## Test plotting the gg_interaction object
  gg.obj <- plot.gg_interaction(ggrf.obj, xvar="bili")
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  # "Incorrect object type: Expects a gg_interaction object"
  
  labels <- c("event indicator (F = censor, T = death)", 
              "Treament (DPCA, Placebo)", 
              "age in years", 
              "Female", 
              "Asictes", 
              "Hepatomegaly", 
              "Spiders", 
              "Edema", 
              "serum bilirubin (mg/dl)", 
              "serum cholesterol (mg/dl)", 
              "albumin (gm/dl)", 
              "urine copper (ug/day)", 
              "alkaline phosphatase (U/liter)", 
              "SGOT (U/ml)", 
              "triglicerides (mg/dl)", 
              "platelets per cubic ml/1000", 
              "prothrombin time (sec)", 
              "histologic stage", 
              "survival time (years)")
  
  dta.labs <- data.frame(cbind(names = colnames(pbc), label = labels))
  
  st.labs <- as.character(dta.labs$label)
  names(st.labs) <- rownames(dta.labs)
  gg.obj <- plot.gg_interaction(ggrf.obj, lbls=st.labs)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})

test_that("gg_interaction regression",{
  
  ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   ggrf.obj<- gg_interaction(airq.obj)
  ## Load the cached forest
  data(interaction_airq, package="ggRandomForests")
  
  # Test the cached interaction structure
  expect_is(interaction_airq, "matrix")
  
  ## Create the correct gg_interaction object
  ggrf.obj <- gg_interaction(interaction_airq)
  
  # Test object type
  expect_is(ggrf.obj, "gg_interaction")
  
  # Test classification dimensions
  expect_equal(dim(ggrf.obj), dim(interaction_airq))
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(ggrf.obj), interaction_airq)
  
  ## Test plotting the gg_interaction object
  gg.obj <- plot.gg_interaction(ggrf.obj, xvar = "Temp")
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
})

test_that("gg_interaction exceptions",{
  
  ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   ggrf.obj<- gg_interaction(airq.obj)
  ## Load the cached forest
  data(rfsrc_mtcars, package="ggRandomForests")
  
  # Test the cached interaction structure
  expect_is(rfsrc_mtcars, "rfsrc")
  
  # This one costs a lot of time in calculating the interaction matrix.
  #   ## Create the correct gg_interaction object
  #   expect_warning(ggrf.obj <- gg_interaction(rfsrc_mtcars))
  #   
  #   # Test object type
  #   expect_is(ggrf.obj, "gg_interaction")
  #   
  data(interaction_mtcars, package="ggRandomForests")
  # Test the cached interaction structure
  expect_is(interaction_mtcars, "matrix")
  
  interaction_mtcars <- interaction_mtcars[-2,]
  expect_error(gg_interaction(interaction_mtcars))
})

