# testthat for gg_vimp function

test_that("gg_vimp classifications", {
  ## Load the cached forest
  data(iris, package = "datasets")
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    importance = TRUE,
    tree.err = TRUE
  )
  # Test the cached forest type
  expect_s3_class(rfsrc_iris, "rfsrc")

  # Test the forest family
  expect_equal(rfsrc_iris$family, "class")

  ## Create the correct gg_error object
  gg_dta <- gg_vimp(rfsrc_iris)

  # Test object type
  expect_s3_class(gg_dta, "gg_vimp")

  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  # Grab only one class... by number.
  gg_dta <- gg_vimp(rfsrc_iris, which.outcome = 2)

  # Test object type
  expect_s3_class(gg_dta, "gg_vimp")
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")
  # Grab only one class... by number - for the overall model.
  gg_dta <- gg_vimp(rfsrc_iris, which.outcome = 0)

  # Test object type
  expect_s3_class(gg_dta, "gg_vimp")
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")
  # Grab only one class... by name - for the overall model.
  gg_dta <- gg_vimp(rfsrc_iris, which.outcome = "all")

  # Test object type
  expect_s3_class(gg_dta, "gg_vimp")
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")
  # Grab only one class... by name - for the overall model.
  gg_dta <- gg_vimp(rfsrc_iris, which.outcome = "setosa")

  # Test object type
  expect_s3_class(gg_dta, "gg_vimp")
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  # Grab only one class... by name - that doesn't exist.
  expect_error(gg_vimp(rfsrc_iris, which.outcome = "nothing special"))

  # Grab only one class... by number - that doesn't exist.
  expect_error(gg_vimp(rfsrc_iris, which.outcome = 200))

  ## Single class/
  iris2 <- iris
  iris2$spec <- factor(as.character(iris2$Species) == "setosa")
  iris2 <- iris2[, -which(colnames(iris2) == "Species")]

  rf <- randomForestSRC::rfsrc(spec ~ ., iris2, importance = TRUE)

  gg_dta <- gg_vimp(rf)

  expect_s3_class(gg_dta, "gg_vimp")

  # Test passing in the wrong object
  expect_error(gg_vimp(gg_dta))
  expect_error(gg_vimp.rfsrc(gg_dta))

  ## RandomForest case
  rf_iris <- randomForest::randomForest(Species ~ .,
                                        data = iris)

  gg_dta <- gg_vimp(rf_iris)

  expect_s3_class(gg_dta, "gg_vimp")

  rf_iris_noimp <- randomForest::randomForest(Species ~ .,
                                              data = iris,
                                              importance = FALSE)
  rf_iris_noimp$importance <- NULL
  expect_warning(gg_dta <- gg_vimp(rf_iris_noimp))
  expect_s3_class(gg_dta, "gg_vimp")

  # Test passing in the wrong object
  expect_error(gg_vimp(gg_dta))
  expect_error(gg_vimp.rfsrc(gg_dta))


  gg_dta <- gg_vimp(rf_iris, which.outcome = 1)

  expect_s3_class(gg_dta, "gg_vimp")


  expect_s3_class(gg_dta, "gg_vimp")
  # Test passing in the wrong object
  expect_error(gg_vimp(gg_dta))
  expect_error(gg_vimp.rfsrc(gg_dta))
  gg_dta <- gg_vimp(rf_iris, which.outcome = NULL)
})


test_that("gg_vimp survival", {
  dta <- new.env()
  data(pbc, package = "randomForestSRC",
       envir = dta)
  pbc <- dta$pbc
  # For whatever reason, the age variable is in days... makes no sense to me
  for (ind in seq_len(dim(pbc)[2])) {
    if (!is.factor(pbc[, ind])) {
      if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
        if (sum(range(pbc[, ind], na.rm = TRUE) == c(0, 1)) == 2) {
          pbc[, ind] <- as.logical(pbc[, ind])
        }
      }
    } else {
      if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
        if (sum(sort(unique(pbc[, ind])) == c(0, 1)) == 2) {
          pbc[, ind] <- as.logical(pbc[, ind])
        }
        if (sum(sort(unique(pbc[, ind])) == c(FALSE, TRUE)) == 2) {
          pbc[, ind] <- as.logical(pbc[, ind])
        }
      }
    }
    if (!is.logical(pbc[, ind]) &
        length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 5) {
      pbc[, ind] <- factor(pbc[, ind])
    }
  }
  # Convert age to years
  pbc$age <- pbc$age / 364.24

  pbc$years <- pbc$days / 364.24
  pbc <- pbc[, -which(colnames(pbc) == "days")]
  pbc$treatment <- as.numeric(pbc$treatment)
  pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
  pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
  pbc$treatment <- factor(pbc$treatment)

  cat("pbc: rfsrc\n")
  dta_train <- pbc[-which(is.na(pbc$treatment)), ]
  # Create a test set from the remaining patients
  pbc_test <- pbc[which(is.na(pbc$treatment)), ]

  rfsrc_pbc <- randomForestSRC::rfsrc(
    Surv(years, status) ~ .,
    dta_train,
    nsplit = 10,
    na.action = "na.impute",
    importance = TRUE,
    tree.err = TRUE
  )
  # Test the cached forest type
  expect_s3_class(rfsrc_pbc, "rfsrc")

  ## Create the correct gg_error object
  gg_dta <- gg_vimp(rfsrc_pbc)

  # Test object type
  expect_s3_class(gg_dta, "gg_vimp")

  # Test varselect is the same
  expect_equal(gg_dta$vimp, as.vector(sort(rfsrc_pbc$importance, decreasing =
                                             TRUE)))

  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta, nvar = 5)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  expect_s3_class(plot(gg_dta, relative = TRUE), "ggplot")

  # Test cutting the size down
  expect_s3_class(gg_dta <- gg_vimp(rfsrc_pbc, nvar = 10), "gg_vimp")
  expect_equal(nrow(gg_dta), 10)
  expect_s3_class(plot(gg_dta), "ggplot")

  # Test the relative vimp output and plotting
  expect_s3_class(
    gg_dta <- gg_vimp(rfsrc_pbc, relative = TRUE),
    "gg_vimp"
  )
  expect_s3_class(plot(gg_dta), "ggplot")

  expect_s3_class(
    gg_dta <- gg_vimp(rfsrc_pbc, nvar = 10, relative = TRUE),
    "gg_vimp"
  )
  expect_s3_class(plot(gg_dta), "ggplot")

  # Test importance calculations.
  # If the forest does not have importance
  rfsrc_pbc$importance <- NULL
  expect_warning(gg_dta <- gg_vimp(rfsrc_pbc))
  expect_s3_class(gg_dta, "gg_vimp")
  expect_s3_class(plot(gg_dta), "ggplot")

})

test_that("gg_vimp regression", {
  ## Load the cached forest
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  rfsrc_boston <- randomForestSRC::rfsrc(medv ~ ., data = boston,
                                         importance = TRUE)
  # Test the cached forest type
  expect_s3_class(rfsrc_boston, "rfsrc")

  ## Create the correct gg_error object
  gg_dta <- gg_vimp(rfsrc_boston)

  # Test object type
  expect_s3_class(gg_dta, "gg_vimp")

  # Test varselect is the same
  expect_equal(gg_dta$vimp, as.vector(sort(rfsrc_boston$importance, decreasing =
                                             TRUE)))

  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta, relative = TRUE)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")


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

  ## Test plotting the rfsrc object
  gg_plt <- plot.gg_vimp(
    rfsrc_boston,
    lbls = st_labs,
    relative = TRUE,
    bars = rfsrc_boston$xvar.names
  )
  expect_s3_class(gg_plt, "ggplot")

  rf_boston <- randomForest::randomForest(medv ~ ., Boston)
  gg_dta <- gg_vimp(rf_boston)
  # Test varselect is the same
  expect_equal(gg_dta$vimp, as.vector(sort(rf_boston$importance, decreasing =
                                             TRUE)))

  gg_plt <- plot(gg_dta)
  expect_s3_class(gg_plt, "ggplot")

  rf_boston_noimp <- randomForest::randomForest(medv ~ ., Boston, importance = FALSE)
  rf_boston_noimp$importance <- NULL
  expect_warning(gg_dta <- gg_vimp(rf_boston_noimp))
  expect_s3_class(gg_dta, "gg_vimp")

})

test_that("gg_vimp.rfsrc single-outcome: positive flag correctly uses the VIMP column", {
  # Regression test for the bug where gg_dta$vimp was accessed but the column
  # is named "VIMP" (uppercase) in single-outcome rfsrc fits, leaving positive
  # always TRUE even for variables with non-positive VIMP.
  #
  # Force one VIMP non-positive so the test deterministically exercises the bug
  # condition. A zero-variance ("const") predictor is unreliable here:
  # permutation VIMP of a weak/constant variable can land slightly > 0 (and
  # rfsrc may drop a constant column), so it does not guarantee a non-positive
  # VIMP on every platform (observed failing on R-release). The pre-fix code
  # left `positive` TRUE regardless of the VIMP sign.
  data(airquality, package = "datasets")
  set.seed(2024L)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50, importance = TRUE)
  rf$importance[1] <- -1
  gg_dta <- gg_vimp(rf)

  expect_s3_class(gg_dta, "gg_vimp")
  expect_true("positive" %in% colnames(gg_dta))

  vimp_col <- intersect(c("vimp", "VIMP"), colnames(gg_dta))[1]
  expect_false(is.na(vimp_col))
  # The invariant, asserted for every row: positive is TRUE exactly when VIMP > 0.
  expect_equal(gg_dta$positive, gg_dta[[vimp_col]] > 0)
  # The injected -1 guarantees at least one non-positive VIMP, so the pre-fix
  # all-TRUE behaviour would fail here.
  expect_true(any(!gg_dta$positive))
  expect_s3_class(plot(gg_dta), "ggplot")
})

test_that("gg_vimp.randomForest regression: vimp column present even when importance is IncNodePurity", {
  # Guard test: when randomForest stores importance as IncNodePurity (not X.IncMSE),
  # gg_vimp must still produce a 'vimp' column so plot.gg_vimp and the positive
  # flag work correctly.
  data(Boston, package = "MASS")
  # importance = FALSE (default) → $importance has only IncNodePurity
  rf_boston <- randomForest::randomForest(medv ~ ., data = Boston,
                                          importance = FALSE)
  gg_dta <- gg_vimp(rf_boston)

  expect_s3_class(gg_dta, "gg_vimp")
  expect_true("vimp" %in% colnames(gg_dta),
              info = "vimp column must exist regardless of original column name")
  expect_false(any(is.na(gg_dta$vimp)),
               info = "vimp values must not be NA")
  expect_true("positive" %in% colnames(gg_dta))
  expect_s3_class(plot(gg_dta), "ggplot")
})

## ── randomForest: one importance measure per ranking ─────────────────────────
## gg_vimp used to pivot every column of randomForest's $importance into a
## single `vimp` column and rank them together. %IncMSE (tens) and
## IncNodePurity (thousands) are incommensurable, so node purity swept the top
## of the ranking and the permutation values the user asked for by passing
## importance = TRUE were truncated off the end and never shown.

test_that("gg_vimp: randomForest importance=TRUE reports permutation VIMP", {
  skip_on_cran()
  skip_if_not_installed("randomForest")
  skip_if_not_installed("MASS")
  data(Boston, package = "MASS")
  set.seed(1)
  rf <- randomForest::randomForest(medv ~ ., Boston, importance = TRUE)
  skip_if_not(all(c("%IncMSE", "IncNodePurity") %in% colnames(rf$importance)),
              "randomForest did not store both measures")

  gg <- as.data.frame(gg_vimp(rf))

  # one measure only -- never both stacked in the same column
  expect_length(unique(gg$set), 1L)
  # and it is the permutation measure, not node purity
  expect_false(any(grepl("Purity", gg$set)))

  pct <- rf$importance[, "%IncMSE"]
  expect_equal(gg$vimp[1], unname(max(pct)))
  expect_equal(as.character(gg$vars[1]), names(which.max(pct)))
})

test_that("gg_vimp: randomForest importance=FALSE reports node purity, labelled", {
  skip_on_cran()
  skip_if_not_installed("randomForest")
  skip_if_not_installed("MASS")
  data(Boston, package = "MASS")
  set.seed(1)
  rf <- randomForest::randomForest(medv ~ ., Boston)
  expect_equal(colnames(rf$importance), "IncNodePurity")

  gg <- as.data.frame(gg_vimp(rf))
  expect_length(unique(gg$set), 1L)
  inc <- rf$importance[, "IncNodePurity"]
  expect_equal(gg$vimp[1], unname(max(inc)))
})

## The classification matrix mixes the same two scales: the per-class columns
## and MeanDecreaseAccuracy are permutation measures, MeanDecreaseGini is node
## impurity. Ranking them together left MeanDecreaseGini as the only survivor.
## The permutation columns are mutually commensurable, so they are kept and
## pivoted together, mirroring gg_vimp.rfsrc's all/<class> columns.

test_that("gg_vimp: randomForest classification importance=TRUE keeps the
          permutation block", {
  skip_on_cran()
  skip_if_not_installed("randomForest")
  set.seed(1)
  rf <- randomForest::randomForest(Species ~ ., iris, importance = TRUE)
  skip_if_not("MeanDecreaseGini" %in% colnames(rf$importance),
              "randomForest did not store node impurity")

  gg <- as.data.frame(gg_vimp(rf))

  # node impurity is incommensurable with the permutation columns, so it goes
  expect_false(any(grepl("Gini", gg$set)))
  # every permutation measure survives: one per class, plus the overall
  expect_setequal(
    unique(gg$set),
    c(levels(iris$Species), "MeanDecreaseAccuracy")
  )
  # and every variable is reported for every measure
  expect_equal(nrow(gg), 16L)

  # values are carried through untouched
  acc <- rf$importance[, "MeanDecreaseAccuracy"]
  got <- gg[gg$set == "MeanDecreaseAccuracy", ]
  expect_equal(
    got$vimp[order(as.character(got$vars))],
    unname(acc[order(names(acc))])
  )
})

test_that("gg_vimp: randomForest classification nvar counts variables, not rows", {
  skip_on_cran()
  skip_if_not_installed("randomForest")
  set.seed(1)
  rf <- randomForest::randomForest(Species ~ ., iris, importance = TRUE)

  gg <- as.data.frame(gg_vimp(rf, nvar = 2))

  # 2 variables x 4 permutation measures -- nvar truncates before the pivot,
  # so it never eats whole measures off the end of the ranking.
  expect_length(unique(as.character(gg$vars)), 2L)
  expect_equal(nrow(gg), 8L)
})

test_that("gg_vimp: randomForest classification importance=FALSE falls back to
          node purity", {
  skip_on_cran()
  skip_if_not_installed("randomForest")
  set.seed(1)
  rf <- randomForest::randomForest(Species ~ ., iris)
  expect_equal(colnames(rf$importance), "MeanDecreaseGini")

  gg <- as.data.frame(gg_vimp(rf))
  expect_length(unique(gg$set), 1L)
  gini <- rf$importance[, "MeanDecreaseGini"]
  expect_equal(gg$vimp[1], unname(max(gini)))
})
