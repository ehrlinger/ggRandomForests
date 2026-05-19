# Regression guard for the v2.8.0 Phase 0 dependency modernization.
# Asserts the Depends->Imports migration took effect and stays effective.

test_that("DESCRIPTION Depends declares only the R version constraint", {
  deps <- read.dcf("../../DESCRIPTION", fields = "Depends")[1, 1]
  deps_pkgs <- trimws(strsplit(deps, ",")[[1]])
  # Every entry must be the R(>= x) constraint, not a package.
  expect_true(all(grepl("^R\\b", deps_pkgs)),
              info = paste("Depends still attaches packages:", deps))
})

test_that("core extractors run without randomForestSRC/randomForest attached", {
  skip_if_not_installed("callr")
  skip_if_not_installed("randomForestSRC")
  skip_if_not_installed("ggRandomForests")

  out <- callr::r(function() {
    # Load ggRandomForests WITHOUT attaching its former Depends.
    suppressMessages(library(ggRandomForests))
    attached <- search()
    stopifnot(!("package:randomForestSRC" %in% attached))
    stopifnot(!("package:randomForest" %in% attached))

    # A regression forest extractor must work with rfsrc only namespaced.
    rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airquality,
                                 ntree = 30, na.action = "na.impute")
    g <- gg_error(rf)
    inherits(g, "gg_error")
  })
  expect_true(out)
})
