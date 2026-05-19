# Regression guard for the v2.8.0 Phase 0 dependency modernization.
# Asserts the Depends->Imports migration took effect and stays effective.

test_that("DESCRIPTION Depends declares only the R version constraint", {
  # system.file locates DESCRIPTION in installed contexts (R CMD check,
  # installed-from-source) and via pkgload's shim under devtools::test.
  # Fall back to the relative path for bare testthat::test_file on the
  # source tree, where the package is not installed (system.file -> "").
  desc_path <- system.file("DESCRIPTION", package = "ggRandomForests")
  if (!nzchar(desc_path)) desc_path <- testthat::test_path("..", "..", "DESCRIPTION")
  deps <- read.dcf(desc_path, fields = "Depends")[1, 1]
  deps_pkgs <- trimws(strsplit(deps, ",")[[1]])
  # Every entry must be the R(>= x) constraint, not a package.
  expect_true(all(grepl("^R([[:space:]]|$)", deps_pkgs)),
              info = paste("Depends still attaches packages:", deps))
})

test_that("core extractors run without randomForestSRC/randomForest attached", {
  skip_if_not_installed("callr")
  skip_if_not_installed("randomForestSRC")
  skip_if_not_installed("randomForest")
  # Run ONLY under R CMD check, where the package under test is freshly
  # installed into the check library. `_R_CHECK_PACKAGE_NAME_` is set by
  # R CMD check during its test phase and is absent under devtools::test()
  # / bare testthat::test_file(), so this skips there instead of loading a
  # possibly-stale separately-installed ggRandomForests in the callr child.
  skip_if_not(
    nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")),
    "clean-session test runs only under R CMD check"
  )

  # Pass the test process's library paths into the child so it resolves
  # ggRandomForests from the SAME library as the check run (the freshly
  # built copy), not an unrelated installed version.
  out <- callr::r(
    function() {
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
    },
    libpath = .libPaths()
  )
  expect_true(out)
})
