if (requireNamespace("lintr", quietly = TRUE)) {
  test_that("Package Style", {
    # Skipped on CRAN for two reasons: lint_package() needs the package
    # source tree, which a check of the installed package does not have,
    # and the overall check has a hard sub-10-minute budget. Locally this
    # costs ~15s under devtools::test(); CI also runs a dedicated lint job.
    skip_on_cran()
    skip_if_not_installed("lintr")
    lintr::expect_lint_free()
  })
}
