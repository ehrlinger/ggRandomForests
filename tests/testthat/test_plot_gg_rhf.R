test_that("plot.gg_rhf builds a ggplot of per-case hazard curves", {
  gg <- gg_rhf(.rhf_pbc())
  p  <- plot(gg, idx = c(1, 5, 10))
  expect_s3_class(p, "ggplot")
  ld <- ggplot2::layer_data(p)            # forces a real build
  expect_gt(nrow(ld), 0)
  expect_equal(p$labels$y, "Hazard")
})

test_that("plot.gg_rhf hazard.only = FALSE plots cumulative hazard", {
  gg <- gg_rhf(.rhf_pbc())
  p  <- plot(gg, idx = 1, hazard.only = FALSE)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "Cumulative hazard")
})

test_that("plot.gg_rhf errors on idx not present", {
  gg <- gg_rhf(.rhf_pbc())
  expect_error(plot(gg, idx = -999L), "idx")
})

test_that("plot.gg_rhf warns on partially-unmatched idx but still builds", {
  gg <- gg_rhf(.rhf_pbc())
  expect_warning(p <- plot(gg, idx = c(1L, -999L)), "idx")
  expect_s3_class(p, "ggplot")
  expect_gt(nrow(ggplot2::layer_data(p)), 0)
})

test_that("plot.gg_rhf drops the NA hazard cells instead of warning", {
  # randomForestRHF >= 2.0.0 returns NA hazard outside each case's observed
  # path, which geom_line() would otherwise report as removed missing values on
  # every hazard plot. The curve is identical either way -- it simply stops at
  # the end of follow-up -- so the NA is dropped here rather than surfaced as a
  # warning about data the forest was never asked to estimate.
  gg <- gg_rhf(.rhf_pbc())
  p  <- plot(gg)
  # handle_na() runs at draw time, not build time, so the gtable has to be
  # realised for the warning to be provoked at all.
  expect_no_warning(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
  ld <- ggplot2::layer_data(p)
  expect_gt(nrow(ld), 0)
  expect_false(anyNA(ld$y))
})

test_that("plot.gg_rhf hazard.only = FALSE keeps every chf row", {
  # chf carries no NA mask, so the cumulative-hazard panel must not lose rows
  # to the hazard-side filter.
  gg <- gg_rhf(.rhf_pbc())
  p  <- plot(gg, idx = c(1, 5, 10), hazard.only = FALSE)
  ld <- ggplot2::layer_data(p)
  expect_equal(nrow(ld), 3L * attr(gg, "ntime"))
})
