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

test_that("plot.gg_rhf hazard.only = FALSE keeps every unmasked chf row", {
  # From randomForestRHF 2.0.3 chf is NA after each case's final stop, so the
  # cumulative-hazard panel loses those cells and no others.
  idx <- c(1, 5, 10)
  gg  <- gg_rhf(.rhf_pbc())
  p   <- plot(gg, idx = idx, hazard.only = FALSE)
  ld  <- ggplot2::layer_data(p)
  expect_equal(nrow(ld), sum(!is.na(gg$chf[gg$id %in% idx])))
  expect_false(anyNA(ld$y))
})

test_that("plot.gg_rhf filters on the column it draws, not on hazard", {
  # The two masks coincide on the .rhf_pbc() fixture, whose cases each carry a
  # single interval, so it cannot catch a panel filtered by the wrong column.
  # With time-dependent covariates they come apart: randomForestRHF 2.0.3 holds
  # chf flat through an internal gap while the hazard goes NA there. Build that
  # shape directly rather than fitting a forest to reach it.
  gg <- data.frame(
    id     = rep(1L, 4L),
    time   = c(1, 2, 3, 4),
    hazard = c(0.1, NA, 0.3, NA),   # NA in an internal gap and after the stop
    chf    = c(0.1, 0.1, 0.4, NA),  # flat through the gap, NA after the stop
    source = "oob"
  )
  attr(gg, "ntime") <- 4L
  class(gg) <- c("gg_rhf", "data.frame")

  expect_equal(nrow(ggplot2::layer_data(plot(gg))), 2L)
  expect_equal(nrow(ggplot2::layer_data(plot(gg, hazard.only = FALSE))), 3L)
})
