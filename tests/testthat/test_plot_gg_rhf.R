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
