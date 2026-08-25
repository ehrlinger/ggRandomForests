test_that("plot.gg_tune_rhf draws the risk path and selected size", {
  x <- gg_tune_rhf(.fake_rhf_tune_risk())
  p <- plot(x)
  built <- ggplot2::ggplot_build(p)

  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$x, "Tree size")
  expect_identical(p$labels$y, "OOB risk")
  expect_equal(nrow(built$data[[1L]]), nrow(x))
  expect_equal(sum(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomRibbon")
  }, logical(1))), 0L)
})

test_that("plot.gg_tune_rhf adds an iAUC ribbon only for finite standard errors", {
  with_se <- plot(gg_tune_rhf(.fake_rhf_tune_iauc()))
  without_se <- plot(gg_tune_rhf(.fake_rhf_tune_iauc(with_se = FALSE)))
  disabled <- plot(gg_tune_rhf(.fake_rhf_tune_iauc()), se_band = FALSE)

  ribbon_count <- function(p) {
    sum(vapply(p$layers, function(layer) {
      inherits(layer$geom, "GeomRibbon")
    }, logical(1)))
  }
  expect_equal(ribbon_count(with_se), 1L)
  expect_equal(ribbon_count(without_se), 0L)
  expect_equal(ribbon_count(disabled), 0L)
})

test_that("plot.gg_tune_rhf validates its object and display arguments", {
  expect_error(plot.gg_tune_rhf(data.frame()), "gg_tune_rhf")
  x <- gg_tune_rhf(.fake_rhf_tune_iauc())
  expect_error(plot(x, se_band = NA), "se_band")
  expect_error(plot(x, se_mult = 0), "se_mult")
})
