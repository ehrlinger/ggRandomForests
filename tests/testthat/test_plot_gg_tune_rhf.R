.fake_rhf_tune_risk <- function() {
  structure(list(
    best.size = 8L,
    best.err = 0.24,
    bounds = c(lower = 2L, upper = 12L),
    n.subjects = 40L,
    C = 3,
    method = "golden",
    perf = "risk",
    path = data.frame(
      treesize = c(2L, 5L, 8L, 12L),
      risk = c(0.40, 0.30, 0.24, 0.29)
    ),
    forest = structure(list(marker = "must not be copied"), class = "rhf")
  ), class = "tune.treesize.rhf")
}

.fake_rhf_tune_iauc <- function(with_se = TRUE) {
  path <- data.frame(
    treesize = c(3L, 6L, 9L),
    iAUC = c(0.68, 0.79, 0.74)
  )
  if (with_se) {
    path$iAUC.se <- c(0.04, 0.03, 0.05)
  }
  structure(list(
    best.size = 6L,
    best.err = 0.21,
    bounds = c(lower = 3L, upper = 9L),
    n.subjects = 30L,
    C = 3,
    method = "bisect",
    perf = "iAUC",
    path = path
  ), class = "tune.treesize.rhf")
}

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
