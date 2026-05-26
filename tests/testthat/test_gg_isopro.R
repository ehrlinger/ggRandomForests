# Tests for gg_isopro() — varPro::isopro tidy-data wrapper.
# Phase 4 of the v2.8.0 varPro integration.

make_iso_fit <- function(seed = 1L, method = "rnd", ntree = 25, sampsize = 16) {
  skip_if_not_installed("varPro")
  set.seed(seed)
  varPro::isopro(
    data     = iris[, 1:4],
    method   = method,
    sampsize = sampsize,
    ntree    = ntree
  )
}

test_that("gg_isopro: returns gg_isopro data.frame with correct columns", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_s3_class(gg, "gg_isopro")
  expect_s3_class(gg, "data.frame")
  expect_named(gg, c("obs", "case.depth", "howbad"), ignore.order = TRUE)
})

test_that("gg_isopro: row count equals n; howbad in [0,1]; obs is 1..n", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_equal(nrow(gg), nrow(iris))
  expect_true(all(gg$howbad >= 0 & gg$howbad <= 1))
  expect_equal(gg$obs, seq_len(nrow(iris)))
})

test_that("gg_isopro: provenance attribute attached", {
  fit  <- make_iso_fit(ntree = 25)
  gg   <- gg_isopro(fit)
  prov <- attr(gg, "provenance")
  expect_type(prov, "list")
  expect_equal(prov$source, "varPro::isopro")
  expect_equal(prov$n, nrow(iris))
  expect_equal(prov$ntree, 25)
})

test_that("plot.gg_isopro: panel='both' returns patchwork with 2 sub-plots that build", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  p   <- plot(gg, panel = "both")
  expect_s3_class(p, "patchwork")
  expect_length(p$patches$plots, 1L) # left panel is `p` itself; right is in $patches$plots
  for (sub in c(list(p), p$patches$plots)) {
    expect_no_error(ggplot2::ggplot_build(sub))
  }
})

test_that("plot.gg_isopro: panel='elbow' returns a single ggplot", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  p   <- plot(gg, panel = "elbow")
  expect_s3_class(p, "ggplot")
  expect_false(inherits(p, "patchwork"))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot.gg_isopro: panel='density' returns a single ggplot", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  p   <- plot(gg, panel = "density")
  expect_s3_class(p, "ggplot")
  expect_false(inherits(p, "patchwork"))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot.gg_isopro: rejects bad panel values via match.arg", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_error(plot(gg, panel = "nope"))
})
