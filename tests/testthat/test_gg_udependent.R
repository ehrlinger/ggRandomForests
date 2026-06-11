# Tests for gg_udependent (Phase 3)

## ── Helpers ──────────────────────────────────────────────────────────────────

make_uvp <- function(ntree = 25L) {
  set.seed(42L)
  varPro::uvarpro(iris[, -5L], ntree = ntree)
}

# gg_udependent() recomputes varPro::get.beta.entropy() on every call (~1.5s,
# the only slow step, and a pure function of the fit). The tests below exercise
# the same fit under several argument combinations, so memoise the result per
# argument signature: identical coverage, but one entropy computation per
# distinct call instead of one per test (this file was ~24s of the suite).
.ggu_cache <- new.env(parent = emptyenv())

make_ggu <- function(...) {
  key <- paste(deparse(list(...)), collapse = "")
  if (is.null(.ggu_cache[[key]])) {
    .ggu_cache[[key]] <- suppressWarnings(gg_udependent(make_uvp(), ...))
  }
  .ggu_cache[[key]]
}

## ── Input validation ─────────────────────────────────────────────────────────

test_that("gg_udependent: missing object -> stop", {
  expect_error(gg_udependent(), regexp = "object")
})

test_that("gg_udependent: non-uvarpro object -> stop", {
  expect_error(gg_udependent(list(x = 1)), regexp = "uvarpro")
})

test_that("gg_udependent: non-positive threshold -> stop", {
  uv <- make_uvp()
  expect_error(gg_udependent(uv, threshold = -0.1), regexp = "threshold")
  expect_error(gg_udependent(uv, threshold = 0),    regexp = "threshold")
})

## ── Class & structure ────────────────────────────────────────────────────────

test_that("gg_udependent returns gg_udependent class", {
  expect_s3_class(make_ggu(), "gg_udependent")
})

test_that("gg_udependent$edges has required columns", {
  gg <- make_ggu()
  expect_true(all(c("variable_from", "variable_to", "weight") %in% names(gg$edges)))
  expect_type(gg$edges$weight, "double")
})

test_that("gg_udependent$nodes has required columns", {
  gg <- make_ggu()
  expect_true(all(c("variable", "degree", "selected") %in% names(gg$nodes)))
  expect_s3_class(gg$nodes$variable, "factor")
  expect_type(gg$nodes$degree,   "integer")
  expect_type(gg$nodes$selected, "logical")
})

test_that("gg_udependent$graph is an igraph", {
  skip_if_not_installed("igraph")
  gg <- make_ggu()
  expect_true(igraph::is_igraph(gg$graph))
})

test_that("gg_udependent directed=TRUE returns directed igraph", {
  skip_if_not_installed("igraph")
  gg <- make_ggu(directed = TRUE)
  expect_true(igraph::is_directed(gg$graph))
})

test_that("gg_udependent directed=FALSE returns undirected igraph", {
  skip_if_not_installed("igraph")
  gg <- make_ggu(directed = FALSE)
  expect_false(igraph::is_directed(gg$graph))
})

test_that("gg_udependent$edges is empty data frame (not NULL) for empty graph", {
  # threshold=999 -> no edges -> empty graph
  gg <- make_ggu(threshold = 999)
  expect_false(is.null(gg$edges))
  expect_s3_class(gg$edges, "data.frame")
  expect_equal(nrow(gg$edges), 0L)
})

test_that("gg_udependent$nodes is empty data frame for empty graph", {
  gg <- make_ggu(threshold = 999)
  expect_false(is.null(gg$nodes))
  expect_equal(nrow(gg$nodes), 0L)
})

## ── Provenance ───────────────────────────────────────────────────────────────

test_that("gg_udependent provenance has all expected fields", {
  gg   <- make_ggu()
  prov <- attr(gg, "provenance")
  expect_type(prov, "list")
  expect_true(all(c("threshold", "q.signal", "directed", "min.degree",
                    "xvar.names", "n") %in% names(prov)))
})

test_that("gg_udependent provenance threshold matches argument", {
  gg <- make_ggu(threshold = 0.5)
  expect_equal(attr(gg, "provenance")$threshold, 0.5)
})

## ── S3 companions ────────────────────────────────────────────────────────────

test_that("print.gg_udependent returns object invisibly", {
  gg  <- make_ggu()
  out <- capture.output(ret <- print(gg))
  expect_identical(ret, gg)
  expect_true(any(grepl("gg_udependent", out)))
})

test_that("summary.gg_udependent returns summary.gg_udependent class", {
  gg <- make_ggu()
  s  <- summary(gg)
  expect_s3_class(s, "summary.gg_udependent")
})

test_that("autoplot.gg_udependent returns a ggplot", {
  skip_if_not_installed("ggraph")
  gg <- make_ggu()
  expect_s3_class(ggplot2::autoplot(gg), "ggplot")
})

## ── Plot smoke tests ─────────────────────────────────────────────────────────

test_that("plot.gg_udependent default returns a ggplot", {
  skip_if_not_installed("ggraph")
  gg <- make_ggu()
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_udependent layout='kk' returns a ggplot", {
  skip_if_not_installed("ggraph")
  gg <- make_ggu()
  p  <- plot(gg, layout = "kk")
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_udependent empty graph -> stop with informative message", {
  gg <- make_ggu(threshold = 999)
  expect_error(plot(gg), regexp = "no edges")
})

## ── vdiffr snapshots — see test_snapshots.R ──────────────────────────────────
## Visual regression tests for plot.gg_udependent are in test_snapshots.R
## (guarded by VDIFFR_RUN_TESTS=true), following the package convention.
