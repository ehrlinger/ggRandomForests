# Tests for gg_udependent (Phase 3)

## ── Helpers ──────────────────────────────────────────────────────────────────

make_uvp <- function(ntree = 25L) {
  set.seed(42L)
  varPro::uvarpro(iris[, -5L], ntree = ntree)
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
  uv <- make_uvp()
  expect_s3_class(gg_udependent(uv), "gg_udependent")
})

test_that("gg_udependent$edges has required columns", {
  uv <- make_uvp()
  gg <- gg_udependent(uv)
  expect_true(all(c("variable_from", "variable_to", "weight") %in% names(gg$edges)))
  expect_type(gg$edges$weight, "double")
})

test_that("gg_udependent$nodes has required columns", {
  uv <- make_uvp()
  gg <- gg_udependent(uv)
  expect_true(all(c("variable", "degree", "selected") %in% names(gg$nodes)))
  expect_s3_class(gg$nodes$variable, "factor")
  expect_type(gg$nodes$degree,   "integer")
  expect_type(gg$nodes$selected, "logical")
})

test_that("gg_udependent$graph is an igraph", {
  skip_if_not_installed("igraph")
  uv <- make_uvp()
  gg <- gg_udependent(uv)
  expect_true(igraph::is_igraph(gg$graph))
})

test_that("gg_udependent directed=TRUE returns directed igraph", {
  skip_if_not_installed("igraph")
  uv <- make_uvp()
  gg <- gg_udependent(uv, directed = TRUE)
  expect_true(igraph::is_directed(gg$graph))
})

test_that("gg_udependent directed=FALSE returns undirected igraph", {
  skip_if_not_installed("igraph")
  uv <- make_uvp()
  gg <- gg_udependent(uv, directed = FALSE)
  expect_false(igraph::is_directed(gg$graph))
})

test_that("gg_udependent$edges is empty data frame (not NULL) for empty graph", {
  uv <- make_uvp()
  # threshold=999 -> no edges -> empty graph
  gg <- suppressWarnings(gg_udependent(uv, threshold = 999))
  expect_false(is.null(gg$edges))
  expect_s3_class(gg$edges, "data.frame")
  expect_equal(nrow(gg$edges), 0L)
})

test_that("gg_udependent$nodes is empty data frame for empty graph", {
  uv <- make_uvp()
  gg <- suppressWarnings(gg_udependent(uv, threshold = 999))
  expect_false(is.null(gg$nodes))
  expect_equal(nrow(gg$nodes), 0L)
})

## ── Provenance ───────────────────────────────────────────────────────────────

test_that("gg_udependent provenance has all expected fields", {
  uv <- make_uvp()
  gg   <- gg_udependent(uv)
  prov <- attr(gg, "provenance")
  expect_type(prov, "list")
  expect_true(all(c("threshold", "q.signal", "directed", "min.degree",
                    "xvar.names", "n") %in% names(prov)))
})

test_that("gg_udependent provenance threshold matches argument", {
  uv <- make_uvp()
  gg <- gg_udependent(uv, threshold = 0.5)
  expect_equal(attr(gg, "provenance")$threshold, 0.5)
})

## ── S3 companions ────────────────────────────────────────────────────────────

test_that("print.gg_udependent returns object invisibly", {
  uv  <- make_uvp()
  gg  <- gg_udependent(uv)
  out <- capture.output(ret <- print(gg))
  expect_identical(ret, gg)
  expect_true(any(grepl("gg_udependent", out)))
})

test_that("summary.gg_udependent returns summary.gg_udependent class", {
  uv <- make_uvp()
  gg <- gg_udependent(uv)
  s  <- summary(gg)
  expect_s3_class(s, "summary.gg_udependent")
})

test_that("autoplot.gg_udependent returns a ggplot", {
  skip_if_not_installed("ggraph")
  uv <- make_uvp()
  gg <- gg_udependent(uv)
  expect_s3_class(ggplot2::autoplot(gg), "ggplot")
})
