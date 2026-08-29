# Tests for gg_udependent (Phase 3)

## ── Helpers ──────────────────────────────────────────────────────────────────

make_uvp <- function(ntree = 25L) {
  # uvarpro() defaults to method = "auto", which grows a multivariate forest
  # from a real formula, so yvar.wt is non-empty and it does NOT trip the
  # entry.c:184 gcc-UBSAN report. Runs on CRAN. See helper-varpro-fixtures.R
  # for the calls that DO reach rfsrc without a formula.
  set.seed(42L)
  varPro::uvarpro(iris[, -5L], ntree = ntree)
}

# gg_udependent() recomputes varPro::get.beta.entropy() on every call (~1.5s,
# the only slow step, and a pure function of the fit). The tests below exercise
# the same fit under several argument combinations, so memoise the result per
# argument signature: identical coverage, but one entropy computation per
# distinct call instead of one per test (this file was ~24s of the suite).
.ggu_cache <- new.env(parent = emptyenv())

# .quiet = TRUE suppresses warnings only for callers that legitimately warn
# (the empty-graph threshold cases); every other call leaves warnings live so
# an unexpected new warning still surfaces as a test failure.
make_ggu <- function(..., .quiet = FALSE) {
  key <- paste(deparse(list(...)), collapse = "")
  if (is.null(.ggu_cache[[key]])) {
    .ggu_cache[[key]] <- if (.quiet) {
      suppressWarnings(gg_udependent(make_uvp(), ...))
    } else {
      gg_udependent(make_uvp(), ...)
    }
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
  skip_on_cran()
  uv <- make_uvp()
  expect_error(gg_udependent(uv, threshold = -0.1), regexp = "threshold")
  expect_error(gg_udependent(uv, threshold = 0),    regexp = "threshold")
})

## ── Class & structure ────────────────────────────────────────────────────────

test_that("gg_udependent returns gg_udependent class", {
  skip_on_cran()
  expect_s3_class(make_ggu(), "gg_udependent")
})

test_that("gg_udependent$edges has required columns", {
  skip_on_cran()
  gg <- make_ggu()
  expect_true(all(c("variable_from", "variable_to", "weight") %in% names(gg$edges)))
  expect_type(gg$edges$weight, "double")
})

test_that("gg_udependent$nodes has required columns", {
  skip_on_cran()
  gg <- make_ggu()
  expect_true(all(c("variable", "degree", "selected") %in% names(gg$nodes)))
  expect_s3_class(gg$nodes$variable, "factor")
  expect_type(gg$nodes$degree,   "integer")
  expect_type(gg$nodes$selected, "logical")
})

test_that("gg_udependent$graph is an igraph", {
  skip_on_cran()
  skip_if_not_installed("igraph")
  gg <- make_ggu()
  expect_true(igraph::is_igraph(gg$graph))
})

test_that("gg_udependent directed=TRUE returns directed igraph", {
  skip_on_cran()
  skip_if_not_installed("igraph")
  gg <- make_ggu(directed = TRUE)
  expect_true(igraph::is_directed(gg$graph))
})

test_that("gg_udependent directed=FALSE returns undirected igraph", {
  skip_on_cran()
  skip_if_not_installed("igraph")
  gg <- make_ggu(directed = FALSE)
  expect_false(igraph::is_directed(gg$graph))
})

test_that("gg_udependent$edges is empty data frame (not NULL) for empty graph", {
  skip_on_cran()
  # threshold=999 -> no edges -> empty graph
  gg <- make_ggu(threshold = 999, .quiet = TRUE)
  expect_false(is.null(gg$edges))
  expect_s3_class(gg$edges, "data.frame")
  expect_equal(nrow(gg$edges), 0L)
})

test_that("gg_udependent$nodes is empty data frame for empty graph", {
  skip_on_cran()
  gg <- make_ggu(threshold = 999, .quiet = TRUE)
  expect_false(is.null(gg$nodes))
  expect_equal(nrow(gg$nodes), 0L)
})

## ── Provenance ───────────────────────────────────────────────────────────────

test_that("gg_udependent provenance has all expected fields", {
  skip_on_cran()
  gg   <- make_ggu()
  prov <- attr(gg, "provenance")
  expect_type(prov, "list")
  expect_true(all(c("threshold", "q.signal", "directed", "min.degree",
                    "xvar.names", "n") %in% names(prov)))
})

test_that("gg_udependent provenance threshold matches argument", {
  skip_on_cran()
  gg <- make_ggu(threshold = 0.5)
  expect_equal(attr(gg, "provenance")$threshold, 0.5)
})

## ── S3 companions ────────────────────────────────────────────────────────────

test_that("print.gg_udependent returns object invisibly", {
  skip_on_cran()
  gg  <- make_ggu()
  out <- capture.output(ret <- print(gg))
  expect_identical(ret, gg)
  expect_true(any(grepl("gg_udependent", out)))
})

test_that("summary.gg_udependent returns summary.gg_udependent class", {
  skip_on_cran()
  gg <- make_ggu()
  s  <- summary(gg)
  expect_s3_class(s, "summary.gg_udependent")
})

test_that("autoplot.gg_udependent returns a ggplot", {
  skip_on_cran()
  skip_if_not_installed("ggraph")
  gg <- make_ggu()
  expect_s3_class(ggplot2::autoplot(gg), "ggplot")
})

## ── Plot smoke tests ─────────────────────────────────────────────────────────

test_that("plot.gg_udependent default returns a ggplot", {
  skip_on_cran()
  skip_if_not_installed("ggraph")
  gg <- make_ggu()
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_udependent layout='kk' returns a ggplot", {
  skip_on_cran()
  skip_if_not_installed("ggraph")
  gg <- make_ggu()
  p  <- plot(gg, layout = "kk")
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_udependent empty graph -> stop with informative message", {
  skip_on_cran()
  gg <- make_ggu(threshold = 999, .quiet = TRUE)
  expect_error(plot(gg), regexp = "no edges")
})

## ── vdiffr snapshots — see test_snapshots.R ──────────────────────────────────
## Visual regression tests for plot.gg_udependent are in test_snapshots.R
## (guarded by VDIFFR_RUN_TESTS=true), following the package convention.

## ---- labels= on the node text (issue #243) ---------------------------------

# The node text is drawn by geom_node_label(); ggraph resolves node data at
# build time, so read the built layer rather than the graph.
.ggu_node_labels <- function(p) {
  built <- ggplot2::ggplot_build(p)
  lab <- unlist(lapply(built$data, function(d) {
    if ("label" %in% names(d)) as.character(d$label) else NULL
  }))
  sort(unique(lab))
}

test_that("plot.gg_udependent labels the node text", {
  skip_on_cran()
  skip_if_not_installed("ggraph")
  gg <- make_ggu()
  raw <- igraph::V(gg$graph)$name
  target <- raw[1L]

  p <- plot(gg, labels = stats::setNames("RENAMED NODE", target))
  labs <- .ggu_node_labels(p)

  expect_true("RENAMED NODE" %in% labs)
  expect_false(target %in% labs)
  if (length(raw) > 1L) expect_true(raw[2L] %in% labs)   # fallback per node
})

test_that("plot.gg_udependent leaves the igraph vertex key untouched", {
  skip_on_cran()
  skip_if_not_installed("ggraph")
  gg <- make_ggu()
  raw <- igraph::V(gg$graph)$name
  target <- raw[1L]

  # 'name' is the vertex key the edge-weight backfill matches on, so relabelling
  # must not rewrite it, and the display string must live on a separate
  # attribute.
  expect_null(igraph::vertex_attr(gg$graph, "node_label"))

  p <- plot(gg, labels = stats::setNames("RENAMED NODE", target))

  # The caller's graph is untouched: same key, and still no display attribute.
  expect_equal(igraph::V(gg$graph)$name, raw)
  expect_null(igraph::vertex_attr(gg$graph, "node_label"))

  # ...while the drawn node text did change, so the two are genuinely distinct.
  expect_true("RENAMED NODE" %in% .ggu_node_labels(p))
})

test_that("plot.gg_udependent with labels = NULL keeps the raw node names", {
  skip_on_cran()
  skip_if_not_installed("ggraph")
  gg <- make_ggu()
  raw <- igraph::V(gg$graph)$name

  expect_true(all(raw %in% .ggu_node_labels(plot(gg))))
})

test_that("plot.gg_udependent warns once when no label resolves", {
  skip_on_cran()
  skip_if_not_installed("ggraph")
  gg <- make_ggu()
  target <- igraph::V(gg$graph)$name[1L]

  expect_warning(plot(gg, labels = stats::setNames("", target)),
                 "No variable labels")
})
