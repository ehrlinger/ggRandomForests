# gg_udependent (varPro Phase 3) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement `gg_udependent()` — a tidy extractor + ggraph plot method that wraps `varPro::get.beta.entropy()` + `varPro::sdependent()` on a `uvarpro` fit.

**Architecture:** `gg_udependent()` calls `varPro::get.beta.entropy(object)` to get the cross-variable dependency matrix `I`, passes it to `varPro::sdependent(I, ..., plot=FALSE)` for signal detection, then rebuilds an igraph and tidy `$edges`/`$nodes` frames. `plot.gg_udependent()` is pure ggraph rendering.

**Tech Stack:** R, varPro (get.beta.entropy, sdependent), igraph (Suggests), ggraph (Suggests), ggplot2, testthat, vdiffr

---

## Key API Facts (discovery already done — do not re-discover)

```r
# Step 1: get cross-variable dependency matrix
I <- varPro::get.beta.entropy(object)
# Returns p' x p' numeric matrix (p' = vars with non-zero importance)
# rownames = colnames = variable names
# I[i, j] = importance of var j for distinguishing var i's regions

# Step 2: run dependency detection
sdep <- varPro::sdependent(I, threshold=0.25, q.signal=0.75,
                            directed=TRUE, min.degree=NULL, plot=FALSE)
# Returns EITHER:
#   list(signal.vars=chr[], imp.score=named_num[], degree=named_num[])
#   OR a character string "graph is null after removing isolated nodes..."

# Step 3: rebuild igraph (sdependent does NOT return the graph)
A  <- (I >= threshold) * 1
diag(A) <- 0
g  <- igraph::graph_from_adjacency_matrix(
        A, mode = if (directed) "directed" else "undirected", diag = FALSE)
isolated <- igraph::degree(g, mode = "all") == 0
g  <- igraph::delete_vertices(g, which(isolated))

# Edge weights come from I, not from A:
edges <- igraph::as_data_frame(g, what = "edges")   # columns: from, to
edges$weight <- mapply(function(i, j) I[i, j], edges$from, edges$to)
names(edges)[1:2] <- c("variable_from", "variable_to")
```

---

## File Map

| Action | Path |
|---|---|
| Create | `R/gg_udependent.R` |
| Create | `R/plot.gg_udependent.R` |
| Create | `tests/testthat/test_gg_udependent.R` |
| Modify | `DESCRIPTION` — bump version, add ggraph to Suggests |
| Modify | `NEWS.md` — Phase 3 bullet |
| Auto-generate | `man/gg_udependent.Rd`, `man/plot.gg_udependent.Rd` |
| Record | `tests/testthat/_snaps/` — vdiffr baseline |

---

## Task 0: Dev cycle setup

**Files:**
- Modify: `DESCRIPTION`

- [ ] **Step 1: Bump version and add ggraph to Suggests**

Open `DESCRIPTION`. Make two changes:

Change line 4:
```
Version: 2.7.3.9003
```
to:
```
Version: 2.7.3.9004
```

In the `Suggests:` block, add `ggraph,` after `igraph,`:
```
    igraph,
    ggraph,
    callr
```

- [ ] **Step 2: Verify devtools::document() runs clean**

```bash
cd /path/to/varpro-phase3-worktree
Rscript -e "devtools::document()"
```
Expected: no errors, NAMESPACE updated.

- [ ] **Step 3: Commit**

```bash
git add DESCRIPTION NAMESPACE
git commit -m "chore: open 2.7.3.9004 dev cycle; add ggraph to Suggests"
```

---

## Task 1: `gg_udependent` extractor (TDD)

**Files:**
- Create: `tests/testthat/test_gg_udependent.R`
- Create: `R/gg_udependent.R`

### Step 1: Write the failing tests

Create `tests/testthat/test_gg_udependent.R`:

```r
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

test_that("gg_udependent: threshold not in (0,1) -> stop", {
  uv <- make_uvp()
  expect_error(gg_udependent(uv, threshold = 1.5), regexp = "threshold")
  expect_error(gg_udependent(uv, threshold = 0),   regexp = "threshold")
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
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_gg_udependent.R')"
```
Expected: all fail with "could not find function 'gg_udependent'".

- [ ] **Step 3: Create `R/gg_udependent.R`**

```r
##=============================================================================
#' Variable dependency graph from a uvarpro model
#'
#' Extracts cross-variable dependency scores from a fitted \code{uvarpro}
#' object using \code{\link[varPro]{get.beta.entropy}} and
#' \code{\link[varPro]{sdependent}}, and returns a tidy list suitable for
#' \code{plot.gg_udependent}.
#'
#' @param object A fitted \code{uvarpro} object (required).
#' @param threshold Numeric in (0, 1); dependency threshold passed to
#'   \code{sdependent()}. An edge \eqn{i \to j} is drawn when
#'   \code{I[i, j] >= threshold}. Default \code{0.25}.
#' @param q.signal Quantile threshold (0--1) for signal variable selection;
#'   passed to \code{sdependent()}. Default \code{0.75}.
#' @param directed Logical; \code{TRUE} (default) builds a directed igraph.
#' @param min.degree Integer or \code{NULL}. When non-\code{NULL}, only nodes
#'   with degree \eqn{\ge} \code{min.degree} are retained in \code{$nodes},
#'   \code{$edges}, and \code{$graph}.
#' @param ... Additional arguments forwarded to \code{varPro::sdependent()}.
#'
#' @return A named list of class \code{"gg_udependent"} with elements:
#' \describe{
#'   \item{\code{$edges}}{Data frame: \code{variable_from}, \code{variable_to},
#'     \code{weight} (raw cross-importance value).}
#'   \item{\code{$nodes}}{Data frame: \code{variable} (factor, levels by
#'     descending degree), \code{degree} (integer), \code{selected} (logical,
#'     \code{TRUE} if in \code{sdependent}'s signal set).}
#'   \item{\code{$graph}}{igraph object. \code{NULL} if no dependencies
#'     detected.}
#' }
#' A \code{"provenance"} attribute carries \code{threshold}, \code{q.signal},
#' \code{directed}, \code{min.degree}, \code{xvar.names}, and \code{n}.
#'
#' @seealso \code{\link{plot.gg_udependent}}
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' uv <- varPro::uvarpro(iris[, -5], ntree = 50)
#' gg <- gg_udependent(uv)
#' print(gg)
#' }
#'
#' @importFrom varPro get.beta.entropy sdependent
#' @export
gg_udependent <- function(object,
                           threshold  = 0.25,
                           q.signal   = 0.75,
                           directed   = TRUE,
                           min.degree = NULL,
                           ...) {
  .validate_udep_inputs(object, threshold, directed)

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required. Install it with: install.packages('igraph')",
         call. = FALSE)
  }

  ## ---- Compute cross-variable dependency matrix ----------------------------
  imp_mat <- varPro::get.beta.entropy(object)

  ## ---- Call sdependent for signal detection --------------------------------
  sdep <- varPro::sdependent(imp_mat, threshold = threshold,
                              q.signal = q.signal, directed = directed,
                              min.degree = min.degree, plot = FALSE, ...)

  ## ---- Handle empty graph --------------------------------------------------
  if (is.character(sdep)) {
    warning("gg_udependent: ", sdep,
            "\nReturning empty structure. Consider lowering threshold.",
            call. = FALSE)
    empty_edges <- data.frame(variable_from = character(0),
                               variable_to   = character(0),
                               weight        = numeric(0),
                               stringsAsFactors = FALSE)
    empty_nodes <- data.frame(variable = factor(character(0)),
                               degree   = integer(0),
                               selected = logical(0),
                               stringsAsFactors = FALSE)
    result <- structure(
      list(edges = empty_edges, nodes = empty_nodes, graph = NULL),
      class = c("gg_udependent", "list")
    )
    attr(result, "provenance") <- .udep_provenance(object, threshold, q.signal,
                                                     directed, min.degree)
    return(result)
  }

  ## ---- Build igraph from adjacency -----------------------------------------
  A <- (imp_mat >= threshold) * 1
  diag(A) <- 0
  g <- igraph::graph_from_adjacency_matrix(
    A,
    mode  = if (isTRUE(directed)) "directed" else "undirected",
    diag  = FALSE
  )
  isolated <- igraph::degree(g, mode = "all") == 0
  g <- igraph::delete_vertices(g, which(isolated))

  ## ---- Build tidy edge data frame with raw weights -------------------------
  edge_df <- igraph::as_data_frame(g, what = "edges")
  if (nrow(edge_df) > 0L) {
    edge_df$weight <- mapply(
      function(i, j) imp_mat[i, j],
      edge_df[[1L]], edge_df[[2L]]
    )
  } else {
    edge_df$weight <- numeric(0)
  }
  names(edge_df)[1:2] <- c("variable_from", "variable_to")

  ## ---- Build tidy node data frame ------------------------------------------
  vnames  <- igraph::V(g)$name
  deg_vec <- if (isTRUE(directed)) {
    igraph::degree(g, mode = "out")[vnames]
  } else {
    igraph::degree(g)[vnames]
  }

  signal_set <- sdep$signal.vars %||% character(0)
  node_df <- data.frame(
    variable = factor(vnames, levels = vnames[order(-deg_vec)]),
    degree   = as.integer(deg_vec),
    selected = vnames %in% signal_set,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  ## ---- Apply min.degree node filtering (user-requested subsetting) ---------
  if (!is.null(min.degree)) {
    keep       <- node_df$degree >= min.degree
    keep_names <- as.character(node_df$variable)[keep]
    drop_names <- as.character(node_df$variable)[!keep]
    g          <- igraph::delete_vertices(g, drop_names)
    edge_df    <- edge_df[
      edge_df$variable_from %in% keep_names &
      edge_df$variable_to   %in% keep_names, , drop = FALSE]
    node_df    <- node_df[keep, , drop = FALSE]
    rownames(edge_df) <- NULL
    rownames(node_df) <- NULL
  }

  ## ---- Set igraph node attributes ------------------------------------------
  if (length(igraph::V(g)) > 0L) {
    igraph::V(g)$degree   <- node_df$degree[
      match(igraph::V(g)$name, as.character(node_df$variable))]
    igraph::V(g)$selected <- node_df$selected[
      match(igraph::V(g)$name, as.character(node_df$variable))]
  }

  ## ---- Assemble result ------------------------------------------------------
  result <- structure(
    list(edges = edge_df, nodes = node_df, graph = g),
    class = c("gg_udependent", "list")
  )
  attr(result, "provenance") <- .udep_provenance(object, threshold, q.signal,
                                                   directed, min.degree)
  result
}

## ---- Internal helpers -------------------------------------------------------

#' @keywords internal
.validate_udep_inputs <- function(object, threshold, directed) {
  if (missing(object) || is.null(object)) {
    stop("'object' must be a fitted uvarpro object.", call. = FALSE)
  }
  if (!inherits(object, "uvarpro")) {
    stop("'object' must be a uvarpro fit (class \"uvarpro\").", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      threshold <= 0 || threshold >= 1) {
    stop("'threshold' must be a single numeric value in (0, 1).", call. = FALSE)
  }
  if (!is.logical(directed) || length(directed) != 1L) {
    stop("'directed' must be a single logical value.", call. = FALSE)
  }
  invisible(NULL)
}

#' @keywords internal
.udep_provenance <- function(object, threshold, q.signal, directed, min.degree) {
  list(
    threshold  = threshold,
    q.signal   = q.signal,
    directed   = directed,
    min.degree = min.degree,
    xvar.names = object$xvar.names,
    n          = nrow(object$x)
  )
}

## ---- S3 companions ----------------------------------------------------------

#' @export
print.gg_udependent <- function(x, ...) {
  prov <- attr(x, "provenance")
  cat(sprintf(
    "<gg_udependent>  n=%d  p=%d  threshold=%.2f\n",
    prov$n, length(prov$xvar.names), prov$threshold
  ))
  cat(sprintf(
    "  Edges: %d  Nodes in graph: %d  Selected: %d/%d\n",
    nrow(x$edges), nrow(x$nodes),
    sum(x$nodes$selected, na.rm = TRUE), nrow(x$nodes)
  ))
  invisible(x)
}

#' @export
summary.gg_udependent <- function(object, ...) {
  prov <- attr(object, "provenance")
  s <- list(
    nodes      = object$nodes,
    edges      = object$edges,
    provenance = prov
  )
  class(s) <- "summary.gg_udependent"
  print(s)
  invisible(s)
}

#' @export
print.summary.gg_udependent <- function(x, ...) {
  prov <- x$provenance
  cat(sprintf(
    "Summary: gg_udependent  threshold=%.2f  q.signal=%.2f  directed=%s\n",
    prov$threshold, prov$q.signal, prov$directed
  ))
  cat("\nNodes:\n")
  print(x$nodes)
  cat("\nEdges:\n")
  print(x$edges)
  invisible(x)
}

#' @export
autoplot.gg_udependent <- function(object, ...) {
  plot.gg_udependent(object, ...)
}
```

- [ ] **Step 4: Run tests**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_gg_udependent.R')"
```
Expected: all pass (or skip where igraph not installed — but igraph is in Suggests so it should be present in dev environment).

- [ ] **Step 5: Commit**

```bash
git add R/gg_udependent.R tests/testthat/test_gg_udependent.R
git commit -m "feat(P3-T1): gg_udependent extractor + print/summary/autoplot (TDD)"
```

---

## Task 2: `plot.gg_udependent` (TDD)

**Files:**
- Modify: `tests/testthat/test_gg_udependent.R` (append plot tests)
- Create: `R/plot.gg_udependent.R`

### Step 1: Append plot tests to `tests/testthat/test_gg_udependent.R`

Add these tests at the end of the file:

```r
## ── Plot smoke tests ─────────────────────────────────────────────────────────

test_that("plot.gg_udependent default returns a ggplot", {
  skip_if_not_installed("ggraph")
  uv <- make_uvp()
  gg <- gg_udependent(uv)
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_udependent layout='kk' returns a ggplot", {
  skip_if_not_installed("ggraph")
  uv <- make_uvp()
  gg <- gg_udependent(uv)
  p  <- plot(gg, layout = "kk")
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_udependent empty graph -> stop with informative message", {
  uv <- make_uvp()
  gg <- suppressWarnings(gg_udependent(uv, threshold = 999))
  expect_error(plot(gg), regexp = "no edges")
})

test_that("plot.gg_udependent missing ggraph -> stop with informative message", {
  skip_if_not_installed("ggraph")
  uv <- make_uvp()
  gg <- gg_udependent(uv)
  # Mock requireNamespace to return FALSE
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "ggraph") FALSE else
      base::requireNamespace(pkg, ...),
    .package = "base"
  )
  expect_error(plot(gg), regexp = "ggraph")
})
```

- [ ] **Step 2: Run new tests to verify they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_gg_udependent.R')"
```
Expected: the new plot tests fail with "could not find function 'plot.gg_udependent'"; existing tests still pass.

- [ ] **Step 3: Create `R/plot.gg_udependent.R`**

```r
##=============================================================================
#' Plot a \code{gg_udependent} variable dependency graph
#'
#' Renders the variable dependency graph from a \code{gg_udependent} object
#' as a ggraph network plot. Nodes are coloured by selection status; edge
#' width and opacity reflect dependency strength.
#'
#' @param x A \code{gg_udependent} object from \code{\link{gg_udependent}}.
#' @param layout Character; igraph/ggraph layout algorithm.  Common choices:
#'   \code{"fr"} (Fruchterman-Reingold, default), \code{"kk"}
#'   (Kamada-Kawai), \code{"stress"}, \code{"circle"}, \code{"grid"}.
#' @param ... Not currently used.
#'
#' @return A \code{ggplot} object (built via ggraph).
#'
#' @seealso \code{\link{gg_udependent}}
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' uv <- varPro::uvarpro(iris[, -5], ntree = 50)
#' plot(gg_udependent(uv))
#' }
#'
#' @name plot.gg_udependent
#' @importFrom ggplot2 aes labs scale_color_manual theme_void
#' @export
plot.gg_udependent <- function(x, layout = "fr", ...) {
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Install the 'ggraph' package to use plot.gg_udependent(): ",
         "install.packages('ggraph')", call. = FALSE)
  }

  prov <- attr(x, "provenance")

  if (is.null(x$graph) || nrow(x$edges) == 0L) {
    stop("gg_udependent: no edges to plot. ",
         "Lower threshold (currently ", prov$threshold, ") to detect dependencies.",
         call. = FALSE)
  }

  ## Ensure selected attribute is on the graph (guard against old objects)
  if (is.null(igraph::vertex_attr(x$graph, "selected"))) {
    sel <- x$nodes$selected[match(igraph::V(x$graph)$name,
                                   as.character(x$nodes$variable))]
    igraph::V(x$graph)$selected <- sel
  }
  if (is.null(igraph::vertex_attr(x$graph, "degree"))) {
    deg <- x$nodes$degree[match(igraph::V(x$graph)$name,
                                 as.character(x$nodes$variable))]
    igraph::V(x$graph)$degree <- deg
  }

  ggraph::ggraph(x$graph, layout = layout) +
    ggraph::geom_edge_link(
      ggplot2::aes(width = .data[["weight"]],
                   alpha = .data[["weight"]]),
      color = "#4e8fcd"
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(color = factor(.data[["selected"]]),
                   size  = .data[["degree"]])
    ) +
    ggraph::geom_node_label(
      ggplot2::aes(label = .data[["name"]]),
      repel = TRUE, size = 3, show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "#4e8fcd", "FALSE" = "#888888"),
      guide  = "none"
    ) +
    ggraph::scale_edge_width(range = c(0.5, 2.5), guide = "none") +
    ggraph::scale_edge_alpha(range = c(0.3, 1.0),  guide = "none") +
    ggplot2::labs(
      size    = "Degree",
      caption = paste0("Dependency threshold: ", prov$threshold,
                       ". Layout: ", layout, ".")
    ) +
    ggplot2::theme_void()
}
```

- [ ] **Step 4: Run all tests**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_gg_udependent.R')"
```
Expected: all tests pass (plot tests skip gracefully if ggraph not installed, but pass if ggraph is installed).

Note: the `local_mocked_bindings` test for the missing-ggraph case may be tricky; if it fails due to mocking limitations, replace with:
```r
test_that("plot.gg_udependent empty graph -> stop with informative message", {
  uv <- make_uvp()
  gg <- suppressWarnings(gg_udependent(uv, threshold = 999))
  expect_error(plot(gg), regexp = "no edges")
})
```
and skip the requireNamespace mock test.

- [ ] **Step 5: Run devtools::document()**

```bash
Rscript -e "devtools::document()"
```
Expected: `man/gg_udependent.Rd` and `man/plot.gg_udependent.Rd` created; NAMESPACE updated.

- [ ] **Step 6: Commit**

```bash
git add R/plot.gg_udependent.R tests/testthat/test_gg_udependent.R man/
git commit -m "feat(P3-T2): plot.gg_udependent ggraph network renderer (TDD)"
```

---

## Task 3: vdiffr snapshots

**Files:**
- Modify: `tests/testthat/test_gg_udependent.R` (append snapshot tests)

- [ ] **Step 1: Append vdiffr snapshot tests**

Add at end of `tests/testthat/test_gg_udependent.R`:

```r
## ── vdiffr snapshots ─────────────────────────────────────────────────────────

test_that("plot.gg_udependent snapshot: default fr layout", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggraph")
  uv <- make_uvp(ntree = 50L)
  gg <- gg_udependent(uv)
  vdiffr::expect_doppelganger("gg-udependent-default", plot(gg))
})

test_that("plot.gg_udependent snapshot: undirected", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggraph")
  uv <- make_uvp(ntree = 50L)
  gg <- gg_udependent(uv, directed = FALSE)
  vdiffr::expect_doppelganger("gg-udependent-undirected", plot(gg))
})
```

- [ ] **Step 2: Record baseline snapshots**

```bash
NOT_CRAN=true VDIFFR_RUN_TESTS=true \
  Rscript -e "testthat::test_file('tests/testthat/test_gg_udependent.R')"
```
Expected: snapshots written to `tests/testthat/_snaps/`.

Alternatively, using testthat snapshot management:
```bash
Rscript -e "testthat::snapshot_accept()"
```

- [ ] **Step 3: Commit snapshots**

```bash
git add tests/testthat/_snaps/ tests/testthat/test_gg_udependent.R
git commit -m "test(P3-T3): vdiffr baseline snapshots for plot.gg_udependent"
```

---

## Task 4: pkgdown + NEWS + final gate + PR

**Files:**
- Modify: `NEWS.md`
- Run: `devtools::document()`, `devtools::check()`

- [ ] **Step 1: Update NEWS.md**

Add after the `gg_varpro` bullet (line 6 area), insert:

```markdown
* **varPro variable dependency: `gg_udependent()` (#86).**
  - `gg_udependent()` extracts cross-variable dependency scores from a
    `uvarpro` fit (unsupervised VarPro) using `varPro::get.beta.entropy()`
    + `varPro::sdependent()`, and returns a tidy list with `$edges`
    (variable_from, variable_to, weight), `$nodes` (variable, degree,
    selected), and `$graph` (igraph object).
  - `plot.gg_udependent()` renders the dependency network using ggraph
    with edge width/opacity scaled by dependency strength and node
    colour by signal-variable status. Layout is configurable
    (`"fr"`, `"kk"`, `"stress"`, etc.).
  - `ggraph` added to `Suggests:`.
```

- [ ] **Step 2: Run devtools::document()**

```bash
Rscript -e "devtools::document()"
```
Expected: all Rd files up to date, NAMESPACE clean.

- [ ] **Step 3: Run full package check**

```bash
Rscript -e "devtools::check(args='--as-cran')"
```
Expected: 0 errors, 0 warnings. Notes about `:::` calls or CRAN-only policies are acceptable.

Fix any errors before proceeding.

- [ ] **Step 4: Run full test suite**

```bash
Rscript -e "devtools::test()"
```
Expected: all tests pass (snapshot tests skip on CRAN, pass locally with `NOT_CRAN=true`).

- [ ] **Step 5: Push branch**

```bash
git push -u origin feat/varpro-phase3-gg-udependent
```

- [ ] **Step 6: Open PR**

```bash
gh pr create \
  --title "varPro Phase 3: gg_udependent — dependency graph for uvarpro fits (#86)" \
  --body "$(cat <<'EOF'
## Summary

- Adds `gg_udependent()`: tidy extractor wrapping `varPro::get.beta.entropy()` + `varPro::sdependent()` on a `uvarpro` fit. Returns `$edges`, `$nodes`, `$graph` (igraph), and a provenance attribute.
- Adds `plot.gg_udependent()`: ggraph network renderer with edge width/opacity by dependency strength, node colour by signal status, configurable layout.
- Adds `print`, `summary`, `autoplot` S3 companions.
- Adds `ggraph` to `Suggests:`.
- Bumps version to `2.7.3.9004`.

## Test plan

- [ ] All `test_gg_udependent.R` tests pass
- [ ] vdiffr snapshots pass locally (`NOT_CRAN=true VDIFFR_RUN_TESTS=true`)
- [ ] `devtools::check(args='--as-cran')` returns 0 errors, 0 warnings
- [ ] CI passes on all platforms

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

---

## Appendix: Reference code for edge-case handling

### Empty graph from sdependent

`sdependent()` returns a character string (not a list) when all nodes become isolated after threshold filtering:

```r
# Return value when threshold is too high:
# [1] "graph is null after removing isolated nodes (degree zero) - increase threshold"
```

Guard in `gg_udependent()`:
```r
if (is.character(sdep)) {
  warning("gg_udependent: ", sdep, call. = FALSE)
  # ... return empty structure ...
}
```

### get.beta.entropy may drop variables

`varPro::get.beta.entropy()` returns a matrix smaller than `p` when some variables have zero cross-importance. The matrix dimensions match the non-zero variables only. This is expected — `sdependent` and the tidy frames will only contain those variables.

### directed degree computation

For directed graphs, `sdependent` uses out-degree for `signal.vars` classification:
```r
# From sdependent source:
node.degrees <- igraph::degree(g, mode = "out")
```
Our `$nodes$degree` must match this for `selected` to be consistent:
```r
deg_vec <- if (isTRUE(directed)) igraph::degree(g, mode = "out")[vnames]
           else igraph::degree(g)[vnames]
```
