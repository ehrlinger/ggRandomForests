##=============================================================================
#' Variable dependency graph from a uvarpro model
#'
#' Extracts cross-variable dependency scores from a fitted \code{uvarpro}
#' object using \code{\link[varPro]{get.beta.entropy}} and
#' \code{\link[varPro]{sdependent}}, and returns a tidy list suitable for
#' \code{plot.gg_udependent}.
#'
#' @param object A fitted \code{uvarpro} object (required).
#' @param threshold Numeric; positive dependency threshold passed to
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

  ## ---- Helper: build and return an empty gg_udependent result ---------------
  .empty_result <- function(msg) {
    warning("gg_udependent: ", msg,
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
    result
  }

  ## ---- Build adjacency from threshold; short-circuit if empty --------------
  A <- (imp_mat >= threshold) * 1
  diag(A) <- 0
  if (sum(A) == 0L) {
    return(.empty_result(
      paste0("no edges found at threshold=", threshold)
    ))
  }

  ## ---- Call sdependent for signal detection --------------------------------
  sdep <- varPro::sdependent(imp_mat, threshold = threshold,
                              q.signal = q.signal, directed = directed,
                              min.degree = min.degree, plot = FALSE, ...)

  ## ---- Handle empty graph (sdependent may also return character) -----------
  if (is.character(sdep)) {
    return(.empty_result(sdep))
  }

  ## ---- Build igraph from adjacency -----------------------------------------
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

  signal_set <- if (is.null(sdep$signal.vars)) character(0) else sdep$signal.vars
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
  if (!is.numeric(threshold) || length(threshold) != 1L || threshold <= 0) {
    stop("'threshold' must be a single positive numeric value.", call. = FALSE)
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
