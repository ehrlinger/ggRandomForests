##=============================================================================
#' Variable dependency graph from a uvarpro model
#'
#' A \code{uvarpro} fit records how strongly each variable depends on the
#' others.  This function pulls those cross-variable dependency scores from
#' the fit with \code{\link[varPro]{get.beta.entropy}} and
#' \code{\link[varPro]{sdependent}}, and returns them as a tidy list that
#' \code{plot.gg_udependent} can draw as a network.
#'
#' @section What cross-variable dependency is doing:
#' UVarPro (Zhou, Lu and Ishwaran, 2026) extends the varpro framework to
#' the unsupervised setting: grow a forest without a response, then use
#' the same region-release contrasts varpro uses for supervised
#' importance to ask, "which variables explain the structure in the
#' data?" The lasso-driven variant frames each region-release contrast
#' as a classification task (does an observation belong to the region
#' or to its release?) and fits a lasso logistic regression with the
#' other variables as predictors. The coefficient on variable \eqn{j}
#' in the model for variable \eqn{i}'s region-release contrast is the
#' entry \eqn{I[i, j]} of the matrix \code{varPro::get.beta.entropy()}
#' returns.
#'
#' Read that entry as "how much does knowing \eqn{j} help separate
#' \eqn{i}'s region from its release". A large \eqn{I[i, j]} says
#' \eqn{j} carries information about the structure varpro picked up in
#' \eqn{i}. \code{varPro::sdependent} thresholds that matrix at a
#' user-chosen cut and returns the set of "signal" variables: the
#' nodes with high enough out-degree to be worth keeping. We pass the
#' threshold through to \code{sdependent} and use the same matrix to
#' weight the edges of the resulting graph.
#'
#' The graph is directed by default because \eqn{I[i, j]} and
#' \eqn{I[j, i]} are separate lasso coefficients and need not agree;
#' setting \code{directed = FALSE} collapses each pair by taking the
#' larger of the two, which is appropriate when you only want to see
#' that two variables are dependent, not which way the dependency
#' reads.
#'
#' @section What's in the output:
#' \code{$edges} has one row per surviving edge with the raw weight
#' \code{I[i, j]} (or, for undirected graphs, the max of the two
#' directions). \code{$nodes} has one row per surviving variable with
#' its degree (out-degree for directed, total degree for undirected)
#' and a \code{selected} flag for membership in the \code{sdependent}
#' signal set. \code{$graph} is the same information packaged as an
#' \code{igraph} object, with \code{weight}, \code{degree}, and
#' \code{selected} attached so \code{plot.gg_udependent} can render it
#' without recomputing anything.
#'
#' @section What you use this for:
#' \itemize{
#'   \item screen a wide unsupervised dataset for the small set of
#'     variables UVarPro thinks are carrying the signal: the nodes
#'     with high degree, or those flagged \code{selected = TRUE};
#'   \item spot clusters of mutually dependent variables (hubs and the
#'     spokes around them) that may be measuring the same underlying
#'     construct;
#'   \item compare two datasets, or two preprocessing pipelines, by
#'     looking at how their dependency graphs change.
#' }
#' An edge in this graph is a statistical dependency in the unsupervised
#' decomposition of the data. It is not a causal arrow. A high
#' \eqn{I[i, j]} says \eqn{j} predicts \eqn{i}'s region membership,
#' not that \eqn{j} causes \eqn{i}.
#'
#' @references
#' Zhou, L., Lu, M. and Ishwaran, H. (2026). Variable priority for
#' unsupervised variable selection. \emph{Pattern Recognition},
#' 172:112727.
#'
#' @param object A fitted \code{uvarpro} object (required).
#' @param threshold Numeric; the positive dependency threshold passed on to
#'   \code{sdependent()}.  An edge \eqn{i \to j} is drawn when
#'   \code{I[i, j] >= threshold}.  Default \code{0.25}.
#' @param q.signal Quantile threshold (0--1) for picking out the signal
#'   variables; passed on to \code{sdependent()}.  Default \code{0.75}.
#' @param directed Logical; \code{TRUE} (default) builds a directed igraph.
#' @param min.degree Integer or \code{NULL}.  When set, only nodes with
#'   degree \eqn{\ge} \code{min.degree} are kept in \code{$nodes},
#'   \code{$edges}, and \code{$graph}.
#' @param ... Additional arguments forwarded to \code{varPro::sdependent()}.
#'
#' @return A named list of class \code{"gg_udependent"} with elements:
#' \describe{
#'   \item{\code{$edges}}{Data frame: \code{variable_from}, \code{variable_to},
#'     \code{weight} (raw cross-importance value).}
#'   \item{\code{$nodes}}{Data frame: \code{variable} (factor, levels by
#'     descending degree), \code{degree} (integer; out-degree when
#'     \code{directed = TRUE}, total degree when \code{directed = FALSE}),
#'     \code{selected} (logical, \code{TRUE} if in \code{sdependent}'s
#'     signal set).}
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
#' @importFrom igraph graph_from_adjacency_matrix degree delete_vertices as_data_frame V
#' @export
gg_udependent <- function(object,
                           threshold  = 0.25,
                           q.signal   = 0.75,
                           directed   = TRUE,
                           min.degree = NULL,
                           ...) {
  .validate_udep_inputs(object, threshold, directed)

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
  adj_mat <- (imp_mat >= threshold) * 1
  diag(adj_mat) <- 0
  if (sum(adj_mat) == 0L) {
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
  ## For undirected, symmetrise first so edge existence = max(I[i,j], I[j,i])
  ## and mode = "undirected" is valid (igraph >= 1.6.0 requires symmetry).
  if (!isTRUE(directed)) {
    adj_mat <- pmax(adj_mat, t(adj_mat))
  }
  g <- igraph::graph_from_adjacency_matrix(
    adj_mat,
    mode  = if (isTRUE(directed)) "directed" else "undirected",
    diag  = FALSE
  )
  isolated <- igraph::degree(g, mode = "all") == 0
  g <- igraph::delete_vertices(g, which(isolated))

  ## ---- Build tidy edge data frame with raw weights -------------------------
  edge_df <- igraph::as_data_frame(g, what = "edges")
  if (nrow(edge_df) > 0L) {
    if (isTRUE(directed)) {
      edge_df$weight <- mapply(function(i, j) imp_mat[i, j],
                                edge_df[[1L]], edge_df[[2L]])
    } else {
      ## Undirected: weight = max of both directions
      edge_df$weight <- mapply(
        function(i, j) max(imp_mat[i, j], imp_mat[j, i]),
        edge_df[[1L]], edge_df[[2L]])
    }
  } else {
    edge_df$weight <- numeric(0)
  }
  names(edge_df)[1:2] <- c("variable_from", "variable_to")

  ## ---- Build tidy node data frame ------------------------------------------
  vnames  <- igraph::V(g)$name
  ## degree: out-degree for directed (matches sdependent's signal.vars logic),
  ## total degree for undirected
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

  ## ---- Set igraph edge weights (order-insensitive for undirected) -----------
  if (length(igraph::E(g)) > 0L && nrow(edge_df) > 0L) {
    el <- igraph::as_data_frame(g, what = "edges")
    if (isTRUE(directed)) {
      idx <- match(paste(el$from, el$to),
                   paste(edge_df$variable_from, edge_df$variable_to))
    } else {
      key_g <- paste(pmin(el$from, el$to),     pmax(el$from, el$to))
      key_e <- paste(pmin(edge_df$variable_from, edge_df$variable_to),
                     pmax(edge_df$variable_from, edge_df$variable_to))
      idx <- match(key_g, key_e)
    }
    igraph::E(g)$weight <- edge_df$weight[idx]
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
