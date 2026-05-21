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
#' @details
#' Requires the \pkg{ggraph} package (\code{Suggests}).  Install it with
#' \code{install.packages("ggraph")}.
#'
#' Node colour: blue (\code{#4e8fcd}) for signal variables
#' (\code{selected = TRUE}), grey (\code{#888888}) otherwise.
#' Node size scales with degree.  Edge width and opacity scale with raw
#' dependency weight (\code{I[i,j]}).
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
  prov <- attr(x, "provenance")

  if (is.null(x$graph) || nrow(x$edges) == 0L) {
    stop("gg_udependent: no edges to plot. ",
         "Lower threshold (currently ", prov$threshold,
         ") to detect dependencies.",
         call. = FALSE)
  }

  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Install the 'ggraph' package to use plot.gg_udependent(): ",
         "install.packages('ggraph')", call. = FALSE)
  }

  ## Ensure vertex attributes are present (guard against old objects) --------
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

  ## Ensure edge weight attribute is set on the igraph object ----------------
  if (is.null(igraph::edge_attr(x$graph, "weight"))) {
    edge_list <- igraph::as_data_frame(x$graph, what = "edges")
    w <- mapply(function(i, j) x$edges$weight[
      x$edges$variable_from == i & x$edges$variable_to == j],
      edge_list$from, edge_list$to)
    igraph::E(x$graph)$weight <- as.numeric(w)
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
      size = 3, show.legend = FALSE
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
