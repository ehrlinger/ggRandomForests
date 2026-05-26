##=============================================================================
#' Plot a \code{gg_udependent} variable dependency graph
#'
#' Draws the dependency graph held in a \code{gg_udependent} object as a
#' ggraph network.  Node colour marks whether a variable made the signal
#' set, and the width and opacity of an edge tell you how strong the
#' dependency between its two variables is.
#'
#' @section Reading the network:
#' Each node is a variable; each edge is a cross-variable dependency
#' that cleared the threshold passed to \code{gg_udependent}. The
#' Fruchterman-Reingold layout (the default) places mutually connected
#' variables near each other, so the picture tends to show hubs and
#' the clusters around them rather than a tidy ring. The eye usually
#' goes first to the largest blue node — a variable that is both in
#' the signal set and connects to many others is a hub of the
#' dependency structure. Edges with wider, more opaque strokes are
#' stronger dependencies; thin, faint edges sit near the threshold and
#' are the ones that would disappear first if you raised it.
#'
#' Isolated, grey, low-degree nodes are the ones UVarPro thinks are
#' not contributing much to the structure. A cluster of mutually
#' connected variables is worth checking for redundancy — they may be
#' several views of the same underlying quantity.
#'
#' @section What this tells you:
#' Use the figure to pick a working set of variables: the hubs and
#' their immediate neighbours are the candidates UVarPro flags as
#' carrying structure. If a cluster of high-degree variables looks
#' like it might be measuring the same thing, that is a cue to look at
#' their pairwise correlations or fit them as a block rather than
#' individually. The threshold and layout are recorded in the caption
#' so a different choice is easy to spot in a later figure.
#'
#' @param x A \code{gg_udependent} object from \code{\link{gg_udependent}}.
#' @param layout Character; the igraph/ggraph layout algorithm.  Common
#'   choices are \code{"fr"} (Fruchterman-Reingold, the default),
#'   \code{"kk"} (Kamada-Kawai), \code{"stress"}, \code{"circle"}, and
#'   \code{"grid"}.
#' @param ... Not currently used.
#'
#' @details
#' This plot needs the \pkg{ggraph} package, which is in \code{Suggests}
#' rather than installed for you.  If it is missing, run
#' \code{install.packages("ggraph")}.
#'
#' A signal variable (\code{selected = TRUE}) gets a blue node
#' (\code{#4e8fcd}); the rest are grey (\code{#888888}).  Node size grows
#' with degree.  Edge width and opacity both grow with the raw dependency
#' weight \code{I[i,j]}.
#'
#' @return A \code{ggplot} object (built via ggraph).
#'
#' @seealso \code{\link{gg_udependent}}
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggraph", quietly = TRUE)) {
#'   set.seed(42)
#'   uv <- varPro::uvarpro(iris[, -5], ntree = 50)
#'   plot(gg_udependent(uv))
#' }
#' }
#'
#' @name plot.gg_udependent
#' @importFrom ggplot2 aes labs scale_color_manual theme_void
#' @importFrom igraph vertex_attr V edge_attr as_data_frame E
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

  ## Edge-weight backfill: guard for legacy objects saved before edge weights
  ## were stored on the igraph.  Order-insensitive for undirected graphs.
  if (is.null(igraph::edge_attr(x$graph, "weight"))) {
    prov_d    <- isTRUE(attr(x, "provenance")$directed)
    edge_list <- igraph::as_data_frame(x$graph, what = "edges")
    if (prov_d) {
      idx <- match(paste(edge_list$from, edge_list$to),
                   paste(x$edges$variable_from, x$edges$variable_to))
    } else {
      key_g <- paste(pmin(edge_list$from, edge_list$to),
                     pmax(edge_list$from, edge_list$to))
      key_e <- paste(pmin(x$edges$variable_from, x$edges$variable_to),
                     pmax(x$edges$variable_from, x$edges$variable_to))
      idx <- match(key_g, key_e)
    }
    igraph::E(x$graph)$weight <- x$edges$weight[idx]
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
