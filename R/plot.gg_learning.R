##=============================================================================
#' Plot a random forest learning curve
#'
#' Draws one panel of a [gg_learning()] curve against the number of events
#' (observations, for regression). The default `"stability"` panel shows the
#' top-k Jaccard index and Kendall rank correlation of the VIMP ranking across
#' replicates; the `"error"` panel shows each replicate's OOB error with the
#' mean at each fraction. Points flagged `low_events` or `optimistic` are drawn
#' open, because both make the curve look worse or better than a fresh cohort
#' of that size would. Unstratified refits, whose counts are unknown, are
#' drawn as crosses and the axis says their counts are expected.
#'
#' @param x A `gg_learning` object from [gg_learning()].
#' @param panel Which curve to draw: `"stability"` (default) or `"error"`.
#' @param ... Not used.
#'
#' @return A `ggplot` object.
#'
#' @seealso [gg_learning()].
#'
#' @importFrom ggplot2 aes geom_line geom_point ggplot guide_legend guides labs
#'   scale_shape_manual theme_bw
#' @name plot.gg_learning
#' @export
plot.gg_learning <- function(x, panel = c("stability", "error"), ...) {
  if (!inherits(x, "gg_learning")) {
    stop("plot.gg_learning() requires a 'gg_learning' object.",
         call. = FALSE)
  }
  panel <- match.arg(panel)
  prov <- attr(x, "provenance")
  xlab <- switch(prov$family,
    regr = "Observations",
    class = "Cases in the smallest class",
    surv = "Events"
  )
  shapes <- c(ok = 16, `few events` = 1, `optimistic` = 2,
              `count unknown` = 4)
  if (any(x$counts == "expected" & x$fraction < 1)) {
    xlab <- paste(xlab, "(expected; refits not stratified)")
  }

  if (panel == "stability") {
    st <- attr(x, "stability")
    st$flag <- .learning_flag_label(st)
    long <- rbind(
      data.frame(st[c("events", "flag")], measure = "Top-k Jaccard",
                 value = st$jaccard),
      data.frame(st[c("events", "flag")], measure = "Kendall tau",
                 value = st$kendall)
    )
    long$measure <- factor(long$measure,
                           levels = c("Top-k Jaccard", "Kendall tau"))
    return(
      ggplot2::ggplot(long, ggplot2::aes(
        x = .data[["events"]], y = .data[["value"]],
        colour = .data[["measure"]]
      )) +
        ggplot2::geom_line(na.rm = TRUE) +
        ggplot2::geom_point(ggplot2::aes(shape = .data[["flag"]]),
                            size = 2.5, na.rm = TRUE) +
        ggplot2::scale_shape_manual(values = shapes, drop = TRUE) +
        # Both legends are untitled, so ggplot2 has no stable order for them
        # and swaps them between sessions. Pin it.
        ggplot2::guides(
          colour = ggplot2::guide_legend(order = 1),
          shape = ggplot2::guide_legend(order = 2)
        ) +
        ggplot2::labs(
          x = xlab,
          y = sprintf("Agreement of top-%d VIMP rankings", prov$top_k),
          colour = NULL, shape = NULL
        ) +
        ggplot2::theme_bw()
    )
  }

  pts <- as.data.frame(x)
  pts$flag <- .learning_flag_label(pts)
  means <- stats::aggregate(error ~ events, data = pts, FUN = mean)
  ggplot2::ggplot(pts, ggplot2::aes(
    x = .data[["events"]], y = .data[["error"]]
  )) +
    ggplot2::geom_point(ggplot2::aes(shape = .data[["flag"]]),
                        colour = "grey55", size = 1.8) +
    ggplot2::geom_line(data = means, colour = "black") +
    ggplot2::geom_point(data = means, colour = "black", size = 2.5) +
    ggplot2::scale_shape_manual(values = shapes, drop = TRUE) +
    ggplot2::labs(x = xlab, y = prov$metric, shape = NULL) +
    ggplot2::theme_bw()
}

.learning_flag_label <- function(d) {
  flag <- ifelse(is.na(d$low_events), "count unknown",
                 ifelse(d$low_events, "few events",
                        ifelse(d$optimistic, "optimistic", "ok")))
  factor(flag, levels = c("ok", "few events", "optimistic", "count unknown"))
}
