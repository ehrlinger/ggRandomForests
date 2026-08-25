##=============================================================================
#' Plot a Random Hazard Forest tuning path
#'
#' Draws the saved evaluated metric at each tree size, highlighting the
#' upstream selected size. OOB risk paths show the criterion minimized by
#' upstream tuning; OOB iAUC paths show the criterion it maximizes. An iAUC
#' path includes a standard-error band only when finite supplied standard
#' errors are available.
#'
#' @param x A `gg_tune_rhf` object from [gg_tune_rhf()].
#' @param se_band Logical; draw an iAUC standard-error band when available.
#' @param se_mult Positive finite multiplier for the standard-error band.
#' @param ... Additional arguments passed to the evaluated-point layer.
#'
#' @return A `ggplot` object.
#'
#' @seealso [gg_tune_rhf()].
#'
#' @importFrom ggplot2 aes geom_line geom_point geom_ribbon ggplot labs theme_bw
#' @name plot.gg_tune_rhf
#' @export
plot.gg_tune_rhf <- function(x, se_band = TRUE, se_mult = 1, ...) {
  .validate_plot_gg_tune_rhf_args(x, se_band, se_mult)

  point_args <- .default_gg_tune_rhf_point_args(list(...))

  p <- ggplot2::ggplot(x, ggplot2::aes(
    x = .data[["treesize"]], y = .data[["value"]]
  ))

  ribbon <- .gg_tune_rhf_ribbon(x, se_band, se_mult)
  if (!is.null(ribbon)) {
    p <- p + ggplot2::geom_ribbon(
      data = ribbon,
      ggplot2::aes(
        ymin = .data[["ymin"]], ymax = .data[["ymax"]]
      ),
      inherit.aes = TRUE,
      fill = "steelblue", alpha = 0.18
    )
  }

  p +
    ggplot2::geom_line(color = "grey45", linewidth = 0.6) +
    do.call(ggplot2::geom_point, point_args) +
    ggplot2::geom_point(
      data = x[x$selected, , drop = FALSE],
      shape = 21, size = 3.5, stroke = 0.8,
      color = "black", fill = "steelblue"
    ) +
    ggplot2::labs(x = "Tree size", y = x$metric[1L]) +
    ggplot2::theme_bw()
}

.validate_plot_gg_tune_rhf_args <- function(x, se_band, se_mult) {
  if (!inherits(x, "gg_tune_rhf")) {
    stop("plot.gg_tune_rhf() requires a 'gg_tune_rhf' object.",
         call. = FALSE)
  }
  if (!is.logical(se_band) || length(se_band) != 1L || is.na(se_band)) {
    stop("'se_band' must be one non-missing logical value.", call. = FALSE)
  }
  if (!is.numeric(se_mult) || length(se_mult) != 1L ||
      !is.finite(se_mult) || se_mult <= 0) {
    stop("'se_mult' must be one finite positive number.", call. = FALSE)
  }
  invisible(NULL)
}

.default_gg_tune_rhf_point_args <- function(point_args) {
  if (!"size" %in% names(point_args)) point_args$size <- 2
  if (!"color" %in% names(point_args)) point_args$color <- "grey25"
  point_args
}

.gg_tune_rhf_ribbon <- function(x, se_band, se_mult) {
  has_band <- isTRUE(se_band) && identical(x$metric[1L], "OOB iAUC") &&
    any(is.finite(x$se))
  if (!has_band) return(NULL)
  ribbon <- x[is.finite(x$se), , drop = FALSE]
  ribbon$ymin <- pmax(0, ribbon$value - se_mult * ribbon$se)
  ribbon$ymax <- pmin(1, ribbon$value + se_mult * ribbon$se)
  ribbon
}
