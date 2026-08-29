##=============================================================================
#' Plot a `gg_sdependent` object
#'
#' Ranked lollipop of the per-variable `sdependent()` signal score, sorted
#' descending so the strongest signal lands at the top. Points are colored
#' blue for variables flagged as signal (`signal == TRUE`) and gray otherwise.
#'
#' @section Reading the chart:
#' Each lollipop is a variable's signal score from [varPro::sdependent()],
#' computed on the unsupervised entropy-region lasso structure of a
#' [varPro::uvarpro()] fit. Blue variables cleared the detection threshold and
#' are reported in `sdependent()$signal.vars`; gray ones did not. Pair with
#' [gg_udependent()] (the dependency graph) to see *how* the signal variables
#' connect, and with [gg_beta_uvarpro()] for the lasso-importance ranking.
#'
#' @param x A `gg_sdependent` object from [gg_sdependent()].
#' @param labels Optional variable labels for the variable axis. One of: a
#'   named character vector (`c(bpd = "BP Diastole")`); a labelled data frame,
#'   whose `attr(col, "label")` values are read; or a two-column `key`/`label`
#'   data frame. Variables with no label keep their raw name. Defaults to
#'   `NULL` (raw names).
#' @param ... Not currently used.
#'
#' @return A `ggplot` object.
#'
#' @seealso [gg_sdependent()], [gg_udependent()], [gg_beta_uvarpro()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   o <- varPro::uvarpro(mtcars, ntree = 50)
#'   plot(gg_sdependent(o))
#' }
#' }
#'
#' @name plot.gg_sdependent
#' @importFrom ggplot2 ggplot aes geom_segment geom_point coord_flip
#' @importFrom ggplot2 scale_color_manual labs theme_minimal
#' @export
plot.gg_sdependent <- function(x, labels = NULL, ...) {
  if (nrow(x) == 0L) {
    stop("plot.gg_sdependent: nothing to plot (gg_sdependent has 0 rows).",
         call. = FALSE)
  }
  prov     <- attr(x, "provenance")
  thr      <- if (!is.null(prov)) prov$threshold %||% NA_real_ else NA_real_
  n_signal <- if (!is.null(prov)) prov$n_signal %||% sum(x$signal) else sum(x$signal)

  caption_txt <- sprintf(
    "varPro::sdependent() signal score. %d of %d flagged as signal (threshold %.3g).",
    n_signal, nrow(x), thr
  )

  gg_plt <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      x     = .data[["variable"]],
      y     = .data[["imp_score"]],
      color = factor(.data[["signal"]])
    )
  ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = .data[["variable"]],
        yend = 0
      )
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "#4e8fcd", "FALSE" = "#888888"),
      guide  = "none"
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Signal score (sdependent)",
      caption = caption_txt
    ) +
    ggplot2::theme_minimal()

  # coord_flip(), so the variable axis is a flipped x, as in plot.gg_vimp().
  lab_lookup <- .forest_labels(labels)
  if (!is.null(lab_lookup)) {
    gg_plt <- gg_plt + ggplot2::scale_x_discrete(
      labels = function(v) .apply_forest_labels(v, lab_lookup)
    )
  }
  gg_plt
}
