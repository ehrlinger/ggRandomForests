##=============================================================================
#' Plot a `gg_beta_uvarpro` object
#'
#' Horizontal bar chart of the mean absolute lasso coefficient
#' \eqn{\mathrm{mean}(|\hat{\beta}|)}{mean(|beta hat|)} per variable from an
#' unsupervised varPro fit, sorted descending so the eye lands on the top
#' variable first. Bars are filled blue above the selection cutoff, gray
#' otherwise, with a dashed red line at the cutoff.
#'
#' @section Reading the chart:
#' Each bar is the average magnitude of a per-region lasso coefficient for
#' that variable, computed by [varPro::get.beta.entropy()] over the
#' unsupervised entropy regions of a [varPro::uvarpro()] fit. There is no
#' response: the score measures how strongly a variable is reconstructed by
#' the others within released regions, i.e. an unsupervised
#' importance / redundancy signal rather than a predictive one. As with
#' [gg_beta_varpro()], the numeric scale carries the predictors' units, so
#' bar lengths are comparable within a data set but not blindly across
#' variables on very different scales.
#'
#' @param x A `gg_beta_uvarpro` object from [gg_beta_uvarpro()].
#' @param ... Not currently used.
#'
#' @return A `ggplot` object.
#'
#' @seealso [gg_beta_uvarpro()], [gg_beta_varpro()], [gg_udependent()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   o <- varPro::uvarpro(mtcars, ntree = 50)
#'   plot(gg_beta_uvarpro(o))
#' }
#' }
#'
#' @name plot.gg_beta_uvarpro
#' @importFrom ggplot2 ggplot aes geom_col geom_hline coord_flip
#' @importFrom ggplot2 scale_fill_manual labs theme_minimal
#' @export
plot.gg_beta_uvarpro <- function(x, ...) {
  if (nrow(x) == 0L) {
    stop("plot.gg_beta_uvarpro: nothing to plot (gg_beta_uvarpro has 0 rows).",
         call. = FALSE)
  }
  prov   <- attr(x, "provenance")
  cutoff <- if (!is.null(prov) && !is.null(prov$cutoff)) {
    unname(prov$cutoff[[1]])
  } else {
    mean(x$beta_mean)
  }
  n_var     <- if (!is.null(prov)) prov$n_var %||% nrow(x) else nrow(x)
  n_regions <- if (!is.null(prov)) prov$n_released_regions %||% NA_integer_ else NA_integer_

  caption_txt <- sprintf(
    "Mean |lasso beta| over %s released region(s), %s variable(s). Cutoff: %.4g.",
    if (is.na(n_regions)) "NA" else format(n_regions),
    format(n_var), cutoff
  )

  ggplot2::ggplot(
    x,
    ggplot2::aes(
      x    = .data[["variable"]],
      y    = .data[["beta_mean"]],
      fill = factor(.data[["selected"]])
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#4e8fcd", "FALSE" = "#888888"),
      guide  = "none"
    ) +
    ggplot2::geom_hline(
      yintercept = cutoff,
      linetype   = "dashed",
      color      = "#e74c3c",
      linewidth  = 0.7
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Mean |lasso beta| (unsupervised, per-region)",
      caption = caption_txt
    ) +
    ggplot2::theme_minimal()
}
