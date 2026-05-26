##=============================================================================
#' Plot a \code{gg_beta_varpro} object
#'
#' Horizontal bar chart of mean |β̂| per variable, sorted descending.
#' Bars filled blue when above the selection cutoff, grey otherwise.
#'
#' @param x A \code{gg_beta_varpro} object from \code{\link{gg_beta_varpro}}.
#' @param ... Not currently used.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{gg_beta_varpro}}
#' @name plot.gg_beta_varpro
#' @importFrom ggplot2 ggplot aes geom_col geom_hline coord_flip
#' @importFrom ggplot2 scale_fill_manual labs theme_minimal
#' @export
plot.gg_beta_varpro <- function(x, ...) {
  if (nrow(x) == 0L) {
    stop("plot.gg_beta_varpro: nothing to plot (gg_beta_varpro has 0 rows).",
         call. = FALSE)
  }
  prov   <- attr(x, "provenance")
  cutoff <- prov$cutoff
  cv_txt <- if (isTRUE(prov$use.cv)) "cv" else "fixed"

  x$variable <- factor(x$variable, levels = x$variable[order(x$beta_mean)])

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
      y = "Mean |β| (per-rule lasso)",
      caption = sprintf(
        "Mean |β| over %d rules. Lasso: %s, cutoff: %.4g.",
        prov$n_rules_total %||% NA_integer_,
        cv_txt,
        cutoff
      )
    ) +
    ggplot2::theme_minimal()
}
