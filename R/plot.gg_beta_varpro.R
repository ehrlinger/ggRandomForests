##=============================================================================
#' Plot a `gg_beta_varpro` object
#'
#' Horizontal bar chart of `mean(|β̂|)` per variable, sorted descending so
#' the eye lands on the top variable first. Bars filled blue when above the
#' selection cutoff, grey otherwise. Dashed red line marks the cutoff.
#'
#' @section Reading the chart:
#' Each bar is the average magnitude of a per-rule lasso coefficient for
#' that variable. **The numeric scale carries the predictor's units** —
#' if "age" is in years and "creatinine" is in mg/dL, a longer bar for
#' age does not mean age is "more important" in any unit-free sense.
#' Comparisons across data sets or across variables with very different
#' units require keeping the units context in mind. Within one data set,
#' bars are comparable up to that unit caveat.
#'
#' Variables above the cutoff are coloured blue and flagged `selected`;
#' variables below are grey. Lasso shrinkage can drive a rule's β̂ to
#' exactly zero — those rules are kept in the average, so a variable
#' with many shrunk-to-zero rules will sit lower in the ranking than
#' one whose released coefficients are consistently non-zero.
#'
#' @section What this tells you:
#' Use the bar chart as a selection ranking, not as an effect-size axis.
#' Pair it with [gg_varpro()] to see where split-strength importance and
#' local lasso-β importance agree or disagree; disagreement is often the
#' interesting signal.
#'
#' @param x A `gg_beta_varpro` object from [gg_beta_varpro()].
#' @param ... Not currently used.
#'
#' @return A `ggplot` object.
#'
#' @seealso [gg_beta_varpro()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
#'   plot(gg_beta_varpro(v))
#' }
#' }
#'
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
      y = "Mean |beta| (per-rule lasso)",
      caption = sprintf(
        "Mean |beta| over %d rules. Lasso: %s, cutoff: %.4g.",
        prov$n_rules_total %||% NA_integer_,
        cv_txt,
        cutoff
      )
    ) +
    ggplot2::theme_minimal()
}
