##=============================================================================
#' Plot a time-varying AUC curve
#'
#' Draws AUC(t) from a [gg_auct()] object: a line over time, a bootstrap
#' confidence ribbon when available, and a dashed reference line at 0.5
#' (chance). The integrated AUC (iAUC) appears in the caption. Times with a
#' non-finite AUC are dropped before drawing: from \pkg{randomForestRHF} 2.0.0
#' the final grid time can be `NA`, where the censoring-weight denominator is
#' undefined once the control set is nearly exhausted.
#'
#' @param x A `gg_auct` object from [gg_auct()].
#' @param ... Not currently used.
#'
#' @return A `ggplot` object.
#'
#' @seealso [gg_auct()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("randomForestRHF", quietly = TRUE)) {
#'   data(pbc, package = "randomForestSRC")
#'   d <- randomForestRHF::convert.counting(
#'     survival::Surv(days, status) ~ ., na.omit(pbc))
#'   o <- randomForestRHF::rhf("Surv(id, start, stop, event) ~ .", d, ntree = 30)
#'   plot(gg_auct(o))
#' }
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_hline labs theme_bw
#' @name plot.gg_auct
#' @export
plot.gg_auct <- function(x, ...) {
  if (!inherits(x, "gg_auct")) {
    stop("plot.gg_auct() requires a 'gg_auct' object.", call. = FALSE)
  }
  iauc <- attr(x, "iauc")
  caption <- if (!is.null(iauc) && is.finite(iauc$uno)) {
    sprintf("iAUC (Uno) = %.3f  |  iAUC (standardized) = %.3f",
            iauc$uno, iauc$std)
  } else {
    NULL
  }

  # randomForestRHF >= 2.0.0 can return an NA AUC at the final grid time, where
  # the censoring-weight denominator is undefined once the control set is
  # nearly exhausted. Drop those rows so geom_line() is not handed missing
  # values; the ribbon below is already guarded on finite bounds.
  dta <- x[is.finite(x$auc), , drop = FALSE]

  p <- ggplot2::ggplot(dta,
                       ggplot2::aes(x = .data[["time"]], y = .data[["auc"]]))

  ci <- x[is.finite(x$lower) & is.finite(x$upper), , drop = FALSE]
  if (nrow(ci) > 0L) {
    p <- p + ggplot2::geom_ribbon(
      data = ci,
      ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
      alpha = 0.2
    )
  }

  p +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed",
                        colour = "grey50") +
    ggplot2::labs(x = "Time", y = "AUC(t)",
                  title = sprintf("Time-varying AUC (%s)", x$marker[1]),
                  caption = caption) +
    ggplot2::theme_bw()
}
