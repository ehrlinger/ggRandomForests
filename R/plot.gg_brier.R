####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' Plot a \code{\link{gg_brier}} object
#'
#' Plot the time-resolved Brier score (default) or running CRPS for a
#' survival forest, optionally stratified by mortality-risk quartile.
#'
#' @param x A \code{\link{gg_brier}} object.
#' @param type Which series to plot: \code{"brier"} (default) or
#'   \code{"crps"}.
#' @param by_quartile Logical. When \code{TRUE}, draws one line per
#'   mortality-risk quartile (q25, q50, q75, q100). When \code{FALSE}
#'   (default), draws the overall series only.
#' @param ... Extra arguments forwarded to \code{ggplot2} layers.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{gg_brier}},
#'   \code{\link[randomForestSRC]{get.brier.survival}}
#'
#' @examples
#' \dontrun{
#' data(pbc, package = "randomForestSRC")
#' rf <- randomForestSRC::rfsrc(Surv(days, status) ~ ., data = pbc,
#'                              nsplit = 10)
#' gg_dta <- gg_brier(rf)
#' plot(gg_dta)
#' plot(gg_dta, type = "crps")
#' plot(gg_dta, by_quartile = TRUE)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme
#' @importFrom tidyr pivot_longer
#' @export
plot.gg_brier <- function(x,
                          type = c("brier", "crps"),
                          by_quartile = FALSE,
                          ...) {
  if (!inherits(x, "gg_brier")) {
    stop("Incorrect object type: Expects a gg_brier object")
  }
  type <- match.arg(type)

  y_label <- if (type == "brier") "Brier score" else "CRPS"

  if (by_quartile) {
    quartile_cols <- if (type == "brier") {
      c("bs.q25", "bs.q50", "bs.q75", "bs.q100")
    } else {
      c("crps.q25", "crps.q50", "crps.q75", "crps.q100")
    }
    pretty_levels <- c("Q1 (low risk)", "Q2", "Q3", "Q4 (high risk)")

    long <- tidyr::pivot_longer(
      x[, c("time", quartile_cols)],
      cols = dplyr::all_of(quartile_cols),
      names_to = "quartile",
      values_to = "value"
    )
    long$quartile <- factor(long$quartile,
                            levels = quartile_cols,
                            labels = pretty_levels)

    gg_plt <- ggplot2::ggplot(
      long,
      ggplot2::aes(
        x = .data[["time"]],
        y = .data[["value"]],
        colour = .data[["quartile"]]
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::labs(x = "Time", y = y_label, colour = "Mortality")
  } else {
    y_col <- if (type == "brier") "brier" else "crps"
    gg_plt <- ggplot2::ggplot(
      x,
      ggplot2::aes(x = .data[["time"]], y = .data[[y_col]])
    ) +
      ggplot2::geom_line() +
      ggplot2::labs(x = "Time", y = y_label) +
      ggplot2::theme(legend.position = "none")
  }

  return(gg_plt)
}
