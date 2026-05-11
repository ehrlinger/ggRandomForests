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
#' survival forest, optionally overlaid with a 15-85 percent per-subject
#' envelope.
#'
#' @param x A \code{\link{gg_brier}} object.
#' @param type Which series to plot: \code{"brier"} (default) or
#'   \code{"crps"}.
#' @param by_quartile Logical. When \code{TRUE}, overlays a ribbon
#'   spanning the 15th-85th percentile of per-subject Brier (or running
#'   CRPS) contributions at each time, around the overall line. When
#'   \code{FALSE} (default), draws the overall series only.
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
#' plot(gg_dta, by_quartile = TRUE)   # adds 15-85% envelope
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs theme
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
  y_col   <- if (type == "brier") "brier" else "crps"
  lo_col  <- if (type == "brier") "bs.lower" else "crps.lower"
  hi_col  <- if (type == "brier") "bs.upper" else "crps.upper"

  gg_plt <- ggplot2::ggplot(
    x,
    ggplot2::aes(x = .data[["time"]], y = .data[[y_col]])
  )

  if (by_quartile) {
    gg_plt <- gg_plt +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[[lo_col]], ymax = .data[[hi_col]]),
        alpha = 0.2,
        fill = "steelblue"
      )
  }

  gg_plt <- gg_plt +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Time", y = y_label) +
    ggplot2::theme(legend.position = "none")

  return(gg_plt)
}
