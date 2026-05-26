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
#' Draws the time-resolved Brier score (the default) or the running CRPS as
#' a curve against time.  Lower means more accurate probabilistic predictions.
#' Turn \code{envelope = TRUE} on and the overall line picks up a ribbon
#' spanning the 15th to 85th percentile of the per-subject contributions,
#' which shows how much the score varies across subjects at each time.
#'
#' @param x A \code{\link{gg_brier}} object.
#' @param type Which series to plot: \code{"brier"} (default) or
#'   \code{"crps"}.
#' @param envelope Logical. When \code{TRUE}, overlays a ribbon spanning
#'   the 15th-85th percentile of per-subject Brier (or running CRPS)
#'   contributions at each time, around the overall line. When \code{FALSE}
#'   (default), draws the overall series only.
#' @param ... Extra arguments forwarded to \code{geom_line()}.
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
#' plot(gg_dta, envelope = TRUE)   # adds 15-85% envelope
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs theme
#' @export
plot.gg_brier <- function(x,
                          type = c("brier", "crps"),
                          envelope = FALSE,
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

  if (envelope) {
    gg_plt <- gg_plt +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[[lo_col]], ymax = .data[[hi_col]]),
        alpha = 0.2,
        fill  = "steelblue"
      )
  }

  gg_plt <- gg_plt +
    ggplot2::geom_line(...) +
    ggplot2::labs(x = "Time", y = y_label) +
    ggplot2::theme(legend.position = "none")

  return(gg_plt)
}
