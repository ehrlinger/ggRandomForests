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
#' Quantile-based cut points for coplots
#'
#' @param object Numeric vector of predictor values.
#' @param groups Number of quantile points (or intervals) to compute.
#' @param intervals Logical indicating whether to return interval boundaries
#'   suitable for \code{cut()} (length \code{groups + 1}) or the interior
#'   quantile points (length \code{groups}).
#'
#'
#' @description
#' This helper wraps \code{\link[stats]{quantile}} to create well-spaced
#' cut points for conditioning plots. When \code{intervals = TRUE} the lower
#' boundary is nudged down so that \code{cut()} treats the minimum value as a
#' valid observation.
#'
#' The output can be passed directly into the breaks argument of the
#' \code{cut} function for creating groups for coplots.
#'
#' @return Numeric vector of quantile points. When \code{intervals = TRUE}
#'   the result is strictly increasing and can be supplied to \code{cut()} to
#'   produce \code{groups} balanced strata.
#'
#' @seealso \code{cut}
#' @importFrom stats quantile
#'
#' @examples
#' data(Boston, package = "MASS")
#' rfsrc_boston <- randomForestSRC::rfsrc(medv ~ ., Boston)
#'
#' # To create 6 intervals, we want 7 points.
#' # quantile_pts will find balanced intervals
#' rm_pts <- quantile_pts(rfsrc_boston$xvar$rm, groups = 6, intervals = TRUE)
#'
#' # Use cut to create the intervals
#' rm_grp <- cut(rfsrc_boston$xvar$rm, breaks = rm_pts)
#'
#' summary(rm_grp)
#'
#' @export
quantile_pts <- function(object, groups, intervals = FALSE) {
  if (!is.numeric(groups) || groups < 1) {
    stop("`groups` must be a positive integer")
  }
  groups <- as.integer(groups)

  object <- stats::na.omit(object)
  if (!length(object)) {
    return(numeric())
  }

  probs <- if (intervals) {
    seq(0, 1, length.out = groups + 1)
  } else {
    seq(0, 1, length.out = groups)
  }

  pts <- as.numeric(stats::quantile(object,
    probs = probs,
    na.rm = TRUE,
    type = 2
  ))

  # Ensure breaks are strictly increasing for cut()
  if (intervals) {
    pts <- unique(pts)
    if (length(pts) < 2) {
      pts <- c(pts, pts + .Machine$double.eps)
    }
    pts[1] <- pts[1] - 1e-7
  }

  pts
}
