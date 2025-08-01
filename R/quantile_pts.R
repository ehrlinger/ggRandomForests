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
#' Find points evenly distributed along the vectors values.
#'
#' @param object vector object of values.
#' @param groups how many points do we want
#' @param intervals should we return the raw points or intervals to
#' be passed to the \code{cut} function
#'
#'
#' @description
#' This function finds point values from a vector argument to produce
#'  \code{groups} intervals. Setting \code{groups=2} will return three values,
#'  the two end points, and one mid point (at the median value of the vector).
#'
#' The output can be passed directly into the breaks argument of the
#' \code{cut} function for creating groups for coplots.
#'
#' @return vector of groups+1 cut point values.
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
  # We need one more break than group,
  breaks <- groups
  if (intervals) {
    breaks <- breaks + 1
  }

  if (sum(is.na(object)) > 0) {
    object <- object[-which(is.na(object))]
  }

  n_x <- length(unique(object))
  if (n_x > breaks) {
    x_uniq <-
      sort(unique(object))[unique(as.integer(seq(1,
        n_x,
        length = min(
          breaks,
          n_x
        )
      )))]
  } else {
    x_uniq <- unique(object)
  }

  # If we're looking for intervals, we need to include the first point
  # cut will make it NA if we don't move this a little bit lower,
  # because of the (lv, uv] interval definition.
  if (intervals) {
    x_uniq[1] <- x_uniq[1] - 1.e-7
  }
  x_uniq
}
