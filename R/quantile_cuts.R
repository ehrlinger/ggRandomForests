#' Find the cut points of a vector, evenly distributed along the vectors values.
#' 
#' @param object vector to be cut
#' @param groups how many groups do we want
#'
#' @description
#' This function finds the cut point values from a vector argument to produce
#'  \code{groups} intervals. Setting \code{groups=2} will return three values, 
#'  the two end points, and one mid point (at the median value of the vector). 
#' 
#' The output can be passed directly into the breaks argument of the
#' \code{cut} function for creating groups for coplots.
#' 
#' @return vector of groups+1 cut point values.
#' 
#' @seealso \code{cut} \code{\link{gg_partial_coplot}}
#' 
#' @examples
#' data(rfsrc_Boston)
#' 
#' # To create 6 intervals, we want 7 points. 
#' # quantile_cuts will find balanced intervals 
#' rm_pts <- quantile_cuts(rfsrc_Boston$xvar$rm, groups=6)
#' 
#' # Use cut to create the intervals
#' rm_grp <- cut(rfsrc_Boston$xvar$rm, breaks=rm_pts)
#' 
#' summary(rm_grp)
#' 
#' @export quantile_cuts

quantile_cuts <- function(object, groups){
  # We need one more break than group,
  breaks <- groups + 1
 
  if(sum(is.na(object))>0)
    object <- object[-which(is.na(object))]
  
  n.x <- length(unique(object))
  if (n.x > breaks) {
    x.uniq <- sort(unique(object))[unique(as.integer(seq(1, n.x, length = min(breaks, n.x))))]
  }else{
    x.uniq <- unique(object)
  }

  x.uniq[1] <- x.uniq[1] - 0.000001
  x.uniq[length(x.uniq)] <- x.uniq[length(x.uniq)]
  
  x.uniq
}
