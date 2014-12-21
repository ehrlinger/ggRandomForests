#' Find the cut points of a vector, evenly distributed along the distribution
#' 
#' @param object vector to be cut
#' @param groups how many groups do we want
#'
#' This function breaks a vector into cut points to produce groups intervals. 
#' Setting groups=2 will return three values,the two end points, and one mid point 
#' at the median value of the vector. 
#' 
#' The output can be passed directly into the breaks argument of the
#' \code{cut} function for creating groups for coplots.
#' 
#' @seealso \code{cut} \code{\link{gg_partial_coplot}}
#' 
#' @examples
#' data(rfsrc_Boston)
#' 
#' # To create 6 intervals, we want 7 points. 
#' # cut_distribution will find balanced intervals 
#' rm_pts <- cut_distribution(rfsrc_Boston$xvar$rm, groups=6)
#' rm_grp <- cut(rfsrc_Boston$xvar$rm, breaks=rm_pts)
#' summary(rm_grp)
#' 
#' @export cut_distribution

cut_distribution <- function(object, groups){
  # We need one more break than group,
  breaks <- groups + 1
  n.x <- length(unique(object))
  if (n.x > breaks) {
    x.uniq <- sort(unique(object))[unique(as.integer(seq(1, n.x, length = min(breaks, n.x))))]
  }
  
  if(x.uniq[1] > trunc(x.uniq[1], digits=1)){
    x.uniq[1] <-  trunc(x.uniq[1], digits=1)
  }else{
    x.uniq[1] <-  trunc(x.uniq[1])
  }
  x.uniq
}