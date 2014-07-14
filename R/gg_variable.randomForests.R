####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####    John Ehrlinger, Ph.D.
####    Assistant Staff
####    Dept of Quantitative Health Sciences
####    Learner Research Institute
####    Cleveland Clinic Foundation
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggrandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' @title Plot the randomForests marginal dependence of variables.
#' 
#' @param object a randomForestSRC object
#' @param time time of interest
#' @param time.labels text label to title the time
#' @param ... extra arguments
#' 
#' @return gg_variable object
#'
#' @seealso \code{\link{plot.gg_variable}}
#' 
#' @export gg_variable.randomForests 
#'
gg_variable.randomForests <- function(object,
                                     time,
                                     time.labels,
                                     ...)
{
  stop("Function in development...")
  class(pDat) <- c("gg_variable", class(pDat))
  invisible(pDat)
}
