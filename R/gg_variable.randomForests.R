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
#' @title Plot the randomForest marginal dependence of variables.
#' 
#' @param object a \code{randomForest::randomForest} object
#' @param time time of interest
#' @param time.labels text label to title the time
#' @param ... extra arguments
#' 
#' @return gg_variable object
#'
#' @seealso \code{\link{plot.gg_variable}} \code{randomForest::randomForest}
#' 
#' export gg_variable.randomForests 
#'
gg_variable.randomForests <- function(object,
                                     time,
                                     time.labels,
                                     ...)
{
  stop("Function in development...")
  class(gg_dta) <- c("gg_variable", class(gg_dta))
  invisible(gg_dta)
}
