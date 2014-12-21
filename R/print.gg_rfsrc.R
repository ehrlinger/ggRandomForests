####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    John Ehrlinger, Ph.D.
####    Assistant Staff
####    Dept of Quantitative Health Sciences
####    Learner Research Institute
####    Cleveland Clinic Foundation
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' Print a \code{\link{gg_rfsrc}} object.
#' 
#' @param x a \code{\link{gg_rfsrc}} object.
#' @param ... optional arguments
#' 
#' @export print.gg_rfsrc
#' 
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## You can build a randomForest
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' # ... or load a cached randomForestSRC object
#' data(rfsrc_iris, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' gg_dta<- gg_rfsrc(rfsrc_iris)
#' print(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' # ... or load a cached randomForestSRC object
#' data(rfsrc_airq, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' gg_dta<- gg_rfsrc(rfsrc_airq)
#' print(gg_dta)
#' 
#' # To nicely print a rfsrc output... 
#' print.gg_rfsrc(rfsrc_airq)
#' 
#' 
print.gg_rfsrc <- function(x, ...){
  ### !!TODO!! need to actually print something meaningfull... like some recreation of 
  ### the print.rfsrc function?
  gg_dta <- x
  
  # If object is not a gg_rfsrc object, check if it is the output
  # from rfsrc::var.select
  if(!inherits(x, "gg_rfsrc"))
    gg_dta <- gg_rfsrc(x)
  
  class(gg_dta)
  
}