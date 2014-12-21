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
#' Print a \code{\link{gg_error}} object.
#' 
#' @param x a \code{\link{gg_error}} object.
#' @param ... optional arguments
#' 
#' We use the tble_df command to print \code{gg_error} objects because they
#' tend to be long (ntree records long).
#' 
#' @export print.gg_error
#' 
#' @importFrom dplyr tbl_df
#' 
#' @seealso \code{rfsrc} \code{tbl_df} \code{\link{gg_error}}
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
#' gg_dta<- gg_error(rfsrc_iris)
#' print(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' data(rfsrc_airq, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' gg_dta<- gg_error(rfsrc_airq)
#' print(gg_dta)
#' 
print.gg_error <- function(x, ...){
  print(tbl_df(x))
}
  