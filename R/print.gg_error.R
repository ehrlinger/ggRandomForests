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
#' # iris_rf <- rfsrc(Species ~ ., data = iris)
#' # ... or load a cached randomForestSRC object
#' data(iris_rf, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' ggrf_err<- gg_error(iris_rf)
#' print(ggrf_err)
#' 
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' data(airq_rf, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' ggrf_err<- gg_error(airq_rf)
#' print(ggrf_err)
#' 
print.gg_error <- function(x, ...){
  print(tbl_df(x))
}
  