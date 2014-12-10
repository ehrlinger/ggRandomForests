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
#' Print a \code{\link{gg_interaction}} object.
#' 
#' @param x a \code{\link{gg_interaction}} object.
#' @param ... optional arguments
#' 
#' We use the tble_df command to print \code{gg_interaction} objects because they
#' tend to be long (ntree records long).
#' 
#' @export print.gg_interaction
#' 
#' @importFrom dplyr tbl_df
#' 
#' @seealso \code{rfsrc} \code{tbl_df} \code{\link{gg_interaction}}
#' 
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## You can build a randomForest
#' # iris_rf <- rfsrc(Species ~ ., data = iris)
#' # iris_interaction <- find.interaction(iris_rf)
#' #
#' # ... or load a cached randomForestSRC object
#' data(iris_interaction, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' gg_dta<- gg_interaction(iris_interaction)
#' print(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' data(airq_interaction, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' gg_dta<- gg_interaction(airq_interaction)
#' print(gg_dta)
#' 
print.gg_interaction <- function(x, ...){
  print(tbl_df(x))
}
  