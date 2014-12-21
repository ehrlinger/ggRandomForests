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
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' # interaction_iris <- find.interaction(rfsrc_iris)
#' #
#' # ... or load a cached randomForestSRC object
#' data(interaction_iris, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' gg_dta<- gg_interaction(interaction_iris)
#' print(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' data(interaction_airq, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' gg_dta<- gg_interaction(interaction_airq)
#' print(gg_dta)
#' 
print.gg_interaction <- function(x, ...){
  print(tbl_df(x))
}
  