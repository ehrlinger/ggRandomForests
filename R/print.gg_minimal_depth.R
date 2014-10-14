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
#' Print a \code{\link{gg_minimal_depth}} object.
#' 
#' @param x a \code{\link{gg_minimal_depth}} object.
#' @param ... optional arguments
#' 
#' @export print.gg_minimal_depth
#' 
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## You can build a randomForest
#' # iris_rf <- rfsrc(Species ~ ., data = iris)
#' # iris_vs <- var.select(iris_rf)
#' # ... or load a cached randomForestSRC object
#' data(iris_vs, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' ggrf.obj<- gg_minimal_depth(iris_vs)
#' print(ggrf.obj)
#' 
print.gg_minimal_depth <- function(x, ...){
  object <- x
  cat("-----------------------------------------------------------\n")
  cat("gg_minimal_depth\n")
  cat("model size         :", object$modelsize, "\n")
  cat("depth threshold    :", round(object$md.obj$threshold, 4),  "\n")
  cat("\n")
  cat("PE :\n")
  print(round(object$err.rate, 3))
  
  cat("-----------------------------------------------------------\n")
  cat("\n")
  cat("Top variables:\n")
  vSel <- select(object$varselect, -names)
  print(round(vSel[1:object$modelsize, , drop = FALSE], 3))
  cat("-----------------------------------------------------------\n")
  
}