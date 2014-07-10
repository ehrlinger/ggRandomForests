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
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#'
#' ggError.ggRandomForests
#' Extract the cumulative OOB randomForest error rates as a function of 
#' number of trees.
#' 
#' The ggError function simply returns the rfsrc err.rate object as 
#' a data.frame.
#' 
#' @param object randomForestSRC object
#' @param ... optional arguments
#' 
#' @return ggError data.frame with one column indicating the tree number, 
#' and the remaining columns from the rfsrc$err.rate return value. 
#' 
#' @export ggError.ggRandomForests ggError
#' 
#' @seealso \code{\link{plot.ggError}} \code{rfsrc} \code{plot.rfsrc}
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression 
#' and Classification (RF-SRC), R package version 1.4.
#' 
#' @aliases ggError ggError.ggRandomForests
#' @examples
#' 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' 
#' # Get a data.frame containing error rates
#' ggrf.obj<- ggError(iris.obj)
#' 
#' # Plot the ggError object
#' plot(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' ggrf.obj <- ggError(v.obj)
#' plot(ggrf.obj)
#' 
#' @importFrom reshape2 melt
#' @importFrom dplyr tbl_df
#'
### error rate plot
ggError.ggRandomForests <- function(object, ...) {
  ## Check that the input obect is of the correct type.
  if (inherits(object, "rfsrc") == FALSE){
    stop("This function only works for Forests grown with the randomForestSRC package.")
  }
  if (is.null(object$err.rate)) {
    stop("Performance values are not available for this forest.")
  }
  
  error <- data.frame(object$err.rate)
  if(is.null(dim(error))){
    error<- data.frame(error=cbind(error))
  }
  
  error$ntree <- 1:dim(error)[1]
  
  dta<-melt(error, id.vars = "ntree")
  dta <- tbl_df(dta)
  
  class(dta) <- c("ggError",class(dta))
  invisible(dta)
}

ggError <- ggError.ggRandomForests
