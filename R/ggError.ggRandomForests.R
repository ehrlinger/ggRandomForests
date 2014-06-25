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
#'
#' ggError.ggRandomForests
#' Cumulative OOB error rates of the forest as a function of number of trees.
#' 
#' @param rfObj randomForestSRC object
#' 
#' @return ggError object
#' 
#' @export ggError.ggRandomForests ggError
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression and Classification (RF-SRC), R package version 1.4.
#' 
#' @examples
#' 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' ggrf.obj<- ggError(iris.obj)
#' plot(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRCM")
#' v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' ggrf.obj <- ggError(v.obj)
#' plot(ggrf.obj)
#'
### error rate plot
ggError.ggRandomForests <- function(rfObj, ...) {
  ## Check that the input obect is of the correct type.
  if (inherits(rfObj, "rfsrc") == FALSE){
    stop("This function only works for Forests grown with the randomForestSRC package.")
  }
  if (is.null(rfObj$err.rate)) {
    stop("Performance values are not available for this forest.")
  }
  
  error <- as.data.frame(rfObj$err.rate)
  if(is.null(dim(error))){
    error<- data.frame(error=cbind(error))
    legend.position="none"
  }
  
  error$indx <- 1:dim(error)[1]
  
  dta<-melt(error, id.vars = "indx")
  
  class(dta) <- c("ggError",class(dta))
  invisible(dta)
}

ggError <- ggError.ggRandomForests
