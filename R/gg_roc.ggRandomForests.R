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
#' ROC (Receiver operator curve) data from a classification random forest.
#' 
#' The sensitivity and specificity of a randomForests classification object. 
#' 
#' @param object an \code{randomForestSRC::rfsrc} classification object
#' @param which.outcome select the classification outcome of interest.
#' @param oob use oob estimates (default TRUE)
#' 
#' @return gg_roc \code{data.frame} for plotting ROC curves.
#' 
#' @seealso \code{\link{plot.gg_roc}} \code{randomForestSRC::rfsrc}
#' 

#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' #iris_rf <- rfsrc(Species ~ ., data = iris)
#' data(iris_rf, package="ggRandomForests")
#'
#' # ROC for setosa
#' gg_dta <- gg_roc(iris_rf, which.outcome=1)
#' plot.gg_roc(gg_dta)
#' 
#' # ROC for versicolor
#' gg_dta <- gg_roc(iris_rf, which.outcome=2)
#' plot.gg_roc(gg_dta)
#' 
#' # ROC for virginica
#' gg_dta <- gg_roc(iris_rf, which.outcome=3)
#' plot.gg_roc(gg_dta)
#' 
#' # Alternatively, you can plot all three outcomes in one go
#' # by calling the plot function on the full forest. 
#' plot.gg_roc(iris_rf)
#' 
#' 
#' @export gg_roc.ggRandomForests gg_roc
#' @aliases gg_roc
gg_roc.ggRandomForests <- function(object, which.outcome, oob=TRUE){
  
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2 & 
        !inherits(object, "randomForest")) {
    stop("This function only works for objects of class `(rfsrc, grow)', '(rfsrc, predict)' or 'randomForest.")
  }
  if(!inherits(object, "class")){
    stop("gg_roc only works with classification forests")
  }
  
  # Want to remove the which.outcomes argument to plot ROC for all outcomes simultaneously.
  if(missing(which.outcome)) which.outcome="all"
  
  if(inherits(object, "randomForest")){
    if(object$type != "classification")
      stop("gg_roc is intended for classification forests only.")
    
    gg_dta<- calc_roc.randomForest(object, object$y, which.outcome=which.outcome)
  }else{
    if(object$family != "class")
      stop("gg_roc is intended for classification forests only.")
    
    gg_dta<- calc_roc.rfsrc(object, object$yvar, which.outcome=which.outcome, oob=oob)
  }
  class(gg_dta) <- c("gg_roc", class(gg_dta))
  
  invisible(gg_dta)
}
gg_roc <- gg_roc.ggRandomForests
