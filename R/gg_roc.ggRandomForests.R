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
#' gg_roc Generate a data from containing the sensitivity and specificity 
#' of a randomForests classification object. 
#' 
#' @param object an rfsrc classification object
#' @param which.outcome select the classification outcome of interest.
#' @param oob use oob estimates (default TRUE)
#' 
#' @return gg_roc data.frame for plotting ROC curves.
#' 
#' @seealso \code{\link{plot.gg_roc}} \code{rfsrc}
#' @examples
#' 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' # iris.obj <- rfsrc(Species ~ ., data = iris)
#' data(iris_rf,package="ggRandomForests")
#' # ROC for setosa
#' ggrf.obj<- gg_roc(iris_rf, which.outcome=1)
#' plot.gg_roc(ggrf.obj)
#' 
#' # ROC for versicolor
#' ggrf.obj<- gg_roc(iris_rf, which.outcome=2)
#' plot.gg_roc(ggrf.obj)
#' 
#' # ROC for virginica
#' ggrf.obj<- gg_roc(iris_rf, which.outcome=3)
#' plot.gg_roc(ggrf.obj)
#' 
#' @export gg_roc.ggRandomForests gg_roc
#' @aliases gg_roc
#' @importFrom dplyr tbl_df
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
    
    roc<- calcROC.randomForest(object, object$y, which.outcome=which.outcome)
  }else{
    if(object$family != "class")
      stop("gg_roc is intended for classification forests only.")
    
    roc<- calcROC.rfsrc(object, object$yvar, which.outcome=which.outcome, oob=oob)
  }
  roc <- tbl_df(roc)
  class(roc) <- c("gg_roc", class(roc))
  
  invisible(roc)
}
gg_roc <- gg_roc.ggRandomForests
