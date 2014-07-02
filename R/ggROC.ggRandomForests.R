#'
#' ggROC create a set of classification performance plots from an rfsrc 
#' classification rfObject
#' 
#' @param rfObject an rfsrc classification rfObject
#' @param which.outcome 
#' @param show
#' @examples
#' 
#' ## Edgar Anderson's iris data
#' iris.obj <- rfsrc(Species ~., data = iris)
#' #ggROC(iris.obj)
#' 
#' @export ggROC.ggRandomForests ggROC
ggROC.ggRandomForests <- function(rfObject, which.outcome, oob=TRUE){
  
  if (sum(inherits(rfObject, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(rfObject, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2 & 
        !inherits(rfObject, "randomForest")) {
    stop("This function only works for rfObjects of class `(rfsrc, grow)', '(rfsrc, predict)' or 'randomForest.")
  }
  
  if(missing(which.outcome)) which.outcome=1
  
  if(inherits(rfObject, "randomForest")){
    if(rfObject$type != "classification")
      stop("ggROC is intended for classification forests only.")
    
    roc<- calcROC.randomForest(rfObject, rfObject$y, which.outcome=which.outcome)
  }else{
    if(rfObject$family != "class")
      stop("ggROC is intended for classification forests only.")
    
    roc<- calcROC(rfObject, rfObject$yvar, which.outcome=which.outcome, oob=oob)
  }
  class(roc) <- c("ggROC", class(roc))
  
  invisible(roc)
}
ggROC <- ggROC.ggRandomForests
