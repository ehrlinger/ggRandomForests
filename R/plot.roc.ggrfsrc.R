#'
#' plot.roc create a set of classification performance plots from an rfsrc 
#' classification object
#' 
#' @param object an rfsrc classification object
#' @param which.outcome 
#' @param show
#' @examples
#' ## Edgar Anderson's iris data
#' iris.obj <- rfsrc(Species ~., data = iris)
#' #plot.roc(iris.obj)
#' 
#' @export plot.roc.ggrfsrc plot.roc
plot.roc.ggrfsrc <- function(object, which.outcome, show=TRUE){
  
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2 & 
        !inherits(object, "randomForest")) {
    stop("This function only works for objects of class `(rfsrc, grow)', '(rfsrc, predict)' or 'randomForest.")
  }
  
  if(missing(which.outcome)) which.outcome=1
  if(inherits(object, "randomForest")){
    if(object$type != "classification")
      stop("plot.roc is intended for classification forests only.")
    
    
    spc<- calc.roc.randomForest(object, object$y, which.outcome=which.outcome)
  }else{
    if(object$family != "class")
      stop("plot.roc is intended for classification forests only.")
    
    spc<- calc.roc(object, object$class, which.outcome=which.outcome)
  }
  plt<-ggplot(data=spc)+geom_line(aes(x=(1-sens), y=spec))+
    geom_abline(a=1, b=0) + coord_cartesian(xlim=c(0,1), ylim=c(0,1))
  
  auc <- calc.auc(spc)
  print(auc)
  if(show) show(plt)
  invisible(plt)
  ##
  ## mstn<- calc.roc(rf, dta$sten_grp, which.outcome=2)
  ## ggplot(data=stn)+geom_line(aes(x=(1-sens), y=spec))+theme_bw()+geom_abline(a=1, b=0)+
  ##   geom_line(aes(x=(1-sens), y=spec), data=mstn,col="red")
  ##
  ## nstn<- calc.roc(rf, dta$sten_grp, which.outcome=1)
  ## ggplot(data=stn)+geom_line(aes(x=(1-sens), y=spec))+theme_bw()+geom_abline(a=1, b=0)+
  ##   geom_line(aes(x=(1-sens), y=spec), data=mstn,col="red") + 
  ##   geom_line(aes(x=(1-sens), y=spec), data=nstn, col="blue")
}
plot.roc <- plot.roc.ggrfsrc
