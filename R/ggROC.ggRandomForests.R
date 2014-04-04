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
ggROC.ggRandomForests <- function(rfObject, which.outcome, show=TRUE){
  
  if (sum(inherits(rfObject, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(rfObject, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2 & 
        !inherits(rfObject, "randomForest")) {
    stop("This function only works for rfObjects of class `(rfsrc, grow)', '(rfsrc, predict)' or 'randomForest.")
  }
  
  if(missing(which.outcome)) which.outcome=1
  
  if(inherits(rfObject, "randomForest")){
    if(rfObject$type != "classification")
      stop("ggROC is intended for classification forests only.")
    
    spc<- calcROC.randomForest(rfObject, rfObject$y, which.outcome=which.outcome)
  }else{
    if(rfObject$family != "class")
      stop("ggROC is intended for classification forests only.")
    
    spc<- calcROC(rfObject, rfObject$class, which.outcome=which.outcome)
  }
  
  plt<-ggplot(data=spc)+geom_line(aes(x=(1-sens), y=spec))+
    geom_abline(a=1, b=0) +coord_fixed()

  auc <- calcAUC(spc)
  plt<-plt+annotate(x=.5,y=.2,geom="text", label=paste("AUC = ",round(auc, digits=3), sep=""), hjust=0)
  if(show) show(plt)
  invisible(plt)
  ##
  ## mstn<- calcROC(rf, dta$sten_grp, which.outcome=2)
  ## ggplot(data=stn)+geom_line(aes(x=(1-sens), y=spec))+theme_bw()+geom_abline(a=1, b=0)+
  ##   geom_line(aes(x=(1-sens), y=spec), data=mstn,col="red")
  ##
  ## nstn<- calcROC(rf, dta$sten_grp, which.outcome=1)
  ## ggplot(data=stn)+geom_line(aes(x=(1-sens), y=spec))+theme_bw()+geom_abline(a=1, b=0)+
  ##   geom_line(aes(x=(1-sens), y=spec), data=mstn,col="red") + 
  ##   geom_line(aes(x=(1-sens), y=spec), data=nstn, col="blue")
}
ggROC <- ggROC.ggRandomForests
