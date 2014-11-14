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
#' Plot a \code{\link{gg_variable}} object,
#' 
#' @param x \code{\link{gg_variable}} object created from a \code{randomForestSRC::rfsrc} object
#' @param x_var variable (or list of variables) of interest.
#' @param time For survival, one or more times of interest
#' @param time_labels string labels for times
#' @param oob oob estimates (boolean)
#' @param smooth type of smooth curve
#' @param span tuning parameter for loess smooths
#' @param ... arguments passed to the \code{\link{gg_variable}} function.
#'   
#' @return \code{ggplot} object
#'   
#' @export plot.gg_variable
#'   
#' @references Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, Rnews,
#' 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression
#' and Classification (RF-SRC), R package version 1.4.
#' 
#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth labs facet_wrap
#' 
#' @examples
#' \dontrun{
#' ## ------------------------------------------------------------
#' ## classification
#' ## ------------------------------------------------------------
#' 
#' ## iris
#' #iris.obj <- rfsrc(Species ~., data = iris)
#' data(iris_rf, package="ggRandomForests")
#' 
#' ## !! TODO... finish classification variable dependence 
#' # ggrf <- gg_variable(iris_rf, which.outcome=1)
#' 
#' 
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#' 
#' ## airquality
#' #airq.obj <- rfsrc(Ozone ~ ., data = airquality)
#' data(airq_rf, package="ggRandomForests")
#' ggrf <- gg_variable(airq_rf)
#' plot(ggrf, x_var="Wind")
#' plot(ggrf, x_var="Temp")
#' plot(ggrf, x_var="Solar.R")
#' 
#' ## motor trend cars
#' #mtcars.obj <- rfsrc(mpg ~ ., data = mtcars)
#' data(mtcars_rf, package="ggRandomForests")
#' ggrf <- gg_variable(mtcars_rf)
#' 
#' # mtcars$cyl is an ordinal variable
#' plot(ggrf, x_var="cyl")
#' 
#' # Others are continuous
#' plot(ggrf, x_var="disp")
#' plot(ggrf, x_var="hp")
#' plot(ggrf, x_var="wt")
#' 
#' ## ------------------------------------------------------------
#' ## survival examples
#' ## ------------------------------------------------------------
#' 
#' ## survival
#' # data(veteran, package = "randomForestSRC")
#' # veteran_rf <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
#' data(veteran_rf, package="ggRandomForests")
#' 
#' # get the 30 day survival time.
#' ggrf <- gg_variable(veteran_rf, time=30)
#' 
#' # Generate variable dependance plots for age and diagtime
#' plot(ggrf, x_var = "age")
#' plot(ggrf, x_var = "diagtime")
#' 
#' # If we want to compare survival at different time points, say 30, 90 day 
#' # and 1 year
#' ggrf <- gg_variable(veteran_rf, time=c(30, 90, 365))
#' 
#' # Generate variable dependance plots for age and diagtime
#' plot(ggrf, x_var = "age")
#' plot(ggrf, x_var = "diagtime") 
#'
#' }
### error rate plot
plot.gg_variable<- function(x, x_var, time, time_labels, oob=TRUE, smooth=TRUE, span, ...){
  object <- x 
  if(inherits(object, "rfsrc")) object<- gg_variable(object, ...)
  
  if(inherits(object, "surv")){
    family <- "surv"
  }else if(inherits(object, "regr")){
    family <- "regr"
  }else{
    family <- "class"
  }
  if(length(grep("yhat.", colnames(object))) > 0){
    # We have a classification forest with multiple outcomes.
    if(length(grep("yhat.", colnames(object))) == 2){
      object  <- object[, -grep("yhat.", colnames(object))[1]]
      colnames(object)[grep("yhat.", colnames(object))] <- "yhat"
    }
  }
  
  sm_curve <- FALSE
  if(is.logical(smooth)){ 
    sm_curve <- smooth
    smooth="loess"
  }else{
    sm_curve <- TRUE
  }
  
  if(sum(colnames(object) == "cens") != 0) family <- "surv"
  
  if(missing(x_var)){
    # We need to remove response variables here
    cls <- c(grep("yhat", colnames(object)),
             grep("cens", colnames(object)))
    x_var <- colnames(object)[-cls]
  }
  
  lng <- length(x_var)
  gDta <- vector("list", length=lng)
  
  for(ind in 1:lng){
    chIndx <- which(colnames(object)==x_var[ind])
    hName <- colnames(object)[chIndx]
    colnames(object)[chIndx] <- "var"
    ccls <- class(object[,"var"])
    
    # Check for logicals...
    if(length(unique(object[,"var"])) < 3 & ccls =="numeric" ){
      ccls <- "logical"
      object[,"var"] <- as.logical(object[,"var"])
    } 
    gDta[[ind]] <- ggplot(object)
    
    if(family == "surv"){
      gDta[[ind]] <- gDta[[ind]]+
        labs(x=hName, y= "Survival")
      if(ccls=="numeric"){
        gDta[[ind]] <- gDta[[ind]]+
          geom_point(aes_string(x="var", y="yhat", color="cens", shape="cens"), 
                     alpha=.5)
        
        if(sm_curve){
          if(missing(span)){
            gDta[[ind]] <- gDta[[ind]] +
              geom_smooth(aes_string(x="var", y="yhat"), se=FALSE, method=smooth)
          }else{
            gDta[[ind]] <- gDta[[ind]] +
              geom_smooth(aes_string(x="var", y="yhat"), se=FALSE, method=smooth,
                          span=span)
          }
        }
      }else{
        gDta[[ind]] <- gDta[[ind]]+
          geom_boxplot(aes_string(x="var", y="yhat"), color="grey",
                       alpha=.5,outlier.shape = NA)+
          geom_jitter(aes_string(x="var", y="yhat", color="cens", shape="cens"), 
                      alpha=.5)
        
      }
      if(length(levels(object$time)) > 1){
        gDta[[ind]]<- gDta[[ind]] + facet_wrap(~time, ncol=1)
      }else{
        gDta[[ind]]<- gDta[[ind]] + 
          labs(x=hName, y= paste("Survival at", object$time[1], "year"))
      }
    }else if(family == "class"){
      if(sum(colnames(object) == "yhat") ==1){
        gDta[[ind]] <- gDta[[ind]] +
          labs(x=hName, y="Predicted")
        if(ccls=="numeric"){
          gDta[[ind]] <- gDta[[ind]] +
            geom_point(aes_string(x="var", y="yhat", color="yvar", shape="yvar"),
                       alpha=.5)
          
          if(sm_curve){
            if(missing(span)){
              gDta[[ind]] <- gDta[[ind]] +
                geom_smooth(aes_string(x="var", y="yhat"), se=FALSE, method=smooth)
            }else{
              gDta[[ind]] <- gDta[[ind]] +
                geom_smooth(aes_string(x="var", y="yhat"), se=FALSE, method=smooth, 
                            span=span)
            }
          }
        }else{
          gDta[[ind]] <- gDta[[ind]]+
            geom_boxplot(aes_string(x="var", y="yhat"), color="grey", 
                         alpha=.5, outlier.shape = NA)+
            geom_jitter(aes_string(x="var", y="yhat", color="yvar", shape="yvar"), 
                        alpha=.5)
          
        }
      }else{
        stop("Multiclass variable dependence has not been implemented yet.")
      }
    }else{
      # assume regression
      gDta[[ind]] <- gDta[[ind]] +
        labs(x=hName, y="Predicted")
      if(ccls=="numeric"){
        gDta[[ind]] <- gDta[[ind]] +
          geom_point(aes_string(x="var", y="yhat"), alpha=.5)
        
        if(sm_curve){
          if(missing(span)){
            gDta[[ind]] <- gDta[[ind]] +
              geom_smooth(aes_string(x="var", y="yhat"), se=FALSE, method=smooth)
          }else{
            gDta[[ind]] <- gDta[[ind]] +
              geom_smooth(aes_string(x="var", y="yhat"), se=FALSE, method=smooth, span=span)
          }
        }
      }else{
        gDta[[ind]] <- gDta[[ind]]+
          geom_boxplot(aes_string(x="var", y="yhat"), color="grey", 
                       alpha=.5, outlier.shape = NA)+
          geom_jitter(aes_string(x="var", y="yhat"), 
                      alpha=.5)
      }
      # Replace the original colname
      colnames(object)[chIndx] <- hName
    }
  }
  
  if(lng == 1) gDta <- gDta[[1]]
  else class(gDta) <- c("gg_variableList", class(gDta))
  
  return(gDta)
}
