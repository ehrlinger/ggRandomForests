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
#' plot.gg_variable Plot a \code{\link{gg_variable}} object,
#' 
#' @param x gg_variable object created from a randomForestSRC object
#' @param x_var variable (or list of variables) of interest.
#' @param time For survival, one or more times of interest
#' @param time_labels string labels for times
#' @param oob oob estimates (boolean)
#' @param smooth type of smooth curve
#' @param span tuning parameter for loess smooths
#' @param ... arguments passed to the \code{\link{gg_variable}} function.
#'   
#' @return ggplot object
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
#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth labs
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
    
    gDta[[ind]] <- ggplot(object)
    if(family == "surv"){
      gDta[[ind]] <- gDta[[ind]] +
        geom_point(aes_string(x="var", y="yhat", col="cens", shape="cens"), alpha=.5)+
        labs(x=hName, y= "Survival")
      
      if(sm_curve){
        if(missing(span)){
          gDta[[ind]] <- gDta[[ind]] +
            geom_smooth(aes_string(x="var", y="yhat"), se=FALSE, method=smooth)
        }else{
          gDta[[ind]] <- gDta[[ind]] +
            geom_smooth(aes_string(x="var", y="yhat"), se=FALSE, method=smooth, span=span)
        }
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
          geom_point(aes_string(x="var", y="yhat", color="yvar", shape="yvar"), alpha=.5)+
          labs(x=hName, y="Predicted")
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
        stop("Multiclass variable dependence has not been implemented yet.")
      }
    }else{
      # assume regression
      gDta[[ind]] <- gDta[[ind]] +
        geom_point(aes_string(x="var", y="yhat"), alpha=.5)+
        labs(x=hName, y="Predicted")
      if(sm_curve){
        if(missing(span)){
          gDta[[ind]] <- gDta[[ind]] +
            geom_smooth(aes_string(x="var", y="yhat"), se=FALSE, method=smooth)
        }else{
          gDta[[ind]] <- gDta[[ind]] +
            geom_smooth(aes_string(x="var", y="yhat"), se=FALSE, method=smooth, span=span)
        }
      }
      
      # Replace the original colname
      colnames(object)[chIndx] <- hName
    }
  }
  
  if(lng == 1) gDta <- gDta[[1]]
  else class(gDta) <- c("gg_variableList", class(gDta))
  
  return(gDta)
}
