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
#' plot.ggVariable
#' Plot a \code{\link{ggVariable}} object, 
#' 
#' @param x ggVariable object created from a randomForestSRC object
#' @param var variable (or list of variables) of interest.
#' @param ... arguments passed to the \code{\link{ggVariable}} function.
#' 
#' @return ggplot object
#' 
#' @export plot.ggVariable
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for 
#' R, Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, 
#' Regression and Classification (RF-SRC), R package version 1.4.
#' 
### error rate plot
plot.ggVariable<- function(x, x.var, time, time.labels, oob=TRUE, smooth=TRUE, ...){
  object <- x 
  if(inherits(object, "rfsrc")) object<- ggVariable(object, ...)
  
  if(length(grep("yhat.", colnames(object))) > 0){
    # We have a classification forest with multiple outcomes.
    if(length(grep("yhat.", colnames(object))) == 2){
      object  <- object[, -grep("yhat.", colnames(object))[1]]
      colnames(object)[grep("yhat.", colnames(object))] <- "yhat"
    }
    family <- "class"
  }
  
  if(sum(colnames(object) == "cens") != 0) family <- "surv"
  
  if(missing(x.var)){
    # We need to remove response variables here
    cls <- c(grep("yhat", colnames(object)),
             grep("cens", colnames(object)))
    x.var <- colnames(object)[-cls]
  }
  lng <- length(x.var)
  gDta <- vector("list", length=lng)
  
  for(ind in 1:lng){
    chIndx <- which(colnames(object)==x.var[ind])
    hName <- colnames(object)[chIndx]
    colnames(object)[chIndx] <- "var"
    
    gDta[[ind]] <- ggplot(object)
    if(family == "surv"){
      gDta[[ind]] <- gDta[[ind]] +
        geom_point(aes(x=var, y=yhat, col=cens, shape=cens), alpha=.5)+
        labs(x=hName, y= "Survival")
      
      if(smooth){
        gDta[[ind]] <- gDta[[ind]] +
          geom_smooth(aes(x=var, y=yhat))
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
          geom_point(aes(x=var, y=yhat, color=yvar, shape=yvar), alpha=.5)+
          labs(x=hName, y="Predicted")
        if(smooth){
          gDta[[ind]] <- gDta[[ind]] +
            geom_smooth(aes(x=var, y=yhat))
        }
        
      }else{
        stop("Multiclass variable dependence has not been implemented yet.")
      }
    }else{
      # assume regression
      gDta[[ind]] <- gDta[[ind]] +
        geom_point(aes(x=var, y=yhat), alpha=.5)+
        labs(x=hName, y="Predicted")
      if(smooth){
        gDta[[ind]] <- gDta[[ind]] +
          geom_smooth(aes(x=var, y=yhat))
      }
    }
    
    # Replace the original colname
    colnames(object)[chIndx] <- hName
  }
  
  if(lng == 1) gDta <- gDta[[1]]
  else class(gDta) <- c("ggVariableList", class(gDta))
  
  return(gDta)
}
