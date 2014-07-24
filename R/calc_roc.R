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
#' @title calcROC Reciever Operator Characteristic calculator for randomForest objects
#' 
#' @description Given the randomForest or randomForestSRC prediction and the actual 
#' response value, calculate the specificty (1-False Positive Rate) and sensitivity 
#' (True Positive Rate) of a predictor.
#' 
#' @param rf randomForest or prediction object containing predicted response
#' @param dta True response variable
#' @param which.outcome If defined, only show ROC for this response. 
#' @param oob Use OOB estimates, the normal validation method (TRUE)
#'  
#' @aliases calcROC.rfsrc calcROC.randomForest calcROC
#' 
#' @importFrom parallel mclapply
#' 
#' @examples
#' \dontrun{
#' ##
#' ## Taken from the gg_roc example
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' roc <- calcROC.rfsrc(iris.obj, iris.obj$yvar, which.outcome=1, oob=TRUE)
#' }
calcROC.rfsrc <- function(rf, dta, which.outcome=1, oob=TRUE){
  if(!is.factor(dta)) dta <- factor(dta)
  dta.roc <- data.frame(cbind(res=(dta == levels(dta)[which.outcome]), 
                              prd=rf$predicted[, which.outcome],
                              oob=rf$predicted.oob[, which.outcome]))
  if(oob)
    pct <- sort(unique(rf$predicted.oob[,which.outcome]))
  else
    pct <- sort(unique(rf$predicted[,which.outcome]))
  
  pct<- pct[-length(pct)]
  
  spc <-mclapply(pct, function(crit){
    if(oob) 
      tbl <- xtabs(~res+(oob>crit), dta.roc)
    else
      tbl <- xtabs(~res+(prd>crit), dta.roc)
    
    spec<-tbl[2,2]/rowSums(tbl)[2]
    sens<-tbl[1,1]/rowSums(tbl)[1]
    cbind(sens=sens,spec=spec )
  })
  
  spc <- do.call(rbind, spc)
  spc <- rbind(c(0,1), spc, c(1,0))
  pct<- c("origin",pct,"limit")
  return(data.frame(spc, row.names=pct))
  
}
calcROC<- calcROC.rfsrc
calcROC.randomForest <- function(rf, dta, which.outcome=1){
  prd <- predict(rf, type="prob")
  dta.roc <- data.frame(cbind(res=(dta == levels(dta)[which.outcome]), 
                              prd=prd[,which.outcome]))
  
  pct <- sort(unique(prd[,which.outcome]))
  pct<- pct[-length(pct)]
  
  spc <-mclapply(pct, function(crit){
    tbl <- xtabs(~res+(prd>crit), dta.roc)
    
    spec<-tbl[2,2]/rowSums(tbl)[2]
    sens<-tbl[1,1]/rowSums(tbl)[1]
    cbind(sens=sens, spec=spec)
  })
  spc <- do.call(rbind, spc)
  
  return(data.frame(spc, row.names=pct))
}

#'
#' @title calcAUC.ggRandomForests calculate the Area Under the ROC Curve
#' 
#' @description calcAUC uses the trapezoidal rule to calculate the area under
#' the ROC curve.
#' 
#' @details sensitivity and specificity 
#' 
#' @param x output from calcROC (or ggROC) 
#' 
#' @return AUC. 50% is random guessing, higher is better.
#' 
calcAUC.ggRandomForests <- function(x){
  ## Use the trapeziod rule, basically calc
  ##
  ## auc = dx/2(f(x_{i+1}) - f(x_i))
  ##
  ## f(x) is sensitivity, x is 1-specificity
  
  # SInce we are leading vectors (x_{i+1} - x_{i}), we need to
  # ensure we are in decreasing order of specificity (x var = 1-spec)
  x <- x[order(x$spec, decreasing=TRUE),]
  
  auc <- (3*lead(x$sens) - x$sens)/2 * (x$spec - lead(x$spec))
  sum(auc, na.rm=TRUE)
}
calcAUC<- calcAUC.ggRandomForests