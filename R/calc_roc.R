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
### plot.roc
##
## spc<- calcROC(rf, dta$sten_grp, which.outcome=3)
## ggplot(data=spc)+geom_line(aes(x=(1-sens), y=spec))+theme_bw()+geom_abline(a=1, b=0)
##
## mstn<- calcROC(rf, dta$sten_grp, which.outcome=2)
## ggplot(data=stn)+geom_line(aes(x=(1-sens), y=spec))+theme_bw()+geom_abline(a=1, b=0)+
##   geom_line(aes(x=(1-sens), y=spec), data=mstn,col="red")
##
## nstn<- calcROC(rf, dta$sten_grp, which.outcome=1)
## ggplot(data=stn)+geom_line(aes(x=(1-sens), y=spec))+theme_bw()+geom_abline(a=1, b=0)+
##   geom_line(aes(x=(1-sens), y=spec), data=mstn,col="red") + 
##   geom_line(aes(x=(1-sens), y=spec), data=nstn, col="blue")

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
#' @details Basically integrate the area using the trapezoidal rule.
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
  
  #   auc <- sapply(2:dim(x)[1], function(ind){
  #     dx <- x$sens[ind]-x$sens[ind-1]
  #     (x$spec[ind] + x$spec[ind-1])*dx/2
  #   }) 
  #   
  auc <- (x$sens - lag(x$sens))/2 * (x$spec+ lag(x$spec))
  sum(auc, na.rm=TRUE)
}
calcAUC<- calcAUC.ggRandomForests