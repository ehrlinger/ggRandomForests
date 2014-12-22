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
#' Reciever Operator Characteristic calculator
#'  
#' @details For a randomForestSRC prediction and the actual 
#' response value, calculate the specificity (1-False Positive Rate) and sensitivity 
#' (True Positive Rate) of a predictor.
#' 
#' This is a helper function for the \code{\link{gg_roc}} functions, and not intended 
#' for use by the end user.
#' 
#' @param object \code{randomForestSRC::rfsrc} or \code{randomForestSRC::predict} object 
#' containing predicted response
#' @param yvar True response variable
#' @param which.outcome If defined, only show ROC for this response. 
#' @param oob Use OOB estimates, the normal validation method (TRUE)
#'  
#' @return A \code{gg_roc} object
#'   
#' @aliases calc_roc.rfsrc calc_roc.randomForest calc_roc
#' 
#' @seealso \code{\link{calc_auc}} \code{\link{gg_roc}} \code{\link{plot.gg_roc}}
#' 
#' @importFrom parallel mclapply
#' 
#' @export calc_roc calc_roc.rfsrc
#' @examples
#' ## Taken from the gg_roc example
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' data(rfsrc_iris)
#' gg_dta <- calc_roc.rfsrc(rfsrc_iris, rfsrc_iris$yvar, which.outcome=1, oob=TRUE)
#' 
calc_roc.rfsrc <- function(object, yvar, which.outcome="all", oob=TRUE){
  if(!is.factor(yvar)) yvar <- factor(yvar)
  if(which.outcome!="all"){
    dta.roc <- data.frame(cbind(res=(yvar == levels(yvar)[which.outcome]), 
                                prd=object$predicted[, which.outcome],
                                oob=object$predicted.oob[, which.outcome]))
    if(oob)
      pct <- sort(unique(object$predicted.oob[,which.outcome]))
    else
      pct <- sort(unique(object$predicted[,which.outcome]))
  }else{
    stop("Must specify which.outcome for now.")
  }
  pct<- pct[-length(pct)]
  
  gg_dta <- mclapply(pct, function(crit){
    if(oob) 
      tbl <- xtabs(~res+(oob>crit), dta.roc)
    else
      tbl <- xtabs(~res+(prd>crit), dta.roc)
    
    spec<-tbl[2,2]/rowSums(tbl)[2]
    sens<-tbl[1,1]/rowSums(tbl)[1]
    cbind(sens=sens,spec=spec )
  })
  
  gg_dta <- do.call(rbind, gg_dta)
  gg_dta <- rbind(c(0,1), gg_dta, c(1,0))
  
  gg_dta <- data.frame(gg_dta, row.names=1:nrow(gg_dta))
  gg_dta$pct <- c(0,pct,1)
  invisible(gg_dta)
  
}

calc_roc<- calc_roc.rfsrc

calc_roc.randomForest <- function(object, dta, which.outcome=1){
  prd <- predict(object, type="prob")
  dta.roc <- data.frame(cbind(res=(dta == levels(dta)[which.outcome]), 
                              prd=prd[,which.outcome]))
  
  pct <- sort(unique(prd[,which.outcome]))
  pct<- pct[-length(pct)]
  
  gg_dta <-mclapply(pct, function(crit){
    tbl <- xtabs(~res+(prd>crit), dta.roc)
    
    spec<-tbl[2,2]/rowSums(tbl)[2]
    sens<-tbl[1,1]/rowSums(tbl)[1]
    cbind(sens=sens, spec=spec)
  })
  gg_dta <- data.frame(do.call(rbind, gg_dta))
  
  gg_dta$pct <- c(0,pct,1)
  
  invisible(gg_dta)
}

#'
#' Area Under the ROC Curve calculator
#' 
#' @details calc_auc uses the trapezoidal rule to calculate the area under
#' the ROC curve.
#' 
#'  This is a helper function for the \code{\link{gg_roc}} functions.
#'  
#' @param x \code{\link{gg_roc}} object
#' 
#' @return AUC. 50\% is random guessing, higher is better.
#' 
#' @importFrom dplyr lead
#' 
#' @seealso \code{\link{calc_roc}} \code{\link{gg_roc}} \code{\link{plot.gg_roc}}
#' 
#' @examples
#' ##
#' ## Taken from the gg_roc example
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' data(rfsrc_iris)
#' gg_dta <- gg_roc(rfsrc_iris, which.outcome=1)
#' 
#' calc_auc(gg_dta)
#' 
#' @export calc_auc
#' @aliases calc_auc calc_auc.gg_roc
#' 
calc_auc <- function(x){
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
calc_auc.gg_roc <- calc_auc