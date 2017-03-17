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
#' @param object \code{\link[randomForestSRC]{rfsrc}} or 
#' \code{\link[randomForestSRC]{predict.rfsrc}} object 
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
#' @importFrom stats xtabs
#' @importFrom utils head tail
#' 
#' @examples
#' ## Taken from the gg_roc example
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' data(rfsrc_iris)
#' gg_dta <- calc_roc.rfsrc(rfsrc_iris, rfsrc_iris$yvar, which.outcome=1, oob=TRUE)
#' gg_dta <- calc_roc.rfsrc(rfsrc_iris, rfsrc_iris$yvar, which.outcome=1, oob=FALSE)
#' @export

calc_roc.rfsrc <- function(object, yvar, which.outcome="all", oob=TRUE){
  if(!is.factor(yvar)) yvar <- factor(yvar)
  
  if(which.outcome != "all"){
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
  
  pct <- pct[-length(pct)]
  
  gg_dta <- parallel::mclapply(pct, function(crit){
    if(oob)
      tbl <- xtabs(~res + (oob > crit), dta.roc)
    else
      tbl <- xtabs(~res + (prd > crit), dta.roc)
    
    spec <- tbl[2,2] / rowSums(tbl)[2]
    sens <- tbl[1,1] / rowSums(tbl)[1]
    cbind(sens = sens,spec = spec )
  })
  
  gg_dta <- do.call(rbind, gg_dta)
  gg_dta <- rbind(c(0,1), gg_dta, c(1,0))
  
  gg_dta <- data.frame(gg_dta, row.names=1:nrow(gg_dta))
  gg_dta$pct <- c(0,pct,1)
  invisible(gg_dta)
}

calc_roc <- calc_roc.rfsrc

## This is in development still.
## We'll return to this when we start the next version.
# calc_roc.randomForest <- function(object, dta, which.outcome=1){
#   prd <- predict(object, type="prob")
#   dta.roc <- data.frame(cbind(res=(dta == levels(dta)[which.outcome]), 
#                               prd=prd[,which.outcome]))
#   
#   pct <- sort(unique(prd[,which.outcome]))
#   pct<- pct[-length(pct)]
#   
#   gg_dta <- parallel::mclapply(pct, function(crit){
#     tbl <- xtabs(~res+(prd>crit), dta.roc)
#     
#     spec<-tbl[2,2]/rowSums(tbl)[2]
#     sens<-tbl[1,1]/rowSums(tbl)[1]
#     cbind(sens=sens, spec=spec)
#   })
#   gg_dta <- data.frame(do.call(rbind, gg_dta))
#   
#   gg_dta$pct <- c(0,pct,1)
#   
#   invisible(gg_dta)
# }

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
# @importFrom dplyr lead
#' 
#' @seealso \code{\link{calc_roc}} \code{\link{gg_roc}} \code{\link{plot.gg_roc}}
#' 
#' @examples
#' ##
#' ## Taken from the gg_roc example
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' data(rfsrc_iris)
#' 
#' \dontrun{
#' gg_dta <- gg_roc(rfsrc_iris, which.outcome=1)
#' 
#' calc_auc(gg_dta)
#' }
#' 
#' gg_dta <- gg_roc(rfsrc_iris, which.outcome=2)
#' 
#' calc_auc(gg_dta)
#' 
#' @aliases calc_auc calc_auc.gg_roc
#' @export
calc_auc <- function(x){
  ## Use the trapeziod rule, basically calc
  ##
  ## auc = dx/2(f(x_{i+1}) - f(x_i))
  ##
  ## f(x) is sensitivity, x is 1-specificity
  
  # SInce we are leading vectors (x_{i+1} - x_{i}), we need to
  # ensure we are in decreasing order of specificity (x var = 1-spec)
  x <- x[order(x$spec, decreasing=TRUE),]
  
  auc <- (3 * shift(x$sens) - x$sens) / 2 * (x$spec - shift(x$spec))
  sum(auc, na.rm=TRUE)
}
calc_auc.gg_roc <- calc_auc

#' lead function to shift by one (or more).
#' 
#' @param x a vector of values
#' @param shift_by an integer of length 1, giving the number of positions 
#' to lead (positive) or lag (negative) by
#' 
#' @details Lead and lag are useful for comparing values offset by a constant 
#' (e.g. the previous or next value)
#' 
#' Taken from:
#' http://ctszkin.com/2012/03/11/generating-a-laglead-variables/
#' 
#' This function allows me to remove the dplyr::lead depends. Still suggest for vignettes though.
#' 
#' @examples
#' d<-data.frame(x=1:15) 
#' #generate lead variable
#' d$df_lead2<-ggRandomForests:::shift(d$x,2)
#' #generate lag variable
#' d$df_lag2<-ggRandomForests:::shift(d$x,-2)
# 
# > d
# x df_lead2 df_lag2
# 1   1        3      NA
# 2   2        4      NA
# 3   3        5       1
# 4   4        6       2
# 5   5        7       3
# 6   6        8       4
# 7   7        9       5
# 8   8       10       6
# 9   9       NA       7
# 10 10       NA       8
# 
# # shift_by is vectorized
# d$df_lead2 shift(d$x,-2:2)
# [,1] [,2] [,3] [,4] [,5]
# [1,]   NA   NA    1    2    3
# [2,]   NA    1    2    3    4
# [3,]    1    2    3    4    5
# [4,]    2    3    4    5    6
# [5,]    3    4    5    6    7
# [6,]    4    5    6    7    8
# [7,]    5    6    7    8    9
# [8,]    6    7    8    9   10
# [9,]    7    8    9   10   NA
# [10,]    8    9   10   NA   NA

shift <- function(x,shift_by=1){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by) > 1)
    return(sapply(shift_by,shift, x=x))
  
  out <- NULL
  abs_shift_by <- abs(shift_by)
  if (shift_by > 0 )
    out <- c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out <- c(rep(NA, abs_shift_by), head(x,-abs_shift_by))
  else
    out <- x
  out
}