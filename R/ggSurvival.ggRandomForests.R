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
#' ggSurvival
#' Plot survival curve from an RF-S object.  
#'
#' @param rfObject An object of class (rfsrc, grow) or (rfsrc, predict).
#' @param subset Vector indicating which individuals we want estimates for. 
#'   All individuals are used if not specified.
#' @param prd.type
#' @param srv.type
#' @param pnts
#' @param show.ind
#' @param strata 
#' @param climits 
#' @param error 
#' @param errbars 
#' @param curve 
#' @param ... Further arguments passed to other methods.
#'
#' @details If subset is not specified, generates the following three plots
#'  (going from top to bottom, left to right):
#' 
#' Forest estimated survival function for each individual (thick red line is overall ensemble survival, thick green line is Nelson-Aalen estimator).
#' 
#' Brier score (0=perfect, 1=poor, and 0.25=guessing) stratified by ensemble mortality. Based on the IPCW method described in Gerds et al. (2006). Stratification is into 4 groups corresponding to the 0-25, 25-50, 50-75 and 75-100 percentile values of mortality. Red line is the overall (non-stratified) Brier score.
#' 
#' Plot of mortality of each individual versus observed time. Points in blue correspond to events, black points are censored observations.
#' 
#' When subset is specified, then for each individual in subset, the following three plots are generated:
#' Forest estimated survival function.
#' Forest estimated cumulative hazard function (CHF) (displayed using black lines). Blue lines are the CHF from the estimated hazard function. See the next item.
#' 
#' A smoothed hazard function derived from the forest estimated CHF (or survival function). The default method, haz.model="spline", models the log CHF using natural cubic splines as described in Royston and Parmar (2002). The lasso is used for model selection, implemented using the glmnet package (this package must be installed for this option to work). If haz.model="ggamma", a three-parameter generalized gamma distribution (using the parameterization described in Cox et al, 2007) is fit to the smoothed forest survival function, where smoothing is imposed using Friedman's supersmoother (implemented by supsmu). If haz.model="nonpar", Friedman's supersmoother is applied to the forest estimated hazard function (obtained by taking the crude derivative of the smoothed forest CHF). Finally, setting haz.model="none" suppresses hazard estimation and no hazard estimate is provided.
#' 
#' At this time, please note that all hazard estimates are considered experimental and users should interpret the results with caution.
#' 
#' Note that when the object x is of class (rfsrc, predict) not all plots will be produced. In particular, Brier scores are not calculated.
#'  
#' Only applies to survival families. In particular, fails for competing risk analyses. Use plot.competing.risk in such cases. 
#' 
#' Whenever possible, out-of-bag (OOB) values are used.
#' 
#' @return Invisibly, the conditional and unconditional Brier scores, and 
#' the integrated Brier score (if they are available).
#' 
#' @export ggSurvival.ggRandomForests ggSurvival
#' 
#' @aliases ggSurvival
#' 
#' @importFrom dplyr tbl_df
#' 
ggSurvival.ggRandomForests <- function (rfObject,
                                        prd.type=c("std", "oob"),
                                        srv.type=c("surv", "chf", "mortality", "hazard"),
                                        pnts = c("none", "kaplan", "nelson"),
                                        show.ind = NULL,
                                        subset,
                                        strata,
                                        climits = .95, 
                                        error = c("none", "bars", "shade", "lines"),
                                        errbars,
                                        curve=c("mean", "median", "both"),
                                        ...)
{ 
  
  ## Verify that the incoming object is of type rfsrc.
  if (sum(inherits(rfObject, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 
        2 & sum(inherits(rfObject, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for RF objects from randomForestSRC.")
  }
  
  # This is supposed to create a survival plot after all.
  if (rfObject$family != "surv") {
    stop(paste("This function only supports Random Forests for survival. This is a ", rfObject$family, " forest."))
  }
  call <- match.call()
  
  # Check the input arguments
  pnts <- match.arg(pnts)
  prd.type <- match.arg(prd.type)
  srv.type <- match.arg(srv.type)
  error <- match.arg(error)
  curve <- match.arg(curve)
  
  ## What type of prediction are we looking for (OOB or not).
  rf.srv <- switch(prd.type,
                   std=rfObject$survival,
                   oob={
                     # In case of predict object without OOB data
                     if(is.null(rfObject$survival.oob)){
                       rfObject$survival 
                     }else{
                       rfObject$survival.oob
                     }})
  
  rf.chf  <- switch(prd.type,
                    std=rfObject$chf,
                    oob={
                      # In case of predict object without OOB data
                      if(is.null(rfObject$chf.oob)){
                        rfObject$chf 
                      }else{
                        rfObject$chf.oob
                      }
                    })
  
  rf.data  <- switch(srv.type,
                     surv = rf.srv,
                     chf = rf.chf,
                     mortality = 1-rf.srv,
                     hazard = NA)
  
  # Get the survival information, getting the bootstrap CI at the correct measure.
  alph <- (1-climits)/2
  
  # Calc all the quantiles required (dplyr?)
  fll<-t(apply(rf.data,2, function(rw){quantile(rw, prob=c(alph, .5, 1-alph))}))
  
  colnames(fll) <- c("lower", "median", "upper")
  fll <- data.frame(cbind(time=rfObject$time.interest,fll, mean=colMeans(rf.data)))
  
  fll <- tbl_df(fll)
  class(fll)  <- c("ggSurvival",class(fll))
  invisible(fll)
}


ggSurvival <- ggSurvival.ggRandomForests
