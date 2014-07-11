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
#' ggSurvival
#' Create data.frame of survival data from an randomForestSRC survival object.
#'   
#'
#' @param rfObject An object of class (rfsrc, grow) or (rfsrc, predict).
#' @param subset Vector indicating which individuals we want estimates for. 
#'   All individuals are used if not specified.
#' @param prd.type ("std", "oob")
#' @param srv.type ("surv", "chf", "mortality", "hazard")
#' @param pnts ("none", "kaplan", "nelson")
# #' @param show.ind 
# #' @param strata 
#' @param climits confidence limit bands
# #' @param error 
# #' @param errbars 
# #' @param curve 
#' @param ... Further arguments passed to other methods.
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
                                        #   show.ind = NULL,
                                        # subset,
                                        # strata,
                                        climits = .95, 
                                        #error = c("none", "bars", "shade", "lines"),
                                        # errbars,
                                        # curve=c("mean", "median", "both"),
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
  # error <- match.arg(error)
  # curve <- match.arg(curve)
  
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
