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
#' Predicted survival data object from an \code{randomForestSRC::rfsrc} survival object.
#'
#' @param object An \code{randomForestSRC::rfsrc} object or \code{randomForestSRC::predict} predict, 
#' or a survival object from \code{\link{eventtable}}.
#' @param prd_type ("std", "oob")
#' @param srv_type ("surv", "chf", "mortality", "hazard")
# # @param pnts ("none", "kaplan", "nelson")
# # @param subset Vector indicating which individuals we want estimates for. 
# #   All individuals are used if not specified.
# # @param show.ind 
# # @param strata  
#' @param climits confidence limit bands
# # @param error 
# # @param errbars 
# # @param curve 
#' @param ... Further arguments passed to other methods.
#' 
#' @export gg_survival.ggRandomForests gg_survival
#' 
#' @seealso \code{\link{plot.gg_survival}} \code{\link{gg_rfsrc}} \code{randomForestSRC::rfsrc}
#' \code{randomForestSRC::predict} \code{\link{eventtable}}
#' 
#' @aliases gg_survival
#' 
#' @examples
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' #data(veteran, package = "randomForestSRCM")
#' #veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' data(veteran_rf, package="ggRandomForests")
#' 
#' ggrf.obj <- gg_survival(veteran_rf)
#' plot(ggrf.obj)
#' 
gg_survival.ggRandomForests <- function(object,
                                        prd_type=c("std", "oob"),
                                        srv_type=c("surv", "chf", "mortality", "hazard"),
                                        #pnts = c("none", "kaplan", "nelson"),
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
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 & 
        sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for RF objects from randomForestSRC.")
  }
  
  # This is supposed to create a survival plot after all.
  if (object$family != "surv") {
    stop(paste("This function only supports Random Forests for survival. This is a ", object$family, " forest."))
  }
  call <- match.call()
   
  # Check the input arguments
 # pnts <- match.arg(pnts)
  prd_type <- match.arg(prd_type)
  srv_type <- match.arg(srv_type)
  # error <- match.arg(error)
  # curve <- match.arg(curve)
  
  ## What type of prediction are we looking for (OOB or not).
  rf.srv <- switch(prd_type,
                   std=object$survival,
                   oob={
                     # In case of predict object without OOB data
                     if(is.null(object$survival.oob)){
                       object$survival 
                     }else{
                       object$survival.oob
                     }})
  
  rf.chf  <- switch(prd_type,
                    std=object$chf,
                    oob={
                      # In case of predict object without OOB data
                      if(is.null(object$chf.oob)){
                        object$chf 
                      }else{
                        object$chf.oob
                      }
                    })
  
  rf.data  <- switch(srv_type,
                     surv = rf.srv,
                     chf = rf.chf,
                     mortality = 1-rf.srv,
                     hazard = NA)
  
  # Get the survival information, getting the bootstrap CI at the correct measure.
  alph <- (1-climits)/2
  
  # Calc all the quantiles required (dplyr?)
  fll<-t(apply(rf.data,2, function(rw){quantile(rw, prob=c(alph, .5, 1-alph))}))
  
  colnames(fll) <- c("lower", "median", "upper")
  fll <- data.frame(cbind(time=object$time.interest,fll, mean=colMeans(rf.data)))
  
  class(fll)  <- c("gg_survival",class(fll))
  invisible(fll)
}


gg_survival <- gg_survival.ggRandomForests
