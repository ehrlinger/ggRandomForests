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
#' plot_survival
#' Plot survival curve from an RF-S object.  
#'
#' @param x An object of class (rfsrc, grow) or (rfsrc, predict).
#' @param subset Vector indicating which individuals we want estimates for. 
#'   All individuals are used if not specified.
#' @param collapse Collapse the survival and cumulative hazard function 
#'   across the individuals specified by subset? Only applies when subset
#'   is specified. (FALSE)
#' @param haz.model Method for estimating the hazard. See details below. 
#'   Applies only when subset is specified.
#' @param k The number of natural cubic spline knots used for estimating 
#'   the hazard function. Applies only when subset is specified.
#' @param span The fraction of the observations in the span of Friedman's 
#'   super-smoother used for estimating the hazard function. Applies only 
#'   when subset is specified.
#' @param cens.model Method for estimating the censoring distribution used 
#'   in the inverse probability of censoring weights (IPCW) for the Brier score:
#'   km: Uses the Kaplan-Meier estimator.
#'   rfscr: Uses random survival forests.
#' @param ... Further arguments passed to or from other methods.
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
#' @returns Invisibly, the conditional and unconditional Brier scores, and 
#' the integrated Brier score (if they are available).
#' 
#' @export plot_survival.ggRandomForests plot_survival
#' 
plot_survival.ggRandomForests <- function (object,
                                   prd.type=c("std", "oob"),
                                   srv.type=c("surv", "chf", "mortality", "hazard"),
                                   pnts = c("none", "kaplan", "nelson"),
                                   show.ind = FALSE,
                                   subset,
                                   strata,
                                   climits = .95, 
                                   error = c("none", "bars", "shade"),
                                   errbars,
                                   xlim, ylim,
                                   xlab, ylab,
                                   axisx, axisy,
                                   ...)
{ 
  
  ## Verify that the incoming object is of type rfsrc.
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 
        2 & sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, predict)'.")
  }
  
  # This is supposed to create a survival plot after all.
  if (object$family != "surv") {
    stop(paste("this function only supports right-censored survival settings. This is a ", object$family, " forest."))
  }
  call <- match.call()
  
  # Check the input arguments
  pnts <- match.arg(pnts)
  prd.type <- match.arg(prd.type)
  srv.type <- match.arg(srv.type)
  error <- match.arg(error)
  
  ## What type of prediction are we looking for (OOB or not).
  rf.srv <- switch(prd.type,
                   std=t(object$survival),
                   oob={
                     # In case of predict object without OOB data
                     if(is.null(object$survival.oob)){
                       t(object$survival) 
                     }else{
                       t(object$survival.oob) 
                     }})
  
  rf.chf  <- switch(prd.type,
                    std=t(object$chf),
                    oob={
                      # In case of predict object without OOB data
                      if(is.null(object$chf.oob)){
                        t(object$chf) 
                      }else{
                        t(object$chf.oob) 
                      }
                    })
  
  rf.data  <- switch(srv.type,
                     surv = rf.srv,
                     chf = rf.chf,
                     mortality = 1-rf.srv,
                     hazard = NA)
  
  # The mean survival time is the average of all predicted survival curves.
  
  # Get the survival information, getting the bootstrap CI at the correct measure.
  alph <- (1-climits)/2
  fll<-t(apply(rf.data,1, function(rw){quantile(rw, prob=c(alph, .5, 1-alph))}))
  colnames(fll) <- c("lower", "mean", "upper")
  fll <- as.data.frame(cbind(time=object$time.interest,fll))
  #
  
  # Now order matters, so we want to place the forest predictions on the bottom
  # Create the figure skeleton,
  plt<-ggplot(fll)
  
  # Do we want to show the rfs estimates for each individual, or a subset of individuals?
  if(show.ind | !missing(subset)){
    
    # Format ALL predictions for plotting.
    if(missing(subset)){
      srv<- as.data.frame(cbind(time=object$time.interest,rf.data))
    }else{
      srv<- as.data.frame(cbind(time=object$time.interest,rf.data[,subset]))
    }
    srv.m <- melt(srv, id="time")
    plt<-plt+geom_step(aes(x=time, y=value, by=variable), data=srv.m, alpha=.1)
  }
  
  # Do we want to show confidence limits?
  plt <- switch(error,
                # Shading the standard errors
                shade = plt + geom_ribbon(aes(x=time, ymax=upper, ymin=lower), data=fll, alpha=.1),
                # Or showing error bars
                bars = {
                  errFll <- fll
                  if(!missing(errbars) )errFll <- errFll[errbars,]
                  plt+ geom_errorbar(aes(x=time, ymax=upper, ymin=lower), data=errFll)
                },
                none=plt)
  
  # Finally plot the estimated curve.
  plt <- plt +  geom_step(aes(x=time, y=mean), data= fll, size=1.5) 
  pts.data <- as.data.frame(cbind(object$yvar,object$xvar))
  
  plt<- switch(pnts,
               none=plt,
               kaplan={
                 kp <- kaplan(object$yvar.names[1],object$yvar.names[2], data=pts.data)
                 kp <- kp[which(kp$cens==0),]
                 switch(srv.type,
                        surv=plt+ geom_point(aes(x=time, y=surv), data=kp),
                        chf=plt+ geom_point(aes(x=time, y=cum_haz), data=kp),
                        mortality=plt+ geom_point(aes(x=time, y=1-surv), data=kp),
                        hazard=plt+ geom_point(aes(x=time, y=hazard), data=kp)
                 )},
               nelson={
                 kp <- nelson(object$yvar.names[1],object$yvar.names[2], data=pts.data)
                 kp <- kp[which(kp$cens==0),]
                 switch(srv.type,
                        surv=plt+ geom_point(aes(x=time, y=surv), data=kp),
                        chf=plt+ geom_point(aes(x=time, y=cum_haz), data=kp),
                        mortality=plt+ geom_point(aes(x=time, y=1-surv), data=kp),
                        hazard=plt+ geom_point(aes(x=time, y=hazard), data=kp)
                 )
               }
  )
  show(plt)
  rtn<-list(call = call, graph=plt)
  class(rtn) <- "ggRandomForests"
  invisible(rtn)
}


plot_survival <- plot_survival.ggRandomForests
