####**********************************************************************
####**********************************************************************
####
####  GGRFSRC - GGPLOT2 GRAPHICS FOR RANDOM FORESTS FOR SURVIVAL, 
####  REGRESSION, AND CLASSIFICATION (RF-SRC)
####  Version 1.0.0
####
####  Copyright 2013, Cleveland Clinic Foundation
####
####  This program is free software; you can redistribute it and/or
####  modify it under the terms of the GNU General Public License
####  as published by the Free Software Foundation; either version 2
####  of the License, or (at your option) any later version.
####
####  This program is distributed in the hope that it will be useful,
####  but WITHOUT ANY WARRANTY; without even the implied warranty of
####  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
####  GNU General Public License for more details.
####
####  You should have received a copy of the GNU General Public
####  License along with this program; if not, write to the Free
####  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
####  Boston, MA  02110-1301, USA.
####
####  ----------------------------------------------------------------
####  Project Partially Funded By: 
####  ----------------------------------------------------------------
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
####    URL:    https://github.com/ehrlinger/ggrfsrc
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' plot.survival 
#'
#' @param x,
#' @param subset, 
#' @param collapse = FALSE,
#' @param haz.model = c("spline", "ggamma", "nonpar"),
#' @param k = 25,
#' @param span = "cv",
#' @param cens.model = c("km", "rfsrc"),
#' @param ...
#'
#' @export plot.survival.ggrfsrc plot.survival


plot.survival.ggrfsrc <- function (object,
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
  class(rtn) <- "ggrfsrc"
  invisible(rtn)
}


plot.survival <- plot.survival.ggrfsrc
