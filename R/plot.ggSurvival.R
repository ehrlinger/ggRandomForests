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
#' plot.ggSurvival
#' Plot a \code{\link{ggSurvival}} object, 
#' 
#' @param x ggSurvival object created from a randomForestSRC object
#' @param type Curve family defaults "surv", other possibilities "cum_haz",
#' "hazard","density","life","proplife".
#' @param error "shade", "bars" or "lines"
#' @param ... not used
#'  
#' @return ggplot object
#' 
#' @export plot.ggSurvival
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, 
#' Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, 
#' Regression and Classification (RF-SRC), R package version 1.4.
#' 
#' @examples
#' 
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRCM")
#' v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' ggrf.obj <- ggSurvival(v.obj)
#' plot(ggrf.obj)
#'
### Survival plots
plot.ggSurvival<- function(x, 
                           type=c("surv", "cum_haz","hazard","density","life","proplife"),
                           error=c("shade","bars","lines"),
                           ...){
  object <- x
  if(inherits(object, "rfsrc")) object<-ggSurvival(object)
  # Now order matters, so we want to place the forest predictions on the bottom
  # Create the figure skeleton,
  plt<-ggplot(object)
  
  
  if(type=="surv"){
    # Do we want to show confidence limits?
    plt <- switch(error,
                  # Shading the standard errors
                  shade = plt + geom_ribbon(aes(x=time, ymax=100*upper_cl, ymin=100*lower_cl), alpha=.1),
                  # Or showing error bars
                  bars = {
                    errFll <- fll
                    if(!missing(errbars) )errFll <- errFll[errbars,]
                    plt+ geom_errorbar(aes(x=time, ymax=100*upper_cl, ymin=100*lower_cl))
                  },
                  lines= plt + geom_step(aes(x=time, y=100*upper_cl), linetype=2)+
                    geom_step(aes(x=time, y=100*lower_cl), linetype=2), 
                  none=plt)
  }
  plt<- switch(type,
               surv= plt + geom_step(aes(x=time, y=100*surv)),
               cum_haz=   plt + geom_step(aes(x=time, y=cum_haz)),
               hazard=  plt + geom_step(aes(x=time, y=hazard)),
               density=  plt + geom_step(aes(x=time, y=density)),
               life=  + geom_step(aes(x=time, y=life)),
               proplife=  plt + geom_step(aes(x=time, y=proplife))
  )
  
  #   
  #   plt<- switch(pnts,
  #                none=plt,
  #                kaplan={
  #                  kp <- kaplan(rfObject$yvar.names[1],rfObject$yvar.names[2], data=pts.data)
  #                  kp <- kp[which(kp$cens==0),]
  #                  switch(srv.type,
  #                         surv=plt+ geom_point(aes(x=time, y=surv), data=kp),
  #                         chf=plt+ geom_point(aes(x=time, y=cum_haz), data=kp),
  #                         mortality=plt+ geom_point(aes(x=time, y=1-surv), data=kp),
  #                         hazard=plt+ geom_point(aes(x=time, y=hazard), data=kp)
  #                  )},
  #                nelson={
  #                  kp <- nelson(rfObject$yvar.names[1],rfObject$yvar.names[2], data=pts.data)
  #                  kp <- kp[which(kp$cens==0),]
  #                  switch(srv.type,
  #                         surv=plt+ geom_point(aes(x=time, y=surv), data=kp),
  #                         chf=plt+ geom_point(aes(x=time, y=cum_haz), data=kp),
  #                         mortality=plt+ geom_point(aes(x=time, y=1-surv), data=kp),
  #                         hazard=plt+ geom_point(aes(x=time, y=hazard), data=kp)
  #                  )
  #                }
  #   )
  #   
  return(plt+
           labs(y=type))
}
