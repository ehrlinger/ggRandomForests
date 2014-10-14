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
#' Plot a \code{\link{gg_survival}} or a survival \code{\link{gg_rfsrc}} object. 
#' 
#' @param x code{\link{gg_survival}} or a survival \code{\link{gg_rfsrc}} object created from a 
#' \code{randomForestSRC::rfsrc} object
#' 
#' @param error "shade", "bars" or "lines"
#' @param ... not used
#'  
#' @return \code{ggplot} object
#' 
#' @export plot.gg_survival
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
#' \dontrun{
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' #data(veteran, package = "randomForestSRCM")
#' #veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' ggrf.obj <- gg_survival(veteran_rf)
#' plot(ggrf.obj)
#'}
#'
#' @importFrom ggplot2 ggplot geom_ribbon aes_string geom_errorbar geom_step labs
#' 
### Survival plots
plot.gg_survival<- function(x, 
                            #  type=c("surv", "cum_haz","hazard","density","life","proplife"),
                            error=c("shade","bars","lines"),
                            ...){
  object <- x
  if(inherits(object, "rfsrc")) object<-gg_survival(object)
  
  error <- match.arg(error)
  
  # Now order matters, so we want to place the forest predictions on the bottom
  # Create the figure skeleton,
  plt<-ggplot(object)+
    geom_step(aes_string(x="time", y="mean"))
  
  # Do we want to show confidence limits?
  plt <- switch(error,
                # Shading the standard errors
                shade = plt + 
                  geom_ribbon(aes_string(x="time", ymax="upper", ymin="lower"),
                              alpha=.1),
                # Or showing error bars
                bars = {
                  # Need to figure out how to remove some of these points when 
                  # requesting error bars, or this will get really messy.
                  #                     errFll <- fll
                  #                     if(!missing(errbars) )errFll <- errFll[errbars,]
                  plt+ 
                    geom_errorbar(aes_string(x="time", ymax="upper", ymin="lower"))
                },
                lines= plt + 
                  geom_step(aes_string(x="time", y="upper"), linetype=2)+
                  geom_step(aes(x="time", y="lower"), linetype=2), 
                none=plt)
  
  return(plt)
}
