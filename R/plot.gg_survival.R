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
#' plot.gg_survival
#' Plot a \code{\link{gg_survival}} object, 
#' 
#' @param x gg_survival object created from a randomForestSRC object
#' @param type Curve family defaults "surv", other possibilities "cum_haz",
#' "hazard","density","life","proplife".
#' @param error "shade", "bars" or "lines"
#' @param ... not used
#'  
#' @return ggplot object
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
#' data(veteran, package = "randomForestSRCM")
#' v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' ggrf.obj <- gg_survival(v.obj)
#' plot(ggrf.obj)
#'}
#'
#' @importFrom ggplot2 ggplot geom_ribbon aes_string geom_errorbar geom_step labs
#' 
### Survival plots
plot.gg_survival<- function(x, 
                           type=c("surv", "cum_haz","hazard","density","life","proplife"),
                           error=c("shade","bars","lines"),
                           ...){
  object <- x
  if(inherits(object, "rfsrc")) object<-gg_survival(object)
  # Now order matters, so we want to place the forest predictions on the bottom
  # Create the figure skeleton,
  plt<-ggplot(object)
  
  
  if(type=="surv"){
    # Do we want to show confidence limits?
    plt <- switch(error,
                  # Shading the standard errors
                  shade = plt + 
                    geom_ribbon(aes_string(x="time", ymax="upper_cl", ymin="lower_cl"),
                                alpha=.1),
                  # Or showing error bars
                  bars = {
                    # Need to figure out how to remove some of these points when 
                    # requesting error bars, or this will get really messy.
#                     errFll <- fll
#                     if(!missing(errbars) )errFll <- errFll[errbars,]
                    plt+ 
                      geom_errorbar(aes_string(x="time", ymax="upper_cl", ymin="lower_cl"))
                  },
                  lines= plt + 
                    geom_step(aes_string(x="time", y="upper_cl"), linetype=2)+
                    geom_step(aes(x="time", y="lower_cl"), linetype=2), 
                  none=plt)
  }
  plt<- switch(type,
               surv= plt + geom_step(aes_string(x="time", y="surv")),
               cum_haz=   plt + geom_step(aes_string(x="time", y="cum_haz")),
               hazard=  plt + geom_step(aes_string(x="time", y="hazard")),
               density=  plt + geom_step(aes_string(x="time", y="density")),
               life=  + geom_step(aes_string(x="time", y="life")),
               proplife=  plt + geom_step(aes_string(x="time", y="proplife"))
  )
  
  return(plt+
           labs(y=type))
}
