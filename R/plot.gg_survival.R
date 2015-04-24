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
#' Plot a \code{\link{gg_survival}}  object. 
#' 
#' @param x \code{\link{gg_survival}} or a survival \code{\link{gg_rfsrc}} object created from a 
#' \code{\link[randomForestSRC]{rfsrc}} object
#' 
#' @param error "shade", "bars", "lines" or "none"
#' @param type "surv", "cum_haz","hazard","density","mid_int", "life","proplife"
#' @param ... not used
#'  
#' @return \code{ggplot} object
#' 
#' @examples
#' \dontrun{
#' ## -------- pbc data
#' data(pbc, package="randomForestSRC")
#' pbc$time <- pbc$days/364.25
#' 
#' # This is the same as kaplan
#' gg_dta <- gg_survival(interval="time", censor="status", 
#'                      data=pbc)
#'                      
#' plot(gg_dta, error="none")
#' plot(gg_dta)
#' 
#' # Stratified on treatment variable.
#' gg_dta <- gg_survival(interval="time", censor="status", 
#'                      data=pbc, by="treatment")
#'                      
#' plot(gg_dta, error="none")
#' plot(gg_dta)
#' 
#' # ...with smaller confidence limits.
#' gg_dta <- gg_survival(interval="time", censor="status", 
#'                      data=pbc, by="treatment", conf.int=.68)
#'                      
#' plot(gg_dta, error="lines")
#' 
#'}
#'
#' @importFrom ggplot2 ggplot geom_ribbon aes_string geom_errorbar geom_step labs
#' @export
### Survival plots
plot.gg_survival <- function(x, 
                            type=c("surv", "cum_haz","hazard","density","mid_int", "life","proplife"),
                            error=c("shade","bars","lines", "none"),
                            ...){
  gg_dta <- x
  if(inherits(gg_dta, "rfsrc")) gg_dta <- gg_survival(gg_dta)
  
  error <- match.arg(error)
  type <- match.arg(type)
  
  # Now order matters, so we want to place the forest predictions on the bottom
  # Create the figure skeleton,
  
  
  if(is.null(gg_dta$groups)){
    gg_plt <- ggplot(gg_dta) +
      geom_step(aes_string(x="time", y=type), ...)
  }else{
    gg_dta$groups <- factor(gg_dta$groups)
    gg_plt <- ggplot(gg_dta) +
      geom_step(aes_string(x="time", y=type, color="groups"), ...)
  }
  # Do we want to show confidence limits?
  if(type == "surv"){
    if(is.null(gg_dta$groups)){
      gg_plt <- switch(error,
                       # Shading the standard errors
                       shade = gg_plt + 
                         geom_ribbon(aes_string(x="time", ymax="upper", ymin="lower"),
                                     alpha=.3),
                       # Or showing error bars
                       bars = {
                         # Need to figure out how to remove some of these points when 
                         # requesting error bars, or this will get really messy.
                         #                     errFll <- fll
                         #                     if(!missing(errbars) )errFll <- errFll[errbars,]
                         gg_plt + 
                           geom_errorbar(aes_string(x="time", ymax="upper", ymin="lower"))
                       },
                       lines= gg_plt + 
                         geom_step(aes_string(x="time", y="upper"), linetype=2) +
                         geom_step(aes_string(x="time", y="lower"), linetype=2), 
                       none=gg_plt)
    }else{
      gg_plt <- switch(error,
                       # Shading the standard errors
                       shade = gg_plt + 
                         geom_ribbon(aes_string(x="time", ymax="upper", ymin="lower", 
                                                fill="groups", color="groups"),
                                     alpha=.3),
                       # Or showing error bars
                       bars = {
                         # Need to figure out how to remove some of these points when 
                         # requesting error bars, or this will get really messy.
                         #                     errFll <- fll
                         #                     if(!missing(errbars) )errFll <- errFll[errbars,]
                         gg_plt+ 
                           geom_errorbar(aes_string(x="time", ymax="upper", ymin="lower", color="groups"))
                       },
                       lines= gg_plt + 
                         geom_step(aes_string(x="time", y="upper", color="groups"), linetype=2) +
                         geom_step(aes_string(x="time", y="lower", color="groups"), linetype=2), 
                       none=gg_plt)
    }
  }
  
  return(gg_plt)
}
