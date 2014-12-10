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
#' 
#' Nonparametric survival estimates.
#' 
#' @details gg_survival is a wrapper function for both generating nonparametric 
#' survival estimates using either nelson-aalen or kaplan-meier estimates.  
#' 
#' @param data name of the training data.frame
#' @param interval name of the interval variable in the training dataset.
#' @param censor name of the censoring variable in the training dataset.
#' @param strat stratifying variable in the training dataset, defaults to NULL
#' @param type one of ("kaplan","nelson"), defaults to kaplan-meier
#' @param ... extra arguments passed to kaplan or nelson functions.
#' 
#' @return A gg_survival object created using the non-parametric kaplan-meier or 
#' nelson-aalon estimators.
#' 
#' @seealso \code{\link{kaplan}} \code{\link{nelson}} \code{\link{plot.gg_survival}}
#' @export gg_survival gg_survival.ggRandomForests
#' @aliases gg_survival
#' 
#' @examples 
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
#'                      data=pbc, strat="treatment")
#'                      
#' plot(gg_dta, error="none")
#' plot(gg_dta)
#' 
#' # ...with smaller confidence limits.
#' gg_dta <- gg_survival(interval="time", censor="status", 
#'                      data=pbc, strat="treatment", conf.int=.68)
#'                      
#' plot(gg_dta, error="lines")
#' 
#'
gg_survival.ggRandomForests <- function(interval, censor, strat=NULL, 
                                        data, 
                                        type=c("kaplan","nelson"), 
                                        ...){
  type <- match.arg(type)
  
  gg_dta <- switch(type,
                   kaplan=kaplan(interval=interval, censor=censor, strat=strat, data=data, ...),
                   nelson=nelson(interval=interval, censor=censor, strat=strat, data=data, ...)
  )
  
  return(gg_dta)
}


gg_survival <- gg_survival.ggRandomForests
