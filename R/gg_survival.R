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
#' @details \code{gg_survival} is a wrapper function for generating nonparametric 
#' survival estimates using either \code{\link{nelson}}-aalen or 
#' \code{\link{kaplan}}-meier estimates.  
#' 
#' @param data name of the training data.frame
#' @param interval name of the interval variable in the training dataset.
#' @param censor name of the censoring variable in the training dataset.
#' @param by stratifying variable in the training dataset, defaults to NULL
#' @param type one of ("kaplan","nelson"), defaults to kaplan-meier
#' @param ... extra arguments passed to kaplan or nelson functions.
#' 
#' @return A \code{gg_survival} object created using the non-parametric kaplan-meier or 
#' nelson-aalon estimators.
#' 
#' @seealso \code{\link{kaplan}} \code{\link{nelson}} \code{\link{plot.gg_survival}}
#' 
#' @examples 
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
#' @export
gg_survival <- function(interval, censor, by=NULL, 
                                        data, 
                                        type=c("kaplan","nelson"), 
                                        ...){
  type <- match.arg(type)
  
  gg_dta <- switch(type,
                   kaplan=kaplan(interval=interval, censor=censor, by=by, data=data, ...),
                   nelson=nelson(interval=interval, censor=censor, by=by, data=data, ...)
  )
  
  return(gg_dta)
}
