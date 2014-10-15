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
#' @details eventtable is a wrapper function for both generating nonparametric 
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
#' @seealso \code{\link{kaplan}} \code{\link{nelson}} \code{\link{gg_survival}}
#' @export eventtable
#' 
eventtable <- function(interval, censor, strat=NULL, 
                       data, 
                       type=c("kaplan","nelson"), 
                       ...){
  type <- match.arg(type)
  
  ltab <- switch(type,
    kaplan=kaplan(interval=interval, censor=censor, strat=strat, data=data, ...),
    nelson=nelson(interval=interval, censor=censor, strat=strat, data=data, ...)
  )
  
  return(ltab)
}
