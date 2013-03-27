###################################################################################
#' 
#' lifetable creates a nonparametric survival estimate using either the kaplan-meier or nelson-aalon '
#' method.
#' 
#' @param data name of the training data.frame
#' @param interval name of the interval variable in the training dataset.
#' @param censor name of the censoring variable in the training dataset.
#' @param strat stratifying variable in the training dataset, defaults to NULL
#' @param type one of ("kaplan","nelson"), defaults to kaplan-meier
#' @param ... extra arguments passed to...
#' 
#' @export lifetable
#' 
#' 


lifetable <- function(interval, censor, strat=NULL, data, type=c("kaplan","nelson"), ...){
  type <- match.arg(type)
  
  ltab <- switch(type,
    kaplan=lifetable.kaplan(interval=interval, censor=censor, strat=strat, data=data, ...),
    nelson=lifetable.nelson(interval=interval, censor=censor, strat=strat, data=data, ...)
  )
  return(ltab)
}
