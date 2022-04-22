#' logit_loess takes 
#' 
#' @param gg_dta dataset contains a yhat to smooth
#' @param xvar name of x variable to smooth along
#' @param level quantile level argument for \code{\link{qnorm}} function.
#'
#'
#'
#'
#' @importFrom stats formula loess predict qnorm
#'
#'
logit_loess <- function(gg_dta, xvar, level){
  # For survival, we have to apply a transform.
  
  # Are any values greater than 1? Probably on a 0-100 scale.
  scl <- sum(gg_dta$yhat > 1) < 1
  
  if(!scl){
    gg_dta$yhat <- gg_dta$yhat/100
  } 
  gg_dta$yhat[which(1-gg_dta$yhat <= .Machine$double.eps)] <- 1-2* .Machine$double.eps
  #transform the response to the logit domain...
  gg_dta$yhat <- log(gg_dta$yhat/(1-gg_dta$yhat))
  
  # print(colnames(gg_dta))
  # Then fit a loess
  
  frm <- formula(paste("yhat~", xvar))
  lfit <- loess(frm, gg_dta)
  
  pts <- predict(lfit, newdata=cbind(gg_dta[,xvar]), se = TRUE)
  
  # IF we have a custom confidence limit, stat_smooth accepts level.
 
  if(is.null(level)) level <- 0.95
  
  level <- (1-level)/2
  se_upp <- pts$fit + qnorm(level)*pts$se.fit
  se_low <- pts$fit - qnorm(level)*pts$se.fit
 
  se_upp <- exp(se_upp)/(1+exp(se_upp))
  se_low <- exp(se_low)/(1+exp(se_low))
  fit <- exp(pts$fit)/(1+exp(pts$fit))
  
  if(!scl){
    fit <- fit * 100
    se_upp <- se_upp*100
    se_low <- se_low*100
  } 
  
  data.frame(x=gg_dta[,xvar], y=fit, upper=se_upp, lower=se_low)
}