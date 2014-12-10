#' nonparametric Nelson-Aalen estimates
#'
#' @param data name of the survival training data.frame
#' @param interval name of the interval variable in the training dataset.
#' @param censor name of the censoring variable in the training dataset.
#' @param strat stratifying variable in the training dataset, defaults to NULL
#' @param weight for each observation (default=NULL)
#' @param ... arguments passed to the \code{survfit} function 
#'
#' @export nelson
#' @importFrom survival Surv survfit strata 
#' 
#' @seealso \code{\link{gg_survival}} \code{\link{nelson}} \code{\link{plot.gg_survival}}
#'
#' @examples 
#' \dontrun{
#' data(pbc, package="randomForestSRC")
#' pbc$time <- pbc$days/364.25
#' 
#' # This is the same as gg_survival
#' gg_dta <- nelson(interval="time", censor="status", 
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
#' }                                            
#' 
nelson <- function(interval, censor, data, strat=NULL, weight=NULL,...){
  call <- match.call()
  
  # Make sure we've speced the confidence limit correctly
  if(climit > 1) climit <- climit/100
  z <- qnorm(1-(1-climit)/2)
  
  # Set weighting for non-events to a value of 0
  # Set up weights (severity of event)                                           
  if(!is.null(weight)) weight <- data[,censor] * weight
  
  # Kaplan-Meier analysis
  
  # Kaplan-Meier analysis
  srv <- Surv(time=data[,interval], event=data[,censor])
  if(is.null(strat)){
    srvTab <- survfit(srv~1,data, ...)
  }else{
    srvTab <- survfit(srv~strata(data[,strat]),data, ...)
  }
  #
  # OR for stratification on 
  # srvTab <- survfit(Surv(interval, event)~strata(stratify),data, type=type)
  #cat(str(srvTab))
  #*******************************************************************************;
  #* Cumulative hazard and hazard estimates from transforms and slopes            ;
  #* as well as integral of survivorship and proportionate life length            ;
  hazard <- srvTab$n.event / srvTab$n.risk
  cumHazard <- vector()
  for(i in 1:length(hazard)) 
    cumHazard[i] <- sum(hazard[1:i])
  cumHazard <- c(cumHazard, cumHazard[length(cumHazard)])
  cumHazard <- -log(srvTab$surv)
  lCumHaz <- log(cumHazard)
  lInterval <- log(data[,interval])
  times <- order(data[,interval])
  deltaTime <- sapply(2:length(times), function(ind){times[ind] - times[ind-1] })
  
  # Still need to add hazard and density.
  tbl <-data.frame(cbind(time=srvTab$time,  n=srvTab$n.risk,
                         cens=srvTab$n.censor, dead=srvTab$n.event, 
                         surv=srvTab$surv, se=srvTab$std.err, lower=srvTab$lower, upper=srvTab$upper,
                         cum_haz=cumHazard) )
  
  # Add group labels when stratifying data.
  if(!is.null(strat)){
    tm_splits <- which(c(FALSE,sapply(2:nrow(tbl), function(ind){tbl$time[ind] < tbl$time[ind-1]})))
    
    lbls <- unique(data[,strat])
    tbl$groups <- lbls[1]
    
    for(ind in 2:(length(tm_splits)+1)){
      tbl$groups[tm_splits[ind-1]:nrow(tbl)] <- lbls[ind]
    }
  }
  
  #, "hazard", "density")            
  #*******************************************************************************;
  # Summarize the various strata
  # only look at events
  gg_dta <- tbl[which(tbl[,"dead"]!= 0),]
  
  # Calculate the hazard estimates from transforms and slopes         
  # as well as integral of survivorship and proportionate life length
  lagS <- c(1,gg_dta$surv)[-(dim(gg_dta)[1]+1)]
  lagT <- c(0,gg_dta$time)[-(dim(gg_dta)[1]+1)]
  
  deltaT <- gg_dta$time - lagT
  hzrd <- log(lagS/gg_dta$surv)/deltaT
  lnHzrd <- log(hzrd)
  dnsty <- (lagS-gg_dta$surv)/deltaT
  midInt <- (gg_dta$time+lagT)/2
  lagL <- 0
  
  life <- vector("numeric", length=dim(gg_dta)[1])
  for(ind in 1:dim(gg_dta)[1]){
    life[ind] <- lagL +deltaT[ind] *(3*gg_dta[ind,"surv"] - lagS[ind])/2
    lagL <- life[ind]
  }
  prpLife <- life/gg_dta$time
  gg_dta<- data.frame(cbind(gg_dta, hazard=hzrd, density=dnsty, mid_int=midInt, life=life, proplife=prpLife))
  
  class(gg_dta) <- c("gg_survival", class(gg_dta)) 
  invisible(gg_dta)
}

