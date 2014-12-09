#' nonparametric Nelson-Aalen estimates
#'
#' @param data name of the survival training data.frame
#' @param interval name of the interval variable in the training dataset.
#' @param censor name of the censoring variable in the training dataset.
#' @param strat stratifying variable in the training dataset, defaults to NULL
#' @param weight for each observation (default=NULL)
#' @param climit confidence bounds (default=0.95)
#'
#' @export nelson
#' @importFrom survival Surv survfit strata 
#' 
nelson <- function(interval, censor, data, strat=NULL, weight=NULL,climit=.95){
  call <- match.call()
  
  # Make sure we've speced the confidence limit correctly
  if(climit > 1) climit <- climit/100
  z <- qnorm(1-(1-climit)/2)
  
  # Set weighting for non-events to a value of 0
  # Set up weights (severity of event)                                           
  if(!is.null(weight)) weight <- data[,censor] * weight
  # Kaplan-Meier analysis
  srv <- Surv(time=data[,interval], event=data[,censor])
  if(is.null(strat)){
    srvTab<-survfit(srv~1,data)
  }else{
    srvTab <- survfit(srv~1,strata(strat),data)
  }
  #
  #*******************************************************************************;
  #* Cumulative hazard and hazard estimates from transforms and slopes            ;
  #* as well as integral of survivorship and proportionate life length            ;
  cumHazard <-cumsum(srvTab$n.event/srvTab$n.risk)
  cumSurv <- exp(-cumHazard)
  lCumHaz <- log(cumHazard)
  lInterval <- log(data[,interval])
  times <- order(data[,interval])
  deltaTime <- sapply(2:length(times), function(ind){times[ind] - times[ind-1] })
  #  cat(srvTab$n.event)
  indx <- which(srvTab$n.event>0)
  sigma2 <-log(1/srvTab$n.risk[indx]/(srvTab$n.event[indx]*cumHazard[indx]) +1)
  mu <- log(cumHazard[indx]) - sigma2/2
  cll <- 1-exp(mu + z*sqrt(sigma2))
  clu <- 1-exp(mu - z*sqrt(sigma2))
  cll[which(cll<0)] <- 0
  # Still need to add hazard and denisty.
  tbl <-cbind(srvTab$time,  srvTab$n.risk,srvTab$n.censor, srvTab$n.risk-srvTab$n.event,
              srvTab$n.event, cumSurv) 
  colnames(tbl) <- c("time", "n", "cens", "num.risk","dead", "surv")
  
  #, "hazard", "density")            
  #*******************************************************************************;
  # Summarize the various strata
  # only look at events
  gg_dta <- tbl[which(tbl[,"dead"]!= 0),]
  
  # Calculate the hazard estimates from transforms and slopes         
  # as well as integral of survivorship and proportionate life length
  lagS <- c(1,gg_dta[,"surv"])[-(dim(gg_dta)[1]+1)]
  lagT <- c(0,gg_dta[,"time"])[-(dim(gg_dta)[1]+1)]
  
  deltaT <- gg_dta[,"time"] - lagT
  hzrd <- log(lagS/gg_dta[,"surv"])/deltaT
  lnHzrd <- log(hzrd)
  dnsty <- (lagS-gg_dta[,"surv"])/deltaT
  midInt <- (gg_dta[,"time"]+lagT)/2
  lagL <- 0
  life <- vector("numeric", length=dim(gg_dta)[1])
  
  for(ind in 1:dim(gg_dta)[1]){
    life[ind] <- lagL +deltaT[ind] *(3*gg_dta[ind,"surv"] - lagS[ind])/2
    lagL <- life[ind]
  }
  prpLife <- life/gg_dta[,"time"]
  gg_dta<- data.frame(cbind(gg_dta, cll, clu, cumHazard[indx], hzrd, dnsty, midInt, life, prpLife))
  colnames(gg_dta) <- c(colnames(gg_dta)[1:6], 
                       "lower_cl", "upper_cl", "cum_haz","hazard", "density", "mid_int", "life", "proplife")
  
  class(gg_dta) <- c("gg_survival", class(gg_dta)) 
  invisible(gg_dta)
}

