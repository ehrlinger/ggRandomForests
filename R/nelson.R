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
  tbl.e <- tbl[which(tbl[,"dead"]!= 0),]
  
  # Calculate the hazard estimates from transforms and slopes         
  # as well as integral of survivorship and proportionate life length
  lagS <- c(1,tbl.e[,"surv"])[-(dim(tbl.e)[1]+1)]
  lagT <- c(0,tbl.e[,"time"])[-(dim(tbl.e)[1]+1)]
  
  deltaT <- tbl.e[,"time"] - lagT
  hzrd <- log(lagS/tbl.e[,"surv"])/deltaT
  lnHzrd <- log(hzrd)
  dnsty <- (lagS-tbl.e[,"surv"])/deltaT
  midInt <- (tbl.e[,"time"]+lagT)/2
  lagL <- 0
  life <- vector("numeric", length=dim(tbl.e)[1])
  
  for(ind in 1:dim(tbl.e)[1]){
    life[ind] <- lagL +deltaT[ind] *(3*tbl.e[ind,"surv"] - lagS[ind])/2
    lagL <- life[ind]
  }
  prpLife <- life/tbl.e[,"time"]
  tbl.e<- data.frame(cbind(tbl.e, cll, clu, cumHazard[indx], hzrd, dnsty, midInt, life, prpLife))
  colnames(tbl.e) <- c(colnames(tbl.e)[1:6], 
                       "lower_cl", "upper_cl", "cum_haz","hazard", "density", "mid_int", "life", "proplife")
  
  class(tbl.e) <- c("gg_survival", class(tbl.e)) 
  invisible(tbl.e)
}

