#' nonparametric kaplan-meier estimates
#'
#' @param data name of the training set \code{data.frame}
#' @param interval name of the interval variable in the training dataset.
#' @param censor name of the censoring variable in the training dataset.
#' @param strat stratifying variable in the training dataset, defaults to NULL
#' 
#' @return \code{\link{eventtable}}
#' 
#' @export kaplan
#' @importFrom survival Surv survfit strata 
#' 
kaplan <- function(interval, 
                   censor,
                   data, 
                   strat=NULL){
  
  # Kaplan-Meier analysis
  srv <- Surv(time=data[,interval], event=data[,censor])
  if(is.null(strat)){
    srvTab<-survfit(srv~1,data)
  }else{
    srvTab <- survfit(srv~strata(data[,strat]),data)
  }
  #
  # OR for stratification on 
  # srvTab <- survfit(Surv(interval, event)~strata(stratify),data, type=type)
  #cat(str(srvTab))
  #*******************************************************************************;
  #* Cumulative hazard and hazard estimates from transforms and slopes            ;
  #* as well as integral of survivorship and proportionate life length            ;
  cumHazard <- -log(srvTab$surv)
  lCumHaz <- log(cumHazard)
  lInterval <- log(data[,interval])
  times <- order(data[,interval])
  deltaTime <- sapply(2:length(times), function(ind){times[ind] - times[ind-1] })
  
  # Still need to add hazard and denisty.
  tbl <-cbind(srvTab$time,  srvTab$n.risk,srvTab$n.censor, srvTab$n.event, 
              srvTab$surv, srvTab$std.err, srvTab$lower, srvTab$upper,
              cumHazard) 
  colnames(tbl) <- c("time", "n", "cens", "dead", "surv", "se", 
                     "lower_cl", "upper_cl", "cum_haz")
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
  gg_dta<- data.frame(cbind(gg_dta, hzrd, dnsty, midInt, life, prpLife))
  colnames(gg_dta) <- c(colnames(gg_dta)[1:9], "hazard", "density", "mid_int", "life", "proplife")
  
  class(gg_dta) <- c("gg_survival", class(gg_dta)) 
  invisible(gg_dta)
}

