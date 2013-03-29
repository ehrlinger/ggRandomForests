#' kaplan creates a nonparametric kaplan-meier curve from the data.frame dataset
#'
#' @param data name of the training data.frame
#' @param interval name of the interval variable in the training dataset.
#' @param censor name of the censoring variable in the training dataset.
#' @param strat stratifying variable in the training dataset, defaults to NULL
#' 
#' @return data.frame containing
#' 
#' @export kaplan
#' @importFrom survival Surv survfit strata 
#' 
#' 
require(survival)
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
  tbl.e<- cbind(tbl.e, hzrd, dnsty, midInt, life, prpLife)
  colnames(tbl.e) <- c(colnames(tbl.e)[1:9], "hazard", "density", "mid_int", "life", "proplife")
  
  invisible(as.data.frame(tbl.e))
}

