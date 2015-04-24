#' nonparametric kaplan-meier estimates
#'
#' @param data name of the training set \code{data.frame}
#' @param interval name of the interval variable in the training dataset.
#' @param censor name of the censoring variable in the training dataset.
#' @param by stratifying variable in the training dataset, defaults to NULL
#' @param ... arguments passed to the \code{survfit} function 
#' 
#' @return \code{\link{gg_survival}} object
#' 
#' @importFrom survival Surv survfit strata 
#' 
#' @seealso \code{\link{gg_survival}} \code{\link{nelson}} \code{\link{plot.gg_survival}}
#'
#' @examples 
#' \dontrun{
#' # These get run through the gg_survival examples.
#' data(pbc, package="randomForestSRC")
#' pbc$time <- pbc$days/364.25
#' 
#' # This is the same as gg_survival
#' gg_dta <- kaplan(interval="time", censor="status", 
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
#' }                                            
#' @export
kaplan <- function(interval, 
                   censor,
                   data, 
                   by=NULL, ...){
  srv <- survival::Surv(time=data[,interval], event=data[,censor])
  
  # Kaplan-Meier analysis
  if(is.null(by)){
    srv_tab <- survival::survfit(srv ~ 1, ...)
    
  }else{
    srv_tab <- survival::survfit(srv ~ survival::strata(data[,by]), ...)
    
  }
  #
  # OR for stratification on 
  # srv_tab <- survfit(Surv(interval, event)~strata(stratify),data, type=type)
  #cat(str(srv_tab))
  #*******************************************************************************;
  #* Cumulative hazard and hazard estimates from transforms and slopes            ;
  #* as well as integral of survivorship and proportionate life length            ;
  cum_hazard <- -log(srv_tab$surv)
  lCumHaz <- log(cum_hazard)
  lInterval <- log(data[,interval])
  times <- order(data[,interval])
  delta_time <- sapply(2:length(times), function(ind){
    times[ind] - times[ind - 1] 
    })
  
  # Still need to add hazard and density.
  tbl <- data.frame(cbind(time=srv_tab$time,  n=srv_tab$n.risk,
                         cens=srv_tab$n.censor, dead=srv_tab$n.event, 
                         surv=srv_tab$surv, se=srv_tab$std.err, lower=srv_tab$lower, 
                         upper=srv_tab$upper,
                         cum_haz=cum_hazard) )
  
  # Add group labels when stratifying data.
  if(!is.null(by)){
    tm_splits <- which(c(FALSE,sapply(2:nrow(tbl), function(ind){tbl$time[ind] < tbl$time[ind - 1]})))
    
    lbls <- unique(data[,by])
    tbl$groups <- lbls[1]
    
    for(ind in 2:(length(tm_splits) + 1)){
      tbl$groups[tm_splits[ind - 1]:nrow(tbl)] <- lbls[ind]
    }
  }
  
  #, "hazard", "density")            
  #*******************************************************************************;
  # Summarize the various strata
  # only look at events
  gg_dta <- tbl[which(tbl[,"dead"] != 0),]
  
  # Calculate the hazard estimates from transforms and slopes         
  # as well as integral of survivorship and proportionate life length
  lag_s <- c(1,gg_dta$surv)[-(dim(gg_dta)[1] + 1)]
  lag_t <- c(0,gg_dta$time)[-(dim(gg_dta)[1] + 1)]
  
  delta_t <- gg_dta$time - lag_t
  hzrd <- log(lag_s / gg_dta$surv) / delta_t
  ln_hzrd <- log(hzrd)
  dnsty <- (lag_s - gg_dta$surv) / delta_t
  mid_int <- (gg_dta$time + lag_t) / 2
  lag_l <- 0
  
  life <- vector("numeric", length=dim(gg_dta)[1])
  for(ind in 1:dim(gg_dta)[1]){
    life[ind] <- lag_l + delta_t[ind] * (3 * gg_dta[ind,"surv"] - lag_s[ind]) / 2
    lag_l <- life[ind]
  }
  prp_life <- life / gg_dta$time
  gg_dta<- data.frame(cbind(gg_dta, hazard = hzrd, 
                            density = dnsty, mid_int = mid_int, life = life, 
                            proplife = prp_life))
  
  class(gg_dta) <- c("gg_survival", class(gg_dta)) 
  invisible(gg_dta)
}

