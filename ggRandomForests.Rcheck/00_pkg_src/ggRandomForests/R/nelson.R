#' nonparametric Nelson-Aalen estimates
#'
#' @param data name of the survival training data.frame
#' @param interval name of the interval variable in the training dataset.
#' @param censor name of the censoring variable in the training dataset.
#' @param by stratifying variable in the training dataset, defaults to NULL
#' @param weight for each observation (default=NULL)
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
#' gg_dta <- nelson(interval="time", censor="status", 
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
#' plot(gg_dta, error="lines")
#' plot(gg_dta)
#' 
#' gg_dta <- gg_survival(interval="time", censor="status", 
#'                      data=pbc, by="treatment",
#'                      type="nelson")
#'                      
#' plot(gg_dta, error="bars")
#' plot(gg_dta)
#' 
#' }                                            
#' @export
nelson <- function(interval, censor, data, by=NULL, weight=NULL,...){
  #call <- match.call()
  
  arg_list <- list(...)
  #   climit <- arg_list[["conf.int"]]
  #   
  #   if(is.null(arg_list$conf.int)) climit <- .95
  #   
  #   # Make sure we've speced the confidence limit correctly
  #   if(climit > 1) climit <- climit / 100
  #   #z <- qnorm(1 - (1 - climit) / 2)
  #   
  # Set weighting for non-events to a value of 0
  # Set up weights (severity of event)                                           
  if(!is.null(weight)) weight <- data[,censor] * weight
  
  # Kaplan-Meier analysis
  
  # Kaplan-Meier analysis
  srv <- survival::Surv(time=data[,interval], event=data[,censor])
  if(is.null(by)){
    srv_tab <- survival::survfit(srv~1, ...)
  }else{
    srv_tab <- survival::survfit(srv~survival::strata(data[,by]), ...)
  }
  #
  # OR for stratification on 
  # srv_tab <- survfit(Surv(interval, event)~strata(stratify),data, type=type)
  #cat(str(srv_tab))
  #*******************************************************************************;
  #* Cumulative hazard and hazard estimates from transforms and slopes            ;
  #* as well as integral of survivorship and proportionate life length            ;
  hazard <- srv_tab$n.event / srv_tab$n.risk
  cum_hazard <- vector()
  for(i in 1:length(hazard)) 
    cum_hazard[i] <- sum(hazard[1:i])
  cum_hazard <- c(cum_hazard, cum_hazard[length(cum_hazard)])
  cum_hazard <- -log(srv_tab$surv)
  ln_cum_haz <- log(cum_hazard)
  l_interval <- log(data[,interval])
  times <- order(data[,interval])
  delta_time <- sapply(2:length(times), function(ind){
    times[ind] - times[ind - 1] 
    })
  
  # Still need to add hazard and density.
  tbl <-data.frame(cbind(time = srv_tab$time,  n = srv_tab$n.risk,
                         cens = srv_tab$n.censor, dead = srv_tab$n.event, 
                         surv = srv_tab$surv, se = srv_tab$std.err, 
                         lower = srv_tab$lower, upper = srv_tab$upper,
                         cum_haz = cum_hazard) )
  
  # Add group labels when stratifying data.
  if(!is.null(by)){
    tm_splits <- which(c(FALSE,sapply(2:nrow(tbl), function(ind){
      tbl$time[ind] < tbl$time[ind - 1]
    })))
    
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
  lag_surv <- c(1,gg_dta$surv)[-(dim(gg_dta)[1] + 1)]
  lag_time <- c(0,gg_dta$time)[-(dim(gg_dta)[1] + 1)]
  
  delta_t <- gg_dta$time - lag_time
  hzrd <- log(lag_surv / gg_dta$surv) / delta_t
  ln_hzrd <- log(hzrd)
  dnsty <- (lag_surv - gg_dta$surv) / delta_t
  mid_int <- (gg_dta$time + lag_time) / 2
  lagL <- 0
  
  life <- vector("numeric", length=dim(gg_dta)[1])
  for(ind in 1:dim(gg_dta)[1]){
    life[ind] <- lagL + delta_t[ind] *(3*gg_dta[ind,"surv"] - lag_surv[ind]) / 2
    lagL <- life[ind]
  }
  prpLife <- life/gg_dta$time
  gg_dta<- data.frame(cbind(gg_dta, hazard = hzrd, density = dnsty, 
                            mid_int = mid_int, life = life, proplife = prpLife))
  
  class(gg_dta) <- c("gg_survival", class(gg_dta)) 
  invisible(gg_dta)
}

