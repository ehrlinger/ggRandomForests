
#' plot.survival 
#'
#' @param x,
#' @param subset, 
#' @param collapse = FALSE,
#' @param haz.model = c("spline", "ggamma", "nonpar"),
#' @param k = 25,
#' @param span = "cv",
#' @param cens.model = c("km", "rfsrc"),
#' @param ...
#'
#' @export plot.survival.ggrfsrc plot.survival
plot.survival.ggrfsrc <- function (object,
                                   prd.type=c("std", "oob"),
                                   srv.type=c("surv", "cumhaz", "mort", "haz"),
                                   points = c("none", "kaplan", "nelson"),
                                   show.ind = FALSE,
                                   subset,
                                   strata,
                                   errorbars,
                                   ...)
{ 
  
  # Select OOB or resub
  srv.data <- object$survival.oob
  
  # The mean survival time is the average of all predicted survival curves.
  
  # Can generate KM or NA estimates from the $yvar object.
  
  
  srv<- as.data.frame(cbind(time=object$time.interest,t(srv.data)))
  srv.m <- melt(srv, id="time")
  
  
  plt<-ggplot(srv.m)+geom_step(aes(x=time, y=value, by=variable), alpha=.1) +
    geom_smooth(aes(x=time, y=value), size=2) + 
    #+ geom_step(aes(x=V1, y=V2), data= srv.mean, col="red", size=1.5)+theme_bw() +
    #+ geom_step(aes(x=time, y=mn), data= srvMN.mean, col="green", size=1)+
    
}


plot.survival <- plot.survival.ggrfsrc
