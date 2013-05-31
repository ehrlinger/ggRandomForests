### plot.roc
##
## spc<- calc.roc(rf, dta$sten_grp, which.outcome=3)
## ggplot(data=spc)+geom_line(aes(x=(1-sens), y=spec))+theme_bw()+geom_abline(a=1, b=0)
##
## mstn<- calc.roc(rf, dta$sten_grp, which.outcome=2)
## ggplot(data=stn)+geom_line(aes(x=(1-sens), y=spec))+theme_bw()+geom_abline(a=1, b=0)+
##   geom_line(aes(x=(1-sens), y=spec), data=mstn,col="red")
##
## nstn<- calc.roc(rf, dta$sten_grp, which.outcome=1)
## ggplot(data=stn)+geom_line(aes(x=(1-sens), y=spec))+theme_bw()+geom_abline(a=1, b=0)+
##   geom_line(aes(x=(1-sens), y=spec), data=mstn,col="red") + 
##   geom_line(aes(x=(1-sens), y=spec), data=nstn, col="blue")

calc.roc.rfsrc <- function(rf, dta, which.outcome=1, oob.prd=TRUE){
  dta.roc <- as.data.frame(cbind(res=(dta == levels(dta)[which.outcome]), prd=rf$predicted[, which.outcome],
                                 oob=rf$predicted.oob[, which.outcome]))
  
  if(oob.prd)
    pct <- sort(unique(rf$predicted.oob[,which.outcome]))
  else
    pct <- sort(unique(rf$predicted[,which.outcome]))
  
  pct<- pct[-length(pct)]
  
  spc <-mclapply(pct, function(crit){
    if(oob.prd) 
      tbl <- xtabs(~res+(oob>crit), dta.roc)
    else
      tbl <- xtabs(~res+(prd>crit), dta.roc)
    
    spec<-tbl[2,2]/rowSums(tbl)[2]
    sens<-tbl[1,1]/rowSums(tbl)[1]
    cbind(spec=spec, sens=sens)
  }, mc.cores = (detectCores()-1))
  return(as.data.frame(do.call(rbind, spc)))
  
}
calc.roc<- calc.roc.rfsrc
calc.auc.rfsrc <- function(x){
  ## Use the trapeziod rule, basically
  ##
  ## auc = dx/2(f(x_{i+1}) - f(x_i))
  ##
  ## f(x) is sensitivity, x is 1-specificity
  
  
  auc <- sapply(2:dim(x)[1], function(ind){
    dx <- x$sens[ind]-x$sens[ind-1]
    (x$spec[ind] + x$spec[ind-1])*dx/2
  }) 
  
  sum(auc)
}
calc.auc<- calc.auc.rfsrc