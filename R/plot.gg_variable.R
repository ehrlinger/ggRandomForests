####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    John Ehrlinger, Ph.D.
####    Assistant Staff
####    Dept of Quantitative Health Sciences
####    Learner Research Institute
####    Cleveland Clinic Foundation
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' 
#' Plot a \code{\link{gg_variable}} object,
#' 
#' @param x \code{\link{gg_variable}} object created from a \code{randomForestSRC::rfsrc} object
#' @param xvar variable (or list of variables) of interest.
#' @param time For survival, one or more times of interest
#' @param time_labels string labels for times
#' @param panel Should plots be facetted along multiple xvar?
#' @param oob oob estimates (boolean)
#' @param smooth type of smooth curve
#' @param ... arguments passed to the \code{\link{gg_variable}} function.
#'   
#' @return A single \code{ggplot} object, or list of \code{ggplot} objects
#'   
#' @export plot.gg_variable
#'   
#' @references Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, Rnews,
#' 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression
#' and Classification (RF-SRC), R package version 1.4.
#' 
#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth labs facet_wrap
#' @importFrom parallel mclapply
#' @importFrom reshape2 melt
#' 
#' @examples
#' \dontrun{
#' ## ------------------------------------------------------------
#' ## classification
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' ## iris
#' #rfsrc_iris <- rfsrc(Species ~., data = iris)
#' data(rfsrc_iris, package="ggRandomForests")
#' 
#' gg_dta <- gg_variable(rfsrc_iris)
#' plot(gg_dta, xvar="Sepal.Width")
#' plot(gg_dta, xvar="Sepal.Length")
#' 
#' ## !! TODO !! this needs to be corrected
#' plot(gg_dta, xvar=rfsrc_iris$xvar.names, 
#'      panel=TRUE, se=FALSE)
#' 
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#' ## -------- air quality data
#' #rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality)
#' data(rfsrc_airq, package="ggRandomForests")
#' gg_dta <- gg_variable(rfsrc_airq)
#' 
#' # an ordinal variable 
#' gg_dta[,"Month"] <- factor(gg_dta[,"Month"])
#' 
#' plot(gg_dta, xvar="Wind")
#' plot(gg_dta, xvar="Temp")
#' plot(gg_dta, xvar="Solar.R")
#' 
#' plot(gg_dta, xvar=c("Solar.R", "Wind", "Temp", "Day"), panel=TRUE)
#' 
#' plot(gg_dta, xvar="Month", notch=TRUE)
#' 
#' ## -------- motor trend cars data
#' #rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
#' data(rfsrc_mtcars, package="ggRandomForests")
#' gg_dta <- gg_variable(rfsrc_mtcars)
#' 
#' # mtcars$cyl is an ordinal variable 
#' gg_dta$cyl <- factor(gg_dta$cyl)
#' gg_dta$am <- factor(gg_dta$am)
#' gg_dta$vs <- factor(gg_dta$vs)
#' gg_dta$gear <- factor(gg_dta$gear)
#' gg_dta$carb <- factor(gg_dta$carb)
#' 
#' plot(gg_dta, xvar="cyl")
#' 
#' # Others are continuous
#' plot(gg_dta, xvar="disp")
#' plot(gg_dta, xvar="hp")
#' plot(gg_dta, xvar="wt")
#' 
#' # panel
#' plot(gg_dta,xvar=c("disp","hp", "drat", "wt", "qsec"),  panel=TRUE)
#' plot(gg_dta, xvar=c("cyl", "vs", "am", "gear", "carb") ,panel=TRUE)
#' 
#' ## -------- Boston data
#' 
#' ## ------------------------------------------------------------
#' ## survival examples
#' ## ------------------------------------------------------------
#' ## -------- veteran data
#' ## survival
#' # data(veteran, package = "randomForestSRC")
#' # rfsrc_veteran <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
#' data(rfsrc_veteran, package="ggRandomForests")
#' 
#' # get the 1 year survival time.
#' gg_dta <- gg_variable(rfsrc_veteran, time=90)
#' 
#' # Generate variable dependance plots for age and diagtime
#' plot(gg_dta, xvar = "age")
#' plot(gg_dta, xvar = "diagtime")
#' 
#' # Generate coplots
#' plot(gg_dta, xvar = c("age", "diagtime"), panel=TRUE)
#' 
#' # If we want to compare survival at different time points, say 30, 90 day 
#' # and 1 year
#' gg_dta <- gg_variable(rfsrc_veteran, time=c(30, 90, 365))
#' 
#' # Generate variable dependance plots for age and diagtime
#' plot(gg_dta, xvar = "age")
#' plot(gg_dta, xvar = "diagtime") 
#' 
#' # Generate coplots
#' plot(gg_dta, xvar =  c("age", "diagtime"), panel=TRUE)
#' 
#' ## -------- pbc data
#' }
#' 


plot.gg_variable<- function(x, xvar, 
                            time, time_labels, 
                            panel=FALSE,
                            oob=TRUE, smooth=TRUE,  ...){
  gg_dta <- x 
  if(inherits(x, "rfsrc")) gg_dta <- gg_variable(x, ...)
  
  if(inherits(gg_dta, "surv")){
    family <- "surv"
  }else if(inherits(gg_dta, "regr")){
    family <- "regr"
  }else{
    family <- "class"
  }
  if(length(grep("yhat.", colnames(gg_dta))) > 0){
    # We have a classification forest with multiple outcomes.
    if(length(grep("yhat.", colnames(gg_dta))) == 2){
      # For the case of two, we are only interested in the TRUE, not FALSE.
      gg_dta  <- gg_dta[, -grep("yhat.", colnames(gg_dta))[1]]
      colnames(gg_dta)[grep("yhat.", colnames(gg_dta))] <- "yhat"
    }else{
      # Else we want to split and duplicate the data... make it long format.
      gg_dtaX <- gg_dta[, -grep("yhat.", colnames(gg_dta))]
      gg_dtaY <- gg_dta[, grep("yhat.", colnames(gg_dta))]
      lng <- ncol(gg_dtaY)
      gg2 <- mclapply(1:ncol(gg_dtaY),
                      function(ind){cbind(gg_dtaX, yhat=gg_dtaY[,ind], outcome=ind)})
      gg3 <- do.call(rbind,gg2)
      gg3$outcome <- factor(gg3$outcome)
      gg_dta <- gg3 
    }
    # print(gg_dta)
  }
  
  if(sum(colnames(gg_dta) == "cens") != 0) family <- "surv"
  
  if(missing(xvar)){
    # We need to remove response variables here
    cls <- c(grep("yhat", colnames(gg_dta)),
             grep("cens", colnames(gg_dta)))
    xvar <- colnames(gg_dta)[-cls]
  }
  
  lng <- length(xvar)
  
  # For now, we only plot survival families as panels. 
  if(panel){
    
    ## Survival plots
    if(family == "surv"){
      #variable <- value <- time <- yhat <- cens <- NA
      ## Create a panel plot
      wchXvar <- which(colnames(gg_dta) %in% xvar)
      
      wchYvar <- which(colnames(gg_dta) %in% c("cens", "yhat", "time"))
      
      # Check for categorical X values...
      ccls <- sapply(gg_dta[,wchXvar], class)
      ccls[which(ccls=="logical")] <- "factor"
      
      gg_dta.mlt <- melt(gg_dta[,c(wchYvar, wchXvar)], id.vars=c("time","yhat","cens"))
      
      gg_dta.mlt$variable <- factor(gg_dta.mlt$variable, levels=xvar)
      gg_plt <- ggplot(gg_dta.mlt)
      
      if(sum(ccls == "factor") < length(ccls)){
        gg_plt <- gg_plt +
          labs(y= "Survival") +
          geom_point(aes_string(x="value", y="yhat", color="cens", shape="cens"), 
                     ...)
        
        if(smooth){
          gg_plt <- gg_plt +
            geom_smooth(aes_string(x="value", y="yhat"), ...)
        }
        
      }else{
        gg_plt<- gg_plt+
          geom_boxplot(aes_string(x="value", y="yhat"), color="grey", 
                       ..., outlier.shape = NA)+
          geom_jitter(aes_string(x="value", y="yhat", color="cens", shape="cens"), 
                      ...)
      }
      
      if(length(levels(gg_dta$time)) > 1){
        gg_plt <- gg_plt+
          facet_grid(reformulate("variable", "time"),
                     scales="free_x")+
          labs(x="")
      }else{
        gg_plt<- gg_plt + 
          facet_wrap(~variable,
                     scales="free_x")+
          labs(x="",y= paste("Survival at", gg_dta$time[1], "year"))
      }
    }else{
      # Panels for 
      # This will work for regression and binary classification... maybe.
      ## Create a panel plot
      wchXvar <- which(colnames(gg_dta) %in% xvar)
      
      wchYvar <- which(colnames(gg_dta) %in% c("yhat"))
      
      # Check for categorical X values...
      ccls <- sapply(gg_dta[,wchXvar], class)
      ccls[which(ccls=="logical")] <- "factor"
      
      if(family=="class"){
        wchYvar <- c(wchYvar, which(colnames(gg_dta)=="yvar"))
        gg_dta.mlt <- melt(gg_dta[,c(wchYvar, wchXvar)], id.vars=c("yhat", "yvar"))
        
      }else{
        gg_dta.mlt <- melt(gg_dta[,c(wchYvar, wchXvar)], id.vars="yhat")
        
      }
      gg_dta.mlt$variable <- factor(gg_dta.mlt$variable, levels=xvar)
      
      gg_plt <- ggplot(gg_dta.mlt)
      if(sum(ccls == "factor") < length(ccls)){
        if(family=="class"){
          gg_plt <- gg_plt +
            geom_point(aes_string(x="value", y="yhat", color="yvar", shape="yvar"), ...)
        }else{
          gg_plt <- gg_plt +
            geom_point(aes_string(x="value", y="yhat"), ...)
        }
      }else{
        if(family=="class"){
          gg_plt <- gg_plt +
            geom_boxplot(aes_string(x="value", y="yhat", color="yvar"), ...)
        }else{
          gg_plt <- gg_plt +
            geom_boxplot(aes_string(x="value", y="yhat"), ...)
        }
      }
      if(smooth & family!="class"){
        gg_plt <- gg_plt +
          geom_smooth(aes_string(x="value", y="yhat"), ...)
      }
      
      gg_plt <- gg_plt +
        facet_wrap(~variable,
                   scales="free_x")+
        labs(x="")
    }
  }else{
    # Plot or list of plots.
    gg_plt <- vector("list", length=lng)
    
    for(ind in 1:lng){
      chIndx <- which(colnames(gg_dta)==xvar[ind])
      hName <- colnames(gg_dta)[chIndx]
      colnames(gg_dta)[chIndx] <- "var"
      ccls <- class(gg_dta[,"var"])
      
      # Check for logicals...
      if(length(unique(gg_dta[,"var"])) < 3 & ccls =="numeric" ){
        ccls <- "logical"
        gg_dta[,"var"] <- as.logical(gg_dta[,"var"])
      } 
      gg_plt[[ind]] <- ggplot(gg_dta)
      
      if(family == "surv"){
        gg_plt[[ind]] <- gg_plt[[ind]]+
          labs(x=hName, y= "Survival")
        if(ccls=="numeric"){
          gg_plt[[ind]] <- gg_plt[[ind]]+
            geom_point(aes_string(x="var", y="yhat", color="cens", shape="cens"), 
                       ...)
          
          if(smooth){
            
            gg_plt[[ind]] <- gg_plt[[ind]] +
              geom_smooth(aes_string(x="var", y="yhat"), ...)
            
          }
        }else{
          gg_plt[[ind]] <- gg_plt[[ind]]+
            geom_boxplot(aes_string(x="var", y="yhat"), color="black",
                         ..., outlier.shape = NA)+
            geom_jitter(aes_string(x="var", y="yhat", color="cens", shape="cens"), 
                        ...)
          
        }
        if(length(levels(gg_dta$time)) > 1){
          gg_plt[[ind]]<- gg_plt[[ind]] + facet_wrap(~time, ncol=1)
        }else{
          gg_plt[[ind]]<- gg_plt[[ind]] + 
            labs(x=hName, y= paste("Survival at", gg_dta$time[1], "year"))
        }
      }else if(family == "class"){
        gg_plt[[ind]] <- gg_plt[[ind]] +
          labs(x=hName, y="Predicted")
        
        if(sum(colnames(gg_dta) == "outcome") ==0){ 
          if(ccls=="numeric"){
            gg_plt[[ind]] <- gg_plt[[ind]] +
              geom_point(aes_string(x="var", y="yhat", color="yvar", shape="yvar"),
                         ...)
            
            if(smooth){
              
              gg_plt[[ind]] <- gg_plt[[ind]] +
                geom_smooth(aes_string(x="var", y="yhat"), ...)
            }
          }else{
            gg_plt[[ind]] <- gg_plt[[ind]]+
              geom_boxplot(aes_string(x="var", y="yhat"), color="grey", 
                           ..., outlier.shape = NA)+
              geom_jitter(aes_string(x="var", y="yhat", color="yvar", shape="yvar"), 
                          ...)
            
          }
        }else{
          
          if(ccls=="numeric"){
            gg_plt[[ind]] <- gg_plt[[ind]] +
              geom_point(aes_string(x="var", y="yhat", color="yvar", shape="yvar"),
                         ...)
            
          }else{
            gg_plt[[ind]] <- gg_plt[[ind]]+
              geom_boxplot(aes_string(x="var", y="yhat"), color="grey", 
                           ..., outlier.shape = NA)+
              geom_jitter(aes_string(x="var", y="yhat", color="yvar", shape="yvar"), 
                          ...)
            
          }
          
          gg_plt[[ind]] <- gg_plt[[ind]] + facet_grid(~outcome)
        }
      }else{
        # assume regression
        gg_plt[[ind]] <- gg_plt[[ind]] +
          labs(x=hName, y="Predicted")
        if(ccls=="numeric"){
          gg_plt[[ind]] <- gg_plt[[ind]] +
            geom_point(aes_string(x="var", y="yhat"), ...)
          
          if(smooth){
            
            gg_plt[[ind]] <- gg_plt[[ind]] +
              geom_smooth(aes_string(x="var", y="yhat"), ...)
            
          }
        }else{
          gg_plt[[ind]] <- gg_plt[[ind]]+
            geom_boxplot(aes_string(x="var", y="yhat"), color="grey", 
                         ..., outlier.shape = NA)+
            geom_jitter(aes_string(x="var", y="yhat"), 
                        ...)
        }
        # Replace the original colname
        colnames(gg_dta)[chIndx] <- hName
      }
    }
    if(lng == 1) gg_plt <- gg_plt[[1]]
  }    
  return(gg_plt)
}
