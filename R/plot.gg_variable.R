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
#' @param x_var variable (or list of variables) of interest.
#' @param time For survival, one or more times of interest
#' @param time_labels string labels for times
#' @param panel Should plots be facetted along multiple x_var?
#' @param oob oob estimates (boolean)
#' @param smooth type of smooth curve
#' @param ... arguments passed to the \code{\link{gg_variable}} function.
#'   
#' @return \code{ggplot} object
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
#' 
#' @examples
#' \dontrun{
#' ## ------------------------------------------------------------
#' ## classification
#' ## ------------------------------------------------------------
#' 
#' ## iris
#' #iris.obj <- rfsrc(Species ~., data = iris)
#' data(iris_rf, package="ggRandomForests")
#' 
#' 
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#' 
#' ## airquality
#' #airq.obj <- rfsrc(Ozone ~ ., data = airquality)
#' data(airq_rf, package="ggRandomForests")
#' ggrf <- gg_variable(airq_rf)
#' plot(ggrf, x_var="Wind")
#' plot(ggrf, x_var="Temp")
#' plot(ggrf, x_var="Solar.R")
#' 
#' ## motor trend cars
#' #mtcars.obj <- rfsrc(mpg ~ ., data = mtcars)
#' data(mtcars_rf, package="ggRandomForests")
#' ggrf <- gg_variable(mtcars_rf)
#' 
#' # mtcars$cyl is an ordinal variable
#' plot(ggrf, x_var="cyl")
#' 
#' # Others are continuous
#' plot(ggrf, x_var="disp")
#' plot(ggrf, x_var="hp")
#' plot(ggrf, x_var="wt")
#' 
#' ## ------------------------------------------------------------
#' ## survival examples
#' ## ------------------------------------------------------------
#' 
#' ## survival
#' # data(veteran, package = "randomForestSRC")
#' # veteran_rf <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
#' data(veteran_rf, package="ggRandomForests")
#' 
#' # get the 30 day survival time.
#' ggrf <- gg_variable(veteran_rf, time=30)
#' 
#' # Generate variable dependance plots for age and diagtime
#' plot(ggrf, x_var = "age")
#' plot(ggrf, x_var = "diagtime")
#' 
#' # If we want to compare survival at different time points, say 30, 90 day 
#' # and 1 year
#' ggrf <- gg_variable(veteran_rf, time=c(30, 90, 365))
#' 
#' # Generate variable dependance plots for age and diagtime
#' plot(ggrf, x_var = "age")
#' plot(ggrf, x_var = "diagtime") 
#'
#' }
### error rate plot
plot.gg_variable<- function(x, x_var, 
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
      gg2 <- lapply(1:ncol(gg_dtaY),
                    function(ind){cbind(gg_dtaX, yhat=gg_dtaY[,ind], outcome=ind)})
      gg3 <- do.call(rbind,gg2)
      gg3$outcome <- factor(gg3$outcome)
      gg_dta <- gg3 
    }
    # print(gg_dta)
  }
  
  if(sum(colnames(gg_dta) == "cens") != 0) family <- "surv"
  
  if(missing(x_var)){
    # We need to remove response variables here
    cls <- c(grep("yhat", colnames(gg_dta)),
             grep("cens", colnames(gg_dta)))
    x_var <- colnames(gg_dta)[-cls]
  }
  
  lng <- length(x_var)
  
  # For now, we only plot survival families as panels. 
  if(panel){
    if(family == "surv"){
      variable <- value <- time <- yhat <- cens <- NA
      ## Create a panel plot
      wchXvar <- which(colnames(gg_dta) %in% x_var)
      
      wchYvar <- which(colnames(gg_dta) %in% c("cens", "yhat", "time"))
      
      gg_dta.mlt <- gg_dta %>% select(c(wchYvar, wchXvar)) %>%
        gather(variable, value,-time, -yhat, -cens)
      
      gg_plt <- ggplot(gg_dta.mlt)+
        labs(y= "Survival") +
        geom_point(aes_string(x="value", y="yhat", color="cens", shape="cens"), 
                   ...)
      
      if(smooth){
        gg_plt <- gg_plt +
          geom_smooth(aes_string(x="value", y="yhat"), ...)
      }
      if(length(levels(gg_dta$time)) > 1){
        gg_plt<- gg_plt+
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
      # This will work for regression and binary classification... maybe.
      variable <- value <- yhat <- NA
      ## Create a panel plot
      wchXvar <- which(colnames(gg_dta) %in% x_var)
      
      wchYvar <- which(colnames(gg_dta) %in% c("yhat"))
      
      gg_dta.mlt <- gg_dta %>% select(c(wchYvar, wchXvar)) %>%
        gather(variable, value, -yhat)
      
      gg_dta.mlt$variable <- factor(gg_dta.mlt$variable,
                                    levels=unique(gg_dta.mlt$variable))
      
      gg_plt <- ggplot(gg_dta.mlt)+
        geom_point(aes_string(x="value", y="yhat"), 
                   ...)
      
      if(smooth){
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
      chIndx <- which(colnames(gg_dta)==x_var[ind])
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
            geom_boxplot(aes_string(x="var", y="yhat"), color="grey",
                         ...,outlier.shape = NA)+
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
