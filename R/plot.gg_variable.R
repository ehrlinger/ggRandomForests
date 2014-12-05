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
  object <- x 
  if(inherits(x, "rfsrc")) object <- gg_variable(x, ...)

  if(inherits(object, "surv")){
    family <- "surv"
  }else if(inherits(object, "regr")){
    family <- "regr"
  }else{
    family <- "class"
  }
  if(length(grep("yhat.", colnames(object))) > 0){
    # We have a classification forest with multiple outcomes.
    if(length(grep("yhat.", colnames(object))) == 2){
      # For the case of two, we are only interested in the TRUE, not FALSE.
      object  <- object[, -grep("yhat.", colnames(object))[1]]
      colnames(object)[grep("yhat.", colnames(object))] <- "yhat"
    }else{
      # Else we want to split and duplicate the data... make it long format.
      objectX <- object[, -grep("yhat.", colnames(object))]
      objectY <- object[, grep("yhat.", colnames(object))]
      lng <- ncol(objectY)
      gg2 <- lapply(1:ncol(objectY),
                    function(ind){cbind(objectX, yhat=objectY[,ind], outcome=ind)})
      gg3 <- do.call(rbind,gg2)
      gg3$outcome <- factor(gg3$outcome)
      object <- gg3 
    }
    # print(object)
  }
  
  if(sum(colnames(object) == "cens") != 0) family <- "surv"
  
  if(missing(x_var)){
    # We need to remove response variables here
    cls <- c(grep("yhat", colnames(object)),
             grep("cens", colnames(object)))
    x_var <- colnames(object)[-cls]
  }
  
  lng <- length(x_var)
  
  # For now, we only plot survival families as panels. 
  if(panel){
    if(family == "surv"){
      variable <- value <- time <- yhat <- cens <- NA
      ## Create a panel plot
      wchXvar <- which(colnames(object) %in% x_var)
      
      wchYvar <- which(colnames(object) %in% c("cens", "yhat", "time"))
      
      dataObject <- object %>% select(c(wchYvar, wchXvar)) %>%
        gather(variable, value,-time, -yhat, -cens)
      
      gDta <- ggplot(dataObject)+
        labs(y= "Survival") +
        geom_point(aes_string(x="value", y="yhat", color="cens", shape="cens"), 
                   ...)
      
      if(smooth){
        gDta <- gDta +
          geom_smooth(aes_string(x="value", y="yhat"), ...)
      }
      if(length(levels(object$time)) > 1){
        gDta<- gDta+
          facet_grid(reformulate("variable", "time"),
                     scales="free_x")+
          labs(x="")
      }else{
        gDta<- gDta + 
          facet_wrap(~variable,
                     scales="free_x")+
          labs(x="",y= paste("Survival at", object$time[1], "year"))
      }
      
    }else{
      # This will work for regression and binary classification... maybe.
      variable <- value <- yhat <- NA
      ## Create a panel plot
      wchXvar <- which(colnames(object) %in% x_var)
      
      wchYvar <- which(colnames(object) %in% c("yhat"))
      
      dataObject <- object %>% select(c(wchYvar, wchXvar)) %>%
        gather(variable, value, -yhat)
      
      dataObject$variable <- factor(dataObject$variable,
                                    levels=unique(dataObject$variable))
      
      gDta <- ggplot(dataObject)+
        geom_point(aes_string(x="value", y="yhat"), 
                   ...)
      
      if(smooth){
        gDta <- gDta +
          geom_smooth(aes_string(x="value", y="yhat"), ...)
      }
      
      gDta <- gDta +
        facet_wrap(~variable,
                   scales="free_x")+
        labs(x="")
    }
  }else{
    # Plot or list of plots.
    gDta <- vector("list", length=lng)
    
    for(ind in 1:lng){
      chIndx <- which(colnames(object)==x_var[ind])
      hName <- colnames(object)[chIndx]
      colnames(object)[chIndx] <- "var"
      ccls <- class(object[,"var"])
      
      # Check for logicals...
      if(length(unique(object[,"var"])) < 3 & ccls =="numeric" ){
        ccls <- "logical"
        object[,"var"] <- as.logical(object[,"var"])
      } 
      gDta[[ind]] <- ggplot(object)
      
      if(family == "surv"){
        gDta[[ind]] <- gDta[[ind]]+
          labs(x=hName, y= "Survival")
        if(ccls=="numeric"){
          gDta[[ind]] <- gDta[[ind]]+
            geom_point(aes_string(x="var", y="yhat", color="cens", shape="cens"), 
                       ...)
          
          if(smooth){
            
            gDta[[ind]] <- gDta[[ind]] +
              geom_smooth(aes_string(x="var", y="yhat"), ...)
            
          }
        }else{
          gDta[[ind]] <- gDta[[ind]]+
            geom_boxplot(aes_string(x="var", y="yhat"), color="grey",
                         ...,outlier.shape = NA)+
            geom_jitter(aes_string(x="var", y="yhat", color="cens", shape="cens"), 
                        ...)
          
        }
        if(length(levels(object$time)) > 1){
          gDta[[ind]]<- gDta[[ind]] + facet_wrap(~time, ncol=1)
        }else{
          gDta[[ind]]<- gDta[[ind]] + 
            labs(x=hName, y= paste("Survival at", object$time[1], "year"))
        }
      }else if(family == "class"){
        gDta[[ind]] <- gDta[[ind]] +
          labs(x=hName, y="Predicted")
        
        if(sum(colnames(object) == "outcome") ==0){ 
          if(ccls=="numeric"){
            gDta[[ind]] <- gDta[[ind]] +
              geom_point(aes_string(x="var", y="yhat", color="yvar", shape="yvar"),
                         ...)
            
            if(smooth){
              
              gDta[[ind]] <- gDta[[ind]] +
                geom_smooth(aes_string(x="var", y="yhat"), ...)
            }
          }else{
            gDta[[ind]] <- gDta[[ind]]+
              geom_boxplot(aes_string(x="var", y="yhat"), color="grey", 
                           ..., outlier.shape = NA)+
              geom_jitter(aes_string(x="var", y="yhat", color="yvar", shape="yvar"), 
                          ...)
            
          }
        }else{
          
          if(ccls=="numeric"){
            gDta[[ind]] <- gDta[[ind]] +
              geom_point(aes_string(x="var", y="yhat", color="yvar", shape="yvar"),
                         ...)
            
          }else{
            gDta[[ind]] <- gDta[[ind]]+
              geom_boxplot(aes_string(x="var", y="yhat"), color="grey", 
                           ..., outlier.shape = NA)+
              geom_jitter(aes_string(x="var", y="yhat", color="yvar", shape="yvar"), 
                          ...)
            
          }
          
          gDta[[ind]] <- gDta[[ind]] + facet_grid(~outcome)
        }
      }else{
        # assume regression
        gDta[[ind]] <- gDta[[ind]] +
          labs(x=hName, y="Predicted")
        if(ccls=="numeric"){
          gDta[[ind]] <- gDta[[ind]] +
            geom_point(aes_string(x="var", y="yhat"), ...)
          
          if(smooth){
            
            gDta[[ind]] <- gDta[[ind]] +
              geom_smooth(aes_string(x="var", y="yhat"), ...)
            
          }
        }else{
          gDta[[ind]] <- gDta[[ind]]+
            geom_boxplot(aes_string(x="var", y="yhat"), color="grey", 
                         ..., outlier.shape = NA)+
            geom_jitter(aes_string(x="var", y="yhat"), 
                        ...)
        }
        # Replace the original colname
        colnames(object)[chIndx] <- hName
      }
    }
    if(lng == 1) gDta <- gDta[[1]]
  }    
  return(gDta)
}
