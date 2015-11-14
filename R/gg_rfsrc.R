####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
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
#' Predicted response data object
#' 
#' Extracts the predicted response values from the \code{\link[randomForestSRC]{rfsrc}}
#'  object, and formats data for plotting the response using \code{\link{plot.gg_rfsrc}}.
#' 
#' @param object \code{\link[randomForestSRC]{rfsrc}} object
#' @param by stratifying variable in the training dataset, defaults to NULL
#' @param oob boolean, should we return the oob prediction , or the full
#' forest prediction.
#' @param ... extra arguments
#' 
#' @return \code{gg_rfsrc} object
#' 
#' @details 
#'    \code{surv_type} ("surv", "chf", "mortality", "hazard") for survival forests
#'    
#'    \code{oob} boolean, should we return the oob prediction , or the full
#' forest prediction.
#' 
#' @seealso \code{\link{plot.gg_rfsrc}} \code{rfsrc} \code{plot.rfsrc} 
#' \code{\link{gg_survival}}
#' 
#' @importFrom tidyr gather_
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' data(rfsrc_iris, package="ggRandomForests")
#' gg_dta<- gg_rfsrc(rfsrc_iris)
#' 
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' \dontrun{
#' ## -------- air quality data
#' # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' data(rfsrc_airq, package="ggRandomForests")
#' gg_dta<- gg_rfsrc(rfsrc_airq)
#' 
#' plot(gg_dta)
#' }
#' 
#' ## -------- Boston data
#' data(rfsrc_Boston, package="ggRandomForests")
#' plot(rfsrc_Boston) 
#' 
#' \dontrun{
#' ## -------- mtcars data
#' data(rfsrc_mtcars, package="ggRandomForests")
#' gg_dta<- gg_rfsrc(rfsrc_mtcars)
#' 
#' plot(gg_dta)
#' }
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' \dontrun{
#' ## -------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRC")
#' # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' data(rfsrc_veteran, package = "ggRandomForests")
#' gg_dta <- gg_rfsrc(rfsrc_veteran)
#' plot(gg_dta)
#' 
#' gg_dta <- gg_rfsrc(rfsrc_veteran, conf.int=.95)
#' plot(gg_dta)
#' 
#' gg_dta <- gg_rfsrc(rfsrc_veteran, by="trt")
#' plot(gg_dta)
#' }
#' 
#' ## -------- pbc data
#' ## We don't run this because of bootstrap confidence limits
#' data(rfsrc_pbc, package = "ggRandomForests")
#' 
#' \dontrun{
#' gg_dta <- gg_rfsrc(rfsrc_pbc)
#' plot(gg_dta)
#' 
#' gg_dta <- gg_rfsrc(rfsrc_pbc, conf.int=.95)
#' plot(gg_dta)
#' }
#' 
#' gg_dta <- gg_rfsrc(rfsrc_pbc, by="treatment")
#' plot(gg_dta)
#'
#' 
#' @aliases gg_rfsrc gg_rfsrc.rfsrc

#' @export 
gg_rfsrc.rfsrc <- function(object, 
                           oob=TRUE, 
                           by, 
                           ...) {
  
  ## Check that the input obect is of the correct type.
  if (inherits(object, "rfsrc") == FALSE){
    stop(paste("This function only works for Forests grown with the", 
               "randomForestSRC package."))
  }
  if (is.null(object$forest)) {
    stop(paste("The function requires the \"forest = TRUE\"",
               "attribute when growing the randomForest"))
  }
  
  # get optional arguments
  arg_list <- list(...)
  
  if (inherits(object, "predict")){
    oob <- FALSE
  }
  
  if(!missing(by)){
    grp <- by
    # If the by argument is a vector, make sure it is the correct length
    if(is.character(grp)){
      if(is.null(object$xvar[,grp])){
        stop(paste("No column named", grp, "in forest training set." ))
      }else{
        grp <- object$xvar[,grp]
      }
    }
    
    if(is.vector(grp) | is.factor(grp)){
      if(length(grp) != nrow(object$xvar))
        stop(paste("By argument does not have the correct dimension ", 
                   nrow(object$xvar)))
    }else{
      stop(paste("By argument should be either a vector, or colname",
                 "of training data", 
                 nrow(object$xvar)))
    }
    grp <- factor(grp, levels=unique(grp))
  }
  
  
  if(object$family == "class"){
    ### Classification models...
    
    # Need to add multiclass methods
    if(oob){
      gg_dta <- 
        if(ncol(object$predicted.oob) <= 2){
          data.frame(cbind(object$predicted.oob[,-1]))
        }else{ 
          data.frame(cbind(object$predicted.oob))
        }
    }else{
      gg_dta <- if(ncol(object$predicted) <= 2){
        data.frame(cbind(object$predicted[,-1]))
      }else{ 
        data.frame(cbind(object$predicted))
      }
    }
    
    # Switch between logical or categorical outcome
    if(ncol(gg_dta) == 1){
      colnames(gg_dta) <- object$yvar.names
      # Force this to logical return value... 
      #
      # This may be a bug in rfsrc, as it converts all classification models
      # into factors.
      gg_dta$y <- as.logical(as.numeric(object$yvar) - 1)
    }else{
      colnames(gg_dta) <- levels(object$yvar)
      gg_dta$y <- object$yvar
      
    }
    
    # Handle the "by" argument.
    if(!missing(by)) gg_dta$group <- grp
    
  }else if(object$family == "surv"){
    
    ### Survival models
    surv_type <- "surv"
    
    if(!is.null(arg_list$surv_type)) surv_type <- arg_list$surv_type
    
    if(oob){
      rng <- switch(surv_type,
                    surv=data.frame(object$survival.oob),
                    chf=data.frame(object$chf.oob),
                    mortality =data.frame(1 - object$survival.oob),
                    stop(paste(surv_type, " not implemented at this time"))
      )
    }else{
      rng <- switch(surv_type,
                    surv=data.frame(object$survival),
                    chf=data.frame(object$chf),
                    mortality =data.frame(1 - object$survival),
                    stop(paste(surv_type, " not implemented at this time"))
      )
    }
    
    # Do we want all lines, or bootstrap confidence bands.
    colnames(rng) <- object$time.interest
    
    rng$ptid <- 1:nrow(rng)
    rng$cens <- as.logical(object$yvar[,2])
    gg_dta <- rng
    
    # If we don't specify either a conf band or group by variable... 
    # Then we want to plot a curve for each observation.
    if(is.null(arg_list$conf.int) & missing(by)){
      gathercols <- colnames(gg_dta)[-which(colnames(gg_dta) %in% c("ptid", "cens"))]
      gg_dta.mlt <- tidyr::gather_(gg_dta, "variable", "value", gathercols)
      gg_dta.mlt$variable <- as.numeric(as.character(gg_dta.mlt$variable))
      gg_dta.mlt$ptid <- factor(gg_dta.mlt$ptid)
      
      gg_dta <- gg_dta.mlt
      
    }else{
      
      level <- if(is.null(arg_list$conf.int)) .95
      else arg_list$conf.int
      
      # If we have one value, then it's two sided.
      if(length(level) == 1 ){
        if (level > 1)
          level <- level / 100
        
        level.set <- c( (1 - level) / 2, 1 - (1 - level) / 2)
        level.set <- sort(level.set) 
      }else{
        level.set <- sort(level) 
      }
      
      if(is.null(arg_list$bs.sample))
        bs.samples <- nrow(gg_dta)
      else{
        bs.samples <- arg_list$bs.sample 
      }
      
      
      if(!missing(by)){
        gg_dta$group <- grp
        #####
        grp_dta <- lapply(levels(grp), function(st){
          if(is.null(arg_list$bs.sample))
            bs.samples <- nrow(gg_dta[which(as.character(gg_dta$group) == st),])
          
          obj <- bootstrap_survival(gg_dta[which(as.character(gg_dta$group) == st),], 
                                    bs.samples, level.set)
          obj$group <- st
          obj
        })
        gg_grp <- do.call(rbind, grp_dta)
        gg_grp$group <- factor(gg_grp$group,
                               levels=unique(gg_grp$group))
        gg_dta <- gg_grp
      }else{
        gg_dta <- bootstrap_survival(gg_dta, bs.samples, level.set)
      }
    }
    
  }else if(object$family == "regr"){
    
    # Need to add multiclass methods
    if(oob){
      gg_dta <- data.frame(cbind(object$predicted.oob, object$yvar))
    }else{
      gg_dta <- data.frame(cbind(object$predicted, object$yvar))
    }
    
    colnames(gg_dta) <- c("yhat", object$yvar.names)
    
    # Handle the "by" argument.
    if(!missing(by)) gg_dta$group <- grp
    
  }else{
    stop(paste("Plotting for ", object$family, " randomForestSRC is not yet implemented.", sep=""))
  }
  
  class(gg_dta) <- c("gg_rfsrc", class(gg_dta), object$family)
  invisible(gg_dta)
}



bootstrap_survival <- function(gg_dta, bs.samples, level.set){
  ## Calculate the leave one out estimate of the mean survival
  gg.t <- gg_dta[, -which(colnames(gg_dta) %in% c("ptid","cens", "group"))]
  mn.bs <- t(sapply(1:bs.samples, 
                    function(pat){
                      st <- sample(1:nrow(gg.t), size = nrow(gg.t), replace=T)
                      colMeans(gg.t[st,])
                    }))
  
  ## now get the confidence interval of the mean, and the median (.5)
  rng <- sapply(1:ncol(mn.bs), 
                function(t_pt){
                  quantile(mn.bs[,t_pt], probs=c(level.set, .5) )
                })
  mn <- sapply(1:ncol(rng), function(t_pt){
    mean(rng[,t_pt])
  })
  
  time.interest <- as.numeric(colnames(gg.t))
  
  dta <- data.frame(cbind(time.interest,
                          t(rng)[-which(colnames(gg_dta) %in% c("ptid", "cens")),],
                          mn[-which(colnames(gg_dta) %in% c("ptid", "cens"))]))
  
  if(ncol(dta) == 5){
    colnames(dta)<- c("time", "lower",  "upper", "median", "mean")
  }else{
    colnames(dta)<- c("time", level.set, "mean")
  }
  dta
}

#' @export
# gg_rfsrc <- function (object, ...) {
#   UseMethod("gg_rfsrc", object)
# }
gg_rfsrc <- gg_rfsrc.rfsrc
