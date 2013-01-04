####**********************************************************************
####**********************************************************************
####
####  GGPLOT FIGURES FOR RANDOM FORESTS FOR SURVIVAL, REGRESSION,
####  AND CLASSIFICATION (RF-SRC)
####  Version 0.5.0
####
####  Copyright 2012, Cleveland Clinic Foundation
####
####  This program is free software; you can redistribute it and/or
####  modify it under the terms of the GNU General Public License
####  as published by the Free Software Foundation; either version 2
####  of the License, or (at your option) any later version.
####
####  This program is distributed in the hope that it will be useful,
####  but WITHOUT ANY WARRANTY; without even the implied warranty of
####  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
####  GNU General Public License for more details.
####
####  You should have received a copy of the GNU General Public
####  License along with this program; if not, write to the Free
####  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
####  Boston, MA  02110-1301, USA.
####
####  ----------------------------------------------------------------
####  Project Partially Funded By: 
####  ----------------------------------------------------------------
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
####    URL:    http://www.kogalur.com
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#'
#' ggplot.variable Plot randomForestSRC Marginal Effect of Variables using 
#' ggplot2
#'
#' @description Plot the marginal effect of an x-variable on the class 
#' probability (classification), response (regression), 
#' mortality (survival), or the expected years lost (competing risk) from 
#' an RF-SRC analysis. 
#' 
#' @param x An object of class (rfsrc, marginal) or (rfsrc, partial) created
#' by \code{\link{pred.variable.rfsrc}}.
#' @param smooth.lines Use lowess to smooth partial plots.
#' @param ...	Further arguments passed to or from other methods.
#'
#' @details The vertical axis displays the ensemble predicted value, while 
#' x-variables are plotted on the horizontal axis.
#' 
#' @author John Ehrlinger john.ehrlinger@gmail.com
#' 
#' @return A list of ggplot figures, referenced by variable name
#' 
#' @details 
#' 
#' @references 
#' Ishwaran H. and Kogalur U.B. (2012). Random Forests for Survival, Regression and Classification (RF-SRC),  R package version 1.0.2.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R. R News 7(2), 25--31.
#'
#' Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S. (2008). Random survival forests. Ann. Appl. Statist. 2(3), 841--860.
#' 
#' Friedman J.H. (2001). Greedy function approximation: a gradient boosting machine, Ann. of Statist., 5:1189-1232.
#' 
#' Ishwaran H., Gerds, T.A. Kogalur U.B., Moore R.D., Gange S.J. and Lau B.M. (2012). Random survival forests for competing risks.
#'
#' H. Wickham. ggplot2: elegant graphics for data analysis. Springer New York, 2009.
#'
#' @seealso rfsrc, predict.rfsrc, plot.variable.rfsrc, pred.variable.rfsrc
#'
#' @examples
#' ## Not run: 
#' ### survival/CR examples
#' 
#' # survival
#' data(veteran, package = "randomForestSRC") 
#' v.obj <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
#' plot.variable(v.obj, plots.per.page = 3)
#' plot.variable(v.obj, plots.per.page = 2, xvar.names = c("trt", "karno", "age"))
#' plot.variable(v.obj, surv.type = "surv", nvar = 1, percentile = 75)
#' plot.variable(v.obj, surv.type = "surv", nvar = 1, time=5) 
#' plot.variable(v.obj, surv.type = "surv", partial = TRUE, smooth.lines = TRUE)
#' plot.variable(v.obj, surv.type = "rel.freq", partial = TRUE, nvar = 2)
#' 
#' # competing risks
#' data(follic, package = "randomForestSRC")
#' follic.obj <- rfsrc(Surv(time, status) ~ ., follic, nsplit = 3, ntree = 100)
#' plot.variable(follic.obj, which.outcome = 2)
#' 
#' ### regression examples
#' 
#' # airquality
#' airq.obj <- rfsrc(Ozone ~ ., data = airquality)
#' plot.variable(airq.obj, partial = TRUE, smooth.lines = TRUE)
#' 
#' # motor trend cars
#' mtcars.obj <- rfsrc(mpg ~ ., data = mtcars)
#' plot.variable(mtcars.obj, partial = TRUE, smooth.lines = TRUE)
#' 
#' ### classification example
#' 
#' # iris
#' iris.obj <- rfsrc(Species ~., data = iris)
#' plot.variable(iris.obj, partial = TRUE)
#' 
#' # motor trend cars: predict number of carburetors
#' mtcars2 <- mtcars
#' mtcars2$carb <- factor(mtcars2$carb,
#'                        labels = paste("carb", sort(unique(mtcars$carb))))
#' mtcars2.obj <- rfsrc(carb ~ ., data = mtcars2)
#' plot.variable(mtcars2.obj, partial = TRUE)
#' 
#' ## End(Not run)

ggplot.variable.rfsrc <- function(
  x,
  smooth.lines = FALSE,
  ...)
{
  call <- match.call()
  ### check that object is interpretable
  ### first rename x to object to avoid confusion with x matrix
  object <- x
  if (sum(inherits(object, c("marginal", "rfsrc"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("partial", "rfsrc"), TRUE) == c(1, 2)) != 2) {
    stop("Function only works for objects of class '(rfsrc, marginal)' or '(rfsrc, partial)'. These objects are created with the pred.variable function.")
  }
  
  # assign missing values to key options
  if(is.null(object$call$percentile)){
    percentile <- 50
  }else{
    percentile <- object$call$percentile
  }
  
  ## process the object depending on the underlying family
  ##survival families
  if (grepl("surv", object$family)) {
    event.info <- object$event.info
    yvar.dim <- event.info$r.dim
    cens <- as.factor(event.info$cens)
    event.type <- event.info$event.type
    
    if (percentile > 1) percentile <- percentile / 100
    if (percentile < 0 | percentile > 1) percentile <- 0.5
    
    ## special processing for  CR analysis
    if (object$family == "surv-CR") {
      if(is.null(object$which.outcome)){
        which.outcome <- 1
      }else{
        which.outcome <- object$which.outcome 
      } 
      if (which.outcome < 1 || which.outcome > max(event.type, na.rm = TRUE)) {
        stop("'which.outcome' is specified incorrectly")
      }
      
      surv.type <- setdiff(object$call$surv.type, c("mort", "rel.freq", "surv"))
      pred.type <- match.arg(surv.type, c("years.lost", "cif", "chf"))
      if(is.null(object$call$time)){
        ylabel <- switch(pred.type,
                         "years.lost" = paste("Years lost for event ", which.outcome),
                         "cif" = paste("CIF for event ", which.outcome, " (", round(100 * percentile), "%)", sep = ""),
                         "chf" = paste("CHF for event ", which.outcome, " (", round(100 * percentile), "%)", sep = ""))
      }else{
        ylabel <- switch(pred.type,
                         "years.lost" = paste("Years lost for event ", which.outcome),
                         "cif" = paste("CIF for event ", which.outcome, sep = ""),
                         "chf" = paste("CHF for event ", which.outcome, sep = ""))
      }     
    }
    else {
      which.outcome <- 1
      surv.type <- setdiff(object$call$surv.type, c("years.lost", "cif", "chf"))
      pred.type <- match.arg(object$call$surv.type, c("mort", "rel.freq", "surv"))
      if(is.null(object$call$time)){
        ylabel <- switch(pred.type,
                         "mort"      = "mortality",
                         "rel.freq"  = "standardized mortality",
                         "surv"      = paste("predicted survival (", round(100 * percentile), "%)", sep = ""))
      }else{
        ylabel <- switch(pred.type,
                         "mort"      = "mortality",
                         "rel.freq"  = "standardized mortality",
                         "surv"      = paste("predicted survival", sep = ""))
      }
    }
  }
  ## classification families
  else {
    if (object$family == "class") {
      if (is.null(object$which.outcome)) {
        which.outcome <- 1
      }else{
        which.outcome <- object$which.outcome
      }
      if (is.character(which.outcome)) {
        which.outcome <- match(match.arg(which.outcome, levels(object$yvar)), levels(object$yvar))
      }
      else {
        if (which.outcome > length(levels(object$yvar)) | which.outcome < 1) {
          stop("which.outcome is specified incorrectly: ", which.outcome, " of ", length(levels(object$yvar)) )
        }
      }
      pred.type <- "prob"
      yvar.dim <- 1
      ylabel <- paste("probability", levels(object$yhat)[which.outcome])
    }
    ## regression families
    else {
      pred.type <- "y"
      yvar.dim <- 1
      ylabel <- expression(hat(y))
    }
  }
  
  ##--------------------------------------------------------------------------------
  ##
  ## Marginal Plots
  ##
  ##--------------------------------------------------------------------------------
  if (is.null(object$call$partial)) {
    xvar <- object$x
    n <- nrow(xvar)
    nvar <- ncol(xvar)
    yhat <- object$yhat
    if (n > 500) cex.pt <- 0.5 else cex.pt <- 0.75
    plt <- vector("list", length=nvar)
    
    # So that we can reference the plots by the variable name
    names(plt) <- colnames(xvar)
    for (k in 1:nvar) {
      x <- xvar[, k]
      x.uniq <- unique(x)
      n.x <- length(x.uniq)
      if (!is.factor(x)) {
        if (grepl("surv", object$family)) {
          dta<- as.data.frame(cbind(x, yhat, cens))
          plt[[k]] <- ggplot(dta)+geom_point(aes(x=x, y=yhat, color=cens), pch = 16)+theme_bw()+
            labs(x=colnames(xvar)[k], y=ylabel)+
            scale_fill_brewer(type="qual",palette="Set1", na.value = "lightgrey", labels=c("Dead", "Cens"))+
            guides(fill=guide_legend(title="Events")) + geom_smooth(aes(x=x, y=yhat), col = 2, lwd=3) 
        }else{
          dta<- as.data.frame(cbind(x, yhat))
          plt[[k]] <- ggplot(dta)+geom_point(aes(x=x, y=yhat), pch = 16)+theme_bw()+
            labs(x=colnames(xvar)[k], y=ylabel)+
            geom_smooth(aes(x=x, y=yhat), col = 2)
        }
        
      }
      else {
        if (is.factor(x)) x <- factor(x, exclude = NULL)
        
        dta<- as.data.frame(cbind(x, yhat))
        plt[[k]] <- ggplot(dta) +geom_boxplot(aes(y=yhat, x=factor(x)), notch = TRUE, fill="bisque")+
          theme_bw()+
          labs(x=colnames(xvar)[k], y=ylabel)
      }
      if (grepl("surv", object$family)) {
        plt[[k]]<- plt[[k]] + scale_y_continuous(breaks=seq(0,100,5)) +
          coord_cartesian(ylim=c(0,100))
      }
      show(plt[[k]])
    }
  }
  ##--------------------------------------------------------------------------------
  ##
  ## Partial Plots
  ##
  ##--------------------------------------------------------------------------------
  else {
    nvar <- length(object$partial)
    plt <- vector("list", length=nvar)
    
    for (k in 1:nvar) {
      x <- object$partial[[k]]$x
      yhat <- object$partial[[k]]$yhat
      yhat.se <- object$partial[[k]]$yhat.se
      x.uniq <- object$partial[[k]]$xhat
      name <- object$partial[[k]]$name
      
      # So that we can reference the plots by the variable name
      name(plt[[k]]) <- name
      
      n.x <- object$partial[[k]]$n.x
      if (n.x > 25) cex.pt <- 0.5 else cex.pt <- 0.75
      factor.x <- !(length(x.uniq) == length(yhat))
      
      if (!factor.x) {
        dta <- as.data.frame(cbind(x.uniq=x.uniq, yhat=yhat, yhat.se=yhat.se))
        plt[[k]] <- ggplot(dta)+ geom_point(aes(x=x.uniq, y=yhat), col=2, pch=16)+
          labs(x=name, y=ylabel) + theme_bw()+
          geom_rug(aes(x=x), data=as.data.frame(cbind(x=x)),side="b")
        
        if (!is.na(yhat.se) && any(yhat.se > 0)) {
          if (smooth.lines) {
            plt[[k]] <- plt[[k]] +
              geom_smooth(aes(x=x.uniq, y=yhat + 2 * yhat.se), lty=3, col=2, se=FALSE)+
              geom_smooth(aes(x=x.uniq, y=yhat - 2 * yhat.se), lty=3, col=2, se=FALSE)
          }
          else {
            plt[[k]] <- plt[[k]] +
              geom_line(aes(x=x.uniq, y=yhat + 2 * yhat.se), lty=3, col=2)+
              geom_line(aes(x=x.uniq, y=yhat - 2 * yhat.se), lty=3, col=2)
          }
        }
        if (smooth.lines) {
          plt[[k]] <- plt[[k]] +
            geom_smooth(aes(x=x.uniq, y=yhat), lty=2)
        }
        else {
          plt[[k]] <- plt[[k]] +
            geom_line(aes(x=x.uniq, y=yhat), lty=2)
        }
      }
      else {
        n <- length(x)
        
        dta<- as.data.frame(cbind(x=rep(x.uniq, rep(n, n.x)), yhat))
        plt[[k]] <- ggplot(dta) +geom_boxplot(aes(y=yhat, x=factor(x)), notch = TRUE, fill="bisque")+
          theme_bw()+
          labs(x=name, y=ylabel)
      }
      if (grepl("surv", object$family)) {
        plt[[k]]<- plt[[k]] + scale_y_continuous(breaks=seq(0,100,5)) +
          coord_cartesian(ylim=c(0,100))
      }
      show(plt[[k]])
    }
  }
  
  # return the ggplot objects. Useful for customizing the graphics.
  invisible(plt)
}

ggplot.variable <- ggplot.variable.rfsrc
