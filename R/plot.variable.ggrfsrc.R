####**********************************************************************
####**********************************************************************
####
####  GGRFSRC - GGPLOT2 GRAPHICS FOR RANDOM FORESTS FOR SURVIVAL, 
####  REGRESSION, AND CLASSIFICATION (RF-SRC)
####  Version 0.6.0
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
####    URL:    https://github.com/ehrlinger/ggrfsrc
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' @title Plot the marginal dependence of variables.
#' 
#' @description plot.variable.ggrfsrc generates a list of either marginal variable 
#' dependance or partial variable dependence figures using \code{\link{ggplot}}.
#' 
#' @param x a marginal or partial rfsrc data object from \code{\link{pred.variable}}
#' @param smooth.lines boolean indicating the inclusion of confidence intervals
#'
#' @return A list of \code{\link{ggplot2}} plot objects corresponding the variables 
#' contained within the \code{x} argument 
#' 
#' @seealso \code{\link{pred.variable}}, \code{\link{plot.variable}}
#' 
#' @export plotgg.variable.ggrfsrc
#' @export plotgg.variable
#' 
#'
plot.variable.ggrfsrc <- function(
  x,
  smooth.lines = FALSE,
  ...)
{
  call <- match.call()
  ### check that object is interpretable
  ### first rename x to object to avoid confusion with x matrix
  object <- x
  if (sum(inherits(object, c("plot.variable", "rfsrc"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2&
        sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("Function only works for objects of class 'rfsrc' or (rfsrc,plot.variable)' These objects are created with the pred.variable function.")
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
        
        # You ONLY want to scale continuous plots to 0,1. Scaling boxplots hides to much
        # information right now. It is best to scale the boxplots, as required on the
        # returned plot objects.
        if (grepl("surv", object$family)) {
          plt[[k]]<- plt[[k]] 
#           + scale_y_continuous(breaks=seq(0,100,5)) +
#             coord_cartesian(ylim=c(0,100))
        }
      }
      else {
        if (is.factor(x)) x <- factor(x, exclude = NULL)
        
        dta<- as.data.frame(cbind(x, yhat))
        plt[[k]] <- ggplot(dta) +geom_boxplot(aes(y=yhat, x=factor(x)), notch = TRUE, fill="bisque")+
          theme_bw()+
          labs(x=colnames(xvar)[k], y=ylabel)
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
      #name(plt[[k]]) <- name
      
      n.x <- object$partial[[k]]$n.x
      if (n.x > 25) cex.pt <- 0.5 else cex.pt <- 0.75
      factor.x <- !(length(x.uniq) == length(yhat))
      
      if (!factor.x) {
        dta <- as.data.frame(cbind(x.uniq=x.uniq, yhat=yhat, yhat.se=yhat.se))
        plt[[k]] <- ggplot(dta)+ geom_point(aes(x=x.uniq, y=yhat), col=2, pch=16, size=4)+
          labs(x=name, y=ylabel) + theme_bw()#+
  #        geom_rug(aes(x=x), data=as.data.frame(cbind(x=x)),side="b")
        
        if (!is.na(yhat.se) && any(yhat.se > 0)) {
          if (smooth.lines) {
            plt[[k]] <- plt[[k]] +
              geom_smooth(aes(x=x.uniq, y=yhat + 2 * yhat.se), lty=3, col=2, se=FALSE)+
              geom_smooth(aes(x=x.uniq, y=yhat - 2 * yhat.se), lty=3, col=2, se=FALSE)
          }
#           else {
#             plt[[k]] <- plt[[k]] +
#               geom_line(aes(x=x.uniq, y=yhat + 2 * yhat.se), lty=3, col=2)+
#               geom_line(aes(x=x.uniq, y=yhat - 2 * yhat.se), lty=3, col=2)
#           }
        }
        if (smooth.lines) {
          plt[[k]] <- plt[[k]] +
            geom_smooth(aes(x=x.uniq, y=yhat), lty=2, lwd=1, color="yellow", se=FALSE,span = 0.9)
        }
#         else {
#           plt[[k]] <- plt[[k]] +
#             geom_line(aes(x=x.uniq, y=yhat), lty=2, lwd=1, color="yellow")
#         }
 
        # You ONLY want to scale continuous plots to 0,1. Scaling boxplots hides to much
        # information right now. It is best to scale the boxplots, as required on the
        # returned plot objects.
        if (grepl("surv", object$family)) {
          plt[[k]]<- plt[[k]] 
#           + scale_y_continuous(breaks=seq(0,100,5)) +
#             coord_cartesian(ylim=c(0,100))
        }
      }
      else {
        n <- length(x)
        
        dta<- as.data.frame(cbind(x=rep(x.uniq, rep(n, n.x)), yhat))
        plt[[k]] <- ggplot(dta) +geom_boxplot(aes(y=yhat, x=factor(x)), notch = TRUE, fill="bisque")+
          theme_bw()+
          labs(x=name, y=ylabel)
      }
    
      show(plt[[k]])
    }
  }
  
  # return the ggplot objects. Useful for customizing the graphics.
  invisible(plt)
}

plotgg.variable <- plotgg.variable.ggrfsrc
