####**********************************************************************
####**********************************************************************
####
####  RANDOM FORESTS FOR SURVIVAL, REGRESSION, AND CLASSIFICATION (RF-SRC)
####  Version 1.0.2
####
####  Copyright 2012, University of Miami
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
####  Dr. Ishwaran's work was funded in part by DMS grant 1148991 from the
####  National Science Foundation and grant R01 CA163739 from the National
####  Cancer Institute.
####
####  Dr. Kogalur's work was funded in part by grant R01 CA163739 from the 
####  National Cancer Institute.
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    Hemant Ishwaran, Ph.D.
####    Director of Statistical Methodology
####    Professor, Division of Biostatistics
####    Clinical Research Building, Room 1058
####    1120 NW 14th Street
####    University of Miami, Miami FL 33136
####
####    email:  hemant.ishwaran@gmail.com
####    URL:    http://web.ccs.miami.edu/~hishwaran
####    --------------------------------------------------------------
####    Udaya B. Kogalur, Ph.D.
####    Adjunct Staff
####    Dept of Quantitative Health Sciences
####    Cleveland Clinic Foundation
####    
####    Kogalur & Company, Inc.
####    5425 Nestleway Drive, Suite L1
####    Clemmons, NC 27012
####
####    email:  kogalurshear@gmail.com
####    URL:    http://www.kogalur.com
####    --------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#'
#' plot.rfsrc
#' 
#' @export plot.rfsrc.ggrfsrc

plot.rfsrc.ggrfsrc <- function (x, plots.one.page = TRUE, sorted = TRUE, verbose = TRUE, ...)
{
  if (sum(inherits(x, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
      sum(inherits(x, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, predict)'.")
  }
  
  ### grow objects under non-standard bootstrapping are devoid of
  ### performance values
  if (is.null(x$err.rate)) {
    stop("object is devoid of performance values")
  }

### set importance to NA if it is NULL
  if (is.null(x$importance)) {
    x$importance <- NA
  }

  ### return when everything is NA
  if (all(is.na(x$err.rate)) & all(is.na(x$importance))) {
    stop("performance values are all NA")
  }

  ### save par for later restoration
  old.par <- par(no.readonly = TRUE)
  cex <- par("cex")
  
  on.exit(par(old.par))

  ### decide what plots to generate
  if (all(is.na(x$importance))) {
    if (x$ntree > 1 && !all(is.na(x$err.rate))) {
      err <- cbind(x$err.rate)  
      par(cex = cex, mfrow = c(1,1))
      plot.err(err)    
    }
  }
  else {
    ### convert err/vimp to matrix format
    err <- cbind(x$err.rate)  
    imp <- cbind(x$importance)
    x.var.names <- rownames(imp)
    n.pred <- nrow(imp)
    if (sorted) pred.order <- order(imp[, 1]) else pred.order <- n.pred:1
    if (ncol(imp) == 1) max.pred <- 100 else max.pred <- 80/ncol(imp)
    if (n.pred > max.pred) {
      dotchart.labels <- rep("",n.pred)
      pretty.pt <- pretty(1:n.pred, n = max.pred)
      dotchart.labels[pretty.pt] <- x.var.names[pred.order][pretty.pt]
    }
    else {
      dotchart.labels <- x.var.names[pred.order]
    }
    if (x$ntree > 1 & !all(is.na(x$err.rate)) & plots.one.page) {
      par(cex = cex, mfrow = c(1,2))
    }
    else {
      par(cex = cex, mfrow = c(1,1))
    }
    if (x$ntree > 1 & !all(is.na(x$err.rate))) {
      plot.err(err)
    }
    ### CR/classification scenarios
    if (ncol(imp) > 1) {
      imp.out <- imp[rev(pred.order),, drop = FALSE]
      dotChart(imp[pred.order,, drop = FALSE], dotchart.labels, cex = cex)
    }
    ### other scenarios
    if (ncol(imp) == 1) {
      dotChart(imp[pred.order, ], dotchart.labels, cex = cex)
      if (!is.null(x$xvar.wt) & length(unique(x$xvar.wt)) > 1 ) {
        if (length(unique(x$xvar.wt)) == 1) x$xvar.wt <- 1
        imp.out <- as.data.frame(cbind(imp, imp/max(abs(imp), na.rm = TRUE), x$xvar.wt),
          row.names = x.var.names)[rev(pred.order),]
        if (nrow(imp.out) == 1) imp.out[1 , 2] <- 1
        colnames(imp.out) <- c("Importance","Relative Imp","xvar weight")
      }
      else {
        imp.out=as.data.frame(cbind(imp, imp/max(abs(imp), na.rm = TRUE)),
          row.names=x.var.names)[rev(pred.order),]
        if (nrow(imp.out) == 1) imp.out[1 , 2] <- 1
        colnames(imp.out) <- c("Importance","Relative Imp")
      }
    }
    cat("\n")
    if (verbose) {
      print(round(imp.out[1:min(n.pred, max.pred),, drop = FALSE],4), justify="right", print.gap=3)
    }    
  } 
}

