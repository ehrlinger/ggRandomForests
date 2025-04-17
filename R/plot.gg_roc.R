####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#'
#' ROC plot generic function for a \code{\link{gg_roc}} object.
#'
#' @param x \code{\link{gg_roc}} object created from a classification forest
#' @param which_outcome for multiclass problems, choose the class for plotting
#' @param ... arguments passed to the \code{\link{gg_roc}} function
#'
#' @return \code{ggplot} object of the ROC curve
#'
#' @seealso \code{\link{gg_roc}} rfsrc
#'
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#'
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R,
#' Rnews, 7(2):25-31.
#'
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival,
#' Regression and Classification (RF-SRC), R package version 1.4.
#'
#' @examples
#' \dontrun{
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' #rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' data(rfsrc_iris, package="ggRandomForests")
#'
#' # ROC for setosa
#' gg_dta <- gg_roc(rfsrc_iris, which_outcome=1)
#' plot.gg_roc(gg_dta)
#'
#' # ROC for versicolor
#' gg_dta <- gg_roc(rfsrc_iris, which_outcome=2)
#' plot.gg_roc(gg_dta)
#'
#' # ROC for virginica
#' gg_dta <- gg_roc(rfsrc_iris, which_outcome=3)
#' plot.gg_roc(gg_dta)
#'
#' # Alternatively, you can plot all three outcomes in one go
#' # by calling the plot function on the forest object.
#' plot.gg_roc(rfsrc_iris)
#'
#' }
#'
#'
#' @export
plot.gg_roc <- function(x, which_outcome = NULL, ...) {
  gg_dta <- x
  
  #If we call this with a forest object instead of a gg_roc object
  if (inherits(gg_dta, "rfsrc")) {
    if (inherits(gg_dta, "class")) {
      # How many classes are there?
      crv <- dim(gg_dta$predicted)[2]
      
      if (crv > 2 && is.null(which_outcome)) {
        gg_dta <- mclapply(1:crv, function(ind) {
          gg_roc(gg_dta, which_outcome = ind, ...)
        })
        
      } else {
        if (is.null(which_outcome))
          which_outcome <- 2
        gg_dta <- gg_roc(gg_dta, which_outcome, ...)
      }
    } else {
      stop("gg_roc expects a classification randomForest.")
    }
  } else if (inherits(gg_dta, "randomForest")) {
    if (gg_dta$type == "classification") {
      # How many classes are there?
      crv <- length(levels(gg_dta$predicted))
      if (crv > 2 && is.null(which_outcome)) {
        gg_dta <- parallel::mclapply(1:crv, function(ind) {
          gg_roc(gg_dta, which_outcome = ind, ...)
        })
        
      } else {
        if (is.null(which_outcome))
          which_outcome <- 2
        gg_dta <- gg_roc(gg_dta, which_outcome, ...)
      }
    }
  }
  
  #
  if (inherits(gg_dta, "gg_roc")) {
    gg_dta <- gg_dta[order(gg_dta$spec), ]
    gg_dta$fpr <- 1 - gg_dta$spec
    auc <- calc_auc(gg_dta)
    
    gg_plt <- ggplot2::ggplot(data = gg_dta) +
      ggplot2::geom_line(ggplot2::aes(x = "fpr", y = "sens")) +
      ggplot2::labs(x = "1 - Specificity (FPR)", y = "Sensitivity (TPR)") +
      ggplot2::geom_abline(
        slope = 1,
        intercept = 0,
        col = "red",
        linetype = 2,
        linewidth = .5
      ) +
      ggplot2::coord_fixed()
    
    gg_plt <- gg_plt +
      ggplot2::annotate(
        x = .5,
        y = .2,
        geom = "text",
        label = paste("AUC = ", round(auc, digits = 3), sep = ""),
        hjust = 0
      )
    
  } else {
    gg_dta <- parallel::mclapply(gg_dta, function(st) {
      st[order(st$spec), ]
      st
    })
    gg_dta <- parallel::mclapply(gg_dta, function(st) {
      st$fpr <- 1 - st$spec
      st
    })
    gg_dta <- parallel::mclapply(seq_len(length(gg_dta)), function(ind) {
      gg_dta[[ind]]$outcome <- ind
      gg_dta[[ind]]
    })
    
    auc <- parallel::mclapply(gg_dta, function(st) {
      calc_auc(st)
    })
    
    o_dta <- do.call(rbind, gg_dta)
    o_dta$outcome <- factor(o_dta$outcome)
    
    gg_plt <- ggplot2::ggplot(data = o_dta) +
      ggplot2::geom_line(ggplot2::aes(
        x = "fpr",
        y = "sens",
        linetype = "outcome",
        col = "outcome"
      )) +
      ggplot2::labs(x = "1 - Specificity (FPR)", y = "Sensitivity (TPR)") +
      ggplot2::geom_abline(
        slope = 1,
        intercept = 0,
        col = "red",
        linetype = 2,
        linewidth = .5
      ) +
      ggplot2::coord_fixed()
    
    if (crv < 2) {
      gg_plt <- gg_plt +
        ggplot2::annotate(
          x = .5,
          y = .2,
          geom = "text",
          label = paste("AUC = ", round(auc, digits = 3), sep = ""),
          hjust = 0
        )
    }
  }
  return(gg_plt)
}
