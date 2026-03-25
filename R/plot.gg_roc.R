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
#' @param x A \code{\link{gg_roc}} object, or a raw
#'   \code{\link[randomForestSRC]{rfsrc}} classification forest or
#'   \code{\link[randomForest]{randomForest}} classification object.  When a
#'   forest is supplied, \code{\link{gg_roc}} is called automatically.
#' @param which_outcome Integer; for multi-class problems, the index of the
#'   class whose ROC curve should be plotted.  When \code{NULL} (default) and
#'   the forest has more than two classes, ROC curves for all classes are
#'   overlaid in a single plot.  For binary forests \code{NULL} defaults to
#'   class index 2.
#' @param ... Additional arguments forwarded to \code{\link{gg_roc}} when
#'   \code{x} is a raw forest object (e.g. \code{oob = FALSE}).
#'
#' @return A \code{ggplot} object.  The x-axis shows 1 − Specificity (FPR)
#'   and the y-axis shows Sensitivity (TPR).  A dashed red diagonal reference
#'   line marks the random-classifier baseline.  The AUC value is annotated
#'   on the plot for single-class curves.  Multi-class plots colour and style
#'   each class curve distinctly.
#'
#' @seealso \code{\link{gg_roc}} \code{\link{calc_roc}} \code{\link{calc_auc}}
#'   \code{\link[randomForestSRC]{rfsrc}}
#'   \code{\link[randomForest]{randomForest}}
#'
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#'
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R,
#' Rnews, 7(2):25-31.
#'
#' Ishwaran H. and Kogalur U.B. randomForestSRC: Random Forests for Survival,
#' Regression and Classification. R package version >= 3.4.0.
#' \url{https://cran.r-project.org/package=randomForestSRC}
#'
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' # Build a small classification forest (ntree=50 keeps example fast)
#' set.seed(42)
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris, ntree = 50)
#'
#' # ROC for setosa (outcome index 1)
#' gg_dta <- gg_roc(rfsrc_iris, which_outcome = 1)
#' plot(gg_dta)
#'
#' # ROC for versicolor (outcome index 2)
#' gg_dta <- gg_roc(rfsrc_iris, which_outcome = 2)
#' plot(gg_dta)
#'
#' # ROC for virginica (outcome index 3)
#' gg_dta <- gg_roc(rfsrc_iris, which_outcome = 3)
#' plot(gg_dta)
#'
#' # Plot all three ROC curves in one call by iterating over outcome indices
#' n_cls <- ncol(rfsrc_iris$predicted)
#' for (i in seq_len(n_cls)) print(plot(gg_roc(rfsrc_iris, which_outcome = i)))
#'
#' @export
plot.gg_roc <- function(x, which_outcome = NULL, ...) {
  gg_dta <- x

  ## ---- Accept a raw rfsrc or randomForest object -----------------------
  # If we call this with a forest object instead of a gg_roc object,
  # automatically compute the ROC data for the appropriate class(es).
  if (inherits(gg_dta, "rfsrc")) {
    if (inherits(gg_dta, "class")) {
      # Determine the number of outcome classes
      crv <- dim(gg_dta$predicted)[2]

      if (crv > 2 && is.null(which_outcome)) {
        # Multi-class: compute ROC for every class in parallel
        gg_dta <- mclapply(seq_len(crv), function(ind) {
          gg_roc(gg_dta, which_outcome = ind, ...)
        })
      } else {
        # Binary (or user specified a class): default to second column
        if (is.null(which_outcome)) {
          which_outcome <- 2
        }
        gg_dta <- gg_roc(gg_dta, which_outcome, ...)
      }
    } else {
      stop("gg_roc expects a classification randomForest.")
    }
  } else if (inherits(gg_dta, "randomForest")) {
    if (gg_dta$type == "classification") {
      # Determine the number of outcome classes
      crv <- length(levels(gg_dta$predicted))
      if (crv > 2 && is.null(which_outcome)) {
        # Multi-class: compute ROC for every class in parallel
        gg_dta <- parallel::mclapply(seq_len(crv), function(ind) {
          gg_roc(gg_dta, which_outcome = ind, ...)
        })
      } else {
        # Binary (or user specified a class): default to second column
        if (is.null(which_outcome)) {
          which_outcome <- 2
        }
        gg_dta <- gg_roc(gg_dta, which_outcome, ...)
      }
    }
  }

  ## ---- Single-class ROC plot ------------------------------------------
  if (inherits(gg_dta, "gg_roc")) {
    # Sort by specificity so the ROC curve is drawn left-to-right
    gg_dta <- gg_dta[order(gg_dta$spec), ]
    # False positive rate = 1 - specificity
    gg_dta$fpr <- 1 - gg_dta$spec
    auc <- calc_auc(gg_dta)

    gg_plt <- ggplot2::ggplot(data = gg_dta) +
      ggplot2::geom_line(ggplot2::aes(x = .data$fpr, y = .data$sens)) +
      ggplot2::labs(x = "1 - Specificity (FPR)", y = "Sensitivity (TPR)") +
      # Reference diagonal for a random classifier
      ggplot2::geom_abline(
        slope = 1,
        intercept = 0,
        col = "red",
        linetype = 2,
        linewidth = .5
      ) +
      ggplot2::coord_fixed()

    # Annotate the plot with the computed AUC value
    gg_plt <- gg_plt +
      ggplot2::annotate(
        x = .5,
        y = .2,
        geom = "text",
        label = paste("AUC = ", round(auc, digits = 3), sep = ""),
        hjust = 0
      )
  } else {
    ## ---- Multi-class ROC plot (list of gg_roc objects) ----------------
    # Sort each class's data by specificity
    gg_dta <- parallel::mclapply(gg_dta, function(st) {
      st <- st[order(st$spec), ]
      st
    })
    # Compute FPR for each class
    gg_dta <- parallel::mclapply(gg_dta, function(st) {
      st$fpr <- 1 - st$spec
      st
    })
    # Tag each subset with its outcome index for colour/linetype mapping
    gg_dta <- parallel::mclapply(seq_along(gg_dta), function(ind) {
      gg_dta[[ind]]$outcome <- ind
      gg_dta[[ind]]
    })

    # Compute AUC for each class
    auc <- parallel::mclapply(gg_dta, function(st) {
      calc_auc(st)
    })

    # Combine all classes into a single data frame for faceting/colouring
    o_dta <- do.call(rbind, gg_dta)
    o_dta$outcome <- factor(o_dta$outcome)

    gg_plt <- ggplot2::ggplot(data = o_dta) +
      ggplot2::geom_line(ggplot2::aes(
        x = .data$fpr,
        y = .data$sens,
        linetype = .data$outcome,
        col = .data$outcome
      )) +
      ggplot2::labs(x = "1 - Specificity (FPR)", y = "Sensitivity (TPR)") +
      # Reference diagonal for a random classifier
      ggplot2::geom_abline(
        slope = 1,
        intercept = 0,
        col = "red",
        linetype = 2,
        linewidth = .5
      ) +
      ggplot2::coord_fixed()

    # Multi-class: do not annotate a single AUC value — each class has its own.
  }
  return(gg_plt)
}
