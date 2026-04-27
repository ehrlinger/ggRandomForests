####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
# Internal helper: normalise the which_outcome argument.
# "all" is not yet fully supported; fall back to class 1 with a warning.
.validate_which_outcome <- function(which_outcome) {
  if (identical(which_outcome, "all")) {
    warning("Must specify which_outcome for now.")
    return(1L)
  }
  which_outcome
}

#' Receiver Operator Characteristic calculator
#'
#' @details For a randomForestSRC prediction and the actual
#' response value, calculate the specificity (1-False Positive Rate) and
#' sensitivity (True Positive Rate) of a predictor.
#'
#' This is a helper function for the \code{\link{gg_roc}} functions, and
#' not intended for use by the end user.
#'
#' @param object A fitted \code{\link[randomForestSRC]{rfsrc}},
#'   \code{\link[randomForestSRC]{predict.rfsrc}}, or
#'   \code{\link[randomForest]{randomForest}} classification object containing
#'   predicted class probabilities.
#' @param dta A factor (or coercible to factor) of the true observed class
#'   labels, one per observation. Typically \code{object$yvar} for rfsrc or
#'   \code{object$y} for randomForest.
#' @param which_outcome Integer index of the class for which the ROC curve is
#'   computed (e.g. \code{1} for the first class, \code{2} for the second).
#'   Use \code{"all"} to request all classes (currently falls back to class 1
#'   with a warning).
#' @param oob Logical; if \code{TRUE} (default for rfsrc) use OOB predicted
#'   probabilities. Forced to \code{FALSE} for \code{randomForest} objects.
#' @param ... Extra arguments passed to helper functions (currently unused).
#'
#' @return A \code{gg_roc} \code{data.frame} with columns \code{sens}
#'   (sensitivity), \code{spec} (specificity), and \code{pct} (the probability
#'   threshold), with one row per unique prediction value. Suitable for passing
#'   to \code{\link{calc_auc}} or \code{\link{plot.gg_roc}}.
#'
#' @aliases calc_roc.rfsrc calc_roc.randomForest calc_roc
#'
#' @seealso \code{\link{calc_auc}} \code{\link{gg_roc}}
#' @seealso \code{\link{plot.gg_roc}}
#'
#' @importFrom parallel mclapply
#' @importFrom stats xtabs
#' @importFrom utils head tail
#' @importFrom randomForest randomForest
#'
#' @examples
#' ## Taken from the gg_roc example
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#'
#' gg_dta <- calc_roc(rfsrc_iris, rfsrc_iris$yvar,
#'   which_outcome = 1, oob = TRUE
#' )
#' gg_dta <- calc_roc(rfsrc_iris, rfsrc_iris$yvar,
#'   which_outcome = 1, oob = FALSE
#' )
#'
#' rf_iris <- randomForest(Species ~ ., data = iris)
#' gg_dta <- calc_roc(rf_iris, rf_iris$yvar,
#'   which_outcome = 1
#' )
#' gg_dta <- calc_roc(rf_iris, rf_iris$yvar,
#'   which_outcome = 2
#' )
#'
#' @export
calc_roc.rfsrc <-
  function(object,
           dta,
           which_outcome = "all",
           oob = TRUE,
           ...) {
    # Ensure response is a factor so levels() is well-defined
    if (!is.factor(dta)) {
      dta <- factor(dta)
    }

    # "all" outcomes not yet supported; fall back to the first class
    which_outcome <- .validate_which_outcome(which_outcome)
    # Build (binary indicator, full-forest prediction, OOB prediction) triplet
    dta_roc <-
      data.frame(cbind(
        res = (dta == levels(dta)[which_outcome]),
        prd = object$predicted[, which_outcome],
        oob_prd = object$predicted.oob[, which_outcome]
      ))

    # Collect the unique predicted probability thresholds for the ROC sweep
    if (oob) {
      pct <- sort(unique(object$predicted.oob[, which_outcome]))
    } else {
      pct <- sort(unique(object$predicted[, which_outcome]))
    }

    last <- length(pct)
    # Remove the maximum threshold (the cutpoint where nothing is classified
    # as positive), which produces the (sens=0, spec=1) anchor point
    pct <- pct[-last]

    # Cap at 200 threshold points to keep multi-class ROC plots manageable
    if (last > 200) {
      pct <- pct[seq(1, length(pct), length.out = 200)]
    }

    # For each threshold, build the 2×2 confusion table and extract TPR/TNR
    gg_dta <- parallel::mclapply(pct, function(crit) {
      if (oob) {
        tbl <- xtabs(~ res + (oob_prd > crit), dta_roc)
      } else {
        tbl <- xtabs(~ res + (prd > crit), dta_roc)
      }

      spec <- tbl[2, 2] / rowSums(tbl)[2]
      sens <- tbl[1, 1] / rowSums(tbl)[1]
      cbind(sens = sens, spec = spec)
    })

    gg_dta <- do.call(rbind, gg_dta)
    # Anchor curve at perfect specificity (0, 1) and perfect sensitivity (1, 0)
    gg_dta <- rbind(c(0, 1), gg_dta, c(1, 0))

    gg_dta <- data.frame(gg_dta, row.names = seq_len(nrow(gg_dta)))
    gg_dta$pct <- c(0, pct, 1)
    invisible(gg_dta)
  }

#' @export
calc_roc <- function(object,
                     dta,
                     which_outcome = "all",
                     oob = TRUE,
                     ...) {
  UseMethod("calc_roc", object)
}

## This is in development still.
#' @export
calc_roc.randomForest <-
  function(object,
           dta,
           which_outcome = 1,
           oob = FALSE,
           ...) {
    prd <- predict(object, type = "prob")

    which_outcome <- .validate_which_outcome(which_outcome)
    dta_roc <-
      data.frame(cbind(res = (dta == levels(dta)[which_outcome]), prd = prd))

    pct <- sort(unique(prd[[which_outcome]]))

    # Make sure we don't have to many points... if the training set was large,
    # This may break plotting all ROC curves in multiclass settings.
    # Arbitrarily reduce this to only include 200 points along the curve
    if (length(pct) > 200) {
      pct <- pct[seq(1, length(pct), length.out = 200)]
    }

    gg_dta <- parallel::mclapply(pct, function(crit) {
      tmp <- dta_roc[, c(1, 1 + which_outcome)]
      colnames(tmp) <- c("res", "prd")
      tbl <- xtabs(~ res + (prd > crit), tmp)

      if (dim(tbl)[2] < 2) {
        tbl <- cbind(tbl, c(0, 0))
        colnames(tbl) <- c("FALSE", "TRUE")
      }
      spec <- tbl[2, 2] / rowSums(tbl)[2]
      sens <- tbl[1, 1] / rowSums(tbl)[1]

      cbind(sens = sens, spec = spec)
    })

    gg_dta <- do.call(rbind, gg_dta)
    gg_dta <- rbind(c(0, 1), gg_dta, c(1, 0))

    gg_dta <- data.frame(gg_dta, row.names = seq_len(nrow(gg_dta)))
    gg_dta$pct <- c(0, pct, 1)
    invisible(gg_dta)
  }

#'
#' Area Under the ROC Curve calculator
#'
#' @details calc_auc uses the trapezoidal rule to calculate the area under
#' the ROC curve.
#'
#'  This is a helper function for the \code{\link{gg_roc}} functions.
#'
#' @param x \code{\link{gg_roc}} object
#'
#' @return AUC. 50\% is random guessing, higher is better.
#'
# @importFrom dplyr lead
#'
#' @seealso \code{\link{calc_roc}} \code{\link{gg_roc}}
#' @seealso \code{\link{plot.gg_roc}}
#'
#' @examples
#' ##
#' ## Taken from the gg_roc example
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#'
#' gg_dta <- gg_roc(rfsrc_iris, which_outcome = 1)
#'
#' calc_auc(gg_dta)
#'
#' gg_dta <- gg_roc(rfsrc_iris, which_outcome = 2)
#'
#' calc_auc(gg_dta)
#'
#' ## randomForest tests
#' rf_iris <- randomForest::randomForest(Species ~ ., data = iris)
#' gg_dta <- gg_roc(rfsrc_iris, which_outcome = 2)
#'
#' calc_auc(gg_dta)
#'
#' @aliases calc_auc calc_auc.gg_roc
#' @export
calc_auc <- function(x) {
  ## Trapezoidal rule:  AUC = Σ (f(x_i) + f(x_{i+1})) / 2 * |Δx|
  ## Here f(x) is sensitivity (TPR) and x is 1 - specificity (FPR).
  ## Sort so that specificity decreases (FPR increases) left-to-right,
  ## then each step moves one FPR increment to the right.

  # Sort in decreasing specificity so FPR = 1-spec increases monotonically
  x <- x[order(x$spec, decreasing = TRUE), ]

  # Δ(FPR) = -(Δspec)  — spec decreases, so (spec[i] - spec[i+1]) > 0
  # Average height of trapezoid = (sens[i] + sens[i+1]) / 2
  auc <- (x$sens + shift(x$sens)) / 2 * (x$spec - shift(x$spec)) # nolint: object_usage_linter
  sum(auc, na.rm = TRUE)
}

calc_auc.gg_roc <- calc_auc # nolint: object_name_linter
