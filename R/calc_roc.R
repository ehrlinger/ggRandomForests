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
# Internal helper: normalize the which_outcome argument.
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
#' @importFrom stats xtabs
#' @importFrom utils head tail
#' @importFrom randomForest randomForest
#'
#' @examples
#' ## Taken from the gg_roc example
#' rfsrc_iris <- randomForestSRC::rfsrc(Species ~ ., data = iris)
#'
#' gg_dta <- calc_roc(rfsrc_iris, rfsrc_iris$yvar,
#'   which_outcome = 1, oob = TRUE
#' )
#' gg_dta <- calc_roc(rfsrc_iris, rfsrc_iris$yvar,
#'   which_outcome = 1, oob = FALSE
#' )
#'
#' rf_iris <- randomForest::randomForest(Species ~ ., data = iris)
#' # randomForest stores the response in $y (rfsrc uses $yvar); pass the
#' # original training factor so calc_roc has the class labels.
#' gg_dta <- calc_roc(rf_iris, iris$Species,
#'   which_outcome = 1
#' )
#' gg_dta <- calc_roc(rf_iris, iris$Species,
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
    gg_dta <- lapply(pct, function(crit) {
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

## randomForest ROC: OOB vote probabilities by default; macro-average for
## the overall ("all"/0) case. Does NOT use the shared
## .validate_which_outcome (rfsrc path unchanged). See #81.
##
## The body is intentionally thin: heavy logic lives in unexported
## helpers (.rf_prob_matrix, .normalize_which_outcome, .rf_one_class_roc,
## .rf_macro_average_roc) to keep cyclomatic complexity under the
## lintr cyclocomp_linter threshold and to make each step testable in
## isolation.

# Build the per-class probability matrix used by the ROC sweep:
#   oob = TRUE  -> object$votes (OOB class proportions; honest)
#   else        -> predict(object, type = "prob") (in-bag)
# randomForest votes can be raw counts; row-normalize to probabilities.
.rf_prob_matrix <- function(object, oob, lvls) {
  prob <- if (isTRUE(oob) && !is.null(object$votes)) {
    as.matrix(object$votes)
  } else {
    stats::predict(object, type = "prob")
  }
  rs <- rowSums(prob)
  if (any(rs > 1 + 1e-8, na.rm = TRUE)) {
    prob <- prob / rs
  }
  colnames(prob) <- lvls
  prob
}

# Coerce/validate which_outcome. Returns either the string "all" (for
# the overall macro-average case) or a single integer class index.
# Accepts: "all", numeric == 0 (handles 0 and 0L), character class
# names (matched against lvls), integer/double indices in 1:nlvls.
# Errors on unknown class names or out-of-range indices.
.normalize_which_outcome <- function(which_outcome, lvls) {
  if (is.character(which_outcome) && !identical(which_outcome, "all")) {
    idx <- match(which_outcome, lvls)
    if (anyNA(idx)) {
      stop("Unknown class name(s) in which_outcome: ",
           paste(which_outcome[is.na(idx)], collapse = ", "),
           ". Must be one of: ", paste(lvls, collapse = ", "))
    }
    return(idx)
  }
  if (is.numeric(which_outcome) && length(which_outcome) == 1L &&
      which_outcome == 0) {
    return("all")
  }
  if (is.numeric(which_outcome) &&
      (any(which_outcome < 1) || any(which_outcome > length(lvls)))) {
    stop("which_outcome out of range; must be in 1:", length(lvls),
         ' or "all" / 0.')
  }
  which_outcome
}

# Build the sensitivity/specificity table for a single class index k.
# Plain lapply (not mclapply): per-threshold work is a single table()
# + a few arithmetic ops (microseconds); fork overhead would dominate,
# and the closure-scope fragility caused the earlier xtabs/Windows
# failure. Returns a data.frame with columns sens, spec, pct.
.rf_one_class_roc <- function(dta, prob, k, lvls) {
  res <- dta == lvls[k]
  score <- prob[, k]
  pct <- sort(unique(score))
  if (length(pct) > 1) pct <- pct[-length(pct)]
  if (length(pct) > 200) {
    pct <- pct[seq(1, length(pct), length.out = 200)]
  }
  rc <- lapply(pct, function(crit) {
    tbl <- table(res, score > crit)
    if (ncol(tbl) < 2) {
      tbl <- cbind(tbl, c(0, 0))
      colnames(tbl) <- c("FALSE", "TRUE")
    }
    spec <- tbl[2, 2] / rowSums(tbl)[2]
    sens <- tbl[1, 1] / rowSums(tbl)[1]
    cbind(sens = sens, spec = spec)
  })
  rc <- do.call(rbind, rc)
  rc <- rbind(c(0, 1), rc, c(1, 0))
  data.frame(rc, pct = c(0, pct, 1), row.names = seq_len(nrow(rc)))
}

# Macro-average one-vs-rest ROC on a shared FPR grid.
.rf_macro_average_roc <- function(dta, prob, lvls) {
  curves <- lapply(seq_along(lvls),
                   function(k) .rf_one_class_roc(dta, prob, k, lvls))
  grid <- seq(0, 1, length.out = 200)
  interp <- vapply(curves, function(cv) {
    cv <- cv[order(cv$spec, decreasing = TRUE), ]
    stats::approx(1 - cv$spec, cv$sens, xout = grid,
                  ties = "ordered", rule = 2)$y
  }, numeric(length(grid)))
  data.frame(
    sens = rowMeans(interp, na.rm = TRUE),
    spec = 1 - grid,
    pct  = grid,
    row.names = seq_along(grid)
  )
}

#' @export
calc_roc.randomForest <-
  function(object,
           dta,
           which_outcome = "all",
           oob = TRUE,
           ...) {
    if (!is.factor(dta)) {
      dta <- factor(dta)
    }
    lvls <- levels(dta)
    prob <- .rf_prob_matrix(object, oob, lvls)
    which_outcome <- .normalize_which_outcome(which_outcome, lvls)

    gg_dta <- if (identical(which_outcome, "all")) {
      .rf_macro_average_roc(dta, prob, lvls)
    } else {
      .rf_one_class_roc(dta, prob, which_outcome, lvls)
    }
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
#' rfsrc_iris <- randomForestSRC::rfsrc(Species ~ ., data = iris)
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

  # Trapezoid area = sens_avg * Δspec, where Δspec = spec[i] - spec[i+1] > 0
  # (spec decreases left-to-right). This equals sens_avg * Δ(1-FPR), which
  # gives the standard AUC = ∫ sens d(FPR) with a positive sign.
  auc <- (x$sens + shift(x$sens)) / 2 * (x$spec - shift(x$spec)) # nolint: object_usage_linter
  sum(auc, na.rm = TRUE)
}

calc_auc.gg_roc <- calc_auc # nolint: object_name_linter
