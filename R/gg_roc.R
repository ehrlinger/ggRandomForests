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
#'
#' ROC (Receiver Operating Characteristic) curve data from a classification forest.
#'
#' Computes sensitivity (true positive rate) and specificity (1 - false positive
#' rate) across all prediction thresholds for one class of a classification
#' \code{\link[randomForestSRC]{rfsrc}} or
#' \code{\link[randomForest]{randomForest}} object.
#'
#' @param object A classification \code{\link[randomForestSRC]{rfsrc}} or
#'   \code{\link[randomForest]{randomForest}} object. Only forests with
#'   \code{family == "class"} (rfsrc) or \code{type == "classification"}
#'   (randomForest) are supported.
#' @param which_outcome Integer index or character name of the class for
#'   which the ROC curve is computed. For binary forests this is typically
#'   \code{1} or \code{2}; for multi-class forests any valid class index or
#'   level name. The behaviour of \code{which_outcome = "all"} or \code{0}
#'   is engine-specific:
#'   \describe{
#'     \item{\code{randomForest} method}{Returns a macro-averaged
#'       one-vs-rest ROC computed over the per-class probabilities.}
#'     \item{\code{rfsrc} method}{Currently warns and falls back to
#'       class 1 (the macro-average / per-class faceting work for the
#'       \code{rfsrc} path is tracked separately under issue #72).}
#'   }
#' @param oob Logical; if \code{TRUE} (default) use out-of-bag predicted
#'   probabilities for the curve. Set to \code{FALSE} to use full in-bag
#'   predictions. For \code{randomForest}, \code{oob = TRUE} uses out-of-bag
#'   vote probabilities (\code{object$votes}); \code{FALSE} uses in-bag
#'   \code{predict(type = "prob")}.
#' @param ... Extra arguments (currently unused).
#'
#' @return A \code{gg_roc} \code{data.frame} with one row per unique prediction
#'   threshold and columns:
#'   \describe{
#'     \item{sens}{Sensitivity (true positive rate) at each threshold.}
#'     \item{spec}{Specificity (true negative rate) at each threshold.}
#'     \item{yvar}{The observed class label for each observation.}
#'   }
#'   Pass to \code{\link{calc_auc}} for the area under the curve.
#'
#' @seealso \code{\link{plot.gg_roc}}, \code{\link{calc_roc}},
#'   \code{\link{calc_auc}},
#'   \code{\link[randomForestSRC]{rfsrc}},
#'   \code{\link[randomForest]{randomForest}}
#'
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' rfsrc_iris <- randomForestSRC::rfsrc(Species ~ ., data = iris)
#'
#' # ROC for setosa
#' gg_dta <- gg_roc(rfsrc_iris, which_outcome = 1)
#' plot(gg_dta)
#'
#' # ROC for versicolor
#' gg_dta <- gg_roc(rfsrc_iris, which_outcome = 2)
#' plot(gg_dta)
#'
#' # ROC for virginica
#' gg_dta <- gg_roc(rfsrc_iris, which_outcome = 3)
#' plot(gg_dta)
#'
#' ## -------- iris data
#' rf_iris <- randomForest::randomForest(Species ~ ., data = iris)
#'
#' # ROC for setosa
#' gg_dta <- gg_roc(rf_iris, which_outcome = 1)
#' plot(gg_dta)
#'
#' # ROC for versicolor
#' gg_dta <- gg_roc(rf_iris, which_outcome = 2)
#' plot(gg_dta)
#'
#' # ROC for virginica
#' gg_dta <- gg_roc(rf_iris, which_outcome = 3)
#' plot(gg_dta)
#'
#' @aliases gg_roc gg_roc.rfsrc gg_roc.randomForest

#' @export
gg_roc.rfsrc <- function(object, which_outcome, oob = TRUE,
                         per_class = FALSE, ...) {
  # Validate that the object was grown with randomForestSRC (grow or predict)
  # or is a randomForest object — the two supported class signatures.
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &&
    sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2 &&
    !inherits(object, "randomForest")) {
    stop(
      "This function only works for objects of class '(rfsrc, grow)', ",
      "'(rfsrc, predict)', or 'randomForest'."
    )
  }
  # ROC curves only make sense for classification; reject other families early.
  if (!inherits(object, "class")) {
    stop("gg_roc only works with classification forests")
  }

  # Default to "all" so the caller can later loop over every class level;
  # the caller may pass a specific integer index to get a single-class curve.
  if (missing(which_outcome)) {
    which_outcome <- "all"
  }

  # Redundant guard: rfsrc sets family = "class" for classification forests.
  # Kept here to surface a clearer error message if the object is somehow
  # mis-labelled.
  if (object$family != "class") {
    stop("gg_roc is intended for classification forests only.")
  }

  # Delegate the threshold-sweep computation to calc_roc, passing the
  # observed response vector and the chosen outcome column index.
  gg_dta <- # nolint: object_usage_linter
    calc_roc(object,
      object$yvar,
      which_outcome = which_outcome,
      oob = oob
    )
  class(gg_dta) <- c("gg_roc", class(gg_dta))
  gg_dta <- .set_provenance(gg_dta, object)

  invisible(gg_dta)
}
#' @export
gg_roc <- function(object, which_outcome, oob = TRUE, per_class = FALSE, ...) {
  UseMethod("gg_roc", object)
}

#' @export
gg_roc.randomForest <- function(object, which_outcome, oob = TRUE,
                                per_class = FALSE, ...) {
  if (!inherits(object, "randomForest")) {
    stop("gg_roc.randomForest only works for objects of class 'randomForest'.")
  }
  if (missing(which_outcome)) {
    which_outcome <- "all"
  }
  if (!(object$type == "classification")) {
    stop("gg_roc only works with classification forests")
  }

  lvls    <- levels(object$y)
  n_class <- length(lvls)

  # ── per_class = TRUE path (multi-class only) ─────────────────────────────
  if (isTRUE(per_class) && n_class > 2L) {
    if (!missing(which_outcome) && !identical(which_outcome, "all")) {
      message("which_outcome is ignored when per_class = TRUE.")
    }
    prob   <- .rf_prob_matrix(object, oob, lvls)
    dta    <- object$y
    curves <- lapply(seq_along(lvls), function(k) {
      cv       <- .rf_one_class_roc(dta, prob, k, lvls)
      cv$class <- lvls[k]
      cv
    })
    auc_vals        <- vapply(curves, calc_auc, numeric(1L))
    names(auc_vals) <- lvls
    auc_ord         <- order(auc_vals, decreasing = TRUE)
    auc_vals        <- auc_vals[auc_ord]
    gg_dta          <- do.call(rbind, curves)
    gg_dta$class    <- factor(gg_dta$class, levels = lvls[auc_ord])
    class(gg_dta)   <- c("gg_roc", class(gg_dta))
    attr(gg_dta, "auc") <- auc_vals
    gg_dta <- .set_provenance(gg_dta, object)
    return(invisible(gg_dta))
  }

  # ── Standard path (binary, or per_class not requested) ──────────────────
  # For randomForest objects the response is stored in $y (not $yvar).
  gg_dta <- # nolint: object_usage_linter
    calc_roc(object, object$y, which_outcome = which_outcome, oob = oob)
  class(gg_dta)       <- c("gg_roc", class(gg_dta))
  attr(gg_dta, "auc") <- calc_auc(gg_dta)
  gg_dta              <- .set_provenance(gg_dta, object)
  invisible(gg_dta)
}



#' @export
gg_roc.default <- gg_roc.rfsrc
