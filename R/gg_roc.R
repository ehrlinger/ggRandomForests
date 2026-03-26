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
#' @param which_outcome Integer index or character name of the class for which
#'   the ROC curve is computed. For binary forests this is typically \code{1}
#'   or \code{2}; for multi-class forests any valid class index. Use
#'   \code{which_outcome = 0} to obtain the overall (averaged) ROC.
#' @param oob Logical; if \code{TRUE} (default) use out-of-bag predicted
#'   probabilities for the curve. Set to \code{FALSE} to use full in-bag
#'   predictions.
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
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris)
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
gg_roc.rfsrc <- function(object, which_outcome, oob = TRUE, ...) {
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

  invisible(gg_dta)
}
#' @export
gg_roc <- function(object, which_outcome, oob = TRUE, ...) {
  UseMethod("gg_roc", object)
}

#' @export
gg_roc.randomForest <- function(object, which_outcome, oob, ...) {
  # Validate that the object is a genuine randomForest instance.
  if (!inherits(object, "randomForest")) {
    stop(
      "gg_roc.randomForest only works for objects of class 'randomForest'."
    )
  }

  # Default to computing the ROC curve for all outcome classes.
  if (missing(which_outcome)) {
    which_outcome <- "all"
  }

  if (!(object$type == "classification")) {
    stop("gg_roc only works with classification forests")
  }

  # For randomForest objects the response is stored in $y (not $yvar).
  gg_dta <- # nolint: object_usage_linter
    calc_roc(object,
      object$y,
      which_outcome = which_outcome
    )
  class(gg_dta) <- c("gg_roc", class(gg_dta))

  invisible(gg_dta)
}



#' @export
gg_roc.default <- gg_roc.rfsrc
