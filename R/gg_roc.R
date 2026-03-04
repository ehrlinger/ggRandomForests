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
#' ROC (Receiver operator curve) data from a classification random forest.
#'
#' The sensitivity and specificity of a randomForest classification object.
#'
#' @param object an \code{\link[randomForestSRC]{rfsrc}} classification object
#' @param which_outcome select the classification outcome of interest.
#' @param oob use oob estimates (default TRUE)
#' @param ... extra arguments (not used)
#'
#' @return \code{gg_roc} \code{data.frame} for plotting ROC curves.
#'
#' @seealso \code{\link{plot.gg_roc}} \code{\link[randomForestSRC]{rfsrc}}
#' \code{\link[randomForest]{randomForest}}
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
gg_roc.rfsrc <- function(object, which_outcome, oob, ...) {
  # Validate that the object was grown with randomForestSRC (grow or predict)
  # or is a randomForest object — the two supported class signatures.
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &&
    sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2 &&
    !inherits(object, "randomForest")) {
    stop(
      "This function only works for objects of class `(rfsrc, grow)',
      '(rfsrc, predict)' or 'randomForest."
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
  gg_dta <-
    calc_roc(object,
      object$yvar,
      which_outcome = which_outcome,
      oob = oob
    )
  class(gg_dta) <- c("gg_roc", class(gg_dta))

  invisible(gg_dta)
}
#' @export
gg_roc <- function(object, which_outcome, oob, ...) {
  UseMethod("gg_roc", object)
}

#' @export
gg_roc.randomForest <- function(object, which_outcome, oob, ...) {
  # Validate that the object is a genuine randomForest instance.
  if (sum(inherits(object, "randomForest", TRUE) == c(1, 2)) != 1) {
    stop(
      "This function only works for objects of class `(rfsrc, grow)',
      '(rfsrc, predict)' or 'randomForest."
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
  gg_dta <-
    calc_roc(object,
      object$y,
      which_outcome = which_outcome
    )
  class(gg_dta) <- c("gg_roc", class(gg_dta))

  invisible(gg_dta)
}



#' @export
gg_roc.default <- gg_roc.rfsrc
