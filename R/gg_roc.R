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
#' @param which.outcome select the classification outcome of interest.
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
#' #rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' data(rfsrc_iris, package="ggRandomForests")
#'
#' # ROC for setosa
#' gg_dta <- gg_roc(rfsrc_iris, which.outcome=1)
#' plot(gg_dta)
#'
#' # ROC for versicolor
#' gg_dta <- gg_roc(rfsrc_iris, which.outcome=2)
#' plot(gg_dta)
#'
#' # ROC for virginica
#' gg_dta <- gg_roc(rfsrc_iris, which.outcome=3)
#' plot(gg_dta)
#'
#'
## -------- iris data
#' rf_iris <- randomForest::randomForest(Species ~ ., data = iris)
#'
#' # ROC for setosa
#' gg_dta <- gg_roc(rf_iris, which.outcome=1)
#' plot(gg_dta)
#'
#' # ROC for versicolor
#' gg_dta <- gg_roc(rf_iris, which.outcome=2)
#' plot(gg_dta)
#'
#' # ROC for virginica
#' gg_dta <- gg_roc(rf_iris, which.outcome=3)
#' plot(gg_dta)
#'
#'
#' @aliases gg_roc gg_roc.rfsrc gg_roc.randomForest

#' @export
gg_roc.rfsrc <- function(object, which.outcome, oob, ...) {
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
      sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2 &
      !inherits(object, "randomForest")) {
    stop(
      "This function only works for objects of class `(rfsrc, grow)',
      '(rfsrc, predict)' or 'randomForest."
    )
  }
  if (!inherits(object, "class")) {
    stop("gg_roc only works with classification forests")
  }
  
  # Want to remove the which.outcomes argument to plot ROC for all
  # outcomes simultaneously.
  if (missing(which.outcome))
    which.outcome <- "all"
  
  if (object$family != "class")
    stop("gg_roc is intended for classification forests only.")
  
  gg_dta <-
    calc_roc(object,
             object$yvar,
             which.outcome = which.outcome,
             oob = oob)
  #   }
  class(gg_dta) <- c("gg_roc", class(gg_dta))
  
  invisible(gg_dta)
}
#' @export
gg_roc <- function(object, which.outcome, oob, ...) {
  UseMethod("gg_roc", object)
}

#' @export
gg_roc.randomForest <- function(object, which.outcome, oob, ...) {
  if (sum(inherits(object, "randomForest", TRUE) == c(1, 2)) != 1)
    stop(
      "This function only works for objects of class `(rfsrc, grow)',
      '(rfsrc, predict)' or 'randomForest."
    )
  
  # Want to remove the which.outcomes argument to plot ROC for all
  # outcomes simultaneously.
  if (missing(which.outcome))
    which.outcome <- "all"
  
  
  if (!(object$type == "classification")) {
    stop("gg_roc only works with classification forests")
  }
  
  gg_dta <-
    calc_roc(object,
             object$y,
             which.outcome = which.outcome)
  #   }
  class(gg_dta) <- c("gg_roc", class(gg_dta))
  
  invisible(gg_dta)
  
}



#' @export
gg_roc.default <- gg_roc.rfsrc
