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
#' randomForest error rate data object
#'
#' Extract the cumulative (OOB) \code{randomForestSRC} error rate as a
#' function of number of trees.
#'
#' @details The \code{gg_error} function simply returns the
#' \code{\link[randomForestSRC]{rfsrc}$err.rate} object as a data.frame,
#' and assigns the class for connecting to the S3
#' \code{\link{plot.gg_error}} function.
#'
#' @param object \code{\link[randomForestSRC]{rfsrc}} object.
#' @param ... optional arguments (not used).
#'
#' @return \code{gg_error} \code{data.frame} with one column indicating
#' the tree number, and the remaining columns from the
#' \code{\link[randomForestSRC]{rfsrc}$err.rate} return value.
#'
#' @seealso \code{\link{plot.gg_error}} \code{rfsrc} \code{plot.rfsrc}
#'
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#'
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R,
#' Rnews, 7(2):25-31.
#'
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression
#' and Classification (RF-SRC), R package version 1.4.
#'
#' @aliases gg_error gg_error.rfsrc gg_error.randomForest
#' @aliases gg_error.randomForest.formula
#'
#' @examples
#' ## Examples from RFSRC package...
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## ------------- iris data
#' ## You can build a randomForest
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris, tree.err = TRUE)
#'
#' # Get a data.frame containing error rates
#' gg_dta <- gg_error(rfsrc_iris)
#'
#' # Plot the gg_error object
#' plot(gg_dta)
#'
#' ## RandomForest example
#' rf_iris <- randomForest::randomForest(Species ~ .,
#'   data = iris,
#'   tree.err = TRUE,
#' )
#' gg_dta <- gg_error(rf_iris)
#' plot(gg_dta)
#'
#' gg_dta <- gg_error(rf_iris, training = TRUE)
#' plot(gg_dta)
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' 
#' ## ------------- airq data
#' rfsrc_airq <- rfsrc(Ozone ~ .,
#'   data = airquality,
#'   na.action = "na.impute", tree.err = TRUE,
#' )
#'
#' # Get a data.frame containing error rates
#' gg_dta <- gg_error(rfsrc_airq)
#'
#' # Plot the gg_error object
#' plot(gg_dta)
#' 
#'
#' ## ------------- Boston data
#' data(Boston, package = "MASS")
#' Boston$chas <- as.logical(Boston$chas)
#' rfsrc_boston <- rfsrc(medv ~ .,
#'   data = Boston,
#'   forest = TRUE,
#'   importance = TRUE,
#'   tree.err = TRUE,
#'   save.memory = TRUE
#' )
#'
#' # Get a data.frame containing error rates
#' gg_dta <- gg_error(rfsrc_boston)
#'
#' # Plot the gg_error object
#' plot(gg_dta)
#'
#' 
#' ## ------------- mtcars data
#' rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars, tree.err = TRUE)
#' 

#' # Get a data.frame containing error rates
#' gg_dta<- gg_error(rfsrc_mtcars)
#'
#' # Plot the gg_error object
#' plot(gg_dta)
#' 
#'
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## ------------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran,
#'                        tree.err = TRUE)
#'
#' gg_dta <- gg_error(rfsrc_veteran)
#' plot(gg_dta)
#'
#' ## ------------- pbc data
#' # Load a cached randomForestSRC object
#' # We need to create this dataset
#' data(pbc, package = "randomForestSRC",)
#' # For whatever reason, the age variable is in days... makes no sense to me
#' for (ind in seq_len(dim(pbc)[2])) {
#'  if (!is.factor(pbc[, ind])) {
#'    if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'      if (sum(range(pbc[, ind], na.rm = TRUE) == c(0, 1)) == 2) {
#'        pbc[, ind] <- as.logical(pbc[, ind])
#'      }
#'    }
#'  } else {
#'    if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'      if (sum(sort(unique(pbc[, ind])) == c(0, 1)) == 2) {
#'        pbc[, ind] <- as.logical(pbc[, ind])
#'      }
#'      if (sum(sort(unique(pbc[, ind])) == c(FALSE, TRUE)) == 2) {
#'        pbc[, ind] <- as.logical(pbc[, ind])
#'      }
#'    }
#'  }
#'  if (!is.logical(pbc[, ind]) &
#'      length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 5) {
#'    pbc[, ind] <- factor(pbc[, ind])
#'  }
#' }
#' #Convert age to years
#' pbc$age <- pbc$age / 364.24
#'
#' pbc$years <- pbc$days / 364.24
#' pbc <- pbc[, -which(colnames(pbc) == "days")]
#' pbc$treatment <- as.numeric(pbc$treatment)
#' pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
#' pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
#' pbc$treatment <- factor(pbc$treatment)
#' dta_train <- pbc[-which(is.na(pbc$treatment)), ]
#' # Create a test set from the remaining patients
#' pbc_test <- pbc[which(is.na(pbc$treatment)), ]
#'
#' #========
#' # build the forest:
#' rfsrc_pbc <- randomForestSRC::rfsrc(
#'   Surv(years, status) ~ .,
#'  dta_train,
#'  nsplit = 10,
#'  na.action = "na.impute",
#'  tree.err = TRUE,
#'  forest = TRUE,
#'  importance = TRUE,
#'  save.memory = TRUE
#' )
#'
#'
#' gg_dta <- gg_error(rfsrc_pbc)
#' plot(gg_dta)
#'
#' @importFrom stats na.omit predict qnorm
#'
#' @export gg_error gg_error.rfsrc gg_error.randomForest
#' @export gg_error.randomForest.formula
gg_error <- function(object, ...) {
  UseMethod("gg_error", object)
}
#' @export
gg_error.rfsrc <- function(object, ...) {
  ## Check that the input obect is of the correct type.
  if (!inherits(object, "rfsrc")) {
    stop(
      paste(
        "This function only works for Forests grown",
        "with the randomForestSRC package."
      )
    )
  }
  if (is.null(object$err.rate)) {
    stop("Performance values are not available for this forest.")
  }

  gg_dta <- data.frame(object$err.rate)

  # If there is only one column in the error rate... name it reasonably.
  if ("object.err.rate" %in% colnames(gg_dta)) {
    colnames(gg_dta)[which(colnames(gg_dta) == "object.err.rate")] <-
      "error"
  }

  gg_dta$ntree <- seq_len(dim(gg_dta)[1])

  arg_list <- as.list(substitute(list(...)))
  training <- FALSE
  if (!is.null(arg_list$training)) {
    training <- arg_list$training
  }
  if (training) {
    trn <- data.frame(cbind(object$xvar, object$yvar))
    colnames(trn) <- c(object$xvar.names, object$yvar.names)
    gg_prd <- predict(
      object,
      newdata = trn,
      importance = "none",
      membership = FALSE
    )
    gg_dta$train <- gg_prd$err.rate
  }
  gg_dta <- na.omit(gg_dta)
  class(gg_dta) <- c("gg_error", class(gg_dta))
  invisible(gg_dta)
}

#' @export
gg_error.randomForest <- function(object, ...) {
  ## Check that the input obect is of the correct type.
  if (!inherits(object, "randomForest")) {
    stop(
      paste(
        "This function only works for Forests grown",
        "with the randomForest package."
      )
    )
  }

  if (!is.null(object$mse)) {
    # For regression
    gg_dta <- data.frame(object$mse)

    # If there is only one column in the error rate... name it reasonably.
    if ("object.mse" %in% colnames(gg_dta)) {
      colnames(gg_dta)[which(colnames(gg_dta) == "object.mse")] <-
        "error"
    }

    gg_dta$ntree <- seq_len(nrow(gg_dta))

    arg_list <- as.list(substitute(list(...)))
    training <- FALSE
    if (!is.null(arg_list$training)) {
      training <- arg_list$training
    }

    if (training) {
      trn <- data.frame(cbind(object$xvar, object$yvar))
      colnames(trn) <- c(object$xvar.names, object$yvar.names)
      gg_prd <- predict(
        object,
        newdata = trn,
        importance = "none",
        membership = FALSE
      )
      gg_dta$train <- gg_prd$err.rate
    }
  } else if (!is.null(object$err.rate)) {
    # For classification
    gg_dta <- data.frame(object$err.rate)

    gg_dta$ntree <- seq_len(nrow(gg_dta))

    arg_list <- as.list(substitute(list(...)))
    training <- FALSE
    if (!is.null(arg_list$training)) {
      training <- arg_list$training
    }
  } else {
    stop("Performance values are not available for this forest.")
  }
  class(gg_dta) <- c("gg_error", class(gg_dta))
  invisible(gg_dta)
}

#' @export
gg_error.randomForest.formula <- gg_error.randomForest
