####**********************************************************************
####**********************************************************************
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
#' Marginal variable dependence data object.
#'
#' @details The marginal variable dependence is determined by comparing
#' relation between the predicted response from the randomForest and a
#' covariate of interest.
#'
#' The \code{gg_variable} function operates on a
#' \code{\link[randomForestSRC]{rfsrc}} object, the output from the
#' \code{\link[randomForestSRC]{plot.variable}} function, or on a fitted
#' \code{\link[randomForest]{randomForest}} object via the formula interface.
#'
#' @description \code{\link[randomForestSRC]{plot.variable}} generates a
#' \code{data.frame} containing the marginal variable dependence or the
#' partial variable dependence. The \code{gg_variable} function creates a
#' \code{data.frame} of containing the full set of covariate data (predictor
#' variables) and the predicted response for each observation. Marginal
#' dependence figures are created using the \code{\link{plot.gg_variable}}
#' function.
#' For \code{randomForest} fits the original model frame is rebuilt
#' from the stored call so that the same predictors can be paired with the
#' in-sample predictions.
#'
#' Optional arguments include \code{time} (scalar or vector of survival times
#' of interest), \code{time_labels} (labels for multiple survival horizons) and
#' \code{oob} which toggles between out-of-bag and in-bag predictions when the
#' forest stores both.
#'
#' @param object A \code{\link[randomForestSRC]{rfsrc}} or
#'   \code{\link[randomForest]{randomForest}} object, or a
#'   \code{\link[randomForestSRC]{plot.variable}} result.
#' @param ... Optional arguments such as \code{time}, \code{time_labels}, and
#'   \code{oob} that tailor the marginal dependence extraction.
#'
#' @return \code{gg_variable} object
#'
#' @seealso \code{\link{plot.gg_variable}}
#' @seealso \code{\link[randomForestSRC]{plot.variable}}
#'
#' @aliases gg_variable gg_variable.rfsrc
#'
#' @examples
#' ## ------------------------------------------------------------
#' ## classification
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' ## iris
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#'
#' gg_dta <- gg_variable(rfsrc_iris)
#' plot(gg_dta, xvar = "Sepal.Width")
#' plot(gg_dta, xvar = "Sepal.Length")
#'
#' plot(gg_dta,
#'   xvar = rfsrc_iris$xvar.names,
#'   panel = TRUE
#' ) # , se=FALSE)
#'
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#' 
#' ## -------- air quality data
#' rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality)
#' gg_dta <- gg_variable(rfsrc_airq)
#'
#' # an ordinal variable
#' gg_dta[, "Month"] <- factor(gg_dta[, "Month"])
#'
#' plot(gg_dta, xvar = "Wind")
#' plot(gg_dta, xvar = "Temp")
#' plot(gg_dta, xvar = "Solar.R")
#'
#'
#' plot(gg_dta, xvar = c("Solar.R", "Wind", "Temp", "Day"), panel = TRUE)
#'
#' plot(gg_dta, xvar = "Month", notch = TRUE)
#' 
#' ## -------- motor trend cars data
#' rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
#'
#' gg_dta <- gg_variable(rfsrc_mtcars)
#'
#' # mtcars$cyl is an ordinal variable
#' gg_dta$cyl <- factor(gg_dta$cyl)
#' gg_dta$am <- factor(gg_dta$am)
#' gg_dta$vs <- factor(gg_dta$vs)
#' gg_dta$gear <- factor(gg_dta$gear)
#' gg_dta$carb <- factor(gg_dta$carb)
#'
#' plot(gg_dta, xvar = "cyl")
#'
#' # Others are continuous
#' plot(gg_dta, xvar = "disp")
#' plot(gg_dta, xvar = "hp")
#' plot(gg_dta, xvar = "wt")
#'
#' # panels
#' plot(gg_dta, xvar = c("disp", "hp", "drat", "wt", "qsec"), panel = TRUE)
#' plot(gg_dta,
#'   xvar = c("cyl", "vs", "am", "gear", "carb"), panel = TRUE,
#'   notch = TRUE
#' )
#' 
#' ## -------- Boston data
#' data(Boston, package = "MASS")
#'
#' rf_boston <- randomForest::randomForest(medv ~ ., data = Boston)
#' gg_dta <- gg_variable(rf_boston)
#' plot(gg_dta)
#' plot(gg_dta, panel = TRUE)
#' ## ------------------------------------------------------------
#' ## survival examples
#' ## ------------------------------------------------------------
#' 
#' ## -------- veteran data
#' ## survival
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., veteran,
#'   nsplit = 10,
#'   ntree = 100
#' )
#'
#' # get the 1 year survival time.
#' gg_dta <- gg_variable(rfsrc_veteran, time = 90)
#'
#' # Generate variable dependence plots for age and diagtime
#' plot(gg_dta, xvar = "age")
#' plot(gg_dta, xvar = "diagtime", )
#'
#' # Generate coplots
#' plot(gg_dta, xvar = c("age", "diagtime"), panel = TRUE, se = FALSE)
#'
#' # If we want to compare survival at different time points, say 30, 90 day
#' # and 1 year
#' gg_dta <- gg_variable(rfsrc_veteran, time = c(30, 90, 365))
#'
#' # Generate variable dependence plots for age and diagtime
#' plot(gg_dta, xvar = "age")
#' 
#' ## -------- pbc data
#' ## We don't run this because of bootstrap confidence limits
#' # We need to create this dataset
#' data(pbc, package = "randomForestSRC", )
#' # For whatever reason, the age variable is in days... makes no sense to me
#' for (ind in seq_len(dim(pbc)[2])) {
#'   if (!is.factor(pbc[, ind])) {
#'     if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'       if (sum(range(pbc[, ind], na.rm = TRUE) == c(0, 1)) == 2) {
#'         pbc[, ind] <- as.logical(pbc[, ind])
#'       }
#'     }
#'   } else {
#'     if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'       if (sum(sort(unique(pbc[, ind])) == c(0, 1)) == 2) {
#'         pbc[, ind] <- as.logical(pbc[, ind])
#'       }
#'       if (sum(sort(unique(pbc[, ind])) == c(FALSE, TRUE)) == 2) {
#'         pbc[, ind] <- as.logical(pbc[, ind])
#'       }
#'     }
#'   }
#'   if (!is.logical(pbc[, ind]) &
#'     length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 5) {
#'     pbc[, ind] <- factor(pbc[, ind])
#'   }
#' }
#' # Convert age to years
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
#' # ========
#' # build the forest:
#' rfsrc_pbc <- randomForestSRC::rfsrc(
#'   Surv(years, status) ~ .,
#'   dta_train,
#'   nsplit = 10,
#'   na.action = "na.impute",
#'   forest = TRUE,
#'   importance = TRUE,
#'   save.memory = TRUE
#' )
#'
#' gg_dta <- gg_variable(rfsrc_pbc, time = c(.5, 1, 3))
#' plot(gg_dta, xvar = "age")
#' plot(gg_dta, xvar = "trig")
#'
#' # Generate coplots
#' plot(gg_dta, xvar = c("age", "trig"), panel = TRUE, se = FALSE)
#' 
#'
#' @aliases gg_variable gg_variable.rfsrc gg_variable.randomForest
#' @aliases gg_variable.random
#' @importFrom stats median
#'
#' @export
gg_variable <- function(object, ...) {
  UseMethod("gg_variable", object)
}
#' @export
gg_variable.rfsrc <- function(object,
                              ...) {
  # Get the extra arguments for handling specifics
  arg_list <- list(...)

  time <- arg_list$time
  time_labels <- arg_list$time_labels
  oob <- if (!is.null(arg_list$oob)) {
    arg_list$oob
  } else {
    TRUE
  }

  # Want to also handle a plot.variable where partial!= TRUE
  if (!inherits(object, "rfsrc")) {
    stop("gg_variable expects a rfsrc or plot.variable object.")
  }

  # IF we called this with a partial plot obect, instead of marginal.
  # if (inherits(object, "plot.variable")) {
  #   if (object$partial) {
  #     invisible(gg_partial(object, ...))
  #   }
  # }

  # !! Have to verify this works with a plot.variable object...

  # gg_variable is really just cutting the data into time slices.
  gg_dta <- data.frame(object$xvar)

  if (object$family == "regr") {
    if (oob) {
      gg_dta$yhat <- object$predicted.oob
    } else {
      gg_dta$yhat <- object$predicted
    }
  } else if (object$family == "class") {
    preds <- if (oob) {
      object$predicted.oob
    } else {
      object$predicted
    }
    colnames(preds) <-
      paste("yhat.", colnames(preds),
        sep = ""
      )
    gg_dta <- cbind(gg_dta, preds)
    gg_dta$yvar <- object$yvar
  } else if (object$family == "surv") {
    gg_dta$event <- as.logical(object$yvar[, 2])
    colnames(gg_dta) <- c(object$xvar.names, "event")

    if (is.null(time)) {
      time <- median(object$time.interest)
    }
    lng <- length(time)

    for (ind in 1:lng) {
      if (ind > 1) {
        gg_dta_t_old <- gg_dta_t
      }
      ## For marginal plot.
      # Plot.variable returns the resubstituted survival, not OOB. So we
      # calculate it. Time is really straight forward since survival is a
      # step function
      #
      # Get the event time occuring before or at 1 year.
      gg_dta_t <- gg_dta
      eligible <- which(object$time.interest <= time[ind])
      if (!length(eligible)) {
        stop(
          "The time of interest is less than the first event time.\n",
          "  Make sure you are using the correct time units."
        )
      }
      in_time <- max(eligible)

      if (oob) {
        gg_dta_t$yhat <- object$survival.oob[, in_time]
      } else {
        gg_dta_t$yhat <- object$survival[, in_time]
      }

      if (is.null(time_labels)) {
        gg_dta_t$time <- time[ind]
      } else {
        gg_dta_t$time <- time_labels[ind]
      }

      if (ind > 1) {
        gg_dta_t <- rbind(gg_dta_t_old, gg_dta_t)
      }
    }

    gg_dta <- gg_dta_t
    gg_dta$time <- factor(gg_dta$time, levels = unique(gg_dta$time))
  }
  class(gg_dta) <- c("gg_variable", object$family, class(gg_dta))
  invisible(gg_dta)
}

#' @export
gg_variable.randomForest <- function(object,
                                     ...) {
  arg_list <- list(...)

  if (!is.null(arg_list$oob)) {
    arg_list$oob <- FALSE
  }

  if (!inherits(object, "randomForest")) {
    stop("gg_variable expects a randomForest object.")
  }

  training_info <- .rf_recover_model_frame(object)
  if (is.null(training_info)) {
    stop(
      "Unable to reconstruct the training data for this randomForest object.",
      " Make sure the model was fit with a formula interface and keep.forest = TRUE."
    )
  }

  model_frame <- training_info$model_frame
  response <- stats::model.response(model_frame)
  resp_name <- training_info$response_name

  predictors <- model_frame
  if (!is.null(resp_name) && resp_name %in% names(predictors)) {
    predictors[[resp_name]] <- NULL
  }
  special_cols <- grep("^\\(", colnames(predictors), value = TRUE)
  if (length(special_cols) > 0) {
    predictors[special_cols] <- NULL
  }

  gg_dta <- predictors
  gg_dta$yhat <- as.vector(object$predicted)
  if (object$type == "classification") {
    gg_dta$yvar <- response
  }

  class(gg_dta) <- c("gg_variable", object$type, class(gg_dta))
  invisible(gg_dta)
}
