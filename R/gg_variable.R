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
#' @return A \code{gg_variable} object: a \code{data.frame} of all predictor
#'   columns from the training data paired with the OOB (or in-bag) predicted
#'   response. For survival forests each requested time horizon produces an
#'   additional column named by \code{time_labels}. The object carries a
#'   \code{"family"} class attribute (\code{"regr"}, \code{"class"}, or
#'   \code{"surv"}) used by \code{\link{plot.gg_variable}} for dispatch.
#'
#' @seealso \code{\link{plot.gg_variable}},
#'   \code{\link[randomForestSRC]{plot.variable}}
#'
#' @aliases gg_variable gg_variable.rfsrc
#'
#' @examples
#' ## ------------------------------------------------------------
#' ## classification (small, runs on CRAN)
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' set.seed(42)
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris, ntree = 50)
#'
#' gg_dta <- gg_variable(rfsrc_iris)
#' plot(gg_dta, xvar = "Sepal.Width")
#'
#' \donttest{
#' ## ------------------------------------------------------------
#' ## Additional classification / regression / survival examples are
#' ## guarded with \donttest because the cumulative example time exceeds
#' ## the 10-second CRAN budget. Run locally with `R CMD check
#' ## --run-donttest` (or `devtools::check(run_dont_test = TRUE)`) to
#' ## exercise them.
#' ## ------------------------------------------------------------
#' plot(gg_dta, xvar = "Sepal.Length")
#' plot(gg_dta, xvar = rfsrc_iris$xvar.names, panel = TRUE)
#'
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#'
#' ## -------- air quality data
#' rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, ntree = 50)
#' gg_dta <- gg_variable(rfsrc_airq)
#'
#' # an ordinal variable
#' gg_dta[, "Month"] <- factor(gg_dta[, "Month"])
#'
#' plot(gg_dta, xvar = "Wind")
#' plot(gg_dta, xvar = "Temp")
#' plot(gg_dta, xvar = "Solar.R")
#' plot(gg_dta, xvar = c("Solar.R", "Wind", "Temp", "Day"), panel = TRUE)
#' plot(gg_dta, xvar = "Month", notch = TRUE)
#'
#' ## -------- motor trend cars data
#' rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars, ntree = 50)
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
#' plot(gg_dta, xvar = "disp")
#' plot(gg_dta, xvar = "hp")
#' plot(gg_dta, xvar = "wt")
#' plot(gg_dta, xvar = c("disp", "hp", "drat", "wt", "qsec"), panel = TRUE)
#' plot(gg_dta,
#'   xvar = c("cyl", "vs", "am", "gear", "carb"), panel = TRUE,
#'   notch = TRUE
#' )
#'
#' ## -------- Boston data
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   data(Boston, package = "MASS")
#'   rf_boston <- randomForest::randomForest(medv ~ ., data = Boston)
#'   gg_dta <- gg_variable(rf_boston)
#'   plot(gg_dta)
#'   plot(gg_dta, panel = TRUE)
#' }
#'
#' ## ------------------------------------------------------------
#' ## survival examples
#' ## ------------------------------------------------------------
#'
#' ## -------- veteran data
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., veteran,
#'   nsplit = 10,
#'   ntree = 50
#' )
#'
#' # get the 90-day survival time.
#' gg_dta <- gg_variable(rfsrc_veteran, time = 90)
#'
#' # Generate variable dependence plots for age and diagtime
#' plot(gg_dta, xvar = "age")
#' plot(gg_dta, xvar = "diagtime")
#'
#' # Generate coplots
#' plot(gg_dta, xvar = c("age", "diagtime"), panel = TRUE, se = FALSE)
#'
#' # Compare survival at 30, 90, and 365 days simultaneously
#' gg_dta <- gg_variable(rfsrc_veteran, time = c(30, 90, 365))
#' plot(gg_dta, xvar = "age")
#' }
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
  # Collect optional arguments: time, time_labels, oob
  arg_list <- list(...)

  time <- arg_list$time
  time_labels <- arg_list$time_labels
  # Default to OOB predictions; callers can override with oob = FALSE to use
  # full-forest (in-bag) predictions instead.
  oob <- if (!is.null(arg_list$oob)) {
    arg_list$oob
  } else {
    TRUE
  }

  if (!inherits(object, "rfsrc")) {
    stop("gg_variable expects a rfsrc or plot.variable object.")
  }

  # Start with the predictor (xvar) data frame; we will append the predicted
  # response column(s) below depending on the forest family.
  gg_dta <- data.frame(object$xvar)

  ## ---- Regression branch ---------------------------------------------------
  if (object$family == "regr") {
    # Append a single "yhat" column containing the OOB (or full) predictions.
    if (oob) {
      gg_dta$yhat <- object$predicted.oob
    } else {
      gg_dta$yhat <- object$predicted
    }

  ## ---- Classification branch -----------------------------------------------
  } else if (object$family == "class") {
    # Append one "yhat.<class>" probability column per outcome class so the
    # caller can choose which class to display on the variable plot.
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

  ## ---- Survival branch -----------------------------------------------------
  } else if (object$family == "surv") {
    # Survival forests predict a matrix (observations × time points); we need
    # to slice it at one or more user-specified time horizons.
    gg_dta$event <- as.logical(object$yvar[, 2])
    colnames(gg_dta) <- c(object$xvar.names, "event")

    # Default to the median observed event time when no time is specified.
    if (is.null(time)) {
      time <- median(object$time.interest)
    }
    lng <- length(time)

    for (ind in seq_len(lng)) {
      if (ind > 1) {
        gg_dta_t_old <- gg_dta_t
      }
      # Find the largest recorded time point that is <= the requested horizon.
      # Survival is a step function, so we read the value at the last eligible
      # recorded time point.
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

      # Attach either the numeric time value or a user-supplied label.
      if (is.null(time_labels)) {
        gg_dta_t$time <- time[ind]
      } else {
        gg_dta_t$time <- time_labels[ind]
      }

      # Stack time-slice data frames for multi-horizon plots.
      if (ind > 1) {
        gg_dta_t <- rbind(gg_dta_t_old, gg_dta_t)
      }
    }

    gg_dta <- gg_dta_t
    # Preserve the ordering of time levels in the factor for downstream plots.
    gg_dta$time <- factor(gg_dta$time, levels = unique(gg_dta$time))
  }
  class(gg_dta) <- c("gg_variable", object$family, class(gg_dta))
  invisible(gg_dta)
}

#' @export
gg_variable.randomForest <- function(object,
                                     ...) {
  arg_list <- list(...)

  # randomForest objects do not store OOB predictions in a way that maps back
  # to the predictor space, so we always use in-bag (full-forest) predictions.
  if (!is.null(arg_list$oob)) {
    arg_list$oob <- FALSE
  }

  if (!inherits(object, "randomForest")) {
    stop("gg_variable expects a randomForest object.")
  }

  # Reconstruct the training data from the stored call so we can pair
  # predictions with the original predictors.
  training_info <- .rf_recover_model_frame(object) # nolint: object_usage_linter
  if (is.null(training_info)) {
    stop(
      "Unable to reconstruct the training data for this randomForest object.",
      " Make sure the model was fit with a formula interface and keep.forest = TRUE."
    )
  }

  model_frame <- training_info$model_frame
  response <- stats::model.response(model_frame)
  resp_name <- training_info$response_name

  # Drop the response column and any special model-frame columns (e.g. offset)
  # to leave only the predictor columns.
  predictors <- model_frame
  if (!is.null(resp_name) && resp_name %in% names(predictors)) {
    predictors[[resp_name]] <- NULL
  }
  special_cols <- grep("^\\(", colnames(predictors), value = TRUE)
  if (length(special_cols) > 0) {
    predictors[special_cols] <- NULL
  }

  gg_dta <- predictors
  # Append the forest's in-bag predicted values.
  gg_dta$yhat <- as.vector(object$predicted)
  if (object$type == "classification") {
    gg_dta$yvar <- response
  }

  class(gg_dta) <- c("gg_variable", object$type, class(gg_dta))
  invisible(gg_dta)
}
