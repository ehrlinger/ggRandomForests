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
#' \code{\link[randomForestSRC]{rfsrc}} object, or the output from the
#' \code{\link[randomForestSRC]{plot.variable}} function.
#'
#' @description \code{\link[randomForestSRC]{plot.variable}} generates a
#' \code{data.frame} containing the marginal variable dependence or the
#' partial variable dependence. The \code{gg_variable} function creates a
#' \code{data.frame} of containing the full set of covariate data (predictor
#' variables) and the predicted response for each observation. Marginal
#' dependence figures are created using the \code{\link{plot.gg_variable}}
#' function.
#'
#' Optional arguments \code{time} point (or vector of points) of interest
#' (for survival forests only) \code{time_labels} If more than one time is
#' specified, a vector of time labels for differentiating the time points
#' (for survival forests only) \code{oob} indicate if predicted results
#' should include oob or full data set.
#'
#' @param object a \code{\link[randomForestSRC]{rfsrc}} object
#' @param ... optional arguments
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
#' \dontrun{
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
#' }
#' \dontrun{
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
#' }
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
#' \dontrun{
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
#' }
#' \dontrun{
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
  if (inherits(object, "plot.variable")) {
    if (object$partial) {
      invisible(gg_partial(object, ...))
    }
  }

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
    if (oob) {
      colnames(object$predicted.oob) <-
        paste("yhat.", colnames(object$predicted.oob),
          sep = ""
        )
      gg_dta <- cbind(gg_dta, object$predicted.oob)
    } else {
      colnames(object$predicted) <-
        paste("yhat.", colnames(object$predicted),
          sep = ""
        )
      gg_dta <- object$predicted
    }
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
      in_time <- which(object$time.interest > time[ind])[1] - 1
      if (in_time == 0) {
        stop(
          "The time of interest is less than the first event time.
          Make sure you are using the correct time units."
        )
      }

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

  # Want to also handle a plot.variable where partial!= TRUE
  if (!inherits(object, "randomForest")) {
    stop("gg_variable expects a randomForest object.")
  }

  # gg_variable is really just the training data and the outcome.
  gg_dta <- get(as.character(object$call$data))


  # Remove the response from the data.frame
  rsp <- as.character(object$call$formula)[2]
  gg_dta <- gg_dta[, -which(colnames(gg_dta) == rsp)]

  gg_dta$yhat <- as.vector(object$predicted)

  class(gg_dta) <- c("gg_variable", object$type, class(gg_dta))
  invisible(gg_dta)
}
