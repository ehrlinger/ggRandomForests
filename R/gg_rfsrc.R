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
#' Predicted response data object
#'
#' Extracts the predicted response values from the
#' \code{\link[randomForestSRC]{rfsrc}} object, and formats data for plotting
#' the response using \code{\link{plot.gg_rfsrc}}.
#'
#' @param object \code{\link[randomForestSRC]{rfsrc}} object
#' @param by stratifying variable in the training dataset, defaults to NULL
#' @param oob boolean, should we return the oob prediction , or the full
#' forest prediction.
#' @param ... extra arguments
#'
#' @return \code{gg_rfsrc} object
#'
#' @details
#'    \code{surv_type} ("surv", "chf", "mortality", "hazard") for survival
#'    forests
#'
#'    \code{oob} boolean, should we return the oob prediction , or the full
#' forest prediction.
#'
#' @seealso \code{\link{plot.gg_rfsrc}} \code{rfsrc} \code{plot.rfsrc}
#' \code{\link{gg_survival}}
#'
#' #' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' gg_dta<- gg_rfsrc(rfsrc_iris)
#'
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#'
#' ## -------- air quality data
#' rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' gg_dta<- gg_rfsrc(rfsrc_airq)
#'
#' plot(gg_dta)
#'
#'
#' ## -------- Boston data
#' data(Boston, package = "MASS")
#' Boston$chas <- as.logical(Boston$chas)
#' rfsrc_boston <- rfsrc(medv ~ .,
#'    data = Boston,
#'    forest = TRUE,
#'    importance = TRUE,
#'    tree.err = TRUE,
#'    save.memory = TRUE)
#'
#' plot(gg_rfsrc(rfsrc_boston))
#'
#' ### randomForest example
#' data(Boston, package="MASS")
#' rf_boston <- randomForest::randomForest(medv ~ ., data = Boston)
#' plot(gg_rfsrc(rf_boston))
#'
#'
#' ## -------- mtcars data
#' rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
#' gg_dta<- gg_rfsrc(rfsrc_mtcars)
#'
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## -------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' gg_dta <- gg_rfsrc(rfsrc_veteran)
#' plot(gg_dta)
#'
#' gg_dta <- gg_rfsrc(rfsrc_veteran, conf.int=.95)
#' plot(gg_dta)
#'
#' gg_dta <- gg_rfsrc(rfsrc_veteran, by="trt")
#' plot(gg_dta)
#'
#'
#' ## -------- pbc data
#' ## We don't run this because of bootstrap confidence limits
#' # We need to create this dataset
#' data(pbc, package = "randomForestSRC",)
#' # For whatever reason, the age variable is in days... makes no sense to me
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
#'  pbc_test <- pbc[which(is.na(pbc$treatment)), ]
#'
#' #========
#' # build the forest:
#' rfsrc_pbc <- randomForestSRC::rfsrc(
#'   Surv(years, status) ~ .,
#'  dta_train,
#'  nsplit = 10,
#'  na.action = "na.impute",
#'  forest = TRUE,
#'  importance = TRUE,
#'  save.memory = TRUE
#' )
#' gg_dta <- gg_rfsrc(rfsrc_pbc)
#' plot(gg_dta)
#'
#' gg_dta <- gg_rfsrc(rfsrc_pbc, conf.int=.95)
#' plot(gg_dta)
#'
#'
#' gg_dta <- gg_rfsrc(rfsrc_pbc, by="treatment")
#' plot(gg_dta)
#'
#'
#' @aliases gg_rfsrc gg_rfsrc.rfsrc

#' @export
gg_rfsrc.rfsrc <- function(object,
                           oob = TRUE,
                           by,
                           ...) {
  ## Check that the input obect is of the correct type.
  if (inherits(object, "rfsrc") == FALSE) {
    stop(
      paste(
        "This function only works for Forests grown with the",
        "randomForestSRC package."
      )
    )
  }
  if (is.null(object$forest)) {
    stop(
      paste(
        "The function requires the \"forest = TRUE\"",
        "attribute when growing the randomForest"
      )
    )
  }

  # get optional arguments
  arg_list <- list(...)

  if (inherits(object, "predict")) {
    oob <- FALSE
  }

  if (!missing(by)) {
    grp <- by
    # If the by argument is a vector, make sure it is the correct length
    if (is.character(grp)) {
      if (is.null(object$xvar[, grp])) {
        stop(paste("No column named", grp, "in forest training set."))
      } else {
        grp <- object$xvar[, grp]
      }
    }

    if (is.vector(grp) || is.factor(grp)) {
      if (length(grp) != nrow(object$xvar)) {
        stop(paste(
          "By argument does not have the correct dimension ",
          nrow(object$xvar)
        ))
      }
    } else {
      stop(
        paste(
          "By argument should be either a vector, or colname",
          "of training data",
          nrow(object$xvar)
        )
      )
    }
    grp <- factor(grp, levels = unique(grp))
  }


  if (object$family == "class") {
    ### Classification models...

    # Need to add multiclass methods
    if (oob) {
      gg_dta <-
        if (ncol(object$predicted.oob) <= 2) {
          data.frame(cbind(object$predicted.oob[, -1]))
        } else {
          data.frame(cbind(object$predicted.oob))
        }
    } else {
      gg_dta <- if (ncol(object$predicted) <= 2) {
        data.frame(cbind(object$predicted[, -1]))
      } else {
        data.frame(cbind(object$predicted))
      }
    }

    # Switch between logical or categorical outcome
    if (ncol(gg_dta) == 1) {
      colnames(gg_dta) <- object$yvar.names
      # Force this to logical return value...
      #
      # This may be a bug in rfsrc, as it converts all classification models
      # into factors.
      gg_dta$y <- as.logical(as.numeric(object$yvar) - 1)
    } else {
      colnames(gg_dta) <- levels(object$yvar)
      gg_dta$y <- object$yvar
    }

    # Handle the "by" argument.
    if (!missing(by)) {
      gg_dta$group <- grp
    }
  } else if (object$family == "surv") {
    ### Survival models
    surv_type <- "surv"

    if (!is.null(arg_list$surv_type)) {
      surv_type <- arg_list$surv_type
    }

    if (oob) {
      rng <- switch(surv_type,
        surv = data.frame(object$survival.oob),
        chf = data.frame(object$chf.oob),
        mortality = data.frame(1 - object$survival.oob),
        stop(paste(
          surv_type, " not implemented at this time"
        ))
      )
    } else {
      rng <- switch(surv_type,
        surv = data.frame(object$survival),
        chf = data.frame(object$chf),
        mortality = data.frame(1 - object$survival),
        stop(paste(
          surv_type, " not implemented at this time"
        ))
      )
    }

    # Do we want all lines, or bootstrap confidence bands.
    colnames(rng) <- object$time.interest

    rng$obs_id <- seq_len(nrow(rng))
    if (is.null(object$yvar)) {
      rng$event <- FALSE
    } else {
      rng$event <- as.logical(object$yvar[, 2])
    }
    gg_dta <- rng

    # If we don't specify either a conf band or group by variable...
    # Then we want to plot a curve for each observation.
    if (is.null(arg_list$conf.int) && missing(by)) {
      gathercols <-
        colnames(gg_dta)[-which(colnames(gg_dta) %in% c("obs_id", "event"))]
      gg_dta_mlt <-
        tidyr::gather(gg_dta, "variable", "value", tidyr::all_of(gathercols))
      gg_dta_mlt$variable <-
        as.numeric(as.character(gg_dta_mlt$variable))
      gg_dta_mlt$obs_id <- factor(gg_dta_mlt$obs_id)

      gg_dta <- gg_dta_mlt
    } else {
      level <- if (is.null(arg_list$conf.int)) {
        .95
      } else {
        arg_list$conf.int
      }

      # If we have one value, then it's two sided.
      if (length(level) == 1) {
        if (level > 1) {
          level <- level / 100
        }

        level_set <- c((1 - level) / 2, 1 - (1 - level) / 2)
        level_set <- sort(level_set)
      } else {
        level_set <- sort(level)
      }

      if (is.null(arg_list$bs.sample)) {
        bs_samples <- nrow(gg_dta)
      } else {
        bs_samples <- arg_list$bs.sample
      }


      if (!missing(by)) {
        gg_dta$group <- grp
        #####
        grp_dta <- lapply(levels(grp), function(st) {
          if (is.null(arg_list$bs.sample)) {
            bs_samples <-
              nrow(gg_dta[which(as.character(gg_dta$group) == st), ])
          }

          obj <-
            bootstrap_survival(
              gg_dta[which(as.character(gg_dta$group) == st), ],
              bs_samples, level_set
            )
          obj$group <- st
          obj
        })
        gg_grp <- do.call(rbind, grp_dta)
        gg_grp$group <- factor(gg_grp$group,
          levels = unique(gg_grp$group)
        )
        gg_dta <- gg_grp
      } else {
        gg_dta <- bootstrap_survival(gg_dta, bs_samples, level_set)
      }
    }
  } else if (object$family == "regr") {
    # Need to add multiclass methods
    if (oob) {
      gg_dta <- data.frame(cbind(object$predicted.oob, object$yvar))
    } else {
      gg_dta <- data.frame(cbind(object$predicted, object$yvar))
    }

    colnames(gg_dta) <- c("yhat", object$yvar.names)

    # Handle the "by" argument.
    if (!missing(by)) {
      gg_dta$group <- grp
    }
  } else {
    stop(
      paste(
        "Plotting for ",
        object$family,
        " randomForestSRC is not yet implemented.",
        sep = ""
      )
    )
  }

  class(gg_dta) <- c("gg_rfsrc", object$family, class(gg_dta))
  invisible(gg_dta)
}



bootstrap_survival <- function(gg_dta, bs_samples, level_set) {
  ## Calculate the leave one out estimate of the mean survival
  gg_t <-
    gg_dta[, -which(colnames(gg_dta) %in% c("obs_id", "event", "group"))]
  mn_bs <- t(sapply(
    seq_len(bs_samples),
    function(pat) {
      st <- sample(seq_len(nrow(gg_t)),
        size = nrow(gg_t),
        replace = TRUE
      )
      colMeans(gg_t[st, ])
    }
  ))

  ## now get the confidence interval of the mean, and the median (.5)
  rng <- sapply(
    seq_len(ncol(mn_bs)),
    function(t_pt) {
      quantile(mn_bs[, t_pt], probs = c(level_set, .5))
    }
  )
  mn <- sapply(seq_len(ncol(rng)), function(t_pt) {
    mean(rng[, t_pt])
  })

  time_interest <- as.numeric(colnames(gg_t))

  dta <- data.frame(cbind(
    time_interest,
    t(rng)[-which(colnames(gg_dta) %in%
      c("obs_id", "event")), ],
    mn[-which(colnames(gg_dta) %in%
      c("obs_id", "event"))]
  ))

  if (ncol(dta) == 5) {
    colnames(dta) <- c("value", "lower", "upper", "median", "mean")
  } else {
    colnames(dta) <- c("value", level_set, "mean")
  }
  dta
}

#' @export
gg_rfsrc <- function(object,
                     oob = TRUE,
                     by, ...) {
  UseMethod("gg_rfsrc", object)
}

#' @export
gg_rfsrc.randomForest <- function(object,
                                  oob = TRUE,
                                  by,
                                  ...) {
  ## Check that the input obect is of the correct type.
  if (inherits(object, "randomForest") == FALSE) {
    stop(
      paste(
        "This function only works for Forests grown with the",
        "randomForest package."
      )
    )
  }

  if (inherits(object, "predict")) {
    oob <- FALSE
  }

  if (!missing(by)) {
    grp <- by
    # If the by argument is a vector, make sure it is the correct length
    if (is.character(grp)) {
      if (is.null(object$xvar[, grp])) {
        stop(paste("No column named", grp, "in forest training set."))
      } else {
        grp <- object$xvar[, grp]
      }
    }

    if (is.vector(grp) || is.factor(grp)) {
      if (length(grp) != nrow(object$xvar)) {
        stop(paste(
          "By argument does not have the correct dimension ",
          nrow(object$xvar)
        ))
      }
    } else {
      stop(
        paste(
          "By argument should be either a vector, or colname",
          "of training data",
          nrow(object$xvar)
        )
      )
    }
    grp <- factor(grp, levels = unique(grp))
  }

  # gg_variable is really just the training data and the outcome.
  gg_dta <- get(as.character(object$call$data))

  # Remove the response from the data.frame
  rsp <- as.character(object$call$formula)[2]
  gg_dta <- gg_dta[, -which(colnames(gg_dta) == rsp)]

  # Do the work...
  if (object$type == "classification") {
    ### Classification models...

    # Need to add multiclass methods
    prd <- predict(object, type = "prob")
    gg_dta <- if (ncol(prd) <= 2) {
      data.frame(cbind(prd[, -1]))
    } else {
      data.frame(cbind(prd))
    }
    colnames(gg_dta) <- object$classes

    # Switch between logical or categorical outcome
    if (ncol(gg_dta) == 1) {
      # Force this to logical return value...
      #
      # This may be a bug in rfsrc, as it converts all classification models
      # into factors.
      gg_dta$y <- as.logical(as.numeric(object$yvar) - 1)
    } else {
      gg_dta$y <- object$y
    }

    # Handle the "by" argument.
    if (!missing(by)) {
      gg_dta$group <- grp
    }
  } else if (object$type == "regression") {
    gg_dta <- data.frame(cbind(object$predicted, object$y))

    colnames(gg_dta) <- c("yhat", rsp)

    # Handle the "by" argument.
    if (!missing(by)) {
      gg_dta$group <- grp
    }
  } else {
    stop(
      paste(
        "Plotting for ",
        object$family,
        " randomForest is not yet implemented for ",
        object$type,
        sep = ""
      )
    )
  }

  class(gg_dta) <- c("gg_rfsrc", object$type, class(gg_dta))
  invisible(gg_dta)
}
