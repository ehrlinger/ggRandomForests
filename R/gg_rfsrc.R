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
#' @param object A fitted \code{\link[randomForestSRC]{rfsrc}} or
#'   \code{\link[randomForest]{randomForest}} object.
#' @param by Optional stratifying variable. Either a character column name
#'   present in the training data, or a vector/factor of the same length as
#'   the training set. When supplied, a \code{group} column is added to the
#'   returned data and bootstrap CI bands (survival) are computed per group.
#'   Omit or leave missing to return an unstratified result.
#' @param oob Logical; if \code{TRUE} (default) return out-of-bag predictions.
#'   Set to \code{FALSE} to use full in-bag (training) predictions. Forced to
#'   \code{FALSE} automatically for \code{predict.rfsrc} objects, which carry
#'   no OOB estimates.
#' @param ... Additional arguments controlling output for specific forest
#'   families:
#'   \describe{
#'     \item{surv_type}{Character; one of \code{"surv"} (default),
#'       \code{"chf"}, or \code{"mortality"} for survival forests.}
#'     \item{conf.int}{Numeric coverage probability (e.g. \code{0.95}) to
#'       request bootstrap pointwise confidence bands for survival forests.
#'       Triggers wide-format output with \code{lower}, \code{upper},
#'       \code{median}, and \code{mean} columns.}
#'     \item{bs.sample}{Integer; number of bootstrap resamples when
#'       \code{conf.int} is set. Defaults to the number of observations.}
#'   }
#'
#' @return A \code{gg_rfsrc} object (a classed \code{data.frame}) whose
#'   structure depends on the forest family:
#'   \describe{
#'     \item{regression}{Columns \code{yhat} and the response name; optionally
#'       a \code{group} column when \code{by} is supplied.}
#'     \item{classification}{One column per class with predicted probabilities;
#'       a \code{y} column with observed class labels; optionally \code{group}.}
#'     \item{survival (no CI / grouping)}{Long-format with columns
#'       \code{variable} (event time), \code{value} (survival probability),
#'       \code{obs_id}, and \code{event}.}
#'     \item{survival (with \code{conf.int} or \code{by})}{Wide-format with
#'       pointwise bootstrap CI columns (\code{lower}, \code{upper},
#'       \code{median}, \code{mean}) per time point; a \code{group} column
#'       when \code{by} is supplied.}
#'   }
#'   The object carries class attributes for the forest family so that
#'   \code{\link{plot.gg_rfsrc}} dispatches correctly.
#'
#' @details
#'   For survival forests, use the \code{surv_type} argument
#'   (\code{"surv"}, \code{"chf"}, or \code{"mortality"}) to select the
#'   predicted quantity. Bootstrap confidence bands are requested by passing
#'   \code{conf.int} (e.g. \code{conf.int = 0.95}); the number of resamples
#'   is controlled by \code{bs.sample}.
#'
#' @seealso \code{\link{plot.gg_rfsrc}},
#'   \code{\link[randomForestSRC]{rfsrc}},
#'   \code{\link{gg_survival}}
#'
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example (small, runs on CRAN)
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' set.seed(42)
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris, ntree = 50)
#' gg_dta <- gg_rfsrc(rfsrc_iris)
#' plot(gg_dta)
#'
#' \donttest{
#' ## ------------------------------------------------------------
#' ## Additional regression / survival examples are guarded with
#' ## \donttest because the cumulative example time exceeds the
#' ## 10-second CRAN budget. Run locally with `R CMD check --run-donttest`
#' ## (or `devtools::check(run_dont_test = TRUE)`) to exercise them.
#' ## ------------------------------------------------------------
#'
#' ## -------- air quality data (regression)
#' rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality,
#'                     na.action = "na.impute", ntree = 50)
#' plot(gg_rfsrc(rfsrc_airq))
#'
#' ## -------- Boston data (rfsrc + randomForest)
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   data(Boston, package = "MASS")
#'   Boston$chas <- as.logical(Boston$chas)
#'   rfsrc_boston <- rfsrc(medv ~ ., data = Boston, ntree = 50,
#'                         forest = TRUE, importance = TRUE,
#'                         tree.err = TRUE, save.memory = TRUE)
#'   plot(gg_rfsrc(rfsrc_boston))
#'
#'   rf_boston <- randomForest::randomForest(medv ~ ., data = Boston,
#'                                           ntree = 50)
#'   plot(gg_rfsrc(rf_boston))
#' }
#'
#' ## -------- mtcars data
#' rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars, ntree = 50)
#' plot(gg_rfsrc(rfsrc_mtcars))
#'
#' ## -------- veteran data (survival; with CI and group-by)
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran,
#'                        ntree = 50)
#' plot(gg_rfsrc(rfsrc_veteran))
#' plot(gg_rfsrc(rfsrc_veteran, conf.int = .95))
#' plot(gg_rfsrc(rfsrc_veteran, by = "trt"))
#' }
#'
#' @aliases gg_rfsrc gg_rfsrc.rfsrc

#' @export
gg_rfsrc.rfsrc <- function(object, # nolint: cyclocomp_linter
                           oob = TRUE,
                           by,
                           ...) {
  ## Check that the input object is of the correct type.
  if (!inherits(object, "rfsrc")) {
    stop(
      paste(
        "This function only works for Forests grown with the",
        "randomForestSRC package."
      )
    )
  }
  # The forest object itself must be stored; rfsrc uses forest = TRUE for this.
  if (is.null(object$forest)) {
    stop(
      paste(
        "The function requires the \"forest = TRUE\"",
        "attribute when growing the randomForest"
      )
    )
  }

  # Collect optional arguments (conf.int, surv_type, bs.sample, etc.)
  arg_list <- list(...)

  # Prediction objects do not have OOB predictions; force full-forest preds.
  if (inherits(object, "predict")) {
    oob <- FALSE
  }

  ## ---- Resolve the stratifying variable ("by") ----------------------------
  if (!missing(by)) {
    grp <- by
    # Accept either a column name (character) or a pre-built vector/factor.
    if (is.character(grp)) {
      if (!grp %in% colnames(object$xvar)) {
        stop(paste("No column named", grp, "in forest training set."))
      }
      grp <- object$xvar[, grp]
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
          "of training data"
        )
      )
    }
    # Convert to factor with levels in order of first appearance to preserve
    # the natural ordering of groups in downstream plots.
    grp <- factor(grp, levels = unique(grp))
  }

  ## ---- Classification branch -----------------------------------------------
  if (object$family == "class") {
    # For binary classification rfsrc stores exactly two probability columns
    # (one per class); we drop the first (the "negative" class probability)
    # since it is redundant — prob(class 2) = 1 - prob(class 1).
    # Multi-class forests (3+ classes) keep all columns.
    if (oob) {
      gg_dta <-
        if (ncol(object$predicted.oob) == 2) {
          data.frame(cbind(object$predicted.oob[, -1]))
        } else {
          data.frame(cbind(object$predicted.oob))
        }
    } else {
      gg_dta <- if (ncol(object$predicted) == 2) {
        data.frame(cbind(object$predicted[, -1]))
      } else {
        data.frame(cbind(object$predicted))
      }
    }

    # Assign column names and append the observed class label column "y".
    if (ncol(gg_dta) == 1) {
      colnames(gg_dta) <- object$yvar.names
      # rfsrc internally stores binary outcomes as a two-level factor even
      # when the original response is logical; convert back to logical here.
      gg_dta$y <- as.logical(as.numeric(object$yvar) - 1)
    } else {
      colnames(gg_dta) <- levels(object$yvar)
      gg_dta$y <- object$yvar
    }

    if (!missing(by)) {
      gg_dta$group <- grp
    }

  ## ---- Survival branch -----------------------------------------------------
  } else if (object$family == "surv") {
    # Default to overall survival; callers can request CHF or mortality.
    surv_type <- "surv"

    if (!is.null(arg_list$surv_type)) {
      surv_type <- arg_list$surv_type
    }

    # Extract the appropriate prediction matrix (rows = observations,
    # columns = unique time points stored in $time.interest).
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

    # Name each column with the corresponding observed event time.
    colnames(rng) <- object$time.interest

    rng$obs_id <- seq_len(nrow(rng))
    if (is.null(object$yvar)) {
      rng$event <- FALSE
    } else {
      # Second column of yvar is the event indicator (0 = censored, 1 = event).
      rng$event <- as.logical(object$yvar[, 2])
    }
    gg_dta <- rng

    if (is.null(arg_list$conf.int) && missing(by)) {
      # No grouping or CI requested: pivot to long form so plot.gg_rfsrc can
      # draw one survival step function per observation.
      pivot_cols <-
        colnames(gg_dta)[-which(colnames(gg_dta) %in% c("obs_id", "event"))]
      gg_dta_mlt <-
        tidyr::pivot_longer(gg_dta, tidyr::all_of(pivot_cols), names_to = "variable", values_to = "value")
      gg_dta_mlt$variable <-
        as.numeric(as.character(gg_dta_mlt$variable))
      gg_dta_mlt$obs_id <- factor(gg_dta_mlt$obs_id)

      gg_dta <- gg_dta_mlt
    } else {
      # Bootstrap confidence bands: compute pointwise quantiles of the
      # bootstrapped mean survival curve at each time point.
      level <- if (is.null(arg_list$conf.int)) {
        .95
      } else {
        arg_list$conf.int
      }

      # Accept a single coverage probability (e.g. 0.95 → two-sided tails
      # at 0.025 and 0.975) or a length-2 vector of explicit quantile probs.
      if (length(level) == 1) {
        if (level > 1) {
          level <- level / 100
        }

        level_set <- c((1 - level) / 2, 1 - (1 - level) / 2)
        level_set <- sort(level_set)
      } else {
        level_set <- sort(level)
      }

      # Number of bootstrap resamples — default to the number of observations.
      if (is.null(arg_list$bs.sample)) {
        bs_samples <- nrow(gg_dta)
      } else {
        bs_samples <- arg_list$bs.sample
      }

      if (!missing(by)) {
        # Stratified: compute separate bootstrap CI bands per group level.
        gg_dta$group <- grp
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
        # Single-group CI band.
        gg_dta <- bootstrap_survival(gg_dta, bs_samples, level_set)
      }
    }

  ## ---- Regression branch ---------------------------------------------------
  } else if (object$family == "regr") {
    # Bind predicted values alongside the observed response for scatter plots.
    if (oob) {
      gg_dta <- data.frame(cbind(object$predicted.oob, object$yvar))
    } else {
      gg_dta <- data.frame(cbind(object$predicted, object$yvar))
    }

    colnames(gg_dta) <- c("yhat", object$yvar.names)

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

  # Tag the data frame with the forest family so plot.gg_rfsrc can dispatch
  # to the correct branch without re-inspecting the original forest object.
  class(gg_dta) <- c("gg_rfsrc", object$family, class(gg_dta))
  invisible(gg_dta)
}



#' Bootstrap pointwise confidence bands for a mean survival curve
#'
#' Draws \code{bs_samples} bootstrap resamples (with replacement) of the
#' per-observation survival curves stored in \code{gg_dta}, computes the column
#' means to obtain a bootstrapped mean curve per resample, then returns the
#' pointwise quantiles at \code{level_set} and the overall mean across
#' resamples.
#'
#' @param gg_dta A wide \code{data.frame} of survival probabilities as returned
#'   by the survival branch of \code{\link{gg_rfsrc.rfsrc}}, before the
#'   optional pivot to long form. Columns \code{obs_id}, \code{event}, and
#'   \code{group} (if present) are excluded from the resampling.
#' @param bs_samples Integer; number of bootstrap resamples.
#' @param level_set Numeric vector of length 2 giving the lower and upper
#'   quantile probabilities for the confidence band (e.g. \code{c(0.025, 0.975)}
#'   for a 95\% CI).
#'
#' @return A \code{data.frame} with one row per unique event time and columns
#'   \code{value} (time), \code{lower}, \code{upper}, \code{median}, and
#'   \code{mean}.
#'
#' @keywords internal
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

  # gg_t already has obs_id/event/group stripped; rng and mn are indexed over
  # time points only, so no further exclusion is needed.
  time_interest <- as.numeric(colnames(gg_t))

  dta <- data.frame(cbind(
    time_interest,
    t(rng),
    mn
  ))

  # rng always has 3 rows: lower quantile, upper quantile, median (.5).
  # Name columns canonically so plot.gg_rfsrc can always find "lower"/"upper".
  colnames(dta) <- c("value", "lower", "upper", "median", "mean")
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
  ## Check that the input object is of the correct type.
  if (!inherits(object, "randomForest")) {
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

  # Recover the training predictor frame once (needed for by-column lookup and
  # dimension checks).  randomForest stores predictors in $forest$xlevels keys
  # but not the actual data; use .rf_recover_model_frame() for that.
  rf_info <- .rf_recover_model_frame(object) # nolint: object_usage_linter
  rf_xvar <- if (!is.null(rf_info)) rf_info$model_frame[
    , setdiff(colnames(rf_info$model_frame), rf_info$response_name),
    drop = FALSE
  ] else NULL
  n_train <- length(object$predicted)

  if (!missing(by)) {
    grp <- by
    # Accept either a column name (character) or a pre-built vector/factor.
    if (is.character(grp)) {
      if (is.null(rf_xvar) || !grp %in% colnames(rf_xvar)) {
        stop(paste("No column named", grp, "in forest training set."))
      }
      grp <- rf_xvar[, grp]
    }

    if (is.vector(grp) || is.factor(grp)) {
      if (length(grp) != n_train) {
        stop(paste(
          "By argument does not have the correct dimension ",
          n_train
        ))
      }
    } else {
      stop(
        paste(
          "By argument should be either a vector, or colname",
          "of training data"
        )
      )
    }
    grp <- factor(grp, levels = unique(grp))
  }

  # Extract the response variable name from the formula for column naming below.
  # (Both branches below build gg_dta from the forest's stored predictions, so
  # there is no need to recover the original training data frame here.)
  rsp <- as.character(object$call$formula)[2]

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
