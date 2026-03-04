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
#' Variable Importance (VIMP) data object
#'
#' \code{gg_vimp} Extracts the variable importance (VIMP) information from a
#' \code{\link[randomForestSRC]{rfsrc}} or \code{\link[randomForest]{randomForest}}
#' object and reshapes it into a tidy data set.
#'
#' @param object A \code{\link[randomForestSRC]{rfsrc}} object, the output from
#' \code{\link[randomForestSRC]{vimp}}, or a fitted
#' \code{\link[randomForest]{randomForest}}.
#' @param nvar argument to control the number of variables included in the
#' output.
#' @param ... arguments passed to the \code{\link[randomForestSRC]{vimp.rfsrc}}
#' function if the \code{\link[randomForestSRC]{rfsrc}} object does not contain
#' importance information.
#'
#' @return \code{gg_vimp} object. A \code{data.frame} of VIMP measures, in rank
#'   order, optionally containing class-specific scores and a relative importance
#'   column. When \code{randomForest} objects lack stored importance values a
#'   warning is issued and \code{NA} placeholders are returned so plots remain
#'   reproducible.
#'
#' @seealso \code{\link{plot.gg_vimp}} \code{\link[randomForestSRC]{rfsrc}}
#' @seealso \code{\link[randomForestSRC]{vimp}}
#'
#' @references
#' Ishwaran H. (2007). Variable importance in binary regression trees and
#' forests, \emph{Electronic J. Statist.}, 1:519-537.
#'
#' @importFrom randomForestSRC vimp
#'
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' rfsrc_iris <- rfsrc(Species ~ .,
#'   data = iris,
#'   importance = TRUE
#' )
#' gg_dta <- gg_vimp(rfsrc_iris)
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' 
#' ## -------- air quality data
#' rfsrc_airq <- rfsrc(Ozone ~ ., airquality,
#'   importance = TRUE
#' )
#' gg_dta <- gg_vimp(rfsrc_airq)
#' plot(gg_dta)
#' 
#'
#' ## -------- Boston data
#' data(Boston, package = "MASS")
#' rfsrc_boston <- randomForestSRC::rfsrc(medv ~ ., Boston,
#'   importance = TRUE
#' )
#' gg_dta <- gg_vimp(rfsrc_boston)
#' plot(gg_dta)
#'
#' ## -------- Boston data
#' rf_boston <- randomForest::randomForest(medv ~ ., Boston)
#' gg_dta <- gg_vimp(rf_boston)
#' plot(gg_dta)
#'
#' 
#' ## -------- mtcars data
#' rfsrc_mtcars <- rfsrc(mpg ~ .,
#'   data = mtcars,
#'   importance = TRUE
#' )
#' gg_dta <- gg_vimp(rfsrc_mtcars)
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## survival example
#' ## ------------------------------------------------------------
#' 
#' ## -------- veteran data
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ .,
#'   data = veteran,
#'   ntree = 100,
#'   importance = TRUE
#' )
#'
#' gg_dta <- gg_vimp(rfsrc_veteran)
#' plot(gg_dta)
#'
#' ## -------- pbc data
#' # We need to create this dataset
#' data(pbc, package = "randomForestSRC", )
#' # For whatever reason, the age variable is in days...
#' # makes no sense to me
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
#' gg_dta <- gg_vimp(rfsrc_pbc)
#' plot(gg_dta)
#'
#' # Restrict to only the top 10.
#' gg_dta <- gg_vimp(rfsrc_pbc, nvar = 10)
#' plot(gg_dta)
#' 
#' @aliases gg_vimp gg_vimp.rfsrc gg_vimp.randomForest
#' @aliases gg_vimp.randomForest.formula
#' @export
gg_vimp <- function(object, nvar, ...) {
  UseMethod("gg_vimp", object)
}
#' @export
gg_vimp.rfsrc <- function(object, nvar, ...) {
  # Validate that the object is an rfsrc grow or predict result.
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &&
    sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)' or
         '(rfsrc, predict)'.")
  }

  # If the forest was grown without importance = TRUE, compute VIMP now via
  # a separate vimp() call (slower, but always safe).
  if (is.null(object$importance)) {
    warning("rfsrc object does not contain VIMP information. Calculating...")
    gg_dta <-
      data.frame(sort(randomForestSRC::vimp(object)$importance,
        decreasing = TRUE
      ))
  } else {
    gg_dta <- data.frame(object$importance)
  }

  # Single-outcome forests: $importance is a named vector → one-column df.
  # Rename the column and add a "vars" column with the variable names.
  if (ncol(gg_dta) == 1) {
    colnames(gg_dta) <- "VIMP"
    gg_dta$vars <- rownames(gg_dta)
    gg_dta <- gg_dta[order(gg_dta$VIMP, decreasing = TRUE), ]
  }

  # Clamp nvar to the number of available variables.
  if (missing(nvar)) {
    nvar <- nrow(gg_dta)
  }
  if (nvar > nrow(gg_dta)) {
    nvar <- nrow(gg_dta)
  }

  # Multi-class forests: $importance is a matrix (vars × classes).
  # Pivot to long form so each row is one (variable, class) combination.
  if (ncol(gg_dta) > 1) {
    arg_set <- list(...)

    if (!is.null(arg_set$which.outcome)) {
      # Caller requested importance for one specific class only.
      if (!is.numeric(arg_set$which.outcome)) {
        # Look up by class name (column name).
        if (arg_set$which.outcome %in% colnames(gg_dta)) {
          gg_v <- data.frame(vimp = sort(gg_dta[, arg_set$which.outcome],
            decreasing = TRUE
          ))
          gg_v$vars <-
            rownames(gg_dta)[order(gg_dta[, arg_set$which.outcome],
              decreasing = TRUE
            )]
        } else {
          stop(
            paste(
              "which.outcome naming is incorrect.",
              arg_set$which.outcome,
              "\nis not in",
              colnames(gg_dta)
            )
          )
        }
      } else {
        # Look up by integer index (1-based into class columns).
        if (arg_set$which.outcome < ncol(gg_dta)) {
          gg_v <- data.frame(vimp = sort(gg_dta[, arg_set$which.outcome + 1],
            decreasing = TRUE
          ))
          gg_v$vars <-
            rownames(gg_dta)[order(gg_dta[, arg_set$which.outcome + 1],
              decreasing = TRUE
            )]
        } else {
          stop(
            paste(
              "which.outcome specified larger than the number of classes (+1).",
              arg_set$which.outcome,
              " >= ",
              ncol(gg_dta)
            )
          )
        }
      }
      gg_dta <- gg_v
    } else {
      # No specific class requested: attach variable names and pivot.
      gg_dta$vars <- rownames(gg_dta)
    }

    gg_dta <- gg_dta[seq_len(nvar), ]
    gathercols <-
      colnames(gg_dta)[-which(colnames(gg_dta) == "vars")]
    # Pivot from wide (one column per class) to long (one row per class-var pair).
    gg_dta <- tidyr::gather(
      gg_dta, "set", "vimp",
      tidyr::all_of(gathercols)
    )
    gg_dta <- gg_dta[order(gg_dta$vimp, decreasing = TRUE), ]
    gg_dta$vars <- factor(gg_dta$vars)
  } else {
    # Single-outcome: compute relative VIMP (each value as a fraction of the
    # top-ranked variable's importance).
    cnms <- colnames(gg_dta)
    gg_dta <- cbind(gg_dta, gg_dta / gg_dta[1, 1])
    colnames(gg_dta) <- c(cnms, "rel_vimp")
    # Patch any NA entries in the vars column that slipped through.
    gg_dta$vars[which(is.na(gg_dta$vars))] <-
      rownames(gg_dta)[which(is.na(gg_dta$vars))]

    gg_dta <- gg_dta[1:nvar, ]
  }

  # Convert vars to an ordered factor (reversed so the most important variable
  # plots at the top of a horizontal bar chart after coord_flip).
  gg_dta$vars <-
    factor(gg_dta$vars, levels = rev(unique(gg_dta$vars)))

  # Flag variables with non-positive VIMP so the plot can colour them
  # differently to indicate they do not improve (or actively hurt) predictions.
  gg_dta$positive <- TRUE
  gg_dta$positive[which(gg_dta$vimp <= 0)] <- FALSE

  class(gg_dta) <- c("gg_vimp", class(gg_dta))
  invisible(gg_dta)
}

#' @export
gg_vimp.randomForest <- function(object, nvar, ...) {
  ## Check that the input obect is of the correct type.
  if (!inherits(object, "randomForest")) {
    stop(
      paste(
        "This function only works for Forests grown",
        "with the randomForest package."
      )
    )
  }

  ### set importance to NA if it is NULL
  if (is.null(object$importance)) {
    importance_est <- tryCatch(
      randomForest::importance(object, scale = FALSE),
      error = function(e) NULL
    )
    if (is.null(importance_est)) {
      vars <- object$forest$xvar.names
      if (is.null(vars)) {
        training_info <- .rf_recover_model_frame(object)
        if (!is.null(training_info)) {
          vars <- setdiff(colnames(training_info$model_frame), training_info$response_name)
        }
      }
      if (is.null(vars)) {
        stop(
          "Variable importance is unavailable for this randomForest object and ",
          "training predictors cannot be recovered."
        )
      }
      warning(
        "randomForest object does not contain importance information. Returning NA values."
      )
      placeholder <- data.frame(
        vimp = rep(NA_real_, length(vars)),
        vars = vars,
        rel_vimp = NA_real_
      )
      placeholder$vars <- factor(placeholder$vars, levels = rev(unique(placeholder$vars)))
      placeholder$positive <- FALSE
      class(placeholder) <- c("gg_vimp", class(placeholder))
      return(placeholder)
    }
    gg_dta <- data.frame(importance_est)
  } else {
    gg_dta <- data.frame(object$importance)
  }
  if (ncol(gg_dta) < 3) {
    gg_dta$vars <- rownames(gg_dta)
    colnames(gg_dta)[which(colnames(gg_dta) == "X.IncMSE")] <-
      "vimp"
    if ("vimp" %in% colnames(gg_dta)) {
      gg_dta <- gg_dta[order(gg_dta$vimp, decreasing = TRUE), ]
    } else {
      cn <- colnames(gg_dta)[1]
      gg_dta <-
        gg_dta[order(gg_dta[, cn], decreasing = TRUE), ]
    }
  }
  if (missing(nvar)) {
    nvar <- nrow(gg_dta)
  }
  if (nvar > nrow(gg_dta)) {
    nvar <- nrow(gg_dta)
  }

  # Handle multiclass importance
  if (ncol(gg_dta) > 1) {
    # Classification...
    arg_set <- list(...)

    if (!is.null(arg_set$which.outcome)) {
      # test which.outcome specification
      if (!is.numeric(arg_set$which.outcome)) {
        if (arg_set$which.outcome %in% colnames(gg_dta)) {
          gg_v <- data.frame(vimp = sort(gg_dta[, arg_set$which.outcome],
            decreasing = TRUE
          ))
          gg_v$vars <-
            rownames(gg_dta)[order(gg_dta[, arg_set$which.outcome],
              decreasing = TRUE
            )]
        } else {
          stop(
            paste(
              "which.outcome naming is incorrect.",
              arg_set$which.outcome,
              "\nis not in",
              colnames(gg_dta)
            )
          )
        }
      } else {
        if (arg_set$which.outcome < ncol(gg_dta)) {
          gg_v <- data.frame(vimp = sort(gg_dta[, arg_set$which.outcome + 1],
            decreasing = TRUE
          ))
          gg_v$vars <-
            rownames(gg_dta)[order(gg_dta[, arg_set$which.outcome + 1],
              decreasing = TRUE
            )]
        } else {
          stop(
            paste(
              "which.outcome specified larger than the number of classes (+1).",
              arg_set$which.outcome,
              " >= ",
              ncol(gg_dta)
            )
          )
        }
      }
      gg_dta <- gg_v
    } else {
      gg_dta$vars <- rownames(gg_dta)
    }

    gathercols <-
      colnames(gg_dta)[-which(colnames(gg_dta) == "vars")]
    gg_dta <- tidyr::gather(
      gg_dta, "set", "vimp",
      tidyr::all_of(gathercols)
    )
    gg_dta <- gg_dta[order(gg_dta$vimp, decreasing = TRUE), ]
    gg_dta$vars <- factor(gg_dta$vars)
  } else {
    gg_dta$vars[which(is.na(gg_dta$vars))] <-
      rownames(gg_dta)[which(is.na(gg_dta$vars))]
  }
  gg_dta <- gg_dta[1:nvar, ]

  gg_dta$vars <-
    factor(gg_dta$vars, levels = rev(unique(gg_dta$vars)))
  gg_dta$positive <- TRUE
  gg_dta$positive[which(gg_dta$vimp <= 0)] <- FALSE

  class(gg_dta) <- c("gg_vimp", class(gg_dta))
  invisible(gg_dta)
}
