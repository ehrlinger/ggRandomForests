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
#' @details
#' \code{gg_vimp()} reports whatever importance the forest stored; it computes
#' nothing itself.  Usually that is \strong{permutation (Breiman-Cutler)
#' variable importance}: the forest permutes a variable's observed values
#' across the out-of-bag (OOB) cases, runs those perturbed cases down the
#' already-grown trees, and measures how much the OOB prediction error climbs.
#' That perturbation is synthetic (the variable's link to the response is
#' broken on purpose) so a large increase means the variable was carrying
#' genuine signal; near-zero or negative values mean it added noise or nothing
#' at all.
#'
#' \strong{A \code{randomForest} fit needs \code{importance = TRUE} to give you
#' this.}  \code{randomForest::randomForest()} defaults to
#' \code{importance = FALSE}, and that fit stores only \code{IncNodePurity} --
#' a node-impurity (RSS or Gini) measure, which is not a permutation quantity
#' and is not comparable to one.  It is the only importance the forest kept, so
#' it is what \code{gg_vimp()} reports, in the \code{vimp} column, same as any
#' other.  Nothing marks the difference in the plot.  So
#' \code{gg_vimp(randomForest(y ~ ., data))} ranks by node purity; pass
#' \code{importance = TRUE} and you get permutation VIMP (\code{\%IncMSE}), and
#' \code{colnames(object$importance)} tells you which one you have.
#' \code{randomForestSRC::rfsrc()} has no such trap: its \code{importance}
#' argument yields permutation VIMP.
#'
#' When a \code{randomForest} fit carries both measures, \code{gg_vimp()}
#' reports the permutation one and leaves node purity out of the ranking --
#' the two run on different scales and mean different things, so putting them
#' in one ordering would be meaningless.  Read
#' \code{randomForest::importance(object)} if you want both.  A classification
#' fit names that pair \code{MeanDecreaseAccuracy} and \code{MeanDecreaseGini},
#' and stores a permutation column per class besides.  Those per-class columns
#' are all permutation measures on one scale, so \code{gg_vimp()} keeps them
#' together, names each in the \code{set} column, and drops only the Gini one.
#'
#' \code{\link{gg_varpro}()} takes the opposite route, comparing local
#' estimators on real observed data through varPro's release rules, with no
#' permutation and no synthetic features.  The two approaches answer "which
#' variables matter?" by opposite mechanisms, so a variable can rank
#' differently under each, and that disagreement is itself informative: it
#' often signals interaction structure or non-monotone effects that one
#' mechanism surfaces and the other obscures.
#'
#' For survival forests, VIMP is measured against the ensemble cumulative
#' hazard function (CHF); the error metric is one minus the concordance index
#' (C-statistic).  Variables with non-positive VIMP are flagged in the
#' \code{positive} column and colored differently by
#' \code{\link{plot.gg_vimp}}.
#'
#' @return \code{gg_vimp} object. A \code{data.frame} of VIMP measures, in rank
#'   order, optionally containing class-specific scores and a relative importance
#'   column. When \code{randomForest} objects lack stored importance values a
#'   warning is issued and \code{NA} placeholders are returned so plots remain
#'   reproducible.
#'
#' @seealso \code{\link{plot.gg_vimp}} \code{\link[randomForestSRC]{rfsrc}}
#' @seealso \code{\link[randomForestSRC]{vimp}} \code{\link{gg_varpro}}
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
#' rfsrc_iris <- randomForestSRC::rfsrc(Species ~ .,
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
#' rfsrc_airq <- randomForestSRC::rfsrc(Ozone ~ ., airquality,
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
#' ## importance = TRUE for permutation VIMP; without it randomForest stores
#' ## only IncNodePurity, which is what you would be ranking (see Details).
#' rf_boston <- randomForest::randomForest(medv ~ ., Boston, importance = TRUE)
#' gg_dta <- gg_vimp(rf_boston)
#' plot(gg_dta)
#'
#'
#' ## -------- mtcars data
#' rfsrc_mtcars <- randomForestSRC::rfsrc(mpg ~ .,
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
#' rfsrc_veteran <- randomForestSRC::rfsrc(Surv(time, status) ~ .,
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
gg_vimp.default <- function(object, nvar, ...) {
  stop("gg_vimp: expected an 'rfsrc' or 'randomForest' object; ",
       "got an object of class ", paste(class(object), collapse = "/"), ".",
       call. = FALSE)
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

  # Single-outcome forests: $importance is a named vector, one-column df.
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
      # Caller requested importance for one specific class only. Resolve the
      # request to a column name, then carry that name through to `set` below:
      # naming the measure is the whole point of the `set` column, and the
      # unfiltered pivot in the else branch already does it.
      if (!is.numeric(arg_set$which.outcome)) {
        # Look up by class name (column name).
        if (arg_set$which.outcome %in% colnames(gg_dta)) {
          which_col <- arg_set$which.outcome
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
        # Look up by integer index. rfsrc's $importance leads with an "all"
        # column, so the overall measure really is column 1 here.
        # which.outcome = 0  gives overall (across-class) importance, column 1
        # which.outcome = k  gives importance for class k, column k+1
        if (arg_set$which.outcome < 0) {
          stop("which.outcome must be a non-negative integer or a class name.")
        }
        if (arg_set$which.outcome < ncol(gg_dta)) {
          which_col <- colnames(gg_dta)[arg_set$which.outcome + 1]
        } else {
          stop(
            paste0(
              "which.outcome (", arg_set$which.outcome, ") is out of range. ",
              "Valid values are 0 (overall) to ", ncol(gg_dta) - 1,
              " (number of classes)."
            )
          )
        }
      }
      gg_v <- data.frame(vimp = sort(gg_dta[, which_col],
        decreasing = TRUE
      ))
      gg_v$vars <-
        rownames(gg_dta)[order(gg_dta[, which_col],
          decreasing = TRUE
        )]
      # Name the column after the measure it holds, not after the "vimp"
      # column the pivot below writes it into -- pivot_longer() takes `set`
      # from the source column name.
      colnames(gg_v)[1] <- which_col
      gg_dta <- gg_v
    } else {
      # No specific class requested: attach variable names and pivot.
      gg_dta$vars <- rownames(gg_dta)
    }

    gg_dta <- gg_dta[seq_len(nvar), ]
    pivot_cols <-
      colnames(gg_dta)[-which(colnames(gg_dta) == "vars")]
    # Pivot from wide (one column per class) to long (one row per class-var pair).
    gg_dta <- tidyr::pivot_longer(
      gg_dta,
      tidyr::all_of(pivot_cols),
      names_to = "set",
      values_to = "vimp"
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

    gg_dta <- gg_dta[seq_len(nvar), ]
  }

  # Convert vars to an ordered factor (reversed so the most important variable
  # plots at the top of a horizontal bar chart after coord_flip).
  gg_dta$vars <-
    factor(gg_dta$vars, levels = rev(unique(gg_dta$vars)))

  # Flag variables with non-positive VIMP so the plot can colour them
  # differently to indicate they do not improve (or actively hurt) predictions.
  # Use the actual VIMP column name: "vimp" after a multi-outcome pivot,
  # "VIMP" (uppercase) for single-outcome fits.
  vimp_col <- intersect(c("vimp", "VIMP"), colnames(gg_dta))[1]
  gg_dta$positive <- TRUE
  if (!is.na(vimp_col)) {
    gg_dta$positive[which(gg_dta[[vimp_col]] <= 0)] <- FALSE
  }

  class(gg_dta) <- c("gg_vimp", class(gg_dta))
  gg_dta <- .set_provenance(gg_dta, object)
  invisible(gg_dta)
}

#' @export
gg_vimp.randomForest <- function(object, nvar, ...) {
  ## Check that the input object is of the correct type.
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
        training_info <- .rf_recover_model_frame(object) # nolint: object_usage_linter
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
  # Drop the node-impurity measure up front, for the same reason the
  # single-outcome branch below keeps only one measure: impurity is not a
  # permutation quantity and runs orders of magnitude larger, so it cannot
  # share a ranking with the permutation measures. Classification skips that
  # branch entirely (its importance matrix is wider than 2 columns), which
  # left MeanDecreaseGini as the sole survivor of the pivot. What remains for
  # a classification fit is the per-class columns plus MeanDecreaseAccuracy --
  # all permutation measures, mutually commensurable, and the direct analogue
  # of the all/<class> columns gg_vimp.rfsrc pivots together. Forests grown
  # with importance = FALSE store impurity alone, so keep it in that case.
  impurity <- c("IncNodePurity", "MeanDecreaseGini")
  measures <- setdiff(colnames(gg_dta), impurity)
  if (length(measures) > 0) {
    gg_dta <- gg_dta[, measures, drop = FALSE]
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
      # Ensure a canonical "vimp" column exists so the positive-flag logic and
      # plot.gg_vimp both work regardless of what randomForest named the column.
      colnames(gg_dta)[1] <- "vimp"
    }
    # With importance = TRUE, randomForest stores a node-impurity measure
    # (IncNodePurity / MeanDecreaseGini) alongside the permutation one, and the
    # multiclass pivot below would stack both into `vimp` and rank them
    # together. They are not commensurable -- node purity runs in the thousands
    # where %IncMSE runs in the tens -- so the impurity rows would sweep the top
    # of the ranking and the permutation values the caller asked for would be
    # truncated away entirely. Keep only the measure chosen above.
    gg_dta <- gg_dta[, c("vimp", "vars"), drop = FALSE]
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
      # test which.outcome specification. Both paths resolve the request to a
      # column name, which then names the measure in `set` below, the way the
      # unfiltered pivot in the else branch already does.
      if (!is.numeric(arg_set$which.outcome)) {
        if (arg_set$which.outcome %in% colnames(gg_dta)) {
          which_col <- arg_set$which.outcome
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
        # Resolve the column by name rather than by offset. randomForest's
        # importance matrix has no overall-first column: it runs one
        # permutation column per class, then MeanDecreaseAccuracy (the overall
        # permutation measure), then MeanDecreaseGini. rfsrc leads with an
        # "all" column instead, so gg_vimp.rfsrc's index arithmetic -- 0 for
        # column 1, k for column k+1 -- does not carry over here. Applied to a
        # randomForest fit it silently hands back the first class for 0 and
        # shifts every class by one.
        #   which.outcome = 0 gives overall (MeanDecreaseAccuracy)
        #   which.outcome = k gives class k
        # A fit grown with importance = FALSE keeps no MeanDecreaseAccuracy
        # column; the sole surviving measure is the overall one by default.
        if (arg_set$which.outcome < 0) {
          stop("which.outcome must be a non-negative integer or a class name.")
        }
        measures <- setdiff(colnames(gg_dta), "vars")
        classes <- setdiff(measures, "MeanDecreaseAccuracy")
        if (arg_set$which.outcome == 0) {
          which_col <- if ("MeanDecreaseAccuracy" %in% measures) {
            "MeanDecreaseAccuracy"
          } else {
            measures[1]
          }
        } else if (arg_set$which.outcome <= length(classes)) {
          which_col <- classes[arg_set$which.outcome]
        } else {
          stop(
            paste0(
              "which.outcome (", arg_set$which.outcome, ") is out of range. ",
              "Valid values are 0 (overall) to ", length(classes),
              " (number of classes)."
            )
          )
        }
      }
      gg_v <- data.frame(vimp = sort(gg_dta[, which_col],
        decreasing = TRUE
      ))
      gg_v$vars <-
        rownames(gg_dta)[order(gg_dta[, which_col],
          decreasing = TRUE
        )]
      # Name the column after the measure it holds, not after the "vimp"
      # column the pivot below writes it into -- pivot_longer() takes `set`
      # from the source column name.
      colnames(gg_v)[1] <- which_col
      gg_dta <- gg_v
    } else {
      gg_dta$vars <- rownames(gg_dta)
    }

    # Trim while the frame is still one row per variable: nvar counts
    # variables, so truncating the pivoted frame would instead lop whole
    # measures off the end of the ranking. gg_vimp.rfsrc trims here too.
    gg_dta <- gg_dta[seq_len(nvar), , drop = FALSE]

    pivot_cols <-
      colnames(gg_dta)[-which(colnames(gg_dta) == "vars")]
    gg_dta <- tidyr::pivot_longer(
      gg_dta,
      tidyr::all_of(pivot_cols),
      names_to = "set",
      values_to = "vimp"
    )
    gg_dta <- gg_dta[order(gg_dta$vimp, decreasing = TRUE), ]
    gg_dta$vars <- factor(gg_dta$vars)
  } else {
    gg_dta$vars[which(is.na(gg_dta$vars))] <-
      rownames(gg_dta)[which(is.na(gg_dta$vars))]
    gg_dta <- gg_dta[seq_len(nvar), ]
  }

  gg_dta$vars <-
    factor(gg_dta$vars, levels = rev(unique(gg_dta$vars)))
  gg_dta$positive <- TRUE
  gg_dta$positive[which(gg_dta$vimp <= 0)] <- FALSE

  class(gg_dta) <- c("gg_vimp", class(gg_dta))
  gg_dta <- .set_provenance(gg_dta, object)
  invisible(gg_dta)
}
