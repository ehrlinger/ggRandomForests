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
#' Random forest error trajectory data object
#'
#' Extract the cumulative out-of-bag (OOB) or in-bag training error rate from
#' \code{randomForestSRC} and \code{randomForest} fits as a function of the
#' number of grown trees.
#'
#' @details For \code{randomForestSRC} objects the function reshapes the
#' \code{\link[randomForestSRC]{rfsrc}$err.rate} matrix and annotates it with
#' the tree index required by \code{\link{plot.gg_error}}. When supplied a
#' \code{\link[randomForest]{randomForest}} object, the method inspects either
#' the \code{$mse} or \code{$err.rate} component and, when
#' \code{training = TRUE} is requested, reconstructs the original training set
#' via the model call to compute an in-bag error curve using per-tree
#' predictions. Training curves are only available when the forest was stored
#' (\code{keep.forest = TRUE}) and the original data can be recovered.
#'
#' @param object A fitted \code{\link[randomForestSRC]{rfsrc}} or
#'   \code{\link[randomForest]{randomForest}} object.
#' @param ... Optional arguments passed to the methods. Set
#'   \code{training = TRUE} to append the in-bag error trajectory when
#'   supported.
#'
#' @return A \code{gg_error} \code{data.frame} containing at least the
#'   cumulative OOB error columns and an \code{ntree} counter. When
#'   \code{training = TRUE} is honored an additional \code{train} column is
#'   included.
#'
#' @seealso \code{\link{plot.gg_error}}, \code{\link[randomForestSRC]{rfsrc}},
#'   \code{\link[randomForest]{randomForest}}
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
#' @importFrom stats as.formula model.frame model.response na.omit predict qnorm
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
      train_curve <- .rf_training_curve(object)
      if (!is.null(train_curve)) {
        gg_dta$train <- train_curve
      }
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
    if (training) {
      train_curve <- .rf_training_curve(object)
      if (!is.null(train_curve)) {
        gg_dta$train <- train_curve
      }
    }
  } else {
    stop("Performance values are not available for this forest.")
  }
  class(gg_dta) <- c("gg_error", class(gg_dta))
  invisible(gg_dta)
}

#' @export
gg_error.randomForest.formula <- gg_error.randomForest

.rf_training_curve <- function(object) {
  if (is.null(object$forest)) {
    warning(
      "Training error curve is unavailable because the forest was not saved. ",
      "Refit with keep.forest = TRUE to enable training=TRUE."
    )
    return(NULL)
  }
  training_info <- .rf_recover_model_frame(object)
  if (is.null(training_info)) {
    warning(
      "Unable to reconstruct the training data for this randomForest object;",
      " training=TRUE is ignored."
    )
    return(NULL)
  }

  training_frame <- training_info$model_frame
  response <- stats::model.response(training_frame)
  resp_name <- training_info$response_name
  predictors <- training_frame
  if (!is.null(resp_name) && resp_name %in% colnames(predictors)) {
    predictors[[resp_name]] <- NULL
  }
  special_cols <- grep("^\\(", colnames(predictors), value = TRUE)
  if (length(special_cols) > 0) {
    predictors[special_cols] <- NULL
  }
  predictors <- as.data.frame(predictors)
  if (ncol(predictors) == 0) {
    warning("No predictor columns available to compute training curve.")
    return(NULL)
  }

  pred_all <- predict(object,
    newdata = predictors,
    predict.all = TRUE
  )

  if (is.null(pred_all$individual)) {
    warning("Unable to extract per-tree predictions; training=TRUE ignored.")
    return(NULL)
  }

  individual <- pred_all$individual
  if (object$type == "regression") {
    return(.rf_training_curve_regression(individual, response))
  }
  if (object$type == "classification") {
    return(.rf_training_curve_classification(individual, response, object$classes))
  }
  warning("Training error curves are not supported for this forest type.")
  NULL
}

.rf_training_curve_regression <- function(individual, response) {
  cum_preds <- t(apply(individual, 1, cumsum))
  ntree <- ncol(individual)
  pred_by_tree <- sweep(cum_preds, 2, seq_len(ntree), "/")
  mse <- colMeans((pred_by_tree - response)^2)
  as.numeric(mse)
}

.rf_training_curve_classification <- function(individual, response, classes) {
  response <- factor(response, levels = classes)
  ntree <- ncol(individual)
  nobs <- nrow(individual)
  votes <- matrix(0, nrow = nobs, ncol = length(classes))
  colnames(votes) <- classes
  err <- numeric(ntree)

  for (tree in seq_len(ntree)) {
    preds <- factor(individual[, tree], levels = classes)
    pred_index <- as.integer(preds)
    valid <- !is.na(pred_index)
    idx <- cbind(seq_len(nobs)[valid], pred_index[valid])
    if (nrow(idx) > 0) {
      votes[idx] <- votes[idx] + 1
    }
    agg <- factor(classes[max.col(votes, ties.method = "first")],
      levels = classes
    )
    err[tree] <- mean(agg != response)
  }
  err
}

.rf_recover_model_frame <- function(object) {
  if (is.null(object$call$formula)) {
    return(NULL)
  }
  formula <- stats::as.formula(object$call$formula)
  data_env <- environment(formula)
  if (is.null(data_env)) {
    data_env <- parent.frame()
  }
  data_expr <- object$call$data
  if (is.null(data_expr)) {
    return(NULL)
  }
  mf_list <- c(
    list(quote(stats::model.frame)),
    list(formula = formula, data = data_expr)
  )
  optional_args <- c("subset", "weights", "na.action", "offset")
  for (arg in optional_args) {
    arg_value <- object$call[[arg]]
    if (!is.null(arg_value)) {
      mf_list[[arg]] <- arg_value
    }
  }
  mf_call <- as.call(mf_list)
  env_candidates <- c(list(data_env), rev(sys.frames()), list(.GlobalEnv))
  mf <- NULL
  for (env in env_candidates) {
    mf <- tryCatch(
      eval(mf_call, envir = env),
      error = function(e) NULL
    )
    if (!is.null(mf)) {
      break
    }
  }
  if (is.null(mf)) {
    return(NULL)
  }
  terms_obj <- attr(mf, "terms")
  resp_idx <- attr(terms_obj, "response")
  resp_name <- NULL
  if (!is.null(resp_idx) && resp_idx > 0) {
    resp_name <- colnames(mf)[resp_idx]
  }
  list(
    model_frame = mf,
    response_name = resp_name
  )
}
