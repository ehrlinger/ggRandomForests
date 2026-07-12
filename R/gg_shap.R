#' SHAP (Shapley additive explanations) data object
#'
#' \code{gg_shap} computes SHAP values for a
#' \code{\link[randomForestSRC]{rfsrc}} or
#' \code{\link[randomForest]{randomForest}} regression or classification forest
#' by wrapping \code{\link[kernelshap]{kernelshap}}, and reshapes them into a
#' tidy data set with one row per (observation, variable).
#'
#' @param object A \code{\link[randomForestSRC]{rfsrc}} or
#'   \code{\link[randomForest]{randomForest}} object (regression or
#'   classification).
#' @param newdata Optional \code{data.frame} of predictor values to explain
#'   (same columns as the model's training predictors). When missing, the
#'   model's own training predictors are used.
#' @param bg_n Size of the background/reference sample drawn from the
#'   training predictors and passed to \code{\link[kernelshap]{kernelshap}}
#'   as \code{bg_X}. Larger values are more accurate but slower.
#' @param which.class For classification forests, the class (integer column
#'   index into the predicted-probability matrix) whose predicted probability
#'   is explained. Defaults to 1.
#' @param ... Passed through to \code{\link[kernelshap]{kernelshap}} (e.g.
#'   \code{seed}, \code{exact}, \code{max_iter}).
#'
#' @return A \code{gg_shap} object: a \code{data.frame} with columns
#'   \code{id} (observation index), \code{vars} (variable name, an ordered
#'   factor ranked by mean absolute SHAP), \code{shap} (the signed SHAP
#'   contribution), \code{value} (numeric feature value, \code{NA} for
#'   categorical features), and \code{value_label} (feature value as
#'   character). The background-sample mean prediction is stored in the
#'   \code{"baseline"} attribute.
#'
#' @seealso \code{\link[kernelshap]{kernelshap}}
#'
#' @examples
#' \donttest{
#' if (requireNamespace("kernelshap", quietly = TRUE)) {
#'   rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
#'                                ntree = 50)
#'   gg_dta <- gg_shap(rf, bg_n = 20)
#' }
#' }
#'
#' @aliases gg_shap gg_shap.rfsrc gg_shap.randomForest
#' @export
gg_shap <- function(object, newdata, bg_n = 50, which.class = 1, ...) {
  UseMethod("gg_shap", object)
}

# Internal: validate and coerce bg_n to a single positive integer. Not exported.
.gg_shap_validate_bg_n <- function(bg_n) {
  if (!is.numeric(bg_n) || length(bg_n) != 1L || is.na(bg_n) || bg_n < 1) {
    stop("gg_shap: bg_n must be a single positive integer.", call. = FALSE)
  }
  as.integer(bg_n)
}

#' @export
gg_shap.default <- function(object, newdata, bg_n = 50, which.class = 1, ...) {
  stop("gg_shap: expected an 'rfsrc' or 'randomForest' object; got an object ",
       "of class ", paste(class(object), collapse = "/"), ".", call. = FALSE)
}

#' @export
gg_shap.rfsrc <- function(object, newdata, bg_n = 50, which.class = 1, ...) {
  if (!requireNamespace("kernelshap", quietly = TRUE)) {
    stop("gg_shap requires the 'kernelshap' package. Install it with ",
         "install.packages('kernelshap').", call. = FALSE)
  }

  if (!object$family %in% c("regr", "class")) {
    stop("gg_shap: only regression and classification forests are supported ",
         "in this version; got family '", object$family, "'. Survival ",
         "support is not yet implemented.", call. = FALSE)
  }

  bg_n <- .gg_shap_validate_bg_n(bg_n)

  x_train <- object$xvar
  x_explain <- if (missing(newdata) || is.null(newdata)) x_train else newdata
  bg_x <- x_train[sample.int(nrow(x_train), min(bg_n, nrow(x_train))), ,
                  drop = FALSE]

  is_class <- object$family == "class"
  if (is_class) {
    n_class <- ncol(object$predicted)
    if (which.class < 1 || which.class > n_class) {
      stop("gg_shap: which.class (", which.class, ") is out of range. Valid ",
           "values are 1 to ", n_class, ".", call. = FALSE)
    }
  }
  pred_fun <- function(object, newdata) {
    pr <- predict(object, newdata)$predicted
    if (is_class) as.numeric(pr[, which.class]) else as.numeric(pr)
  }

  res <- kernelshap::kernelshap(object, X = x_explain, bg_X = bg_x,
                                pred_fun = pred_fun, verbose = FALSE, ...)

  invisible(.gg_shap_reshape(res$S, x_explain, res$baseline, object,
                             bg_n = bg_n, which.class = which.class))
}

#' @export
gg_shap.randomForest <- function(object, newdata, bg_n = 50,
                                 which.class = 1, ...) {
  if (!requireNamespace("kernelshap", quietly = TRUE)) {
    stop("gg_shap requires the 'kernelshap' package. Install it with ",
         "install.packages('kernelshap').", call. = FALSE)
  }

  if (!object$type %in% c("regression", "classification")) {
    stop("gg_shap: only regression and classification forests are supported; ",
         "got type '", object$type, "'.", call. = FALSE)
  }

  bg_n <- .gg_shap_validate_bg_n(bg_n)

  info <- .rf_recover_model_frame(object)
  if (is.null(info)) {
    stop("gg_shap: could not recover training predictors from this ",
         "randomForest object.", call. = FALSE)
  }
  x_train <- info$model_frame[, setdiff(colnames(info$model_frame),
                                        info$response_name), drop = FALSE]
  x_explain <- if (missing(newdata) || is.null(newdata)) x_train else newdata
  bg_x <- x_train[sample.int(nrow(x_train), min(bg_n, nrow(x_train))), ,
                  drop = FALSE]

  is_class <- object$type == "classification"
  if (is_class) {
    n_class <- length(object$classes)
    if (which.class < 1 || which.class > n_class) {
      stop("gg_shap: which.class (", which.class, ") is out of range. Valid ",
           "values are 1 to ", n_class, ".", call. = FALSE)
    }
  }
  pred_fun <- function(object, newdata) {
    if (is_class) {
      as.numeric(predict(object, newdata, type = "prob")[, which.class])
    } else {
      as.numeric(predict(object, newdata))
    }
  }

  res <- kernelshap::kernelshap(object, X = x_explain, bg_X = bg_x,
                                pred_fun = pred_fun, verbose = FALSE, ...)

  invisible(.gg_shap_reshape(res$S, x_explain, res$baseline, object,
                             bg_n = bg_n, which.class = which.class))
}

# Internal: turn a SHAP matrix (obs x vars) + the explained predictors into a
# long tidy gg_shap data.frame. Not exported.
.gg_shap_reshape <- function(sv, x_explain, baseline, object,
                             bg_n, which.class) {
  sv <- as.data.frame(sv)
  n <- nrow(sv)
  vars <- colnames(sv)

  sv$id <- seq_len(n)
  shap_long <- tidyr::pivot_longer(sv, cols = tidyr::all_of(vars),
                                   names_to = "vars", values_to = "shap")

  # numeric feature value (NA for non-numeric columns), for beeswarm coloring
  num_mat <- vapply(x_explain[vars], function(col) {
    if (is.numeric(col)) as.numeric(col) else rep(NA_real_, length(col))
  }, numeric(n))
  val_num <- data.frame(
    id    = rep(seq_len(n), times = length(vars)),
    vars  = rep(vars, each = n),
    value = as.vector(num_mat),
    stringsAsFactors = FALSE
  )
  val_lab <- data.frame(
    id          = rep(seq_len(n), times = length(vars)),
    vars        = rep(vars, each = n),
    value_label = as.vector(vapply(x_explain[vars], as.character,
                                   character(n))),
    stringsAsFactors = FALSE
  )

  gg_dta <- merge(merge(shap_long, val_num, by = c("id", "vars")),
                  val_lab, by = c("id", "vars"))

  # rank variables by mean absolute SHAP; reverse levels so the most important
  # plots at the top after coord_flip (matching plot.gg_vimp).
  rank <- stats::aggregate(abs(gg_dta$shap),
                           by = list(vars = gg_dta$vars), FUN = mean)
  ord <- rank$vars[order(rank$x, decreasing = TRUE)]
  gg_dta$vars <- factor(gg_dta$vars, levels = rev(as.character(ord)))

  attr(gg_dta, "baseline") <- baseline
  attr(gg_dta, "bg_n") <- bg_n
  attr(gg_dta, "which.class") <- which.class
  class(gg_dta) <- c("gg_shap", class(gg_dta))
  .set_provenance(gg_dta, object)
}
