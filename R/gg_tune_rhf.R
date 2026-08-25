#' Tidy a Random Hazard Forest tuning path
#'
#' Extracts an already calculated tree-size tuning path from
#' [randomForestRHF::tune.treesize.rhf()] into a data frame for inspection and
#' plotting. The expensive step is upstream tuning. Calculate and retain that
#' result once, then supply it to `gg_tune_rhf()` when you need the saved search
#' path or its plot. `gg_tune_rhf()` only prepares that path; it never tunes a
#' forest.
#'
#' @param tune_fit An object inheriting from `tune.treesize.rhf`, typically
#'   returned by [randomForestRHF::tune.treesize.rhf()],
#'   [randomForestRHF::tune.rhf()], or
#'   [randomForestRHF::tune.iAUC.rhf()].
#' @param ... Additional arguments reserved for methods.
#'
#' @details
#' The returned path preserves the row order in `tune_fit$path`. Its columns
#' are `treesize` (evaluated forest size), `metric` (`"OOB risk"` or
#' `"OOB iAUC"`), `value` (the observed metric), `se` (the supplied bootstrap
#' iAUC standard error, or `NA_real_`), and `selected` (whether that size is
#' the upstream `best.size`). Upstream tuning minimizes OOB risk or maximizes
#' OOB iAUC.
#'
#' Provenance is stored in the `provenance` attribute: `best_size` is the
#' selected tree size; `best_err` is the optimized upstream criterion; `perf`
#' identifies the criterion; `method` is the upstream search method; `bounds`
#' gives its tree-size range; `n_evaluations` counts the evaluated sizes; and
#' `randomForestRHF_version` records the installed upstream package version.
#' The optional fitted forest is not copied into the tidy result.
#'
#' @return A `data.frame` with class
#'   `c("gg_tune_rhf", "data.frame")` and columns `treesize`, `metric`,
#'   `value`, `se`, and `selected`. The `provenance` attribute contains the
#'   upstream settings described in \code{\link{gg_tune_rhf}}.
#'
#' @references
#' Ishwaran H, Hsich EM, Kogalur UB, Lee DKK (2026). Random Hazard Forests.
#' arXiv:2608.21597. \doi{10.48550/arXiv.2608.21597}.
#'
#' Ishwaran H, Kogalur UB (2026). \emph{randomForestRHF: Random Hazard
#' Forests}. R package version 1.0.1.
#' \url{https://CRAN.R-project.org/package=randomForestRHF}.
#'
#' @seealso [plot()],
#'   [randomForestRHF::tune.treesize.rhf()]
#'
#' @examples
#' \donttest{
#' if (requireNamespace("randomForestRHF", quietly = TRUE)) {
#'   ## Calculate this expensive result once and retain it for reuse.
#'   simulated <- randomForestRHF::hazard.simulation(1, n = 100, nrecords = 3)
#'   tune_fit <- randomForestRHF::tune.iAUC.rhf(
#'     "Surv(id, start, stop, event) ~ .",
#'     simulated$dta,
#'     ntree = 12L,
#'     lower = 2L,
#'     upper = 5L,
#'     verbose = FALSE,
#'     forest = FALSE
#'   )
#'   tuning <- gg_tune_rhf(tune_fit)
#'   plot(tuning)
#' }
#' }
#'
#' @export
gg_tune_rhf <- function(tune_fit, ...) {
  if (!inherits(tune_fit, "tune.treesize.rhf")) {
    stop("'tune_fit' must inherit from 'tune.treesize.rhf'.",
         call. = FALSE)
  }
  UseMethod("gg_tune_rhf", tune_fit)
}

#' @rdname gg_tune_rhf
#' @export
gg_tune_rhf.tune.treesize.rhf <- function(tune_fit, ...) {
  .validate_rhf_tune_fit(tune_fit)
  path <- tune_fit$path
  is_iauc <- identical(tune_fit$perf, "iAUC")
  se <- if (is_iauc && "iAUC.se" %in% names(path)) {
    as.numeric(path$iAUC.se)
  } else {
    rep(NA_real_, nrow(path))
  }
  out <- data.frame(
    treesize = as.integer(path$treesize),
    metric = rep(if (is_iauc) "OOB iAUC" else "OOB risk", nrow(path)),
    value = as.numeric(if (is_iauc) path$iAUC else path$risk),
    se = se,
    selected = path$treesize == tune_fit$best.size,
    stringsAsFactors = FALSE
  )
  class(out) <- c("gg_tune_rhf", "data.frame")
  attr(out, "provenance") <- list(
    best_size = as.integer(tune_fit$best.size),
    best_err = as.numeric(tune_fit$best.err),
    perf = tune_fit$perf,
    method = tune_fit$method,
    bounds = tune_fit$bounds,
    n_evaluations = nrow(path),
    randomForestRHF_version = if (
      requireNamespace("randomForestRHF", quietly = TRUE)
    ) {
      as.character(utils::packageVersion("randomForestRHF"))
    } else {
      NA_character_
    }
  )
  invisible(out)
}

.validate_rhf_tune_fit <- function(fit) {
  .validate_rhf_tune_fields(fit)
  metric_name <- if (identical(fit$perf, "iAUC")) "iAUC" else "risk"
  metric <- .validate_rhf_tune_path(fit$path, metric_name)
  .validate_rhf_tune_optimum(fit, metric)
  invisible(TRUE)
}

.validate_rhf_tune_fields <- function(fit) {
  required <- c("best.size", "best.err", "bounds", "method", "perf", "path")
  missing <- setdiff(required, names(fit))
  if (length(missing)) {
    stop("Missing required field(s): ", paste(missing, collapse = ", "),
         ".", call. = FALSE)
  }

  .validate_rhf_tune_scalar(fit$best.size, "best.size", positive = TRUE)
  .validate_rhf_tune_scalar(fit$best.err, "best.err", finite = TRUE)
  if (!is.numeric(fit$bounds) || length(fit$bounds) != 2L ||
      any(!is.finite(fit$bounds)) || fit$bounds[1L] <= 0 ||
      fit$bounds[2L] < fit$bounds[1L]) {
    stop("'bounds' must contain two finite, ordered positive values.",
         call. = FALSE)
  }
  .validate_rhf_tune_scalar(fit$method, "method", character = TRUE)
  .validate_rhf_tune_scalar(fit$perf, "perf", character = TRUE)
  if (!fit$perf %in% c("risk", "iAUC")) {
    stop("'perf' must be either 'risk' or 'iAUC'.", call. = FALSE)
  }
  invisible(TRUE)
}

.validate_rhf_tune_path <- function(path, metric_name) {
  if (!is.data.frame(path) || !nrow(path)) {
    stop("'path' must be a non-empty data frame.", call. = FALSE)
  }
  if (!"treesize" %in% names(path)) {
    stop("'path$treesize' is required.", call. = FALSE)
  }
  if (!is.numeric(path$treesize) || any(!is.finite(path$treesize)) ||
      any(path$treesize <= 0) || anyDuplicated(path$treesize)) {
    stop("'path$treesize' must contain unique positive finite values.",
         call. = FALSE)
  }
  if (!metric_name %in% names(path)) {
    stop("'path$", metric_name, "' is required.", call. = FALSE)
  }
  metric <- path[[metric_name]]
  if (!is.numeric(metric) || any(!is.finite(metric))) {
    stop("'path$", metric_name, "' must contain finite numeric values.",
         call. = FALSE)
  }
  if ("iAUC.se" %in% names(path)) {
    .validate_rhf_tune_se(path$iAUC.se, nrow(path))
  }
  metric
}

.validate_rhf_tune_se <- function(se, n) {
  if (!is.numeric(se) || length(se) != n ||
      any(!is.na(se) & !is.finite(se)) || any(!is.na(se) & se < 0)) {
    stop("'path$iAUC.se' must contain non-negative finite numeric values,",
         " or NA for an unevaluated standard error.", call. = FALSE)
  }
  invisible(TRUE)
}

.validate_rhf_tune_optimum <- function(fit, metric) {
  path <- fit$path
  selected <- which(path$treesize == fit$best.size)
  if (length(selected) != 1L) {
    stop("'best.size' must identify exactly one evaluated path row.",
         call. = FALSE)
  }
  criterion <- if (identical(fit$perf, "iAUC")) 1 - metric else metric
  optimum <- which(abs(criterion - min(criterion)) <=
                     sqrt(.Machine$double.eps))
  if (length(optimum) != 1L || !identical(selected, optimum)) {
    stop("The tuning path must contain a unique optimum at 'best.size'.",
         call. = FALSE)
  }
  if (!isTRUE(all.equal(
    as.numeric(fit$best.err), criterion[selected],
    tolerance = sqrt(.Machine$double.eps)
  ))) {
    stop("'best.err' does not agree with the selected path metric.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.validate_rhf_tune_scalar <- function(value, field, finite = FALSE,
                                       positive = FALSE, character = FALSE) {
  if (character) {
    valid <- is.character(value) && length(value) == 1L &&
      nzchar(value) && !is.na(value)
  } else {
    valid <- is.numeric(value) && length(value) == 1L && !is.na(value)
    if (finite) {
      valid <- valid && is.finite(value)
    }
    if (positive) {
      valid <- valid && is.finite(value) && value > 0
    }
  }
  if (!valid) {
    stop("'", field, "' must be a valid scalar.", call. = FALSE)
  }
  invisible(TRUE)
}
