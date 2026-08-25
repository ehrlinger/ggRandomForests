##=============================================================================
#' Tidy time-localized variable priority from a Random Hazard Forest
#'
#' Extracts the variable-priority matrix from
#' [randomForestRHF::importance.rhf()] into a tidy data frame, one row per
#' variable and time window. The score measures how much the fitted integrated
#' hazard changes when rules involving a variable are released.
#'
#' @param object A fitted `rhf` object from pkg{randomForestRHF}.
#' @param importance_fit Optional precomputed
#'   [randomForestRHF::importance.rhf()] result for the same `object`. Supply
#'   this object when you have already calculated variable priority. `NULL`
#'   (default) calculates it from `object`.
#' @param cache Optional precomputed [randomForestRHF::varpro.cache.rhf()]
#'   result used when `importance_fit` is `NULL`.
#' @param time.index Optional time-grid indices passed to
#'   [randomForestRHF::importance.rhf()] when `importance_fit` is `NULL`.
#' @param ... Additional arguments passed to
#'   [randomForestRHF::importance.rhf()] when `importance_fit` is `NULL`.
#'
#' @details
#' Variable priority is time-localized. A large value means that releasing
#' rules involving that variable changed the log integrated hazard more in
#' that window. It is a ranking score, not a z-score, and this function does
#' not apply a significance cutoff.
#'
#' Calculating the upstream result can be expensive. For an analysis you will
#' revisit, calculate `importance_fit` once and supply it here. The extractor
#' accepts `cache`, `time.index`, and additional calculation arguments only
#' when `importance_fit` is `NULL`.
#'
#' Variables are ordered for plotting by their 90th percentile (`q90`)
#' priority across windows. This changes the factor levels, but does not change
#' the upstream row order or priority values.
#'
#' @return A `data.frame` of class
#'   `c("gg_rhf_importance", "data.frame")` with columns:
#'   \describe{
#'     \item{variable}{Variable name, ordered by `q90` priority for plotting.}
#'     \item{time_window}{Upstream time-window label.}
#'     \item{time}{Evaluation time at the end of the window.}
#'     \item{time_index}{Index of `time` on the RHF time grid.}
#'     \item{start, stop, midpoint}{Window boundaries and midpoint.}
#'     \item{n_risk}{Number of observations at risk.}
#'     \item{n_rules}{Number of rules contributing to the window.}
#'     \item{priority}{RHF variable-priority score.}
#'   }
#'   A `provenance` attribute records the source forest, upstream settings,
#'   whether `importance_fit` was supplied, and the installed
#'   pkg{randomForestRHF} version.
#'
#' @references
#' Ishwaran H, Hsich EM, Kogalur UB, Lee DKK (2026). Random Hazard Forests.
#' arXiv:2608.21597. \doi{10.48550/arXiv.2608.21597}.
#'
#' Ishwaran H, Kogalur UB (2026). \emph{randomForestRHF: Random Hazard
#' Forests}. R package version 1.0.1.
#' \url{https://CRAN.R-project.org/package=randomForestRHF}.
#'
#' @seealso [plot.gg_rhf_importance()], [randomForestRHF::importance.rhf()]
#'
#' @examples
#' \donttest{
#' if (requireNamespace("randomForestRHF", quietly = TRUE)) {
#'   data(pbc, package = "randomForestSRC")
#'   d <- randomForestRHF::convert.counting(
#'     survival::Surv(days, status) ~ ., na.omit(pbc))
#'   o <- randomForestRHF::rhf(
#'     "Surv(id, start, stop, event) ~ .", d, ntree = 30)
#'
#'   priority_fit <- randomForestRHF::importance.rhf(o)
#'   priority <- gg_rhf_importance(o, importance_fit = priority_fit)
#'   plot(priority)
#' }
#' }
#'
#' @export
gg_rhf_importance <- function(object, ...) {
  UseMethod("gg_rhf_importance", object)
}

#' @rdname gg_rhf_importance
#' @export
gg_rhf_importance.rhf <- function(object, importance_fit = NULL, cache = NULL,
                                  time.index = NULL, ...) {
  if (!inherits(object, "rhf")) {
    stop("gg_rhf_importance() only works on 'rhf' objects from ",
         "randomForestRHF.", call. = FALSE)
  }
  dots <- list(...)
  precomputed <- !is.null(importance_fit)
  if (precomputed && (!is.null(cache) || !is.null(time.index) ||
                      length(dots))) {
    stop("Do not supply calculation arguments with 'importance_fit'.",
         call. = FALSE)
  }
  if (!precomputed) {
    if (!requireNamespace("randomForestRHF", quietly = TRUE)) {
      stop("Install the 'randomForestRHF' package to use ",
           "gg_rhf_importance(): install.packages('randomForestRHF')",
           call. = FALSE)
    }
    importance_fit <- do.call(
      randomForestRHF::importance.rhf,
      c(list(o = object, cache = cache, time.index = time.index), dots)
    )
  }

  .validate_rhf_importance_fit(object, importance_fit)
  d <- importance_fit$importance.long
  out <- data.frame(
    variable = as.character(d$variable),
    time_window = as.character(d$window),
    time = as.numeric(d$time),
    time_index = as.integer(d$time.index),
    start = as.numeric(d$start),
    stop = as.numeric(d$stop),
    midpoint = as.numeric(d$midpoint),
    n_risk = as.integer(d$n.risk),
    n_rules = as.integer(d$n.rules),
    priority = as.numeric(d$importance),
    stringsAsFactors = FALSE
  )
  rank <- .rhf_priority_summary(out)
  out$variable <- factor(out$variable, levels = rev(rank$variable))
  class(out) <- c("gg_rhf_importance", class(out))
  out <- .set_provenance(out, object)

  prov <- attr(out, "provenance") %||% list()
  prov$precomputed <- precomputed
  prov$y_source <- importance_fit$y.source %||% NA_character_
  prov$trim <- importance_fit$trim %||% NA_real_
  prov$n_windows <- nrow(importance_fit$window.info)
  prov$rank_by <- "q90"
  prov$randomForestRHF_version <- if (
    requireNamespace("randomForestRHF", quietly = TRUE)
  ) {
    as.character(utils::packageVersion("randomForestRHF"))
  } else {
    NA_character_
  }
  attr(out, "provenance") <- prov
  invisible(out)
}

.rhf_priority_summary <- function(x) {
  by_var <- split(x$priority, as.character(x$variable))
  stat <- function(v, fun) {
    v <- v[is.finite(v)]
    if (length(v)) fun(v) else NA_real_
  }
  out <- data.frame(
    variable = names(by_var),
    q90 = vapply(by_var, stat, numeric(1), fun = function(v) {
      unname(stats::quantile(v, 0.9, names = FALSE))
    }),
    median = vapply(by_var, stat, numeric(1), fun = stats::median),
    mean = vapply(by_var, stat, numeric(1), fun = mean),
    max = vapply(by_var, stat, numeric(1), fun = max),
    n_windows = vapply(names(by_var), function(v) {
      length(unique(x$time_index[as.character(x$variable) == v]))
    }, integer(1)),
    n_finite = vapply(by_var, function(v) sum(is.finite(v)), integer(1))
  )
  out[order(-out$q90, -out$median, -out$max, out$variable,
            na.last = TRUE), ]
}

.validate_rhf_importance_fit <- function(object, fit) {
  if (!inherits(fit, "importance.rhf")) {
    stop("'importance_fit' must inherit from 'importance.rhf'.",
         call. = FALSE)
  }
  if (!identical(as.character(fit$xvar.names),
                 as.character(object$xvar.names))) {
    stop("'importance_fit$xvar.names' do not match the RHF object.",
         call. = FALSE)
  }

  mat <- .validate_rhf_priority_matrix(fit)
  win <- .validate_rhf_priority_windows(object, fit, mat)
  .validate_rhf_priority_long(fit, mat, win)
  invisible(TRUE)
}

.validate_rhf_priority_matrix <- function(fit) {
  mat <- fit$importance.matrix
  if (!is.matrix(mat) || !is.numeric(mat) || !length(mat) ||
      !identical(rownames(mat), fit$xvar.names)) {
    stop("'importance_fit$importance.matrix' is malformed.", call. = FALSE)
  }
  mat
}

.validate_rhf_priority_windows <- function(object, fit, mat) {
  win <- fit$window.info
  win_names <- c(
    "index", "time", "start", "stop", "midpoint", "n.risk", "n.rules",
    "label"
  )
  if (!is.data.frame(win) || !all(win_names %in% names(win)) ||
      nrow(win) != ncol(mat)) {
    stop("'importance_fit$window.info' does not align with the matrix.",
         call. = FALSE)
  }
  if (any(!is.finite(win$index)) || any(win$index < 1L) ||
      any(win$index > length(object$time.interest)) ||
      !isTRUE(all.equal(
        as.numeric(win$time),
        as.numeric(object$time.interest[win$index]),
        tolerance = sqrt(.Machine$double.eps)
      ))) {
    stop("'importance_fit$window.info' does not match ",
         "object$time.interest.", call. = FALSE)
  }
  win
}

.validate_rhf_priority_long <- function(fit, mat, win) {
  long <- fit$importance.long
  long_names <- c(
    "variable", "time", "time.index", "window", "start", "stop",
    "midpoint", "n.risk", "n.rules", "importance"
  )
  if (!is.data.frame(long) || !all(long_names %in% names(long)) ||
      nrow(long) != length(mat)) {
    stop("'importance_fit$importance.long' is malformed.", call. = FALSE)
  }
  row_index <- match(long$variable, rownames(mat))
  col_index <- match(long$time.index, win$index)
  key <- paste(long$variable, long$time.index, sep = "\r")
  if (anyNA(row_index) || anyNA(col_index) ||
      length(unique(key)) != length(mat)) {
    stop("'importance_fit$importance.long' has unknown variables or windows.",
         call. = FALSE)
  }

  expected <- mat[cbind(row_index, col_index)]
  if (!isTRUE(all.equal(
    as.numeric(long$importance),
    as.numeric(expected),
    tolerance = sqrt(.Machine$double.eps)
  ))) {
    stop("'importance_fit$importance.long' does not match ",
         "importance.matrix.", call. = FALSE)
  }

  win_row <- match(long$time.index, win$index)
  metadata_pairs <- list(
    c("time", "time"), c("window", "label"), c("start", "start"),
    c("stop", "stop"), c("midpoint", "midpoint"),
    c("n.risk", "n.risk"), c("n.rules", "n.rules")
  )
  metadata_ok <- vapply(metadata_pairs, function(pair) {
    isTRUE(all.equal(
      long[[pair[1L]]],
      win[[pair[2L]]][win_row],
      tolerance = sqrt(.Machine$double.eps),
      check.attributes = FALSE
    ))
  }, logical(1))
  if (!all(metadata_ok)) {
    stop("'importance_fit$importance.long' does not match window.info.",
         call. = FALSE)
  }
  if (any(long$importance[is.finite(long$importance)] < 0)) {
    stop("'importance_fit$importance.long' contains negative priority values.",
         call. = FALSE)
  }
  invisible(TRUE)
}
