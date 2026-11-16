#' Run the subsamples for a learning curve
#'
#' Calls [randomForestSRC::subsample()] once per fraction, with
#' `performance = TRUE`, and optionally once more as a double bootstrap for
#' the whole cohort, and returns the list that [gg_learning()] tidies. It
#' records the `stratify` setting on each result, which `subsample()` itself
#' does not keep, so `gg_learning()` can report the number of events in each
#' refit as exact rather than assumed.
#'
#' This is the expensive step: it grows `replicates` forests per fraction,
#' plus as many for the bootstrap, each the size of `object`. Run it once,
#' save the result and pass it to `gg_learning()` as often as needed.
#'
#' @param object A `randomForestSRC` forest (class `rfsrc`), grown with the
#'   settings the refits should copy.
#' @param fractions Fractions of the cohort to refit on, each greater than 0
#'   and less than 1.
#' @param replicates Positive whole number; refits per fraction, passed to
#'   `subsample()` as `B`.
#' @param bootstrap Logical; also run the double bootstrap for the whole
#'   cohort (fraction 1). Defaults to `TRUE`.
#' @param stratify Logical; passed to `subsample()`, which samples within each
#'   class (or survival status) when `TRUE`, its default.
#' @param ... Further arguments to `subsample()`, such as `importance`.
#'
#' @details
#' `subsample()` draws its subsamples with R's random number generator, so set
#' the seed before calling this function for a reproducible curve.
#'
#' @return A list of `subsample()` results, one per fraction in increasing
#'   order and the bootstrap last, each carrying a `stratify` attribute.
#'
#' @seealso [gg_learning()], [randomForestSRC::subsample()].
#'
#' @examples
#' \donttest{
#' fit <- randomForestSRC::rfsrc(mpg ~ ., data = mtcars, ntree = 50,
#'                               importance = "permute", seed = -1)
#' set.seed(1)
#' subsamples <- learning_subsample(fit, fractions = c(0.25, 0.5),
#'                                  replicates = 5)
#' curve <- gg_learning(subsamples, top_k = 3, min_events = 10)
#' summary(curve)
#' }
#'
#' @export
learning_subsample <- function(object, fractions = c(0.125, 0.25, 0.5),
                               replicates = 20L, bootstrap = TRUE,
                               stratify = TRUE, ...) {
  .validate_learning_subsample(object, fractions, replicates, bootstrap,
                               stratify)
  # These are set here; passed again through `...` they would reach
  # subsample() twice, or quietly change what the curve measures.
  taken <- intersect(names(list(...)), c("obj", "B", "subratio", "bootstrap",
                                         "performance", "stratify", "verbose"))
  if (length(taken)) {
    stop("learning_subsample() sets ", paste0("'", taken, "'", collapse = ", "),
         " itself; use 'replicates', 'fractions', 'bootstrap' or 'stratify'.",
         call. = FALSE)
  }
  refit <- function(...) {
    s <- randomForestSRC::subsample(object, B = replicates, performance = TRUE,
                                    stratify = stratify, verbose = FALSE, ...)
    attr(s, "stratify") <- stratify
    s
  }
  out <- lapply(sort(unique(fractions)), function(f) refit(subratio = f, ...))
  if (bootstrap) out <- c(out, list(refit(bootstrap = TRUE, ...)))
  out
}

.validate_learning_subsample <- function(object, fractions, replicates,
                                         bootstrap, stratify) {
  if (!inherits(object, "rfsrc")) {
    stop("'object' must be a randomForestSRC forest.", call. = FALSE)
  }
  if (!is.numeric(fractions) || !length(fractions) ||
      any(!is.finite(fractions)) || any(fractions <= 0 | fractions >= 1)) {
    stop("'fractions' must be numbers greater than 0 and less than 1; ",
         "the whole cohort comes from 'bootstrap'.", call. = FALSE)
  }
  if (!.is_finite_scalar(replicates) || replicates < 1 ||
      replicates != round(replicates)) {
    stop("'replicates' must be one positive whole number.", call. = FALSE)
  }
  .validate_learning_flag(bootstrap, "bootstrap")
  .validate_learning_flag(stratify, "stratify")
  invisible(TRUE)
}

.validate_learning_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("'", name, "' must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}
