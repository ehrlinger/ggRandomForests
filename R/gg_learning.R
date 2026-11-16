#' Tidy a random forest learning curve
#'
#' Extracts learning curves from a set of already calculated
#' [randomForestSRC::subsample()] results: OOB error, and the stability of the
#' variable importance (VIMP) ranking, against the number of events (or
#' observations, for regression) in each subsample. The two curves answer
#' different questions. The error curve says whether the cohort is large
#' enough to predict; the stability curve says whether it is large enough to
#' trust the VIMP ranking and the partial plots drawn from it. A cohort can
#' pass the first and fail the second.
#'
#' The expensive step is the refitting inside `subsample()`. Run it once per
#' fraction, keep the results, and supply the list to `gg_learning()`.
#' `gg_learning()` only tidies those results; it never fits a forest.
#'
#' @param subsamples A list of objects returned by
#'   [randomForestSRC::subsample()], one per subsampling fraction, all from
#'   the same forest and all run with `performance = TRUE`. A double-bootstrap
#'   result (`subsample(bootstrap = TRUE, performance = TRUE)`) may be
#'   included and is placed at fraction 1. A plain subsample with
#'   `subratio = 1` is refused: it refits the same cases every time, so its
#'   agreement measures forest randomness, not resampling stability. A single
#'   result is also accepted.
#' @param top_k Positive integer; the number of top-ranked variables compared
#'   between replicates. Defaults to 5, or the number of predictors if fewer.
#' @param min_events Non-negative number; fractions with fewer events than
#'   this are flagged in `low_events`. Defaults to 50.
#' @param stratified Logical; how the subsamples were drawn, as passed to
#'   `subsample(stratify = )`. Defaults to `TRUE`, `subsample()`'s own
#'   default. The results do not record it, so it cannot be checked here.
#' @param ... Not used.
#'
#' @details
#' **Error.** Each replicate's OOB error is the `"err"` row that
#' `subsample(performance = TRUE)` appends to its VIMP table, on the forest's
#' own scale: 1 - C-index for survival, mean squared error for regression,
#' and the overall error rate (the `all` column) for classification.
#'
#' **Stability.** At each fraction, every pair of replicates is compared on
#' its top `top_k` variables. `jaccard` is the mean pairwise Jaccard index of
#' the two top-k sets. `kendall` is the mean pairwise Kendall rank
#' correlation, computed over the union of the two top-k sets with ranks taken
#' from each replicate's full VIMP vector, so a variable that drops out of one
#' top-k set counts against agreement. Both are restricted to the top k
#' because the long tail of noise variables would otherwise dominate.
#'
#' **Events.** The x axis counts events for survival (deaths, or whatever the
#' status codes as 1), the size of the smallest class for classification, and
#' observations for regression. The counts are computed, not observed:
#' `subsample()` keeps no record of which cases it drew. With its default
#' `stratify = TRUE` it takes the whole part of each class (or status) count
#' times the fraction, so the counts here are exact (`counts = "exact"`).
#' With `stratify = FALSE` (say so with `stratified = FALSE`) the counts vary
#' from refit to refit: the ones reported are what a stratified draw would
#' give, `counts` is `"expected"`, and `low_events` is `NA`, because whether a
#' refit fell short is unknown. The double bootstrap draws as many cases as
#' the cohort holds, with replacement; it is reported at the cohort's counts,
#' `counts = "expected"`.
#'
#' **Caveats.** Subsamples at large fractions share most of their cases:
#' any two at 0.75 share about three quarters of each other's. Between-replicate
#' agreement there overstates the stability a fresh cohort of that size would
#' show, so fractions above 0.5 and below 1 are flagged `optimistic`. Use the
#' double bootstrap for the full-cohort point. OOB error from a forest grown on
#' fewer than about 50 events is pessimistic and noisy, so those fractions are
#' flagged `low_events`. The curve covers cohort sizes up to the one observed;
#' it does not extrapolate.
#'
#' Provenance is stored in the `provenance` attribute: `family`, `n` and
#' `events` of the full cohort, `ntree`, `top_k`, `min_events`, `fractions`,
#' replicates per fraction (`B`), the error `metric`, and the installed
#' `randomForestSRC` version.
#'
#' @return A `data.frame` with class `c("gg_learning", "data.frame")`, one row
#'   per replicate, with columns `fraction`, `n`, `events`, `replicate`,
#'   `error`, `counts`, `low_events` and `optimistic`. The `stability`
#'   attribute is a `data.frame` with one row per fraction and columns
#'   `fraction`, `n`, `events`, `B`, `jaccard`, `kendall`, `counts`,
#'   `low_events` and `optimistic`.
#'
#' @seealso [plot.gg_learning()], [randomForestSRC::subsample()],
#'   [gg_error()] for convergence in the number of trees.
#'
#' @references
#' Ishwaran H, Lu M (2019). Standard errors and confidence intervals for
#' variable importance in random forest regression, classification, and
#' survival. \emph{Statistics in Medicine}, 38(4), 558--582.
#' \doi{10.1002/sim.7803}.
#'
#' @examples
#' \donttest{
#' ## Calculate the subsamples once and keep them for reuse.
#' fit <- randomForestSRC::rfsrc(mpg ~ ., data = mtcars, ntree = 50,
#'                               importance = "permute", seed = -1)
#' subsamples <- lapply(c(0.25, 0.5), function(f) {
#'   randomForestSRC::subsample(fit, B = 5, subratio = f,
#'                              performance = TRUE, verbose = FALSE)
#' })
#' subsamples$full <- randomForestSRC::subsample(fit, B = 5, bootstrap = TRUE,
#'                                               performance = TRUE,
#'                                               verbose = FALSE)
#' curve <- gg_learning(subsamples, top_k = 3, min_events = 10)
#' summary(curve)
#' plot(curve)
#' plot(curve, panel = "error")
#' }
#'
#' @export
gg_learning <- function(subsamples, top_k = 5L, min_events = 50,
                        stratified = TRUE, ...) {
  if (inherits(subsamples, c("subsample", "bootsample"))) {
    subsamples <- list(subsamples)
  }
  .validate_learning_args(subsamples, top_k, min_events)
  if (!is.logical(stratified) || length(stratified) != 1L ||
      is.na(stratified)) {
    stop("'stratified' must be TRUE or FALSE.", call. = FALSE)
  }
  info <- .learning_outcome(subsamples[[1L]]$rf)

  pieces <- lapply(subsamples, .learning_piece, info = info)
  fractions <- vapply(pieces, function(p) p$fraction, numeric(1))
  if (anyDuplicated(fractions)) {
    stop("Each subsampling fraction may appear only once.", call. = FALSE)
  }
  pieces <- pieces[order(fractions)]
  k <- min(as.integer(top_k), length(pieces[[1L]]$vimp[[1L]]))

  rows <- lapply(pieces, function(p) {
    data.frame(fraction = p$fraction, n = p$n, events = p$events,
               replicate = seq_along(p$error), error = p$error)
  })
  out <- .learning_flag(do.call(rbind, rows), min_events, stratified)

  stab <- lapply(pieces, function(p) {
    s <- .learning_stability(p$vimp, k)
    data.frame(fraction = p$fraction, n = p$n, events = p$events,
               B = length(p$vimp), jaccard = s[["jaccard"]],
               kendall = s[["kendall"]])
  })
  stab <- .learning_flag(do.call(rbind, stab), min_events, stratified)
  rownames(out) <- rownames(stab) <- NULL

  class(out) <- c("gg_learning", "data.frame")
  attr(out, "stability") <- stab
  attr(out, "provenance") <- list(
    source = "subsample",
    family = info$family,
    n = info$n,
    events = info$events,
    ntree = info$ntree,
    top_k = k,
    min_events = min_events,
    stratified = stratified,
    fractions = stab$fraction,
    B = stab$B,
    metric = info$metric,
    randomForestSRC_version =
      as.character(utils::packageVersion("randomForestSRC"))
  )
  out
}

.validate_learning_args <- function(subsamples, top_k, min_events) {
  .validate_learning_list(subsamples)
  if (!.is_finite_scalar(top_k) || top_k < 1 || top_k != round(top_k)) {
    stop("'top_k' must be one positive whole number.", call. = FALSE)
  }
  if (!.is_finite_scalar(min_events) || min_events < 0) {
    stop("'min_events' must be one finite non-negative number.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.is_finite_scalar <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x)
}

.validate_learning_list <- function(subsamples) {
  if (inherits(subsamples, "randomForest")) {
    stop("gg_learning() needs randomForestSRC::subsample() results; ",
         "randomForest fits have no subsample() method.", call. = FALSE)
  }
  is_sub <- vapply(subsamples, inherits, logical(1),
                   what = c("subsample", "bootsample"))
  if (!is.list(subsamples) || !length(subsamples) || !all(is_sub)) {
    stop("'subsamples' must be a list of randomForestSRC::subsample() ",
         "results.", call. = FALSE)
  }
  # subsample() keeps the parent forest in $rf, so results from one forest
  # carry identical data and settings. Shape alone (family, n, names) would
  # pass two different cohorts of the same size.
  # The whole forest is compared: subsample() refits with every setting the
  # parent carries (nsplit, weights, sampling and so on), and a list of named
  # fields would miss whichever was left out. Environments are ignored, so a
  # list saved and read back still matches itself.
  first <- subsamples[[1L]]$rf
  same <- vapply(subsamples, function(s) {
    identical(s$rf, first, ignore.environment = TRUE)
  }, logical(1))
  if (!all(same)) {
    stop("All 'subsamples' must come from the same forest.", call. = FALSE)
  }
  invisible(TRUE)
}

.learning_outcome <- function(fit) {
  family <- fit$family
  if (!family %in% c("regr", "class", "surv")) {
    stop("gg_learning() supports regression, classification and ",
         "right-censored survival forests, not family '", family, "'.",
         call. = FALSE)
  }
  y <- fit$yvar
  # The strata subsample() samples within when stratify = TRUE: the classes,
  # or the survival status. Regression is not stratified.
  strata <- switch(family,
    regr = NULL,
    class = table(y),
    surv = table(y[[2L]])
  )
  events <- switch(family,
    regr = fit$n,
    class = min(strata),
    surv = sum(y[[2L]] == 1)
  )
  metric <- switch(family,
    regr = "OOB mean squared error",
    class = "OOB error rate",
    surv = "OOB error (1 - C-index)"
  )
  list(family = family, n = fit$n, events = events, ntree = fit$ntree,
       metric = metric, strata = strata)
}

.learning_piece <- function(s, info) {
  boot <- inherits(s, "bootsample")
  reps <- if (boot) s$vmpB else s$vmpS
  fraction <- if (boot) 1 else as.numeric(s$subratio)
  if (!boot && fraction >= 1) {
    stop("A subsample() with subratio = 1 refits the same cases every time, ",
         "so its agreement is not resampling stability. For the whole ",
         "cohort use subsample(bootstrap = TRUE, performance = TRUE).",
         call. = FALSE)
  }
  tabs <- lapply(reps, function(r) {
    tab <- r[[1L]]
    stats::setNames(tab[[1L]], rownames(tab))
  })
  if (!all(vapply(tabs, function(v) "err" %in% names(v), logical(1)))) {
    stop("Run randomForestSRC::subsample() with performance = TRUE; ",
         "the result has no OOB error.", call. = FALSE)
  }
  counts <- .learning_counts(fraction, boot, info)
  list(
    fraction = fraction,
    n = counts[["n"]],
    events = counts[["events"]],
    error = vapply(tabs, function(v) v[["err"]], numeric(1)),
    vimp = lapply(tabs, function(v) v[names(v) != "err"])
  )
}

# Cases and events in each refit. A subsample takes as.integer(count * fraction)
# from each stratum, as randomForestSRC 3.10.0's make.strat.sample() does, and
# regression takes as.integer(n * fraction). Exact under the default
# stratify = TRUE; with stratify = FALSE they are expected counts, because the
# result keeps no record of the draw. The double bootstrap draws n cases with
# replacement, so its counts are the cohort's.
.learning_counts <- function(fraction, boot, info) {
  if (boot) return(c(n = info$n, events = info$events))
  if (is.null(info$strata)) {
    n <- as.integer(info$n * fraction)
    return(c(n = n, events = n))
  }
  per <- as.integer(as.numeric(info$strata) * fraction)
  names(per) <- names(info$strata)
  events <- if (info$family == "surv") per[["1"]] else min(per)
  c(n = sum(per), events = events)
}

.learning_stability <- function(vimp, k) {
  if (length(vimp) < 2L) {
    return(c(jaccard = NA_real_, kendall = NA_real_))
  }
  tops <- lapply(vimp, function(v) names(sort(v, decreasing = TRUE))[seq_len(k)])
  pairs <- utils::combn(length(vimp), 2L)
  jac <- apply(pairs, 2L, function(ij) {
    a <- tops[[ij[1L]]]
    b <- tops[[ij[2L]]]
    length(intersect(a, b)) / length(union(a, b))
  })
  tau <- apply(pairs, 2L, function(ij) {
    u <- union(tops[[ij[1L]]], tops[[ij[2L]]])
    r1 <- rank(-vimp[[ij[1L]]])[u]
    r2 <- rank(-vimp[[ij[2L]]])[u]
    suppressWarnings(stats::cor(r1, r2, method = "kendall"))
  })
  tau <- if (all(is.na(tau))) NA_real_ else mean(tau, na.rm = TRUE)
  c(jaccard = mean(jac), kendall = tau)
}

# counts is "exact" for a stratified subsample and "expected" otherwise (an
# unstratified subsample, or the double bootstrap, whose draws vary). An
# unstratified subsample's count is unknown, so its low_events verdict is NA
# rather than a guess; the bootstrap's expected count is the cohort's.
.learning_flag <- function(d, min_events, stratified) {
  boot <- d$fraction == 1
  d$counts <- ifelse(stratified & !boot, "exact", "expected")
  d$low_events <- ifelse(!stratified & !boot, NA, d$events < min_events)
  d$optimistic <- d$fraction > 0.5 & d$fraction < 1
  d
}
