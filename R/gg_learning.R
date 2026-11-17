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
#' The expensive step is the refitting inside `subsample()`.
#' [learning_subsample()] runs it for every fraction and records how each
#' draw was made; keep its result and supply it to `gg_learning()`.
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
#'   between replicates. Defaults to 5. It must leave at least one predictor
#'   out: a top set of every predictor is the same in every replicate, so
#'   `top_k` at or above the number of predictors warns, is capped at that
#'   number, and gives `jaccard` as `NA` rather than a perfect score that says
#'   nothing.
#' @param min_events Non-negative number; fractions with fewer events than
#'   this are flagged in `low_events`. Defaults to 50.
#' @param stratified Logical; for results that do not record it, how the
#'   subsamples were drawn, as passed to `subsample(stratify = )`. Defaults
#'   to `TRUE`, `subsample()`'s own default. Results from
#'   [learning_subsample()] record the setting, and it is used instead.
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
#' times the fraction. `counts` says how far that can be trusted:
#' `"exact"` when [learning_subsample()] recorded a stratified draw;
#' `"declared"` when the results came from `subsample()` directly, which keeps
#' no record, and `stratified = TRUE` vouches for them; and `"expected"` for
#' an unstratified draw, whose counts vary from refit to refit. For those,
#' the reported counts are what a stratified draw would give and `low_events`
#' is `NA`, because whether a refit fell short is unknown. The double
#' bootstrap draws as many cases as the cohort holds, with replacement; it is
#' reported at the cohort's counts, `counts = "expected"`.
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
#' @seealso [learning_subsample()], [plot.gg_learning()],
#'   [randomForestSRC::subsample()],
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
#' set.seed(1)
#' subsamples <- learning_subsample(fit, fractions = c(0.25, 0.5),
#'                                  replicates = 5)
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
  .validate_learning_flag(stratified, "stratified")
  info <- .learning_outcome(subsamples[[1L]]$rf)

  pieces <- lapply(subsamples, .learning_piece, info = info,
                   stratified = stratified)
  fractions <- vapply(pieces, function(p) p$fraction, numeric(1))
  if (anyDuplicated(fractions)) {
    stop("Each subsampling fraction may appear only once.", call. = FALSE)
  }
  pieces <- pieces[order(fractions)]
  p_vars <- length(pieces[[1L]]$vimp[[1L]])
  k <- min(as.integer(top_k), p_vars)
  # A top set holding every predictor is identical in every replicate, so its
  # Jaccard index is 1 however the ranking moves.
  whole_set <- k >= p_vars
  if (whole_set) {
    warning("'top_k' (", top_k, ") covers all ", p_vars, " predictors, so the ",
            "top sets cannot differ and 'jaccard' is NA. Use 'top_k' below ",
            p_vars, ".", call. = FALSE)
  }

  rows <- lapply(pieces, function(p) {
    data.frame(fraction = p$fraction, n = p$n, events = p$events,
               replicate = seq_along(p$error), error = p$error,
               counts = p$counts)
  })
  out <- .learning_flag(do.call(rbind, rows), min_events)

  stab <- lapply(pieces, function(p) {
    s <- .learning_stability(p$vimp, k)
    if (whole_set) s[["jaccard"]] <- NA_real_
    data.frame(fraction = p$fraction, n = p$n, events = p$events,
               B = length(p$vimp), jaccard = s[["jaccard"]],
               kendall = s[["kendall"]], counts = p$counts)
  })
  stab <- .learning_flag(do.call(rbind, stab), min_events)
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
  first <- .learning_parent(subsamples[[1L]]$rf)
  same <- vapply(subsamples, function(s) {
    identical(.learning_parent(s$rf), first, ignore.environment = TRUE)
  }, logical(1))
  if (!all(same)) {
    stop("All 'subsamples' must come from the same forest.", call. = FALSE)
  }
  invisible(TRUE)
}

# The parent forest less what subsample() derives on each call. Grown without
# VIMP, the forest is run again inside every subsample() to compute it, so
# its timings and recorded call differ from call to call (the bootstrap
# records a different one) and it gains error and VIMP output. The data,
# settings and grown trees still have to match, and the trees are what tell
# two forests on the same data apart.
.learning_derived <- c("call", "ctime.internal", "ctime.external",
                       "importance",
                       "err.rate", "err.block.rate", "perf.type",
                       "ptn.membership", "uno.weights")

.learning_parent <- function(rf) {
  rf[setdiff(names(rf), .learning_derived)]
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

.learning_piece <- function(s, info, stratified) {
  boot <- inherits(s, "bootsample")
  # learning_subsample() records the setting; subsample() alone does not, and
  # then the caller's declaration stands in for it.
  captured <- attr(s, "stratify", exact = TRUE)
  counts <- if (boot) {
    "expected"
  } else if (!is.null(captured)) {
    if (isTRUE(captured)) "exact" else "expected"
  } else {
    if (stratified) "declared" else "expected"
  }
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
  sizes <- .learning_counts(fraction, boot, info)
  list(
    fraction = fraction,
    n = sizes[["n"]],
    events = sizes[["events"]],
    counts = counts,
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

# An unstratified subsample's event count is unknown, so its low_events
# verdict is NA rather than a guess. The bootstrap's expected count is the
# cohort's, and is judged.
.learning_flag <- function(d, min_events) {
  unknown <- d$counts == "expected" & d$fraction < 1
  d$low_events <- ifelse(unknown, NA, d$events < min_events)
  d$optimistic <- d$fraction > 0.5 & d$fraction < 1
  d
}
