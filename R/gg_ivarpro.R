##=============================================================================
#' Individual (local) variable importance from a varPro fit
#'
#' Tidy wrapper around [varPro::ivarpro()] for the regression or
#' classification family. Returns one row per (observation, variable)
#' pair where the local-importance cell is non-NA; classification adds
#' a `class` column. `which_obs` collapses to a per-observation
#' profile; `which_class` collapses to a single class. Optional
#' `ivarpro_fit` argument lets callers cache the expensive
#' `ivarpro()` call.
#'
#' @section What this is doing:
#' The varPro framework builds importance from release rules: for a given
#' rule region, it compares a local estimator inside that region to what
#' the estimator becomes after the constraint on the tested variable is
#' removed ("released"). That contrast is summed over many rules and trees
#' to get a global z-score: the quantity [gg_varpro()] shows. What
#' `ivarpro()` adds is a per-observation view of the same mechanism.
#'
#' Concretely: `ivarpro()` walks the forest's rules and, for each
#' (observation, variable) pair, computes a scaled per-rule contribution
#' to predicting that observation. Per-rule LOO removes the observation
#' from its own rule before scoring, so the contribution is not inflated
#' by the observation having helped define the region. Per-region scaling
#' (`scale = "local"`, default) standardises the contribution by the
#' rule's local response standard deviation so values are comparable
#' across rules of different size. Aggregating those per-rule scores into
#' one number per (obs, variable) pair gives the `local_imp` cell.
#'
#' No permutation, no synthetic data: the contrast is always between real
#' subsets of the observed data, defined by the forest's own rules. This
#' is the same no-synthetic-features property that distinguishes
#' [gg_varpro()] from [gg_vimp()]'s Breiman-Cutler permutation importance.
#'
#' @section What `local_imp` actually is (pedantic):
#' `local_imp[i, v]` is the **scaled aggregated rule contribution** of
#' variable `v` to predicting observation `i`, NOT a permutation
#' importance and NOT a SHAP value. **Sign carries direction** of the
#' local response shift inside the rule's region. **Magnitude is on
#' the response scale** when `scale = "global"`, or unit-free when
#' `scale = "local"` (the default). The matrix is **heavily sparse** -
#' an observation contributes only to rules that retain it as OOB; on
#' real data, per-variable NA fractions of 50-95% are common.
#' Comparison with `gg_varpro()` (aggregate split-strength) and
#' `gg_beta_varpro()` (per-rule lasso beta) is diagnostic: a variable
#' that's important globally but has low per-observation contribution
#' for a specific case is interesting; the inverse - high local but
#' low global - flags a regime-specific signal.
#'
#' @section What's in the output:
#' Long-format tidy frame. Regression has columns `obs`, `variable`,
#' `local_imp`, `selected`. Classification adds a `class` column
#' (factor in response-level order). `variable` is a factor whose
#' levels are set by `mean(|local_imp|)` descending across all rows;
#' for classification that aggregate is across all (obs, class) so
#' every facet / panel shows variables in the same row order. NA
#' cells are filtered out - the source matrix is sparse, and the
#' tidy frame only carries the cells where local importance is
#' defined.
#'
#' Provenance attribute carries `source`, `family`, `ntree`, `cutoff`
#' (named numeric vector - length 1 named `"regr"` for regression,
#' length K named with class levels for classification),
#' `cutoff_default`, `use.loo`, `scale`, `n_train`, `n_obs`, `n_var`,
#' `precomputed`, `xvar.names`, `class_levels` (classification only),
#' `which_obs`, `which_class`.
#'
#' @section What you use this for:
#' Per-observation interpretation ("which variables drive *this*
#' prediction?"), variable-selection diagnostics via the aggregate
#' distribution view, and side-by-side comparison against
#' [gg_varpro()] / [gg_beta_varpro()] to spot variables that matter
#' locally but not globally (or vice versa).
#'
#' @section Caching:
#' `ivarpro()` is **the most expensive call in varPro** (per-rule
#' leave-one-out + per-region scaling, often minutes on real data).
#' Compute it once and reuse:
#'
#' ```r
#' v   <- varPro::varpro(medv ~ ., data = Boston, ntree = 200)
#' iv  <- varPro::ivarpro(v, scale = "local")              # expensive, once
#' gg_aggregate <- gg_ivarpro(v, ivarpro_fit = iv)          # cheap
#' gg_case1     <- gg_ivarpro(v, ivarpro_fit = iv, which_obs = 1L)
#' ```
#'
#' Provenance carries `precomputed = TRUE` when `ivarpro_fit` was supplied.
#'
#' @section Classification:
#' For a classification fit, `ivarpro()` returns a list of K matrices
#' (one per class) for multi-class, or a flat data.frame for binary
#' (positive-class importances only - the wrapper normalises this to
#' a single-element list under the last factor level). The wrapper
#' stacks per-class frames into a long-format frame with a `class`
#' column. `which_class = NULL` returns all classes (binary defaults
#' to the last factor level, the positive-class convention used by
#' `glm` and `gg_roc`); `which_class = "<name>"` filters to a single
#' class. `cutoff` polymorphism mirrors [gg_beta_varpro()] - `NULL`
#' is per-class mean(|local_imp|), a scalar broadcasts, a named
#' numeric vector overrides per class with fallback to that class's
#' mean.
#'
#' @section Reproducibility:
#' Byte-for-byte agreement between cached (`ivarpro_fit = iv`) and
#' uncached (`ivarpro_fit = NULL`) outputs requires reusing the same
#' `ivarpro()` result. `set.seed()` alone is not sufficient because
#' per-rule LOO subsampling can drift across separate calls. Reuse
#' `ivarpro_fit` when reproducibility matters.
#'
#' @note Multivariate regression (`regr+`) and survival families are
#'   out of scope for this release. The non-regression / non-class
#'   path errors with a message naming v3.1.0 as the tracker.
#'
#' @param object A `varpro` fit from [varPro::varpro()] (regression or
#'   classification family).
#' @param ... Forwarded to [varPro::ivarpro()] when `ivarpro_fit = NULL`;
#'   ignored otherwise (with a warning). Documented forwardables:
#'   `adaptive`, `cut`, `cut.max`, `ncut`, `nmin`, `nmax`, `noise.na`,
#'   `max.rules.tree`, `max.tree`, `use.loo`, `use.abs`, `scale`.
#' @param which_obs Optional integer scalar - 1-based row index into the
#'   training data. `NULL` (default) returns the aggregate view.
#' @param which_class Optional response level name. `NULL` default on a
#'   binary classification fit resolves to the last factor level
#'   (positive-class convention). Ignored with a warning on regression
#'   fits.
#' @param cutoff Selection threshold on `|local_imp|`. `NULL` (default)
#'   resolves to the per-class `mean(|local_imp|)` (or per-frame mean
#'   for regression). A numeric scalar broadcasts. A named numeric
#'   vector (names a subset of class levels) overrides per class with
#'   fallback to the per-class mean for missing names.
#' @param ivarpro_fit Optional pre-computed [varPro::ivarpro()] result
#'   for the same `object`. Shape-validated.
#'
#' @return A `data.frame` of class `c("gg_ivarpro", "data.frame")`.
#'   Regression: columns `obs / variable / local_imp / selected`.
#'   Classification: long-format with an extra `class` column.
#'   `variable` is a factor whose levels are set by
#'   `mean(|local_imp|)` descending across all rows (the unified
#'   ranking axis shared across facets / panels).
#'
#' @seealso [gg_varpro()], [gg_vimp()], [gg_beta_varpro()], [varPro::ivarpro()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE) &&
#'     requireNamespace("MASS", quietly = TRUE)) {
#'   set.seed(1)
#'   v <- varPro::varpro(medv ~ ., data = MASS::Boston, ntree = 50)
#'   iv <- varPro::ivarpro(v)
#'   gg <- gg_ivarpro(v, ivarpro_fit = iv)
#'   plot(gg)
#' }
#' }
#'
#' @export
gg_ivarpro <- function(object, ..., which_obs = NULL, which_class = NULL,
                       cutoff = NULL, ivarpro_fit = NULL) {
  UseMethod("gg_ivarpro", object)
}

#' @export
gg_ivarpro.default <- function(object, ..., which_obs = NULL,
                               which_class = NULL, cutoff = NULL,
                               ivarpro_fit = NULL) {
  stop("gg_ivarpro: expected a 'varpro' object from varPro::varpro(); ",
       "got an object of class ", paste(class(object), collapse = "/"), ".",
       call. = FALSE)
}

#' @export
gg_ivarpro.varpro <- function(object, ..., which_obs = NULL,
                              which_class = NULL, cutoff = NULL,
                              ivarpro_fit = NULL) {
  fam <- object$family
  if (!fam %in% c("regr", "class")) {
    stop(sprintf(
      paste0("gg_ivarpro currently supports varpro regression and ",
             "classification forests only; got family = '%s'. regr+ and ",
             "survival are tracked for v3.1.0 (see NEWS)."),
      fam
    ), call. = FALSE)
  }

  # Capture forwardable args before dispatch (the dots don't pass through
  # to the internal frames).
  dots_use_loo <- if (is.null(ivarpro_fit)) isTRUE(list(...)$use.loo) else NA
  dots_scale   <- if (is.null(ivarpro_fit)) {
    s <- list(...)$scale
    if (is.null(s)) NA_character_ else s[1L]
  } else {
    NA_character_
  }

  iv <- .resolve_ivarpro_fit(ivarpro_fit, object, fam, list(...))

  # Warn on which_class with regression
  if (fam == "regr" && !is.null(which_class)) {
    warning("gg_ivarpro: which_class ignored for regression family.",
            call. = FALSE)
    which_class <- NULL
  }

  n_train <- if (fam == "regr") nrow(iv) else nrow(iv[[1L]])
  which_obs <- .validate_which_obs(which_obs, n_train)

  if (fam == "regr") {
    return(.gg_ivarpro_regr(object, iv, cutoff, which_obs, ivarpro_fit,
                            dots_use_loo, dots_scale))
  }
  .gg_ivarpro_class(object, iv, cutoff, which_obs, which_class, ivarpro_fit,
                    dots_use_loo, dots_scale)
}

#' @noRd
.resolve_ivarpro_fit <- function(ivarpro_fit, object, fam, dots) {
  if (is.null(ivarpro_fit)) {
    iv <- do.call(varPro::ivarpro, c(list(object), dots))
  } else {
    .validate_ivarpro_fit(ivarpro_fit, object, fam)
    if (length(dots) > 0L) {
      warning(
        "gg_ivarpro: arguments in '...' ignored because ivarpro_fit is supplied.",
        call. = FALSE
      )
    }
    iv <- ivarpro_fit
  }
  # varPro::ivarpro() returns a flat data.frame for binary classification
  # (positive-class importances only). Normalise to the spec's list-of-K
  # shape by wrapping under the last factor level (positive class).
  if (fam == "class" && is.data.frame(iv)) {
    cls_levels <- .ivarpro_class_levels(object)
    iv <- stats::setNames(list(iv), cls_levels[length(cls_levels)])
  }
  iv
}

#' @noRd
.validate_which_obs <- function(which_obs, n_train) {
  if (is.null(which_obs)) return(NULL)
  if (!is.numeric(which_obs) || length(which_obs) != 1L ||
      which_obs != as.integer(which_obs) ||
      which_obs < 1L || which_obs > n_train) {
    stop(sprintf(
      "gg_ivarpro: which_obs = %s is out of range. Valid range: 1..%d.",
      format(which_obs), n_train
    ), call. = FALSE)
  }
  as.integer(which_obs)
}

#' @noRd
.validate_ivarpro_fit <- function(ivarpro_fit, object, fam) {
  xvars <- object$xvar.org.names
  n_train <- nrow(object$x)

  if (fam == "regr") {
    if (!is.data.frame(ivarpro_fit)) {
      stop("gg_ivarpro: ivarpro_fit does not look like a varPro::ivarpro() ",
           "result (expected a data.frame for the regression family).",
           call. = FALSE)
    }
    # varPro::ivarpro() may omit columns for variables it deems
    # uninformative; only require at least one xvar column to be present.
    if (length(intersect(xvars, names(ivarpro_fit))) == 0L) {
      stop("gg_ivarpro: ivarpro_fit has no columns matching the predictors.",
           call. = FALSE)
    }
    if (nrow(ivarpro_fit) != n_train) {
      stop(sprintf(
        paste0("gg_ivarpro: ivarpro_fit has %d rows but object trained ",
               "on %d observations."),
        nrow(ivarpro_fit), n_train), call. = FALSE)
    }
  } else {
    cls <- .ivarpro_class_levels(object)
    # varPro::ivarpro() returns a flat data.frame for binary classification
    # (single positive-class frame), and a named list of K frames for
    # multi-class. Accept either shape here.
    if (is.data.frame(ivarpro_fit)) {
      if (length(cls) != 2L) {
        stop("gg_ivarpro: ivarpro_fit does not look like a varPro::ivarpro() ",
             "result (expected a list of data.frames, one per class).",
             call. = FALSE)
      }
      # varPro::ivarpro() may omit columns for variables it deems
      # uninformative; only require at least one xvar column to be present.
      if (length(intersect(xvars, names(ivarpro_fit))) == 0L) {
        stop("gg_ivarpro: ivarpro_fit has no columns matching the predictors.",
             call. = FALSE)
      }
      if (nrow(ivarpro_fit) != n_train) {
        stop(sprintf(
          paste0("gg_ivarpro: ivarpro_fit has %d rows but object trained ",
                 "on %d observations."),
          nrow(ivarpro_fit), n_train), call. = FALSE)
      }
      return(invisible(NULL))
    }
    if (!is.list(ivarpro_fit)) {
      stop("gg_ivarpro: ivarpro_fit does not look like a varPro::ivarpro() ",
           "result (expected a list of data.frames, one per class).",
           call. = FALSE)
    }
    if (!identical(sort(names(ivarpro_fit)), sort(cls))) {
      stop("gg_ivarpro: ivarpro_fit class names mismatch. Expected: ",
           paste(cls, collapse = ", "), ". Got: ",
           paste(names(ivarpro_fit), collapse = ", "), ".",
           call. = FALSE)
    }
    # Each per-class element must be a data.frame with the right row count
    # and at least one predictor column.
    for (k in names(ivarpro_fit)) {
      el <- ivarpro_fit[[k]]
      if (!is.data.frame(el)) {
        stop(sprintf(
          paste0("gg_ivarpro: ivarpro_fit[['%s']] is not a data.frame ",
                 "(got class '%s')."),
          k, class(el)[1L]), call. = FALSE)
      }
      if (nrow(el) != n_train) {
        stop(sprintf(
          paste0("gg_ivarpro: ivarpro_fit[['%s']] has %d rows but object ",
                 "trained on %d observations."),
          k, nrow(el), n_train), call. = FALSE)
      }
      if (length(intersect(xvars, names(el))) == 0L) {
        stop(sprintf(
          "gg_ivarpro: ivarpro_fit[['%s']] has no columns matching the predictors.",
          k), call. = FALSE)
      }
    }
  }
  invisible(NULL)
}

#' @noRd
.ivarpro_class_levels <- function(object) {
  # Same priority chain as gg_beta_varpro classification - y.org first because
  # varPro internally recodes binary y to 0/1.
  if (!is.null(object$y.org) && is.factor(object$y.org)) return(levels(object$y.org))
  if (is.factor(object$y)) return(levels(object$y))
  if (!is.null(attr(object$y, "levels"))) return(attr(object$y, "levels"))
  sort(unique(as.character(object$y)))
}

#' @noRd
.ivarpro_long <- function(mat, xvars) {
  # mat: nrow = n_train, ncol = p (xvars). Returns long-format
  # data.frame (obs, variable, local_imp) with NA cells dropped.
  rows <- list()
  for (j in seq_along(xvars)) {
    col_vals <- mat[[xvars[j]]]
    keep <- which(is.finite(col_vals))
    if (length(keep) == 0L) next
    rows[[j]] <- data.frame(
      obs       = keep,
      variable  = xvars[j],
      local_imp = col_vals[keep],
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(data.frame(obs = integer(0), variable = character(0),
                      local_imp = numeric(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

#' @noRd
.gg_ivarpro_regr <- function(object, iv, cutoff, which_obs, ivarpro_fit,
                             use_loo, scale_arg) {
  xvars <- object$xvar.org.names
  long  <- .ivarpro_long(iv, xvars)

  # Factor levels are REVERSED descending mean(|local_imp|) so the
  # most-important variable lands at the TOP after coord_flip, matching
  # the gg_vimp / gg_varpro convention.
  agg <- tapply(abs(long$local_imp), long$variable, mean, na.rm = TRUE)
  ord_names <- names(sort(agg, decreasing = TRUE))
  long$variable <- factor(long$variable, levels = rev(ord_names))

  resolved_cutoff <- if (is.null(cutoff)) mean(abs(long$local_imp)) else as.numeric(cutoff)
  long$selected <- abs(long$local_imp) >= resolved_cutoff

  if (!is.null(which_obs)) long <- long[long$obs == which_obs, , drop = FALSE]
  ## Rows stay most-important-first (obs ascending within variable); the
  ## reversed factor levels only drive the plot's vertical order.
  long <- long[order(-as.integer(long$variable), long$obs), , drop = FALSE]
  rownames(long) <- NULL

  class(long) <- c("gg_ivarpro", "data.frame")
  attr(long, "provenance") <- list(
    source         = "varPro::ivarpro",
    family         = "regr",
    ntree          = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
    cutoff         = stats::setNames(resolved_cutoff, "regr"),
    cutoff_default = is.null(cutoff),
    use.loo        = use_loo,
    scale          = scale_arg,
    n_train        = nrow(object$x),
    n_obs          = length(unique(long$obs)),
    n_var          = nlevels(long$variable),
    precomputed    = !is.null(ivarpro_fit),
    xvar.names     = xvars,
    which_obs      = which_obs,
    which_class    = NULL
  )
  long
}

#' @noRd
.gg_ivarpro_class <- function(object, iv, cutoff, which_obs, which_class,
                              ivarpro_fit, use_loo, scale_arg) {
  cls   <- .ivarpro_class_levels(object)
  xvars <- object$xvar.org.names
  n_cls <- length(cls)

  # Per-class long-format frames
  per_class_long <- lapply(cls, function(c_name) {
    mat <- iv[[c_name]]
    if (is.null(mat)) return(NULL)
    df <- .ivarpro_long(mat, xvars)
    df$class <- c_name
    df
  })
  long <- do.call(rbind, per_class_long)

  if (nrow(long) == 0L) {
    return(.gg_ivarpro_empty(object, "class", which_obs, which_class,
                             ivarpro_fit, cutoff))
  }

  # Unified factor-level ordering across all (obs, class), REVERSED so the
  # most-important variable lands at the TOP after coord_flip; shared
  # across every class facet for alignment.
  agg <- tapply(abs(long$local_imp), long$variable, mean, na.rm = TRUE)
  ord_names <- names(sort(agg, decreasing = TRUE))
  long$variable <- factor(long$variable, levels = rev(ord_names))

  # Validate which_class
  if (!is.null(which_class)) {
    if (!which_class %in% cls) {
      stop(sprintf(
        "gg_ivarpro: which_class = '%s' is not a level of the response. Levels: %s.",
        which_class, paste(cls, collapse = ", ")
      ), call. = FALSE)
    }
  } else if (n_cls == 2L) {
    which_class <- cls[n_cls]   # binary default = last factor level
  }

  # Per-class mean(|local_imp|) for cutoff resolution
  per_class_mean <- vapply(cls, function(c_name) {
    vals <- long$local_imp[long$class == c_name]
    if (length(vals) == 0L) NA_real_ else mean(abs(vals))
  }, numeric(1))
  names(per_class_mean) <- cls

  resolved_cutoff <- .resolve_class_cutoff(cutoff, per_class_mean, cls)

  long$selected <- mapply(function(li, cl) abs(li) >= resolved_cutoff[[cl]],
                          long$local_imp, long$class)

  if (!is.null(which_class)) long <- long[long$class == which_class, , drop = FALSE]
  if (!is.null(which_obs))   long <- long[long$obs   == which_obs,   , drop = FALSE]

  long$class <- factor(long$class, levels = cls)
  ## Within each class, rows stay most-important-first (obs ascending); the
  ## reversed variable factor levels only drive the plot's vertical order.
  long <- long[order(long$class, -as.integer(long$variable), long$obs), ,
               drop = FALSE]
  rownames(long) <- NULL

  class(long) <- c("gg_ivarpro", "data.frame")
  attr(long, "provenance") <- list(
    source         = "varPro::ivarpro",
    family         = "class",
    ntree          = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
    cutoff         = resolved_cutoff,
    cutoff_default = is.null(cutoff),
    use.loo        = use_loo,
    scale          = scale_arg,
    n_train        = nrow(object$x),
    n_obs          = length(unique(long$obs)),
    n_var          = nlevels(long$variable),
    precomputed    = !is.null(ivarpro_fit),
    xvar.names     = xvars,
    class_levels   = cls,
    which_obs      = which_obs,
    which_class    = which_class
  )
  long
}

#' @noRd
.gg_ivarpro_empty <- function(object, fam, which_obs, which_class,
                              ivarpro_fit, cutoff) {
  cols <- if (fam == "class") {
    list(obs = integer(0), variable = factor(character(0)),
         class = factor(character(0)),
         local_imp = numeric(0), selected = logical(0))
  } else {
    list(obs = integer(0), variable = factor(character(0)),
         local_imp = numeric(0), selected = logical(0))
  }
  base <- as.data.frame(cols, stringsAsFactors = FALSE)
  class(base) <- c("gg_ivarpro", "data.frame")
  prov <- if (fam == "class") {
    cls <- .ivarpro_class_levels(object)
    list(source = "varPro::ivarpro", family = "class",
         cutoff = stats::setNames(rep(NA_real_, length(cls)), cls),
         cutoff_default = is.null(cutoff),
         n_train = nrow(object$x), n_obs = 0L,
         precomputed = !is.null(ivarpro_fit),
         class_levels = cls,
         which_obs = which_obs, which_class = which_class)
  } else {
    list(source = "varPro::ivarpro", family = "regr",
         cutoff = stats::setNames(NA_real_, "regr"),
         cutoff_default = is.null(cutoff),
         n_train = nrow(object$x), n_obs = 0L,
         precomputed = !is.null(ivarpro_fit),
         which_obs = which_obs, which_class = NULL)
  }
  attr(base, "provenance") <- prov
  base
}

#' @importFrom ggplot2 autoplot
#' @export
autoplot.gg_ivarpro <- function(object, ...) {
  plot(object, ...)
}
