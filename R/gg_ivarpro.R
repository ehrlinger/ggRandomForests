##=============================================================================
#' Individual (local) variable importance from a varPro fit
#'
#' Tidy wrapper around [varPro::ivarpro()] for regression and classification
#' families. Returns a long-format frame with one row per (observation,
#' variable) pair where the local-importance cell is non-NA;
#' classification adds an extra `class` column. `which_obs` collapses to
#' a single-observation profile; `which_class` collapses to a single
#' class. Optional `ivarpro_fit` argument lets callers cache the
#' expensive `ivarpro()` call.
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
#' @seealso [gg_varpro()], [gg_beta_varpro()], [varPro::ivarpro()].
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
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
gg_ivarpro.varpro <- function(object, ..., which_obs = NULL,
                              which_class = NULL, cutoff = NULL,
                              ivarpro_fit = NULL) {
  if (!inherits(object, "varpro")) {
    stop("gg_ivarpro: expected a 'varpro' object from varPro::varpro().",
         call. = FALSE)
  }
  fam <- object$family
  if (!fam %in% c("regr", "class")) {
    stop(sprintf(
      paste0("gg_ivarpro currently supports varpro regression and ",
             "classification forests only; got family = '%s'. regr+ and ",
             "survival are tracked under Phase 4d follow-ups (see NEWS)."),
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

  # Resolve ivarpro_fit
  if (is.null(ivarpro_fit)) {
    iv <- varPro::ivarpro(object, ...)
  } else {
    .validate_ivarpro_fit(ivarpro_fit, object, fam)
    if (length(list(...)) > 0L) {
      warning("gg_ivarpro: arguments in '...' ignored because ivarpro_fit is supplied.",
              call. = FALSE)
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

  # Warn on which_class with regression
  if (fam == "regr" && !is.null(which_class)) {
    warning("gg_ivarpro: which_class ignored for regression family.",
            call. = FALSE)
    which_class <- NULL
  }

  # Validate which_obs
  n_train <- if (fam == "regr") nrow(iv) else nrow(iv[[1L]])
  if (!is.null(which_obs)) {
    if (!is.numeric(which_obs) || length(which_obs) != 1L ||
        which_obs != as.integer(which_obs) ||
        which_obs < 1L || which_obs > n_train) {
      stop(sprintf(
        "gg_ivarpro: which_obs = %s is out of range. Valid range: 1..%d.",
        format(which_obs), n_train
      ), call. = FALSE)
    }
    which_obs <- as.integer(which_obs)
  }

  if (fam == "regr") {
    return(.gg_ivarpro_regr(object, iv, cutoff, which_obs, ivarpro_fit,
                            dots_use_loo, dots_scale))
  }
  .gg_ivarpro_class(object, iv, cutoff, which_obs, which_class, ivarpro_fit,
                    dots_use_loo, dots_scale)
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
      stop(sprintf("gg_ivarpro: ivarpro_fit has %d rows but object trained ",
                   "on %d observations.", nrow(ivarpro_fit), n_train),
           call. = FALSE)
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

  # Factor-level ordering by descending mean(|local_imp|)
  agg <- tapply(abs(long$local_imp), long$variable, mean, na.rm = TRUE)
  ord_names <- names(sort(agg, decreasing = TRUE))
  long$variable <- factor(long$variable, levels = ord_names)

  resolved_cutoff <- if (is.null(cutoff)) mean(abs(long$local_imp)) else as.numeric(cutoff)
  long$selected <- abs(long$local_imp) >= resolved_cutoff

  if (!is.null(which_obs)) long <- long[long$obs == which_obs, , drop = FALSE]
  long <- long[order(long$variable, long$obs), , drop = FALSE]
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

  # Unified factor-level ordering across all (obs, class)
  agg <- tapply(abs(long$local_imp), long$variable, mean, na.rm = TRUE)
  ord_names <- names(sort(agg, decreasing = TRUE))
  long$variable <- factor(long$variable, levels = ord_names)

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
  long <- long[order(long$class, long$variable, long$obs), , drop = FALSE]
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
