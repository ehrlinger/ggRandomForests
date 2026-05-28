##=============================================================================
#' Per-variable lasso-beta importance from a varPro fit
#'
#' Tidy wrapper around [varPro::beta.varpro()] for the regression or
#' classification family.
#' Aggregates the per-rule lasso coefficient \eqn{\hat{\beta}}{beta hat} by
#' variable into the mean absolute value
#' \eqn{\mathrm{mean}(|\hat{\beta}|)}{mean(|beta hat|)} and flags variables
#' above a scalar cutoff. Optional
#' `beta_fit` argument lets callers compute the expensive
#' `beta.varpro()` step once and reuse the result.
#'
#' @section What this is doing:
#' For each rule (a tree-branch pair) in the forest, [varPro::beta.varpro()]
#' fits a one-predictor lasso regression of the response on the released
#' variable's values, restricted to the OOB observations inside the rule's
#' region. The wrapper aggregates those per-rule coefficients into one
#' number per variable.
#'
#' @section What `imp` actually is (pedantic, because the column name is misleading):
#' The `imp` column on `beta.varpro()`'s `$results` is **not** a
#' variable-importance score in the conventional sense. It is a regularised
#' regression coefficient. Specifically:
#'
#' - Per rule, `glmnet` fits a one-predictor lasso of the response on
#'   the released variable inside the rule's OOB region. `use.cv = TRUE`
#'   selects \eqn{\lambda}{lambda} by 10-fold CV (default `nfolds = 10`);
#'   `use.1se = TRUE` (default) picks `lambda.1se`. `use.cv = FALSE` uses the
#'   full \eqn{\lambda}{lambda} path.
#' - `imp` is the **fitted coefficient** \eqn{\hat{\beta}}{beta hat} at the
#'   chosen \eqn{\lambda}{lambda}. **Sign is
#'   real** (direction of local association). **Magnitude depends on
#'   the predictor's units** (raw `x`, no standardisation); a predictor
#'   in millimetres has a smaller \eqn{|\hat{\beta}|}{|beta hat|} than the
#'   same predictor in metres.
#' - Lasso shrinkage can drive \eqn{\hat{\beta}}{beta hat} to **exactly zero**.
#'   Those zeros are
#'   data, not missingness, and are kept in the aggregation. Convergence
#'   failures land as `NA_real_` and are dropped.
#' - The per-variable aggregate is `beta_mean`,
#'   \eqn{\mathrm{mean}(|\hat{\beta}|)}{mean(|beta hat|)}, across the
#'   rules where this variable was released. It is **not** a permutation
#'   importance, **not** a split-strength importance, and **not** directly
#'   comparable on the same numeric axis to `gg_varpro()`'s z-scores.
#'   Disagreement with `gg_varpro` is often diagnostic, not a bug.
#'
#' In code form: `imp_r` is the glmnet coefficient \eqn{\hat{\beta}}{beta hat}
#' fit on `y ~ x_v` restricted to rule r, with \eqn{\lambda}{lambda} chosen by
#' `use.cv` / `use.1se`.
#'
#' @section What's in the output:
#' One row per released variable. Columns:
#' - `variable`: predictor name.
#' - `beta_mean`: mean of \eqn{|\hat{\beta}|}{|beta hat|} across that
#'   variable's rules.
#' - `n_rules`: count of rules contributing (zero-beta rules included; only
#'   `NA` failures excluded).
#' - `selected`: `beta_mean >= cutoff`.
#'
#' Provenance attribute carries `source`, `family`, `ntree`, `cutoff`,
#' `cutoff_default`, `use.cv`, `n_rules_total`, `n_rules_nonzero`,
#' `precomputed`, and `xvar.names`.
#'
#' @section What you use this for:
#' Picking variables when local effects matter more than aggregate
#' split-strength contribution. Compare side-by-side with [gg_varpro()] —
#' a variable that scores high here but low in `gg_varpro` is one whose
#' local linear effect inside many rules is real even though its
#' release-rule contrast is modest.
#'
#' @section Caching:
#' `beta.varpro()` is the expensive call (per-rule `glmnet` / `cv.glmnet`,
#' often minutes on real data). Compute it once and reuse:
#'
#' ```r
#' v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 200)
#' b <- varPro::beta.varpro(v, use.cv = TRUE)        # expensive, once
#' gg_a <- gg_beta_varpro(v, beta_fit = b)            # cheap
#' gg_b <- gg_beta_varpro(v, beta_fit = b, cutoff = 0.5)
#' ```
#'
#' Provenance carries `precomputed = TRUE` when `beta_fit` was supplied.
#'
#' @section Classification:
#' For a varpro classification fit (`object$family == "class"`,
#' binary or multi-class), the returned frame is long-format with an
#' extra `class` column: one row per (variable, class) pair. The
#' `beta_mean` column aggregates the **per-class lasso coefficient**
#' \eqn{\hat{\beta}}{beta hat} stored in
#' `beta.varpro()`'s `imp.<k>` columns (one per class level). Same
#' pedantic-beta semantics as regression, applied independently to each
#' class.
#'
#' **Binary default**: `which_class = NULL` resolves to the *last*
#' factor level of the response — the positive-class convention used
#' by `glm` and `gg_roc`. For a 30-day-mortality outcome with levels
#' `c("no", "yes")`, that means the wrapper shows you `"yes"` (the
#' event) by default.
#'
#' **Multi-class default**: `which_class = NULL` returns all K
#' classes; the plot method renders `facet_wrap(~ class)` with one
#' cutoff line per facet.
#'
#' **`which_class = "<name>"`** filters to a single class regardless
#' of K. Errors if the name isn't in the response levels.
#'
#' **Per-class cutoffs**: `cutoff = NULL` resolves to each class's
#' `mean(beta_mean)`. A scalar broadcasts. A named numeric vector
#' overrides per class; missing names fall back to that class's mean.
#'
#' Example (30-day mortality, binary):
#' ```r
#' fit <- varPro::varpro(event_30d ~ ., data = clinical, ntree = 200)
#' gg  <- gg_beta_varpro(fit)   # default: "yes" panel
#' plot(gg)
#' ```
#'
#' @section Reproducibility:
#' Byte-for-byte agreement between cached (`beta_fit = b`) and uncached
#' (`beta_fit = NULL`) outputs requires that `b` was computed by
#' `beta.varpro(object, ...)` on the same `object` — `set.seed()` alone is
#' not sufficient, because `beta.varpro`'s internal `cv.glmnet` fits can
#' pick slightly different folds across separate calls. Reuse `beta_fit`
#' when reproducibility matters.
#'
#' @note Multivariate regression (`regr+`) and survival families are out
#'   of scope for this release and tracked for v3.1.0. The
#'   unsupported-family path errors with a message pointing at that work.
#'
#' @param object A `varpro` fit from [varPro::varpro()] (regression or
#'   classification family).
#' @param ... Forwarded to [varPro::beta.varpro()] when `beta_fit = NULL`;
#'   ignored otherwise (with a warning). Documented forwardables: `use.cv`,
#'   `use.1se`, `nfolds`, `maxit`, `thresh`, `max.rules.tree`, `max.tree`.
#' @param cutoff Selection threshold on `beta_mean`. `NULL` (default) →
#'   `mean(beta_mean)` across released variables. Numeric scalar otherwise.
#' @param beta_fit Optional pre-computed [varPro::beta.varpro()] result for
#'   the same `object`. `NULL` (default) → the wrapper runs `beta.varpro()`
#'   itself. When supplied, must be a `varpro`-class object whose `$results`
#'   has columns `tree / branch / variable / n.oob / imp`.
#' @param which_class For a classification fit, name of a single response
#'   level to subset on. `NULL` (default) returns all classes (binary fits
#'   resolve to the *last* factor level — the positive-class convention
#'   used by `glm` and `gg_roc`). Ignored with a warning on regression
#'   fits.
#'
#' @return A `data.frame` of class `c("gg_beta_varpro", "data.frame")`.
#'   For a regression fit: one row per released variable, sorted by
#'   `beta_mean` descending. For a classification fit: long-format with
#'   an extra `class` column, one row per (variable, class) pair;
#'   `variable` is a factor whose levels are set by
#'   `mean(|sum-of-class-beta|)` descending so every facet / panel shares
#'   the same row order. `which_class` (or the binary default
#'   last-factor-level) collapses the output to a single class.
#'
#' @seealso [gg_varpro()], [plot.gg_beta_varpro()], [varPro::beta.varpro()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
#'   b <- varPro::beta.varpro(v)
#'   gg <- gg_beta_varpro(v, beta_fit = b)
#'   plot(gg)
#' }
#' }
#'
#' @export
gg_beta_varpro <- function(object, ..., cutoff = NULL, beta_fit = NULL,
                           which_class = NULL) {
  UseMethod("gg_beta_varpro", object)
}

#' @export
gg_beta_varpro.varpro <- function(object, ..., cutoff = NULL,
                                  beta_fit = NULL, which_class = NULL) {
  if (!inherits(object, "varpro")) {
    stop("gg_beta_varpro: expected a 'varpro' object from varPro::varpro().",
         call. = FALSE)
  }
  fam <- object$family
  if (!fam %in% c("regr", "class")) {
    stop(sprintf(
      paste0("gg_beta_varpro currently supports varpro regression and ",
             "classification forests only; got family = '%s'. regr+ and ",
             "survival are tracked for v3.1.0 (see vignette / NEWS)."),
      fam
    ), call. = FALSE)
  }

  # Resolve beta_fit (cache path)
  if (is.null(beta_fit)) {
    b <- varPro::beta.varpro(object, ...)
  } else {
    .validate_beta_fit(beta_fit, object, fam)
    if (length(list(...)) > 0L) {
      warning("gg_beta_varpro: arguments in '...' ignored because beta_fit is supplied.",
              call. = FALSE)
    }
    b <- beta_fit
  }

  # Warn on which_class with regression
  if (fam == "regr" && !is.null(which_class)) {
    warning("gg_beta_varpro: which_class ignored for regression family.",
            call. = FALSE)
    which_class <- NULL
  }

  # Capture use.cv from `...` here (NOT inside the internals — the dots
  # don't pass through to the internal frame).
  dots_use_cv <- if (is.null(beta_fit)) isTRUE(list(...)$use.cv) else NA

  # Empty fast-path
  if (is.null(b) || nrow(b$results) == 0L) {
    return(.gg_beta_varpro_empty(object, fam, which_class, beta_fit, cutoff))
  }

  if (fam == "regr") {
    return(.gg_beta_varpro_regr(object, b, cutoff, beta_fit, use_cv = dots_use_cv))
  }
  .gg_beta_varpro_class(object, b, cutoff, which_class, beta_fit,
                        use_cv = dots_use_cv)
}

#' @noRd
.gg_beta_varpro_regr <- function(object, b, cutoff, beta_fit, use_cv) {
  res <- b$results
  res <- res[is.finite(res$imp), , drop = FALSE]
  n_rules_total   <- nrow(b$results)
  n_rules_nonzero <- sum(abs(res$imp) > 0)

  var_name <- b$xvar.names[res$variable]
  beta_mean_v <- vapply(split(abs(res$imp), var_name), mean, numeric(1))
  n_rules_v   <- vapply(split(res$imp,        var_name), length, integer(1))

  # Rows are most-important-first (descending); the variable factor levels
  # are REVERSED so that, after coord_flip() in the plot method, the
  # most-important variable lands at the TOP. This matches the
  # gg_vimp / gg_varpro convention.
  ord_names <- names(sort(beta_mean_v, decreasing = TRUE))

  resolved_cutoff <- if (is.null(cutoff)) mean(beta_mean_v) else as.numeric(cutoff)

  out <- data.frame(
    variable  = factor(ord_names, levels = rev(ord_names)),
    beta_mean = unname(beta_mean_v[ord_names]),
    n_rules   = as.integer(unname(n_rules_v[ord_names])),
    stringsAsFactors = FALSE
  )
  out$selected <- out$beta_mean >= resolved_cutoff
  rownames(out) <- NULL

  class(out) <- c("gg_beta_varpro", "data.frame")
  attr(out, "provenance") <- list(
    source          = "varPro::beta.varpro",
    family          = "regr",
    ntree           = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
    cutoff          = stats::setNames(resolved_cutoff, "regr"),
    cutoff_default  = is.null(cutoff),
    use.cv          = use_cv,
    n_rules_total   = n_rules_total,
    n_rules_nonzero = n_rules_nonzero,
    precomputed     = !is.null(beta_fit),
    xvar.names      = b$xvar.names
  )
  out
}

#' @noRd
.validate_beta_fit <- function(beta_fit, object, fam) {
  if (!inherits(beta_fit, "varpro") || !is.data.frame(beta_fit$results)) {
    stop("gg_beta_varpro: beta_fit does not look like a varPro::beta.varpro() result. ",
         "Expected a varpro-class object with a data.frame in $results.",
         call. = FALSE)
  }
  required_cols <- c("tree", "branch", "variable", "n.oob", "imp")
  missing_cols <- setdiff(required_cols, names(beta_fit$results))
  if (length(missing_cols) > 0L) {
    stop("gg_beta_varpro: beta_fit does not look like a varPro::beta.varpro() result. ",
         "Missing column(s): ", paste(missing_cols, collapse = ", "), ".",
         call. = FALSE)
  }
  if (fam == "class") {
    cls <- .class_levels_from_varpro(object)
    cls_cols <- paste0("imp.", seq_along(cls))
    missing_cls_cols <- setdiff(cls_cols, names(beta_fit$results))
    if (length(missing_cls_cols) > 0L) {
      stop("gg_beta_varpro: beta_fit does not look like a classification ",
           "varPro::beta.varpro() result. Missing per-class column(s): ",
           paste(missing_cls_cols, collapse = ", "), ". ",
           "Expected one imp.<k> column per response level (",
           length(cls), " levels: ", paste(cls, collapse = ", "), ").",
           call. = FALSE)
    }
  }
  invisible(NULL)
}

#' @noRd
.class_levels_from_varpro <- function(object) {
  # varPro preserves original factor names in y.org; object$y may have been
  # internally relabelled to 0/1 for binary fits.
  if (!is.null(object$y.org) && is.factor(object$y.org)) {
    return(levels(object$y.org))
  }
  if (is.factor(object$y)) {
    return(levels(object$y))
  }
  if (!is.null(attr(object$y, "levels"))) {
    return(attr(object$y, "levels"))
  }
  sort(unique(as.character(object$y)))
}

#' @noRd
.resolve_class_cutoff <- function(cutoff, per_class_mean, class_levels) {
  n_classes <- length(class_levels)
  if (is.null(cutoff)) {
    return(per_class_mean)
  }
  if (is.null(names(cutoff))) {
    if (length(cutoff) != 1L) {
      stop("gg_beta_varpro: cutoff must be NULL, scalar, or a named vector with names in class levels.",
           call. = FALSE)
    }
    return(stats::setNames(rep(as.numeric(cutoff), n_classes), class_levels))
  }
  bad <- setdiff(names(cutoff), class_levels)
  if (length(bad) > 0L) {
    stop(sprintf(
      "gg_beta_varpro: cutoff name(s) '%s' is not a level of the response. Levels: %s.",
      paste(bad, collapse = ", "),
      paste(class_levels, collapse = ", ")
    ), call. = FALSE)
  }
  cv <- per_class_mean
  cv[names(cutoff)] <- as.numeric(cutoff)
  cv
}

#' @noRd
.gg_beta_varpro_class <- function(object, b, cutoff, which_class, beta_fit,
                                  use_cv) {
  class_levels <- .class_levels_from_varpro(object)
  n_classes    <- length(class_levels)
  imp_cols     <- paste0("imp.", seq_len(n_classes))

  # Validate which_class
  if (!is.null(which_class)) {
    if (!which_class %in% class_levels) {
      stop(sprintf(
        "gg_beta_varpro: which_class = '%s' is not a level of the response. Levels: %s.",
        which_class, paste(class_levels, collapse = ", ")
      ), call. = FALSE)
    }
  } else if (n_classes == 2L) {
    which_class <- class_levels[n_classes]   # binary default = last (positive class)
  }

  res <- b$results
  var_name <- b$xvar.names[res$variable]

  # Total |imp| per variable. ord_names is most-important-first (drives row
  # sort + summary); lvl is the REVERSED order used for the shared factor
  # levels so the most-important variable lands at the TOP after coord_flip,
  # consistently across every class facet.
  res_total <- res[is.finite(res$imp), , drop = FALSE]
  total_var <- b$xvar.names[res_total$variable]
  beta_mean_total <- vapply(split(abs(res_total$imp), total_var), mean, numeric(1))
  ord_names <- names(sort(beta_mean_total, decreasing = TRUE))
  lvl <- rev(ord_names)

  # Per-class aggregation — long format
  rows <- list()
  for (k in seq_len(n_classes)) {
    col <- imp_cols[k]
    imp_k <- res[[col]]
    keep  <- is.finite(imp_k)
    if (!any(keep)) next
    sub_var <- var_name[keep]
    sub_imp <- imp_k[keep]
    beta_mean_k <- vapply(split(abs(sub_imp), sub_var), mean, numeric(1))
    n_rules_k   <- vapply(split(sub_imp,        sub_var), length, integer(1))
    vars_present <- intersect(ord_names, names(beta_mean_k))
    rows[[k]] <- data.frame(
      variable  = factor(vars_present, levels = lvl),
      class     = class_levels[k],
      beta_mean = unname(beta_mean_k[vars_present]),
      n_rules   = as.integer(unname(n_rules_k[vars_present])),
      stringsAsFactors = FALSE
    )
  }
  long <- do.call(rbind, rows)

  # Resolve cutoff
  per_class_mean <- vapply(class_levels, function(cls) {
    vals <- long$beta_mean[long$class == cls]
    if (length(vals) == 0L) NA_real_ else mean(vals)
  }, numeric(1))
  names(per_class_mean) <- class_levels

  resolved_cutoff <- .resolve_class_cutoff(cutoff, per_class_mean, class_levels)

  long$selected <- mapply(function(bm, cls) bm >= resolved_cutoff[[cls]],
                          long$beta_mean, long$class)

  # Filter to single class if requested
  if (!is.null(which_class)) {
    long <- long[long$class == which_class, , drop = FALSE]
  }

  # Sort: class factor order, then most-important-first within class. The
  # reversed variable factor levels only drive the plot's vertical order.
  long$class <- factor(long$class, levels = class_levels)
  long <- long[order(long$class, -as.integer(long$variable)), , drop = FALSE]
  rownames(long) <- NULL

  class(long) <- c("gg_beta_varpro", "data.frame")
  attr(long, "provenance") <- list(
    source          = "varPro::beta.varpro",
    family          = "class",
    ntree           = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
    cutoff          = resolved_cutoff,
    cutoff_default  = is.null(cutoff),
    use.cv          = use_cv,
    n_rules_total   = nrow(b$results),
    n_rules_nonzero = sum(abs(res_total$imp) > 0, na.rm = TRUE),
    precomputed     = !is.null(beta_fit),
    xvar.names      = b$xvar.names,
    class_levels    = class_levels,
    which_class     = which_class
  )
  long
}

#' @noRd
.gg_beta_varpro_empty <- function(object, fam, which_class, beta_fit, cutoff) {
  base <- data.frame(
    variable  = factor(character(0)),
    beta_mean = numeric(0),
    n_rules   = integer(0),
    selected  = logical(0),
    stringsAsFactors = FALSE
  )
  if (fam == "class") {
    base <- cbind(base[, "variable", drop = FALSE],
                  class    = character(0),
                  base[, c("beta_mean", "n_rules", "selected"), drop = FALSE])
  }
  class(base) <- c("gg_beta_varpro", "data.frame")

  # Build provenance with shape-stable cutoff:
  # regr  → c("regr" = NA_real_)
  # class → named NA_real_ vector, one entry per class level
  if (fam == "class") {
    class_levels <- .class_levels_from_varpro(object)
    cutoff_empty <- stats::setNames(rep(NA_real_, length(class_levels)),
                                    class_levels)
    prov <- list(
      source = "varPro::beta.varpro", family = "class",
      n_rules_total = 0L, n_rules_nonzero = 0L,
      cutoff = cutoff_empty,
      cutoff_default = is.null(cutoff),
      use.cv = NA,
      precomputed = !is.null(beta_fit),
      class_levels = class_levels,
      which_class = which_class
    )
  } else {
    prov <- list(
      source = "varPro::beta.varpro", family = "regr",
      n_rules_total = 0L, n_rules_nonzero = 0L,
      cutoff = stats::setNames(NA_real_, "regr"),
      cutoff_default = is.null(cutoff),
      use.cv = NA,
      precomputed = !is.null(beta_fit),
      which_class = which_class
    )
  }
  attr(base, "provenance") <- prov
  base
}

#' @importFrom ggplot2 autoplot
#' @export
autoplot.gg_beta_varpro <- function(object, ...) {
  plot.gg_beta_varpro(object, ...)
}
