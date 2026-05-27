##=============================================================================
#' Per-variable lasso-β importance from a varPro fit
#'
#' Tidy wrapper around [varPro::beta.varpro()] for the regression family.
#' Aggregates the per-rule lasso coefficient (β̂) by variable into
#' `mean(|β̂|)` and flags variables above a scalar cutoff. Optional
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
#'   selects λ by 10-fold CV (default `nfolds = 10`); `use.1se = TRUE`
#'   (default) picks `lambda.1se`. `use.cv = FALSE` uses the full λ path.
#' - `imp` is the **fitted coefficient β̂** at the chosen λ. **Sign is
#'   real** (direction of local association). **Magnitude depends on
#'   the predictor's units** (raw `x`, no standardisation); a predictor
#'   in millimetres has a smaller |β̂| than the same predictor in metres.
#' - Lasso shrinkage can drive β̂ to **exactly zero**. Those zeros are
#'   data, not missingness, and are kept in the aggregation. Convergence
#'   failures land as `NA_real_` and are dropped.
#' - The per-variable aggregate is `beta_mean = mean(|β̂|)` across the
#'   rules where this variable was released. It is **not** a permutation
#'   importance, **not** a split-strength importance, and **not** directly
#'   comparable on the same numeric axis to `gg_varpro()`'s z-scores.
#'   Disagreement with `gg_varpro` is often diagnostic, not a bug.
#'
#' In code form: `imp_r = β̂_glmnet(y | x_v restricted to rule r, λ chosen by use.cv / use.1se)`.
#'
#' @section What's in the output:
#' One row per released variable. Columns:
#' - `variable`: predictor name.
#' - `beta_mean`: mean of `|β̂|` across that variable's rules.
#' - `n_rules`: count of rules contributing (zero-β rules included; only
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
#' @section Reproducibility:
#' Byte-for-byte agreement between cached (`beta_fit = b`) and uncached
#' (`beta_fit = NULL`) outputs requires that `b` was computed by
#' `beta.varpro(object, ...)` on the same `object` — `set.seed()` alone is
#' not sufficient, because `beta.varpro`'s internal `cv.glmnet` fits can
#' pick slightly different folds across separate calls. Reuse `beta_fit`
#' when reproducibility matters.
#'
#' @note Classification, multivariate regression (`regr+`), and survival
#'   families are out of scope for this release. The non-regression path
#'   errors with a message naming Phase 4d as the tracker (see NEWS).
#'
#' @param object A `varpro` fit from [varPro::varpro()] (regression family).
#' @param ... Forwarded to [varPro::beta.varpro()] when `beta_fit = NULL`;
#'   ignored otherwise (with a warning). Documented forwardables: `use.cv`,
#'   `use.1se`, `nfolds`, `maxit`, `thresh`, `max.rules.tree`, `max.tree`.
#' @param cutoff Selection threshold on `beta_mean`. `NULL` (default) →
#'   `mean(beta_mean)` across released variables. Numeric scalar otherwise.
#' @param beta_fit Optional pre-computed [varPro::beta.varpro()] result for
#'   the same `object`. `NULL` (default) → the wrapper runs `beta.varpro()`
#'   itself. When supplied, must be a `varpro`-class object whose `$results`
#'   has columns `tree / branch / variable / n.oob / imp`.
#' @param which_class For classification fits only: name of a single response
#'   level to subset on. `NULL` (default) returns all classes (binary fits
#'   resolve to the *last* factor level — the positive-class convention).
#'   Ignored with a warning on regression fits. Full classification details
#'   land in a dedicated `@section` in a subsequent doc pass.
#'
#' @return A `data.frame` of class `c("gg_beta_varpro", "data.frame")`,
#'   one row per released variable, sorted by `beta_mean` descending.
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
             "survival are tracked under Phase 4d (see vignette / NEWS)."),
      fam
    ), call. = FALSE)
  }

  # Resolve beta_fit (cache path)
  if (is.null(beta_fit)) {
    b <- varPro::beta.varpro(object, ...)
  } else {
    required_cols <- c("tree", "branch", "variable", "n.oob", "imp")
    if (!inherits(beta_fit, "varpro") || !is.data.frame(beta_fit$results)) {
      stop("gg_beta_varpro: beta_fit does not look like a varPro::beta.varpro() result. ",
           "Expected a varpro-class object with a data.frame in $results.",
           call. = FALSE)
    }
    missing_cols <- setdiff(required_cols, names(beta_fit$results))
    if (length(missing_cols) > 0L) {
      stop("gg_beta_varpro: beta_fit does not look like a varPro::beta.varpro() result. ",
           "Missing column(s): ", paste(missing_cols, collapse = ", "), ".",
           call. = FALSE)
    }
    dots <- list(...)
    if (length(dots) > 0L) {
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
    return(.gg_beta_varpro_empty(fam, which_class, beta_fit, cutoff))
  }

  if (fam == "regr") {
    return(.gg_beta_varpro_regr(object, b, cutoff, beta_fit, use_cv = dots_use_cv))
  }
  .gg_beta_varpro_class(object, b, cutoff, which_class, beta_fit,
                        use_cv = dots_use_cv)
}

#' @keywords internal
.gg_beta_varpro_regr <- function(object, b, cutoff, beta_fit, use_cv) {
  res <- b$results
  res <- res[is.finite(res$imp), , drop = FALSE]
  n_rules_total   <- nrow(b$results)
  n_rules_nonzero <- sum(abs(res$imp) > 0)

  var_name <- b$xvar.names[res$variable]
  beta_mean_v <- vapply(split(abs(res$imp), var_name), mean, numeric(1))
  n_rules_v   <- vapply(split(res$imp,        var_name), length, integer(1))

  # Factor levels = imp-descending
  ord_names <- names(sort(beta_mean_v, decreasing = TRUE))

  resolved_cutoff <- if (is.null(cutoff)) mean(beta_mean_v) else as.numeric(cutoff)

  out <- data.frame(
    variable  = factor(ord_names, levels = ord_names),
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

#' @keywords internal
.gg_beta_varpro_class <- function(object, b, cutoff, which_class, beta_fit,
                                  use_cv) {
  # Class levels — prefer y.org (varPro preserves original factor names there;
  # object$y may have been internally relabelled to 0/1 for binary fits).
  class_levels <- NULL
  if (!is.null(object$y.org) && is.factor(object$y.org)) {
    class_levels <- levels(object$y.org)
  }
  if (is.null(class_levels) && is.factor(object$y)) {
    class_levels <- levels(object$y)
  }
  if (is.null(class_levels) && !is.null(attr(object$y, "levels"))) {
    class_levels <- attr(object$y, "levels")
  }
  if (is.null(class_levels)) {
    class_levels <- sort(unique(as.character(object$y)))
  }
  K <- length(class_levels)
  imp_cols <- paste0("imp.", seq_len(K))

  # Validate which_class
  if (!is.null(which_class)) {
    if (!which_class %in% class_levels) {
      stop(sprintf(
        "gg_beta_varpro: which_class = '%s' is not a level of the response. Levels: %s.",
        which_class, paste(class_levels, collapse = ", ")
      ), call. = FALSE)
    }
  } else if (K == 2L) {
    which_class <- class_levels[K]   # binary default = last (positive class)
  }

  res <- b$results
  var_name <- b$xvar.names[res$variable]

  # Total |imp| per variable (for factor-level ordering)
  res_total <- res[is.finite(res$imp), , drop = FALSE]
  total_var <- b$xvar.names[res_total$variable]
  beta_mean_total <- vapply(split(abs(res_total$imp), total_var), mean, numeric(1))
  ord_names <- names(sort(beta_mean_total, decreasing = TRUE))

  # Per-class aggregation — long format
  rows <- list()
  for (k in seq_len(K)) {
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
      variable  = factor(vars_present, levels = ord_names),
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

  resolved_cutoff <- if (is.null(cutoff)) {
    per_class_mean
  } else if (is.null(names(cutoff))) {
    if (length(cutoff) != 1L) {
      stop("gg_beta_varpro: cutoff must be NULL, scalar, or a named vector with names in class levels.",
           call. = FALSE)
    }
    stats::setNames(rep(as.numeric(cutoff), K), class_levels)
  } else {
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

  long$selected <- mapply(function(bm, cls) bm >= resolved_cutoff[[cls]],
                          long$beta_mean, long$class)

  # Filter to single class if requested
  if (!is.null(which_class)) {
    long <- long[long$class == which_class, , drop = FALSE]
  }

  # Sort: class factor order, then variable factor order
  long$class <- factor(long$class, levels = class_levels)
  long <- long[order(long$class, long$variable), , drop = FALSE]
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

#' @keywords internal
.gg_beta_varpro_empty <- function(fam, which_class, beta_fit, cutoff) {
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
  attr(base, "provenance") <- list(
    source = "varPro::beta.varpro", family = fam,
    n_rules_total = 0L,
    cutoff = if (fam == "regr") stats::setNames(NA_real_, "regr") else NA_real_,
    cutoff_default = is.null(cutoff),
    precomputed = !is.null(beta_fit),
    which_class = which_class
  )
  base
}

#' @importFrom ggplot2 autoplot
#' @export
autoplot.gg_beta_varpro <- function(object, ...) {
  plot.gg_beta_varpro(object, ...)
}
