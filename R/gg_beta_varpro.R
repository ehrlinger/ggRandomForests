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
gg_beta_varpro <- function(object, ..., cutoff = NULL, beta_fit = NULL) {
  UseMethod("gg_beta_varpro", object)
}

#' @export
gg_beta_varpro.varpro <- function(object, ..., cutoff = NULL, beta_fit = NULL) {
  if (!inherits(object, "varpro")) {
    stop("gg_beta_varpro: expected a 'varpro' object from varPro::varpro().",
         call. = FALSE)
  }
  fam <- object$family
  if (!identical(fam, "regr")) {
    stop(sprintf(
      paste0("gg_beta_varpro currently supports varpro regression forests ",
             "only; got family = '%s'. Classification, regr+, and survival ",
             "are tracked under Phase 4d (see vignette / NEWS)."),
      fam
    ), call. = FALSE)
  }

  if (is.null(beta_fit)) {
    b <- varPro::beta.varpro(object, ...)
  } else {
    required_cols <- c("tree", "branch", "variable", "n.oob", "imp")
    if (!inherits(beta_fit, "varpro") ||
        !is.data.frame(beta_fit$results)) {
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

  if (is.null(b)) {
    out <- data.frame(
      variable  = character(0),
      beta_mean = numeric(0),
      n_rules   = integer(0),
      selected  = logical(0),
      stringsAsFactors = FALSE
    )
    class(out) <- c("gg_beta_varpro", "data.frame")
    attr(out, "provenance") <- list(
      source = "varPro::beta.varpro", family = fam,
      n_rules_total = 0L, cutoff = NA_real_, cutoff_default = is.null(cutoff),
      precomputed = !is.null(beta_fit)
    )
    return(out)
  }

  res <- b$results
  res <- res[is.finite(res$imp), , drop = FALSE]
  n_rules_total <- nrow(b$results)
  n_rules_nonzero <- sum(abs(res$imp) > 0)

  if (nrow(res) == 0L) {
    out <- data.frame(
      variable  = character(0),
      beta_mean = numeric(0),
      n_rules   = integer(0),
      selected  = logical(0),
      stringsAsFactors = FALSE
    )
    class(out) <- c("gg_beta_varpro", "data.frame")
    attr(out, "provenance") <- list(
      source          = "varPro::beta.varpro",
      family          = fam,
      ntree           = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
      cutoff          = NA_real_,
      cutoff_default  = is.null(cutoff),
      use.cv          = isTRUE(list(...)$use.cv),
      n_rules_total   = n_rules_total,
      n_rules_nonzero = n_rules_nonzero,
      precomputed     = !is.null(beta_fit),
      xvar.names      = b$xvar.names
    )
    return(out)
  }

  var_idx <- res$variable
  var_name <- b$xvar.names[var_idx]
  agg <- stats::aggregate(
    list(beta_mean = abs(res$imp), n_rules = rep(1L, nrow(res))),
    by = list(variable = var_name),
    FUN = function(x) if (is.logical(x[1L])) sum(x) else sum(x)
  )
  # aggregate above gives sum; convert to mean for beta_mean
  agg$beta_mean <- vapply(
    split(abs(res$imp), var_name),
    mean, numeric(1)
  )[agg$variable]
  agg$n_rules <- as.integer(agg$n_rules)

  resolved_cutoff <- if (is.null(cutoff)) mean(agg$beta_mean) else as.numeric(cutoff)
  agg$selected <- agg$beta_mean >= resolved_cutoff

  agg <- agg[order(-agg$beta_mean), , drop = FALSE]
  rownames(agg) <- NULL

  class(agg) <- c("gg_beta_varpro", "data.frame")
  attr(agg, "provenance") <- list(
    source          = "varPro::beta.varpro",
    family          = fam,
    ntree           = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
    cutoff          = resolved_cutoff,
    cutoff_default  = is.null(cutoff),
    use.cv          = isTRUE(list(...)$use.cv),
    n_rules_total   = n_rules_total,
    n_rules_nonzero = n_rules_nonzero,
    precomputed     = !is.null(beta_fit),
    xvar.names      = b$xvar.names
  )
  agg
}

#' @export
print.gg_beta_varpro <- function(x, ...) {
  prov <- attr(x, "provenance")
  cat("gg_beta_varpro (varPro::beta.varpro)\n")
  cat("  family        :", prov$family %||% NA, "\n")
  cat("  variables     :", nrow(x), "\n")
  cat("  n_rules_total :", prov$n_rules_total %||% NA, "\n")
  cat("  cutoff        :", format(prov$cutoff, digits = 4),
      if (isTRUE(prov$cutoff_default)) " (default)" else "", "\n", sep = "")
  cat("  precomputed   :", isTRUE(prov$precomputed), "\n")
  cat("Use head(x) to see rows.\n")
  invisible(x)
}

#' @export
summary.gg_beta_varpro <- function(object, ...) {
  v <- object$beta_mean
  names(v) <- as.character(object$variable)
  v <- sort(v, decreasing = TRUE)
  structure(v,
            n_rules  = stats::setNames(object$n_rules, as.character(object$variable))[names(v)],
            class    = "summary.gg_beta_varpro")
}

#' @export
print.summary.gg_beta_varpro <- function(x, ...) {
  cat("Mean |beta| per variable (descending):\n")
  print(unclass(x))
  cat("\nRule counts:\n")
  print(attr(x, "n_rules"))
  invisible(x)
}

#' @importFrom ggplot2 autoplot
#' @export
autoplot.gg_beta_varpro <- function(object, ...) {
  plot.gg_beta_varpro(object, ...)
}
