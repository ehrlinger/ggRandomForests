##=============================================================================
#' Per-variable lasso-beta importance from a varPro fit
#'
#' Tidy wrapper around \code{varPro::beta.varpro()} for the regression
#' family. Aggregates the per-rule lasso coefficient by variable into
#' \code{mean(|beta|)} and flags variables above a scalar cutoff. Optional
#' \code{beta_fit} argument lets callers compute the expensive
#' \code{beta.varpro()} step once and reuse the result.
#'
#' @param object A \code{varpro} fit from \code{\link[varPro]{varpro}}
#'   (regression family).
#' @param ... Forwarded to \code{\link[varPro]{beta.varpro}} when
#'   \code{beta_fit} is \code{NULL}; ignored otherwise (with a warning).
#' @param cutoff Selection threshold on \code{beta_mean}. \code{NULL}
#'   (default) -> \code{mean(beta_mean)}.
#' @param beta_fit Optional pre-computed \code{varPro::beta.varpro()}
#'   result for the same \code{object}. When supplied, the wrapper skips
#'   the expensive lasso fit.
#'
#' @return A \code{data.frame} of class \code{c("gg_beta_varpro", "data.frame")}.
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
