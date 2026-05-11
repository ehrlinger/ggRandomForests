####**********************************************************************
####  summary.gg_* methods + shared print.summary.gg_* printer.
####
####  Each summary method returns a list of pre-formatted lines plus a few
####  raw numeric fields, classed "summary.gg" so print.summary.gg can
####  format it uniformly.
####**********************************************************************

# Build the standard summary skeleton: header line + body lines.
.summary_skel <- function(x, class_label, body = character(0)) {
  out <- list(
    header = .gg_header(x, class_label),
    body   = body
  )
  class(out) <- c(sprintf("summary.%s", class_label), "summary.gg")
  out
}

#' @export
print.summary.gg <- function(x, ...) {
  cat(x$header, "\n", sep = "")
  for (line in x$body) {
    cat("  ", line, "\n", sep = "")
  }
  invisible(x)
}

#' @export
summary.gg_error <- function(object, ...) {
  err_cols <- setdiff(names(object), c("ntree", "train"))
  final  <- vapply(err_cols, function(c) {
    v <- object[[c]]
    v[length(v)]
  }, numeric(1))
  min_err <- vapply(err_cols, function(c) min(object[[c]], na.rm = TRUE),
                    numeric(1))
  body <- c(
    sprintf("ntree: %d", max(object$ntree, na.rm = TRUE)),
    sprintf("final OOB error: %s",
            paste(sprintf("%s=%.4g", err_cols, final), collapse = ", ")),
    sprintf("min OOB error:   %s",
            paste(sprintf("%s=%.4g", err_cols, min_err), collapse = ", "))
  )
  .summary_skel(object, "gg_error", body)
}

#' @export
summary.gg_vimp <- function(object, ...) {
  top_n <- min(5L, nrow(object))
  top   <- utils::head(object[order(-object$vimp), , drop = FALSE], top_n)
  body  <- c(
    sprintf("variables: %d", nrow(object)),
    sprintf("positive VIMP: %d / negative: %d",
            sum(object$positive, na.rm = TRUE),
            sum(!object$positive, na.rm = TRUE)),
    sprintf("top %d: %s", top_n,
            paste(sprintf("%s (%.4g)", top$vars, top$vimp), collapse = ", "))
  )
  .summary_skel(object, "gg_vimp", body)
}

#' @export
summary.gg_rfsrc <- function(object, ...) {
  body <- c(sprintf("rows: %d, cols: %d", nrow(object), ncol(object)))
  .summary_skel(object, "gg_rfsrc", body)
}

#' @export
summary.gg_variable <- function(object, ...) {
  resp <- intersect(c("yhat", "yvar"), names(object))
  body <- c(
    sprintf("rows: %d", nrow(object)),
    sprintf("response columns: %s",
            if (length(resp) > 0) paste(resp, collapse = ", ") else "(none)")
  )
  .summary_skel(object, "gg_variable", body)
}

# Internal: shared partial-summary body builder.
.partial_body <- function(x) {
  nvar_cont <- if (is.data.frame(x$continuous) && nrow(x$continuous) > 0) {
    length(unique(x$continuous$name))
  } else 0L
  nvar_cat  <- if (is.data.frame(x$categorical) && nrow(x$categorical) > 0) {
    length(unique(x$categorical$name))
  } else 0L
  yhat_rng  <- if (is.data.frame(x$continuous) && nrow(x$continuous) > 0 &&
                   "yhat" %in% names(x$continuous)) {
    sprintf("yhat range: [%.4g, %.4g]",
            min(x$continuous$yhat, na.rm = TRUE),
            max(x$continuous$yhat, na.rm = TRUE))
  } else NULL
  c(sprintf("continuous: %d, categorical: %d", nvar_cont, nvar_cat),
    yhat_rng)
}

.partialpro_body <- function(x) {
  nvar_cont <- if (is.data.frame(x$continuous) && nrow(x$continuous) > 0) {
    length(unique(x$continuous$name))
  } else 0L
  nvar_cat  <- if (is.data.frame(x$categorical) && nrow(x$categorical) > 0) {
    length(unique(x$categorical$name))
  } else 0L
  # gg_partialpro has parametric / nonparametric / causal columns, not yhat.
  rng_lines <- NULL
  if (is.data.frame(x$continuous) && nrow(x$continuous) > 0) {
    for (col in c("parametric", "nonparametric", "causal")) {
      if (col %in% names(x$continuous)) {
        vals <- x$continuous[[col]]
        rng_lines <- c(rng_lines,
          sprintf("%s range: [%.4g, %.4g]",
                  col, min(vals, na.rm = TRUE), max(vals, na.rm = TRUE)))
      }
    }
  }
  c(sprintf("continuous: %d, categorical: %d", nvar_cont, nvar_cat),
    rng_lines)
}

#' @export
summary.gg_partial <- function(object, ...) {
  .summary_skel(object, "gg_partial", .partial_body(object))
}

#' @export
summary.gg_partial_rfsrc <- function(object, ...) {
  .summary_skel(object, "gg_partial_rfsrc", .partial_body(object))
}

#' @export
summary.gg_partialpro <- function(object, ...) {
  .summary_skel(object, "gg_partialpro", .partialpro_body(object))
}

#' @export
summary.gg_roc <- function(object, ...) {
  body <- c(
    sprintf("thresholds: %d", nrow(object)),
    sprintf("AUC: %.4g",
            attr(object, "auc") %||% .gg_auc_trap(object))
  )
  .summary_skel(object, "gg_roc", body)
}

# Trapezoidal AUC from a gg_roc data.frame (fpr / tpr).
.gg_auc_trap <- function(x) {
  if (!all(c("fpr", "tpr") %in% names(x))) return(NA_real_)
  ord <- order(x$fpr)
  fpr <- x$fpr[ord]; tpr <- x$tpr[ord]
  sum((fpr[-1] - fpr[-length(fpr)]) * (tpr[-1] + tpr[-length(tpr)])) / 2
}

#' @export
summary.gg_survival <- function(object, ...) {
  body <- c(
    sprintf("time range: [%.4g, %.4g]",
            min(object$time, na.rm = TRUE),
            max(object$time, na.rm = TRUE)),
    sprintf("rows: %d", nrow(object))
  )
  .summary_skel(object, "gg_survival", body)
}

#' @export
summary.gg_brier <- function(object, ...) {
  crps <- attr(object, "crps_integrated")
  envelope_mean <- mean(object$bs.upper - object$bs.lower, na.rm = TRUE)
  body <- c(
    sprintf("time range: [%.4g, %.4g]",
            min(object$time, na.rm = TRUE),
            max(object$time, na.rm = TRUE)),
    sprintf("peak Brier: %.4g at time %.4g",
            max(object$brier, na.rm = TRUE),
            object$time[which.max(object$brier)]),
    sprintf("integrated CRPS: %s",
            if (is.null(crps)) "NA" else sprintf("%.4g", crps)),
    sprintf("mean 15-85%% envelope width: %.4g", envelope_mean)
  )
  .summary_skel(object, "gg_brier", body)
}
