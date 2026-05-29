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

#' Summary methods for gg_* data objects
#'
#' Where \code{print} gives you a one-line header, \code{summary} digs a level
#' deeper. Each \code{summary.gg_*()} method returns a \code{summary.gg}
#' object: a header line plus a few diagnostic statistics for that object
#' type (the OOB error curve, the top VIMP variables, a time range, the
#' integrated CRPS, and so on). \code{print.summary.gg()} renders it to the
#' console.
#'
#' @param object A \code{gg_*} data object.
#' @param x A \code{summary.gg} object (for \code{print.summary.gg}).
#' @param ... Not currently used.
#'
#' @return A \code{summary.gg} object: a list with \code{header} and
#'   \code{body} character vectors. \code{print.summary.gg} returns it
#'   invisibly.
#'
#' @seealso \code{\link{print.gg}}, \code{\link{autoplot.gg}}
#'
#' @examples
#' set.seed(42)
#' airq <- na.omit(airquality)
#' rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50)
#' summary(gg_error(rf))
#' summary(gg_vimp(rf))
#'
#' @name summary.gg
NULL

#' @rdname summary.gg
#' @export
print.summary.gg <- function(x, ...) {
  cat(x$header, "\n", sep = "")
  for (line in x$body) {
    cat("  ", line, "\n", sep = "")
  }
  invisible(x)
}

#' @rdname summary.gg
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

#' @rdname summary.gg
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

#' @rdname summary.gg
#' @export
summary.gg_rfsrc <- function(object, ...) {
  body <- c(sprintf("rows: %d, cols: %d", nrow(object), ncol(object)))
  .summary_skel(object, "gg_rfsrc", body)
}

#' @rdname summary.gg
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
  } else {
    0L
  }
  nvar_cat  <- if (is.data.frame(x$categorical) && nrow(x$categorical) > 0) {
    length(unique(x$categorical$name))
  } else {
    0L
  }
  yhat_rng  <- if (is.data.frame(x$continuous) && nrow(x$continuous) > 0 &&
                   "yhat" %in% names(x$continuous)) {
    sprintf("yhat range: [%.4g, %.4g]",
            min(x$continuous$yhat, na.rm = TRUE),
            max(x$continuous$yhat, na.rm = TRUE))
  } else {
    NULL
  }
  c(sprintf("continuous: %d, categorical: %d", nvar_cont, nvar_cat),
    yhat_rng)
}

.partialpro_body <- function(x) {
  nvar_cont <- if (is.data.frame(x$continuous) && nrow(x$continuous) > 0) {
    length(unique(x$continuous$name))
  } else {
    0L
  }
  nvar_cat  <- if (is.data.frame(x$categorical) && nrow(x$categorical) > 0) {
    length(unique(x$categorical$name))
  } else {
    0L
  }
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

#' @rdname summary.gg
#' @export
summary.gg_partial <- function(object, ...) {
  .summary_skel(object, "gg_partial", .partial_body(object))
}

#' @rdname summary.gg
#' @export
summary.gg_partial_rfsrc <- function(object, ...) {
  .summary_skel(object, "gg_partial_rfsrc", .partial_body(object))
}

#' @rdname summary.gg
#' @export
summary.gg_partialpro <- function(object, ...) {
  ## Deprecated-class shim.
  class(object) <- c("gg_partial_varpro",
                      setdiff(class(object), "gg_partialpro"))
  summary.gg_partial_varpro(object, ...)
}

#' @rdname summary.gg
#' @export
summary.gg_partial_varpro <- function(object, ...) {
  .summary_skel(object, "gg_partial_varpro", .partialpro_body(object))
}

#' @rdname summary.gg
#' @export
summary.gg_roc <- function(object, ...) {
  auc <- attr(object, "auc") %||% .gg_auc_trap(object)
  if ("class" %in% names(object)) {
    # per_class = TRUE path: named AUC vector, one entry per class
    n_cls   <- nlevels(object$class)
    auc_str <- paste(sprintf("%s=%.4g", names(auc), auc), collapse = ", ")
    body <- c(sprintf("classes: %d", n_cls),
              sprintf("AUC: %s", auc_str))
  } else {
    body <- c(
      sprintf("thresholds: %d", nrow(object)),
      sprintf("AUC: %.4g", auc)
    )
  }
  .summary_skel(object, "gg_roc", body)
}

# Trapezoidal AUC from a gg_roc data.frame (fpr / tpr).
.gg_auc_trap <- function(x) {
  if (!all(c("fpr", "tpr") %in% names(x))) return(NA_real_)
  ord <- order(x$fpr)
  fpr <- x$fpr[ord]
  tpr <- x$tpr[ord]
  sum((fpr[-1] - fpr[-length(fpr)]) * (tpr[-1] + tpr[-length(tpr)])) / 2
}

#' @rdname summary.gg
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

.varpro_body <- function(x) {
  prov   <- attr(x, "provenance")
  cutoff <- if (!is.null(prov)) prov$cutoff %||% 0.79 else 0.79
  n_sel  <- sum(x$imp$selected, na.rm = TRUE)
  top_n  <- min(5L, nrow(x$stats))
  top_df <- x$stats[order(-x$stats$median), ][seq_len(top_n), ]
  c(
    sprintf("variables: %d total, %d selected (z > %.2g)",
            nrow(x$imp), n_sel, cutoff),
    sprintf("top %d by median z: %s", top_n,
            paste(sprintf("%s (%.3g)",
                          as.character(top_df$variable),
                          top_df$median),
                  collapse = ", ")),
    sprintf("z range: [%.3g, %.3g]",
            min(x$imp$z, na.rm = TRUE),
            max(x$imp$z, na.rm = TRUE))
  )
}

#' @rdname summary.gg
#' @export
summary.gg_varpro <- function(object, ...) {
  .summary_skel(object, "gg_varpro", .varpro_body(object))
}

#' @rdname summary.gg
#' @export
summary.gg_udependent <- function(object, ...) {
  prov <- attr(object, "provenance")
  n    <- if (!is.null(prov)) prov$n else NA
  p    <- if (!is.null(prov)) length(prov$xvar.names) else NA
  thr  <- if (!is.null(prov)) prov$threshold else NA
  body <- c(
    sprintf("Edges: %d  Nodes: %d  Selected: %d/%d",
            nrow(object$edges), nrow(object$nodes),
            sum(object$nodes$selected, na.rm = TRUE), nrow(object$nodes)),
    sprintf("n = %s  p = %s  threshold = %s", n, p, thr)
  )
  .summary_skel(object, "gg_udependent", body)
}

#' @rdname summary.gg
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

#' @rdname summary.gg
#' @export
summary.gg_rhf <- function(object, ...) {
  agg <- stats::aggregate(
    cbind(hazard, chf) ~ time, data = as.data.frame(object),
    FUN = function(v) mean(v, na.rm = TRUE)
  )
  names(agg) <- c("time", "hazard.mean", "chf.mean")
  agg
}

#' @rdname summary.gg
#' @export
summary.gg_isopro <- function(object, ...) {
  q  <- stats::quantile(object$howbad, probs = c(0.05, 0.50, 0.95),
                        na.rm = TRUE, names = FALSE)
  body <- c(
    sprintf("rows: %d", nrow(object)),
    sprintf("howbad range: [%.4g, %.4g]",
            min(object$howbad, na.rm = TRUE),
            max(object$howbad, na.rm = TRUE)),
    sprintf("howbad quantiles (5%%/50%%/95%%): %.4g / %.4g / %.4g",
            q[1], q[2], q[3])
  )
  .summary_skel(object, "gg_isopro", body)
}

#' @rdname summary.gg
#' @export
summary.gg_beta_varpro <- function(object, ...) {
  prov   <- attr(object, "provenance")
  family <- if (!is.null(prov)) prov$family %||% "regr" else "regr"

  if (identical(family, "class") && "class" %in% names(object)) {
    per_class <- split(object, object$class, drop = TRUE)
    by_class  <- lapply(per_class, function(df) {
      v <- df$beta_mean
      names(v) <- as.character(df$variable)
      v <- sort(v, decreasing = TRUE)
      structure(v,
                n_rules = stats::setNames(df$n_rules, as.character(df$variable))[names(v)])
    })
    structure(by_class, class = "summary.gg_beta_varpro")
  } else {
    v <- object$beta_mean
    names(v) <- as.character(object$variable)
    v <- sort(v, decreasing = TRUE)
    structure(v,
              n_rules = stats::setNames(object$n_rules,
                                        as.character(object$variable))[names(v)],
              class   = "summary.gg_beta_varpro")
  }
}

#' @rdname summary.gg
#' @export
summary.gg_ivarpro <- function(object, ...) {
  prov   <- attr(object, "provenance")
  family <- if (!is.null(prov)) prov$family %||% "regr" else "regr"

  per_var <- function(df) {
    v <- tapply(abs(df$local_imp), df$variable, mean, na.rm = TRUE)
    v <- v[!is.na(v)]
    v <- sort(v, decreasing = TRUE)
    n_obs <- tapply(df$obs, df$variable,
                    function(o) length(unique(o)))[names(v)]
    structure(v, n_obs = n_obs)
  }

  if (identical(family, "class") && "class" %in% names(object)) {
    per_class <- split(object, object$class, drop = TRUE)
    by_class  <- lapply(per_class, per_var)
    structure(by_class, class = "summary.gg_ivarpro")
  } else {
    structure(per_var(object), class = "summary.gg_ivarpro")
  }
}
