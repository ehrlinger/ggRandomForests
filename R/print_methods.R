####**********************************************************************
####  print.gg_* methods.
####
####  All print methods follow the same shape: emit a one-line header
####  (class label + provenance) and return the object invisibly. They do
####  NOT print rows — gg_* objects inherit data.frame (or list), so
####  head() and as.data.frame() remain available for inspecting contents.
####**********************************************************************

#' Print methods for gg_* data objects
#'
#' Each \code{print.gg_*()} method prints a one-line header: the class label
#' and, where the forest recorded it, provenance (source package, family,
#' ntree, n). It returns the object invisibly, so \code{print()} sits cleanly
#' in a pipe.
#'
#' To see the rows themselves, use \code{head()}; for per-class diagnostics,
#' use \code{\link{summary.gg}}.
#'
#' @param x A \code{gg_*} data object.
#' @param ... Not currently used.
#'
#' @return The object \code{x}, invisibly.
#'
#' @seealso \code{\link{summary.gg}}, \code{\link{autoplot.gg}}
#'
#' @examples
#' set.seed(42)
#' airq <- na.omit(airquality)
#' rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50)
#' print(gg_error(rf))
#' print(gg_vimp(rf))
#'
#' @name print.gg
NULL

#' @rdname print.gg
#' @export
print.gg_error <- function(x, ...) {
  cat(.gg_header(x, "gg_error"), "\n", sep = "")
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_vimp <- function(x, ...) {
  cat(.gg_header(x, "gg_vimp"),
      sprintf("  |  variables: %d", nrow(x)),
      "\n", sep = "")
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_rfsrc <- function(x, ...) {
  cat(.gg_header(x, "gg_rfsrc"), "\n", sep = "")
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_variable <- function(x, ...) {
  cat(.gg_header(x, "gg_variable"), "\n", sep = "")
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_partial <- function(x, ...) {
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
  cat(.gg_header(x, "gg_partial"),
      sprintf("  |  continuous: %d, categorical: %d", nvar_cont, nvar_cat),
      "\n", sep = "")
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_partial_rfsrc <- function(x, ...) {
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
  cat(.gg_header(x, "gg_partial_rfsrc"),
      sprintf("  |  continuous: %d, categorical: %d", nvar_cont, nvar_cat),
      "\n", sep = "")
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_partialpro <- function(x, ...) {
  ## Deprecated-class shim: re-dispatch to print.gg_partial_varpro.
  class(x) <- c("gg_partial_varpro", setdiff(class(x), "gg_partialpro"))
  print.gg_partial_varpro(x, ...)
}

#' @rdname print.gg
#' @export
print.gg_partial_varpro <- function(x, ...) {
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
  prov  <- attr(x, "provenance")
  scale <- if (!is.null(prov)) prov$scale %||% NA_character_ else NA_character_
  cat(.gg_header(x, "gg_partial_varpro"),
      sprintf("  |  continuous: %d, categorical: %d", nvar_cont, nvar_cat),
      if (!is.na(scale)) sprintf("  |  scale: %s", scale) else "",
      "\n", sep = "")
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_roc <- function(x, ...) {
  cat(.gg_header(x, "gg_roc"), "\n", sep = "")
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_survival <- function(x, ...) {
  cat(.gg_header(x, "gg_survival"), "\n", sep = "")
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_brier <- function(x, ...) {
  crps <- attr(x, "crps_integrated")
  suffix <- if (!is.null(crps)) {
    sprintf("  |  integrated CRPS: %.4g", crps)
  } else {
    ""
  }
  cat(.gg_header(x, "gg_brier"), suffix, "\n", sep = "")
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_rhf <- function(x, ...) {
  cat(.gg_header(x, "gg_rhf"),
      sprintf("  |  cases: %d  times: %d  source: %s",
              length(unique(x$id)),
              attr(x, "ntime") %||% length(unique(x$time)),
              x$source[1]),
      "\n", sep = "")
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_udependent <- function(x, ...) {
  prov <- attr(x, "provenance")
  n    <- if (!is.null(prov)) prov$n else "?"
  p    <- if (!is.null(prov)) length(prov$xvar.names) else "?"
  thr  <- if (!is.null(prov)) prov$threshold else "?"
  cat(sprintf("<gg_udependent>  n=%s  p=%s  threshold=%s\n", n, p, thr))
  cat(sprintf(
    "  Edges: %d  Nodes in graph: %d  Selected: %d/%d\n",
    nrow(x$edges), nrow(x$nodes),
    sum(x$nodes$selected, na.rm = TRUE), nrow(x$nodes)
  ))
  invisible(x)
}

#' @rdname print.gg
#' @export
print.summary.gg_udependent <- function(x, ...) {
  ## summary.gg_udependent returns a summary.gg skeleton; delegate to
  ## print.summary.gg which renders header + body lines.
  NextMethod()
}

#' @rdname print.gg
#' @export
print.gg_varpro <- function(x, ...) {
  prov     <- attr(x, "provenance")
  cutoff   <- if (!is.null(prov)) prov$cutoff   %||% 0.79 else 0.79
  faithful <- if (!is.null(prov)) prov$faithful  %||% FALSE else FALSE
  family   <- if (!is.null(prov)) prov$family    %||% NA_character_ else NA_character_
  n_total  <- nrow(x$imp)
  n_sel    <- sum(x$imp$selected, na.rm = TRUE)
  cat(.gg_header(x, "gg_varpro"),
      sprintf("  |  family: %s", family),
      sprintf("  |  cutoff: %.2g", cutoff),
      sprintf("  |  faithful: %s", faithful),
      "\n",
      sprintf("  %d of %d variables selected (z > %.2g)\n",
              n_sel, n_total, cutoff),
      sep = "")
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_isopro <- function(x, ...) {
  cat(.gg_header(x, "gg_isopro"), "\n", sep = "")
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_beta_varpro <- function(x, ...) {
  prov           <- attr(x, "provenance")
  family         <- if (!is.null(prov)) prov$family %||% NA_character_ else NA_character_
  precomputed    <- isTRUE(if (!is.null(prov)) prov$precomputed else FALSE)
  n_total        <- if (!is.null(prov)) prov$n_rules_total  %||% NA_integer_ else NA_integer_
  n_nonzero      <- if (!is.null(prov)) prov$n_rules_nonzero %||% NA_integer_ else NA_integer_
  n_sel          <- sum(x$selected, na.rm = TRUE)

  if (identical(family, "class") && "class" %in% names(x)) {
    n_classes   <- length(unique(x$class))
    which_class <- if (!is.null(prov)) prov$which_class else NULL
    view_str <- if (is.null(which_class)) "faceted" else sprintf("which_class: %s", which_class)
    cat(.gg_header(x, "gg_beta_varpro"),
        sprintf("  |  n_classes: %d  |  view: %s", n_classes, view_str),
        sprintf("  |  precomputed: %s", precomputed),
        "\n",
        sprintf("  %d of %d (variable, class) pairs selected; %d / %d rules with non-zero beta\n",
                n_sel, nrow(x), n_nonzero, n_total),
        sep = "")
  } else {
    cutoff         <- if (!is.null(prov)) prov$cutoff %||% NA_real_ else NA_real_
    cutoff_val     <- if (length(cutoff) >= 1L) cutoff[[1]] else NA_real_
    cutoff_default <- isTRUE(if (!is.null(prov)) prov$cutoff_default else FALSE)
    cat(.gg_header(x, "gg_beta_varpro"),
        sprintf("  |  cutoff: %.4g%s", cutoff_val,
                if (cutoff_default) " (default)" else ""),
        sprintf("  |  precomputed: %s", precomputed),
        "\n",
        sprintf("  %d of %d variables selected; %d / %d rules with non-zero beta\n",
                n_sel, nrow(x), n_nonzero, n_total),
        sep = "")
  }
  invisible(x)
}

#' @export
print.summary.gg_beta_varpro <- function(x, ...) {
  if (is.list(unclass(x)) && !is.numeric(unclass(x))) {
    # Classification: list of per-class summaries
    for (cls in names(x)) {
      cat(sprintf("Class '%s' -- mean |beta| per variable (descending):\n", cls))
      print(unclass(x[[cls]]))
      cat("\nRule counts:\n")
      print(attr(x[[cls]], "n_rules"))
      cat("\n")
    }
  } else {
    cat("Mean |beta| per variable (descending):\n")
    print(unclass(x))
    cat("\nRule counts:\n")
    print(attr(x, "n_rules"))
  }
  invisible(x)
}

#' @rdname print.gg
#' @export
print.gg_ivarpro <- function(x, ...) {
  prov        <- attr(x, "provenance")
  precomputed <- isTRUE(if (!is.null(prov)) prov$precomputed else FALSE)
  n_obs       <- if (!is.null(prov)) prov$n_obs %||% NA_integer_ else NA_integer_
  n_var       <- if (!is.null(prov)) prov$n_var %||% NA_integer_ else NA_integer_
  which_obs   <- if (!is.null(prov)) prov$which_obs else NULL
  which_cls   <- if (!is.null(prov)) prov$which_class else NULL
  has_class   <- "class" %in% names(x)

  view <- if (!is.null(which_obs)) sprintf("obs %d", which_obs) else "aggregate"
  cls_part <- if (has_class) {
    n_cls <- length(unique(x$class))
    if (!is.null(which_cls)) {
      sprintf("  |  class: %s", which_cls)
    } else {
      sprintf("  |  %d classes (faceted)", n_cls)
    }
  } else {
    ""
  }

  cat(.gg_header(x, "gg_ivarpro"),
      sprintf("  |  view: %s", view),
      cls_part,
      sprintf("  |  precomputed: %s", precomputed),
      "\n",
      sprintf("  %d (obs x variable) cells; %d unique obs across %d variables\n",
              nrow(x), n_obs, n_var),
      sep = "")
  invisible(x)
}

#' @export
print.summary.gg_ivarpro <- function(x, ...) {
  if (is.list(unclass(x)) && !is.numeric(unclass(x))) {
    for (cls in names(x)) {
      cat(sprintf("Class '%s' - mean |local_imp| per variable (descending):\n", cls))
      print(unclass(x[[cls]]))
      cat("\nObservation counts:\n")
      print(attr(x[[cls]], "n_obs"))
      cat("\n")
    }
  } else {
    cat("Mean |local_imp| per variable (descending):\n")
    print(unclass(x))
    cat("\nObservation counts:\n")
    print(attr(x, "n_obs"))
  }
  invisible(x)
}
