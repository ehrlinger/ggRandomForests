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
#' Each \code{print.gg_*()} method emits a single-line header containing the
#' class label and, when available, forest provenance metadata (source package,
#' family, ntree, n). The object is returned invisibly so \code{print()} calls
#' chain cleanly in pipes.
#'
#' To inspect rows use \code{head()}.  To retrieve per-class diagnostics use
#' \code{\link{summary.gg}}.
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
  cat(.gg_header(x, "gg_partialpro"),
      sprintf("  |  continuous: %d, categorical: %d", nvar_cont, nvar_cat),
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
