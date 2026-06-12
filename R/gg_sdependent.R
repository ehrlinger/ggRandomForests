##=============================================================================
#' Signal-variable detection from an unsupervised varPro fit
#'
#' Tidy wrapper around [varPro::sdependent()] for a `uvarpro` object. Where
#' [gg_udependent()] draws the cross-variable dependency *graph*,
#' `gg_sdependent()` surfaces `sdependent()`'s *signal-variable detection*: a
#' ranked table of the per-variable signal score and graph degree, with the
#' variables flagged as "signal" (those whose dependency structure clears the
#' detection threshold).
#'
#' @details
#' `sdependent()` runs on the `varPro::get.beta.entropy()` lasso-coefficient
#' matrix and returns, with `plot = FALSE`, a list of `imp.score` (per-variable
#' signal score), `degree` (node degree in the dependency graph), and
#' `signal.vars` (the detected signal set). This wrapper tidies that into one
#' row per candidate variable, ranked by `imp.score`. Because the entropy
#' matrix is the expensive part, `beta_fit` accepts a precomputed
#' [varPro::get.beta.entropy()] matrix (shared with [gg_beta_uvarpro()] and
#' [gg_udependent()]).
#'
#' @param object A `uvarpro` object from [varPro::uvarpro()].
#' @param ... Forwarded to [varPro::get.beta.entropy()] when `beta_fit = NULL`;
#'   ignored, with a warning, when `beta_fit` is supplied.
#' @param threshold,q.signal,directed,min.degree Passed to
#'   [varPro::sdependent()] (defaults match [gg_udependent()]).
#' @param beta_fit Optional precomputed [varPro::get.beta.entropy()] matrix.
#'
#' @return A `gg_sdependent` object (a `data.frame`), one row per candidate
#'   variable, most-signal first, with columns:
#'   \describe{
#'     \item{`variable`}{factor; levels reversed so the top variable lands at
#'       the top after `coord_flip()`.}
#'     \item{`imp_score`}{`sdependent()` per-variable signal score.}
#'     \item{`degree`}{node degree in the dependency graph.}
#'     \item{`signal`}{logical; variable is in `sdependent()$signal.vars`.}
#'   }
#'   The `provenance` attribute records `source`, `family` (`"unsupv"`),
#'   `threshold`, `q.signal`, `directed`, `n_signal`, and `n_var`.
#'
#' @seealso [gg_udependent()] (the dependency graph), [gg_beta_uvarpro()]
#'   (lasso importance), [varPro::sdependent()], [varPro::uvarpro()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   o <- varPro::uvarpro(mtcars, ntree = 50)
#'   gg <- gg_sdependent(o)
#'   plot(gg)
#' }
#' }
#'
#' @export
gg_sdependent <- function(object, ..., threshold = 0.25, q.signal = 0.75,
                          directed = TRUE, min.degree = NULL, beta_fit = NULL) {
  UseMethod("gg_sdependent", object)
}

#' @export
gg_sdependent.default <- function(object, ...) {
  stop("gg_sdependent: expected a 'uvarpro' object from varPro::uvarpro(); ",
       "got an object of class ", paste(class(object), collapse = "/"), ".",
       call. = FALSE)
}

#' @export
gg_sdependent.uvarpro <- function(object, ..., threshold = 0.25,
                                  q.signal = 0.75, directed = TRUE,
                                  min.degree = NULL, beta_fit = NULL) {
  if (!inherits(object, "uvarpro")) {
    stop("gg_sdependent: expected a 'uvarpro' object from varPro::uvarpro().",
         call. = FALSE)
  }

  if (is.null(beta_fit)) {
    imp_mat <- varPro::get.beta.entropy(object, ...)
  } else {
    .validate_beta_uvarpro(beta_fit)
    if (length(list(...)) > 0L) {
      warning("gg_sdependent: arguments in '...' ignored because beta_fit is supplied.",
              call. = FALSE)
    }
    imp_mat <- beta_fit
  }

  prov <- list(
    source    = "varPro::sdependent",
    family    = "unsupv",
    ntree     = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
    threshold = threshold,
    q.signal  = q.signal,
    directed  = isTRUE(directed),
    precomputed = !is.null(beta_fit)
  )

  if (is.null(imp_mat) || !is.matrix(imp_mat) || nrow(imp_mat) == 0L ||
        ncol(imp_mat) == 0L) {
    return(.gg_sdependent_empty(prov))
  }
  .gg_sdependent_build(imp_mat, threshold, q.signal, directed, min.degree, prov)
}

#' @noRd
.gg_sdependent_build <- function(imp_mat, threshold, q.signal, directed,
                                 min.degree, prov) {
  s <- varPro::sdependent(imp_mat, threshold = threshold, q.signal = q.signal,
                          directed = directed, min.degree = min.degree,
                          plot = FALSE)

  # sdependent() may return a character message when no graph is found.
  if (is.character(s) || is.null(s$imp.score) || length(s$imp.score) == 0L) {
    return(.gg_sdependent_empty(prov))
  }

  imp_score  <- s$imp.score
  vars       <- names(imp_score) %||% as.character(seq_along(imp_score))
  degree     <- if (!is.null(s$degree)) s$degree[vars] else rep(NA_real_, length(vars))
  signal_set <- s$signal.vars %||% character(0)

  ord      <- order(imp_score, decreasing = TRUE)
  ord_vars <- vars[ord]

  out <- data.frame(
    variable  = factor(ord_vars, levels = rev(ord_vars)),
    imp_score = unname(imp_score[ord]),
    degree    = unname(as.numeric(degree)[ord]),
    signal    = ord_vars %in% signal_set,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL

  class(out) <- c("gg_sdependent", "data.frame")
  prov$n_var    <- nrow(out)
  prov$n_signal <- sum(out$signal)
  attr(out, "provenance") <- prov
  out
}

#' @noRd
.gg_sdependent_empty <- function(prov) {
  out <- data.frame(
    variable  = factor(character(0)),
    imp_score = numeric(0),
    degree    = numeric(0),
    signal    = logical(0),
    stringsAsFactors = FALSE
  )
  class(out) <- c("gg_sdependent", "data.frame")
  prov$n_var    <- 0L
  prov$n_signal <- 0L
  attr(out, "provenance") <- prov
  out
}

#' @rdname print.gg
#' @export
print.gg_sdependent <- function(x, ...) {
  prov     <- attr(x, "provenance")
  n_signal <- if (!is.null(prov)) prov$n_signal %||% sum(x$signal) else sum(x$signal)
  thr      <- if (!is.null(prov)) prov$threshold %||% NA_real_ else NA_real_
  cat(.gg_header(x, "gg_sdependent"),
      sprintf("  |  threshold: %.3g", thr),
      sprintf("  |  precomputed: %s",
              isTRUE(if (!is.null(prov)) prov$precomputed else FALSE)),
      "\n",
      sprintf("  %d of %d variables flagged as signal\n", n_signal, nrow(x)),
      sep = "")
  invisible(x)
}

#' @rdname summary.gg
#' @export
summary.gg_sdependent <- function(object, ...) {
  sig <- as.character(object$variable[object$signal])
  body <- c(
    sprintf("variables: %d  (signal: %d)", nrow(object), sum(object$signal)),
    sprintf("signal variables: %s",
            if (length(sig)) paste(sig, collapse = ", ") else "(none)")
  )
  .summary_skel(object, "gg_sdependent", body)
}

#' @importFrom ggplot2 autoplot
#' @export
autoplot.gg_sdependent <- function(object, ...) {
  plot.gg_sdependent(object, ...)
}
