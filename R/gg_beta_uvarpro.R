##=============================================================================
#' Per-variable lasso-beta importance from an unsupervised varPro fit
#'
#' Tidy wrapper around [varPro::get.beta.entropy()] for a `uvarpro` object.
#' Where [gg_beta_varpro()] refines the *supervised* release-rule contrast,
#' `gg_beta_uvarpro()` does the unsupervised analogue: `uvarpro()` builds
#' entropy regions with no response, and `get.beta.entropy()` fits a
#' cross-validated lasso within each region to ask how strongly every other
#' variable explains the released variable. Averaging the absolute lasso
#' coefficients per variable gives one number per variable: an unsupervised,
#' lasso-flavoured importance.
#'
#' @details
#' `get.beta.entropy(o)` returns a (released-variable x variable) numeric
#' matrix of absolute lasso coefficients. The column mean (`na.rm = TRUE`) is
#' the per-variable importance reported here, matching the canonical
#' `sort(colMeans(beta, na.rm = TRUE), decreasing = TRUE)` idiom in the
#' `varPro::uvarpro()` help ("iowa housing - illustrates lasso importance").
#'
#' Because `get.beta.entropy()` is expensive (a cross-validated `glmnet` per
#' region), the `beta_fit` argument accepts a pre-computed matrix so you can
#' iterate on the cutoff without re-fitting. The pairing mirrors the
#' `beta_fit` argument of [gg_beta_varpro()].
#'
#' @param object A `uvarpro` object from [varPro::uvarpro()].
#' @param ... Forwarded to [varPro::get.beta.entropy()] when
#'   `beta_fit = NULL` (e.g. `pre.filter`, `second.stage`, `use.cv`).
#'   Ignored, with a warning, when `beta_fit` is supplied.
#' @param cutoff Selection threshold on `beta_mean`. `NULL` (default) uses
#'   `mean(beta_mean)`; a scalar sets it explicitly. Variables at or above the
#'   cutoff are flagged `selected`.
#' @param beta_fit Optional pre-computed [varPro::get.beta.entropy()] matrix
#'   for `object`. When supplied, must be a numeric matrix with column names
#'   (the variables); `...` is then ignored.
#'
#' @return A `gg_beta_uvarpro` object (a `data.frame`), one row per variable,
#'   most-important first, with columns:
#'   \describe{
#'     \item{`variable`}{factor; levels reversed so the most-important
#'       variable lands at the top after `coord_flip()` (the `gg_vimp`
#'       convention).}
#'     \item{`beta_mean`}{`mean(|lasso beta|)` over the released regions
#'       (`colMeans(beta, na.rm = TRUE)`).}
#'     \item{`n_released`}{number of regions contributing a non-`NA`
#'       coefficient for the variable.}
#'     \item{`selected`}{logical; `beta_mean >= cutoff`.}
#'   }
#'   The `provenance` attribute records `source`, `family` (`"unsupv"`),
#'   `cutoff`, `n_var`, `n_released_regions`, and `precomputed`.
#'
#' @seealso [gg_beta_varpro()] (supervised analogue), [gg_udependent()],
#'   [varPro::get.beta.entropy()], [varPro::uvarpro()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   o <- varPro::uvarpro(mtcars, ntree = 50)
#'   gg <- gg_beta_uvarpro(o)
#'   plot(gg)
#' }
#' }
#'
#' @export
gg_beta_uvarpro <- function(object, ..., cutoff = NULL, beta_fit = NULL) {
  UseMethod("gg_beta_uvarpro", object)
}

#' @export
gg_beta_uvarpro.default <- function(object, ..., cutoff = NULL,
                                    beta_fit = NULL) {
  stop("gg_beta_uvarpro: expected a 'uvarpro' object from varPro::uvarpro(); ",
       "got an object of class ", paste(class(object), collapse = "/"), ".",
       call. = FALSE)
}

#' @export
gg_beta_uvarpro.uvarpro <- function(object, ..., cutoff = NULL,
                                    beta_fit = NULL) {
  if (!inherits(object, "uvarpro")) {
    stop("gg_beta_uvarpro: expected a 'uvarpro' object from varPro::uvarpro().",
         call. = FALSE)
  }

  # Resolve the beta matrix (cache path)
  if (is.null(beta_fit)) {
    b <- varPro::get.beta.entropy(object, ...)
  } else {
    .validate_beta_uvarpro(beta_fit)
    if (length(list(...)) > 0L) {
      warning("gg_beta_uvarpro: arguments in '...' ignored because beta_fit is supplied.",
              call. = FALSE)
    }
    b <- beta_fit
  }

  # Empty fast-path: no regions / no variables survived
  if (is.null(b) || !is.matrix(b) || nrow(b) == 0L || ncol(b) == 0L) {
    return(.gg_beta_uvarpro_empty(object, beta_fit, cutoff))
  }

  beta_mean_v  <- colMeans(b, na.rm = TRUE)
  n_released_v <- colSums(!is.na(b))

  # Most-important first; reverse the factor levels so coord_flip() puts the
  # top variable at the top (matches gg_vimp / gg_beta_varpro).
  ord_names <- names(sort(beta_mean_v, decreasing = TRUE))

  resolved_cutoff <- if (is.null(cutoff)) {
    mean(beta_mean_v, na.rm = TRUE)
  } else {
    as.numeric(cutoff)
  }

  out <- data.frame(
    variable   = factor(ord_names, levels = rev(ord_names)),
    beta_mean  = unname(beta_mean_v[ord_names]),
    n_released = as.integer(unname(n_released_v[ord_names])),
    stringsAsFactors = FALSE
  )
  out$selected <- out$beta_mean >= resolved_cutoff
  rownames(out) <- NULL

  class(out) <- c("gg_beta_uvarpro", "data.frame")
  attr(out, "provenance") <- list(
    source             = "varPro::get.beta.entropy",
    family             = "unsupv",
    ntree              = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
    cutoff             = stats::setNames(resolved_cutoff, "unsupv"),
    cutoff_default     = is.null(cutoff),
    n_var              = ncol(b),
    n_released_regions = nrow(b),
    precomputed        = !is.null(beta_fit),
    xvar.names         = colnames(b)
  )
  out
}

#' @noRd
.validate_beta_uvarpro <- function(beta_fit) {
  if (!is.matrix(beta_fit) || !is.numeric(beta_fit)) {
    stop("gg_beta_uvarpro: beta_fit does not look like a ",
         "varPro::get.beta.entropy() result. Expected a numeric matrix.",
         call. = FALSE)
  }
  if (ncol(beta_fit) > 0L && is.null(colnames(beta_fit))) {
    stop("gg_beta_uvarpro: beta_fit must have column names (the variables). ",
         "varPro::get.beta.entropy() returns a named matrix.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' @noRd
.gg_beta_uvarpro_empty <- function(object, beta_fit, cutoff) {
  out <- data.frame(
    variable   = factor(character(0)),
    beta_mean  = numeric(0),
    n_released = integer(0),
    selected   = logical(0),
    stringsAsFactors = FALSE
  )
  class(out) <- c("gg_beta_uvarpro", "data.frame")
  attr(out, "provenance") <- list(
    source             = "varPro::get.beta.entropy",
    family             = "unsupv",
    ntree              = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
    cutoff             = stats::setNames(if (is.null(cutoff)) NA_real_ else as.numeric(cutoff), "unsupv"),
    cutoff_default     = is.null(cutoff),
    n_var              = 0L,
    n_released_regions = 0L,
    precomputed        = !is.null(beta_fit),
    xvar.names         = character(0)
  )
  out
}
