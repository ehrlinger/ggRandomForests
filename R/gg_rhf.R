##=============================================================================
#' Tidy hazard and cumulative-hazard curves from a Random Hazard Forest
#'
#' Extracts case-specific ensemble hazard and cumulative-hazard estimates from
#' a fitted [randomForestRHF::rhf()] object into a tidy long data frame, one
#' row per (case, time) pair on the forest's `time.interest` grid.
#'
#' @param object A fitted `rhf` object from \pkg{randomForestRHF}.
#' @param source Which ensemble estimate to extract: `"oob"` (default,
#'   out-of-bag) or `"inbag"`. Falls back to the other when the requested one
#'   is absent (e.g. `bootstrap = "none"` has no OOB estimate).
#' @param ... Not currently used.
#'
#' @return A `data.frame` of class `c("gg_rhf", "data.frame")` with columns
#'   `id`, `time`, `hazard`, `chf`, `source`, an integer `ntime` attribute
#'   (number of grid points), and a `provenance` attribute.
#'
#'   The frame always has `ntime` rows per case. `hazard` may be `NA`: from
#'   \pkg{randomForestRHF} 2.0.0 the pointwise hazard is defined only where a
#'   grid point falls inside one of the case's supplied `(start, stop]`
#'   intervals, and is `NA` in gaps and after the final stop time. `chf` is not
#'   masked. It accumulates the exact interval overlap, so it stays flat across
#'   those regions. The values are passed through unchanged; use
#'   `na.rm = TRUE` when summarising `hazard`, or see [plot.gg_rhf()], which
#'   drops the masked cells before drawing.
#'
#' @references
#' Ishwaran H, Hsich EM, Kogalur UB, Lee DKK (2026). Random Hazard Forests.
#' arXiv:2608.21597. \doi{10.48550/arXiv.2608.21597}.
#'
#' Ishwaran H, Kogalur UB (2026). \emph{randomForestRHF: Random Hazard
#' Forests}. R package version 2.0.0.
#' \url{https://CRAN.R-project.org/package=randomForestRHF}.
#'
#' @seealso [plot.gg_rhf()], [randomForestRHF::rhf()]
#'
#' @examples
#' \donttest{
#' if (requireNamespace("randomForestRHF", quietly = TRUE)) {
#'   data(pbc, package = "randomForestSRC")
#'   d <- randomForestRHF::convert.counting(
#'     survival::Surv(days, status) ~ ., na.omit(pbc))
#'   o <- randomForestRHF::rhf("Surv(id, start, stop, event) ~ .", d, ntree = 30)
#'   gg <- gg_rhf(o)
#'   plot(gg, idx = c(1, 5, 10))
#' }
#' }
#'
#' @export
gg_rhf <- function(object, ...) {
  UseMethod("gg_rhf", object)
}

#' @rdname gg_rhf
#' @export
gg_rhf.rhf <- function(object, source = c("oob", "inbag"), ...) {
  if (!inherits(object, "rhf")) {
    stop("gg_rhf() only works on 'rhf' objects from randomForestRHF.",
         call. = FALSE)
  }
  source <- match.arg(source)

  haz <- object[[paste0("hazard.", source)]]
  chf <- object[[paste0("chf.", source)]]
  if (is.null(haz)) {
    alt <- setdiff(c("oob", "inbag"), source)
    haz <- object[[paste0("hazard.", alt)]]
    chf <- object[[paste0("chf.", alt)]]
    if (is.null(haz)) {
      stop("rhf object carries no hazard estimates for source = '", source,
           "'.", call. = FALSE)
    }
    source <- alt
  }

  time   <- object$time.interest
  ids    <- object$ensemble.id %||% seq_len(nrow(haz))
  n_case <- nrow(haz)
  n_time <- length(time)

  # haz / chf are n_case x n_time; as.vector() reads column-major (time-major),
  # so id repeats within each time block and time repeats across cases.
  gg_dta <- data.frame(
    id     = rep(ids, times = n_time),
    time   = rep(time, each = n_case),
    hazard = as.vector(haz),
    chf    = as.vector(chf),
    source = source,
    stringsAsFactors = FALSE
  )

  attr(gg_dta, "ntime") <- n_time
  class(gg_dta) <- c("gg_rhf", class(gg_dta))
  gg_dta <- .set_provenance(gg_dta, object)
  invisible(gg_dta)
}
