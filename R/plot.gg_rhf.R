##=============================================================================
#' Plot Random Hazard Forest hazard / cumulative-hazard curves
#'
#' Draws case-specific ensemble curves from a [gg_rhf()] object: hazard
#' (default) or cumulative hazard, one line per case selected by `idx`.
#'
#' @param x A `gg_rhf` object from [gg_rhf()].
#' @param idx Integer vector of case ids (matched against the `id` column) to
#'   draw. `NULL` (default) draws every case.
#' @param hazard.only Logical; `TRUE` (default) plots the hazard, `FALSE`
#'   plots the cumulative hazard.
#' @param ... Not currently used.
#'
#' @return A `ggplot` object.
#'
#' @seealso [gg_rhf()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("randomForestRHF", quietly = TRUE)) {
#'   data(pbc, package = "randomForestSRC")
#'   d <- randomForestRHF::convert.counting(
#'     survival::Surv(days, status) ~ ., na.omit(pbc))
#'   o <- randomForestRHF::rhf("Surv(id, start, stop, event) ~ .", d, ntree = 30)
#'   plot(gg_rhf(o), idx = c(1, 5, 10))
#' }
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_bw
#' @name plot.gg_rhf
#' @export
plot.gg_rhf <- function(x, idx = NULL, hazard.only = TRUE, ...) {
  if (!inherits(x, "gg_rhf")) {
    stop("plot.gg_rhf() requires a 'gg_rhf' object.", call. = FALSE)
  }
  ids <- unique(x$id)
  if (is.null(idx)) idx <- ids
  if (!any(idx %in% ids)) {
    stop("none of the requested idx values are present in the gg_rhf id ",
         "column.", call. = FALSE)
  }
  missing_ids <- idx[!idx %in% ids]
  if (length(missing_ids)) {
    warning("idx values not found in gg_rhf and will be ignored: ",
            paste(missing_ids, collapse = ", "), call. = FALSE)
  }
  dta <- x[x$id %in% idx, , drop = FALSE]
  dta$id <- factor(dta$id)

  yvar <- if (hazard.only) "hazard" else "chf"
  ylab <- if (hazard.only) "Hazard" else "Cumulative hazard"

  ggplot2::ggplot(
    dta,
    ggplot2::aes(
      x      = .data[["time"]],
      y      = .data[[yvar]],
      colour = .data[["id"]],
      group  = .data[["id"]]
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Time", y = ylab, colour = "Case") +
    ggplot2::theme_bw()
}
