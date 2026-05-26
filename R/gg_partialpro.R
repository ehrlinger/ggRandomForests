##=============================================================================
#' Split varpro partial dependence data into continuous or categorical datasets
#' (deprecated)
#'
#' \emph{Deprecated.} \code{gg_partialpro()} has been superseded by
#' \code{\link{gg_partial_varpro}()} and is now a thin alias for it.  It will
#' be removed in the release after \pkg{ggRandomForests} v2.8.0; use
#' \code{\link{gg_partial_varpro}()} directly.
#'
#' @param part_dta Passed to \code{\link{gg_partial_varpro}}.
#' @param object Passed to \code{\link{gg_partial_varpro}}.
#' @param scale Passed to \code{\link{gg_partial_varpro}}.
#' @param time Passed to \code{\link{gg_partial_varpro}}.
#' @param nvars Passed to \code{\link{gg_partial_varpro}}.
#' @param cat_limit Passed to \code{\link{gg_partial_varpro}}.
#' @param model Passed to \code{\link{gg_partial_varpro}}.
#'
#' @return A \code{gg_partial_varpro} object (see
#'   \code{\link{gg_partial_varpro}}).
#'
#' @seealso \code{\link{gg_partial_varpro}}
#'
#' @rdname gg_partial_varpro
#' @export
gg_partialpro <- function(part_dta,
                          object    = NULL,
                          scale     = c("auto", "rmst", "mortality",
                                        "surv", "chf"),
                          time      = NULL,
                          nvars     = NULL,
                          cat_limit = 10,
                          model     = NULL) {
  .Deprecated(
    new     = "gg_partial_varpro",
    package = "ggRandomForests",
    msg     = paste0("'gg_partialpro()' is deprecated. ",
                     "Use 'gg_partial_varpro()' instead.")
  )
  gg_partial_varpro(part_dta  = part_dta,
                    object    = object,
                    scale     = scale,
                    time      = time,
                    nvars     = nvars,
                    cat_limit = cat_limit,
                    model     = model)
}
