##=============================================================================
##=============================================================================
#' Split varpro partial dependence data into continuous or categorical datasets
#' (deprecated)
#'
#' \lifecycle{deprecated}
#'
#' \code{gg_partialpro()} has been renamed to
#' \code{\link{gg_partial_varpro}()}.  This function is a thin alias that
#' will be removed in the release after \pkg{ggRandomForests} v2.8.0.
#'
#' @param part_dta Passed to \code{\link{gg_partial_varpro}}.
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
                           nvars     = NULL,
                           cat_limit = 10,
                           model     = NULL) {
  .Deprecated(
    new = "gg_partial_varpro",
    msg = paste0("'gg_partialpro()' is deprecated. ",
                 "Use 'gg_partial_varpro()' instead.")
  )
  gg_partial_varpro(part_dta = part_dta,
                    nvars     = nvars,
                    cat_limit = cat_limit,
                    model     = model)
}
