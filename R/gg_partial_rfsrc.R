##=============================================================================
#' Split partial lots into continuous or categorical datasets
#'
#' gg_partial_rfsrc uses the \code{rfsrc::partial.rfsrc} to generate the partial
#' plot data internally. So you provide the \code{rfsrc::rfsrc} model, and the
#' xvar.names to generate the data.
#'
#' @param rf_model \code{rfsrc::rfsrc} model
#' @param xvar.names list(<str>) Which variables to calculate partial plots
#' @param xvar2.name <str> a single grouping feature that is in the newx dataset
#' @param newx a \code{data.frame} containing data to use for the partial plots
#' @param cat_limit Categorical features are build when there are fewer than
#'  cat_limit unique features.
#'
#' @examples
#' ## ------------------------------------------------------------
#' ##
#' ## regression
#' ##
#' ## ------------------------------------------------------------
#'
#' airq.obj <- rfsrc(Ozone ~ ., data = airquality)
#'
#' ## partial effect for wind
#' prt_dta <- gg_partial_rfsrc(airq.obj,
#'                        xvar.names = c("Wind"))
#'
#' @importFrom dplyr mutate filter select all_of
#' @export
gg_partial_rfsrc <- function(rf_model,
                             xvar.names = NULL,
                             xvar2.name = NULL,
                             newx = NULL,
                             cat_limit = 10) {
  # Check the rfsrc type
  # rf_model$family
  
  # we supply new data, make sure we use that and that it is a dataframe...
  if (is.null(newx)) {
    newx = rf_model$xvar
  }
  
  if (sum(colnames(newx) %in% rf_model$xvar.names) != ncol(newx)) {
    return("newx must be a dataframe with the same columns used to train the rfsrc object")
  }
  
  if (!is.null(xvar.names)) {
    if (sum(xvar.names %in% colnames(newx)) != length(xvar.names)) {
      return("xvar.names contains column names not found in the rfsrc object")
    }
  }
  
  if (is.null(xvar2.name)) {
    pdta <- lapply(xvar.names, function(xname) {
      xval <- unlist(newx |>
                       dplyr::select(dplyr::all_of(xname)))
      gr <- length(unique(xval)) < cat_limit
      partial.obj <- randomForestSRC::partial.rfsrc(
        object = rf_model,
        partial.xvar = xname,
        partial.values = xval
      )
      pout <- randomForestSRC::get.partial.plot.data(partial.obj, granule = gr)
      out_dta <- data.frame(x = pout$x, yhat = pout$yhat)
      out_dta$name <- xname
      out_dta$type <- c("continuous", "categorical")[gr + 1]
      if (!is.null(pout$partial.time)) {
        out_dta$time <- pout$partial.time
      }
      return(out_dta)
    })
  } else{
    xv2 <- unique(unlist(newx |>
                           dplyr::select(dplyr::all_of(xvar2.name))))
    pdta <- lapply(xv2, function(x2val) {
      p1dta <- lapply(xvar.names, function(xname) {
        xval <- unlist(newx |>
                         dplyr::select(dplyr::all_of(xname)))
        gr <- length(unique(xval)) < cat_limit
        partial.obj <- randomForestSRC::partial.rfsrc(
          object = rf_model,
          partial.xvar = xname,
          partial.values = xval,
          partial.xvar2 = xvar2.name,
          partial.values2 = x2val
        )
        pout <- randomForestSRC::get.partial.plot.data(partial.obj, granule = gr)
        out_dta <- data.frame(x = pout$x, yhat = pout$yhat)
        out_dta$name <- xname
        out_dta$type <- c("continuous", "categorical")[gr + 1]
        if (!is.null(pout$partial.time)) {
          out_dta$time <- pout$partial.time
        }
        return(out_dta)
      })
      p1dta <- do.call("rbind", p1dta)
      p1dta$grp <- x2val
      return(p1dta)
    })
    
  }
  pdta <- do.call("rbind", pdta)
  continuous <- pdta |> dplyr::filter(.data$type == "continuous") |>
    mutate(x = as.numeric(.data$x)) |> dplyr::select(-"type")
  categorical <- pdta |> dplyr::filter(.data$type == "categorical") |>
    dplyr::select(-"type")
  return(list(continuous = continuous, categorical = categorical))
}