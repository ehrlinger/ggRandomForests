##=============================================================================
#' Split partial lots into continuous or categorical datasets
#'
#' gg_partial_rfsrc uses the \code{rfsrc::partial.rfsrc} to generate the partial
#' plot data internally. So you provide the \code{rfsrc::rfsrc} model, and the
#' xvar.names to generate the data.
#'
#' @param rf_model \code{rfsrc::rfsrc} model
#' @param xvar.names Which variables to calculate partial plots
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
#' @export
gg_partial_rfsrc <- function(rf_model,
                             xvar.names=NULL,
                             newx=NULL, 
                             cat_limit = 10) {
  # Check the rfsrc type
  # rf_model$family
  
  # we supply new data, make sure we use that and that it is a dataframe... 
  if(is.null(newx)){
    newx = rf_model$xvar
  }
  
  if(length(intersect(colnames(newx), 
                         rf_model$xvar.names)) == ncol(newx)){
    return("newx must be a dataframe with the same columns used to train the rfsrc object")
  }
  
  if(!is.null(xvar.names)){
    if(length(intersect(xvar.names, colnames(newx))) == ncol(newx)){
      return("xvar.names contains column names not found in the rfsrc object")
    }
  }
  
  pdta <- lapply(xvar.names, function(xname) {
    xval <- unlist( newx |>
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
  pdta <- do.call("rbind", pdta)
  continuous <- pdta |> dplyr::filter(type == "continuous") |>
    mutate(x = as.numeric(x)) |> dplyr::select(-type)
  categorical <- pdta |> dplyr::filter(type == "categorical")  |>
    dplyr::select(-type)
  return(list(continuous = continuous, categorical = categorical))
}