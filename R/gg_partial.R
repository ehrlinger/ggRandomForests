##=============================================================================
#' Split partial lots into continuous or categorical datasets
#' @param part_dta partial plot data from \code{rfsrc::plot.variable}
#' @param nvars how many of the partial plot variables to calculate
#' @param cat_limit Categorical features are build when there are fewer than
#'  cat_limit unique features.
#' @param model a label name applied to all features. Useful when combining
#'  multiple partial plot objects in figures.
#'
#' @export
gg_partial = function(part_dta,
                      nvars = NULL,
                      cat_limit = 10,
                      model = NULL) {
  ## Prepare the partial dependencies data for panel plots
  if (is.null(nvars)) {
    nvars = length(part_dta$plotthis)
  }
  
  cont_list = list()
  cat_list = list()
  for (feature in seq(nvars)) {
    ## Format any continuous features (those with fewer than cat_limit unique values)
    if (length(unique(part_dta$plotthis[[feature]]$x)) > cat_limit) {
      plt.df = as.data.frame(cbind(
        x = part_dta$plotthis[[feature]]$x,
        yhat = part_dta$plotthis[[feature]]$yhat
      ))
      plt.df$name = names(part_dta$plotthis)[[feature]]
      
      cont_list[[feature]] <- plt.df
    } else{
      ## Categorical features
      
      ## Though VarPro works with logical or continuous only. Factors are
      ## one hot encoded internal to the varPro call.
      plt.df = as.data.frame(cbind(
        x = factor(part_dta$plotthis[[feature]]$x),
        yhat = part_dta$plotthis[[feature]]$yhat
      ))
      plt.df$name = names(part_dta$plotthis)[[feature]]
      
      cat_list[[feature]] <- plt.df
    }
  }
  continuous = dplyr::bind_rows(cont_list)
  categorical = dplyr::bind_rows(cat_list)
  
  if (!is.null(model)) {
    continuous$model <- categorical$model <- model
  }
  
  return(list(continuous = continuous, categorical = categorical))
}