##=============================================================================
##=============================================================================
#' Split partial lots into continuous or categorical datasets
#' @param part_dta partial plot data from \code{rfsrc::plot.variable}
#' @param nvars how many of the partial plot variables to calculate
#' @param cat_limit Categorical features are build when there are fewer than 
#'  cat_limit unique features.
#' @param name a label name applied to all features. Useful when combining 
#'  multiple partial plot objects in figures.
#'    
#' @export
#' 
df_partialpro = function(part_dta, nvars = NULL, cat_limit=12, name=NULL) {
  ## Prepare the partial pro  dependencies data for panel plots
  if (is.null(nvars)) {
    nvars = length(part_dta)
  }
  
  cont_list = list()
  cat_list = list()
  for (feature in seq(nvars)) {
    ## Format any continuous features (those with fewer than 10 unique values)
    if (length(part_dta[[feature]]$xvirtual) > cat_limit) {
      plt.df = as.data.frame(
        cbind(
          variable = part_dta[[feature]]$xvirtual,
          parametric = colMeans(part_dta[[feature]]$yhat.par, na.rm =
                                  TRUE),
          nonparametric = colMeans(part_dta[[feature]]$yhat.nonpar, na.rm =
                                     TRUE),
          causal = colMeans(part_dta[[feature]]$yhat.causal, na.rm =
                              TRUE)
        )
      )
      plt.df$name = names(part_dta)[[feature]]
      
      cont_list[[feature]] <- plt.df
    } else{
      ## Categorical features
      
      ## Though VarPro works with logical or continuous only. Factors are
      ## one hot encoded internal to the varPro call.
      cat_feat = list()
      ## Each yhat has at least 2 columns, for logical values...
      for (ind in seq(length(unique(part_dta[[feature]]$xorg)))) {
        cat_feat[[ind]] = as.data.frame(
          cbind(
            parametric = part_dta[[feature]]$yhat.par[, ind],
            nonparametric = part_dta[[feature]]$yhat.nonpar[, ind],
            causal = part_dta[[feature]]$yhat.causal[, ind]
          )
        )
        cat_feat[[ind]]$variable <-
          unique(part_dta[[feature]]$xorg)[ind]
        if (ind == 1) {
          plt.df <- cat_feat[[ind]]
        } else{
          plt.df <- dplyr::bind_rows(plt.df, cat_feat[[ind]])
        }
      }
      
      plt.df$name = names(part_dta)[[feature]]
      
      cat_list[[feature]] <- plt.df
    }
  }
  
  continuous = dplyr::bind_rows(cont_list)
  categorical = dplyr::bind_rows(cat_list)
  
  if(!is.na(name)){
    continuous$model <- categorical$model <- name
  }
  
  return(list(
    continuous = continuous,
    categorical = categorical
  ))
}