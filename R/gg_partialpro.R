##=============================================================================
##=============================================================================
#' Split varpro partial dependence data into continuous or categorical datasets
#'
#' Takes the list returned by \code{varpro::partialpro} and separates variables
#' into two data frames: one for continuous predictors (parametric, non-
#' parametric, and causal effect curves) and one for categorical predictors
#' (one row per observation per category level).
#'
#' The split is governed by \code{cat_limit}: a variable is treated as
#' continuous when \code{length(xvirtual) > cat_limit}; otherwise it is
#' treated as categorical and the per-category rows are stacked.
#'
#' @param part_dta partial plot data from \code{varpro::partialpro}.  Each
#'   element of the list must contain fields \code{xvirtual}, \code{xorg},
#'   \code{yhat.par}, \code{yhat.nonpar}, and \code{yhat.causal}.
#' @param nvars how many variables (list elements) to process.  Defaults to
#'   all variables in \code{part_dta}.
#' @param cat_limit Variables with \code{length(xvirtual)} \eqn{\le}
#'   \code{cat_limit} are treated as categorical.  Default \code{10}.
#' @param model a label applied to all rows.  Useful when combining results
#'   from multiple models in a single figure.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{continuous}{data.frame with columns \code{variable},
#'       \code{parametric}, \code{nonparametric}, \code{causal}, \code{name}
#'       (and optionally \code{model})}
#'     \item{categorical}{data.frame with the same columns but one row per
#'       observation per category level}
#'   }
#'
#' @seealso \code{\link{gg_partial}} \code{\link{varpro_feature_names}}
#'
#' @examples
#' ## Construct mock varpro partialpro output:
#' ##   - "age": a continuous predictor (xvirtual has > 10 points)
#' ##   - "sex": a categorical predictor (xvirtual has 2 points)
#' set.seed(42)
#' n_obs <- 30   # number of observations (rows in yhat matrices)
#' n_pts <- 15   # number of evaluation points for continuous variables
#'
#' mock_data <- list(
#'   age = list(
#'     # xvirtual: evaluation grid for the marginal effect curve
#'     xvirtual   = seq(30, 80, length.out = n_pts),
#'     # xorg: original observed values (used only for categorical detection)
#'     xorg       = sample(seq(30, 80, by = 5), n_obs, replace = TRUE),
#'     # yhat matrices: n_obs rows x n_pts columns (predictions at each grid pt)
#'     yhat.par   = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
#'     yhat.nonpar = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
#'     yhat.causal = matrix(rnorm(n_obs * n_pts), nrow = n_obs)
#'   ),
#'   sex = list(
#'     # Two categories: the xvirtual grid has only 2 points
#'     xvirtual   = c(0, 1),
#'     xorg       = sample(c(0, 1), n_obs, replace = TRUE),
#'     # Two-column yhat matrices (one column per category)
#'     yhat.par   = matrix(rnorm(n_obs * 2), nrow = n_obs),
#'     yhat.nonpar = matrix(rnorm(n_obs * 2), nrow = n_obs),
#'     yhat.causal = matrix(rnorm(n_obs * 2), nrow = n_obs)
#'   )
#' )
#'
#' result <- gg_partialpro(mock_data)
#'
#' ## Continuous result: one row per evaluation grid point
#' head(result$continuous)
#'
#' ## Categorical result: n_obs rows per category level
#' head(result$categorical)
#'
#' @export
#'
gg_partialpro <- function(part_dta,
                          nvars = NULL,
                          cat_limit = 10,
                          model = NULL) {
  ## Default: process all variables in the partialpro output list
  if (is.null(nvars)) {
    nvars <- length(part_dta)
  }

  # Accumulate per-variable data frames before binding
  cont_list <- list()
  cat_list  <- list()

  for (feature in seq(nvars)) {
    ## ---- Continuous variable: xvirtual grid has more points than cat_limit --
    if (length(part_dta[[feature]]$xvirtual) > cat_limit) {
      # Summarise each evaluation grid point by averaging across observations
      plt.df <- dplyr::bind_cols(
        variable     = part_dta[[feature]]$xvirtual,
        parametric   = colMeans(part_dta[[feature]]$yhat.par,    na.rm = TRUE),
        nonparametric = colMeans(part_dta[[feature]]$yhat.nonpar, na.rm = TRUE),
        causal       = colMeans(part_dta[[feature]]$yhat.causal,  na.rm = TRUE)
      )

      # Tag rows with variable name for downstream faceting
      plt.df$name <- names(part_dta)[[feature]]

      cont_list[[feature]] <- plt.df

    } else {
      ## ---- Categorical variable: stack per-observation rows per category ----
      ## VarPro works with logical/continuous only; factors are one-hot encoded
      ## internally.  We reconstruct the per-category rows here.
      cat_feat <- list()

      # Each column of yhat.par corresponds to one unique original category
      n_cats <- length(unique(part_dta[[feature]]$xorg))
      for (ind in seq(n_cats)) {
        # One row per observation for this category
        cat_feat[[ind]] <- dplyr::bind_cols(
          parametric    = part_dta[[feature]]$yhat.par[, ind],
          nonparametric = part_dta[[feature]]$yhat.nonpar[, ind],
          causal        = part_dta[[feature]]$yhat.causal[, ind]
        )
        # Record the category label this subset belongs to
        cat_feat[[ind]]$variable <- unique(part_dta[[feature]]$xorg)[ind]

        # Stack iteratively (first iteration initialises plt.df)
        if (ind == 1) {
          plt.df <- cat_feat[[ind]]
        } else {
          plt.df <- dplyr::bind_rows(plt.df, cat_feat[[ind]])
        }
      }

      # Tag rows with variable name for downstream faceting
      plt.df$name <- names(part_dta)[[feature]]

      cat_list[[feature]] <- plt.df
    }
  }

  # Combine per-variable lists into single data frames (NULL entries dropped)
  continuous  <- dplyr::bind_rows(cont_list)
  categorical <- dplyr::bind_rows(cat_list)

  ## Optionally attach a model label (useful when overlaying multiple models)
  if (!is.null(model)) {
    continuous$model <- categorical$model <- model
  }

  return(list(continuous = continuous, categorical = categorical))
}
