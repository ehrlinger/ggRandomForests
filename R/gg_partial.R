##=============================================================================
#' Split partial dependence data into continuous or categorical datasets
#'
#' Takes the list returned by \code{rfsrc::plot.variable(partial = TRUE)} and
#' separates the variables into two data frames: one for continuous predictors
#' and one for categorical (factor-like) predictors.  The split is controlled
#' by \code{cat_limit}: variables with more unique x-values than this threshold
#' are treated as continuous; all others are categorical.
#'
#' @param part_dta partial plot data from \code{rfsrc::plot.variable}
#' @param nvars how many of the partial plot variables to calculate
#' @param cat_limit Categorical features are built when there are fewer than
#'  \code{cat_limit} unique feature values.
#' @param model a label name applied to all features. Useful when combining
#'  multiple partial plot objects in figures.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{continuous}{data.frame with columns \code{x}, \code{yhat},
#'       \code{name} (and optionally \code{model}) for continuous variables}
#'     \item{categorical}{data.frame with the same columns but with \code{x}
#'       as a factor, for low-cardinality / categorical variables}
#'   }
#'
#' @seealso \code{\link{gg_partial_rfsrc}} \code{\link{gg_partialpro}}
#'
#' @examples
#' ## Build a small regression forest on the airquality dataset
#' set.seed(42)
#' airq <- na.omit(airquality)
#' rf <- rfsrc(Ozone ~ ., data = airq, ntree = 50)
#'
#' ## Compute partial dependence via plot.variable (show.plots = FALSE to
#' ## suppress the base-graphics output — we only want the data)
#' pv <- randomForestSRC::plot.variable(rf, partial = TRUE,
#'                                       show.plots = FALSE)
#'
#' ## Split into continuous and categorical data frames
#' result <- gg_partial(pv)
#' head(result$continuous)
#'
#' ## Label this model for later comparison with a second forest
#' result_labelled <- gg_partial(pv, model = "airq_model")
#' unique(result_labelled$continuous$model)
#'
#' @export
gg_partial <- function(part_dta,
                       nvars = NULL,
                       cat_limit = 10,
                       model = NULL) {
  ## Default: process all variables returned by plot.variable
  if (is.null(nvars)) {
    nvars <- length(part_dta$plotthis)
  }

  # Accumulate per-variable data frames before binding
  cont_list <- list()
  cat_list <- list()

  for (feature in seq(nvars)) {
    x_vals <- part_dta$plotthis[[feature]]$x

    ## ---- Continuous variable: more unique x values than cat_limit -------
    if (length(unique(x_vals)) > cat_limit) {
      plt.df <- dplyr::bind_cols(
        x    = x_vals,
        yhat = part_dta$plotthis[[feature]]$yhat
      )
      # Tag each row with the variable name for downstream faceting
      plt.df$name <- names(part_dta$plotthis)[[feature]]

      cont_list[[feature]] <- plt.df

    } else {
      ## ---- Categorical variable: few unique x values -------------------
      ## VarPro works with logical or continuous only; factors are
      ## one-hot encoded internally in the varPro call.
      ## Normalize to character so bind_rows sees a consistent type; we'll
      ## re-factor within each feature after stacking.
      x_chr <- as.character(x_vals)

      plt.df <- dplyr::bind_cols(
        x    = x_chr,
        yhat = part_dta$plotthis[[feature]]$yhat
      )
      plt.df$name <- names(part_dta$plotthis)[[feature]]

      cat_list[[feature]] <- plt.df
    }
  }

  # Combine per-variable lists into single data frames (NULL entries dropped)
  continuous  <- dplyr::bind_rows(cont_list)
  if (length(cat_list) == 0) {
    categorical <- data.frame(x = character(0), yhat = numeric(0), name = character(0))
  } else {
    categorical <- dplyr::bind_rows(cat_list)
    # Set within-group factor levels to order-of-appearance (base-R, no .data pronoun)
    split_grps <- split(seq_len(nrow(categorical)), categorical$name)
    for (grp_idx in split_grps) {
      vals <- categorical$x[grp_idx]
      categorical$x[grp_idx] <- as.character(factor(vals, levels = unique(vals)))
    }
    categorical$x <- factor(categorical$x)
  }
  ## Optionally attach a model label (useful when overlaying multiple forests)
  if (!is.null(model)) {
    continuous$model <- model
    if (!is.null(categorical) && nrow(categorical) > 0) {
      categorical$model <- model
    }
  }

  return(list(continuous = continuous, categorical = categorical))
}
