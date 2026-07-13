##=============================================================================
#' Split partial dependence data into continuous or categorical datasets
#'
#' A partial dependence curve answers a what-if question about a forest: hold
#' every other predictor at its observed value, sweep one of them across its
#' range, and watch how the ensemble prediction moves.  Marginalized over the
#' joint distribution of the other variables, the resulting curve isolates the
#' average effect of the swept predictor alone.
#'
#' \code{gg_partial} handles the bookkeeping step after you've already called
#' \code{randomForestSRC::plot.variable(partial = TRUE)}: it takes the list
#' that function returns and separates the variables into two tidy data frames
#' -- one for continuous predictors (plotted as lines) and one for categorical
#' predictors (plotted as bar charts).  The split is controlled by
#' \code{cat_limit}: variables with more unique x-values than this threshold
#' are treated as continuous; all others are categorical.
#'
#' If you'd rather skip the \code{plot.variable} step and pass the fitted
#' forest directly, see \code{\link{gg_partial_rfsrc}}, which calls
#' \code{partial.rfsrc} for you.
#'
#' @param part_dta partial plot data from
#'   \code{randomForestSRC::plot.variable}
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
#' rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50)
#'
#' ## Compute partial dependence via plot.variable (show.plots = FALSE to
#' ## suppress the base-graphics output, we only want the data)
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
#' @note Partial-dependence extraction is `randomForestSRC`-only;
#'   there is no `randomForest` method (the `randomForest` package
#'   provides no comparable partial-dependence interface).
#'
#' @note For survival forests, \code{randomForestSRC::plot.variable} defaults
#'   to \code{surv.type = "mort"}, so \code{yhat} is \emph{mortality} -- the
#'   expected number of events -- and not a survival probability. It is
#'   therefore not on \eqn{[0, 1]} and is not directly comparable with the
#'   survival probabilities returned by \code{\link{gg_variable}}. For a
#'   comparable quantity, ask for it explicitly:
#'   \code{randomForestSRC::plot.variable(rf, partial = TRUE,
#'   surv.type = "surv")}. The label describing the plotted quantity is
#'   recorded on the returned object as \code{attr(x, "ylabel")} and is used
#'   as the y-axis title by \code{\link{plot.gg_partial}}.
#'
#'   Note that \code{\link{gg_partial_rfsrc}} defaults to
#'   \code{partial.type = "surv"} and so reports survival probabilities. The
#'   two entry points therefore report different quantities by default.
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

  result <- list(continuous = continuous, categorical = categorical)
  class(result) <- "gg_partial"
  ## Carry plot.variable()'s own description of the plotted quantity. For
  ## survival forests it defaults to surv.type = "mort", so yhat is mortality
  ## (an expected event count) rather than a survival probability; without this
  ## label the two are easily confused. See gg_partial's @note.
  attr(result, "ylabel") <- part_dta$ylabel
  return(result)
}
