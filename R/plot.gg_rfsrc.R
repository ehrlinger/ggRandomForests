####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' Predicted response plot from a \code{\link{gg_rfsrc}} object.
#'
#' Plot the predicted response from a \code{\link{gg_rfsrc}} object, the
#' \code{\link[randomForestSRC]{rfsrc}} prediction, using the OOB prediction
#' from the forest.  The plot type adapts automatically to the forest family:
#' jitter + boxplot for regression and classification, step curves for
#' survival.
#'
#' @param x A \code{\link{gg_rfsrc}} object, or a raw
#'   \code{\link[randomForestSRC]{rfsrc}} object (which will be passed through
#'   \code{\link{gg_rfsrc}} automatically before plotting).
#' @param notch Logical; whether to draw notched boxplots for regression and
#'   classification forests (default \code{TRUE}).  Set \code{notch = FALSE}
#'   to suppress notches when sample sizes are too small for reliable
#'   confidence intervals on the median.
#' @param ... Additional arguments forwarded to the underlying
#'   \code{ggplot2} geometry calls.  Commonly useful arguments include:
#'   \describe{
#'     \item{\code{alpha}}{Numeric in \eqn{[0,1]}; point/ribbon transparency.
#'       For survival plots with confidence bands the ribbon alpha is
#'       automatically halved relative to the value supplied here.}
#'     \item{\code{size}}{Point or line size passed to \code{geom_jitter},
#'       \code{geom_step}, etc.}
#'   }
#'   Arguments that control \code{\link{gg_rfsrc}} (e.g. \code{conf.int},
#'   \code{surv_type}, \code{by}) should be applied when constructing the
#'   \code{gg_rfsrc} object before calling \code{plot()}.
#'
#' @return A \code{ggplot} object.  The plot appearance depends on the forest
#'   family stored in \code{x}:
#'   \describe{
#'     \item{Regression (\code{"regr"})}{Jitter + notched boxplot of OOB
#'       predicted values.  If a \code{group} column is present the x-axis
#'       shows each group label; otherwise observations are collapsed to a
#'       single x-position.}
#'     \item{Classification (\code{"class"})}{Binary: jitter + notched
#'       boxplot of the predicted class probability.  Multi-class: jitter
#'       plot with one panel per class (class probabilities in long form).}
#'     \item{Survival (\code{"surv"})}{Step curves of the ensemble survival
#'       function.  When \code{gg_rfsrc} was called with \code{conf.int},
#'       a shaded ribbon is added.  When called with \code{by}, curves are
#'       coloured by group.}
#'   }
#'
#' @seealso \code{\link{gg_rfsrc}} \code{\link[randomForestSRC]{rfsrc}}
#'   \code{\link[randomForest]{randomForest}}
#'
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#'
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for
#' R, Rnews, 7(2):25-31.
#'
#' Ishwaran H. and Kogalur U.B. randomForestSRC: Random Forests for Survival,
#' Regression and Classification. R package version >= 3.4.0.
#' \url{https://cran.r-project.org/package=randomForestSRC}
#'
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' # Build a small classification forest (ntree=50 keeps example fast)
#' set.seed(42)
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris, ntree = 50)
#' gg_dta <- gg_rfsrc(rfsrc_iris)
#'
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' ## -------- air quality data
#' # na.action = "na.impute" handles missing Ozone / Solar.R values
#' set.seed(42)
#' rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality,
#'                     na.action = "na.impute", ntree = 50)
#' gg_dta <- gg_rfsrc(rfsrc_airq)
#'
#' plot(gg_dta)
#'
#' ## -------- mtcars data
#' set.seed(42)
#' rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars, ntree = 50)
#' gg_dta <- gg_rfsrc(rfsrc_mtcars)
#'
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## -------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' set.seed(42)
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 50)
#' gg_dta <- gg_rfsrc(rfsrc_veteran)
#' plot(gg_dta)
#'
#' # With 95% pointwise bootstrap confidence bands
#' gg_dta <- gg_rfsrc(rfsrc_veteran, conf.int = .95)
#' plot(gg_dta)
#'
#' # Stratified by treatment arm
#' gg_dta <- gg_rfsrc(rfsrc_veteran, by = "trt")
#' plot(gg_dta)
#'
#' ## -------- pbc data (larger dataset -- skipped on CRAN)
#' \donttest{
#' data(pbc, package = "randomForestSRC")
#' # For whatever reason, the age variable is in days; convert to years
#' for (ind in seq_len(dim(pbc)[2])) {
#'   if (!is.factor(pbc[, ind])) {
#'     if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'       if (sum(range(pbc[, ind], na.rm = TRUE) == c(0, 1)) == 2) {
#'         pbc[, ind] <- as.logical(pbc[, ind])
#'       }
#'     }
#'   } else {
#'     if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'       if (sum(sort(unique(pbc[, ind])) == c(0, 1)) == 2) {
#'         pbc[, ind] <- as.logical(pbc[, ind])
#'       }
#'       if (sum(sort(unique(pbc[, ind])) == c(FALSE, TRUE)) == 2) {
#'         pbc[, ind] <- as.logical(pbc[, ind])
#'       }
#'     }
#'   }
#'   if (!is.logical(pbc[, ind]) &
#'     length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 5) {
#'     pbc[, ind] <- factor(pbc[, ind])
#'   }
#' }
#' # Convert age from days to years
#' pbc$age <- pbc$age / 364.24
#' pbc$years <- pbc$days / 364.24
#' pbc <- pbc[, -which(colnames(pbc) == "days")]
#' pbc$treatment <- as.numeric(pbc$treatment)
#' pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
#' pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
#' pbc$treatment <- factor(pbc$treatment)
#' # Remove test-set patients (those with no assigned treatment)
#' dta_train <- pbc[-which(is.na(pbc$treatment)), ]
#'
#' set.seed(42)
#' rfsrc_pbc <- randomForestSRC::rfsrc(
#'   Surv(years, status) ~ .,
#'   dta_train,
#'   nsplit = 10,
#'   na.action = "na.impute",
#'   forest = TRUE,
#'   importance = TRUE,
#'   save.memory = TRUE
#' )
#'
#' gg_dta <- gg_rfsrc(rfsrc_pbc)
#' plot(gg_dta)
#'
#' gg_dta <- gg_rfsrc(rfsrc_pbc, conf.int = .95)
#' plot(gg_dta)
#'
#' gg_dta <- gg_rfsrc(rfsrc_pbc, by = "treatment")
#' plot(gg_dta)
#' }
#'
#' @export
plot.gg_rfsrc <- function(x, notch = TRUE, ...) {
  gg_dta <- x

  # Capture extra passthrough args (e.g. alpha for survival ribbons).
  # notch is a named formal above so it is never present in arg_set.
  arg_set <- list(...)

  ## If the user passed a raw rfsrc object, extract predictions first
  if (inherits(gg_dta, "rfsrc")) {
    gg_dta <- gg_rfsrc(gg_dta)
  }

  ## ---- Classification forest branch ------------------------------------
  if (inherits(gg_dta, "class") ||
    inherits(gg_dta, "classification")) {
    # Identify probability columns (everything except "y" and "group")
    non_prob <- c("y", "group")
    prob_cols <- setdiff(colnames(gg_dta), non_prob)
    prob_col <- prob_cols[1]
    obs_col  <- "y"
    if (length(prob_cols) < 2) {
      # Binary classification: single probability column + observed class
      gg_plt <- ggplot2::ggplot(gg_dta) +
        ggplot2::geom_jitter(
          ggplot2::aes(
            x = 1,
            y = .data[[prob_col]],
            color = .data[[obs_col]],
            shape = .data[[obs_col]]
          ),
          ...
        ) +
        ggplot2::geom_boxplot(
          ggplot2::aes(x = 1, y = .data[[prob_col]]),
          outlier.colour = "transparent",
          fill = "transparent",
          notch = notch
        ) +
        ggplot2::theme(
          axis.ticks = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank()
        )
    } else {
      # Multi-class: gather all class probability columns into long form
      pivot_cols <- setdiff(colnames(gg_dta), c("y", "group"))
      gg_dta_mlt <-
        tidyr::pivot_longer(gg_dta, tidyr::all_of(pivot_cols), names_to = "variable", values_to = "value")

      gg_plt <-
        ggplot2::ggplot(
          gg_dta_mlt,
          ggplot2::aes(x = .data$variable, y = .data$value)
        ) +
        ggplot2::geom_jitter(
          ggplot2::aes(color = .data$y, shape = .data$y),
          alpha = .5
        )
    }
    gg_plt <- gg_plt + ggplot2::labs(y = "Predicted (%)", x = "")

  ## ---- Survival forest branch ------------------------------------------
  } else if (inherits(gg_dta, "surv")) {
    # Detect whether bootstrap confidence bands have been computed
    if ("lower" %in% colnames(gg_dta)) {
      # Split ... into ribbon alpha (halved) and step dots (full alpha).
      # Prevents duplicate-formal error when user passes alpha via ...
      spl  <- .gg_split_alpha(arg_set)
      alph <- spl$ribbon_alpha

      if ("group" %in% colnames(gg_dta)) {
        # Stratified survival curves with CI ribbon, coloured by group
        gg_plt <- ggplot2::ggplot(gg_dta) +
          do.call(
            ggplot2::geom_ribbon,
            c(list(
              ggplot2::aes(
                x    = .data$value,
                ymin = .data$lower,
                ymax = .data$upper,
                fill = .data$group
              ),
              alpha = alph
            ), spl$ribbon_dots)
          ) +
          do.call(
            ggplot2::geom_step,
            c(list(ggplot2::aes(
              x     = .data$value,
              y     = .data$median,
              color = .data$group
            )), spl$step_dots)
          )
      } else {
        # Single-group survival curve with CI ribbon
        gg_plt <- ggplot2::ggplot(gg_dta) +
          do.call(
            ggplot2::geom_ribbon,
            c(list(
              ggplot2::aes(
                x    = .data$value,
                ymin = .data$lower,
                ymax = .data$upper
              ),
              alpha = alph,
              fill  = .gg_ribbon_fill
            ), spl$ribbon_dots)
          ) +
          do.call(
            ggplot2::geom_step,
            c(list(ggplot2::aes(
              x = .data$value,
              y = .data$median
            )), spl$step_dots)
          )
      }
    } else {
      # No confidence bands: draw one step line per observation
      gg_plt <- ggplot2::ggplot(
        gg_dta,
        ggplot2::aes(
          x = .data$variable,
          y = .data$value,
          col = .data$event,
          group = .data$obs_id
        )
      ) +
        ggplot2::geom_step(...)
    }

    gg_plt <- gg_plt +
      ggplot2::labs(x = "time (years)", y = "Survival (%)")

  ## ---- Regression forest branch ----------------------------------------
  } else if (inherits(gg_dta, "regr") ||
    inherits(gg_dta, "regression")) {
    if ("group" %in% colnames(gg_dta)) {
      # Grouped regression: x-axis shows each group label
      gg_plt <- ggplot2::ggplot(gg_dta, ggplot2::aes(x = .data$group, y = .data$yhat))
    } else {
      # Single-group regression: collapse to a single x position
      gg_plt <- ggplot2::ggplot(gg_dta, ggplot2::aes(x = 1, y = .data$yhat))
    }

    gg_plt <- gg_plt +
      ggplot2::geom_jitter(...) +
      ggplot2::geom_boxplot(
        outlier.colour = "transparent",
        fill = "transparent",
        notch = notch
      ) +
      ggplot2::labs(
        y = "Predicted Value",
        x = if ("group" %in% colnames(gg_dta)) "Group" else ""
      ) +
      ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.text.x = if ("group" %in% colnames(gg_dta)) {
          ggplot2::element_text()
        } else {
          ggplot2::element_blank()
        }
      )
  } else {
    # Unknown forest type — not yet implemented
    stop(paste(
      "Plotting for ",
      class(gg_dta)[2],
      " randomForestSRC is not yet implemented."
    ))
  }
  return(gg_plt)
}
