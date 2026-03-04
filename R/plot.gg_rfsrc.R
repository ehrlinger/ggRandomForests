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
#' from the forest.
#'
#' @param x \code{\link{gg_rfsrc}} object created from a
#' \code{\link[randomForestSRC]{rfsrc}} object
#' @param ... arguments passed to \code{\link{gg_rfsrc}}.
#'
#' @return \code{ggplot} object
#'
#' @seealso \code{\link{gg_rfsrc}} \code{\link[randomForestSRC]{rfsrc}}
#'
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#'
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for
#' R, Rnews, 7(2):25-31.
#'
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression
#' and Classification (RF-SRC), R package version 1.4.
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
#' @export plot.gg_rfsrc
plot.gg_rfsrc <- function(x, ...) {
  gg_dta <- x

  # Capture any extra named arguments (e.g. alpha, size) for geom calls
  arg_set <- list(...)

  ## If the user passed a raw rfsrc object, extract predictions first
  if (inherits(gg_dta, "rfsrc")) {
    gg_dta <- gg_rfsrc(gg_dta)
  }

  ## ---- Classification forest branch ------------------------------------
  if (inherits(gg_dta, "class") ||
    inherits(gg_dta, "classification")) {
    if (ncol(gg_dta) < 3) {
      # Binary classification: single probability column + observed class
      gg_plt <- ggplot2::ggplot(gg_dta) +
        ggplot2::geom_jitter(
          ggplot2::aes(
            x = 1,
            y = colnames(gg_dta)[1],
            color = colnames(gg_dta)[2],
            shape = colnames(gg_dta)[2]
          ),
          ...
        ) +
        ggplot2::geom_boxplot(
          ggplot2::aes(x = 1, y = colnames(gg_dta)[1]),
          outlier.colour = "transparent",
          fill = "transparent",
          notch = TRUE,
          ...
        ) +
        ggplot2::theme(
          axis.ticks = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank()
        )
    } else {
      # Multi-class: gather all class probability columns into long form
      gathercols <- colnames(gg_dta)[-which(colnames(gg_dta) == "y")]
      gg_dta_mlt <-
        tidyr::gather(gg_dta, "variable", "value", tidyr::all_of(gathercols))

      gg_plt <-
        ggplot2::ggplot(
          gg_dta_mlt,
          ggplot2::aes(x = "variable", y = "value")
        ) +
        ggplot2::geom_jitter(ggplot2::aes(color = "y", shape = "y"),
          alpha = .5
        )
    }
    gg_plt <- gg_plt + ggplot2::labs(y = "Predicted (%)", x = "")

  ## ---- Survival forest branch ------------------------------------------
  } else if (inherits(gg_dta, "surv")) {
    # Detect whether bootstrap confidence bands have been computed
    if ("lower" %in% colnames(gg_dta)) {
      # Determine ribbon transparency; halve user-supplied alpha if given
      if (is.null(arg_set$alpha)) {
        alph <- .3
      } else {
        alph <- arg_set$alpha * .5
        arg_set$alpha <- NULL
      }

      if ("group" %in% colnames(gg_dta)) {
        # Stratified survival curves with CI ribbon, coloured by group
        gg_plt <- ggplot2::ggplot(gg_dta) +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              x = "value",
              ymin = "lower",
              ymax = "upper",
              fill = "group"
            ),
            alpha = alph,
            ...
          ) +
          ggplot2::geom_step(ggplot2::aes(
            x = "value",
            y = "median",
            color = "group"
          ), ...)
      } else {
        # Single-group survival curve with CI ribbon
        gg_plt <- ggplot2::ggplot(gg_dta) +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              x = "value",
              ymin = "lower",
              ymax = "upper"
            ),
            alpha = alph
          ) +
          ggplot2::geom_step(ggplot2::aes(x = "value", y = "median"), ...)
      }
    } else {
      # No confidence bands: draw one step line per observation
      gg_plt <- ggplot2::ggplot(
        gg_dta,
        ggplot2::aes(
          x = "variable",
          y = "value",
          col = "event",
          by = "obs_id"
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
      gg_plt <- ggplot2::ggplot(gg_dta, ggplot2::aes(x = "group", y = "yhat"))
    } else {
      # Single-group regression: collapse to a single x position
      gg_plt <- ggplot2::ggplot(gg_dta, ggplot2::aes(x = 1, y = "yhat"))
    }

    gg_plt <- gg_plt +
      ggplot2::geom_jitter(, ...) +
      ggplot2::geom_boxplot(
        outlier.colour = "transparent",
        fill = "transparent",
        notch = TRUE,
        ...
      ) +
      ggplot2::labs(y = "Predicted Value", x = colnames(gg_dta)[2]) +
      ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
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
