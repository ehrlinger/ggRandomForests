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
#'
#' Plot a \code{\link{gg_variable}} object,
#'
#' @param x \code{\link{gg_variable}} object created from a
#' \code{\link[randomForestSRC]{rfsrc}} object
#' @param xvar variable (or list of variables) of interest.
#' @param time For survival, one or more times of interest
#' @param time_labels string labels for times
#' @param panel Should plots be faceted along multiple xvar?
#' @param oob oob estimates (boolean)
#' @param points plot the raw data points (boolean)
#' @param smooth include a smooth curve (boolean)
#' @param ... arguments passed to the \code{ggplot2} functions.
#'
#' @return A single \code{ggplot} object when \code{length(xvar) == 1} or
#'   \code{panel = TRUE}. Otherwise a named list of \code{ggplot} objects, one
#'   per variable in \code{xvar}.
#'
#' @seealso \code{\link{gg_variable}}, \code{\link{gg_partial}},
#'   \code{\link[randomForestSRC]{plot.variable}}
#'
#' @references Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#'
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, Rnews,
#' 7(2):25-31.
#'
#' Ishwaran H. and Kogalur U.B. randomForestSRC: Random Forests for Survival,
#' Regression and Classification. R package version >= 3.4.0.
#' \url{https://cran.r-project.org/package=randomForestSRC}
#'
#'
#' @importFrom ggplot2 .data
#' @examples
#' ## ------------------------------------------------------------
#' ## classification
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' set.seed(42)
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris, ntree = 50)
#'
#' gg_dta <- gg_variable(rfsrc_iris)
#' plot(gg_dta, xvar = "Sepal.Width")
#' plot(gg_dta, xvar = "Sepal.Length")
#'
#' ## Panel plot across all predictors
#' plot(gg_dta,
#'   xvar = rfsrc_iris$xvar.names,
#'   panel = TRUE, se = FALSE
#' )
#'
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#' ## -------- air quality data
#' # na.action = "na.impute" handles missing Ozone / Solar.R values
#' set.seed(42)
#' rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality,
#'                     na.action = "na.impute", ntree = 50)
#' gg_dta <- gg_variable(rfsrc_airq)
#'
#' # Treat Month as an ordinal factor for better visualisation
#' gg_dta[, "Month"] <- factor(gg_dta[, "Month"])
#'
#' plot(gg_dta, xvar = "Wind")
#' plot(gg_dta, xvar = "Temp")
#' plot(gg_dta, xvar = "Solar.R")
#'
#' # Panel plot across continuous predictors
#' plot(gg_dta, xvar = c("Solar.R", "Wind", "Temp", "Day"), panel = TRUE)
#'
#' # Factor variable uses notched boxplots
#' plot(gg_dta, xvar = "Month", notch = TRUE)
#'
#' ## ------------------------------------------------------------
#' ## survival examples
#' ## ------------------------------------------------------------
#' ## -------- veteran data
#' data(veteran, package = "randomForestSRC")
#' set.seed(42)
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., veteran,
#'   nsplit = 10,
#'   ntree = 50
#' )
#'
#' # Marginal survival at 90 days
#' gg_dta <- gg_variable(rfsrc_veteran, time = 90)
#'
#' # Single-variable dependence plots
#' plot(gg_dta, xvar = "age")
#' plot(gg_dta, xvar = "diagtime")
#'
#' # Panel coplot for two predictors at a single time
#' plot(gg_dta, xvar = c("age", "diagtime"), panel = TRUE)
#'
#' # Compare survival at 30, 90, and 365 days simultaneously
#' gg_dta <- gg_variable(rfsrc_veteran, time = c(30, 90, 365))
#'
#' # Single-variable plot (one facet per time point)
#' plot(gg_dta, xvar = "age")
#'
#' # Panel coplot across two predictors and three time points
#' plot(gg_dta, xvar = c("age", "diagtime"), panel = TRUE)
#'
#' @export
plot.gg_variable <- function(x, # nolint: cyclocomp_linter
                             xvar,
                             time,
                             time_labels,
                             panel = FALSE,
                             oob = TRUE,
                             points = TRUE,
                             smooth = TRUE,
                             ...) {
  gg_dta <- x

  # I don't think this will work with latest S3 models.
  if (inherits(x, "rfsrc")) {
    gg_dta <- gg_variable(x, ...)
  }

  ## ---- Detect forest family from gg_variable class attributes ----------
  # Default to classification; override if survival or regression flags found
  family <- "class"
  if (inherits(gg_dta, "surv")) {
    family <- "surv"
  } else if (inherits(gg_dta, "regr")) {
    family <- "regr"
  } else if (inherits(gg_dta, "regression")) {
    family <- "regr"
  }

  # Fallback detection: presence of an "event" column signals survival
  if (sum(colnames(gg_dta) == "event") != 0) {
    family <- "surv"
  }

  ## ---- Reshape multi-class classification data -------------------------
  # Multiple yhat.* columns indicate a multi-class forest
  if (length(grep("yhat.", colnames(gg_dta))) > 0) {
    if (length(grep("yhat.", colnames(gg_dta))) == 2) {
      # Binary: drop the first class column and rename the second to "yhat"
      gg_dta <- gg_dta[, -grep("yhat.", colnames(gg_dta))[1]]
      colnames(gg_dta)[grep("yhat.", colnames(gg_dta))] <- "yhat"
    } else {
      # Multi-class: pivot to long format so each class becomes a row group
      gg_dta_x <- gg_dta[, -grep("yhat.", colnames(gg_dta))]
      gg_dta_y <- gg_dta[, grep("yhat.", colnames(gg_dta))]
      lng <- ncol(gg_dta_y)
      gg2 <- parallel::mclapply(seq_len(ncol(gg_dta_y)), function(ind) {
        cbind(gg_dta_x, yhat = gg_dta_y[, ind], outcome = ind)
      })
      gg3 <- do.call(rbind, gg2)
      gg3$outcome <- factor(gg3$outcome)
      gg_dta <- gg3
    }
  }

  ## ---- Default xvar: all predictor columns -----------------------------
  if (missing(xvar)) {
    # Remove response-side columns (yhat, event, time) to isolate predictors
    cls <- c(
      grep("yhat", colnames(gg_dta)),
      grep("event", colnames(gg_dta)),
      grep("time", colnames(gg_dta))
    )
    xvar <- colnames(gg_dta)[-cls]
  }

  lng <- length(xvar)

  # Column indices corresponding to the requested predictor(s)
  wch_x_var <- which(colnames(gg_dta) %in% xvar)

  ## ---- Coerce 0/1 indicator columns to logical -------------------------
  # Columns with exactly two values in {0, 1} are treated as boolean flags
  for (ind in seq_len(ncol(gg_dta))) {
    if (!is.factor(gg_dta[, ind])) {
      if (length(unique(gg_dta[which(!is.na(gg_dta[, ind])), ind])) <= 2) {
        if (sum(range(gg_dta[, ind], na.rm = TRUE) == c(0, 1)) == 2) {
          gg_dta[, ind] <- as.logical(gg_dta[, ind])
        }
      }
    } else {
      if (length(unique(gg_dta[which(!is.na(gg_dta[, ind])), ind])) <= 2) {
        if (sum(sort(unique(gg_dta[, ind])) == c(0, 1)) == 2) {
          gg_dta[, ind] <- as.logical(gg_dta[, ind])
        }
        if (sum(sort(unique(gg_dta[, ind])) == c(FALSE, TRUE)) == 2) {
          gg_dta[, ind] <- as.logical(gg_dta[, ind])
        }
      }
    }
  }

  ## ---- Record each column's storage class for plot dispatch -----------
  # "integer" is treated as numeric for plot branching purposes
  ccls <- sapply(gg_dta, class)
  ccls[which(ccls == "integer")] <- "numeric"

  ## =========================================================
  ## PANEL PLOT branch — facet multiple predictors in one figure
  ## =========================================================
  if (panel) {
    ## ---- Survival panel plot ----------------------------------------
    if (family == "surv") {
      ## Indices of response columns (time, event, and yhat)
      wch_y_var <-
        which(colnames(gg_dta) %in% c("event", "yhat", "time"))

      # Subset to response + requested predictors, then pivot to long form
      tmp_dta <- gg_dta[, c(wch_y_var, wch_x_var)]
      pivot_cols <-
        colnames(tmp_dta)[-which(colnames(tmp_dta) %in%
          c("time", "event", "yhat"))]
      gg_dta_mlt <-
        tidyr::pivot_longer(tmp_dta, tidyr::all_of(pivot_cols), names_to = "variable", values_to = "value")

      # Preserve user-supplied xvar ordering in the facet strips
      gg_dta_mlt$variable <-
        factor(gg_dta_mlt$variable, levels = xvar)
      if (points) {
        gg_plt <- ggplot2::ggplot(
          gg_dta_mlt,
          ggplot2::aes(
            x = .data$value,
            y = .data$yhat,
            color = .data$event,
            shape = .data$event
          )
        )
      } else {
        gg_plt <- ggplot2::ggplot(
          gg_dta_mlt,
          ggplot2::aes(x = .data$value, y = .data$yhat)
        )
      }
      # If these are all continuous...
      if (sum(ccls[wch_x_var] == "numeric") == length(wch_x_var)) {
        gg_plt <- gg_plt +
          ggplot2::labs(y = "Survival")
        if (points) {
          gg_plt <- gg_plt +
            ggplot2::geom_point(...)
        } else {
          gg_plt <- gg_plt +
            ggplot2::geom_smooth(...)
        }
        if (smooth) {
          gg_plt <- gg_plt +
            ggplot2::geom_smooth(...)
        }
      } else {
        # Mixed or all-factor predictors: fall back to boxplot+jitter
        if (sum(ccls[wch_x_var] == "numeric") > 0) {
          warning(
            "Mismatched variable types for panel plots...
            assuming these are all factor variables."
          )
        }

        gg_plt <- gg_plt +
          ggplot2::geom_boxplot(
            ggplot2::aes(x = .data$value, y = .data$yhat),
            color = "grey",
            ...,
            outlier.shape = NA
          ) +
          ggplot2::geom_jitter(
            ggplot2::aes(
              x = .data$value,
              y = .data$yhat,
              color = .data$event,
              shape = .data$event
            ),
            ...
          )
      }

      # Multiple time points: grid of (time × variable); single time: wrap
      if (length(levels(gg_dta$time)) > 1) {
        gg_plt <- gg_plt +
          ggplot2::facet_grid(stats::reformulate("variable", "time"),
            scales = "free_x"
          ) +
          labs(x = "")
      } else {
        gg_plt <- gg_plt +
          ggplot2::facet_wrap(~variable, scales = "free_x") +
          labs(
            x = "",
            y = paste("Survival at", gg_dta$time[1], "year")
          )
      }

    } else {
      ## ---- Regression / classification panel plot ---------------------
      wch_y_var <- which(colnames(gg_dta) %in% c("yhat"))

      if (family == "class") {
        # Include the observed class label column for colouring
        wch_y_var <- c(wch_y_var, which(colnames(gg_dta) == "yvar"))
        tmp_dta <- gg_dta[, c(wch_y_var, wch_x_var)]
        pivot_cols <-
          colnames(tmp_dta)[-which(colnames(tmp_dta) %in% c("yvar", "yhat"))]
        gg_dta_mlt <-
          tidyr::pivot_longer(
            tmp_dta,
            tidyr::all_of(pivot_cols),
            names_to = "variable",
            values_to = "value"
          )
      } else {
        # Regression: keep yhat and the optional yvar reference column
        wch_y_var <- c(wch_y_var, which(colnames(gg_dta) == "yvar"))
        tmp_dta <- gg_dta[, c(wch_y_var, wch_x_var)]
        pivot_cols <-
          colnames(tmp_dta)[-which(colnames(tmp_dta) == "yhat")]
        gg_dta_mlt <-
          tidyr::pivot_longer(
            tmp_dta,
            tidyr::all_of(pivot_cols),
            names_to = "variable",
            values_to = "value"
          )
      }
      # Preserve user-supplied xvar ordering in the facet strips
      gg_dta_mlt$variable <-
        factor(gg_dta_mlt$variable, levels = xvar)

      # All continuous predictors → scatter; any factor → boxplot
      if (sum(ccls[wch_x_var] == "numeric") == length(wch_x_var)) {
        if (family == "class") {
          gg_plt <-
            ggplot2::ggplot(
              gg_dta_mlt,
              ggplot2::aes(
                x = .data$value,
                y = .data$yhat,
                color = .data$yvar,
                shape = .data$yvar
              )
            ) +
            ggplot2::geom_point(...)
        } else {
          gg_plt <- ggplot2::ggplot(
            gg_dta_mlt,
            ggplot2::aes(x = .data$value, y = .data$yhat)
          ) +
            ggplot2::geom_point(...)
        }
      } else {
        # Warn if numeric and factor predictors are mixed in the same panel
        if (sum(ccls[wch_x_var] == "numeric") > 0) {
          warning("Mismatched variable types...
                  assuming these are all factor variables.")
        }

        if (family == "class") {
          gg_plt <-
            ggplot2::ggplot(
              gg_dta_mlt,
              ggplot2::aes(
                x = .data$value,
                y = .data$yhat,
                color = .data$yvar
              )
            ) +
            ggplot2::geom_boxplot(...)
        } else {
          gg_plt <- ggplot2::ggplot(
            gg_dta_mlt,
            ggplot2::aes(x = .data$value, y = .data$yhat)
          ) +
            ggplot2::geom_boxplot(...)
        }
      }
      # Add point/smooth layers for non-classification forests
      if (family != "class") {
        if (points) {
          gg_plt <- gg_plt +
            ggplot2::geom_point(...)
        } else {
          gg_plt <- gg_plt +
            ggplot2::geom_smooth(...)
        }
        if (smooth) {
          gg_plt <- gg_plt +
            ggplot2::geom_smooth(...)
        }
      }

      gg_plt <- gg_plt +
        ggplot2::facet_wrap(~variable, scales = "free_x") +
        ggplot2::labs(x = "")
    }

  ## =========================================================
  ## INDIVIDUAL PLOT branch — one ggplot per predictor variable
  ## =========================================================
  } else {
    # Pre-allocate a list; collapsed to a single object when lng == 1
    gg_plt <- vector("list", length = lng)

    for (ind in seq_len(lng)) {
      # Temporarily rename the target predictor column to "var" for aes()
      ch_indx <- which(colnames(gg_dta) == xvar[ind])
      h_name <- colnames(gg_dta)[ch_indx]
      colnames(gg_dta)[ch_indx] <- "var"
      # Use only the primary class (class() can return multiple strings, e.g.
      # c("POSIXct", "POSIXt")); a multi-element vector in if() triggers a warning.
      ccls_var <- class(gg_dta[, "var"])[1L]
      if (ccls_var == "integer") ccls_var <- "numeric"

      gg_plt[[ind]] <- ggplot2::ggplot(gg_dta)

      ## ---- Survival individual plot -----------------------------------
      if (family == "surv") {
        gg_plt[[ind]] <- gg_plt[[ind]] +
          ggplot2::labs(x = h_name, y = "Survival")

        if (ccls_var == "numeric") {
          # Continuous predictor: scatter (and optional smooth)
          if (points) {
            gg_plt[[ind]] <- gg_plt[[ind]] +
              ggplot2::geom_point(
                ggplot2::aes(
                  x = .data$var,
                  y = .data$yhat,
                  color = .data$event,
                  shape = .data$event
                ),
                ...
              )
          } else {
            gg_plt[[ind]] <- gg_plt[[ind]] +
              ggplot2::geom_smooth(ggplot2::aes(x = .data$var, y = .data$yhat), ...)
          }
          if (smooth) {
            gg_plt[[ind]] <- gg_plt[[ind]] +
              ggplot2::geom_smooth(ggplot2::aes(x = .data$var, y = .data$yhat), ...)
          }
        } else {
          # Factor predictor: boxplot + jittered points coloured by event
          gg_plt[[ind]] <- gg_plt[[ind]] +
            ggplot2::geom_boxplot(
              ggplot2::aes(x = .data$var, y = .data$yhat),
              color = "black",
              ...,
              outlier.shape = NA
            ) +
            ggplot2::geom_jitter(
              ggplot2::aes(
                x = .data$var,
                y = .data$yhat,
                color = .data$event,
                shape = .data$event
              ),
              ...
            )
        }
        # Multiple time points: facet vertically by time
        if (length(levels(gg_dta$time)) > 1) {
          gg_plt[[ind]] <- gg_plt[[ind]] +
            ggplot2::facet_wrap(~time, ncol = 1)
        } else {
          gg_plt[[ind]] <- gg_plt[[ind]] +
            ggplot2::labs(
              x = h_name,
              y = paste("Survival at", gg_dta$time[1], "year")
            )
        }

      ## ---- Classification individual plot ----------------------------
      } else if (family == "class") {
        gg_plt[[ind]] <- gg_plt[[ind]] +
          ggplot2::labs(x = h_name, y = "Predicted")

        if (sum(colnames(gg_dta) == "outcome") == 0) {
          # Single-outcome (binary) classification
          if (ccls_var == "numeric") {
            if (points) {
              gg_plt[[ind]] <- gg_plt[[ind]] +
                ggplot2::geom_point(
                  ggplot2::aes(
                    x = .data$var,
                    y = .data$yhat,
                    color = .data$yvar,
                    shape = .data$yvar
                  ),
                  ...
                )
            } else {
              gg_plt[[ind]] <- gg_plt[[ind]] +
                ggplot2::geom_smooth(
                  ggplot2::aes(x = .data$var, y = .data$yhat),
                  color = "black",
                  linetype = 2,
                  ...
                )
            }
            if (smooth) {
              gg_plt[[ind]] <- gg_plt[[ind]] +
                ggplot2::geom_smooth(...)
            }
          } else {
            # Factor predictor: jitter + boxplot coloured by observed class
            gg_plt[[ind]] <- gg_plt[[ind]] +
              ggplot2::geom_jitter(
                ggplot2::aes(
                  x = .data$var,
                  y = .data$yhat,
                  color = .data$yvar,
                  shape = .data$yvar
                ),
                ...
              ) +
              ggplot2::geom_boxplot(
                ggplot2::aes(x = .data$var, y = .data$yhat),
                color = "grey",
                ...,
                outlier.shape = NA
              )
          }
        } else {
          # Multi-class: facet by outcome class
          if (ccls_var == "numeric") {
            gg_plt[[ind]] <- gg_plt[[ind]] +
              ggplot2::geom_point(
                ggplot2::aes(
                  x = .data$var,
                  y = .data$yhat,
                  color = .data$yvar,
                  shape = .data$yvar
                ),
                ...
              )
          } else {
            gg_plt[[ind]] <- gg_plt[[ind]] +
              ggplot2::geom_boxplot(
                ggplot2::aes(x = .data$var, y = .data$yhat),
                color = "grey",
                ...,
                outlier.shape = NA
              ) +
              ggplot2::geom_jitter(
                ggplot2::aes(
                  x = .data$var,
                  y = .data$yhat,
                  color = .data$yvar,
                  shape = .data$yvar
                ),
                ...
              )
          }

          gg_plt[[ind]] <- gg_plt[[ind]] +
            ggplot2::facet_grid(~outcome)
        }

      ## ---- Regression individual plot --------------------------------
      } else {
        # assume regression
        gg_plt[[ind]] <- gg_plt[[ind]] +
          ggplot2::labs(x = h_name, y = "Predicted")
        if (ccls_var == "numeric") {
          if (points) {
            gg_plt[[ind]] <- gg_plt[[ind]] +
              ggplot2::geom_point(ggplot2::aes(x = .data$var, y = .data$yhat), ...)
          } else {
            gg_plt[[ind]] <- gg_plt[[ind]] +
              ggplot2::geom_smooth(ggplot2::aes(x = .data$var, y = .data$yhat), ...)
          }
          if (smooth) {
            gg_plt[[ind]] <- gg_plt[[ind]] +
              ggplot2::geom_smooth(ggplot2::aes(x = .data$var, y = .data$yhat), ...)
          }
        } else {
          # Factor predictor: boxplot + jitter
          gg_plt[[ind]] <- gg_plt[[ind]] +
            ggplot2::geom_boxplot(
              ggplot2::aes(x = .data$var, y = .data$yhat),
              color = "grey",
              ...,
              outlier.shape = NA
            ) +
            ggplot2::geom_jitter(ggplot2::aes(x = .data$var, y = .data$yhat), ...)
        }
      }

      # Restore the original column name before the next iteration
      colnames(gg_dta)[ch_indx] <- h_name
    }
    # Return a single ggplot when only one variable was requested
    if (lng == 1) {
      gg_plt <- gg_plt[[1]]
    }
  }
  return(gg_plt)
}
