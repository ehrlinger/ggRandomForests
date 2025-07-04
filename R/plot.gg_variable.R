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
#' @return A single \code{ggplot} object, or list of \code{ggplot} objects
#'
#' @references Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#'
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, Rnews,
#' 7(2):25-31.
#'
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression
#' and Classification (RF-SRC), R package version 1.4.
#'
#'
#' @importFrom ggplot2 .data
#' @examples
#' \dontrun{
#' ## ------------------------------------------------------------
#' ## classification
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' ## iris
#' # rfsrc_iris <- rfsrc(Species ~., data = iris)
#' data(rfsrc_iris, package = "ggRandomForests")
#'
#' gg_dta <- gg_variable(rfsrc_iris)
#' plot(gg_dta, xvar = "Sepal.Width")
#' plot(gg_dta, xvar = "Sepal.Length")
#'
#' ## !! TODO !! this needs to be corrected
#' plot(gg_dta,
#'   xvar = rfsrc_iris$xvar.names,
#'   panel = TRUE, se = FALSE
#' )
#'
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#' ## -------- air quality data
#' # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality)
#' data(rfsrc_airq, package = "ggRandomForests")
#' gg_dta <- gg_variable(rfsrc_airq)
#'
#' # an ordinal variable
#' gg_dta[, "Month"] <- factor(gg_dta[, "Month"])
#'
#' plot(gg_dta, xvar = "Wind")
#' plot(gg_dta, xvar = "Temp")
#' plot(gg_dta, xvar = "Solar.R")
#'
#' plot(gg_dta, xvar = c("Solar.R", "Wind", "Temp", "Day"), panel = TRUE)
#'
#' plot(gg_dta, xvar = "Month", notch = TRUE)
#'
#' ## -------- motor trend cars data
#' # rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
#' data(rfsrc_mtcars, package = "ggRandomForests")
#' gg_dta <- gg_variable(rfsrc_mtcars)
#'
#' # mtcars$cyl is an ordinal variable
#' gg_dta$cyl <- factor(gg_dta$cyl)
#' gg_dta$am <- factor(gg_dta$am)
#' gg_dta$vs <- factor(gg_dta$vs)
#' gg_dta$gear <- factor(gg_dta$gear)
#' gg_dta$carb <- factor(gg_dta$carb)
#'
#' plot(gg_dta, xvar = "cyl")
#'
#' # Others are continuous
#' plot(gg_dta, xvar = "disp")
#' plot(gg_dta, xvar = "hp")
#' plot(gg_dta, xvar = "wt")
#'
#' # panel
#' plot(gg_dta, xvar = c("disp", "hp", "drat", "wt", "qsec"), panel = TRUE)
#' plot(gg_dta, xvar = c("cyl", "vs", "am", "gear", "carb"), panel = TRUE)
#'
#' ## -------- Boston data
#'
#' ## ------------------------------------------------------------
#' ## survival examples
#' ## ------------------------------------------------------------
#' ## -------- veteran data
#' ## survival
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., veteran,
#'   nsplit = 10,
#'   ntree = 100
#' )
#'
#' # get the 1 year survival time.
#' gg_dta <- gg_variable(rfsrc_veteran, time = 90)
#'
#' # Generate variable dependance plots for age and diagtime
#' plot(gg_dta, xvar = "age")
#' plot(gg_dta, xvar = "diagtime")
#'
#' # Generate coplots
#' plot(gg_dta, xvar = c("age", "diagtime"), panel = TRUE)
#'
#' # If we want to compare survival at different time points, say 30, 90 day
#' # and 1 year
#' gg_dta <- gg_variable(rfsrc_veteran, time = c(30, 90, 365))
#'
#' # Generate variable dependance plots for age and diagtime
#' plot(gg_dta, xvar = "age")
#' plot(gg_dta, xvar = "diagtime")
#'
#' # Generate coplots
#' plot(gg_dta, xvar = c("age", "diagtime"), panel = TRUE)
#'
#' ## -------- pbc data
#' }
#'
#' @export
plot.gg_variable <- function(x,
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

  # Set the family to decide how to plot the data.
  family <- "class"
  if (inherits(gg_dta, "surv")) {
    family <- "surv"
  } else if (inherits(gg_dta, "regr")) {
    family <- "regr"
  } else if (inherits(gg_dta, "regression")) {
    family <- "regr"
  }

  # These may be dangerous because they work on column names only.
  if (sum(colnames(gg_dta) == "event") != 0) {
    family <- "surv"
  }

  # Same here, but it's how I know there are multiple classes right now.
  if (length(grep("yhat.", colnames(gg_dta))) > 0) {
    # We have a classification forest with multiple outcomes.
    if (length(grep("yhat.", colnames(gg_dta))) == 2) {
      # For the case of two, we are only interested in the TRUE, not FALSE.
      gg_dta <- gg_dta[, -grep("yhat.", colnames(gg_dta))[1]]
      colnames(gg_dta)[grep("yhat.", colnames(gg_dta))] <- "yhat"
    } else {
      # Else we want to split and duplicate the data... make it long format.
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

  # If we don't know what to plot...
  if (missing(xvar)) {
    # We need to remove response variables here
    cls <- c(
      grep("yhat", colnames(gg_dta)),
      grep("event", colnames(gg_dta)),
      grep("time", colnames(gg_dta))
    )
    xvar <- colnames(gg_dta)[-cls]
  }

  lng <- length(xvar)

  # Predictor variables
  wch_x_var <- which(colnames(gg_dta) %in% xvar)

  # Two types of variables, categorical and continuous.
  # By default, if there are less than 10 unique values,
  # assume categorical.
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

  # Check for categorical X values...
  ccls <- sapply(gg_dta, class)
  ccls[which(ccls == "integer")] <- "numeric"

  if (panel) {
    # The different families are driven by the response variables.
    ## Survival plots
    if (family == "surv") {
      ## response variables.
      wch_y_var <-
        which(colnames(gg_dta) %in% c("event", "yhat", "time"))

      # Handle categorical and continuous differently...
      tmp_dta <- gg_dta[, c(wch_y_var, wch_x_var)]
      gathercols <-
        colnames(tmp_dta)[-which(colnames(tmp_dta) %in%
          c("time", "event", "yhat"))]
      gg_dta_mlt <-
        tidyr::gather(tmp_dta, "variable", "value", gathercols)

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
        # Check if there are numeric variables here...
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
      # Panels for
      # This will work for regression and binary classification... maybe.
      ## Create a panel plot
      wch_y_var <- which(colnames(gg_dta) %in% c("yhat"))

      if (family == "class") {
        wch_y_var <- c(wch_y_var, which(colnames(gg_dta) == "yvar"))
        tmp_dta <- gg_dta[, c(wch_y_var, wch_x_var)]
        gathercols <-
          colnames(tmp_dta)[-which(colnames(tmp_dta) %in% c("yvar", "yhat"))]
        gg_dta_mlt <-
          tidyr::gather(
            tmp_dta, "variable", "value",
            tidyr::all_of(gathercols)
          )
      } else {
        wch_y_var <- c(wch_y_var, which(colnames(gg_dta) == "yvar"))
        tmp_dta <- gg_dta[, c(wch_y_var, wch_x_var)]
        gathercols <-
          colnames(tmp_dta)[-which(colnames(tmp_dta) == "yhat")]
        gg_dta_mlt <-
          tidyr::gather(
            tmp_dta, "variable", "value",
            tidyr::all_of(gathercols)
          )
      }
      gg_dta_mlt$variable <-
        factor(gg_dta_mlt$variable, levels = xvar)

      # If these are all continuous...
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
        # Check if there are numberic variables here...
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
      if (family != "class") {
        if (points) {
          gg_plt <- ggplot2::ggplot(
            gg_dta_mlt,
            ggplot2::aes(x = .data$value, y = .data$yhat)
          ) +
            ggplot2::geom_point(...)
        } else {
          gg_plt <- ggplot2::ggplot(
            gg_dta_mlt,
            ggplot2::aes(x = .data$value, y = .data$yhat)
          ) +
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
  } else {
    # Don't want a panel, but a single plot or list of plots.
    gg_plt <- vector("list", length = lng)

    for (ind in 1:lng) {
      ch_indx <- which(colnames(gg_dta) == xvar[ind])
      h_name <- colnames(gg_dta)[ch_indx]
      colnames(gg_dta)[ch_indx] <- "var"
      ccls <- class(gg_dta[, "var"])
      ccls[which(ccls == "integer")] <- "numeric"

      gg_plt[[ind]] <- ggplot2::ggplot(gg_dta)

      if (family == "surv") {
        gg_plt[[ind]] <- gg_plt[[ind]] +
          ggplot2::labs(x = h_name, y = "Survival")

        if (ccls == "numeric") {
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
      } else if (family == "class") {
        gg_plt[[ind]] <- gg_plt[[ind]] +
          ggplot2::labs(x = h_name, y = "Predicted")

        if (sum(colnames(gg_dta) == "outcome") == 0) {
          if (ccls == "numeric") {
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
          if (ccls == "numeric") {
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
      } else {
        # assume regression
        gg_plt[[ind]] <- gg_plt[[ind]] +
          ggplot2::labs(x = h_name, y = "Predicted")
        if (ccls == "numeric") {
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

      # Replace the original colname
      colnames(gg_dta)[ch_indx] <- h_name
    }
    if (lng == 1) {
      gg_plt <- gg_plt[[1]]
    }
  }
  return(gg_plt)
}
