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
#' \dontrun{
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' data(rfsrc_iris, package="ggRandomForests")
#' gg_dta<- gg_rfsrc(rfsrc_iris)
#'
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' ## -------- air quality data
#' rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' gg_dta<- gg_rfsrc(rfsrc_airq)
#'
#' plot(gg_dta)
#'
#' ## -------- Boston data
#' data(Boston, package = "MASS")
#' rfsrc_boston <- randomForestSRC::rfsrc(medv~., Boston)
#' 
#' plot(rfsrc_boston)
#'
#' ## -------- mtcars data
#' rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
#' gg_dta<- gg_rfsrc(rfsrc_mtcars)
#'
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## -------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' gg_dta <- gg_rfsrc(rfsrc_veteran)
#' plot(gg_dta)
#'
#' gg_dta <- gg_rfsrc(rfsrc_veteran, conf.int=.95)
#' plot(gg_dta)
#'
#' gg_dta <- gg_rfsrc(rfsrc_veteran, by="trt")
#' plot(gg_dta)
#'
#' ## -------- pbc data
#' #' # We need to create this dataset
#' data(pbc, package = "randomForestSRC",) 
#' # For whatever reason, the age variable is in days... makes no sense to me
#' for (ind in seq_len(dim(pbc)[2])) {
#'  if (!is.factor(pbc[, ind])) {
#'    if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'      if (sum(range(pbc[, ind], na.rm = TRUE) == c(0, 1)) == 2) {
#'        pbc[, ind] <- as.logical(pbc[, ind])
#'      }
#'    }
#'  } else {
#'    if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'      if (sum(sort(unique(pbc[, ind])) == c(0, 1)) == 2) {
#'        pbc[, ind] <- as.logical(pbc[, ind])
#'      }
#'      if (sum(sort(unique(pbc[, ind])) == c(FALSE, TRUE)) == 2) {
#'        pbc[, ind] <- as.logical(pbc[, ind])
#'      }
#'    }
#'  }
#'  if (!is.logical(pbc[, ind]) &
#'      length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 5) {
#'    pbc[, ind] <- factor(pbc[, ind])
#'  }
#' }
#' #Convert age to years
#' pbc$age <- pbc$age / 364.24
#'
#' pbc$years <- pbc$days / 364.24
#' pbc <- pbc[, -which(colnames(pbc) == "days")]
#' pbc$treatment <- as.numeric(pbc$treatment)
#' pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
#' pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
#' pbc$treatment <- factor(pbc$treatment)
#' dta_train <- pbc[-which(is.na(pbc$treatment)), ]
#' # Create a test set from the remaining patients
#' pbc_test <- pbc[which(is.na(pbc$treatment)), ]
#'
#' #========
#' # build the forest:
#' rfsrc_pbc <- randomForestSRC::rfsrc(
#'   Surv(years, status) ~ .,
#'  dta_train,
#'  nsplit = 10,
#'  na.action = "na.impute",
#'  forest = TRUE,
#'  importance = TRUE,
#'  save.memory = TRUE
#' )
#' 
#' gg_dta <- gg_rfsrc(rfsrc_pbc)
#' plot(gg_dta)
#'
#' gg_dta <- gg_rfsrc(rfsrc_pbc, conf.int=.95)
#' plot(gg_dta)
#'
#' gg_dta <- gg_rfsrc(rfsrc_pbc, by="treatment")
#' plot(gg_dta)
#'
#'
#' }
#' @importFrom ggplot2 ggplot aes_string geom_step geom_ribbon labs
#' geom_point geom_jitter geom_boxplot theme element_blank
#' @importFrom tidyr gather
#'
#' @export
plot.gg_rfsrc <- function(x, ...) {
  gg_dta <- x
  
  # Unpack argument list
  arg_set <- list(...)
  
  ## rfsrc places the class in position 1.
  if (inherits(gg_dta, "rfsrc"))
    gg_dta <- gg_rfsrc(gg_dta)
  
  ## Classification forest?
  if (inherits(gg_dta, "class") ||
      inherits(gg_dta, "classification")) {
    if (ncol(gg_dta) < 3) {
      gg_plt <- ggplot(gg_dta) +
        geom_jitter(aes_string(
          x = 1,
          y = colnames(gg_dta)[1],
          color = colnames(gg_dta)[2],
          shape = colnames(gg_dta)[2]
        ),
        ...) +
        geom_boxplot(
          aes_string(x = 1, y = colnames(gg_dta)[1]),
          outlier.colour = "transparent",
          fill = "transparent",
          notch = TRUE,
          ...
        ) +
        theme(axis.ticks = element_blank(), axis.text.x = element_blank())
    } else {
      gathercols <- colnames(gg_dta)[-which(colnames(gg_dta) == "y")]
      gg_dta_mlt <-
        tidyr::gather(gg_dta, "variable", "value", gathercols)
      
      gg_plt <-
        ggplot(gg_dta_mlt, aes_string(x = "variable", y = "value")) +
        geom_jitter(aes_string(color = "y", shape = "y"), alpha = .5)
    }
    gg_plt <- gg_plt + labs(y = "Predicted (%)", x = "")
    
    
  } else if (inherits(gg_dta, "surv")) {
    # Check for conf.int calculations
    if ("lower" %in% colnames(gg_dta)) {
      if (is.null(arg_set$alpha)) {
        alph <- .3
      } else {
        alph <- arg_set$alpha * .5
        arg_set$alpha <- NULL
      }
      
      if ("group" %in% colnames(gg_dta)) {
        gg_plt <- ggplot(gg_dta) +
          geom_ribbon(
            aes_string(
              x = "value",
              ymin = "lower",
              ymax = "upper",
              fill = "group"
            ),
            alpha = alph,
            ...
          ) +
          geom_step(aes_string(
            x = "value",
            y = "median",
            color = "group"
          ), ...)
      } else {
        gg_plt <- ggplot(gg_dta) +
          geom_ribbon(aes_string(
            x = "value",
            ymin = "lower",
            ymax = "upper"
          ),
          alpha = alph) +
          geom_step(aes_string(x = "value", y = "median"), ...)
      }
    } else {
      # Lines by observation
      gg_plt <- ggplot(gg_dta,
                       aes_string(
                         x = "variable",
                         y = "value",
                         col = "event",
                         by = "obs_id"
                       )) +
        geom_step(...)
    }
    
    gg_plt <- gg_plt  +
      labs(x = "time (years)", y = "Survival (%)")
    
    
  } else if (inherits(gg_dta, "regr") ||
             inherits(gg_dta, "regression")) {
    if ("group" %in% colnames(gg_dta)) {
      gg_plt <- ggplot(gg_dta, aes_string(x = "group", y = "yhat"))
    } else {
      gg_plt <- ggplot(gg_dta, aes_string(x = 1, y = "yhat"))
    }
    
    gg_plt <- gg_plt +
      geom_jitter(, ...) +
      geom_boxplot(outlier.colour = "transparent",
                   fill = "transparent",
                   notch = TRUE,
                   ...) +
      labs(y = "Predicted Value", x = colnames(gg_dta)[2]) +
      theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  } else {
    stop(paste(
      "Plotting for ",
      class(gg_dta)[2],
      " randomForestSRC is not yet implemented."
    ))
  }
  return(gg_plt)
}
