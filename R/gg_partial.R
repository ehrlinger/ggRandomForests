####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' Partial variable dependence object
#'
#' @description The \code{\link[randomForestSRC]{plot.variable}} function
#' returns a list of either marginal variable dependence or partial variable
#' dependence data from a \code{\link[randomForestSRC]{rfsrc}} object.
#' The \code{gg_partial} function formulates the
#' \code{\link[randomForestSRC]{plot.variable}} output for partial plots
#' (where \code{partial=TRUE}) into a data object for creation of partial
#' dependence plots using the \code{\link{plot.gg_partial}} function.
#'
#' Partial variable dependence plots are the risk adjusted estimates of the
#' specified response as a function of a single covariate, possibly subsetted
#' on other covariates.
#'
#' An option \code{named} argument can name a column for merging multiple
#' plots together
#'
#' @param object the partial variable dependence data object from
#'   \code{\link[randomForestSRC]{plot.variable}} function
#' @param ... optional arguments
#'
#' @return \code{gg_partial} object. A \code{data.frame} or \code{list} of
#' \code{data.frames} corresponding the variables
#' contained within the \code{\link[randomForestSRC]{plot.variable}} output.
#'
#' @seealso \code{\link{plot.gg_partial}}
#' @seealso \code{\link[randomForestSRC]{plot.variable}}
#'
#' @importFrom parallel mclapply
#'
#' @references
#' Friedman, Jerome H. 2000. "Greedy Function Approximation: A Gradient
#' Boosting Machine." Annals of Statistics 29: 1189-1232."
#'
#' @examples
#' ## ------------------------------------------------------------
#' ## classification
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' ## iris "Petal.Width" partial dependence plot
#' ##
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' partial_iris <- plot.variable(rfsrc_iris,
#'   xvar.names = "Petal.Width",
#'   partial = TRUE
#' )
#'
#' gg_dta <- gg_partial(partial_iris)
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#' 
#' ## -------- air quality data
#' ## airquality "Wind" partial dependence plot
#' ##
#' rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality)
#' partial_airq <- plot.variable(rfsrc_airq,
#'   xvar.names = "Wind",
#'   partial = TRUE, show.plot = FALSE
#' )
#'
#' gg_dta <- gg_partial(partial_airq)
#' plot(gg_dta)
#' 
#' ## -------- Boston data
#' data(Boston, package = "MASS")
#' Boston$chas <- as.logical(Boston$chas)
#' rfsrc_boston <- rfsrc(medv ~ .,
#'   data = Boston,
#'   forest = TRUE,
#'   importance = TRUE,
#'   tree.err = TRUE,
#'   save.memory = TRUE
#' )
#'
#' nms <- rfsrc_boston$xvar.names
#' nms <- nms[nms != "chas"] 
#'
#' partial_boston <- plot.variable(rfsrc_boston,
#'   xvar = nms, 
#'   sorted = FALSE,
#'   partial = TRUE,
#'   show.plots = FALSE
#' )
#' gg_dta <- gg_partial(partial_boston)
#' plot(gg_dta, panel = TRUE)
#' 
#' ## -------- mtcars data
#' rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
#'
#' partial_mtcars <- plot.variable(rfsrc_mtcars,
#'   sorted = FALSE,
#'   partial = TRUE,
#'   show.plots = FALSE
#' )
#'
#' gg_dta <- gg_partial(partial_mtcars)
#'
#' gg_dta.cat <- gg_dta
#' gg_dta.cat[["disp"]] <- gg_dta.cat[["wt"]] <- gg_dta.cat[["hp"]] <- NULL
#' gg_dta.cat[["drat"]] <- gg_dta.cat[["carb"]] <- gg_dta.cat[["qsec"]] <- NULL
#'
#' plot(gg_dta.cat, panel = TRUE, notch = TRUE)
#'
#' gg_dta[["cyl"]] <- gg_dta[["vs"]] <- gg_dta[["am"]] <- NULL
#' gg_dta[["gear"]] <- NULL
#' plot(gg_dta, panel = TRUE)
#' 
#'
#' ## ------------------------------------------------------------
#' ## survival examples
#' ## ------------------------------------------------------------
#' 
#' ## -------- veteran data
#' ## survival "age" partial variable dependence plot
#' ##
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., veteran,
#'   nsplit = 10,
#'   ntree = 100
#' )
#'
#' ## 30 day partial plot for age
#' partial_veteran <- plot.variable(rfsrc_veteran,
#'   surv.type = "surv",
#'   partial = TRUE, time = 30,
#'   show.plots = FALSE
#' )
#'
#' gg_dta <- gg_partial(partial_veteran)
#' plot(gg_dta, panel = TRUE)
#'
#' gg_dta.cat <- gg_dta
#' gg_dta[["celltype"]] <- gg_dta[["trt"]] <- gg_dta[["prior"]] <- NULL
#' plot(gg_dta, panel = TRUE)
#'
#' gg_dta.cat[["karno"]] <- gg_dta.cat[["diagtime"]] <-
#'   gg_dta.cat[["age"]] <- NULL
#' plot(gg_dta.cat, panel = TRUE, notch = TRUE)
#'
#' gg_dta <- lapply(partial_veteran, ggRandomForests::gg_partial)
#' gg_dta <- combine.gg_partial(gg_dta[[1]], gg_dta[[2]])
#'
#' plot(gg_dta[["karno"]])
#' plot(gg_dta[["celltype"]])
#'
#' gg_dta.cat <- gg_dta
#' gg_dta[["celltype"]] <- gg_dta[["trt"]] <- gg_dta[["prior"]] <- NULL
#' plot(gg_dta, panel = TRUE)
#'
#' gg_dta.cat[["karno"]] <- gg_dta.cat[["diagtime"]] <-
#'   gg_dta.cat[["age"]] <- NULL
#' plot(gg_dta.cat, panel = TRUE, notch = TRUE)
#'
#' ## ------------------------------------------------------------
#' ## -------- pbc data
#' # We need to create this dataset
#' data(pbc, package = "randomForestSRC", )
#' # For whatever reason, the age variable is in days... makes no sense to me
#' pbc <- r_data_types(pbc)
#' 
#' # Convert age to years
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
#' # ========
#' # build the forest:
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
#'
#' xvar <- rfsrc_pbc$xvar.names
#'
#' # Convert all partial plots to gg_partial objects
#' gg_dta <- lapply(partial_pbc, gg_partial)
#'
#' # Combine the objects to get multiple time curves
#' # along variables on a single figure.
#' pbc_ggpart <- combine.gg_partial(gg_dta[[1]], gg_dta[[2]],
#'   lbls = c("1 Year", "3 Years")
#' )
#'
#' summary(pbc_ggpart)
#' class(pbc_ggpart[["bili"]])
#'
#' # Plot the highest ranked variable, by name.
#' # plot(pbc_ggpart[["bili"]])
#'
#' # Create a temporary holder and remove the stage and edema data
#' ggpart <- pbc_ggpart
#' ggpart$edema <- NULL
#'
#' # Panel plot the remainder.
#' plot(ggpart, panel = TRUE)
#'
#' plot(pbc_ggpart[["edema"]])
#' 
#' @aliases gg_partial gg_partial_list gg_partial.rfsrc
#' @name gg_partial
#' @aliases gg_partial_list
#' @export
gg_partial <- function(object, ...) {
  print(class(object))
  UseMethod("gg_partial",  object)
}

#' @export
gg_partial.rfsrc <- function(object,
                             ...) {
  if (!inherits(object, "plot.variable")) {
    stop(
      paste(
        "gg_partial expects a plot.variable object, ",
        "Run plot.variable with partial=TRUE"
      )
    )
  }

  # If we pass it a plot.variable output, without setting partial=TRUE,
  # We'll want a gg_variable object.
  if (!object$partial) {
    invisible(gg_variable(object, ...))
  }

  call_v <- match.call(expand.dots = TRUE)
  named <- eval.parent(call_v$named)

  # How many variables
  n_var <- length(object$pData)

  # Create a list of data
  gg_dta <- parallel::mclapply(seq_len(n_var), function(ind) {
    if (length(object$pData[[ind]]$x.uniq) ==
      length(object$pData[[ind]]$yhat)) {
      if (object$family == "surv") {
        # Survival family has weird standard errors because of non-normal
        # transforms
        data.frame(cbind(
          yhat = object$pData[[ind]]$yhat,
          x = object$pData[[ind]]$x.uniq
        ))
      } else {
        # We assume RC forests are "normal"
        data.frame(
          cbind(
            yhat = object$pData[[ind]]$yhat,
            x = object$pData[[ind]]$x.uniq,
            se = object$pData[[ind]]$yhat.se
          )
        )
      }
    } else {
      x <- rep(
        as.character(object$pData[[ind]]$x.uniq),
        rep(object$n, object$pData[[ind]]$n.x)
      )
      tmp <- data.frame(cbind(yhat = x, x = x))
      tmp$x <- factor(tmp$x)
      tmp$yhat <- object$pData[[ind]]$yhat
      tmp
    }
  })

  names(gg_dta) <- object$xvar.names

  # name the data, so labels come out correctly.
  for (ind in seq_len(n_var)) {
    colnames(gg_dta[[ind]])[which(colnames(gg_dta[[ind]]) == "x")] <-
      object$xvar.names[ind]
    if (!missing(named)) {
      gg_dta[[ind]]$id <- named
    }
    class(gg_dta[[ind]]) <- c(
      "gg_partial", class(gg_dta[[ind]]),
      object$family
    )
  }

  if (n_var == 1) {
    # If there is only one, no need for a list
    invisible(gg_dta[[1]])
  } else {
    # otherwise, add a class label so we can handle it correctly.
    class(gg_dta) <- c("gg_partial_list", class(gg_dta))
    invisible(gg_dta)
  }
}