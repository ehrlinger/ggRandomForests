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
#' combine two gg_partial objects
#'
#' @description
#' The \code{combine.gg_partial} function assumes the two 
#' \code{\link{gg_partial}} objects were generated from the same 
#' \code{\link[randomForestSRC]{rfsrc}} object. So, the function joins along the
#' \code{\link{gg_partial}} list item names (one per partial plot variable). 
#' Further, we combine the two \code{\link{gg_partial}} objects along the group 
#' variable.
#'
#' Hence, to join three \code{\link{gg_partial}} objects together (i.e. for
#' three different time points from a survival random forest) would require
#' two \code{combine.gg_partial} calls: One to join the first two
#' \code{\link{gg_partial}} object, and one to append the third
#' \code{\link{gg_partial}} object to the output from the first call.
#' The second call will append a single \code{lbls} label to the
#' \code{\link{gg_partial}} object.
#'
#' @param x \code{\link{gg_partial}}  object
#' @param y \code{\link{gg_partial}}  object
#' @param lbls vector of 2 strings to label the combined data.
#' @param ... not used
#'
#' @return \code{\link{gg_partial}} or \code{gg_partial_list} based on
#'  class of x and y.
#'
#' @aliases combine.gg_partial combine.gg_partial_list
#'
#' @importFrom parallel mclapply
#'
#' @examples
#' \dontrun{
#' # We need to create this dataset
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
#'pbc$age <- pbc$age / 364.24
#'
#'pbc$years <- pbc$days / 364.24
#'pbc <- pbc[, -which(colnames(pbc) == "days")]
#'pbc$treatment <- as.numeric(pbc$treatment)
#'pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
#'pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
#'pbc$treatment <- factor(pbc$treatment)

#'dta_train <- pbc[-which(is.na(pbc$treatment)), ]
#'# Create a test set from the remaining patients
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
#'
#' # A list of 2 plot.variable objects
#' length(partial_pbc)
#' class(partial_pbc)
#'
#' class(partial_pbc[[1]])
#' class(partial_pbc[[2]])
#'
#' # Create gg_partial objects
#' ggPrtl.1 <- gg_partial(partial_pbc[[1]])
#' ggPrtl.2 <- gg_partial(partial_pbc[[2]])
#'
#' # Combine the objects to get multiple time curves
#' # along variables on a single figure.
#' ggpart <- combine.gg_partial(ggPrtl.1, ggPrtl.2,
#'                              lbls = c("1 year", "3 years"))
#'
#' # Plot each figure separately
#' plot(ggpart)
#'
#' # Get the continuous data for a panel of continuous plots.
#' ggcont <- ggpart
#' ggcont$edema <- ggcont$ascites <- ggcont$stage <- NULL
#' plot(ggcont, panel=TRUE)
#'
#' # And the categorical for a panel of categorical plots.
#' nms <- colnames(sapply(ggcont, function(st) {st}))
#' for(ind in nms) {
#'    ggpart[[ind]] <- NULL
#' }
#' plot(ggpart, panel=TRUE)
#'
#' }
#' @export
combine.gg_partial <- function(x, y, lbls, ...) {
  return(combine.gg_partial_list(x, y, lbls, ...))
}

combine.gg_partial_list <- function(x, y, lbls, ...) {
  if (inherits(x, "plot.variable"))
    x <- gg_partial(x)
  if (inherits(y, "plot.variable"))
    y <- gg_partial(y)
  
  if ((!inherits(x, "gg_partial_list") &&
       !inherits(x, "gg_partial"))  &&
      (!inherits(y, "gg_partial_list") &&
       !inherits(y, "gg_partial"))) {
    stop(
      paste(
        "combine.gg_partial expects either a",
        "ggRandomForests::gg_partial or ",
        "randomForestSRC::plot.variable object"
      )
    )
  }
  
  if (missing(lbls)) {
    lbls <- c("x1", "x2")
  }
  ### !!TODO!! check for lbls length
  
  cls <- class(x)
  
  ### We need to check for the case when x and y already have
  ### a group column,
  if (is.null(x[[1]]$group))
    x <- parallel::mclapply(x, function(st) {
      st$group <- lbls[1]
      st
    })
  
  if (is.null(y[[1]]$group)) {
    ind_l <- length(lbls)
    y <- parallel::mclapply(y, function(st) {
      st$group <- lbls[ind_l]
      st
    })
  }
  # By names
  nm <- names(x)
  
  gg_dta <- parallel::mclapply(nm, function(ind) {
    rbind(x[[ind]], y[[ind]])
  })
  
  names(gg_dta) <- names(x)
  class(gg_dta) <- cls
  return(gg_dta)
}
