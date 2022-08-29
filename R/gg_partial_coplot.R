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
#' Data structures for stratified partial coplots
#'
#' @param object \code{\link[randomForestSRC]{rfsrc}} object
#' @param xvar list of partial plot variables
#' @param groups vector of stratification variable.
#' @param surv_type for survival random forests,  c("mort", "rel.freq", "surv",
#' "years.lost", "cif", "chf")
#' @param time vector of time points for survival random forests partial plots.
#' @param show_plots boolean passed to 
#'  \code{\link[randomForestSRC]{plot.variable}} show.plots argument.
#' @param ... extra arguments passed to
#'  \code{\link[randomForestSRC]{plot.variable}} function
#'
#' @return \code{gg_partial_coplot} object. An subclass of a
#' \code{\link{gg_partial_list}} object
#'
#' @aliases gg_partial_coplot
#'
#' @importFrom randomForestSRC plot.variable
#' @importFrom parallel mclapply
#'
#' @examples
#' \dontrun{
#' 
#' ## ------------------------------------------------------------
#' ## -------- pbc data# We need to create this dataset
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
#'  pbc_test <- pbc[which(is.na(pbc$treatment)), ]
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
#' # Create the variable plot.
#' ggvar <- gg_variable(rfsrc_pbc, time = 1)
#'
#' # Find intervals with similar number of observations.
#' copper_cts <- quantile_pts(ggvar$copper, groups = 6, intervals = TRUE)
#'
#' # Create the conditional groups and add to the gg_variable object
#' copper_grp <- cut(ggvar$copper, breaks = copper_cts)
#'
#' ## We would run this, but it's expensive
#' partial_coplot_pbc <- gg_partial_coplot(rfsrc_pbc, xvar = "bili",
#'                                          groups = copper_grp,
#'                                          surv_type = "surv",
#'                                          time = 1,
#'                                          show.plots = FALSE)
#'
#' # Partial coplot
#' plot(partial_coplot_pbc) #, se = FALSE)
#'}
#'
#' @export
gg_partial_coplot.rfsrc <- function(object,
                                    xvar,
                                    groups,
                                    surv_type = c("mort",
                                                  "rel.freq",
                                                  "surv",
                                                  "years.lost",
                                                  "cif",
                                                  "chf"),
                                    time,
                                    show_plots = FALSE,
                                    ...) {
  # Some sanity checks:
  
  ## Check that the input obect is of the correct type.
  if (inherits(object, "rfsrc") == FALSE) {
    stop(
      paste(
        "This function only works for Forests grown with the",
        "randomForestSRC package."
      )
    )
  }
  
  if (is.null(object$forest)) {
    stop(
      paste(
        "The function requires the \"forest = TRUE\"",
        "attribute when growing the randomForest"
      )
    )
  }
  
  #---
  # Unpack the elipse argument.
  arg_list <- list(...)
  
  surv_type <- match.arg(surv_type)
  
  # Get the training data to work with...
  dta_train  <- object$xvar
  
  # Make sure all xvars are in the training set.
  
  # If we don't have a groups variable, we may have a subsets in the
  # ellipse list.
  if (missing(groups)) {
    if (is.null(arg_list$subset)) { 
      stop(
        paste(
          "partial_coplot requires a groups argument to",
          "stratify the partial plots."
        )
      )
    } else {
      # We may be able to coherce a groups argument from subset that is
      # normally passed to plot.variable.
      
    }
  }
  # Make sure we have more than one group in the subset list.
  
  # If groups is a variable name, check for existance.
  
  # if not, make sure it has the correct dimension (nrow(rfsrc$xvar))
  
  # Check the length of groups...
  
  dta_train$group <- groups[seq_len(nrow(dta_train))]
  
  # make groups a factor
  lvl <- levels(groups)
  # how many curves per coplot?
  lng <- length(lvl)
  
  # Create the subsets for the plot.variable function
  sbst <- parallel::mclapply(1:lng, function(ind) {
    st <- which(dta_train$group == levels(groups)[ind])
    if (length(st) == 0)
      NULL
    else
      st
  })
  
  # Collapse the subset list to interesting items (those with observations)
  # If you work backwards, you do extra tests, but it
  # cuts the correct items. Cute.
  for (ind in lng:1) {
    if (is.null(sbst[[ind]])) {
      sbst[[ind]] <- NULL
      
      # reset the levels, so we can label things later
      lvl <- lvl[-ind]
    }
  }
  
  # This will return a list of subseted partial plots, one for each group,
  # all variables in xvar.
  pdat_partlist <- lapply(seq_len(length(sbst)), function(ind) {
    randomForestSRC::plot.variable(
      object,
      surv.type = surv_type,
      time = time,
      subset = sbst[[ind]],
      xvar.names = xvar,
      partial = TRUE, 
      show.plots = show_plots
    )
  })
  
  ## Make them all gg_partials.
  gg_part <- parallel::mclapply(pdat_partlist, gg_partial)
  
  ## With the subsets marked for plotting
  for (ind in seq_len(length(gg_part))) {
    gg_part[[ind]]$group <- lvl[ind]
  }
  
  ## Collapse the grouped data into a single structure
  gg_merge <- do.call(rbind, gg_part)
  
  ## Make the group variable a factor (order on unique levels)
  gg_merge$group <-
    factor(gg_merge$group, levels = unique(gg_merge$group))
  
  # Make is so we can easily plot it with an S3 function.
  class(gg_merge) <- c("gg_partial_coplot", class(gg_merge))
  gg_merge
}
#' @export
gg_partial_coplot.default <- gg_partial_coplot.rfsrc

#' @export
gg_partial_coplot <- function(object,
                              xvar,
                              groups,
                              surv_type = c("mort",
                                            "rel.freq",
                                            "surv",
                                            "years.lost",
                                            "cif",
                                            "chf"),
                              time,
                              ...) {
  UseMethod("gg_partial_coplot", object)
}


#' @export
gg_partial_coplot.randomForest <- function(object,
                                           xvar,
                                           groups,
                                           surv_type = NULL,
                                           time = NULL,
                                           ...) {
  stop("gg_partial_coplot is not yet support for randomForest objects")
}
