#' Data structures for stratified partial coplots
#'
#' @param object \code{\link[randomForestSRC]{rfsrc}} object
#' @param xvar list of partial plot variables
#' @param groups vector of stratification variable.
#' @param surv_type for survival random forests,  c("mort", "rel.freq", "surv", 
#' "years.lost", "cif", "chf")
#' @param time vector of time points for survival random forests partial plots.
#' @param ... extra arguments passed to \code{\link[randomForestSRC]{plot.variable}} function
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
#' # Load the forest
#' data(rfsrc_pbc, package="ggRandomForests")
#' 
#' # Create the variable plot.
#' ggvar <- gg_variable(rfsrc_pbc, time = 1)
#' 
#' # Find intervals with similar number of observations.
#' copper_cts <-quantile_pts(ggvar$copper, groups = 6, intervals = TRUE)
#' 
#' # Create the conditional groups and add to the gg_variable object
#' copper_grp <- cut(ggvar$copper, breaks = copper_cts)
#' 
#' \dontrun{
#' ## We would run this, but it's expensive 
#' partial_coplot_pbc <- gg_partial_coplot(rfsrc_pbc, xvar = "bili", 
#'                                          groups = copper_grp, 
#'                                          surv_type = "surv", 
#'                                          time = 1, 
#'                                          show.plots = FALSE)
#' }
#' ## so load the cached set
#' data(partial_coplot_pbc, package="ggRandomForests")
#' 
#' # Partial coplot
#' plot(partial_coplot_pbc) #, se = FALSE)
#'  
#' 
#' @export
gg_partial_coplot.rfsrc <- function(object, 
                                    xvar, 
                                    groups, 
                                    surv_type=c("mort", 
                                                "rel.freq",
                                                "surv",
                                                "years.lost",
                                                "cif",
                                                "chf"), 
                                    time,
                                    ...){
  
  # Some sanity checks:
  
  ## Check that the input obect is of the correct type.
  if (inherits(object, "rfsrc") == FALSE){
    stop(paste("This function only works for Forests grown with the",
               "randomForestSRC package."))
  }
  
  if (is.null(object$forest)) {
    stop(paste("The function requires the \"forest = TRUE\"", 
               "attribute when growing the randomForest"))
  }
  
  #---
  # Unpack the elipse argument.
  arg_list <- list(...)
  
  surv_type <- match.arg(surv_type)
  
  #family <- object$family
  
  # Get the training data to work with...
  dta.train  <- object$xvar
  
  # Make sure all xvars are in the training set.
  
  # If we don't have a groups variable, we may have a subsets in the ellipse list.
  if(missing(groups)){
    if(is.null(arg_list$subset))
      stop(paste("partial_coplot requires a groups argument to",
                 "stratify the partial plots."))
    else{
      # We may be able to coherce a groups argument from subset that is normally
      # passed to plot.variable.
      
    }
  }
  # Make sure we have more than one group in the subset list.
  
  # If groups is a variable name, check for existance.
  
  # if not, make sure it has the correct dimension (nrow(rfsrc$xvar))
  
  
  dta.train$group <- groups
  
  # make groups a factor
  
  # how many curves per coplot?
  lvl <- levels(groups)
  lng <- length(lvl)
  
  # Create the subsets for the plot.variable function
  sbst <- parallel::mclapply(1:lng, function(ind){
    st <- which(dta.train$group == levels(groups)[ind])
    if(length(st) == 0) NULL
    else st
  })
  
  # Collapse the subset list to interesting items (those with observations)
  # If you work backwards, you do extra tests, but it 
  # cuts the correct items. Cute.
  for (ind in lng:1){
    if(is.null(sbst[[ind]])){
      sbst[[ind]] <- NULL
      
      # reset the levels, so we can label things later
      lvl <- lvl[-ind]
    }
  }
  
  # If survival family, make sure we have a time and surv_type.
  # if not default to time=1, and surv_type="surv" with warning.
  #   
  #   # If we got a surv.type instead of surv_type, let's use that.
  #   if(!is.null(arg_list$surv.type)){
  #     
  #   }
  #   # If we got a surv.type instead of surv_type, let's use that.
  #   if(is.null(arg_list$show.plots)){
  #     arg_list$show.plots <- FALSE
  #   }
  # what about multiple time points?
  
  # This will return a list of subseted partial plots, one for each group, 
  # all variables in xvar.
  pdat.partlist <- lapply(1:length(sbst), function(ind){
    randomForestSRC::plot.variable(object, surv.type=surv_type, time=time,
                  subset = sbst[[ind]],
                  xvar.names=xvar, partial=TRUE, ...)
  })
  
  ## Make them all gg_partials.
  gg_part <- parallel::mclapply(pdat.partlist, gg_partial)
  
  ## With the subsets marked for plotting
  for(ind in 1:length(gg_part)){
    gg_part[[ind]]$group <- lvl[ind]
  }
  
  ## Collapse the grouped data into a single structure
  gg_merge <- do.call(rbind, gg_part)
  
  ## Make the group variable a factor (order on unique levels)
  gg_merge$group <- factor(gg_merge$group, levels=unique(gg_merge$group))
  
  # Make is so we can easily plot it with an S3 function.
  class(gg_merge) <- c("gg_partial_coplot", class(gg_merge))
  gg_merge
}
#' @export
gg_partial_coplot <- gg_partial_coplot.rfsrc
# gg_partial_coplot <- function (object, ...) {
#   UseMethod("gg_partial_coplot", object)
# }
