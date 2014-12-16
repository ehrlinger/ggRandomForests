#' Data structures for stratified partial coplots
#' 
#' 
#' 
#' @param object randomForestSRC::rfsrc object
#' @param xvar list of partial plot variables
#' @param groups vector of stratification variable.
#' @param surv_type for survival random forests,  c("mort", "rel.freq", "surv", "years.lost", "cif", "chf")
#' @param time vector of time points for survival random forests partial plots.
#' @param ... extra arguments passed to randomForestSRC::plot.variable function
#' 
#' @aliases gg_partial_coplot
#' @export gg_partial_coplot gg_partial_coplot.ggRandomForests
#' 
#' @importFrom randomForestSRC plot.variable
#' @importFrom parallel mclapply
#' 
#' @examples
#' \dontrun{
#' data(pbc_rf, package="ggRandomForests")
#' 
#' # Create a 3d coplot, survival as a function of bilirubin and prothrombin
#' prothrombin_grp <- cut(pbc_rf$xvar$prothrombin, breaks=c(8.9,10,11,12,18))
#' 
#' gg_dta <- gg_partial_coplot(pbc_rf, xvar="bili", groups=prothrombin_grp,
#'   surv_type="surv", time=1, show.plots=FALSE)
#' 
#' ggpl <- ggplot(gg_dta, 
#'                aes(x=bili, y=yhat, shape=groups, color=groups))+
#'        geom_point()+geom_smooth(se=FALSE)+
#'        labs(x="Surgical Date", y="Survival 1 year", 
#'             shape="Prothrombin", color="Prothrombin")+
#'        scale_color_brewer(palette="Set1")
#'        
#' ggpl
#' 
#' ## Build a list of 25 split points
#' prothrom <- pbc_rf$xvar %>% filter(!is.na(prothrombin))
#' n.x <- length(unique(prothrom$prothrombin))
#' npts <- 25
#' prothrombin_pts <- sort(unique(prothrom$prothrombin))[
#'    unique(as.integer(seq(1, n.x, length = min(npts, n.x))))]
#'
#' # Create a 3d coplot, survival as a function of bilirubin and prothrombin
#' prothrombin_grp <- cut(pbc_rf$xvar$prothrombin, breaks=prothrombin_pts)
#' 
#' gg_dta <- gg_partial_coplot(pbc_rf, xvar="bili", groups=prothrombin_grp,
#'   surv_type="surv", time=1, show.plots=FALSE)
#' 
#' ggpl <- ggplot(gg_dta, 
#'                aes(x=bili, y=yhat, shape=groups, color=groups))+
#'        geom_point()+geom_smooth(se=FALSE)+
#'        labs(x="Surgical Date", y="Survival 1 year", 
#'             shape="Prothrombin", color="Prothrombin")+
#'        scale_color_brewer(palette="Set1")
#'        
#' ggpl  
#' 
#' }
#'
gg_partial_coplot.ggRandomForests <- function(object, 
                                           xvar, 
                                           groups, 
                                           surv_type=c("mort", "rel.freq", "surv", "years.lost", "cif", "chf"), 
                                           time,
                                           ...){
  
  # Some sanity checks:
  
  ## Check that the input obect is of the correct type.
  if (inherits(object, "rfsrc") == FALSE){
    stop("This function only works for Forests grown with the randomForestSRC package.")
  }
  if (is.null(object$forest)) {
    stop("The function requires the \"forest = TRUE\" attribute when growing the randomForest")
  }
  
  #---
  # Unpack the elipse argument.
  arg_list <- list(...)
  
  surv_type <- match.arg(surv_type)
  
  family <- object$family
  
  # Get the training data to work with...
  dta.train  <- object$xvar
  
  # Make sure all xvars are in the training set.
  
  # If we don't have a groups variable, we may have a subsets in the ellipse list.
  if(missing(groups)){
    if(is.null(arg_list$subset))
      stop("partial_coplot requires a groups argument to stratify the partial plots.")
    else{
      # We may be able to coherce a groups argument from subset that is normally
      # passed to plot.variable.
      
    }
  }
  # Make sure we have more than one group in the subset list.
  
  # If groups is a variable name, check for existance.
  
  # if not, make sure it has the correct dimension (nrow(rfsrc$xvar))
  
  
  dta.train$groups <- groups
  
  # make groups a factor
  
  # how many curves per coplot?
  lvl <- levels(groups)
  lng <- length(lvl)
  
  # Create the subsets for the plot.variable function
  sbst <- mclapply(1:lng, function(ind){
    st <- which(dta.train$groups==levels(groups)[ind])
    if(length(st) == 0) NULL
    else st
  })
  
  # Collapse the subset list to interesting items (those with observations)
  # If you work backwards, you do extra tests, but it 
  # cuts the correct items. Cute.
  for(ind in lng:1){
    if(is.null(sbst[[ind]])){
      sbst[[ind]] <- NULL
      
      # reset the levels, so we can label things later
      lvl <- lvl[-ind]
    }
  }
  
  # If survival family, make sure we have a time and surv_type.
  # if not default to time=1, and surv_type="surv" with warning.
  
  # If we got a surv.type instead of surv_type, let's use that.
  if(!is.null(arg_list$surv.type)){
    
  }
  # If we got a surv.type instead of surv_type, let's use that.
  if(is.null(arg_list$show.plots)){
    arg_list$show.plots <- FALSE
  }
  # what about multiple time points?
  
  # This will return a list of subseted partial plots, one for each group, 
  # all variables in xvar.
  pDat.partlist <- lapply(1:length(sbst), function(ind){
    plot.variable(object, surv.type=surv_type, time=time,
                  subset = sbst[[ind]],
                  xvar.names=xvar, partial=TRUE, ...)
  })
  
  ## Make them all gg_partials.
  gg_part <- mclapply(pDat.partlist, gg_partial)
  
  ## With the subsets marked for plotting
  for(ind in 1:length(gg_part)){
    gg_part[[ind]]$groups <- lvl[ind]
  }
  
  ## Collapse the grouped data into a single structure
  gg_merge <- do.call(rbind, gg_part)
  
  # Make is so we can easily plot it with an S3 function.
  class(gg_merge) <- c("gg_partial_coplot", class(gg_merge))
  gg_merge
}

gg_partial_coplot <- gg_partial_coplot.ggRandomForests
