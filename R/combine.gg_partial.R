#' combine two gg_partial objects
#' 
#' The combine.gg_partial function assumes the two gg_partial object
#' were generated from the same randomForestSRC::rfsrc object. Further,
#' we assume the combine is along the group variable.
#' 
#'  @param x gg_partial object
#'  @param y gg_partial object
#'  @param labels how to label the combined data.
#'  @param ... not used
#'  
#'  @export combine.gg_partial_list combine.gg_partial combine
#'  @aliases combine combine.gg_partial combine.gg_partial_list
#'
#' @examples 
#' \dontrun{
#' #!!TODO!! examples
#' # Calculate the 1 year partial dependence
#' pbc_prtl <- plot.variable(pbc_rf, surv.type = "surv", 
#'                           time = 364.25, 
#'                           xvar.names = xvar, partial = TRUE, 
#'                           show.plots = FALSE)
#' 
#' # Calculate the 3 year partial dependence
#' pbc_prtl.3 <- plot.variable(pbc_rf, surv.type = "surv", 
#'                             time = 3*364.25, 
#'                             xvar.names = xvar, partial = TRUE, 
#'                             show.plots = FALSE)
#' 
#' # Create gg_partial objects
#' ggPrtl <- gg_partial(pbc_prtl)
#' ggPrtl.3 <- gg_partial(pbc_prtl.3)
#' 
#' # Combine the objects to get multiple time curves 
#' # along variables on a single figure.
#' pbc_ggpart <- combine(ggPrtl, ggPrtl.3, 
#'                       labels = c("1 Year", "3 Years"))
#' 
#' }
combine <- function(x, y, labels,...){
  UseMethod("combine",x)
}
combine.gg_partial <- function(x, y, labels, ...){
  return(combine.gg_partial_list(x, y, labels, ...))
}
combine.gg_partial_list <- function(x, y, labels, ...){
  
  if(inherits(x,"plot.variable"))
    x <- gg_partial(x)
  if(inherits(y,"plot.variable"))
    y <- gg_partial(y)
  
  if((!inherits(x,"gg_partial_list") & !inherits(x,"gg_partial"))  &
       (!inherits(y,"gg_partial_list") & !inherits(y,"gg_partial"))  ){
    stop("combine.gg_partial expects either a ggRandomForests::gg_partial or randomForestSRC::plot.variable object")
  }
  
  if(missing(labels)){
    labels=c("x1", "x2")
  }
  ### !!TODO!! check for labels length
  
  cls <- class(x)
  
  ### We need to check for the case when x and y already have
  ### a group column, 
  
  if(is.null(x$group))
    x <- lapply(x, function(st){st$group <- labels[1]; st})
  
  if(is.null(y$group))
    y <- lapply(y, function(st){st$group <- labels[2]; st})
  
  # By names
  nm <- names(x)
  
  gg_dta <- lapply(nm, function(ind){
    rbind(x[[ind]], y[[ind]])
  })
  
  names(gg_dta) <- names(x)
  class(gg_dta) <- cls
  return(gg_dta)
}
