#' combine two gg_partial objects
#' 
#' The combine.gg_partial function assumes the two gg_partial object
#' were generated from the same randomForestSRC::rfsrc object. Further,
#' we assume the combine is along the group variable.
#' 
#'  @param x gg_partial object
#'  @param y gg_partial object
#'  @param lbls how to label the combined data.
#'  @param ... not used
#'  
#'  @export combine.gg_partial_list combine.gg_partial
#'  @aliases combine.gg_partial combine.gg_partial_list
#' 
#' @importFrom parallel mclapply
#' 
#' @examples 
#' # Load a set of plot.variable partial plot data
#' data(partial_veteran)
#' 
#' # A list of 2 plot.variable objects
#' length(partial_veteran) 
#' class(partial_veteran)
#' 
#' class(partial_veteran[[1]])
#' class(partial_veteran[[2]])
#' 
#' # Create gg_partial objects
#' ggPrtl.1 <- gg_partial(partial_veteran[[1]])
#' ggPrtl.2 <- gg_partial(partial_veteran[[2]])
#' 
#' # Combine the objects to get multiple time curves 
#' # along variables on a single figure.
#' ggpart <- combine.gg_partial(ggPrtl.1, ggPrtl.2, 
#'                              lbls = c("30 day", "6 month"))
#'                              
#' # Plot each figure separately
#' plot(ggpart)                                  
#' 
#' # Get the continuous data
#' ggcont <- ggpart
#' ggcont$celltype <- ggcont$trt <- ggcont$prior <- NULL
#' plot(ggcont, panel=TRUE) 
#' 
#' # And the categorical
#' ggpart$karno <- ggpart$diagtime <- ggpart$age <- NULL
#' plot(ggpart, panel=TRUE) 
#' 
combine.gg_partial <- function(x, y, lbls, ...){
  return(combine.gg_partial_list(x, y, lbls, ...))
}

combine.gg_partial_list <- function(x, y, lbls, ...){
  
  if(inherits(x,"plot.variable"))
    x <- gg_partial(x)
  if(inherits(y,"plot.variable"))
    y <- gg_partial(y)
  
  if((!inherits(x,"gg_partial_list") & !inherits(x,"gg_partial"))  &
       (!inherits(y,"gg_partial_list") & !inherits(y,"gg_partial"))  ){
    stop("combine.gg_partial expects either a ggRandomForests::gg_partial or randomForestSRC::plot.variable object")
  }
  
  if(missing(lbls)){
    lbls=c("x1", "x2")
  }
  ### !!TODO!! check for lbls length
  
  cls <- class(x)
  
  ### We need to check for the case when x and y already have
  ### a group column, 
  
  if(is.null(x[[1]]$group))
    x <- mclapply(x, function(st){st$group <- lbls[1]; st})
  
  if(is.null(y[[1]]$group)){
    ind.l <- length(lbls)
    y <- mclapply(y, function(st){st$group <- lbls[ind.l]; st})
  }
  # By names
  nm <- names(x)
  
  gg_dta <- mclapply(nm, function(ind){
    rbind(x[[ind]], y[[ind]])
  })
  
  names(gg_dta) <- names(x)
  class(gg_dta) <- cls
  return(gg_dta)
}
