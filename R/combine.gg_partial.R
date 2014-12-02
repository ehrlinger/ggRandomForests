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
  
  # If the plot.variable object returned the time argument, 
  # we could get it from there. Instead, we'll make something up.
  if(missing(labels)) labels=c("x1", "x2")
  cls <- class(x)
  
  x <- lapply(x, function(st){st$group <- labels[1]; st})
  y <- lapply(y, function(st){st$group <- labels[2]; st})
  
  # By names
  nm <- names(x)
  
  object <- lapply(nm, function(ind){
    rbind(x[[ind]], y[[ind]])
  })
  
  names(object) <- names(x)
  class(object) <- cls
  return(object)
}
