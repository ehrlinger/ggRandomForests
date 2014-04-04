#' coplot.ggRandomForests This function produces two variants of the conditioning plots discussed
#' in the reference below.
#' 
#' @param formula, 
#' @param data
#' @param given.values
#' @param panel = points
#' @param rows
#' @param columns,
#' @param show.given = TRUE, 
#' @param xlab = c(x.name, paste("Given :", a.name)),
#' @param ylab = c(y.name, paste("Given :", b.name)),
#' @param xlim
#' @param ylim
#' @param  ...
#' 
#' @export ggCoplot.ggRandomForests
#' 
ggCoplot.ggRandomForests <- function(formula, 
                                  data, 
                                  given.values, panel = points, rows, columns,
                                  show.given = TRUE, 
                                  xlab = c(x.name, paste("Given :", a.name)),
                                  ylab = c(y.name, paste("Given :", b.name)),
                                  xlim, ylim, ...){
  
  
}

