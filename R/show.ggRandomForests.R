#' show.ggRandomForests?
#'
#' @export show.ggRandomForests


show.ggRandomForests<- function(object){
  print(object$call)
  show(object$graph)
}
