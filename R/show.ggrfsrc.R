#' show.ggrfsrc?
#'
#' @export show.ggrfsrc


show.ggrfsrc<- function(object){
  print(object$call)
  show(object$graph)
}