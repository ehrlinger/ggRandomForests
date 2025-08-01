
##=============================================================================
#' varpro one hot encodes features, so we need to get the "raw"
#' original variable names. This loops through the variable names
#' not in the original dataset, and cuts one character off the end
#' until we find the variable name in the original data.
#' 
#' @param varpro_names vector of names output from varpro analysis
#' @param dataset the dataset used for varpro input.
#' 
#' @importFrom stringr str_sub
#' @export
varpro_feature_names <- function(varpro_names, dataset) {
  inc_set <- varpro_names[which(varpro_names %in% colnames(dataset))]
  one_set <- varpro_names[which(!varpro_names %in% colnames(dataset))]
  while (length(one_set) > 0) {
    orig <- unlist(lapply(one_set, stringr::str_sub, 1,-2))
    inc_set <-
      union(inc_set, orig[which(orig %in% colnames(dataset))])
    one_set <- orig[which(!orig %in% colnames(dataset))]
  }
  return(inc_set)
}
