####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' Plot a \code{\link{gg_vimp}} object, extracted variable importance of a
#' \code{\link[randomForestSRC]{rfsrc}} object
#'
#' @param x \code{\link{gg_vimp}} object created from a
#' \code{\link[randomForestSRC]{rfsrc}} object
#' @param relative should we plot vimp or relative vimp. Defaults to vimp.
#' @param lbls A vector of alternative variable labels. Item names should be
#' the same as the variable names.
#' @param ... optional arguments passed to gg_vimp if necessary
#'
#' @return \code{ggplot} object
#'
#' @seealso \code{\link{gg_vimp}}
#'
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#'
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for
#' R, Rnews, 7(2):25-31.
#'
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival,
#' Regression and Classification (RF-SRC), R package version 1.4.
#'
#' @examples
#' \dontrun{
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' data(rfsrc_iris, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_iris)
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' ## -------- air quality data
#' # rfsrc_airq <- rfsrc(Ozone ~ ., airquality)
#' data(rfsrc_airq, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_airq)
#' plot(gg_dta)
#'
#' ## -------- Boston data
#' data(rfsrc_boston, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_boston)
#' plot(gg_dta)
#'
#' ## -------- mtcars data
#' data(rfsrc_mtcars, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_mtcars)
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## survival example
#' ## ------------------------------------------------------------
#' ## -------- veteran data
#' data(rfsrc_veteran, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_veteran)
#' plot(gg_dta)
#'
#' ## -------- pbc data
#' data(rfsrc_pbc, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_pbc)
#' plot(gg_dta)
#'
#'}
#'
#' @importFrom ggplot2 ggplot geom_bar aes_string labs coord_flip facet_grid
#' @importFrom ggplot2 scale_x_discrete
#' @export
plot.gg_vimp <- function(x, relative, lbls, ...) {
  gg_dta  <- x
  if (!inherits(gg_dta, "gg_vimp"))
    gg_dta <- gg_vimp(gg_dta, ...)
  
  # Classification...
  arg_set <- as.list(substitute(list(...)))[-1L]
  
  nvar <- nrow(gg_dta)
  if (!is.null(arg_set$nvar)) {
    if (is.numeric(arg_set$nvar) && arg_set$nvar > 1) {
      if (arg_set$nvar < nrow(gg_dta)) {
        nvar <- arg_set$nvar
        gg_dta <- gg_dta[1:nvar, ]
      }
    }
  }
  
  gg_plt <- ggplot(gg_dta)
  
  msr <- "vimp"
  if (!msr %in% colnames(gg_dta))
    msr <- colnames(gg_dta)[1]
  
  #  if(missing(relative) | is.null(gg_dta$rel_vimp)) {
  if (length(unique(gg_dta$positive)) > 1) {
    gg_plt <- gg_plt +
      geom_bar(
        aes_string(y = msr, 
                   x = "vars", 
                   fill = "positive",
                   color = "positive"),
        stat = "identity",
        width = .5,
      )
  } else {
    gg_plt <- gg_plt +
      geom_bar(aes_string(y = msr, 
                          x = "vars",
                          color = "positive"),
               stat = "identity",
               width = .5,)
  }
  gg_plt <- gg_plt + labs(x = "", y = msr)
  
  if (!missing(lbls)) {
    # Print a warning if the lbls is not a named vector.
    
    if (length(lbls) >= length(gg_dta$vars)) {
      st_lbls <- lbls[as.character(gg_dta$vars)]
      names(st_lbls) <- as.character(gg_dta$vars)
      st_lbls[which(is.na(st_lbls))] <-
        names(st_lbls[which(is.na(st_lbls))])
      
      gg_plt <- gg_plt +
        scale_x_discrete(labels = st_lbls)
    }
  }
  if (is.null(gg_dta$set) || length(unique(gg_dta$set)) < 2) {
    gg_plt <- gg_plt +
      coord_flip()
  } else {
    gg_plt <- gg_plt +
      coord_flip() + facet_grid( ~ set)
  }
  
  return(gg_plt)
}
