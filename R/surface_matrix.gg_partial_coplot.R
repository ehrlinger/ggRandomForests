####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' Construct a set of (x, y, z) matrices for surface plotting a
#' \code{gg_partial_coplot} object
#'
#' @param dta a gg_partial_coplot object containing at least 3 numeric columns
#' of data
#' @param xvar a vector of 3 column names from the data object, in (x, y, z)
#' order
#'
#' @details To create a surface plot, the \code{plot3D::surf3D} function expects
#' 3 matrices of n.x by n.y. Take the p+1 by n \code{gg_partial_coplot} object,
#' and extract and construct the x, y and z matrices from the provided
#' \code{xvar} column names.
#'
#' @examples
#' \dontrun{
#' ## From vignette(randomForestRegression, package="ggRandomForests")
#' data(Boston, package="MASS")
#' rfsrc_boston <- randomForestSRC::rfsrc(medv~., Boston)
#' 
#' varsel_boston <- var.select(rfsrc_boston)
#' 
#' rm_pts <- quantile_pts(rfsrc_boston$xvar$rm, groups=9, intervals=TRUE)
#'
#' 
#' partial_boston_surf <- lapply(rm_pts, function(ct) {
#'   rfsrc_boston$xvar$rm <- ct
#'   randomForestSRC::plot.variable(
#'     rfsrc_boston,
#'     xvar.names = "lstat",
#'     time = 1,
#'     npts = 10,
#'     show.plots = FALSE,
#'     partial = TRUE
#'   )
#'
#' # Instead of groups, we want the raw rm point values,
#' # To make the dimensions match, we need to repeat the values
#' # for each of the 50 points in the lstat direction
#' rm.tmp <- do.call(c,lapply(rm_pts,
#'                            function(grp) {rep(grp, l
#'                            ength(partial_boston_surf))}))
#'
#' # Convert the list of plot.variable output to
#' partial_surf <- do.call(rbind,lapply(partial_boston_surf, gg_partial))
#'
#' # attach the data to the gg_partial_coplot
#' partial_surf$rm <- rm.tmp
#'
#' # Transform the gg_partial_coplot object into a list of three named matrices
#' # for surface plotting with plot3D::surf3D
#' srf <- surface_matrix(partial_surf, c("lstat", "rm", "yhat"))
#'}
#'
#' \dontrun{
#' # surf3D is in the plot3D package.
#' library(plot3D)
#' # Generate the figure.
#' surf3D(x=srf$x, y=srf$y, z=srf$z, col=topo.colors(10),
#'        colkey=FALSE, border = "black", bty="b2",
#'        shade = 0.5, expand = 0.5,
#'        lighting = TRUE, lphi = -50,
#'        xlab="Lower Status", ylab="Average Rooms", zlab="Median Value"
#' )
#' }
#' @aliases surface_matrix  surface_matrix.gg_partial_coplot
#' @export
surface_matrix <- function(dta, xvar) {
  # Test for class type
  if (!inherits(dta, "gg_partial_coplot")) {
    warning("data object is not a gg_partial_coplot object.")
    print(class(dta))
  }
  
  # Get the columns of interest.
  if (missing(xvar)) {
    xvar <- colnames(dta)[which(colnames(dta) != "group")]
  }
  
  # Verify there are three
  if (length(xvar) != 3) {
    stop("We expect the xvar argument to contain three column names
         for (x, y, z) specification.")
  }
  
  # Verify the columns are in the dta object
  if (sum(xvar %in% colnames(dta)) != length(xvar)) {
    stop("xvar argument does not reference columns in this data set.")
  }
  if (is.null(dta$group))
    dta$group <- factor(dta[, xvar[1]])
  
  x_tmp <- lapply(unique(dta$group),
                  function(grp) {
                    dta[which(dta$group == grp), xvar[1]]
                  })
  
  y_tmp <- lapply(unique(dta$group),
                  function(grp) {
                    dta[which(dta$group == grp), xvar[2]]
                  })
  
  z_tmp <- lapply(unique(dta$group),
                  function(grp) {
                    dta[which(dta$group == grp), xvar[3]]
                  })
  
  x_tmp <- do.call(rbind, x_tmp)
  y_tmp <- do.call(rbind, y_tmp)
  z_tmp <- do.call(rbind, z_tmp)
  
  invisible(list(x = x_tmp, y = y_tmp, z = z_tmp))
}

surface_matrix.gg_partial_coplot <- surface_matrix
