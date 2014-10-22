#' \code{randomForestSRC::plot.variable} Object for the pbc data.
#' 
#' The \code{randomForestSRC::plot.variable} data set for the \code{pbc} survival dataset.
#' 
#' @references 
#' Ishwaran H. and Kogalur U.B. (2014). Random Forests for
#' Survival, Regression and Classification (RF-SRC), R package
#' version 1.5.4.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests
#' for R. R News 7(2), 25-31.
#' 
#' Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S.
#' (2008). Random survival forests. Ann. Appl. Statist. 2(3),
#' 841-860.
#' 
#' @examples
#' \dontrun{
#' ## Examples from randomForestSRC::rfsrc 
#' ##
#' data(pbc, package = "randomForestSRC")
#' pbc_rf <- rfsrc(Surv(days, status) ~ ., pbc, nsplit = 10)
#' 
#' # generate partial plot data for 4 different covariates.
#' pbc_prtl <- plot.variable(pbc_rf, 
#'                           xvar.names = c("bili", "copper", "albumin", "age"),
#'                           partial=TRUE, show.plot=FALSE)
#'                           
#' # Format for ggRandomForests                           
#' ggprtl <- gg_partial(pbc_prtl)
#' 
#' # Without specifying which plot, plot.gg_partial returns a list of all four
#' # figures. 
#' plot(ggprtl)
#' 
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A randomForestSRC::plot.variable object for survival
#' @name pbc_prtl
NULL
