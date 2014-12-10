#' A \code{\link{gg_partial}} object for the pbc data, combined from 1 and 3 year \code{plot.variable} for \code{pbc} data.
#' 
#' This object is built for use in the vignette "Survival with Random Forests".
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
#' data(pbc_rf)
#' data(pbc_vs)
#' 
#' # For the top 6 covariates of the rfsrc object by minimal depth
#' xvar <- pbc_vs$topvars[1:6]
#'
#' # Calculate the 1 year partial dependence
#' pbc_prtl <- plot.variable(pbc_rf, surv.type="surv",
#'                          time=1, 
#'                          xvar.names=xvar, partial=TRUE,
#'                          show.plots = FALSE)
#'
#' 
#' # Calculate the 3 year partial dependence
#' pbc_prtl.3 <- plot.variable(pbc_rf, surv.type="surv", 
#'                             time=3, 
#'                             xvar.names=xvar, partial=TRUE,
#'                             show.plots = FALSE)
#' 
#' # Create gg_partial objects
#' gg_dta <- gg_partial(pbc_prtl)
#' gg_dta.3 <- gg_partial(pbc_prtl.3)
#' 
#' # Combine the objects to get multiple time curves 
#' # along variables on a single figure.
#' pbc_ggpart <- combine(gg_dta, gg_dta.3, 
#'                       labels=c("1 Year", "3 Years"))
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A gg_partial object for survival
#' @name pbc_ggpart
NULL
