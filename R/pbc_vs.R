#' \code{randomForestSRC::var.select} Object for the pbc data.
#' 
#' The \code{randomForestSRC::var.select} data set for the \code{pbc} survival dataset.
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
#' pbc_vs <- var.select(pbc_rf)
#' 
#' pbc_md <- gg_minimal_depth(pbc_vs)
#' plot(pbc_md)
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A randomForestSRC::var.select object for survival
#' @name pbc_vs
NULL
