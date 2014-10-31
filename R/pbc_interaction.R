#' \code{randomForestSRC::find.interaction} matrix for the pbs data.
#' 
#' The \code{randomForestSRC::find.interaction} data set for the \code{pbc} survival dataset.
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
#' pbc_rf <- rfsrc(Surv(days, status) ~ ., pbc, nsplit = 10, ntree=500)
#' print(pbc.obj)
#' 
#' pbc_interaction <- find.interaction(pbc_rf)
#' 
#' plot(pbc_interaction, x_var="bili")
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format rdata matrix from \code{randomForestSRC::find.interaction}
#' @name pbc_interaction
NULL
