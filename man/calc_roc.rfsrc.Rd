% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_roc.R
\name{calc_roc.rfsrc}
\alias{calc_roc.rfsrc}
\alias{calc_roc.randomForest}
\alias{calc_roc}
\title{Receiver Operator Characteristic calculator}
\usage{
\method{calc_roc}{rfsrc}(object, dta, which_outcome = "all", oob = TRUE, ...)
}
\arguments{
\item{object}{\code{\link[randomForestSRC]{rfsrc}} or
\code{\link[randomForestSRC]{predict.rfsrc}} object
containing predicted response}

\item{dta}{True response variable}

\item{which_outcome}{If defined, only show ROC for this response.}

\item{oob}{Use OOB estimates, the normal validation method (TRUE)}

\item{...}{extra arguments passed to helper functions}
}
\value{
A \code{gg_roc} object
}
\description{
Receiver Operator Characteristic calculator
}
\details{
For a randomForestSRC prediction and the actual
response value, calculate the specificity (1-False Positive Rate) and
sensitivity (True Positive Rate) of a predictor.

This is a helper function for the \code{\link{gg_roc}} functions, and
not intended for use by the end user.
}
\examples{
## Taken from the gg_roc example
rfsrc_iris <- rfsrc(Species ~ ., data = iris)

gg_dta <- calc_roc(rfsrc_iris, rfsrc_iris$yvar,
  which_outcome = 1, oob = TRUE
)
gg_dta <- calc_roc(rfsrc_iris, rfsrc_iris$yvar,
  which_outcome = 1, oob = FALSE
)

rf_iris <- randomForest(Species ~ ., data = iris)
gg_dta <- calc_roc(rf_iris, rf_iris$yvar,
  which_outcome = 1
)
gg_dta <- calc_roc(rf_iris, rf_iris$yvar,
  which_outcome = 2
)

}
\seealso{
\code{\link{calc_auc}} \code{\link{gg_roc}}

\code{\link{plot.gg_roc}}
}
