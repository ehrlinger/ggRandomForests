% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_partial_rfsrc.R
\name{gg_partial_rfsrc}
\alias{gg_partial_rfsrc}
\title{Split partial lots into continuous or categorical datasets}
\usage{
gg_partial_rfsrc(rf_model, xvar.names = NULL, newx = NULL, cat_limit = 10)
}
\arguments{
\item{rf_model}{\code{rfsrc::rfsrc} model}

\item{xvar.names}{Which variables to calculate partial plots}

\item{newx}{a \code{data.frame} containing data to use for the partial plots}

\item{cat_limit}{Categorical features are build when there are fewer than
cat_limit unique features.}
}
\description{
gg_partial_rfsrc uses the \code{rfsrc::partial.rfsrc} to generate the partial
plot data internally. So you provide the \code{rfsrc::rfsrc} model, and the
xvar.names to generate the data.
}
\examples{
## ------------------------------------------------------------
##
## regression
##
## ------------------------------------------------------------

airq.obj <- rfsrc(Ozone ~ ., data = airquality)

## partial effect for wind
prt_dta <- gg_partial_rfsrc(airq.obj,
                       xvar.names = c("Wind"))

}
