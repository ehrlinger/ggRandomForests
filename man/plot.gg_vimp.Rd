% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.gg_vimp.R
\name{plot.gg_vimp}
\alias{plot.gg_vimp}
\title{Plot a \code{\link{gg_vimp}} object, extracted variable importance of a
\code{\link[randomForestSRC]{rfsrc}} object}
\usage{
\method{plot}{gg_vimp}(x, relative, lbls, ...)
}
\arguments{
\item{x}{\code{\link{gg_vimp}} object created from a
\code{\link[randomForestSRC]{rfsrc}} object}

\item{relative}{should we plot vimp or relative vimp. Defaults to vimp.}

\item{lbls}{A vector of alternative variable labels. Item names should be
the same as the variable names.}

\item{...}{optional arguments passed to gg_vimp if necessary}
}
\value{
\code{ggplot} object
}
\description{
Plot a \code{\link{gg_vimp}} object, extracted variable importance of a
\code{\link[randomForestSRC]{rfsrc}} object
}
\examples{
## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## -------- iris data
rfsrc_iris <- rfsrc(Species ~ ., data = iris)
gg_dta <- gg_vimp(rfsrc_iris)
plot(gg_dta)

## ------------------------------------------------------------
## regression example
## ------------------------------------------------------------
## -------- air quality data
rfsrc_airq <- rfsrc(Ozone ~ ., airquality)
gg_dta <- gg_vimp(rfsrc_airq)
plot(gg_dta)


}
\references{
Breiman L. (2001). Random forests, Machine Learning, 45:5-32.

Ishwaran H. and Kogalur U.B. (2007). Random survival forests for
R, Rnews, 7(2):25-31.

Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival,
Regression and Classification (RF-SRC), R package version 1.4.
}
\seealso{
\code{\link{gg_vimp}}
}
