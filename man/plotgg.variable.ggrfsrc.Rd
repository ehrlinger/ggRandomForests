\name{plotgg.variable.ggrfsrc}
\alias{plotgg.variable.ggrfsrc}
\title{Plot the marginal dependence of variables.}
\usage{
  plotgg.variable.ggrfsrc(x, smooth.lines = FALSE, ...)
}
\arguments{
  \item{x}{a marginal or partial rfsrc data object from
  \code{\link{pred.variable}}}

  \item{smooth.lines}{boolean indicating the inclusion of
  confidence intervals}
}
\value{
  A list of \code{\link{ggplot2}} plot objects
  corresponding the variables contained within the \code{x}
  argument
}
\description{
  plotgg.variable generates a list of either marginal
  variable dependance or partial variable dependence
  figures using \code{\link{ggplot}}.
}
\seealso{
  \code{\link{pred.variable}}, \code{\link{plot.variable}}
}

