% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.gg_partial_list.R
\name{plot.gg_partial_list}
\alias{plot.gg_partial_list}
\title{Partial variable dependence plot, operates on a \code{gg_partial_list}
object.}
\usage{
\method{plot}{gg_partial_list}(x, points = TRUE, panel = FALSE, ...)
}
\arguments{
\item{x}{\code{gg_partial_list} object created from a
\code{\link{gg_partial}} forest object}

\item{points}{plot points (boolean) or a smooth line.}

\item{panel}{should the entire list be plotted together?}

\item{...}{extra arguments}
}
\value{
list of \code{ggplot} objects, or a single faceted \code{ggplot}
object
}
\description{
Generate a risk adjusted (partial) variable dependence plot.
The function plots the \code{\link[randomForestSRC]{rfsrc}} response
variable (y-axis) against the covariate of interest (specified when creating
the \code{gg_partial_list} object).
}
\examples{
\dontrun{
## ------------------------------------------------------------
## classification
## ------------------------------------------------------------
## -------- iris data

## iris "Petal.Width" partial dependence plot
##
# rfsrc_iris <- rfsrc(Species ~., data = iris)
# partial_iris <- plot.variable(rfsrc_iris, xvar.names = "Petal.Width",
#                            partial=TRUE)
data(partial_iris, package="ggRandomForests")

gg_dta <- gg_partial(partial_iris)
plot(gg_dta)

## ------------------------------------------------------------
## regression
## ------------------------------------------------------------
## -------- air quality data
## airquality "Wind" partial dependence plot
##
# rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality)
# partial_airq <- plot.variable(rfsrc_airq, xvar.names = "Wind",
#                            partial=TRUE, show.plot=FALSE)
data(partial_airq, package="ggRandomForests")

gg_dta <- gg_partial(partial_airq)
plot(gg_dta)

gg_dta.m <- gg_dta[["Month"]]
plot(gg_dta.m, notch=TRUE)

gg_dta[["Month"]] <- NULL
plot(gg_dta, panel=TRUE)

## -------- Boston data
data(partial_boston, package="ggRandomForests")

gg_dta <- gg_partial(partial_boston)
plot(gg_dta)
plot(gg_dta, panel=TRUE)

## -------- mtcars data
data(partial_mtcars, package="ggRandomForests")

gg_dta <- gg_partial(partial_mtcars)

plot(gg_dta)

gg_dta.cat <- gg_dta
gg_dta.cat[["disp"]] <- gg_dta.cat[["wt"]] <- gg_dta.cat[["hp"]] <- NULL
gg_dta.cat[["drat"]] <- gg_dta.cat[["carb"]] <- gg_dta.cat[["qsec"]] <- NULL

plot(gg_dta.cat, panel=TRUE)

gg_dta[["cyl"]] <- gg_dta[["vs"]] <- gg_dta[["am"]] <- NULL
gg_dta[["gear"]] <- NULL
plot(gg_dta, panel=TRUE)

## ------------------------------------------------------------
## survival examples
## ------------------------------------------------------------
## -------- veteran data
## survival "age" partial variable dependence plot
##
# data(veteran, package = "randomForestSRC")
# rfsrc_veteran <- rfsrc(Surv(time,status)~., veteran, nsplit = 10,
#                        ntree = 100)
#
## 30 day partial plot for age
# partial_veteran <- plot.variable(rfsrc_veteran, surv.type = "surv",
#                               partial = TRUE, time=30,
#                               xvar.names = "age",
#                               show.plots=FALSE)
data(partial_veteran, package="ggRandomForests")

gg_dta <- gg_partial(partial_veteran[[1]])
plot(gg_dta)

gg_dta.cat <- gg_dta
gg_dta[["celltype"]] <- gg_dta[["trt"]] <- gg_dta[["prior"]] <- NULL
plot(gg_dta, panel=TRUE)

gg_dta.cat[["karno"]] <- gg_dta.cat[["diagtime"]] <-
    gg_dta.cat[["age"]] <- NULL
plot(gg_dta.cat, panel=TRUE, notch=TRUE)

gg_dta <- lapply(partial_veteran, gg_partial)
length(gg_dta)
gg_dta <- combine.gg_partial(gg_dta[[1]], gg_dta[[2]] )

plot(gg_dta[["karno"]])
plot(gg_dta[["celltype"]])

gg_dta.cat <- gg_dta
gg_dta[["celltype"]] <- gg_dta[["trt"]] <- gg_dta[["prior"]] <- NULL
plot(gg_dta, panel=TRUE)

gg_dta.cat[["karno"]] <- gg_dta.cat[["diagtime"]] <-
     gg_dta.cat[["age"]] <- NULL
plot(gg_dta.cat, panel=TRUE, notch=TRUE)

## -------- pbc data
}


}
\references{
Breiman L. (2001). Random forests, Machine Learning, 45:5-32.

Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R,
Rnews, 7(2):25-31.

Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival,
Regression and Classification (RF-SRC), R package version 1.4.
}
\seealso{
\code{\link[randomForestSRC]{plot.variable}}
\code{\link{gg_partial}}
\code{\link{plot.gg_partial}} \code{\link{gg_variable}}
\code{\link{plot.gg_variable}}
}
