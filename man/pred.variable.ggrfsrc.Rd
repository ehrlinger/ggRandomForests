\name{pred.variable.ggrfsrc}
\alias{pred.variable.ggrfsrc}
\title{Predict Marginal Effect of Variables}
\usage{
  pred.variable(x, xvar.names, surv.type = c("mort",
    "rel.freq", "surv", "years.lost", "cif", "chf"),
    percentile, time=NULL, which.outcome, partial = FALSE,
    plots.per.page = 4, granule = 5, sorted = TRUE, nvar,
    npts = 25, smooth.lines = FALSE, subset, ...)
}
\arguments{
  \item{x}{An object of class (rfsrc, grow) or (rfsrc,
  predict).}

  \item{xvar.names}{Names of the x-variables to be used.}

  \item{surv.type}{For survival families, specifies the
  predicted value.}

  \item{percentile}{For survival families, the percentile
  of the follow up time used for plotting predicted
  survival. Default is the median.}

  \item{time}{For survival families, the follow up time
  used for plotting predicted survival. Default is to use
  the median percentile.}

  \item{which.outcome}{For classification families, an
  integer or character value specifying the class to focus
  on (defaults to the first class). For competing risk
  families, an integer value between 1 and J indicating the
  event of interest, where J is the number of event types.
  The default is to use the first event type.}

  \item{partial}{Should partial plots be used?}

  \item{granule}{Integer value controlling whether a plot
  for a specific variable should be given as a boxplot or
  scatter plot. Larger values coerce boxplots.}

  \item{sorted}{Should variables be sorted by importance
  values.}

  \item{nvar}{Number of variables to be predicted. Default
  is all.}

  \item{npts}{Maximum number of points used when generating
  partial predictions for continuous variables.}

  \item{subset}{Vector indicating which rows of the
  x-variable matrix x$xvar to use. All rows are used if not
  specified.}

  \item{...}{Further arguments passed to or from other
  methods.}
}
\description{
  Predict the marginal effect of an x-variable on the class
  probability (classification), response (regression),
  mortality (survival), or the expected years lost
  (competing risk) from a RF-SRC analysis. Users can select
  between marginal (unadjusted, but fast) and partial plots
  (adjusted, but slow).
}
\details{
  The logic for this function was cut out of the
  \code{\link{plot.variable.rfsrc}} (V 1.0.2) directly.
  Then was modified to create the data objects used by
  \code{\link{plot.variable.ggrfsrc}} and
  \code{\link{ggplot.variable}}.

  The function creates data structures suitable for
  plotting using the \code{\link{plot.variable}} function.
  \itemize{ \item For regression, the predicted response is
  used. \item For classification, it is the predicted class
  probabilities optionally specified by which.outcome.
  \item For survival, the choices are: \itemize{ \item
  Mortality (mort). \item Relative frequency of mortality
  (rel.freq). \item Predicted survival (surv), where the
  predicted survival is for the time point specified using
  the percentile of the follow up time (the default is 50,
  the median follow up time). Alternatively, a specific
  follow up time can also be specified. } \item For
  competing risks, the choices are: \itemize{ \item The
  expected number of life years lost (years.lost).  \item
  The cumulative incidence function (cif). \item The
  cumulative hazard function (chf). } In all three cases,
  the predicted value is for the event type specified by
  which.outcome. For cif and chf the quantity is evaluated
  at the time point specified by percentile or by time. }
  For partial plots use partial=TRUE. Their interpretation
  are different than marginal plots. The y-value for a
  variable X, evaluated at X=x, is

  \deqn{\tilde{f}(x) = \frac{1}{n} \sum_{i=1}^n \hat{f}(x,
  x_{i,o})},

  where x_{i,o} represents the value for all other
  variables other than X for individual i and
  \deqn{\hat{f}} is the predicted value. Generating partial
  plots can be very slow. Choosing a small value for npts
  can speed up computational times as this restricts the
  number of distinct x values used in computing
  \deqn{\tilde{f}}.

  For continuous variables, red points are used to indicate
  partial values and dashed red lines indicate a smoothed
  error bar of +/- two standard errors. Black dashed line
  are the partial values. Set smooth.lines=TRUE for lowess
  smoothed lines. For discrete variables, partial values
  are indicated using boxplots with whiskers extending out
  approximately two standard errors from the mean. Standard
  errors are meant only to be a guide and should be
  interpreted with caution.

  Partial plots can be slow. Setting npts to a smaller
  number can help.
}
\examples{
## Not run:
### survival/CR examples

# survival
data(veteran, package = "randomForestSRC")
v.obj <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
plot.variable(v.obj, plots.per.page = 3)
plot.variable(v.obj, plots.per.page = 2, xvar.names = c("trt", "karno", "age"))
plot.variable(v.obj, surv.type = "surv", nvar = 1, percentile = 75)
plot.variable(v.obj, surv.type = "surv", nvar = 1, time=5)
plot.variable(v.obj, surv.type = "surv", partial = TRUE, smooth.lines = TRUE)
plot.variable(v.obj, surv.type = "rel.freq", partial = TRUE, nvar = 2)

# competing risks
data(follic, package = "randomForestSRC")
follic.obj <- rfsrc(Surv(time, status) ~ ., follic, nsplit = 3, ntree = 100)
plot.variable(follic.obj, which.outcome = 2)

### regression examples

# airquality
airq.obj <- rfsrc(Ozone ~ ., data = airquality)
plot.variable(airq.obj, partial = TRUE, smooth.lines = TRUE)

# motor trend cars
mtcars.obj <- rfsrc(mpg ~ ., data = mtcars)
plot.variable(mtcars.obj, partial = TRUE, smooth.lines = TRUE)

### classification example

# iris
iris.obj <- rfsrc(Species ~., data = iris)
plot.variable(iris.obj, partial = TRUE)

# motor trend cars: predict number of carburetors
mtcars2 <- mtcars
mtcars2$carb <- factor(mtcars2$carb,
                       labels = paste("carb", sort(unique(mtcars$carb))))
mtcars2.obj <- rfsrc(carb ~ ., data = mtcars2)
plot.variable(mtcars2.obj, partial = TRUE)

## End(Not run)
}
\author{
  Hemant Ishwaran \code{hemant.ishwaran
}
\references{
  Friedman J.H. (2001). Greedy function approximation: a
  gradient boosting machine, Ann. of Statist., 5:1189-1232.
  Ishwaran H., Kogalur U.B. (2007). Random survival forests
  for R, Rnews, 7(2):25-31. Ishwaran H., Kogalur U.B.,
  Blackstone E.H. and Lauer M.S. (2008). Random survival
  forests, Ann. App. Statist., 2:841-860. Ishwaran H.,
  Gerds, T.A. Kogalur U.B., Moore R.D., Gange S.J. and Lau
  B.M. (2012). Random survival forests for competing risks.
}
\seealso{
  \code{\link{rfsrc}}, \code{\link{predict.rfsrc}},
  \code{\link{plot.variable.rfsrc}},
  \code{\link{plot.variable}}
}

