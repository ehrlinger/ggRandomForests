## ----setup,include=FALSE, cache=FALSE, echo=FALSE---------------------------------------
library(knitr)
# set global chunk options for knitr. These can be changed in the header for each individual R code chunk
opts_chunk$set(fig.path='figure/vig-', 
               fig.align='center',
               fig.pos="!htpb",
               fig.show='hold',
               fig.height=3,
               fig.width=4,
               size='footnotesize',
               prompt=TRUE,
               highlight=FALSE,
               comment=NA, echo=TRUE, results=TRUE, message=FALSE, warnings=FALSE,
               error=FALSE, dev='pdf')

# Setup the R environment
options(replace.assign=TRUE,object.size=Inf,expressions=100000,memory=Inf,
        replace.assign=TRUE, width=90, prompt="R> ")

#################
# Load_packages #
#################
library(ggplot2) # Graphics engine for generating all types of plots

# library(dplyr) # Better data manipulations
# library(tidyr)

library(ggRandomForests)

# Analysis packages.
library(randomForestSRC) 
library(RColorBrewer)

options(mc.cores = 1, rf.cores=1)

#########################################################################
# Default computation settings
#########################################################################
theme_set(theme_bw())
event.marks <- c(1,4)
event.labels <- c(FALSE, TRUE)
strCol <- brewer.pal(3, "Set1")
strCol <- strCol[c(2,1,3)]
alpha <- .3

## ----echo = FALSE---------------------------------------------------------------------------------

## save initial options
o1 <- getOption("prompt")
o2 <- getOption("continue")
o3 <- getOption("width")
o4 <- getOption("digits")
options(prompt = "R> ", continue = "+  ", width = 100, digits = 4)

## ----iris-rf-class, eval=FALSE--------------------------------------------------------------------
#  # Grow the classification forest
#  iris_rf <- rfsrc(Species ~., data = iris)
#  
#  # Plot the error convergence rate for the forest growth
#  plot.gg_error(iris_rf)
#  
#  # Plot predicted class probabilities for the training data
#  plot.gg_rfsrc(iris_rf)
#  
#  # Plot predicted class ROC curves
#  plot.gg_roc(iris_rf)

## ----iris-rf-error, echo=FALSE, fig.cap="Classification Forest: Forest error convergence rate.", fig.width=5----
data(iris_rf, package="ggRandomForests")
print(iris_rf)
plot.gg_error(iris_rf)+coord_cartesian(ylim=c(-.01,.2))

## ----iris-rf-pred, echo=FALSE, fig.cap="Classification Forest: Predicted probability of IRIS class.", fig.width=5----
plot.gg_rfsrc(iris_rf)

## ----iris-rf-roc, echo=FALSE, fig.cap="Classification Forest: ROC curves.", fig.width=5-----------
plot.gg_roc(iris_rf)

## ----airq-rf-class, eval=FALSE--------------------------------------------------------------------
#  # Grow the regression forest
#  airq_rf <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#  
#  # Plot the error convergence rate for the forest growth
#  gg_err <- gg_error(airq_rf)
#  plot(gg_err)
#  
#  # Plot predicted values for the training data
#  gg_rfr <- gg_rfsrc(airq_rf)
#  plot(gg_rfr)

## ----airq-rf-error, echo=FALSE, results=FALSE, fig.cap="Regression forest:  Forest error convergence rate."----
data(airq_rf, package="ggRandomForests")

print(airq_rf)

gg_err <- gg_error(airq_rf)
plot(gg_err)

## ----airq-rf-plot, echo=FALSE, results=FALSE, fig.cap="Regression forest: Predicted Ozone level."----
gg_rfr <- gg_rfsrc(airq_rf)
plot(gg_rfr)

## ----surv-rf-class, eval=FALSE--------------------------------------------------------------------
#  # Load the dataset,
#  data(pbc, package = "randomForestSRC")
#  
#  # Grow the survival forest
#  pbc_rf <- rfsrc(Surv(days, status) ~ ., pbc, nsplit = 10, ntree=500)
#  
#  # Plot the error convergence rate for the forest growth
#  plot.gg_error(pbc_rf)
#  
#  # Plot predicted survival probabilities for the training data
#  plot.gg_rfsrc(pbc_rf) +
#    theme(legend.position=c(.8,.8))

## ----surv-rf-error, echo=FALSE, fig.cap="Survival Forest: Predicted probability of survival.", fig.width=5----
data(pbc_rf, package="ggRandomForests")
print(pbc_rf)
plot.gg_error(pbc_rf)

## ----surv-rf-plot, echo=FALSE, fig.cap="Survival Forest: Predicted probability of survival.", fig.width=5----
plot.gg_rfsrc(pbc_rf) +
  theme(legend.position=c(.2,.2))

## ----iris-vimp, fig.width=5.5---------------------------------------------------------------------
plot.gg_vimp(iris_rf)+
  theme(legend.position="none")

## ----airq-vimp------------------------------------------------------------------------------------
plot.gg_vimp(airq_rf)

## ----pbc-vimp, warning=FALSE----------------------------------------------------------------------
plot.gg_vimp(pbc_rf)+
  theme(legend.position=c(.8,.2))

## ----pbc-mindepth---------------------------------------------------------------------------------
data(pbc_vs, package="ggRandomForests")
plot.gg_minimal_depth(pbc_vs)

## ----pbc-minvimp, fig.width=6---------------------------------------------------------------------
plot.gg_minimal_vimp(pbc_rf)

## ----variable-plot--------------------------------------------------------------------------------
ggrf <- gg_variable(pbc_rf, time=90)

plot(ggrf, x_var = "bili") +
  theme(legend.position=c(.8,.2))

## ----variable-plot-stacks, fig.height=6, fig.width=6----------------------------------------------
ggrf <- gg_variable(pbc_rf, time=c(90,  365, 3*365))

plot(ggrf, x_var = "bili")+
  scale_shape_manual(values=event.marks, labels=event.labels)+
  scale_color_manual(values=strCol, na.value="lightgrey", drop=FALSE,
                     labels=event.labels)

## ----pbc-partial-bili-----------------------------------------------------------------------------
data(pbc_prtl, package="ggRandomForests")
ggprtl <- gg_partial(pbc_prtl)

plot(ggprtl[[1]])

## ----pbc-partial-albumin--------------------------------------------------------------------------
plot(ggprtl[[4]], se=FALSE)

## ----echo=FALSE-------------------------------------------------------------------------
## re-specify initial options
options(prompt = o1, continue = o2, width = o3, digits = o4)

