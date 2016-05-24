## ----setup, echo=FALSE---------------------------------------------------
## Not displayed ##
library("knitr")
# set global chunk options for knitr. These can be changed in the header for each individual R code chunk
opts_chunk$set(fig.path = 'rmd-rf/rf-', # standard vignette
               prompt = TRUE, 
               comment = NA,
               echo = TRUE, # Change this to TRUE if you want to see all the code examples
               results = FALSE, message = FALSE, warning = FALSE, 
               error = FALSE)

# Setup the R environment
options(object.size = Inf, expressions = 100000, memory = Inf)
options(mc.cores = 1, rf.cores = 0)

## ----libraries-----------------------------------------------------------
library("ggplot2")       # Graphics engine
library("RColorBrewer")    # Nice color palettes
library("plot3D")          # for 3d surfaces. 
library("dplyr")           # Better data manipulations
library("tidyr")           # gather variables into long format
library("parallel")        # mclapply for multicore processing

# Analysis packages.
library("randomForest")    # random forests
library("ggRandomForests") # ggplot2 random forest figures (This!)
theme_set(theme_bw())     # A ggplot2 theme with white background

## ----datastep------------------------------------------------------------
# Load the Boston Housing data
data(Boston, package="MASS")

# Set modes correctly. For binary variables: transform to logical
Boston$chas <- as.logical(Boston$chas)

## ----table, echo=FALSE---------------------------------------------------
cls <- sapply(Boston, class) 
# 
lbls <- 
  #crim
  c("Crime rate by town.",
    # zn
    "Proportion of residential land zoned for lots over 25,000 sq.ft.",
    # indus
    "Proportion of non-retail business acres per town.",
    # chas
    "Charles River (tract bounds river).",
    # nox
    "Nitrogen oxides concentration (10 ppm).",
    # rm
    "Number of rooms per dwelling.",
    # age
    "Proportion of units built prior to 1940.",
    # dis
    "Distances to Boston employment center.",
    # rad
    "Accessibility to highways.",
    # tax
    "Property tax rate per $10,000.",
    # ptratio
    "Pupil teacher ratio by town.",
    # black
    "Proportion of blacks by town.",
    # lstat
    "Lower status of the population (percent).",
    # medv
    "Median value of homes ($1000s).")

# Build a table for data description
dta.labs <- data.frame(cbind(Variable=names(cls), Description=lbls, type=cls))

# Build a named vector for labeling figures later/
st.labs <- as.character(dta.labs$Description)
names(st.labs) <- names(cls)

# Print the descriptive table.
kable(dta.labs, 
      row.names = FALSE, 
      caption="\\code{Boston} housing data dictionary.",
      booktabs = FALSE)

## ----eda, fig.cap="__Figure 1__ EDA variable plots. Points indicate variable value against the median home value variable. Points are colored according to the chas variable.", fig.width=7, fig.height=5----
# Use tidyr::gather to transform the data into long format.
dta <- gather(Boston, variable, value, -medv, -chas)

# plot panels for each covariate colored by the logical chas variable.
ggplot(dta)+
  geom_point(alpha=0.4, aes(x=medv, y=value, color=chas))+
  geom_smooth(aes(x=medv, y=value), se=FALSE)+ 
  labs(y="", x=st.labs["medv"]) +
  scale_color_brewer(palette="Set2")+
  facet_wrap(~variable, scales="free_y", ncol=3)

## ----randomforest--------------------------------------------------------
# Load the data, from the call:
# rf_Boston <- rfsrc(medv~., data=Boston)
rf_Boston <- randomForest(medv~., data=Boston, ntree=1000,
                          importance=TRUE)

# print the forest summary
rf_Boston

## ----error, echo=TRUE, fig.cap="__Figure 2__ Random forest generalization error. OOB error convergence along the number of trees in the forest."----
# Plot the OOB errors against the growth of the forest.
gg_e <- gg_error(rf_Boston)
plot(gg_e)

## ----rfsrc, echo=TRUE, fig.cap="__Figure 3__ OOB predicted median home values. Points are jittered to help visualize predictions for each observation. Boxplot indicates the distribution of the predicted values."----
# Plot predicted median home values.
plot(gg_rfsrc(rf_Boston), alpha=.5)+
  coord_cartesian(ylim=c(5,49))

## ----vimp, echo=TRUE, fig.cap="__Figure 4__ Random forest VIMP plot. Bars are colored by sign of VIMP, longer blue bars indicate more important variables.", fig.width=7, fig.height=5----
# Plot the VIMP rankings of independent variables.
gg_dta <- gg_vimp(rf_Boston)
plot(gg_dta, lbls=st.labs)

## ----variable, echo=TRUE, fig.cap="__Figure 5__ Variable dependence plot. Individual case predictions are marked with points. Loess smooth curve indicates the trend as the variables increase with shaded 95\\% confidence band.", fig.width=7, fig.height=5----
# Create the variable dependence object from the random forest
gg_v <- gg_variable(rf_Boston)

# We want the top ranked minimal depth variables only,
# plotted in minimal depth rank order. 
xvar <- gg_dta$vars[gg_dta$vimp>1]

# plot the variable list in a single panel plot
plot(gg_v, xvar=xvar, panel=TRUE, alpha=.5)+
  labs(y=st.labs["medv"], x="")

## ----chas, echo=TRUE, fig.cap="__Figure 6__ Variable dependence for Charles River logical variable."----
plot(gg_v, xvar="chas", alpha=.4)+
  labs(y=st.labs["medv"])

