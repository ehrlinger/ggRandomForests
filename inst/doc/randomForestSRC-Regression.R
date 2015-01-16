## ----setup, include = FALSE, cache = FALSE, echo = FALSE--------------------------------
library("knitr")
# set global chunk options for knitr. These can be changed in the header for each individual R code chunk
opts_chunk$set(fig.path = 'fig-rfr/rfr-', 
               # fig.align = 'center', 
               # size = 'footnotesize', 
               prompt = TRUE, 
               comment = NA, 
               echo = TRUE, results = TRUE, 
               message = FALSE, warning = FALSE, 
               error = FALSE, prompt = TRUE)

# Setup the R environment
options(object.size = Inf, expressions = 100000, memory = Inf, 
        replace.assign = TRUE, width = 90)

options(mc.cores = 1, rf.cores = 0)

## ----libraries--------------------------------------------------------------------------
#################
# Load_packages #
#################
library("ggplot2")         # Graphics engine for generating all types of plots
library("RColorBrewer")    # Nice color palettes
library("plot3D")          # for 3d surfaces. 

library("dplyr")           # Better data manipulations
library("reshape2")        # for transforming wide data into long data (melt)

library("parallel")        # mclapply for multicore processing

# Analysis packages.
library("randomForestSRC") # random forests for survival, regression and classification
library("ggRandomForests") # ggplot2 random forest figures

#########################################################################
# Default settings
#########################################################################
theme_set(theme_bw())     # A ggplot2 theme with white background

## ----vignette, eval=FALSE---------------------------------------------------------------
#  vignette("randomForestSRC-Regression", package="ggRandomForests")

## ----datastep---------------------------------------------------------------------------
# Load the Boston Housing data
data(Boston, package="MASS")

# Set modes correctly. For binary variables: transform to logical
Boston$chas <- as.logical(Boston$chas)

## ----cleanup, echo=FALSE, results="asis"------------------------------------------------
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
    "Property-tax rate per $10,000.",
    # ptratio
    "Pupil-teacher ratio by town.",
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
kable(dta.labs, format="markdown",row.names = FALSE)

## ----data, fig.cap="_**EDA variable plots.**<a name=\"f:eda\"></a> Points indicate variable value against the median home value variable. Points are colored according to the chas variable._", fig.width=7, fig.height=5----
# Use reshape2::melt to transform the data into long format.
dta <- melt(Boston, id.vars=c("medv","chas"))

# plot panels for each covariate colored by the logical chas variable.
ggplot(dta, aes(x=medv, y=value, color=chas))+
  geom_point(alpha=.4)+
  geom_rug(data=dta %>% filter(is.na(value)))+
  labs(y="", x=st.labs["medv"]) +
  scale_color_brewer(palette="Set2")+
  facet_wrap(~variable, scales="free_y", ncol=3)

## ----randomforest-----------------------------------------------------------------------
# Load the data, from the call:
# rfsrc_Boston <- rfsrc(medv~., data=Boston)
data(rfsrc_Boston)

# print the forest summary
rfsrc_Boston

## ----error, fig.cap="_**Random forest generalization error.**<a name=\"f:error\"></a> OOB error convergence along the number of trees in the forest._"----
# Plot the OOB errors against the growth of the forest.
gg_e <- gg_error(rfsrc_Boston)
plot(gg_e)

## ----rfsrc, fig.cap="_**OOB predicted median home values.**<a name=\"f:prediction\"></a> Points are jittered to help visualize predictions for each observation. Boxplot indicates the distribution of the predicted values._"----
# Plot predicted median home values.
plot(gg_rfsrc(rfsrc_Boston), alpha=.5)+
  coord_cartesian(ylim=c(5,49))

## ----vimp, fig.cap="_**Random forest VIMP plot.**<a name=\"f:vimp\"></a> Bars are colored by sign of VIMP, longer blue bars indicate more important variables._", fig.width=7, fig.height=5----
# Plot the VIMP rankings of independent variables.
plot(gg_vimp(rfsrc_Boston), lbls=st.labs)

## ----minimaldepth, fig.cap="_**Minimal Depth**<a name=\"f:mindepth\"></a> variables in rank order, most important at the top. Vertical dashed line indicates the maximal minimal depth for important variables._", fig.width=7, fig.height=5----
# Load the data, from the call:
# varsel_Boston <- var.select(rfsrc_Boston)
data(varsel_Boston)

# Save the gg_minimal_depth object for later use.
gg_md <- gg_minimal_depth(varsel_Boston)

# plot the object
plot(gg_md, lbls=st.labs)

## ----minimalvimp, fig.cap="_**Comparing Minimal Depth and Vimp rankings**<a name=\"f:minvimp\"></a>. Points on the red dashed line are ranked equivalently, points below have higher VIMP, those above have higher minimal depth ranking. Variables are colored by the sign of the VIMP measure._"----
# gg_minimal_depth objects contain information about
# both minimal depth and VIMP.
plot.gg_minimal_vimp(gg_md)

## ----variable, fig.cap="_**Variable dependence plot.**<a name=\"f:vardepend\"></a> Individual case predictions are marked with points. Loess smooth curve indicates the trend as the variables increase with shaded 95% confidence band._", fig.width=7, fig.height=5----
# Create the variable dependence object from the random forest
gg_v <- gg_variable(rfsrc_Boston)

# We want the top ranked minimal depth variables only,
# plotted in minimal depth rank order. 
xvar <- gg_md$topvars

# plot the variable list in a single panel plot
plot(gg_v, xvar=xvar, panel=TRUE, 
     se=.95, span=1.2, alpha=.4)+
  labs(y=st.labs["medv"], x="")

## ----chas, fig.cap="_**Variable dependence**<a name=\"f:catvardep\"></a> for Charles River logical variable._"----
plot(gg_v, xvar="chas", points=FALSE,
     se=FALSE, notch=TRUE, alpha=.4)+
  labs(y=st.labs["medv"])

## ----partial, fig.cap="_**Partial dependence panels.**<a name=\"f:partial\"></a> Risk adjusted variable dependence for variables in minimal depth rank order._", fig.width=7, fig.height=5----
# Load the data, from the call:
# partial_Boston <- plot.variable(rfsrc_Boston, 
#                                 xvar=gg_md$topvars, 
#                                 partial=TRUE, sorted=FALSE, 
#                                 show.plots = FALSE )
data(partial_Boston)

# generate a list of gg_partial objects, one per xvar.
gg_p <- gg_partial(partial_Boston)

# plot the variable list in a single panel plot
plot(gg_p, xvar=xvar, panel=TRUE, se=FALSE) +
  labs(y=st.labs["medv"], x="")

## ----interactions, fig.cap="_**Minimal depth variable interactions.**<a name=\"f:interaction\"></a> Reference variables are marked with red cross in each panel. Higher values indicate lower interactivity with reference variable._", fig.width=7, fig.height=5----
# Load the data, from the call:
# interaction_Boston <- find.interactions(rfsrc_Boston)
data(interaction_Boston)

# Plot the results in a single panel.
plot(gg_interaction(interaction_Boston), 
     xvar=gg_md$topvars, panel=TRUE)

## ----coplots, fig.cap="_**Variable Coplots.**<a name=\"f:coplot_lstat\"></a> Predicted median home values as a function of percentage of lower status population, stratified by average number of rooms groups._", fig.width=7, fig.height=5----
# Find the rm variable points to create 6 intervals of roughly 
# equal size population
rm_pts <- quantile_pts(rfsrc_Boston$xvar$rm, groups=6, intervals=TRUE)

# Pass these variable points to create the 6 (factor) intervals
rm_grp <- cut(rfsrc_Boston$xvar$rm, breaks=rm_pts)

# Append the group factor to the gg_variable object
gg_v$rm_grp <- rm_grp

# Modify the labels for descriptive panel titles 
levels(gg_v$rm_grp) <- paste("rm in ", levels(gg_v$rm_grp), sep="")

# Create a variable dependence (co)plot, faceted on group membership.
plot(gg_v, xvar = "lstat", smooth = TRUE, 
     method = "loess", span=1.5, alpha = .5, se = FALSE) + 
  labs(y = st.labs["medv"], x=st.labs["lstat"]) + 
  theme(legend.position = "none") + 
  scale_color_brewer(palette = "Set3") + 
  facet_wrap(~rm_grp)

## ----coplots2, fig.cap="_**Variable Coplots.**<a name=\"f:coplot_rm\"></a> Predicted median home value as a function of average number of rooms, stratified by percentage of lower status groups._", fig.width=7, fig.height=5----
# Find the lstat variable points to create 6 intervals of roughly 
# equal size population
lstat_pts <- quantile_pts(rfsrc_Boston$xvar$lstat, groups=6, intervals=TRUE)

# Pass these variable points to create the 6 (factor) intervals
lstat_grp <- cut(rfsrc_Boston$xvar$lstat, breaks=lstat_pts)

# Append the group factor to the gg_variable object
gg_v$lstat_grp <- lstat_grp

# Modify the labels for descriptive panel titles 
levels(gg_v$lstat_grp) <- paste("lstat in ", levels(gg_v$lstat_grp), " (%)",sep="")

# Create a variable dependence (co)plot, faceted on group membership.
plot(gg_v, xvar = "rm", smooth = TRUE, 
     method = "loess", span=1.5, alpha = .5, se = FALSE) + 
  labs(y = st.labs["medv"], x=st.labs["rm"]) + 
  theme(legend.position = "none") + 
  scale_color_brewer(palette = "Set3") + 
  #scale_shape_manual(values = event.marks, labels = event.labels)+ 
  facet_wrap(~lstat_grp)

## ----prtl-copl, eval=FALSE--------------------------------------------------------------
#  partial_coplot_Boston <- gg_partial_coplot(rfsrc_Boston, xvar="lstat",
#                                             groups=rm_grp,
#                                             show.plots=FALSE)

## ----prtl-coplots, fig.cap="_**Partial Coplots.**<a name=\"f:part_coplot_lstat\"></a> Risk adjusted predicted median value as a function of Lower Status, conditional on groups of average number of rooms._", fig.width=7, fig.height=5----
# Load the stored partial coplot data.
data(partial_coplot_Boston)

# Partial coplot
plot(partial_coplot_Boston, se=FALSE)+
  labs(x=st.labs["lstat"], y=st.labs["medv"], 
       color="Room", shape="Room")+
  scale_color_brewer(palette="Set1")

## ----prtl-copl2, eval=FALSE-------------------------------------------------------------
#  partial_coplot_Boston2 <- gg_partial_coplot(rfsrc_Boston, xvar="rm",
#                                              groups=lstat_grp,
#                                              show.plots=FALSE)

## ----prtl-coplots2, fig.cap="_**Partial Coplots.**<a name=\"f:part_coplot_rm\"></a> Risk adjusted predicted median value as a function of average number of rooms, conditional on groups of percentage of lower status population._", fig.width=7, fig.height=5----
# Load the stored partial coplot data.
data(partial_coplot_Boston2)

# Partial coplot
plot(partial_coplot_Boston2, se=FALSE)+
  labs(x=st.labs["rm"], y=st.labs["medv"], 
       color="Lower Status", shape="Lower Status")+
  scale_color_brewer(palette="Set1")

## ----def-pts----------------------------------------------------------------------------
# Find the quantile points to create 50 cut points
rm_pts <- quantile_pts(rfsrc_Boston$xvar$rm, groups=50)

## ----prtl-surface, eval=FALSE-----------------------------------------------------------
#  # Generate the gg_partial_coplot data object
#  system.time(partial_Boston_surf <- lapply(rm_pts, function(ct){
#    rfsrc_Boston$xvar$rm <- ct
#    plot.variable(rfsrc_Boston, xvar = "lstat", time = 1,
#                  npts = 50, show.plots = FALSE,
#                  partial = TRUE)
#  }))
#  #     user   system  elapsed
#  # 1109.641   76.516 1199.732

## ----contour3d, fig.cap="_**Partial coplot contour plot.**<a name=\"f:contour\"></a> Contours of median home value along the lstat/rm plane._", fig.width=7, fig.height=5----
# Load the stored partial coplot data.
data(partial_Boston_surf)

# Instead of groups, we want the raw rm point values,
# To make the dimensions match, we need to repeat the values
# for each of the 50 points in the lstat direction
rm.tmp <- do.call(c,lapply(rm_pts, 
                           function(grp){rep(grp, 50)}))

# Convert the list of plot.variable output to 
partial_surf <- do.call(rbind,lapply(partial_Boston_surf, gg_partial))

# attach the data to the gg_partial_coplot
partial_surf$rm <- rm.tmp

# ggplot2 contour plot of x, y and z data.
ggplot(partial_surf, aes(x=lstat, y=rm, z=yhat))+
  stat_contour(aes(colour = ..level..), binwidth = .5)+
  labs(x=st.labs["lstat"], y=st.labs["rm"], 
       color="Median Home Values")+
  scale_colour_gradientn(colours=topo.colors(10))

## ----surface3d, fig.cap="_**Partial plot surface.**<a name=\"f:surface\"></a>_", fig.width=7, fig.height=5----
# Modify the figure margins to make the figure larger
par(mai = c(0,0,0,0))

# Transform the gg_partial_coplot object into a list of three named matrices
# for surface plotting with plot3D::surf3D
srf <- surface_matrix(partial_surf, c("lstat", "rm", "yhat"))

# Generate the figure.
surf3D(x=srf$x, y=srf$y, z=srf$z, col=topo.colors(10),
       colkey=FALSE, border = "black", bty="b2", 
       shade = 0.5, expand = 0.5, 
       lighting = TRUE, lphi = -50,
       xlab="Lower Status", ylab="Average Rooms", zlab="Median Value"
)

