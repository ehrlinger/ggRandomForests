pkgname <- "ggRandomForests"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "ggRandomForests-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('ggRandomForests')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("calc_auc")
### * calc_auc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_auc
### Title: Area Under the ROC Curve calculator
### Aliases: calc_auc calc_auc.gg_roc

### ** Examples

##
## Taken from the gg_roc example
# rfsrc_iris <- rfsrc(Species ~ ., data = iris)
data(rfsrc_iris)

## Not run: 
##D gg_dta <- gg_roc(rfsrc_iris, which.outcome=1)
##D 
##D calc_auc(gg_dta)
## End(Not run)

gg_dta <- gg_roc(rfsrc_iris, which.outcome=2)

calc_auc(gg_dta)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_auc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_roc.rfsrc")
### * calc_roc.rfsrc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_roc.rfsrc
### Title: Receiver Operator Characteristic calculator
### Aliases: calc_roc.rfsrc calc_roc.randomForest calc_roc

### ** Examples

## Taken from the gg_roc example
# rfsrc_iris <- rfsrc(Species ~ ., data = iris)
data(rfsrc_iris)
gg_dta <- calc_roc.rfsrc(rfsrc_iris, rfsrc_iris$yvar, which.outcome=1, oob=TRUE)
gg_dta <- calc_roc.rfsrc(rfsrc_iris, rfsrc_iris$yvar, which.outcome=1, oob=FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_roc.rfsrc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("combine.gg_partial")
### * combine.gg_partial

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: combine.gg_partial
### Title: combine two gg_partial objects
### Aliases: combine.gg_partial combine.gg_partial_list

### ** Examples

# Load a set of plot.variable partial plot data
data(partial_pbc)

# A list of 2 plot.variable objects
length(partial_pbc) 
class(partial_pbc)

class(partial_pbc[[1]])
class(partial_pbc[[2]])

# Create gg_partial objects
ggPrtl.1 <- gg_partial(partial_pbc[[1]])
ggPrtl.2 <- gg_partial(partial_pbc[[2]])

# Combine the objects to get multiple time curves 
# along variables on a single figure.
ggpart <- combine.gg_partial(ggPrtl.1, ggPrtl.2, 
                             lbls = c("1 year", "3 years"))
                             
# Plot each figure separately
plot(ggpart)                                  

# Get the continuous data for a panel of continuous plots.
ggcont <- ggpart
ggcont$edema <- ggcont$ascites <- ggcont$stage <- NULL
plot(ggcont, panel=TRUE) 

# And the categorical for a panel of categorical plots.
nms <- colnames(sapply(ggcont, function(st){st}))
for(ind in nms){
   ggpart[[ind]] <- NULL
}
plot(ggpart, panel=TRUE) 





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("combine.gg_partial", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gg_error")
### * gg_error

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gg_error
### Title: randomForestSRC error rate data object
### Aliases: gg_error gg_error.rfsrc gg_error.randomForest
###   gg_error.randomForest.formula

### ** Examples

## Examples from RFSRC package...
## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## ------------- iris data
## You can build a randomForest
# rfsrc_iris <- rfsrc(Species ~ ., data = iris)
# ... or load a cached randomForestSRC object
data(rfsrc_iris, package="ggRandomForests")

# Get a data.frame containing error rates
gg_dta<- gg_error(rfsrc_iris)

# Plot the gg_error object
plot(gg_dta)

## ------------------------------------------------------------
## Regression example
## ------------------------------------------------------------
## Not run: 
##D ## ------------- airq data
##D rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
##D 
##D # Get a data.frame containing error rates
##D gg_dta<- gg_error(rfsrc_airq)
##D 
##D # Plot the gg_error object
##D plot(gg_dta)
## End(Not run)

## ------------- Boston data
data(rfsrc_Boston, package="ggRandomForests")

# Get a data.frame containing error rates
gg_dta<- gg_error(rfsrc_Boston)

# Plot the gg_error object
plot(gg_dta)

## Not run: 
##D ## ------------- mtcars data
##D 
##D # Get a data.frame containing error rates
##D gg_dta<- gg_error(rfsrc_mtcars)
##D 
##D # Plot the gg_error object
##D plot(gg_dta)
## End(Not run)

## ------------------------------------------------------------
## Survival example
## ------------------------------------------------------------
## Not run: 
##D ## ------------- veteran data
##D ## randomized trial of two treatment regimens for lung cancer
##D data(veteran, package = "randomForestSRC")
##D rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = dta$veteran, ...)
##D 
##D gg_dta <- gg_error(rfsrc_veteran)
##D plot(gg_dta)
## End(Not run)

## ------------- pbc data
# Load a cached randomForestSRC object
data(rfsrc_pbc, package="ggRandomForests")

gg_dta <- gg_error(rfsrc_pbc)
plot(gg_dta)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gg_error", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gg_interaction")
### * gg_interaction

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gg_interaction
### Title: Minimal Depth Variable Interaction data object
###   ('find.interaction').
### Aliases: gg_interaction gg_interaction.randomForest
###   gg_interaction.rfsrc

### ** Examples

## Examples from randomForestSRC package... 
## ------------------------------------------------------------
## find interactions, classification setting
## ------------------------------------------------------------
## -------- iris data
## iris.obj <- rfsrc(Species ~., data = iris)
## TODO: VIMP interactions not handled yet....
## randomForestSRC::find.interaction(iris.obj, method = "vimp", nrep = 3)
## interaction_iris <- randomForestSRC::find.interaction(iris.obj)
data(interaction_iris, package="ggRandomForests")
gg_dta <- gg_interaction(interaction_iris)

plot(gg_dta, xvar="Petal.Width")
plot(gg_dta, panel=TRUE)

## ------------------------------------------------------------
## find interactions, regression setting
## ------------------------------------------------------------
## Not run: 
##D ## -------- air quality data
##D ## airq.obj <- rfsrc(Ozone ~ ., data = airquality)
##D ##
##D ## TODO: VIMP interactions not handled yet....
##D ## randomForestSRC::find.interaction(airq.obj, method = "vimp", nrep = 3)
##D ## interaction_airq <- randomForestSRC::find.interaction(airq.obj)
##D data(interaction_airq, package="ggRandomForests")
##D gg_dta <- gg_interaction(interaction_airq)
##D 
##D plot(gg_dta, xvar="Temp")
##D plot(gg_dta, xvar="Solar.R")
##D 
##D plot(gg_dta, panel=TRUE)
## End(Not run)

## -------- Boston data
data(interaction_Boston, package="ggRandomForests")
gg_dta <- gg_interaction(interaction_Boston)

plot(gg_dta, panel=TRUE)

## Not run: 
##D ## -------- mtcars data
##D data(interaction_mtcars, package="ggRandomForests")
##D gg_dta <- gg_interaction(interaction_mtcars)
##D 
##D plot(gg_dta, panel=TRUE)
## End(Not run)

## ------------------------------------------------------------
## find interactions, survival setting
## ------------------------------------------------------------
## -------- pbc data
## data(pbc, package = "randomForestSRC") 
## pbc.obj <- rfsrc(Surv(days,status) ~ ., pbc, nsplit = 10)
## interaction_pbc <- randomForestSRC::find.interaction(pbc.obj, nvar = 8)
data(interaction_pbc, package="ggRandomForests")
gg_dta <- gg_interaction(interaction_pbc)

plot(gg_dta, xvar="bili")
plot(gg_dta, panel=TRUE)

## Not run: 
##D ## -------- veteran data
##D data(interaction_veteran, package="ggRandomForests")
##D gg_dta <- gg_interaction(interaction_veteran)
##D 
##D plot(gg_dta, panel=TRUE)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gg_interaction", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gg_minimal_depth")
### * gg_minimal_depth

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gg_minimal_depth
### Title: Minimal depth data object ('[randomForestSRC]{var.select}')
### Aliases: gg_minimal_depth gg_minimal_depth.rfsrc
###   gg_minimal_depth.randomForest

### ** Examples

## Examples from RFSRC package... 
## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## -------- iris data
## You can build a randomForest
# rfsrc_iris <- rfsrc(Species ~ ., data = iris)
# varsel_iris <- randomForestSRC::var.select(rfsrc_iris)
# ... or load a cached randomForestSRC object
data(varsel_iris, package="ggRandomForests")

# Get a data.frame containing minimaldepth measures
gg_dta<- gg_minimal_depth(varsel_iris)

# Plot the gg_minimal_depth object
plot(gg_dta)

## ------------------------------------------------------------
## Regression example
## ------------------------------------------------------------
## Not run: 
##D ## -------- air quality data
##D # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
##D # varsel_airq <- randomForestSRC::var.select(rfsrc_airq)
##D # ... or load a cached randomForestSRC object
##D data(varsel_airq, package="ggRandomForests")
##D 
##D # Get a data.frame containing error rates
##D gg_dta<- gg_minimal_depth(varsel_airq)
##D 
##D # Plot the gg_minimal_depth object
##D plot(gg_dta)
## End(Not run)

## -------- Boston data
data(varsel_Boston, package="ggRandomForests")

# Get a data.frame containing error rates
plot(gg_minimal_depth(varsel_Boston))

## Not run: 
##D ## -------- mtcars data
##D data(varsel_mtcars, package="ggRandomForests")
##D 
##D # Get a data.frame containing error rates
##D plot.gg_minimal_depth(varsel_mtcars)
## End(Not run)

## ------------------------------------------------------------
## Survival example
## ------------------------------------------------------------
## Not run: 
##D ## -------- veteran data
##D ## veteran data
##D ## randomized trial of two treatment regimens for lung cancer
##D # data(veteran, package = "randomForestSRC")
##D # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
##D # varsel_veteran <- randomForestSRC::var.select(rfsrc_veteran)
##D # Load a cached randomForestSRC object
##D data(varsel_veteran, package="ggRandomForests")
##D 
##D gg_dta <- gg_minimal_depth(varsel_veteran)
##D plot(gg_dta)
## End(Not run)

## -------- pbc data
data(varsel_pbc, package="ggRandomForests")

gg_dta <- gg_minimal_depth(varsel_pbc)
plot(gg_dta)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gg_minimal_depth", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gg_minimal_vimp")
### * gg_minimal_vimp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gg_minimal_vimp
### Title: Minimal depth vs VIMP comparison by variable rankings.
### Aliases: gg_minimal_vimp gg_minimal_vimp.randomForest
###   gg_minimal_vimp.rfsrc

### ** Examples

## Examples from RFSRC package... 
## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## -------- iris data
## You can build a randomForest
# rfsrc_iris <- rfsrc(Species ~ ., data = iris)
# varsel_iris <- randomForestSRC::var.select(rfsrc_iris)
# ... or load a cached randomForestSRC object
data(varsel_iris, package="ggRandomForests")

# Get a data.frame containing minimaldepth measures
gg_dta<- gg_minimal_vimp(varsel_iris)

# Plot the gg_minimal_depth object
plot(gg_dta)

## ------------------------------------------------------------
## Regression example
## ------------------------------------------------------------
## Not run: 
##D ## -------- air quality data
##D # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
##D # varsel_airq <- randomForestSRC::var.select(rfsrc_airq)
##D # ... or load a cached randomForestSRC object
##D data(varsel_airq, package="ggRandomForests")
##D 
##D # Get a data.frame containing error rates
##D gg_dta<- gg_minimal_vimp(varsel_airq)
##D 
##D # Plot the gg_minimal_vimp object
##D plot(gg_dta)
## End(Not run)

## -------- Boston data
data(varsel_Boston, package="ggRandomForests")

# Get a data.frame containing error rates
gg_dta<- gg_minimal_vimp(varsel_Boston)

# Plot the gg_minimal_vimp object
plot(gg_dta)

## Not run: 
##D ## -------- mtcars data
##D data(varsel_mtcars, package="ggRandomForests")
##D 
##D # Get a data.frame containing error rates
##D gg_dta<- gg_minimal_vimp(varsel_mtcars)
##D 
##D # Plot the gg_minimal_vimp object
##D plot(gg_dta)
## End(Not run)
## ------------------------------------------------------------
## Survival example
## ------------------------------------------------------------
## Not run: 
##D ## -------- veteran data
##D ## randomized trial of two treatment regimens for lung cancer
##D # data(veteran, package = "randomForestSRC")
##D # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
##D # varsel_veteran <- randomForestSRC::var.select(rfsrc_veteran)
##D # Load a cached randomForestSRC object
##D data(varsel_veteran, package="ggRandomForests")
##D 
##D gg_dta <- gg_minimal_vimp(varsel_veteran)
##D plot(gg_dta)
## End(Not run)
## -------- pbc data
data(varsel_pbc, package="ggRandomForests")

gg_dta <- gg_minimal_vimp(varsel_pbc)
plot(gg_dta)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gg_minimal_vimp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gg_partial")
### * gg_partial

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gg_partial
### Title: Partial variable dependence object
### Aliases: gg_partial gg_partial_list gg_partial.rfsrc
###   gg_partial.randomForest

### ** Examples

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
## Not run: 
##D ## -------- air quality data
##D ## airquality "Wind" partial dependence plot
##D ##
##D # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality)
##D # partial_airq <- plot.variable(rfsrc_airq, xvar.names = "Wind",
##D #                            partial=TRUE, show.plot=FALSE)
##D data(partial_airq, package="ggRandomForests")
##D 
##D gg_dta <- gg_partial(partial_airq)
##D plot(gg_dta)
##D 
##D gg_dta.m <- gg_dta[["Month"]]
##D plot(gg_dta.m, notch=TRUE)
##D 
##D gg_dta[["Month"]] <- NULL
##D plot(gg_dta, panel=TRUE)
## End(Not run)

## -------- Boston data
data(partial_Boston, package="ggRandomForests")

gg_dta <- gg_partial(partial_Boston)
plot(gg_dta, panel=TRUE)

## Not run: 
##D ## -------- mtcars data
##D data(partial_mtcars, package="ggRandomForests")
##D gg_dta <- gg_partial(partial_mtcars)
##D 
##D gg_dta.cat <- gg_dta
##D gg_dta.cat[["disp"]] <- gg_dta.cat[["wt"]] <- gg_dta.cat[["hp"]] <- NULL
##D gg_dta.cat[["drat"]] <- gg_dta.cat[["carb"]] <- gg_dta.cat[["qsec"]] <- NULL
##D  
##D plot(gg_dta.cat, panel=TRUE, notch=TRUE)
##D 
##D gg_dta[["cyl"]] <- gg_dta[["vs"]] <- gg_dta[["am"]] <- NULL
##D gg_dta[["gear"]] <- NULL
##D plot(gg_dta, panel=TRUE)
## End(Not run)

## ------------------------------------------------------------
## survival examples
## ------------------------------------------------------------
## Not run: 
##D ## -------- veteran data
##D ## survival "age" partial variable dependence plot
##D ##
##D # data(veteran, package = "randomForestSRC")
##D # rfsrc_veteran <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
##D #
##D ## 30 day partial plot for age
##D # partial_veteran <- plot.variable(rfsrc_veteran, surv.type = "surv", 
##D #                               partial = TRUE, time=30, 
##D #                               xvar.names = "age", 
##D #                               show.plots=FALSE)
##D data(partial_veteran, package="ggRandomForests")
##D 
##D gg_dta <- gg_partial(partial_veteran[[1]])
##D plot(gg_dta)
##D 
##D gg_dta.cat <- gg_dta
##D gg_dta[["celltype"]] <- gg_dta[["trt"]] <- gg_dta[["prior"]] <- NULL
##D plot(gg_dta, panel=TRUE)
##D 
##D gg_dta.cat[["karno"]] <- gg_dta.cat[["diagtime"]] <- gg_dta.cat[["age"]] <- NULL 
##D plot(gg_dta.cat, panel=TRUE, notch=TRUE)
##D 
##D gg_dta <- lapply(partial_veteran, gg_partial)
##D gg_dta <- combine.gg_partial(gg_dta[[1]], gg_dta[[2]] )
##D 
##D plot(gg_dta[["karno"]])
##D plot(gg_dta[["celltype"]])
##D 
##D gg_dta.cat <- gg_dta
##D gg_dta[["celltype"]] <- gg_dta[["trt"]] <- gg_dta[["prior"]] <- NULL
##D plot(gg_dta, panel=TRUE)
##D 
##D gg_dta.cat[["karno"]] <- gg_dta.cat[["diagtime"]] <- gg_dta.cat[["age"]] <- NULL 
##D plot(gg_dta.cat, panel=TRUE, notch=TRUE)
## End(Not run)
## -------- pbc data
data("partial_pbc", package = "ggRandomForests")
data("varsel_pbc", package = "ggRandomForests")
xvar <- varsel_pbc$topvars

# Convert all partial plots to gg_partial objects
gg_dta <- lapply(partial_pbc, gg_partial)

# Combine the objects to get multiple time curves 
# along variables on a single figure.
pbc_ggpart <- combine.gg_partial(gg_dta[[1]], gg_dta[[2]], 
                                 lbls = c("1 Year", "3 Years"))

summary(pbc_ggpart)
class(pbc_ggpart[["bili"]])

# Plot the highest ranked variable, by name.
#plot(pbc_ggpart[["bili"]])
     
# Create a temporary holder and remove the stage and edema data
ggpart <- pbc_ggpart
ggpart$edema <- NULL

# Panel plot the remainder.
plot(ggpart, panel = TRUE)

#plot(pbc_ggpart[["edema"]], panel=TRUE) #,
     # notch = TRUE, alpha = .3, outlier.shape = NA) 
  



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gg_partial", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gg_partial_coplot.rfsrc")
### * gg_partial_coplot.rfsrc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gg_partial_coplot.rfsrc
### Title: Data structures for stratified partial coplots
### Aliases: gg_partial_coplot.rfsrc gg_partial_coplot

### ** Examples

# Load the forest
data(rfsrc_pbc, package="ggRandomForests")

# Create the variable plot.
ggvar <- gg_variable(rfsrc_pbc, time = 1)

# Find intervals with similar number of observations.
copper_cts <- quantile_pts(ggvar$copper, groups = 6, intervals = TRUE)

# Create the conditional groups and add to the gg_variable object
copper_grp <- cut(ggvar$copper, breaks = copper_cts)

## Not run: 
##D ## We would run this, but it's expensive 
##D partial_coplot_pbc <- gg_partial_coplot(rfsrc_pbc, xvar = "bili", 
##D                                          groups = copper_grp, 
##D                                          surv_type = "surv", 
##D                                          time = 1, 
##D                                          show.plots = FALSE)
## End(Not run)
## so load the cached set
data(partial_coplot_pbc, package="ggRandomForests")

# Partial coplot
plot(partial_coplot_pbc) #, se = FALSE)
 




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gg_partial_coplot.rfsrc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gg_rfsrc.rfsrc")
### * gg_rfsrc.rfsrc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gg_rfsrc.rfsrc
### Title: Predicted response data object
### Aliases: gg_rfsrc.rfsrc gg_rfsrc

### ** Examples

## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## -------- iris data
# rfsrc_iris <- rfsrc(Species ~ ., data = iris)
data(rfsrc_iris, package="ggRandomForests")
gg_dta<- gg_rfsrc(rfsrc_iris)

plot(gg_dta)

## ------------------------------------------------------------
## Regression example
## ------------------------------------------------------------
## Not run: 
##D ## -------- air quality data
##D # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
##D data(rfsrc_airq, package="ggRandomForests")
##D gg_dta<- gg_rfsrc(rfsrc_airq)
##D 
##D plot(gg_dta)
## End(Not run)

## -------- Boston data
data(rfsrc_Boston, package="ggRandomForests")
plot(gg_rfsrc(rfsrc_Boston)) 

## Not run: 
##D ## -------- mtcars data
##D data(rfsrc_mtcars, package="ggRandomForests")
##D gg_dta<- gg_rfsrc(rfsrc_mtcars)
##D 
##D plot(gg_dta)
## End(Not run)
## ------------------------------------------------------------
## Survival example
## ------------------------------------------------------------
## Not run: 
##D ## -------- veteran data
##D ## randomized trial of two treatment regimens for lung cancer
##D # data(veteran, package = "randomForestSRC")
##D # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
##D data(rfsrc_veteran, package = "ggRandomForests")
##D gg_dta <- gg_rfsrc(rfsrc_veteran)
##D plot(gg_dta)
##D 
##D gg_dta <- gg_rfsrc(rfsrc_veteran, conf.int=.95)
##D plot(gg_dta)
##D 
##D gg_dta <- gg_rfsrc(rfsrc_veteran, by="trt")
##D plot(gg_dta)
## End(Not run)

## -------- pbc data
## We don't run this because of bootstrap confidence limits
data(rfsrc_pbc, package = "ggRandomForests")

## Not run: 
##D gg_dta <- gg_rfsrc(rfsrc_pbc)
##D plot(gg_dta)
##D 
##D gg_dta <- gg_rfsrc(rfsrc_pbc, conf.int=.95)
##D plot(gg_dta)
## End(Not run)

gg_dta <- gg_rfsrc(rfsrc_pbc, by="treatment")
plot(gg_dta)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gg_rfsrc.rfsrc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gg_roc.rfsrc")
### * gg_roc.rfsrc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gg_roc.rfsrc
### Title: ROC (Receiver operator curve) data from a classification random
###   forest.
### Aliases: gg_roc.rfsrc gg_roc gg_roc.randomForest

### ** Examples

## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## -------- iris data
#rfsrc_iris <- rfsrc(Species ~ ., data = iris)
data(rfsrc_iris, package="ggRandomForests")

# ROC for setosa
gg_dta <- gg_roc(rfsrc_iris, which.outcome=1)
plot(gg_dta)

# ROC for versicolor
gg_dta <- gg_roc(rfsrc_iris, which.outcome=2)
plot(gg_dta)

# ROC for virginica
gg_dta <- gg_roc(rfsrc_iris, which.outcome=3)
plot(gg_dta)






base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gg_roc.rfsrc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gg_survival")
### * gg_survival

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gg_survival
### Title: Nonparametric survival estimates.
### Aliases: gg_survival

### ** Examples

## -------- pbc data
data(pbc, package="randomForestSRC")
pbc$time <- pbc$days/364.25

# This is the same as kaplan
gg_dta <- gg_survival(interval="time", censor="status", 
                     data=pbc)
                     
plot(gg_dta, error="none")
plot(gg_dta)

# Stratified on treatment variable.
gg_dta <- gg_survival(interval="time", censor="status", 
                     data=pbc, by="treatment")
                     
plot(gg_dta, error="none")
plot(gg_dta)

# ...with smaller confidence limits.
gg_dta <- gg_survival(interval="time", censor="status", 
                     data=pbc, by="treatment", conf.int=.68)
                     
plot(gg_dta, error="lines")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gg_survival", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gg_variable")
### * gg_variable

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gg_variable
### Title: Marginal variable depedance data object.
### Aliases: gg_variable gg_variable.rfsrc gg_variable.randomForest
###   gg_variable.random

### ** Examples

## ------------------------------------------------------------
## classification
## ------------------------------------------------------------
## -------- iris data
## iris
#rfsrc_iris <- rfsrc(Species ~., data = iris)
data(rfsrc_iris, package="ggRandomForests")

gg_dta <- gg_variable(rfsrc_iris)
plot(gg_dta, xvar="Sepal.Width")
plot(gg_dta, xvar="Sepal.Length")

plot(gg_dta, xvar=rfsrc_iris$xvar.names, 
     panel=TRUE) # , se=FALSE)

## ------------------------------------------------------------
## regression
## ------------------------------------------------------------
## Not run: 
##D ## -------- air quality data
##D #rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality)
##D data(rfsrc_airq, package="ggRandomForests")
##D gg_dta <- gg_variable(rfsrc_airq)
##D 
##D # an ordinal variable 
##D gg_dta[,"Month"] <- factor(gg_dta[,"Month"])
##D 
##D plot(gg_dta, xvar="Wind")
##D plot(gg_dta, xvar="Temp")
##D plot(gg_dta, xvar="Solar.R")
##D 
##D 
##D plot(gg_dta, xvar=c("Solar.R", "Wind", "Temp", "Day"), panel=TRUE)
##D 
##D plot(gg_dta, xvar="Month", notch=TRUE)
## End(Not run)
## Not run: 
##D ## -------- motor trend cars data
##D #rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
##D data(rfsrc_mtcars, package="ggRandomForests")
##D gg_dta <- gg_variable(rfsrc_mtcars)
##D 
##D # mtcars$cyl is an ordinal variable 
##D gg_dta$cyl <- factor(gg_dta$cyl)
##D gg_dta$am <- factor(gg_dta$am)
##D gg_dta$vs <- factor(gg_dta$vs)
##D gg_dta$gear <- factor(gg_dta$gear)
##D gg_dta$carb <- factor(gg_dta$carb)
##D 
##D plot(gg_dta, xvar="cyl")
##D 
##D # Others are continuous
##D plot(gg_dta, xvar="disp")
##D plot(gg_dta, xvar="hp")
##D plot(gg_dta, xvar="wt")
##D 
##D 
##D # panels
##D plot(gg_dta,xvar=c("disp","hp", "drat", "wt", "qsec"),  panel=TRUE)
##D plot(gg_dta, xvar=c("cyl", "vs", "am", "gear", "carb"), panel=TRUE, notch=TRUE)
## End(Not run)
## -------- Boston data

## ------------------------------------------------------------
## survival examples
## ------------------------------------------------------------
## Not run: 
##D ## -------- veteran data
##D ## survival
##D # data(veteran, package = "randomForestSRC")
##D # rfsrc_veteran <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
##D data(rfsrc_veteran, package="ggRandomForests")
##D 
##D # get the 1 year survival time.
##D gg_dta <- gg_variable(rfsrc_veteran, time=90)
##D 
##D # Generate variable dependence plots for age and diagtime
##D plot(gg_dta, xvar = "age")
##D plot(gg_dta, xvar = "diagtime", )
##D 
##D # Generate coplots
##D plot(gg_dta, xvar = c("age", "diagtime"), panel=TRUE, se=FALSE)
##D 
##D # If we want to compare survival at different time points, say 30, 90 day 
##D # and 1 year
##D gg_dta <- gg_variable(rfsrc_veteran, time=c(30, 90, 365))
##D 
##D # Generate variable dependence plots for age and diagtime
##D plot(gg_dta, xvar = "age")
## End(Not run)
## -------- pbc data



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gg_variable", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gg_vimp")
### * gg_vimp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gg_vimp
### Title: Variable Importance (VIMP) data object
### Aliases: gg_vimp gg_vimp.rfsrc gg_vimp.randomForest
###   gg_vimp.randomForest.formula

### ** Examples

## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## -------- iris data
# rfsrc_iris <- rfsrc(Species ~ ., data = iris)
data(rfsrc_iris, package="ggRandomForests")
gg_dta <- gg_vimp(rfsrc_iris)
plot(gg_dta)
 
## ------------------------------------------------------------
## regression example
## ------------------------------------------------------------
## Not run: 
##D ## -------- air quality data 
##D # rfsrc_airq <- rfsrc(Ozone ~ ., airquality)
##D data(rfsrc_airq, package="ggRandomForests")
##D gg_dta <- gg_vimp(rfsrc_airq)
##D plot(gg_dta)
## End(Not run)

## -------- Boston data
data(rfsrc_Boston, package="ggRandomForests")
gg_dta <- gg_vimp(rfsrc_Boston)
plot(gg_dta)

## Not run: 
##D ## -------- mtcars data
##D data(rfsrc_mtcars, package="ggRandomForests")
##D gg_dta <- gg_vimp(rfsrc_mtcars)
##D plot(gg_dta)
## End(Not run)
## ------------------------------------------------------------
## survival example
## ------------------------------------------------------------
## Not run: 
##D ## -------- veteran data
##D data(rfsrc_veteran, package="ggRandomForests")
##D gg_dta <- gg_vimp(rfsrc_veteran)
##D plot(gg_dta)
## End(Not run)

## -------- pbc data
data(rfsrc_pbc, package="ggRandomForests")
gg_dta <- gg_vimp(rfsrc_pbc)
plot(gg_dta)

# Restrict to only the top 10.
gg_dta <- gg_vimp(rfsrc_pbc, nvar=10)
plot(gg_dta)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gg_vimp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("interaction_data")
### * interaction_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: interaction_data
### Title: Cached 'find.interaction' matrix objects for examples,
###   diagnostics and vignettes. Data sets storing 'find.interaction'
###   matrix objects corresponding to training data according to the
###   following naming convention: * 'interaction_iris' - from a
###   randomForestSR[C] for the 'iris' data set.  * 'interaction_Boston' -
###   from a randomForestS[R]C for the 'Boston' housing data set ('MASS'
###   package).  * 'interaction_pbc' - from a randomForest[S]RC for the
###   'pbc' data set ('randomForestSRC' package)
### Aliases: interaction_data interaction_iris interaction_Boston
###   interaction_pbc interaction_iris, interaction_Boston,
### Keywords: datasets

### ** Examples

## Not run: 
##D #---------------------------------------------------------------------
##D # iris data - classification random forest
##D #---------------------------------------------------------------------
##D # load the rfsrc object from the cached data
##D data(rfsrc_iris, package="ggRandomForests")
##D 
##D # The interaction table 
##D interaction_iris <- find.interaction(rfsrc_iris)
##D 
##D # plot the forest interaction table
##D gg_dta <- gg_interaction(interaction_iris)
##D plot(gg_dta, panel=TRUE)
##D 
##D #---------------------------------------------------------------------
##D # MASS::Boston data - regression random forest 
##D #---------------------------------------------------------------------
##D # load the rfsrc object from the cached data
##D data(rfsrc_Boston, package="ggRandomForests")
##D 
##D # The interaction table 
##D interaction_Boston <- find.interaction(rfsrc_Boston)
##D 
##D # plot the forest interaction table
##D gg_dta <- gg_interaction(interaction_Boston)
##D plot(gg_dta, panel=TRUE)
##D 
##D #---------------------------------------------------------------------
##D # randomForestSRC::pbc data - survival random forest
##D #---------------------------------------------------------------------
##D # load the rfsrc object from the cached data
##D data(rfsrc_pbc, package="ggRandomForests")
##D 
##D # The interaction table 
##D interaction_pbc <- find.interaction(rfsrc_pbc)
##D 
##D # plot the forest interaction table
##D gg_dta <- gg_interaction(interaction_pbc)
##D plot(gg_dta, panel=TRUE)
##D                    
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("interaction_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("kaplan")
### * kaplan

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: kaplan
### Title: nonparametric Kaplan-Meier estimates
### Aliases: kaplan

### ** Examples

## Not run: 
##D # These get run through the gg_survival examples.
##D data(pbc, package="randomForestSRC")
##D pbc$time <- pbc$days/364.25
##D 
##D # This is the same as gg_survival
##D gg_dta <- kaplan(interval="time", censor="status", 
##D                      data=pbc)
##D                      
##D plot(gg_dta, error="none")
##D plot(gg_dta)
##D 
##D # Stratified on treatment variable.
##D gg_dta <- gg_survival(interval="time", censor="status", 
##D                      data=pbc, by="treatment")
##D                      
##D plot(gg_dta, error="none")
##D plot(gg_dta)
## End(Not run)                                            



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("kaplan", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nelson")
### * nelson

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nelson
### Title: nonparametric Nelson-Aalen estimates
### Aliases: nelson

### ** Examples

## Not run: 
##D # These get run through the gg_survival examples.
##D data(pbc, package="randomForestSRC")
##D pbc$time <- pbc$days/364.25
##D 
##D # This is the same as gg_survival
##D gg_dta <- nelson(interval="time", censor="status", 
##D                      data=pbc)
##D                      
##D plot(gg_dta, error="none")
##D plot(gg_dta)
##D 
##D # Stratified on treatment variable.
##D gg_dta <- gg_survival(interval="time", censor="status", 
##D                      data=pbc, by="treatment")
##D                      
##D plot(gg_dta, error="none")
##D plot(gg_dta, error="lines")
##D plot(gg_dta)
##D 
##D gg_dta <- gg_survival(interval="time", censor="status", 
##D                      data=pbc, by="treatment",
##D                      type="nelson")
##D                      
##D plot(gg_dta, error="bars")
##D plot(gg_dta)
##D 
## End(Not run)                                            



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nelson", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("partial_coplot_data")
### * partial_coplot_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: partial_coplot_data
### Title: Cached 'plot.variable' objects for examples, diagnostics and
###   vignettes. Data sets storing 'rfsrc' objects corresponding to
###   training data according to the following naming convention: *
###   'partial_coplot_Boston' - randomForestS[R]C for the 'Boston' housing
###   data set ('MASS' package).
### Aliases: partial_coplot_data partial_coplot_Boston
###   partial_coplot_Boston2 partial_coplot_pbc partial_coplot_pbc2
###   partial_coplot_Boston, partial_coplot_Boston2, partial_coplot_pbc,
### Keywords: datasets

### ** Examples

## Not run: 
##D #---------------------------------------------------------------------
##D # MASS::Boston data - regression random forest 
##D #---------------------------------------------------------------------
##D data(Boston_rfsrc, package="ggRandomForests")
##D 
##D # Cut the codependent variable
##D rm_pts <- cut_distribution(rfsrc_Boston$xvar$rm, groups=6)
##D rm_grp <- cut(rfsrc_Boston$xvar$rm, breaks=rm_pts)
##D 
##D # plot.variable for lstat on subsets of rm (this will take some time.)
##D  partial_coplot_Boston <- gg_partial_coplot(rfsrc_Boston, xvar="lstat", 
##D                                             groups=rm_grp, 
##D                                             show.plots=FALSE)
##D                                             
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("partial_coplot_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("partial_data")
### * partial_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: partial_data
### Title: Cached 'plot.variable' objects for examples, diagnostics and
###   vignettes. Data sets storing 'plot.variable' objects corresponding to
###   training data according to the following naming convention: *
###   'partial_iris' - from a randomForestSR[C] for the 'iris' data set.  *
###   'partial_Boston' - from a randomForestS[R]C for the 'Boston' housing
###   data set ('MASS' package).  * 'partial_pbc' - from a
###   randomForest[S]RC for the 'pbc' data set ('randomForestSRC' package)
### Aliases: partial_data partial_iris partial_Boston partial_pbc
###   partial_iris, partial_Boston,
### Keywords: datasets

### ** Examples

## Not run: 
##D #---------------------------------------------------------------------
##D # iris data - classification random forest
##D #---------------------------------------------------------------------
##D # load the rfsrc object from the cached data
##D data(rfsrc_iris, package="ggRandomForests")
##D 
##D # The plot.variable call
##D  partial_iris <- plot.variable(rfsrc_iris,
##D                                partial=TRUE, show.plots=FALSE)
##D 
##D # plot the forest partial plots
##D gg_dta <- gg_partial(partial_iris)
##D plot(gg_dta, panel=TRUE)
##D 
##D #---------------------------------------------------------------------
##D # MASS::Boston data - regression random forest 
##D #---------------------------------------------------------------------
##D # load the rfsrc object from the cached data
##D data(rfsrc_Boston, package="ggRandomForests")
##D 
##D # The plot.variable call
##D partial_Boston <- plot.variable(rfsrc_Boston,
##D                                 partial=TRUE, show.plots = FALSE )
##D 
##D # plot the forest partial plots
##D gg_dta <- gg_partial(partial_Boston)
##D plot(gg_dta, panel=TRUE)
##D 
##D #---------------------------------------------------------------------
##D # randomForestSRC::pbc data - survival random forest
##D #---------------------------------------------------------------------
##D # load the rfsrc object from the cached data
##D data(rfsrc_pbc, package="ggRandomForests")
##D 
##D # The plot.variable call - 
##D # survival requires a time point specification.
##D # for the pbc data, we want 1, 3 and 5 year survival.
##D partial_pbc <- lapply(c(1,3,5), function(tm){
##D                       plot.variable(rfsrc_pbc, surv.type = "surv", 
##D                                     time = tm,
##D                                     xvar.names = xvar, 
##D                                     partial = TRUE,
##D                                     show.plots = FALSE)
##D                                     })
##D                                     
##D # plot the forest partial plots
##D gg_dta <- gg_partial(partial_pbc)
##D plot(gg_dta)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("partial_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("partial_surface_data")
### * partial_surface_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: partial_surface_data
### Title: Cached 'plot.variable' objects for examples, diagnostics and
###   vignettes. Data sets storing 'plot.variable' objects corresponding to
###   training data according to the following naming convention: *
###   'partial_Boston_surf' - from a randomForestS[R]C for the 'Boston'
###   housing data set ('MASS' package).  * 'partial_pbc_surf' - from a
###   randomForest[S]RC for the 'pbc' data set ('randomForestSRC' package)
###   * 'partial_pbc_time' - from a randomForest[S]RC for the 'pbc' data
###   set ('randomForestSRC' package)
### Aliases: partial_surface_data partial_Boston_surf partial_pbc_surf
###   partial_pbc_time partial_Boston_surf, partial_pbc_surf,
### Keywords: datasets

### ** Examples

## Not run: 
##D #---------------------------------------------------------------------
##D # MASS::Boston data - regression random forest 
##D #---------------------------------------------------------------------
##D # load the rfsrc object from the cached data
##D data(rfsrc_Boston, package="ggRandomForests")
##D 
##D # The plot.variable call
##D partial_Boston <- plot.variable(rfsrc_Boston,
##D                                 partial=TRUE, show.plots = FALSE )
##D 
##D # plot the forest partial plots
##D gg_dta <- gg_partial(partial_Boston)
##D plot(gg_dta, panel=TRUE)
##D 
##D #---------------------------------------------------------------------
##D # randomForestSRC::pbc data - survival random forest
##D #---------------------------------------------------------------------
##D # load the rfsrc object from the cached data
##D data(rfsrc_pbc, package="ggRandomForests")
##D 
##D # Restrict the time of interest to less than 5 years.
##D time_pts <- rfsrc_pbc$time.interest[which(rfsrc_pbc$time.interest<=5)]
##D 
##D # Find the 50 points in time, evenly space along the distribution of 
##D # event times for a series of partial dependence curves
##D time_cts <-quantile_pts(time_pts, groups = 50)
##D 
##D # Generate the gg_partial_coplot data object
##D system.time(partial_pbc_time <- lapply(time_cts, function(ct){
##D    plot.variable(rfsrc_pbc, xvar = "bili", time = ct,
##D                  npts = 50, show.plots = FALSE, 
##D                  partial = TRUE, surv.type="surv")
##D    }))
##D #     user   system  elapsed 
##D # 2561.313   81.446 2641.707 
##D 
##D # Find the quantile points to create 50 cut points
##D alb_partial_pts <-quantile_pts(rfsrc_pbc$xvar$albumin, groups = 50)
##D 
##D system.time(partial_pbc_surf <- lapply(alb_partial_pts, function(ct){
##D   rfsrc_pbc$xvar$albumin <- ct
##D   plot.variable(rfsrc_pbc, xvar = "bili", time = 1,
##D                 npts = 50, show.plots = FALSE, 
##D                 partial = TRUE, surv.type="surv")
##D   }))
##D # user   system  elapsed 
##D # 2547.482   91.978 2671.870 
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("partial_surface_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.gg_error")
### * plot.gg_error

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.gg_error
### Title: Plot a 'gg_error' object
### Aliases: plot.gg_error

### ** Examples

## Not run: 
##D ## Examples from RFSRC package... 
##D ## ------------------------------------------------------------
##D ## classification example
##D ## ------------------------------------------------------------
##D ## ------------- iris data
##D ## You can build a randomForest
##D # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
##D # ... or load a cached randomForestSRC object
##D data(rfsrc_iris, package="ggRandomForests")
##D 
##D # Get a data.frame containing error rates
##D gg_dta<- gg_error(rfsrc_iris)
##D 
##D # Plot the gg_error object
##D plot(gg_dta)
##D 
##D ## ------------------------------------------------------------
##D ## Regression example
##D ## ------------------------------------------------------------
##D ## ------------- airq data
##D # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
##D # ... or load a cached randomForestSRC object
##D data(rfsrc_airq, package="ggRandomForests")
##D 
##D # Get a data.frame containing error rates
##D gg_dta<- gg_error(rfsrc_airq)
##D 
##D # Plot the gg_error object
##D plot(gg_dta)
##D 
##D ## ------------- Boston data
##D data(rfsrc_Boston, package="ggRandomForests")
##D 
##D # Get a data.frame containing error rates
##D gg_dta<- gg_error(rfsrc_Boston)
##D 
##D # Plot the gg_error object
##D plot(gg_dta)
##D 
##D ## ------------- mtcars data
##D data(rfsrc_mtcars, package="ggRandomForests")
##D 
##D # Get a data.frame containing error rates
##D gg_dta<- gg_error(rfsrc_mtcars)
##D 
##D # Plot the gg_error object
##D plot(gg_dta)
##D 
##D ## ------------------------------------------------------------
##D ## Survival example
##D ## ------------------------------------------------------------
##D ## ------------- veteran data
##D ## randomized trial of two treatment regimens for lung cancer
##D # data(veteran, package = "randomForestSRC")
##D # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
##D 
##D # Load a cached randomForestSRC object
##D data(rfsrc_veteran, package="ggRandomForests")
##D 
##D gg_dta <- gg_error(rfsrc_veteran)
##D plot(gg_dta)
##D 
##D ## ------------- pbc data
##D # Load a cached randomForestSRC object
##D data(rfsrc_pbc, package="ggRandomForests")
##D 
##D gg_dta <- gg_error(rfsrc_pbc)
##D plot(gg_dta)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.gg_error", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.gg_interaction")
### * plot.gg_interaction

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.gg_interaction
### Title: plot.gg_interaction Plot a 'gg_interaction' object,
### Aliases: plot.gg_interaction

### ** Examples

## Not run: 
##D ## Examples from randomForestSRC package... 
##D ## ------------------------------------------------------------
##D ## find interactions, classification setting
##D ## ------------------------------------------------------------
##D ## -------- iris data
##D ## iris.obj <- rfsrc(Species ~., data = iris)
##D ## TODO: VIMP interactions not handled yet....
##D ## find.interaction(iris.obj, method = "vimp", nrep = 3)
##D ## interaction_iris <- find.interaction(iris.obj)
##D data(interaction_iris, package="ggRandomForests")
##D gg_dta <- gg_interaction(interaction_iris)
##D 
##D plot(gg_dta, xvar="Petal.Width")
##D plot(gg_dta, xvar="Petal.Length")
##D plot(gg_dta, panel=TRUE)
##D 
##D ## ------------------------------------------------------------
##D ## find interactions, regression setting
##D ## ------------------------------------------------------------
##D ## -------- air quality data
##D ## airq.obj <- rfsrc(Ozone ~ ., data = airquality)
##D ##
##D ## TODO: VIMP interactions not handled yet....
##D ## find.interaction(airq.obj, method = "vimp", nrep = 3)
##D ## interaction_airq <- find.interaction(airq.obj)
##D data(interaction_airq, package="ggRandomForests")
##D gg_dta <- gg_interaction(interaction_airq)
##D 
##D plot(gg_dta, xvar="Temp")
##D plot(gg_dta, xvar="Solar.R")
##D plot(gg_dta, panel=TRUE)
##D 
##D ## -------- Boston data
##D data(interaction_Boston, package="ggRandomForests")
##D gg_dta <- gg_interaction(interaction_Boston)
##D 
##D plot(gg_dta, panel=TRUE)
##D 
##D ## -------- mtcars data
##D data(interaction_mtcars, package="ggRandomForests")
##D gg_dta <- gg_interaction(interaction_mtcars)
##D 
##D plot(gg_dta, panel=TRUE)
##D 
##D ## ------------------------------------------------------------
##D ## find interactions, survival setting
##D ## ------------------------------------------------------------
##D ## -------- pbc data
##D ## data(pbc, package = "randomForestSRC") 
##D ## pbc.obj <- rfsrc(Surv(days,status) ~ ., pbc, nsplit = 10)
##D ## interaction_pbc <- find.interaction(pbc.obj, nvar = 8)
##D data(interaction_pbc, package="ggRandomForests")
##D gg_dta <- gg_interaction(interaction_pbc)
##D 
##D plot(gg_dta, xvar="bili")
##D plot(gg_dta, xvar="copper")
##D plot(gg_dta, panel=TRUE)
##D 
##D ## -------- veteran data
##D data(interaction_veteran, package="ggRandomForests")
##D gg_dta <- gg_interaction(interaction_veteran)
##D 
##D plot(gg_dta, panel=TRUE)
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.gg_interaction", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.gg_minimal_depth")
### * plot.gg_minimal_depth

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.gg_minimal_depth
### Title: Plot a 'gg_minimal_depth' object for random forest variable
###   ranking.
### Aliases: plot.gg_minimal_depth

### ** Examples

## Not run: 
##D ## Examples from RFSRC package... 
##D ## ------------------------------------------------------------
##D ## classification example
##D ## ------------------------------------------------------------
##D ## -------- iris data
##D ## You can build a randomForest
##D # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
##D # varsel_iris <- var.select(rfsrc_iris)
##D # ... or load a cached randomForestSRC object
##D data(varsel_iris, package="ggRandomForests")
##D 
##D # Get a data.frame containing minimaldepth measures
##D gg_dta<- gg_minimal_depth(varsel_iris)
##D 
##D # Plot the gg_minimal_depth object
##D plot(gg_dta)
##D 
##D ## ------------------------------------------------------------
##D ## Regression example
##D ## ------------------------------------------------------------
##D ## -------- air quality data
##D # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
##D # varsel_airq <- var.select(rfsrc_airq)
##D # ... or load a cached randomForestSRC object
##D data(varsel_airq, package="ggRandomForests")
##D 
##D # Get a data.frame containing error rates
##D gg_dta<- gg_minimal_depth(varsel_airq)
##D 
##D # Plot the gg_minimal_depth object
##D plot(gg_dta)
##D 
##D ## -------- Boston data
##D data(varsel_Boston, package="ggRandomForests")
##D 
##D # Get a data.frame containing error rates
##D plot(gg_minimal_depth(varsel_Boston))
##D 
##D ## -------- mtcars data
##D data(varsel_mtcars, package="ggRandomForests")
##D 
##D # Get a data.frame containing error rates
##D plot.gg_minimal_depth(varsel_mtcars)
##D 
##D ## ------------------------------------------------------------
##D ## Survival example
##D ## ------------------------------------------------------------
##D ## -------- veteran data
##D ## veteran data
##D ## randomized trial of two treatment regimens for lung cancer
##D # data(veteran, package = "randomForestSRC")
##D # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
##D # varsel_veteran <- var.select(rfsrc_veteran)
##D # Load a cached randomForestSRC object
##D data(varsel_veteran, package="ggRandomForests")
##D 
##D gg_dta <- gg_minimal_depth(varsel_veteran)
##D plot(gg_dta)
##D 
##D ## -------- pbc data
##D data(varsel_pbc, package="ggRandomForests")
##D 
##D gg_dta <- gg_minimal_depth(varsel_pbc)
##D plot(gg_dta)
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.gg_minimal_depth", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.gg_minimal_vimp")
### * plot.gg_minimal_vimp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.gg_minimal_vimp
### Title: Plot a 'gg_minimal_vimp' object for comparing the Minimal Depth
###   and VIMP variable rankings.
### Aliases: plot.gg_minimal_vimp

### ** Examples

## Not run: 
##D ## Examples from RFSRC package... 
##D ## ------------------------------------------------------------
##D ## classification example
##D ## ------------------------------------------------------------
##D ## -------- iris data
##D ## You can build a randomForest
##D # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
##D # varsel_iris <- var.select(rfsrc_iris)
##D # ... or load a cached randomForestSRC object
##D data(varsel_iris, package="ggRandomForests")
##D 
##D # Get a data.frame containing minimaldepth measures
##D gg_dta<- gg_minimal_vimp(varsel_iris)
##D 
##D # Plot the gg_minimal_depth object
##D plot(gg_dta)
##D 
##D ## ------------------------------------------------------------
##D ## Regression example
##D ## ------------------------------------------------------------
##D ## -------- air quality data
##D rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
##D varsel_airq <- var.select(rfsrc_airq)
##D 
##D # Get a data.frame containing error rates
##D gg_dta<- gg_minimal_vimp(varsel_airq)
##D 
##D # Plot the gg_minimal_vimp object
##D plot(gg_dta)
##D 
##D ## -------- Boston data
##D data(varsel_Boston, package="ggRandomForests")
##D 
##D # Get a data.frame containing error rates
##D gg_dta<- gg_minimal_vimp(varsel_Boston)
##D 
##D # Plot the gg_minimal_vimp object
##D plot(gg_dta)
##D 
##D ## -------- mtcars data
##D data(varsel_mtcars, package="ggRandomForests")
##D 
##D # Get a data.frame containing error rates
##D gg_dta<- gg_minimal_vimp(varsel_mtcars)
##D 
##D # Plot the gg_minimal_vimp object
##D plot(gg_dta)
##D 
##D ## ------------------------------------------------------------
##D ## Survival example
##D ## ------------------------------------------------------------
##D ## -------- veteran data
##D ## randomized trial of two treatment regimens for lung cancer
##D # data(veteran, package = "randomForestSRC")
##D # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
##D # varsel_veteran <- var.select(rfsrc_veteran)
##D # Load a cached randomForestSRC object
##D data(varsel_veteran, package="ggRandomForests")
##D 
##D gg_dta <- gg_minimal_vimp(varsel_veteran)
##D plot(gg_dta)
##D   
##D ## -------- pbc data
##D data(varsel_pbc, package="ggRandomForests")
##D 
##D gg_dta <- gg_minimal_vimp(varsel_pbc)
##D plot(gg_dta)
## End(Not run) 




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.gg_minimal_vimp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.gg_partial")
### * plot.gg_partial

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.gg_partial
### Title: Partial variable dependence plot, operates on a 'gg_partial'
###   object.
### Aliases: plot.gg_partial

### ** Examples

## Not run: 
##D ## ------------------------------------------------------------
##D ## classification
##D ## ------------------------------------------------------------
##D ## -------- iris data
##D 
##D ## iris "Petal.Width" partial dependence plot
##D ##
##D # rfsrc_iris <- rfsrc(Species ~., data = iris)
##D # partial_iris <- plot.variable(rfsrc_iris, xvar.names = "Petal.Width",
##D #                            partial=TRUE)
##D data(partial_iris, package="ggRandomForests")
##D 
##D gg_dta <- gg_partial(partial_iris)
##D plot(gg_dta)
##D 
##D ## ------------------------------------------------------------
##D ## regression
##D ## ------------------------------------------------------------
##D ## -------- air quality data
##D ## airquality "Wind" partial dependence plot
##D ##
##D # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality)
##D # partial_airq <- plot.variable(rfsrc_airq, xvar.names = "Wind",
##D #                            partial=TRUE, show.plot=FALSE)
##D data(partial_airq, package="ggRandomForests")
##D 
##D gg_dta <- gg_partial(partial_airq)
##D plot(gg_dta)
##D 
##D gg_dta.m <- gg_dta[["Month"]]
##D plot(gg_dta.m, notch=TRUE)
##D 
##D gg_dta[["Month"]] <- NULL
##D plot(gg_dta, panel=TRUE)
##D 
##D ## -------- Boston data
##D data(partial_Boston, package="ggRandomForests")
##D 
##D gg_dta <- gg_partial(partial_Boston)
##D plot(gg_dta)
##D plot(gg_dta, panel=TRUE)
##D 
##D ## -------- mtcars data
##D data(partial_mtcars, package="ggRandomForests")
##D 
##D gg_dta <- gg_partial(partial_mtcars)
##D 
##D plot(gg_dta)
##D 
##D gg_dta.cat <- gg_dta
##D gg_dta.cat[["disp"]] <- gg_dta.cat[["wt"]] <- gg_dta.cat[["hp"]] <- NULL
##D gg_dta.cat[["drat"]] <- gg_dta.cat[["carb"]] <- gg_dta.cat[["qsec"]] <- NULL
##D  
##D plot(gg_dta.cat, panel=TRUE)
##D 
##D gg_dta[["cyl"]] <- gg_dta[["vs"]] <- gg_dta[["am"]] <- NULL
##D gg_dta[["gear"]] <- NULL
##D plot(gg_dta, panel=TRUE)
##D 
##D ## ------------------------------------------------------------
##D ## survival examples
##D ## ------------------------------------------------------------
##D ## -------- veteran data
##D ## survival "age" partial variable dependence plot
##D ##
##D # data(veteran, package = "randomForestSRC")
##D # rfsrc_veteran <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
##D #
##D ## 30 day partial plot for age
##D # partial_veteran <- plot.variable(rfsrc_veteran, surv.type = "surv", 
##D #                               partial = TRUE, time=30, 
##D #                               xvar.names = "age", 
##D #                               show.plots=FALSE)
##D data(partial_veteran, package="ggRandomForests")
##D 
##D gg_dta <- gg_partial(partial_veteran[[1]])
##D plot(gg_dta)
##D 
##D gg_dta.cat <- gg_dta
##D gg_dta[["celltype"]] <- gg_dta[["trt"]] <- gg_dta[["prior"]] <- NULL
##D plot(gg_dta, panel=TRUE)
##D 
##D gg_dta.cat[["karno"]] <- gg_dta.cat[["diagtime"]] <- gg_dta.cat[["age"]] <- NULL 
##D plot(gg_dta.cat, panel=TRUE, notch=TRUE)
##D 
##D gg_dta <- lapply(partial_veteran, gg_partial)
##D length(gg_dta)
##D gg_dta <- combine.gg_partial(gg_dta[[1]], gg_dta[[2]] )
##D 
##D plot(gg_dta[["karno"]])
##D plot(gg_dta[["celltype"]])
##D 
##D gg_dta.cat <- gg_dta
##D gg_dta[["celltype"]] <- gg_dta[["trt"]] <- gg_dta[["prior"]] <- NULL
##D plot(gg_dta, panel=TRUE)
##D 
##D gg_dta.cat[["karno"]] <- gg_dta.cat[["diagtime"]] <- gg_dta.cat[["age"]] <- NULL 
##D plot(gg_dta.cat, panel=TRUE, notch=TRUE)
##D 
##D ## -------- pbc data
## End(Not run)
 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.gg_partial", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.gg_partial_list")
### * plot.gg_partial_list

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.gg_partial_list
### Title: Partial variable dependence plot, operates on a
###   'gg_partial_list' object.
### Aliases: plot.gg_partial_list

### ** Examples

## Not run: 
##D ## ------------------------------------------------------------
##D ## classification
##D ## ------------------------------------------------------------
##D ## -------- iris data
##D 
##D ## iris "Petal.Width" partial dependence plot
##D ##
##D # rfsrc_iris <- rfsrc(Species ~., data = iris)
##D # partial_iris <- plot.variable(rfsrc_iris, xvar.names = "Petal.Width",
##D #                            partial=TRUE)
##D data(partial_iris, package="ggRandomForests")
##D 
##D gg_dta <- gg_partial(partial_iris)
##D plot(gg_dta)
##D 
##D ## ------------------------------------------------------------
##D ## regression
##D ## ------------------------------------------------------------
##D ## -------- air quality data
##D ## airquality "Wind" partial dependence plot
##D ##
##D # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality)
##D # partial_airq <- plot.variable(rfsrc_airq, xvar.names = "Wind",
##D #                            partial=TRUE, show.plot=FALSE)
##D data(partial_airq, package="ggRandomForests")
##D 
##D gg_dta <- gg_partial(partial_airq)
##D plot(gg_dta)
##D 
##D gg_dta.m <- gg_dta[["Month"]]
##D plot(gg_dta.m, notch=TRUE)
##D 
##D gg_dta[["Month"]] <- NULL
##D plot(gg_dta, panel=TRUE)
##D 
##D ## -------- Boston data
##D data(partial_Boston, package="ggRandomForests")
##D 
##D gg_dta <- gg_partial(partial_Boston)
##D plot(gg_dta)
##D plot(gg_dta, panel=TRUE)
##D 
##D ## -------- mtcars data
##D data(partial_mtcars, package="ggRandomForests")
##D 
##D gg_dta <- gg_partial(partial_mtcars)
##D 
##D plot(gg_dta)
##D 
##D gg_dta.cat <- gg_dta
##D gg_dta.cat[["disp"]] <- gg_dta.cat[["wt"]] <- gg_dta.cat[["hp"]] <- NULL
##D gg_dta.cat[["drat"]] <- gg_dta.cat[["carb"]] <- gg_dta.cat[["qsec"]] <- NULL
##D  
##D plot(gg_dta.cat, panel=TRUE)
##D 
##D gg_dta[["cyl"]] <- gg_dta[["vs"]] <- gg_dta[["am"]] <- NULL
##D gg_dta[["gear"]] <- NULL
##D plot(gg_dta, panel=TRUE)
##D 
##D ## ------------------------------------------------------------
##D ## survival examples
##D ## ------------------------------------------------------------
##D ## -------- veteran data
##D ## survival "age" partial variable dependence plot
##D ##
##D # data(veteran, package = "randomForestSRC")
##D # rfsrc_veteran <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
##D #
##D ## 30 day partial plot for age
##D # partial_veteran <- plot.variable(rfsrc_veteran, surv.type = "surv", 
##D #                               partial = TRUE, time=30, 
##D #                               xvar.names = "age", 
##D #                               show.plots=FALSE)
##D data(partial_veteran, package="ggRandomForests")
##D 
##D gg_dta <- gg_partial(partial_veteran[[1]])
##D plot(gg_dta)
##D 
##D gg_dta.cat <- gg_dta
##D gg_dta[["celltype"]] <- gg_dta[["trt"]] <- gg_dta[["prior"]] <- NULL
##D plot(gg_dta, panel=TRUE)
##D 
##D gg_dta.cat[["karno"]] <- gg_dta.cat[["diagtime"]] <- gg_dta.cat[["age"]] <- NULL 
##D plot(gg_dta.cat, panel=TRUE, notch=TRUE)
##D 
##D gg_dta <- lapply(partial_veteran, gg_partial)
##D length(gg_dta)
##D gg_dta <- combine.gg_partial(gg_dta[[1]], gg_dta[[2]] )
##D 
##D plot(gg_dta[["karno"]])
##D plot(gg_dta[["celltype"]])
##D 
##D gg_dta.cat <- gg_dta
##D gg_dta[["celltype"]] <- gg_dta[["trt"]] <- gg_dta[["prior"]] <- NULL
##D plot(gg_dta, panel=TRUE)
##D 
##D gg_dta.cat[["karno"]] <- gg_dta.cat[["diagtime"]] <- gg_dta.cat[["age"]] <- NULL 
##D plot(gg_dta.cat, panel=TRUE, notch=TRUE)
##D 
##D ## -------- pbc data
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.gg_partial_list", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.gg_rfsrc")
### * plot.gg_rfsrc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.gg_rfsrc
### Title: Predicted response plot from a 'gg_rfsrc' object.
### Aliases: plot.gg_rfsrc

### ** Examples

## Not run: 
##D ## ------------------------------------------------------------
##D ## classification example
##D ## ------------------------------------------------------------
##D ## -------- iris data
##D # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
##D data(rfsrc_iris, package="ggRandomForests")
##D gg_dta<- gg_rfsrc(rfsrc_iris)
##D 
##D plot(gg_dta)
##D 
##D ## ------------------------------------------------------------
##D ## Regression example
##D ## ------------------------------------------------------------
##D ## -------- air quality data
##D # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
##D data(rfsrc_airq, package="ggRandomForests")
##D gg_dta<- gg_rfsrc(rfsrc_airq)
##D 
##D plot(gg_dta)
##D 
##D ## -------- Boston data
##D data(rfsrc_Boston, package="ggRandomForests")
##D plot(rfsrc_Boston) 
##D 
##D ## -------- mtcars data
##D data(rfsrc_mtcars, package="ggRandomForests")
##D gg_dta<- gg_rfsrc(rfsrc_mtcars)
##D 
##D plot(gg_dta)
##D 
##D ## ------------------------------------------------------------
##D ## Survival example
##D ## ------------------------------------------------------------
##D ## -------- veteran data
##D ## randomized trial of two treatment regimens for lung cancer
##D # data(veteran, package = "randomForestSRC")
##D # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
##D data(rfsrc_veteran, package = "ggRandomForests")
##D gg_dta <- gg_rfsrc(rfsrc_veteran)
##D plot(gg_dta)
##D 
##D gg_dta <- gg_rfsrc(rfsrc_veteran, conf.int=.95)
##D plot(gg_dta)
##D 
##D gg_dta <- gg_rfsrc(rfsrc_veteran, by="trt")
##D plot(gg_dta)
##D 
##D ## -------- pbc data
##D data(rfsrc_pbc, package = "ggRandomForests")
##D gg_dta <- gg_rfsrc(rfsrc_pbc)
##D plot(gg_dta)
##D 
##D gg_dta <- gg_rfsrc(rfsrc_pbc, conf.int=.95)
##D plot(gg_dta)
##D 
##D gg_dta <- gg_rfsrc(rfsrc_pbc, by="treatment")
##D plot(gg_dta)
##D 
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.gg_rfsrc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.gg_roc")
### * plot.gg_roc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.gg_roc
### Title: ROC plot generic function for a 'gg_roc' object.
### Aliases: plot.gg_roc

### ** Examples

## Not run: 
##D ## ------------------------------------------------------------
##D ## classification example
##D ## ------------------------------------------------------------
##D ## -------- iris data
##D #rfsrc_iris <- rfsrc(Species ~ ., data = iris)
##D data(rfsrc_iris, package="ggRandomForests")
##D 
##D # ROC for setosa
##D gg_dta <- gg_roc(rfsrc_iris, which.outcome=1)
##D plot.gg_roc(gg_dta)
##D 
##D # ROC for versicolor
##D gg_dta <- gg_roc(rfsrc_iris, which.outcome=2)
##D plot.gg_roc(gg_dta)
##D 
##D # ROC for virginica
##D gg_dta <- gg_roc(rfsrc_iris, which.outcome=3)
##D plot.gg_roc(gg_dta)
##D 
##D # Alternatively, you can plot all three outcomes in one go
##D # by calling the plot function on the forest object. 
##D plot.gg_roc(rfsrc_iris)
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.gg_roc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.gg_survival")
### * plot.gg_survival

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.gg_survival
### Title: Plot a 'gg_survival' object.
### Aliases: plot.gg_survival

### ** Examples

## Not run: 
##D ## -------- pbc data
##D data(pbc, package="randomForestSRC")
##D pbc$time <- pbc$days/364.25
##D 
##D # This is the same as kaplan
##D gg_dta <- gg_survival(interval="time", censor="status", 
##D                      data=pbc)
##D                      
##D plot(gg_dta, error="none")
##D plot(gg_dta)
##D 
##D # Stratified on treatment variable.
##D gg_dta <- gg_survival(interval="time", censor="status", 
##D                      data=pbc, by="treatment")
##D                      
##D plot(gg_dta, error="none")
##D plot(gg_dta)
##D 
##D # ...with smaller confidence limits.
##D gg_dta <- gg_survival(interval="time", censor="status", 
##D                      data=pbc, by="treatment", conf.int=.68)
##D                      
##D plot(gg_dta, error="lines")
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.gg_survival", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.gg_variable")
### * plot.gg_variable

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.gg_variable
### Title: Plot a 'gg_variable' object,
### Aliases: plot.gg_variable

### ** Examples

## Not run: 
##D ## ------------------------------------------------------------
##D ## classification
##D ## ------------------------------------------------------------
##D ## -------- iris data
##D ## iris
##D #rfsrc_iris <- rfsrc(Species ~., data = iris)
##D data(rfsrc_iris, package="ggRandomForests")
##D 
##D gg_dta <- gg_variable(rfsrc_iris)
##D plot(gg_dta, xvar="Sepal.Width")
##D plot(gg_dta, xvar="Sepal.Length")
##D 
##D ## !! TODO !! this needs to be corrected
##D plot(gg_dta, xvar=rfsrc_iris$xvar.names, 
##D      panel=TRUE, se=FALSE)
##D 
##D ## ------------------------------------------------------------
##D ## regression
##D ## ------------------------------------------------------------
##D ## -------- air quality data
##D #rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality)
##D data(rfsrc_airq, package="ggRandomForests")
##D gg_dta <- gg_variable(rfsrc_airq)
##D 
##D # an ordinal variable 
##D gg_dta[,"Month"] <- factor(gg_dta[,"Month"])
##D 
##D plot(gg_dta, xvar="Wind")
##D plot(gg_dta, xvar="Temp")
##D plot(gg_dta, xvar="Solar.R")
##D 
##D plot(gg_dta, xvar=c("Solar.R", "Wind", "Temp", "Day"), panel=TRUE)
##D 
##D plot(gg_dta, xvar="Month", notch=TRUE)
##D 
##D ## -------- motor trend cars data
##D #rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
##D data(rfsrc_mtcars, package="ggRandomForests")
##D gg_dta <- gg_variable(rfsrc_mtcars)
##D 
##D # mtcars$cyl is an ordinal variable 
##D gg_dta$cyl <- factor(gg_dta$cyl)
##D gg_dta$am <- factor(gg_dta$am)
##D gg_dta$vs <- factor(gg_dta$vs)
##D gg_dta$gear <- factor(gg_dta$gear)
##D gg_dta$carb <- factor(gg_dta$carb)
##D 
##D plot(gg_dta, xvar="cyl")
##D 
##D # Others are continuous
##D plot(gg_dta, xvar="disp")
##D plot(gg_dta, xvar="hp")
##D plot(gg_dta, xvar="wt")
##D 
##D # panel
##D plot(gg_dta,xvar=c("disp","hp", "drat", "wt", "qsec"),  panel=TRUE)
##D plot(gg_dta, xvar=c("cyl", "vs", "am", "gear", "carb") ,panel=TRUE)
##D 
##D ## -------- Boston data
##D 
##D ## ------------------------------------------------------------
##D ## survival examples
##D ## ------------------------------------------------------------
##D ## -------- veteran data
##D ## survival
##D data(veteran, package = "randomForestSRC")
##D rfsrc_veteran <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
##D 
##D # get the 1 year survival time.
##D gg_dta <- gg_variable(rfsrc_veteran, time=90)
##D 
##D # Generate variable dependance plots for age and diagtime
##D plot(gg_dta, xvar = "age")
##D plot(gg_dta, xvar = "diagtime")
##D 
##D # Generate coplots
##D plot(gg_dta, xvar = c("age", "diagtime"), panel=TRUE)
##D 
##D # If we want to compare survival at different time points, say 30, 90 day 
##D # and 1 year
##D gg_dta <- gg_variable(rfsrc_veteran, time=c(30, 90, 365))
##D 
##D # Generate variable dependance plots for age and diagtime
##D plot(gg_dta, xvar = "age")
##D plot(gg_dta, xvar = "diagtime") 
##D 
##D # Generate coplots
##D plot(gg_dta, xvar =  c("age", "diagtime"), panel=TRUE)
##D 
##D ## -------- pbc data
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.gg_variable", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.gg_vimp")
### * plot.gg_vimp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.gg_vimp
### Title: Plot a 'gg_vimp' object, extracted variable importance of a
###   'rfsrc' object
### Aliases: plot.gg_vimp

### ** Examples

## Not run: 
##D ## ------------------------------------------------------------
##D ## classification example
##D ## ------------------------------------------------------------
##D ## -------- iris data
##D # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
##D data(rfsrc_iris, package="ggRandomForests")
##D gg_dta <- gg_vimp(rfsrc_iris)
##D plot(gg_dta)
##D  
##D ## ------------------------------------------------------------
##D ## regression example
##D ## ------------------------------------------------------------
##D ## -------- air quality data 
##D # rfsrc_airq <- rfsrc(Ozone ~ ., airquality)
##D data(rfsrc_airq, package="ggRandomForests")
##D gg_dta <- gg_vimp(rfsrc_airq)
##D plot(gg_dta)
##D 
##D ## -------- Boston data
##D data(rfsrc_Boston, package="ggRandomForests")
##D gg_dta <- gg_vimp(rfsrc_Boston)
##D plot(gg_dta)
##D 
##D ## -------- mtcars data
##D data(rfsrc_mtcars, package="ggRandomForests")
##D gg_dta <- gg_vimp(rfsrc_mtcars)
##D plot(gg_dta)
##D 
##D ## ------------------------------------------------------------
##D ## survival example
##D ## ------------------------------------------------------------
##D ## -------- veteran data
##D data(rfsrc_veteran, package="ggRandomForests")
##D gg_dta <- gg_vimp(rfsrc_veteran)
##D plot(gg_dta)
##D 
##D ## -------- pbc data
##D data(rfsrc_pbc, package="ggRandomForests")
##D gg_dta <- gg_vimp(rfsrc_pbc)
##D plot(gg_dta)
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.gg_vimp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print.gg_minimal_depth")
### * print.gg_minimal_depth

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print.gg_minimal_depth
### Title: Print a 'gg_minimal_depth' object.
### Aliases: print.gg_minimal_depth

### ** Examples

## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## You can build a randomForest
# rfsrc_iris <- rfsrc(Species ~ ., data = iris)
# varsel_iris <- var.select(rfsrc_iris)
# ... or load a cached randomForestSRC object
data(varsel_iris, package="ggRandomForests")

# Get a data.frame containing minimaldepth measures
gg_dta <- gg_minimal_depth(varsel_iris)
print(gg_dta)

## ------------------------------------------------------------
## regression example
## ------------------------------------------------------------
## Not run: 
##D # ... or load a cached randomForestSRC object
##D data(varsel_airq, package="ggRandomForests")
##D 
##D # Get a data.frame containing minimaldepth measures
##D gg_dta<- gg_minimal_depth(varsel_airq)
##D print(gg_dta)
##D 
##D # To nicely print a rfsrc::var.select output... 
##D print(varsel_airq)
## End(Not run)

# ... or load a cached randomForestSRC object
data(varsel_Boston, package="ggRandomForests")

# Get a data.frame containing minimaldepth measures
gg_dta<- gg_minimal_depth(varsel_Boston)
print(gg_dta)

# To nicely print a rfsrc::var.select output... 
print(varsel_Boston)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print.gg_minimal_depth", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("quantile_pts")
### * quantile_pts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: quantile_pts
### Title: Find points evenly distributed along the vectors values.
### Aliases: quantile_pts

### ** Examples

data(rfsrc_Boston)

# To create 6 intervals, we want 7 points. 
# quantile_pts will find balanced intervals 
rm_pts <- quantile_pts(rfsrc_Boston$xvar$rm, groups=6, intervals=TRUE)

# Use cut to create the intervals
rm_grp <- cut(rfsrc_Boston$xvar$rm, breaks=rm_pts)

summary(rm_grp)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("quantile_pts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rfsrc_data")
### * rfsrc_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rfsrc_data
### Title: Cached 'rfsrc' objects for examples, diagnostics and vignettes.
### Aliases: rfsrc_data rfsrc_iris rfsrc_Boston rfsrc_pbc rfsrc_pbc_test
###   rfsrc_iris, rfsrc_Boston, rfsrc_pbc,
### Keywords: datasets

### ** Examples

## Not run: 
##D #---------------------------------------------------------------------
##D # iris data - classification random forest
##D #---------------------------------------------------------------------
##D # rfsrc grow call
##D rfsrc_iris <- rfsrc(Species ~., data = iris)
##D 
##D # plot the forest generalization error convergence
##D gg_dta <- gg_error(rfsrc_iris)
##D plot(gg_dta)
##D 
##D # Plot the forest predictions
##D gg_dta <- gg_rfsrc(rfsrc_iris)
##D plot(gg_dta)
##D 
##D #---------------------------------------------------------------------
##D # MASS::Boston data - regression random forest
##D #---------------------------------------------------------------------
##D # Load the data...
##D data(Boston, package="MASS")
##D Boston$chas <- as.logical(Boston$chas)
##D 
##D # rfsrc grow call
##D rfsrc_Boston <- rfsrc(medv~., data=Boston)
##D 
##D # plot the forest generalization error convergence
##D gg_dta <- gg_error(rfsrc_Boston)
##D plot(gg_dta)
##D 
##D # Plot the forest predictions
##D gg_dta <- gg_rfsrc(rfsrc_Boston)
##D plot(gg_dta)
##D 
##D #---------------------------------------------------------------------
##D # randomForestSRC::pbc data - survival random forest
##D #---------------------------------------------------------------------
##D # Load the data...
##D # For simplicity here. We do a bit of data tidying
##D # before running the stored random forest.
##D data(pbc, package="randomForestSRC")
##D 
##D # Remove non-randomized cases
##D dta.train <- pbc[-which(is.na(pbc$treatment)),]
##D 
##D # rfsrc grow call
##D rfsrc_pbc <- rfsrc(Surv(years, status) ~ ., dta.train, nsplit = 10,
##D                    na.action="na.impute")
##D 
##D # plot the forest generalization error convergence
##D gg_dta <- gg_error(rfsrc_pbc)
##D plot(gg_dta)
##D 
##D # Plot the forest predictions
##D gg_dta <- gg_rfsrc(rfsrc_pbc)
##D plot(gg_dta)
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rfsrc_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("shift")
### * shift

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: shift
### Title: lead function to shift by one (or more).
### Aliases: shift

### ** Examples

d<-data.frame(x=1:15) 
#generate lead variable
d$df_lead2<-ggRandomForests:::shift(d$x,2)
#generate lag variable
d$df_lag2<-ggRandomForests:::shift(d$x,-2)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("shift", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("surface_matrix")
### * surface_matrix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: surface_matrix
### Title: Construct a set of (x, y, z) matrices for surface plotting a
###   'gg_partial_coplot' object
### Aliases: surface_matrix surface_matrix.gg_partial_coplot

### ** Examples

## Not run: 
##D ## From vignette(randomForestRegression, package="ggRandomForests")
##D ##
##D data(rfsrc_Boston)
##D rm_pts <- quantile_pts(rfsrc_Boston$xvar$rm, groups=49, intervals=TRUE)
##D 
##D # Load the stored partial coplot data.
##D data(partial_Boston_surf)
##D 
##D # Instead of groups, we want the raw rm point values,
##D # To make the dimensions match, we need to repeat the values
##D # for each of the 50 points in the lstat direction
##D rm.tmp <- do.call(c,lapply(rm_pts, 
##D                            function(grp){rep(grp, length(partial_Boston_surf))}))
##D 
##D # Convert the list of plot.variable output to 
##D partial_surf <- do.call(rbind,lapply(partial_Boston_surf, gg_partial))
##D 
##D # attach the data to the gg_partial_coplot
##D partial_surf$rm <- rm.tmp
##D 
##D # Transform the gg_partial_coplot object into a list of three named matrices
##D # for surface plotting with plot3D::surf3D
##D srf <- surface_matrix(partial_surf, c("lstat", "rm", "yhat"))
## End(Not run)

## Not run: 
##D # surf3D is in the plot3D package.
##D library(plot3D)
##D # Generate the figure.
##D surf3D(x=srf$x, y=srf$y, z=srf$z, col=topo.colors(10),
##D        colkey=FALSE, border = "black", bty="b2", 
##D        shade = 0.5, expand = 0.5, 
##D        lighting = TRUE, lphi = -50,
##D        xlab="Lower Status", ylab="Average Rooms", zlab="Median Value"
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("surface_matrix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("varsel_data")
### * varsel_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: varsel_data
### Title: Cached 'var.select' objects for examples, diagnostics and
###   vignettes. Data sets storing 'var.select' objects corresponding to
###   training data according to the following naming convention: *
###   'varsel_iris' - from a randomForestSR[C] for the 'iris' data set.  *
###   'varsel_Boston' - from a randomForestS[R]C for the 'Boston' housing
###   data set ('MASS' package).  * 'varsel_pbc' - from a randomForest[S]RC
###   for the 'pbc' data set ('randomForestSRC' package)
### Aliases: varsel_data varsel_iris varsel_Boston varsel_pbc varsel_iris,
###   varsel_Boston,
### Keywords: datasets

### ** Examples

## Not run: 
##D #---------------------------------------------------------------------
##D # iris data - classification random forest
##D #---------------------------------------------------------------------
##D # load the rfsrc object from the cached data
##D data(rfsrc_iris, package="ggRandomForests")
##D 
##D # The var.select call
##D  varsel_iris <- var.select(rfsrc_iris)
##D 
##D # plot the forestminimal depth ranking
##D gg_dta <- gg_minimal_depth(varsel_iris)
##D plot(gg_dta)
##D 
##D 
##D #---------------------------------------------------------------------
##D # MASS::Boston data - regression random forest 
##D #---------------------------------------------------------------------
##D # load the rfsrc object from the cached data
##D data(rfsrc_Boston, package="ggRandomForests")
##D 
##D # The var.select call
##D varsel_Boston <- var.select(rfsrc_Boston)
##D 
##D # plot the forestminimal depth ranking
##D gg_dta <- gg_minimal_depth(varsel_Boston)
##D plot(gg_dta)
##D 
##D #---------------------------------------------------------------------
##D # randomForestSRC::pbc data - survival random forest
##D #---------------------------------------------------------------------
##D # load the rfsrc object from the cached data
##D data(rfsrc_pbc, package="ggRandomForests")
##D 
##D # The var.select call 
##D varsel_pbc <- var.select(rfsrc_pbc)
##D                                     
##D # plot the forestminimal depth ranking
##D gg_dta <- gg_minimal_depth(varsel_pbc)
##D plot(gg_dta)
##D                    
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("varsel_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
