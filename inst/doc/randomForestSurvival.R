## ----setup, include = FALSE, cache = FALSE, echo=FALSE----------------------------------
## Not displayed ##
library("knitr")
knitr::render_sweave() 
# set global chunk options for knitr. These can be changed in the header for each individual R code chunk
opts_chunk$set(fig.path = 'figure/rfs-', 
               fig.align = 'center', 
               fig.pos = "!htpb", 
               fig.show = 'hold', 
               fig.height = 3, 
               fig.width = 4, 
               size = 'footnotesize', 
               prompt = TRUE, 
               highlight = FALSE, 
               comment = NA, 
               echo = FALSE, results = FALSE, message = FALSE, warning = FALSE, 
               error = FALSE, dev = 'pdf', prompt = TRUE)

# Setup the R environment
options(object.size = Inf, expressions = 100000, memory = Inf, 
        replace.assign = TRUE, width = 90, prompt = "R> ")

#################
# Load_packages #
#################
library("ggplot2") # Graphics engine for generating all types of plots
library("RColorBrewer") # nice color palettes
library("plot3D") # for 3d surfaces. 

library("xtable") # Nice latex tables.

# Data manipulation
library("dplyr") # Better data manipulations
library("reshape2") # melt

library("parallel") # mclapply

# Analysis packages.
library("randomForestSRC") # rfsrc 
library("ggRandomForests") # Graphics for rfsrc


options(mc.cores = 1, rf.cores = 0)

#########################################################################
# Default computation settings
#########################################################################
theme_set(theme_bw())

## Set the event/censor marks. Want open circle for censored, and 
## x for events 
event.marks = c(1, 4)
event.labels = c(FALSE, TRUE)
strCol <- brewer.pal(3, "Set1")
strCol <- c(strCol[2], strCol[1])

## ----vignette, eval=FALSE, echo=TRUE----------------------------------------------------
#  vignette("randomForestSurvival", package = "ggRandomForests")

## ----datastep, echo=TRUE----------------------------------------------------------------
data(pbc, package = "randomForestSRC")

## ----data-clean-------------------------------------------------------------------------
## Not displayed ##

## Set modes correctly. For binary variables: transform to logical
## Check for range of 0, 1
## There is probably a better way to do this.
for(ind in 1:dim(pbc)[2]){
  if(!is.factor(pbc[, ind])){
    if(length(unique(pbc[which(!is.na(pbc[, ind])), ind]))<= 2) {
      if(sum(range(pbc[, ind], na.rm = TRUE) ==  c(0, 1)) ==  2){
        pbc[, ind] <- as.logical(pbc[, ind])
        }
  }
 }else{
  if(length(unique(pbc[which(!is.na(pbc[, ind])), ind]))<= 2) {
   if(sum(sort(unique(pbc[, ind])) ==  c(0, 1)) ==  2){
    pbc[, ind] <- as.logical(pbc[, ind])
   }
   if(sum(sort(unique(pbc[, ind])) ==  c(FALSE, TRUE)) ==  2){
    pbc[, ind] <- as.logical(pbc[, ind])
   }
  }
 }
 if(!is.logical(pbc[, ind]) & 
    length(unique(pbc[which(!is.na(pbc[, ind])), ind]))<= 5) {
  pbc[, ind] <- factor(pbc[, ind])
 }
}
# Convert age to years
pbc$age <- pbc$age/364.24
pbc$years <- pbc$days/364.24
pbc <- pbc %>% select(-days)
pbc$treatment <- as.numeric(pbc$treatment)
pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
pbc$treatment <- factor(pbc$treatment)

cls <- sapply(pbc, class) 

labels <- c("event indicator (F = censor, T = death)", 
            "Treament (DPCA, Placebo)", 
            "age in years", 
            "Female", 
            "Asictes", 
            "Hepatomegaly", 
            "Spiders", 
            "Edema", 
            "serum bilirubin (mg/dl)", 
            "serum cholesterol (mg/dl)", 
            "albumin (gm/dl)", 
            "urine copper (ug/day)", 
            "alkaline phosphatase (U/liter)", 
            "SGOT (U/ml)", 
            "triglicerides (mg/dl)", 
            "platelets per cubic ml/1000", 
            "prothrombin time (sec)", 
            "histologic stage", 
            "survival time (years)")

dta.labs <- data.frame(cbind(names = colnames(pbc), label = labels, type = cls))

st.labs <- as.character(dta.labs$label)
names(st.labs) <- rownames(dta.labs)

## ----dta-table, results="asis"----------------------------------------------------------
## Not displayed ##
# create a data dictionary table

rws <- seq(1, (nrow(dta.labs)), by = 2)
col <- rep("\\rowcolor[gray]{0.95}", length(rws))

print(xtable(dta.labs %>% select(-names), 
             caption = "PBC Data field descriptions", 
             label = "T:dataLabs", 
             digits = 3), 
      size = 'footnotesize', # fontsize
      booktabs = TRUE, 
      add.to.row = list(pos = as.list(rws), command = col), 
      command= c('\\toprule' , 
                   '\\midrule' , 
                   '\\bottomrule ')
      )

## ----categoricalEDA, fig.cap="Categorical variable EDA plots. Bars indicate counts within 1 year of followup for each categorical variable. Bars are colored according to the class membership within each variable. Missing values are colored dark grey.", fig.width=7----
## Not displayed ##

# Use reshape2::melt to transform the data into long format.
cnt <- c(which(cls == "numeric" ), which(cls == "integer"))
fct <- setdiff(1:ncol(pbc), cnt)
fct <- c(fct, which(colnames(pbc) == "years"))
dta <- melt(pbc[,fct], id.vars = "years")

# plot panels for each covariate colored by the logical chas variable.
ggplot(dta, aes(x = years, fill = value))+
  geom_histogram(color = "black", binwidth = 1)+
  labs(y = "", x = st.labs["years"]) +
#  scale_fill_brewer(na.value = "grey50" )+
  facet_wrap(~variable, scales = "free_y", nrow = 2)+
  theme(legend.position = "none")

## ----continuousEDA, fig.cap="Continuous variable EDA plots. Points indicate variable value against the follow up time in years. Points are colored according to the death event in the  \\code{status} variable. Missing values are indicated by the rug marks along the X-axis", fig.width=7, fig.height=4----
## Not displayed ##

# Use reshape2::melt to transform the data into long format.
cnt <- c(cnt, which(colnames(pbc) == "status"))
dta <- melt(pbc[,cnt], id.vars = c("years", "status"))

# plot panels for each covariate colored by the logical chas variable.
ggplot(dta, aes(x = years, y = value, color = status, shape = status))+
  geom_point(alpha = .3)+
  geom_rug(data = dta[which(is.na(dta$value)),], color = "grey50")+
  labs(y = "", x = st.labs["years"], color = "Death", shape = "Death") +
  scale_color_manual(values = strCol) +
  scale_shape_manual(values = event.marks)+
  facet_wrap(~variable, scales = "free_y", ncol = 4)+
  theme(legend.position = c(.8,.2))

## ----missing, results="asis"------------------------------------------------------------
## Not displayed ##
# create a missing data table

pbc.trial <- pbc[-which(is.na(pbc$treatment)),]
st <- apply(pbc,2, function(rw){sum(is.na(rw))})
st.t <- apply(pbc.trial,2, function(rw){sum(is.na(rw))})

st <- data.frame(cbind(full = st, trial = st.t))

st <- st[which(st$full>0),]
rws <- seq(1, (nrow(st)), by = 2)
col <- rep("\\rowcolor[gray]{0.95}", length(rws))

print(xtable(st, 
             caption = "PBC missing values", 
             label = "T:missing", 
             digits = 3), 
      size = 'footnotesize', # fontsize
      booktabs = TRUE, 
      add.to.row = list(pos = as.list(rws), command = col), 
      command= c('\\toprule' , 
                   '\\midrule' , 
                   '\\bottomrule ')
      )

## ----gg_survival, echo=TRUE-------------------------------------------------------------
# Include only the randomized patients.
pbc.trial <- pbc[-which(is.na(pbc$treatment)),]

# Create a test set from the remaining patients
pbc.test <- pbc[which(is.na(pbc$treatment)),]

# Create the gg_survival object
gg_dta <- gg_survival(interval = "years",
                      censor = "status", 
                      by = "treatment", 
                      data = pbc.trial, 
                      conf.int = .95)

## ----plot_gg_survival, fig.cap="Kaplan--Meier pbc data survival estimates comparing the treatment with placebo. Mean survival with shaded 95\\% condfidence band.", echo=TRUE----
plot(gg_dta) +
  labs(y = "Survival Probability", 
       x = "Observation Time (years)", 
       color = "Treatment", fill = "Treatment")+
  theme(legend.position = c(.2,.2))+
  coord_cartesian(y = c(0,1.01))

## ----plot_gg_cum_hazard, fig.cap="Kaplan--Meier pbc data cumulative hazard estimates comparing the treatment with placebo.", echo=TRUE----
plot(gg_dta, type="cum_haz") +
  labs(y = "Cumulative Hazard", 
       x = "Observation Time (years)", 
       color = "Treatment", fill = "Treatment")+
  theme(legend.position = c(.2,.8))

## ----xtab, results="asis"---------------------------------------------------------------
## Not displayed ##
# Create a table summarizing the ph model from fleming and harrington 1991
fleming.table <- data.frame(matrix(ncol = 3, nrow = 5))
rownames(fleming.table) <- 
  c("Age", "log(Albumin)", "log(Bilirubin)", "Edema", "log(Prothrombin Time)")
colnames(fleming.table) <- c("Coef.", "Std. Err.", "Z stat.")
fleming.table[,1] <- c(0.0333, -3.0553,0.8792, 0.7847, 3.0157) 
fleming.table[,2] <- c(0.00866, 0.72408,0.09873,0.29913,1.02380) 
fleming.table[,3] <- c(3.84,-4.22,8.9,2.62,2.95) 

rws <- seq(1, (nrow(fleming.table)), by = 2)
col <- rep("\\rowcolor[gray]{0.95}", length(rws))

print(xtable(fleming.table, 
             caption = "Regression model with log transformations of continuous variables, 312 randomized cases with PBC.", 
             label = "T:FHmodel", 
             digits = 4), 
      size = 'footnotesize', # fontsize
      booktabs = TRUE, 
      add.to.row = list(pos = as.list(rws), command = col), 
      command= c('\\toprule' , 
                   '\\midrule' , 
                   '\\bottomrule ')
      )

## ----gg_survival-bili, fig.cap="Kaplan--Meier pbc data survival estimates comparing Bilirubin measures. Groups defined in~\\cite{fleming:1991}.", echo=TRUE, fig.width=5.5----
# Duplicate the trial data
pbc.bili <- pbc.trial

# Group by bilirubin values 
pbc.bili$bili_grp <- cut(pbc.trial$bili, 
                         breaks = c(0, .8, 1.3, 3.4, 
                                  max(pbc.trial$bili)))

# plot the gg_survival object directly
plot(gg_survival(interval = "years",censor = "status", 
                 by = "bili_grp", data = pbc.bili),
     error = "none") +
  labs(y = "Survival Probability", 
       x = "Observation Time (years)", 
       color = "Bilirubin")

## ----rfsrc, echo=TRUE, eval=FALSE-------------------------------------------------------
#  # Grow and store the random survival forest
#  # Use random splitting (nsplit = 10) and impute
#  # missing values (na.action = "na.impute")
#  rfsrc_pbc <- rfsrc(Surv(years, status) ~ .,
#                     data = pbc.trial,
#                     nsplit = 10,
#                     na.action = "na.impute")
#  
#  # Print the forest summary
#  rfsrc_pbc

## ----read-forest, echo=FALSE, results=FALSE---------------------------------------------
# in reality, we use data caching to make vignette 
# compilation quicker. The rfsrc_pbc forest is stored
# as a ggRandomForests data sets
#
# This code block produces the R output from the 
# rfsrc grow block above. We set the chunk argument 
# "echo=FALSE" above so this code does not show up 
# in the manuscript.
data(rfsrc_pbc, package = "ggRandomForests")
rfsrc_pbc

## ----errorPlot, fig.cap="Random forest prediction error estimates as a function of the number of trees in the forest."----
# Data extraction
ggerr <- gg_error(rfsrc_pbc)

# Display the figure
plot(ggerr)+
  coord_cartesian(y = c(.09,.31))

## ----rfsrc-plot, fig.cap="Random forest predicted survival. Blue lines correspond to censored observations, red lines correspond to patients who experienced the event (death)."----
# Data extraction 
gg_dta <- gg_rfsrc(rfsrc_pbc)

# Save the ggplot2 object
ggRFsrc <- plot(gg_dta, alpha = .2) + 
  scale_color_manual(values = strCol) + 
  theme(legend.position = "none") + 
  labs(y = "Survival Probability", x = "time (years)")+
  coord_cartesian(y = c(-.01,1.01))

# Display the figure
show(ggRFsrc)

## ----rfsrc-mean, fig.cap="Mean value random forest predicted survival with shaded 95\\% confidence band."----
plot(gg_rfsrc(rfsrc_pbc, conf.int = .95)) + 
  theme(legend.position = "none") + 
  labs(y = "Survival Probability", x = "time (years)")+
  coord_cartesian(y = c(-.01,1.01))

## ----rfsrc-mean2, fig.cap="Mean value random forest predicted survival with shaded 95\\% confidence band. Treatment effects."----
plot(gg_rfsrc(rfsrc_pbc, by="treatment")) + 
  theme(legend.position = c(.2,.2)) + 
  labs(y = "Survival Probability", x = "time (years)")+
  coord_cartesian(y = c(-.01,1.01))

## ----predict, echo=TRUE, eval=FALSE-----------------------------------------------------
#  # Predict survival for 106 patients not in randomized trial
#  rfsrc_pbc_test <- predict(rfsrc_pbc,
#                      newdata = pbc.test,
#                      na.action = "na.impute")
#  
#  # Print prediction summary
#  rfsrc_pbc_test

## ----predict-load, echo=FALSE-----------------------------------------------------------
# Predict survival for 106 patients not in randomized trial
data(rfsrc_pbc_test, package="ggRandomForests")
# Print prediction summary  
rfsrc_pbc_test

## ----predictPlot, fig.cap="Test set prediction: 106 observations."----------------------
# Test set predicted survival
plot(gg_rfsrc(rfsrc_pbc_test), alpha=.2)+ 
  scale_color_manual(values = strCol) + 
  theme(legend.position = "none") + 
  labs(y = "Survival Probability", x = "time (years)")+
  coord_cartesian(y = c(-.01,1.01))

## ----rf-vimp, echo=TRUE, fig.cap="Random forest variable Importance (VIMP). Blue bars indicate important variables (positive VIMP), red indicates noise variables (negative VIMP).", fig.width=5----
plot.gg_vimp(rfsrc_pbc, lbls = st.labs) + 
  theme(legend.position = c(.8,.2))+
  labs(fill = "VIMP > 0")+
  scale_fill_brewer(palette = "Set1")

## ----mindepth-view, eval=FALSE, echo=TRUE-----------------------------------------------
#  varsel_pbc <- var.select(rfsrc_pbc)
#  ggMindepth <- gg_minimal_depth(varsel_pbc, lbls = st.labs)
#  print(ggMindepth)

## ----mindepth-load----------------------------------------------------------------------
data(varsel_pbc, package = "ggRandomForests")
ggMindepth <- gg_minimal_depth(varsel_pbc)
ggMindepth

## ----mindepth-plot, echo=TRUE, fig.cap="Minimal Depth variable selection. Low minimal depth indicates important variables. The dashed line is the threshold of maximum value for variable selection.", fig.width=5----
plot(ggMindepth, lbls = st.labs)

## ----depthVimp, fig.cap="Comparing Minimal Depth and Vimp rankings. Points on the red dashed line are ranked equivalently, points below have higher VIMP, those above have higher minimal depth ranking. Variables are colored by the sign of the VIMP measure.", fig.width=5----
gg.mv <- gg_minimal_vimp(varsel_pbc)
plot(gg.mv, lbls = st.labs)+
  theme(legend.position=c(.8,.2))+
  scale_y_continuous(breaks = seq(0,20,2))

## ----rfsrc-plot3Mnth, echo=TRUE, fig.cap="Random forest OOB predicted patient survival. Red curves correspond to patients which have died, blue corresponds to alive (or censored) cases. Vertical dashed lines indicate the 1 and 3 year survival estimates."----
ggRFsrc + 
  geom_vline(aes(xintercept = c(1, 3)), linetype = "dashed") + 
  coord_cartesian(x = c(0, 4))

## ----variable-plotbili, echo=TRUE, fig.cap="Bilirubin variable dependence at 1 and 3 years. Individual cases are marked with blue circles (alive or censored) and red xs (dead). Loess smooth curve with shaded 95\\% confidence band indicates the survival trend with increasing bilirubin.", fig.height=4----
# Get the minimal depth selected variables
xvar <- varsel_pbc$topvars

# Data generation
ggrf <- gg_variable(rfsrc_pbc, time = c(1, 3), 
                    time.labels = c("1 Year", "3 Years"))

# Plot the bilirubin variable dependence plot
plot(ggrf, xvar = "bili", se = .95, alpha = .3) + 
  labs(y = "Survival", x = st.labs["bili"]) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = strCol, labels = event.labels) + 
  scale_shape_manual(values = event.marks, labels = event.labels)+
  coord_cartesian(y = c(-.01,1.01))

## ----variable-plot, echo=TRUE, fig.cap="Bilirubin variable dependence at 1 and 3 years. Individual cases are marked with blue circles (alive or censored) and red xs (dead). Loess smooth curve with shaded 95\\% confidence band indicates the survival trend with increasing bilirubin.", fig.height=4, fig.width=7----
# Pull the categorical variables
xvar.cat <- c("edema", "stage")
xvar <- xvar[-which(xvar %in% xvar.cat)]

# plot the next 5 continuous variable dependence plots.
plot(ggrf, xvar = xvar[2:6], panel = TRUE, 
     se = FALSE, alpha = .3, 
     method = "glm", formula = y~poly(x,2)) + 
  labs(y = "Survival") + 
  theme(legend.position = "none") + 
  scale_color_manual(values = strCol, labels = event.labels) + 
  scale_shape_manual(values = event.marks, labels = event.labels)+
  coord_cartesian(y = c(-.01,1.01))

## ----variable-plotCat, echo=TRUE, fig.cap="Variable dependence plots at 1 and 3 years for continuous variables age, albumin, copper and prothrombin. Individual cases are marked with blue circles (alive or censored) and red xs (dead). Loess smooth curve indicates the survival trend with increasing variable value.", fig.height=4----
plot(ggrf, xvar = xvar.cat, panel = TRUE, notch = TRUE, alpha = .3) + 
  labs(y = "Survival") + 
  theme(legend.position = "none") + 
  scale_color_manual(values = strCol, labels = event.labels) + 
  scale_shape_manual(values = event.marks, labels = event.labels)+
  coord_cartesian(y = c(-.01,1.02))

## ----pbc-partial, echo=TRUE, eval=FALSE-------------------------------------------------
#  # Calculate the 1, 3 and 5 year partial dependence
#  partial_pbc <- lapply(c(1,3,5), function(tm){
#    plot.variable(rfsrc_pbc, surv.type = "surv",
#                  time = tm,
#                  xvar.names = xvar, partial = TRUE,
#                  show.plots = FALSE)
#    })

## ----pbc-partial-load-------------------------------------------------------------------
data("partial_pbc", package = "ggRandomForests")
xvar <- varsel_pbc$topvars

## ----pbc-partial-bili, echo=TRUE, fig.cap="Partial dependence plot of (risk adjusted) predicted survival probability as a function of serum bilirubin at 1 year (red circle) and 3 years (blue triangle). Loess smooth curves indicates the trend."----
# Convert all partial plots to gg_partial objects
gg_dta <- lapply(partial_pbc, gg_partial)

# Combine the objects to get multiple time curves 
# along variables on a single figure.
pbc_ggpart <- combine.gg_partial(gg_dta[[1]], gg_dta[[2]], 
                                 lbls = c("1 Year", "3 Years"))


plot(pbc_ggpart[["bili"]], se = FALSE) + 
  theme(legend.position = c(.2, .2)) + 
  labs(y = "Survival", 
       x = st.labs["bili"],
       color = "Time", shape = "Time")+
  scale_color_brewer(palette = "Set2")+
  coord_cartesian(y = c(25,101))

## ----pbc-partial-panel, echo=TRUE, fig.cap="Partial dependence plot of (risk adjusted) predicted survival probability as a function continuous variables prothrombin time, albumin, age and urin copper at 1 year (red circle) and 3 years (blue triangle).", fig.width=5, fig.height=5----
# Create a temporary holder and remove the stage and edema data
ggpart <- pbc_ggpart
ggpart$edema <- ggpart$stage <- NULL
ggpart$bili <- ggpart$sgot <- ggpart$chol <- NULL
ggpart$platelet <- ggpart$trig <- ggpart$alk <- NULL

# Panel plot the remainder.
plot(ggpart, se = FALSE, panel = TRUE) + 
  labs(x = "", y = "Survival", color = "Time", shape = "Time") +
  scale_color_brewer(palette = "Set2") + 
  theme(legend.position = c(.2, .15)) + 
  coord_cartesian(y = c(25,101))

## ----pbc-partial-edema, echo=TRUE, fig.cap="Partial dependence plot of (risk adjusted) predicted survival probability as a function of edema (categorical variable) at 1 year (red) and 3 years (blue triangle). Points indicate risk adjusted prediction for all patients within each edema group. Box plots indicate distributional properties within each group.", fig.width=5----
ggpart <- vector("list", length=2)
ggpart[[1]] <- pbc_ggpart[["edema"]]
ggpart[[2]] <- pbc_ggpart[["stage"]]
names(ggpart) <- c("edema", "stage")
class(ggpart) <- c("gg_partial_list", class(ggpart))

plot.gg_partial_list(ggpart, panel=TRUE,
                     notch = TRUE, alpha = .3, outlier.shape = NA) + 
  labs(x = "", y = "Survival (%)", color="Time", shape="Time")+
  scale_color_brewer(palette = "Set2")+
  theme(legend.position = c(.2, .2))+
  coord_cartesian(y = c(25,101))

## ----def-time-pts-----------------------------------------------------------------------
time_pts <- rfsrc_pbc$time.interest[which(rfsrc_pbc$time.interest<=5)]
# Find the quantile points to create 30 interval groups
time_cts <-quantile_pts(time_pts, groups = 50)

## ----prtl-time-surface, eval=FALSE------------------------------------------------------
#  # Generate the gg_partial_coplot data object
#  system.time(partial_pbc_time <- lapply(time_cts, function(ct){
#    plot.variable(rfsrc_pbc, xvar = "bili", time = ct,
#                  npts = 50, show.plots = FALSE,
#                  partial = TRUE, surv.type="surv")
#    }))
#  #     user   system  elapsed
#  # 2561.313   81.446 2641.707

## ----timeContour3D, fig.cap="Partial coplot contour plot.", fig.width=7, fig.height=5----
# Load the stored partial coplot data.
data(partial_pbc_time)

# Instead of groups, we want the raw rm point values,
# To make the dimensions match, we need to repeat the values
# for each of the 50 points in the lstat direction
time.tmp <- do.call(c,lapply(time_cts, 
                               function(grp){rep(grp, 50)}))

# Convert the list of plot.variable output to 
partial_time <- do.call(rbind,lapply(partial_pbc_time, gg_partial))

# attach the data to the gg_partial_coplot
partial_time$time <- time.tmp

# ggplot2 contour plot of x, y and z data.
ggplot(partial_time, aes(y = bili, x = time, z = yhat))+
  stat_contour(aes(colour = ..level..), binwidth = .25)+
  labs(y = st.labs["bili"], x = "time (years)", 
       color = "Survival (%)")+
  scale_colour_gradientn(colours = heat.colors(25))

## ----timeSurface3d, fig.cap="Partial coplot surface.", fig.width=7, fig.height=5--------
# Modify the figure margins to make the figure larger
par(mai = c(0,0,0,0))

# Transform the gg_partial_coplot object into a list of three named matrices
# for surface plotting with plot3D::surf3D
srf <- surface_matrix(partial_time, c("time", "bili", "yhat"))

# Generate the figure.
surf3D(x = srf$x, y = srf$y, z = srf$z, col = heat.colors(25),
       colkey = FALSE, border = "black", bty = "b2", 
       shade = 0.5, expand = 0.5, 
       lighting = TRUE, lphi = -50,
       ylab = "Bilirubin", xlab = "Time", zlab = "Survival"
)

## ----interaction-show, echo=TRUE, eval=FALSE--------------------------------------------
#  interaction_pbc <- find.interaction(rfsrc_pbc)
#  
#  ggint <- gg_interaction(interaction_pbc)

## ----interaction------------------------------------------------------------------------
data(interaction_pbc, package = "ggRandomForests")
ggint <- gg_interaction(interaction_pbc)

## ----interactionPanel, echo=TRUE, fig.cap="Minimal depth variable interaction panel with prothrombin time, albumin, urine copper and edema. Higher values indicate lower interactivity with target variable.", fig.width=7, fig.height=5----
plot(ggint, xvar = xvar) + 
  labs(y = "Interactive Minimal Depth") + 
  theme(legend.position = "none")

## ----var_dep, echo=TRUE, fig.cap="Variable dependence plot. Survival at 1 year against bilirubin. Individual cases are marked with blue circles (alive or censored) and red x (dead). Loess smooth curve indicates the trend as bilirubin  increases."----
ggvar <- gg_variable(rfsrc_pbc, time = 1)
ggvar$stage <- paste("stage = ", ggvar$stage, sep = "")

var_dep <- plot(ggvar, xvar = "bili", 
                method = "glm",
                alpha = .5, se = FALSE) + 
  labs(y = "Survival", 
       x = st.labs["bili"]) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = strCol, labels = event.labels) + 
  scale_shape_manual(values = event.marks, labels = event.labels)+
  coord_cartesian(y = c(-.01,1.01))

show(var_dep)

## ----coplot_bilirubin, echo=TRUE, fig.cap="Variable dependence coplot. Survival at 1 year against bilirubin, stratified by treatment and histological stage.", fig.width=7, fig.height=4----
var_dep + 
  facet_grid(edema~stage)

## ----copper-coplot, fig.cap="Variable dependence coplot. Survival at 1 year against bilirubin, stratified by conditonal membership in Urine Copper measurement intervalse.", fig.width=7, fig.height=4, echo=TRUE----
# Find intervals with similar number of observations.
copper_cts <-quantile_pts(ggvar$copper, groups = 6, intervals = TRUE)

# We need to move the minimal value so we include that observation
copper_cts[1] <- copper_cts[1] - 1.e-7

# Create the conditional groups and add to the gg_variable object
copper_grp <- cut(ggvar$copper, breaks = copper_cts)
ggvar$copper_grp <- copper_grp

# Adjust naming for facets
levels(ggvar$copper_grp) <- paste("copper = ",levels(copper_grp), sep = "")

# plot.gg_variable
plot(ggvar[-which(is.na(ggvar$copper)),], xvar = "bili", 
                method = "glm", alpha = .5, se = FALSE) + 
  labs(y = "Survival", x = st.labs["bili"]) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = strCol, labels = event.labels) + 
  scale_shape_manual(values = event.marks, labels = event.labels)+ 
  facet_wrap(~copper_grp)+
  coord_cartesian(y = c(-.01,1.01))

## ----bili-coplot, fig.cap="Variable dependence coplot. Survival at 1 year against bilirubin, stratified by conditonal membership in Urine Copper measurement intervalse.", fig.width=7, fig.height=4, echo=TRUE----
# Find intervals with similar number of observations.
bili_cts <-quantile_pts(ggvar$bili, groups = 6, intervals = TRUE)

# We need to move the minimal value so we include that observation
bili_cts[1] <- bili_cts[1] - 1.e-7

# Create the conditional groups and add to the gg_variable object
bili_grp <- cut(ggvar$bili, breaks = bili_cts)
ggvar$bili_grp <- bili_grp

# Adjust naming for facets
levels(ggvar$bili_grp) <- paste("bilirubin = ",levels(bili_grp), sep = "")

# plot.gg_variable
plot(ggvar[-which(is.na(ggvar$copper)),], xvar = "copper", 
                method = "glm", alpha = .5, se = FALSE) + 
  labs(y = "Survival", x = st.labs["copper"]) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = strCol, labels = event.labels) + 
  scale_shape_manual(values = event.marks, labels = event.labels)+ 
  facet_wrap(~bili_grp)+
  coord_cartesian(y = c(-.01,1.01))

## ----build-bili-copper, eval=FALSE------------------------------------------------------
#  partial_coplot_pbc <- gg_partial_coplot(rfsrc_pbc, xvar = "bili",
#                                           groups = copper_grp,
#                                           surv_type = "surv",
#                                           time = 1,
#                                           show.plots = FALSE)

## ----bili-copper, fig.cap="Partial (risk adjusted) variable dependence coplot. Survival at 1 year against bilirubin, stratified by copper groups. Points mark risk adjusted estimates, loess smooth indicates predicted trend within each age group as a function of bilirubin.", fig.width=7, fig.height=4, echo=TRUE----
# Load cached partial plot data
data(partial_coplot_pbc, package = "ggRandomForests")

# Partial coplot
plot(partial_coplot_pbc, se = FALSE)+
  labs(x = st.labs["bili"], y = "Survival at 1 year (%)", 
       color = "Urine Copper", shape = "Urine Copper")+
  scale_color_brewer(palette = "Set2")+
  coord_cartesian(y = c(49,101))

## ----build-copper-bili, eval=FALSE------------------------------------------------------
#  partial_coplot_pbc2 <- gg_partial_coplot(rfsrc_pbc, xvar = "copper",
#                                           groups = bili_grp,
#                                           surv_type = "surv",
#                                           time = 1,
#                                           show.plots = FALSE)

## ----copper-bili, fig.cap="Partial (risk adjusted) variable dependence coplot. Survival at 1 year against bilirubin, stratified by copper groups. Points mark risk adjusted estimates, loess smooth indicates predicted trend within each age group as a function of bilirubin.", fig.width=7, fig.height=4, echo=TRUE----
# Load cached partial plot data
data(partial_coplot_pbc2, package = "ggRandomForests")

# Partial coplot
plot(partial_coplot_pbc2, se = FALSE)+
  labs(x = st.labs["copper"], y = "Survival at 1 year (%)", 
       color = "Bilirubin", shape = "Bilirubin")+
  scale_color_brewer(palette = "Set2")+
  coord_cartesian(y = c(49,101))

## ----def-pts----------------------------------------------------------------------------
# Find the quantile points to create 50 cut points for 49 groups
copper_cts <-quantile_pts(ggvar$copper, groups = 50)

## ----prtl-surface, eval=FALSE-----------------------------------------------------------
#  system.time(partial_pbc_surf <- lapply(copper_cts, function(ct){
#    rfsrc_pbc$xvar$copper <- ct
#    plot.variable(rfsrc_pbc, xvar = "bili", time = 1,
#                  npts = 50, show.plots = FALSE,
#                  partial = TRUE, surv.type="surv")
#    }))
#  # user   system  elapsed
#  # 2547.482   91.978 2671.870
#  

## ----contour3d, fig.cap="Partial coplot contour plot.", fig.width=7, fig.height=5-------
# Load the stored partial coplot data.
data(partial_pbc_surf)

# Instead of groups, we want the raw copper point values,
# To make the dimensions match, we need to repeat the values
# for each of the 50 points in the lstat direction
copper.tmp <- do.call(c,lapply(copper_cts, 
                               function(grp){rep(grp, 50)}))

# Convert the list of plot.variable output to 
partial_surf <- do.call(rbind,lapply(partial_pbc_surf, gg_partial))

# attach the data to the gg_partial_coplot
partial_surf$copper <- copper.tmp

# ggplot2 contour plot of x, y and z data.
ggplot(partial_surf, aes(x = bili, y = copper, z = yhat))+
  stat_contour(aes(colour = ..level..), binwidth = .25)+
  labs(x = st.labs["bili"], y = st.labs["copper"], 
       color = "Survival at 1 year (%)")+
  scale_colour_gradientn(colours = topo.colors(25))

## ----surface3d, fig.cap="Partial coplot surface.", fig.width=7, fig.height=5------------
# Modify the figure margins to make the figure larger
par(mai = c(0,0,0,0))

# Transform the gg_partial_coplot object into a list of three named matrices
# for surface plotting with plot3D::surf3D
srf <- surface_matrix(partial_surf, c("bili", "copper", "yhat"))

# Generate the figure.
surf3D(x = srf$x, y = srf$y, z = srf$z, col = topo.colors(25),
       colkey = FALSE, border = "black", bty = "b2", 
       shade = 0.5, expand = 0.5, 
       lighting = TRUE, lphi = -50,
       xlab = "Bilirubin", ylab = "Urine Copper", zlab = "Survival at 1 Year"
)

