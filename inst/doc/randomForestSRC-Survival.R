## ----setup, include = FALSE, cache = FALSE, echo=FALSE----------------------------------
## Not displayed ##
library("knitr")
knitr::render_sweave() 
# set global chunk options for knitr. These can be changed in the header for each individual R code chunk
opts_chunk$set(fig.path = 'fig-rfs/rfs-', 
               fig.align = 'center', 
               fig.pos = "!htb", 
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
options(mc.cores = 1, rf.cores = 0)

## ----libraries--------------------------------------------------------------------------
################## Load packages ##################
library("ggplot2")         # Graphics engine
library("RColorBrewer")    # Nice color palettes
library("plot3D")          # for 3d surfaces. 
library("dplyr")           # Better data manipulations
library("parallel")        # mclapply for multicore processing

# Analysis packages.
library("randomForestSRC") # random forest for survival, regression and 
                           # classification
library("ggRandomForests") # ggplot2 random forest figures (This!)

################ Default Settings ##################
theme_set(theme_bw())     # A ggplot2 theme with white background

## Set open circle for censored, and x for events 
event.marks <- c(1, 4)
event.labels <- c(FALSE, TRUE)

## We want red for death events, so reorder this set.
strCol <- brewer.pal(3, "Set1")[c(2,1,3)]

## ----datastep, echo=TRUE----------------------------------------------------------------
data("pbc", package = "randomForestSRC")

## ----data-clean-------------------------------------------------------------------------
library("tidyr")        # Transforming wide data into long data (gather)

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

labels <- c("Event (F = censor, T = death)", 
            "Treament (DPCA, Placebo)", 
            "Age (years)", 
            "Female = T", 
            "Presence of Asictes", 
            "Presence of Hepatomegaly", 
            "Presence of Spiders", 
            "Edema (0, 0.5, 1)", 
            "Serum Bilirubin (mg/dl)", 
            "Serum Cholesterol (mg/dl)", 
            "Albumin (gm/dl)", 
            "Urine Copper (ug/day)", 
            "Alkaline Phosphatase (U/liter)", 
            "SGOT (U/ml)", 
            "Triglicerides (mg/dl)", 
            "Platelets per cubic ml/1000", 
            "Prothrombin time (sec)", 
            "Histologic Stage", 
            "Time (years)")

dta.labs <- data.frame(cbind(names = colnames(pbc), label = labels, type = cls))
# Put the "years" variable on top.
dta.labs <- rbind(dta.labs[nrow(dta.labs),], dta.labs[-nrow(dta.labs),])

st.labs <- as.character(dta.labs$label)
names(st.labs) <- rownames(dta.labs)

## ----dta-table, results="asis"----------------------------------------------------------
## Not displayed ##
# create a data dictionary table
tmp <- dta.labs
colnames(tmp) <- c("Variable name", "Description", "Type")
kable(tmp, 
      #format="latex",
      caption = "\\label{T:dataLabs}\\code{pbc} data set variable dictionary.",
      row.names = FALSE,
      booktabs=TRUE)

## ----categoricalEDA, fig.cap="EDA plots for categorical variables (logicals and factors). Bars indicate number of patients within 1 year of followup interval for each categorical variable. Colors correspond to class membership within each variable. Missing values are included in white.", fig.width=7----
## Not displayed ##
# Use tidyr::gather to transform the data into long format.
cnt <- c(which(cls == "numeric" ), which(cls == "integer"))
fct <- setdiff(1:ncol(pbc), cnt)
fct <- c(fct, which(colnames(pbc) == "years"))
dta <- gather(pbc[,fct], variable, value, -years)

# plot panels for each covariate colored by the logical chas variable.
ggplot(dta, aes(x = years, fill = value)) +
  geom_histogram(color = "black", binwidth = 1) +
  labs(y = "", x = st.labs["years"]) +
  scale_fill_brewer(palette="RdBu",na.value = "white" ) +
  facet_wrap(~variable, scales = "free_y", nrow = 2) +
  theme(legend.position = "none")

## ----continuousEDA, fig.cap="EDA plots for continuous variables. Symbols indicate observations with variable value on Y-axis against follow up time in years. Symbols are colored and shaped according to the death event  (\\code{status} variable). Missing values are indicated by rug marks along the X-axis", fig.width=7, fig.height=4----
## Not displayed ##

# Use tidyr::gather to transform the data into long format.
cnt <- c(cnt, which(colnames(pbc) == "status"))
dta <- gather(pbc[,cnt], variable, value, -years, -status)

# plot panels for each covariate colored by the logical chas variable.
ggplot(dta, aes(x = years, y = value, color = status, shape = status)) +
  geom_point(alpha = 0.4) +
  geom_rug(data = dta[which(is.na(dta$value)),], color = "grey50") +
  labs(y = "", x = st.labs["years"], color = "Death", shape = "Death") +
  scale_color_manual(values = strCol) +
  scale_shape_manual(values = event.marks) +
  facet_wrap(~variable, scales = "free_y", ncol = 4) +
  theme(legend.position = c(0.8, 0.2))

## ----missing, results="asis"------------------------------------------------------------
## Not displayed ##
# create a missing data table
pbc.trial <- pbc %>% filter(!is.na(treatment))
st <- apply(pbc,2, function(rw){sum(is.na(rw))})
st.t <- apply(pbc.trial,2, function(rw){sum(is.na(rw))})
st <- data.frame(cbind(full = st, trial = st.t))
st <- st[which(st$full>0),]
colnames(st) <- c("pbc", "pbc.trial")

kable(st, 
      format="latex",
      caption = "\\label{T:missing}Missing value counts in \\code{pbc} data set and pbc clinical trial observations (\\code{pbc.trial}).",
      digits = 3,
      booktabs=TRUE)

## ----gg_survival------------------------------------------------------------------------
# Create the trial and test data sets.
pbc.trial <- pbc %>% filter(!is.na(treatment))
pbc.test <- pbc %>% filter(is.na(treatment))

# Create the gg_survival object
gg_dta <- gg_survival(interval = "years",
                      censor = "status", 
                      by = "treatment", 
                      data = pbc.trial, 
                      conf.int = 0.95)

## ----plot_gg_survival, fig.cap="Kaplan--Meier survival estimates comparing the \\code{DPCA} treatment (red) with \\code{placebo} (blue) groups for the \\code{pbc.trail} data set. Median survival with shaded 95\\% confidence band.", echo=TRUE----
plot(gg_dta) +
  labs(y = "Survival Probability", x = "Observation Time (years)", 
       color = "Treatment", fill = "Treatment") +
  theme(legend.position = c(0.2, 0.2)) +
  coord_cartesian(y = c(0, 1.01))

## ----plot_gg_cum_hazard, fig.cap="Kaplan--Meier cumulative hazard estimates comparing the \\code{DPCA} treatment (red) with \\code{placebo} (blue) groups for the \\code{pbc} data set.", echo=TRUE----
plot(gg_dta, type = "cum_haz") +
  labs(y = "Cumulative Hazard", x = "Observation Time (years)", 
       color = "Treatment", fill = "Treatment") +
  theme(legend.position = c(0.2, 0.8)) +
  coord_cartesian(ylim = c(-0.02, 1.22))

## ----gg_survival-bili, fig.cap="Kaplan--Meier survival estimates comparing different groups of Bilirubin measures (\\code{bili}) for the \\code{pbc} data set. Groups defined in Chapter 4 of~\\cite{fleming:1991}.", echo=TRUE, fig.width=5.5----
pbc.bili <- pbc.trial
pbc.bili$bili_grp <- cut(pbc.bili$bili, breaks = c(0, 0.8, 1.3, 3.4, 29))

plot(gg_survival(interval = "years", censor = "status", by = "bili_grp", 
                 data = pbc.bili), error = "none") +
  labs(y = "Survival Probability", x = "Observation Time (years)", 
       color = "Bilirubin")

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

kable(fleming.table, 
      format="latex",
      caption = "\\label{T:FHmodel}\\code{pbc} proportional hazards model summary of 312 randomized cases in \\code{pbc.trial} data set. ~\\citep[Table 4.4.3c]{fleming:1991} ", 
      digits = 3,
      booktabs=TRUE)

## ----rfsrc, echo=TRUE, eval=FALSE-------------------------------------------------------
#  rfsrc_pbc <- rfsrc(Surv(years, status) ~ ., data = pbc.trial,
#                     nsplit = 10, na.action = "na.impute")

## ----read-forest, echo=FALSE, results=FALSE---------------------------------------------
# in reality, we use data caching to make vignette 
# compilation quicker. The rfsrc_pbc forest is stored
# as a ggRandomForests data sets
#
# This code block produces the R output from the 
# rfsrc grow block above. We set the chunk argument 
# "echo=FALSE" above so this code does not show up 
# in the manuscript.
data("rfsrc_pbc", package = "ggRandomForests")
rfsrc_pbc

## ----errorPlot, fig.cap="Random forest OOB prediction error estimates as a function of the number of trees in the forest.", echo=TRUE----
plot(gg_error(rfsrc_pbc)) + coord_cartesian(ylim = c(0.09, 0.31))

## ----rfsrc-plot, fig.cap="Random forest OOB predicted survival. Blue curves correspond to censored observations, red curves correspond to observations experiencing death events.", echo=TRUE----
ggRFsrc <- plot(gg_rfsrc(rfsrc_pbc), alpha = 0.2) + 
  scale_color_manual(values = strCol) + 
  theme(legend.position = "none") + 
  labs(y = "Survival Probability", x = "Time (years)") +
  coord_cartesian(ylim = c(-0.01, 1.01))
show(ggRFsrc)

## ----rfsrc-mean2, fig.cap="Random forest predicted survival stratified by treatment groups. \\code{DPCA} group in red, \\code{placebo} in blue with shaded 95\\% confidence bands.", echo=TRUE----
plot(gg_rfsrc(rfsrc_pbc, by = "treatment")) +  
  theme(legend.position = c(0.2, 0.2)) + 
  labs(y = "Survival Probability", x = "Time (years)") +
  coord_cartesian(ylim = c(-0.01, 1.01))

## ----predict, echo=TRUE, eval=FALSE-----------------------------------------------------
#  rfsrc_pbc_test <- predict(rfsrc_pbc, newdata = pbc.test,
#                            na.action = "na.impute")

## ----predict-load, echo=FALSE-----------------------------------------------------------
# Predict survival for 106 patients not in randomized trial
data("rfsrc_pbc_test", package="ggRandomForests")
# Print prediction summary  
rfsrc_pbc_test

## ----predictPlot, fig.cap="Random forest survival estimates for patients in the \\code{pbc.test} data set. Blue curves correspond to censored patients, red curves correspond to patients experiencing a death event.", echo=TRUE----
plot(gg_rfsrc(rfsrc_pbc_test), alpha=.2) + 
  scale_color_manual(values = strCol) + 
  theme(legend.position = "none") + 
  labs(y = "Survival Probability", x = "Time (years)") +
  coord_cartesian(ylim = c(-0.01, 1.01))

## ----rf-vimp, echo=TRUE, fig.cap="Random forest Variable Importance (VIMP). Blue bars indicates positive VIMP, red indicates negative VIMP. Importance is relative to positive length of bars.", fig.width=5----
plot(gg_vimp(rfsrc_pbc), lbls = st.labs) + 
  theme(legend.position = c(0.8, 0.2)) + labs(fill = "VIMP > 0")

## ----nms--------------------------------------------------------------------------------
## calculate for document
ggda <- gg_vimp(rfsrc_pbc)

## ----mindepth-view, eval=FALSE, echo=TRUE-----------------------------------------------
#  varsel_pbc <- var.select(rfsrc_pbc)
#  gg_md <- gg_minimal_depth(varsel_pbc, lbls = st.labs)
#  print(gg_md)

## ----mindepth-load----------------------------------------------------------------------
data("varsel_pbc", package = "ggRandomForests")
gg_md <- gg_minimal_depth(varsel_pbc)
gg_md

## ----mindepth-plot, echo=TRUE, fig.cap="Minimal Depth variable selection. Low minimal depth indicates important variables. The dashed line is the threshold of maximum value for variable selection.", fig.width=5----
plot(gg_md, lbls = st.labs)

## ----depthVimp, fig.cap="Comparing Minimal Depth and Vimp rankings. Points on the red dashed line are ranked equivalently, points above have higher VIMP ranking, those below have higher minimal depth ranking.", fig.width=5, echo=TRUE----
plot(gg_minimal_vimp(gg_md), lbls = st.labs) +
  theme(legend.position=c(0.8, 0.2))

## ----models-----------------------------------------------------------------------------
fleming.table$nm <- c("age","albumin", "bili","edema", "prothrombin")
fh.model <- data.frame(cbind(names = fleming.table$nm, 
                             FH = order(abs(fleming.table$`Z stat.`), 
                                        decreasing = TRUE),
                             Variable=rownames(fleming.table), 
                             Coeff=fleming.table$Coef.
                             ))
gg_v <- gg_vimp(rfsrc_pbc)
gg_v$rank <- 1:nrow(gg_v)
rownames(gg_v) <- gg_v$vars
md <- data.frame(cbind(names=gg_md$topvars))
md$rank <- 1:nrow(md)
rownames(md) <- gg_md$topvars

md$vimp <- gg_v[rownames(md),]$rank
md <- left_join(md, fh.model, by = "names")
md <- md[,c(1, 4, 2,3)]
colnames(md) <- c("Variable", "FH","Min depth", "VIMP" )
kable(md, 
      format="latex",
      caption = "\\label{T:modelComp}Comparison of variable selection criteria. Minimal depth ranking, VIMP ranking and ~\\cite{fleming:1991} (FH) proportional hazards model ranked according to \\code{abs(Z stat)} from Table~\\ref{T:FHmodel}.", 
      align=c("l", "r","r","r"),
      digits = 3,
      row.names = FALSE,
      booktabs=TRUE)

## ----rfsrc-plot3Mnth, echo=TRUE, fig.cap="Random forest predicted survival (Figure~\\ref{fig:rfsrc-plot}) with vertical dashed lines indicate the 1 and 3 year survival estimates."----
ggRFsrc + geom_vline(aes(xintercept = 1), linetype = "dashed") + 
   geom_vline(aes(xintercept = 3), linetype = "dashed") + 
  coord_cartesian(xlim = c(0, 5))

## ----variable-plotbili, echo=TRUE, fig.cap="Variable dependence of survival at 1 and 3 years on \\code{bili} variable. Individual cases are marked with blue circles (alive or censored) and red `x's (dead). Loess smooth curve with shaded 95\\% confidence band indicates decreasing survival with increasing bilirubin.", fig.height=4----
gg_v <- gg_variable(rfsrc_pbc, time = c(1, 3), 
                    time.labels = c("1 Year", "3 Years"))

plot(gg_v, xvar = "bili", alpha = 0.4) + #, se=FALSE
  labs(y = "Survival", x = st.labs["bili"]) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = strCol, labels = event.labels) + 
  scale_shape_manual(values = event.marks, labels = event.labels) +
  coord_cartesian(ylim = c(-0.01, 1.01))

## ----variable-plot, echo=TRUE, fig.cap="Variable dependence of predicted survival at 1 and 3 years on continuous variables of interest. Individual cases are marked with blue circles for censored cases and red `x's for death events. Loess smooth curve indicates the survival trend with increasing values.", fig.height=4, fig.width=7----
xvar <- c("bili", "albumin", "copper", "prothrombin", "age")
xvar.cat <- c("edema")

plot(gg_v, xvar = xvar[-1], panel = TRUE, alpha = 0.4) + #se = FALSE, , span=1
  labs(y = "Survival") + 
  theme(legend.position = "none") + 
  scale_color_manual(values = strCol, labels = event.labels) + 
  scale_shape_manual(values = event.marks, labels = event.labels) +
  coord_cartesian(ylim = c(-0.05, 1.05))

## ----variable-plotCat, echo=TRUE, fig.cap="Variable dependence of survival 1 and 3 years on \\code{edema} categorical variable. Symbols with blue circles indicate censored cases and red `x's indicate death events. Boxplots indicate distribution of predicted survival for all observations within each \\code{edema} group.", fig.height=4----
plot(gg_v, xvar = xvar.cat, alpha = 0.4) + labs(y = "Survival") + 
  theme(legend.position = "none") + 
  scale_color_manual(values = strCol, labels = event.labels) + 
  scale_shape_manual(values = event.marks, labels = event.labels) +
  coord_cartesian(ylim = c(-0.01, 1.02))

## ----pbc-partial, echo=TRUE, eval=FALSE-------------------------------------------------
#  xvar <- c(xvar, xvar.cat)
#  partial_pbc <- mclapply(c(1,3,5), function(tm){
#    plot.variable(rfsrc_pbc, surv.type = "surv", time = tm, xvar.names = xvar,
#                  partial = TRUE, show.plots = FALSE)
#    })

## ----pbc-partial-load-------------------------------------------------------------------
data("partial_pbc", package = "ggRandomForests")
xvar <- c(xvar, xvar.cat)

## ----pbc-partial-bili, echo=TRUE--------------------------------------------------------
gg_dta <- mclapply(partial_pbc, gg_partial)
pbc_ggpart <- combine.gg_partial(gg_dta[[1]], gg_dta[[2]], 
                                 lbls = c("1 Year", "3 Years"))

## ----pbc-partial-panel, echo=TRUE, fig.cap="Partial dependence of predicted survival at 1 year (red circle) and 3 years (blue triangle) as a function continuous variables of interest. Symbols are partial dependence point estimates with loess smooth line to indicate trends.", fig.width=5, fig.height=5----
ggpart <- pbc_ggpart
ggpart$edema <- NULL

plot(ggpart, panel = TRUE) + #, se = FALSE
  labs(x = "", y = "Survival", color = "Time", shape = "Time") +
  theme(legend.position = c(0.8, 0.2)) + 
  coord_cartesian(ylim = c(25, 101))

## ----pbc-partial-edema, echo=TRUE, fig.cap="Partial dependence plot of predicted survival at 1 year (red) and 3 years (blue) as a function of \\code{edema} groups (categorical variable). Boxplots indicate distribution within each group."----
ggplot(pbc_ggpart[["edema"]], aes(y=yhat, x=edema, col=group))+
  geom_boxplot(notch = TRUE, 
               outlier.shape = NA) + # panel=TRUE, 
  labs(x = "Edema", y = "Survival (%)", color="Time", shape="Time") +
  theme(legend.position = c(0.2, 0.2)) +
  coord_cartesian(ylim = c(25, 101))

## ----timeSurface3d, fig.cap="Partial dependence surface. Partial dependence of predicted survival (0 to 5 years) as a function of \\code{bili}. Blue lines indicate partial dependence at 1 and 3 years, as in \\code{bili} panel of Figure~\\ref{fig:pbc-partial-panel}.", fig.width=7, fig.height=5, echo=FALSE----
# Restrict the time of interest to less than 5 years.
time_pts <- rfsrc_pbc$time.interest[which(rfsrc_pbc$time.interest<=5)]

# Find the 50 points in time, evenly space along the distribution of 
# event times for a series of partial dependence curves
time_cts <-quantile_pts(time_pts, groups = 50)

# Load the stored partial coplot data.
data("partial_pbc_time")

# We need to attach the time points of interest to our data.
time.tmp <- do.call(c,lapply(time_cts, 
                             function(grp){rep(grp, 50)}))

# Convert the list of plot.variable output to gg_partial
partial_time <- do.call(rbind,lapply(partial_pbc_time, gg_partial))

# attach the time data to the gg_partial_coplot
partial_time$time <- time.tmp

# Modify the figure margins to make it larger
par(mai = c(0.5,0.55,0,0))

# Transform the gg_partial_coplot object into a list of three named matrices
# for surface plotting with plot3D::surf3D
srf <- surface_matrix(partial_time, c("time", "bili", "yhat"))

# Generate the figure.
surf3D(x = srf$x, y = srf$y, z = srf$z, col = heat.colors(25),
       colkey = FALSE, border = "black", bty = "b2", 
       shade = 0.5, expand = 0.5, theta=110, phi=15,
       lighting = TRUE, lphi = -50, ticktype="detailed",
       ylab = "Bilirubin", xlab = "Time", zlab = "Survival"
)

# Extract the 1 and 3 year points.
# Find the indices of the points closest in time
t.pts <- sapply(c(1,3), function(pt){min(abs(srf$x - pt), na.rm=TRUE)})
indx <- vector("list", length=2)
indx[[1]] <- which(abs(srf$x - 1) < t.pts[1]+1.e-5)
indx[[2]] <- which(abs(srf$x - 3) < t.pts[2]+1.e-5)

# Generate curves along 1 and 3 year partial dependence 
alt <- lapply(indx, function(ind){
  lines3D(x=srf$x[ind], y=srf$y[ind],z=srf$z[ind],
          add=TRUE, col="blue", lwd=6)
})

## ----var_dep, echo=TRUE, fig.cap="Variable dependence of survival at 1 year against \\code{bili} variable. Reproduction of top panel of Figure~\\ref{fig:variable-plotbili} with a linear smooth to indicate trend."----
# Get variable dependence at 1 year
ggvar <- gg_variable(rfsrc_pbc, time = 1)

# For labeling coplot membership
ggvar$edema <- paste("edema = ", ggvar$edema, sep = "")

# Plot with linear smooth (method argument)
var_dep <- plot(ggvar, xvar = "bili", 
                alpha = 0.5) + 
#  geom_smooth(method = "glm",se = FALSE) +
  labs(y = "Survival", 
       x = st.labs["bili"]) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = strCol, labels = event.labels) + 
  scale_shape_manual(values = event.marks, labels = event.labels) +
  coord_cartesian(y = c(-.01,1.01))

var_dep

## ----coplot_bilirubin, echo=TRUE, fig.cap="Variable dependence coplot of survival at 1 year against \\code{bili}, conditional on \\code{edema} group membership. Linear smooth indicates trend of variable dependence.", fig.width=7----
var_dep + facet_grid(~edema)

## ----albumin-coplot, fig.cap="Variable dependence coplot of survival at 1 year against \\code{bili}, conditional on \\code{albumin} interval group membership.", fig.width=7, fig.height=4, echo=TRUE----
# Find intervals with similar number of observations and create groups.
albumin_cts <- quantile_pts(ggvar$albumin, groups = 6, intervals = TRUE)
ggvar$albumin_grp <- cut(ggvar$albumin, breaks = albumin_cts)

# Adjust naming for facets
levels(ggvar$albumin_grp) <- paste("albumin =", levels(ggvar$albumin_grp))

plot(ggvar, xvar = "bili", alpha = 0.5) +  #method = "glm", , se = FALSE
  labs(y = "Survival", x = st.labs["bili"]) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = strCol, labels = event.labels) + 
  scale_shape_manual(values = event.marks, labels = event.labels) + 
  facet_wrap(~albumin_grp) +
  coord_cartesian(y = c(-.01,1.01))

## ----bili-coplot, fig.cap="Variable dependence coplot of survival at 1 year against \\code{albumin}, conditonal on \\code{bili} interval group membership.", fig.width=7, fig.height=4, echo=FALSE, results=FALSE----
# Find intervals with similar number of observations.
bili_cts <-quantile_pts(ggvar$bili, groups = 6, intervals = TRUE)

# We need to move the minimal value so we include that observation
bili_cts[1] <- bili_cts[1] - 1.e-7

# Create the conditional groups and add to the gg_variable object
bili_grp <- cut(ggvar$bili, breaks = bili_cts)
ggvar$bili_grp <- bili_grp

# Adjust naming for facets
levels(ggvar$bili_grp) <- paste("bilirubin =", levels(bili_grp))

# plot.gg_variable
plot(ggvar, xvar = "albumin", alpha = 0.5) +
#     method = "glm", se = FALSE) + 
  labs(y = "Survival", x = st.labs["albumin"]) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = strCol, labels = event.labels) + 
  scale_shape_manual(values = event.marks, labels = event.labels) + 
  facet_wrap(~bili_grp) +
  coord_cartesian(ylim = c(-0.01,1.01))

## ----build-bili-albumin, eval=FALSE,echo=TRUE-------------------------------------------
#  partial_coplot_pbc <- gg_partial_coplot(rfsrc_pbc, xvar = "bili",
#                                           groups = ggvar$albumin_grp,
#                                           surv_type = "surv",
#                                           time = 1,
#                                           show.plots = FALSE)

## ----load-pbc-coplot, echo=FALSE--------------------------------------------------------
# Load cached partial plot data
data("partial_coplot_pbc", package = "ggRandomForests")

## ----bili-albumin, fig.cap="Partial dependence coplot of survival at 1 year against \\code{bili}, conditional on \\code{albumin} interval group membership. Points estimates with loess smooth to indicate trend within each group.", fig.width=7, fig.height=4, echo=TRUE----
ggplot(partial_coplot_pbc, aes(x=bili, y=yhat, col=group, shape=group)) + #
  geom_smooth(se = FALSE) +
  labs(x = st.labs["bili"], y = "Survival at 1 year (%)", 
       color = "albumin", shape = "albumin") +
  coord_cartesian(y = c(49,101))

## ----albumin-bili, fig.cap="Partial dependence coplot of survival at 1 year against \\code{albumin}, conditional on \\code{bili} interval group membership. Points estimates with loess smooth to indicate trend within each group.", fig.width=7, fig.height=4, echo=FALSE----
# Load cached partial plot data
data("partial_coplot_pbc2", package = "ggRandomForests")

# Partial coplot
ggplot(partial_coplot_pbc2, aes(x=albumin, y=yhat, col=group, shape=group))+
  geom_smooth(se = FALSE) +
  labs(x = st.labs["albumin"], y = "Survival at 1 year (%)", 
       color = "Bilirubin", shape = "Bilirubin") +
  coord_cartesian(y = c(49,101))

## ----surface3d, fig.cap="Partial dependence surface of survival at 1 year as a funtion of \\code{bili} and \\code{albumin}. Blue lines indicate partial coplot cut points for \\code{albumin} (Figure~\\ref{fig:bili-albumin}) and \\code{bili} (Figure~\\ref{fig:albumin-bili}).", fig.width=7, fig.height=5, echo=FALSE----
# Find the quantile points to create 50 cut points
alb_partial_pts <-quantile_pts(ggvar$albumin, groups = 50)

# Load the stored partial coplot data.
data("partial_pbc_surf")

# Instead of groups, we want the raw albumin point values,
# To make the dimensions match, we need to repeat the values
# for each of the 50 points in the albumin direction
albumin.tmp <- do.call(c,lapply(alb_partial_pts, 
                                function(grp){rep(grp, 50)}))

# Convert the list of plot.variable output to 
partial_surf <- do.call(rbind,lapply(partial_pbc_surf, gg_partial))

# attach the data to the gg_partial_coplot
partial_surf$albumin <- albumin.tmp

# Modify the figure margins to make the figure larger
par(mai = c(0.5,.55,0,0))

# Transform the gg_partial_coplot object into a list of three named matrices
# for surface plotting with plot3D::surf3D
srf <- surface_matrix(partial_surf, c("bili", "albumin", "yhat"))

# Generate the figure.
surf3D(x = srf$x, y = srf$y, z = srf$z, col = topo.colors(25),
       colkey = FALSE, border = "black", bty = "b2", 
       shade = 0.5, expand = 0.5, theta=55, phi=15,
       lighting = TRUE, lphi = -50, ticktype="detailed",
       xlab = "Bilirubin", ylab = "Albumin", zlab = "Survival at 1 Year"
       )

# Extract the albumin and bilirubin points
# Remove end points
bli <- bili_cts[-c(1,7)]
alb <- albumin_cts[-c(1,7)]

# Find the indices of the points closest to split points
alb.pts <- lapply(alb, function(pt){min(abs(srf$y - pt), na.rm=TRUE)})
bli.pts <- lapply(bli, function(pt){min(abs(srf$x - pt), na.rm=TRUE)})

indx.alb <- lapply(1:length(alb.pts), function(al){
  which(abs(srf$y - alb[al]) < alb.pts[[al]]+1.e-5)})
indx.bli <- lapply(1:length(bli.pts), function(al){
  which(abs(srf$x - bli[al]) < bli.pts[[al]]+1.e-5)})

# Draw the lines
indx <- c(indx.alb, indx.bli)
st <- lapply(indx, function(ind){
  lines3D(x=srf$x[ind], 
          y=srf$y[ind],
          z=srf$z[ind],
          add=TRUE, col="blue", lwd=6)})

## ----vignette, eval=FALSE, echo=TRUE----------------------------------------------------
#  vignette("randomForestSRC-Survival", package = "ggRandomForests")

## ----src-listing-timeSurface, echo=TRUE, eval=FALSE-------------------------------------
#  # Restrict the time of interest to less than 5 years.
#  time_pts <- rfsrc_pbc$time.interest[which(rfsrc_pbc$time.interest<=5)]
#  
#  # Find the 50 points in time, evenly space along the distribution of
#  # event times for a series of partial dependence curves
#  time_cts <-quantile_pts(time_pts, groups = 50)
#  
#  # Load stored data from the package.
#  # See ?partial_pbc_time for how this data was generated.
#  #
#  # Time surfaces are created with the partial.rfsrc command
#  # partial_pbc_time <- partial.rfsrc(rfsrc_pbc, xvar = "bili",sav
#  #                                   npts = 50, show.plots = FALSE,
#  #                                   surv.type="surv")
#  #
#  load(partial_pbc_time, package="ggRandomForests")
#  
#  # We need to attach the time points of interest to our data.
#  time.tmp <- do.call(c,lapply(time_cts,
#                                 function(grp){rep(grp, 50)}))
#  
#  # Convert the list of plot.variable output to gg_partial
#  partial_time <- do.call(rbind,lapply(partial_pbc_time, gg_partial))
#  
#  # attach the time data to the gg_partial_coplot
#  partial_time$time <- time.tmp
#  
#  # Modify the figure margins to make it larger
#  par(mai = c(0,0.3,0,0))
#  
#  # Transform the gg_partial_coplot object into a list of three named matrices
#  # for surface plotting with plot3D::surf3D
#  srf <- surface_matrix(partial_time, c("time", "bili", "yhat"))
#  
#  # Generate the figure.
#  surf3D(x = srf$x, y = srf$y, z = srf$z, col = heat.colors(25),
#         colkey = FALSE, border = "black", bty = "b2",
#         shade = 0.5, expand = 0.5, theta=110,phi=15,
#         lighting = TRUE, lphi = -50,
#         ylab = "Bilirubin", xlab = "Time", zlab = "Survival"
#  )
#  
#  # Extract the 1 and 3 year points.
#  # Find the indices of the points closest in time
#  t.pts <- sapply(c(1,3), function(pt){min(abs(srf$x - pt), na.rm=TRUE)})
#  # Extract the 1 and 3 year points.
#  # Find the indices of the points closest in time
#  t.pts <- sapply(c(1,3), function(pt){min(abs(srf$x - pt), na.rm=TRUE)})
#  indx <- vector("list", length=2)
#  indx[[1]] <- which(abs(srf$x - 1) < t.pts[1]+1.e-5)
#  indx[[2]] <- which(abs(srf$x - 3) < t.pts[2]+1.e-5)
#  
#  # Generate curves along 1 and 3 year partial dependence
#  alt <- lapply(indx, function(ind){
#    lines3D(x=srf$x[ind], y=srf$y[ind],z=srf$z[ind],
#             add=TRUE, col="blue", lwd=6)
#    })

## ----src-listing-bilicoplot, echo=TRUE, eval=FALSE--------------------------------------
#  # Find intervals with similar number of observations.
#  bili_cts <-quantile_pts(ggvar$bili, groups = 6, intervals = TRUE)
#  
#  # We need to move the minimal value so we include that observation
#  bili_cts[1] <- bili_cts[1] - 1.e-7
#  
#  # Create the conditional groups and add to the gg_variable object
#  ggvar$bili_grp <- cut(ggvar$bili, breaks = bili_cts)
#  
#  # Adjust naming for facets
#  levels(ggvar$bili_grp) <- paste("bilirubin = ",levels(ggvar$bili_grp), sep = "")
#  
#  # plot.gg_variable
#  plot(ggvar[-which(is.na(ggvar$albumin)),], xvar = "albumin",
#                  method = "glm", alpha = 0.5, se = FALSE) +
#    labs(y = "Survival", x = st.labs["albumin"]) +
#    theme(legend.position = "none") +
#    scale_color_manual(values = strCol, labels = event.labels) +
#    scale_shape_manual(values = event.marks, labels = event.labels) +
#    facet_wrap(~bili_grp) +
#    coord_cartesian(ylim = c(-0.01, 1.01))

## ----albumin-bili-src, eval=FALSE,echo=TRUE---------------------------------------------
#  partial_coplot_pbc2 <- gg_partial_coplot(rfsrc_pbc, xvar = "albumin",
#                                           groups = bili_grp,
#                                           surv_type = "surv",
#                                           time = 1,
#                                           show.plots = FALSE)
#  
#  
#  # Stored in
#  # data(partial_coplot_pbc2, package = "ggRandomForests")
#  
#  plot(partial_coplot_pbc2, se = FALSE) +
#    labs(x = st.labs["albumin"], y = "Survival at 1 year (%)",
#         color = "Bilirubin", shape = "Bilirubin") +
#    scale_color_brewer(palette = "Set2") +
#    coord_cartesian(y = c(49,101))

## ----src-listing-variableSurface, echo=TRUE, eval=FALSE---------------------------------
#  # Find the quantile points to create 50 cut points
#  alb_partial_pts <-quantile_pts(ggvar$albumin, groups = 50)
#  
#  # Load the stored partial coplot data.
#  # See ?partial_pbc_surf for how this data was generated.
#  #
#  # partial_pbc_surf <- lapply(alb_partial_pts, function(ct){
#  #    rfsrc_pbc$xvar$albumin <- ct
#  #     plot.variable(rfsrc_pbc, xvar = "bili", time = 1,
#  #                   npts = 50, show.plots = FALSE,
#  #                   partial = TRUE, surv.type="surv")
#  # })
#  #
#  data("partial_pbc_surf")
#  
#  # Instead of groups, we want the raw albumin point values,
#  # To make the dimensions match, we need to repeat the values
#  # for each of the 50 points in the albumin direction
#  albumin.tmp <- do.call(c,lapply(alb_partial_pts,
#                                  function(grp){rep(grp, 50)}))
#  
#  # Convert the list of plot.variable output to
#  partial_surf <- do.call(rbind,lapply(partial_pbc_surf, gg_partial))
#  
#  # attach the data to the gg_partial_coplot
#  partial_surf$albumin <- albumin.tmp
#  
#  # Modify the figure margins to make the figure larger
#  par(mai = c(0,.3,0,0))
#  
#  # Transform the gg_partial_coplot object into a list of three named matrices
#  # for surface plotting with plot3D::surf3D
#  srf <- surface_matrix(partial_surf, c("bili", "albumin", "yhat"))
#  
#  # Generate the figure.
#  surf3D(x = srf$x, y = srf$y, z = srf$z, col = topo.colors(25),
#         colkey = FALSE, border = "black", bty = "b2",
#         shade = 0.5, expand = 0.5, theta=55, phi=15,
#         lighting = TRUE, lphi = -50,
#         xlab = "Bilirubin", ylab = "Albumin", zlab = "Survival at 1 Year"
#         )
#  
#  # Extract the albumin and bilirubin points
#  # Remove end points
#  bli <- bili_cts[-c(1,7)]
#  alb <- albumin_cts[-c(1,7)]
#  
#  # Find the indices of the points closest to split points
#  alb.pts <- lapply(alb, function(pt){min(abs(srf$y - pt), na.rm=TRUE)})
#  bli.pts <- lapply(bli, function(pt){min(abs(srf$x - pt), na.rm=TRUE)})
#  
#  indx.alb <- lapply(1:length(alb.pts), function(al){
#    which(abs(srf$y - alb[al]) < alb.pts[[al]]+1.e-5)})
#  indx.bli <- lapply(1:length(bli.pts), function(al){
#    which(abs(srf$x - bli[al]) < bli.pts[[al]]+1.e-5)})
#  
#  # Draw the lines
#  indx <- c(indx.alb, indx.bli)
#  st <- lapply(indx, function(ind){
#    lines3D(x=srf$x[ind],
#            y=srf$y[ind],
#            z=srf$z[ind],
#            add=TRUE, col="blue", lwd=6)})

