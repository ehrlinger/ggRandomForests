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
               echo = FALSE, # Change this to TRUE if you want to see all the code examples
               results = FALSE, 
               message = FALSE, 
               warning = FALSE, 
               error = FALSE, 
               dev = 'pdf', prompt = TRUE)

# Setup the R environment
options(object.size = Inf, expressions = 100000, memory = Inf, 
        replace.assign = TRUE, width = 90, prompt = "R> ")
options(mc.cores = 1, rf.cores = 0, stringsAsFactors = FALSE)

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
fct <- setdiff(1:ncol(pbc), cnt) # The complement of numeric/integers.
fct <- c(fct, which(colnames(pbc) == "years"))
dta <- suppressWarnings(gather(pbc[,fct], variable, value, -years))

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
ggplot(dta %>% filter(!is.na(value)), 
       aes(x = years, y = value, color = status, shape = status)) +
  geom_point(alpha = 0.4) +
  geom_rug(data = dta[which(is.na(dta$value)),], color = "grey50") +
  labs(y = "", x = st.labs["years"], color = "Death", shape = "Death") +
  scale_color_manual(values = strCol) +
  scale_shape_manual(values = event.marks) +
  facet_wrap(~variable, scales = "free_y", ncol = 4) +
  theme(legend.position = c(0.8, 0.2))

## ----missing, results="asis"-------------------------------------------------
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

## ----gg_survival, echo=TRUE--------------------------------------------------
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

## ----xtab, results="asis"----------------------------------------------------
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

## ----rfsrc, echo=TRUE, eval=FALSE--------------------------------------------
#  rfsrc_pbc <- rfsrc(Surv(years, status) ~ ., data = pbc.trial,
#                     nsplit = 10, na.action = "na.impute")

