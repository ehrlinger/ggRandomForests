## ----setup,include=FALSE, cache=FALSE, echo=FALSE-------------------------------------------------
#' @import RColorBrewer devtools gridExtra reporttools scales xtable

library(knitr)
# set global chunk options for knitr. These can be changed in the header for each individual R code chunk
opts_chunk$set(fig.path='figure/beamer-', 
               fig.align='center',
               fig.pos="!htpb",
               fig.show='hold', 
               size='footnotesize',
               comment="", echo=FALSE, results=FALSE, message=FALSE, warnings=FALSE,
               error=FALSE, dev='pdf')

# Setup the R environment
options(replace.assign=TRUE,object.size=Inf,expressions=100000,memory=Inf, width=100)

#################
# Load_packages #
#################
library(ggplot2) # Graphics engine for generating all types of plots
library(reshape2) # Used to modify the data for plotting
library(gridExtra) # for combined ggplots
library(RColorBrewer) # Color schemes
library(scales) # For modifying ggplot

library(xtable) # Make tables in latex
library(reporttools) # for descriptive stats

library(dplyr) # Better data manipulations

library(parallel)
options(mc.cores = detectCores()-1, rf.cores=detectCores()-1)

#library(devtools)
#load_all("../ggRandomForests/")
library(ggRandomForests)

# Analysis packages.
library(randomForestSRC) 

#########################################################################
# Default computation settings
#########################################################################

## ----echo = FALSE---------------------------------------------------------------------------------

## save initial options
o1 <- getOption("prompt")
o2 <- getOption("continue")
o3 <- getOption("width")
o4 <- getOption("digits")
options(prompt = "R> ", continue = "+  ", width = 100, digits = 4)

## ----echo=FALSE-----------------------------------------------------------------------------------
## re-specify initial options
options(prompt = o1, continue = o2, width = o3, digits = o4)

