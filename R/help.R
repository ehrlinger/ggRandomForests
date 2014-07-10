###############################################################################
# Package documentation
###############################################################################
#' @title ggRandomForests package for plotting of random forest objects using 
#' the \code{ggplot2} package.
#' 
#' @description ggRandomForests is an add on package for \code{randomForestSRC}. 
#' The package is designed to simplify the graphical analysis and exploration 
#' of randomForests.   
#' 
#' The randomForestSRC package provides a unified treatment of Breiman's random 
#' forests (Breiman 2001) for a variety of data settings. Regression and 
#' classification forests are grown when the response is numeric or categorical 
#' (factor) while survival and competing risk forests (Ishwaran et al. 2008, 2012) 
#' are grown for right-censored survival data. Support for unsupervised and 
#' multivariate randomForests have also recently been added.
#' 
#' Many of the features of the ggRandomForests package are available 
#' within the randomForestSRC package. However, the ggRandomForests offers the 
#' following advantages:
#' \itemize{
#' \item Separation of data and figures: ggRandomForest contains functions that 
#' operate on either the raw randomForestSRC (rfsrc) forest object directly, or on 
#' the output from rfsrc posprocessing functions (i.e. \code{vimp.rfsrc}, 
#' \code{var.select.rfsrc}, \code{plot.variable.rfsrc}) to generate intermediate 
#' data.frame objects. These objects are then passed to corresponding plot functions 
#' using the S3 object model. Alternatively, the user can use these data object for 
#' additional external, custom plotting or analysis operations.
#' 
#' 
#' \item ggplot2 figures: We chose to use the \code{ggplot2} package for our figures. 
#'  The plot functions all return either a single ggplot2 object, or a list of ggplot2 object. 
#'  The user can then use additional \code{ggplot2} functions to modify and customise the 
#'  figures to their liking. 
#' }
#'
#' The ggRandomForests package contains the following functions:
#' \itemize{
#' \item ggRFsrc: randomForest[SRC] prediction
#' \item ggError: randomForest[SRC] convergence rate based on the OOB error rate.
#' \item ggROC: ROC curves for randomForest classification models.
#' \item ggVimp: Variable Importance ranking for variable selection
#' \item ggMinimalDepth: Minimal Depth ranking for variable selection
#' \item ggInteractions: Minimal Depth interaction detection (under development)
#' \item ggVariable: Marginal variable dependence (including conditional dependence)
#' \item ggPartial: Partial variable dependence  (including conditional partial dependence)
#' \item ggSurvival: Non-parameteric Kaplan-Meier and Nelson-Aalon survival estimates  
#' }
#' 
#' All functions have an associated plotting function that returns ggplot2 graphics, either 
#' individually or as a list, that can be further customised using standard ggplot2 commands.
#'  
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, 
#' Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2014). Random Forests for Survival, 
#' Regression and Classification (RF-SRC), R package version 1.5.
#' 
#' Ishwaran, H., U. B. Kogalur, E. Z. Gorodeski, A. J. Minn, and M. S. Lauer (2010). 
#' High-dimensional variable selection for survival data. J. Amer. Statist. Assoc. 
#' 105, 205–217.
#' 
#' Ishwaran H. (2007). Variable importance in binary regression trees and forests. 
#' Electronic J. Statist., 1, 519-537.
#' 
#' H. Wickham. ggplot2: elegant graphics for data analysis. Springer New York, 2009.
#' 


#' @docType package
#' @name ggRandomForests
#' 
################
NULL
