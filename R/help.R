###############################################################################
# Package documentation
###############################################################################
#' @title ggRandomForests: Visually Exploring Random Forests
#' 
#' @description \code{ggRandomForests} is a utility package for \code{randomForestSRC} 
#' (Iswaran et.al. 2014, 2008, 2007) for survival, regression and  
#' classification forests and uses the \code{ggplot2} (Wickham 2009) package for plotting results. 
#' \code{ggRandomForests} is structured to extract data objects from the random forest
#' and provides S3 functions for printing and plotting these objects.
#' 
#' The \code{randomForestSRC} package provides a unified treatment of Breiman's (2001) random 
#' forests for a variety of data settings. Regression and 
#' classification forests are grown when the response is numeric or categorical 
#' (factor) while survival and competing risk forests (Ishwaran et al. 2008, 2012) 
#' are grown for right-censored survival data.
#' 
#' Many of the figures created by the \code{ggRandomForests} package are also available 
#' directly from within the \code{randomForestSRC} package. However, \code{ggRandomForests} offers the 
#' following advantages:
#' \itemize{
#' \item Separation of data and figures: \code{ggRandomForest} contains functions that 
#' operate on either the \code{\link[randomForestSRC]{rfsrc}} forest object directly, or on 
#' the output from \code{randomForestSRC} post processing functions (i.e. 
#' \code{plot.variable}, 
#' \code{var.select}, 
#' \code{find.interaction}) to 
#' generate intermediate \code{ggRandomForests} data objects. S3 functions are provide to
#' further process these objects and plot results using the \code{ggplot2} graphics package.
#' Alternatively, users can use these data objects for additional custom plotting or 
#' analysis operations.
#' 
#' \item Each data object/figure is a single, self contained object. This allows simple
#' modification and manipulation of the data or \code{ggplot2} objects to meet users specific 
#' needs and requirements. 
#' 
#' \item The use of \code{ggplot2} for plotting. We chose to use the \code{ggplot2} package 
#' for our figures to allow users flexibility in modifying the figures to their liking. Each S3
#' plot function returns either a single \code{ggplot2} object, or a \code{list} of 
#'  \code{ggplot2} objects, allowing users to use additional \code{ggplot2} functions or themes
#'  to modify and customise the figures to their liking. 
#' }
#'
#' The \code{ggRandomForests} package contains the following data functions:
#' \itemize{
#' \item \code{\link{gg_rfsrc}}: randomForest[SRC] predictions.
#' \item \code{\link{gg_error}}: randomForest[SRC] convergence rate based on the OOB error rate.
#' \item \code{\link{gg_roc}}: ROC curves for randomForest classification models.
#' \item \code{\link{gg_vimp}}: Variable Importance ranking for variable selection.
#' \item \code{\link{gg_minimal_depth}}: Minimal Depth ranking for variable selection 
#' (Ishwaran et.al. 2010).
#' \item \code{\link{gg_minimal_vimp}}: Comparing Minimal Depth and VIMP rankings for variable
#'  selection.
#' \item \code{\link{gg_interaction}}: Minimal Depth interaction detection (Ishwaran et.al. 2010)
#' \item \code{\link{gg_variable}}: Marginal variable dependence.
#' \item \code{\link{gg_partial}}: Partial (risk adjusted) variable dependence.
#' \item \code{\link{gg_partial_coplot}}: Partial variable conditional dependence 
#' (computationally expensive).
#' \item \code{\link{gg_survival}}: Kaplan-Meier/Nelson-Aalon hazard analysis.  
#' }
#' 
#' Each of these data functions has an associated S3 plot function that returns \code{ggplot2} objects, either 
#' individually or as a list, which can be further customised using standard \code{ggplot2} commands. 
#'  
#' @references
#' Breiman, L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2014). Random Forests for Survival,
#' Regression and Classification (RF-SRC), R package version 1.5.5.12.
#'
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R. R News
#' 7(2), 25--31.
#'
#' Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S. (2008). Random
#' survival forests. Ann. Appl. Statist. 2(3), 841--860.
#' 
#' Ishwaran, H., U. B. Kogalur, E. Z. Gorodeski, A. J. Minn, and M. S. Lauer (2010). 
#' High-dimensional variable selection for survival data. J. Amer. Statist. Assoc. 
#' 105, 205-217.
#' 
#' Ishwaran, H. (2007). Variable importance in binary regression trees and forests. 
#' Electronic J. Statist., 1, 519-537.
#' 
#' Wickham, H. ggplot2: elegant graphics for data analysis. Springer New York, 2009.
#' 
#' @docType package
#' @name ggRandomForests-package
#' 
################
NULL
