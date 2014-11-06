ggRandomForests Package
========================================================
[![DOI](https://zenodo.org/badge/5745/ehrlinger/ggRandomForests.png)](http://dx.doi.org/10.5281/zenodo.11526)

[![Build Status](https://travis-ci.org/ehrlinger/ggRandomForests.svg?branch=master)](https://travis-ci.org/ehrlinger/ggRandomForests)

Main Goal: Simplify graphical exploration of randomForests.

ggRandomForests is designed to be used with the randomForestSRC (RF-SRC) package. 
The ggRandomForests package is meant to replace and extend the plots already created 
by the randomForestSRC package. 

The package has two main design elements :

* Extract data.frame objects for analytics from a randomForest[SRC] model. 
* Encapsulate the generation of graphic elements from these data frames.

We have chosen to use the ggplot2 package for our graphics, as it allows the user to easily modify the graphic output in an intuitive manner. 

We include a series of functions for working  with rfsrc objects:
* gg_rfsrc Prediction results from the randomForest[SRC] object 
* gg_error randomForest[SRC] convergence as OOB error rate stability
* gg_roc Receiver Operator Charactertic curves for classification forests
* gg_vimp Variable importance ranking for variable selection
* gg_minimal_depth Minimal Depth ranking variable selection
* gg_minimal_vimp Minimal depth vs VIMP camparison by variable rank
* gg_variable marginal variable dependence 
* gg_partial partial variable dependence
* gg_interaction Reporting minimal depth based variable interactions

We use the S3 object model to provide plot methods each of these objects. 

## References
Ishwaran H. and Kogalur U.B. (2014). Random Forests for Survival, Regression and Classification (RF-SRC), R package version 1.5.

Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R. R News 7(2), 25--31.

Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S. (2008). Random survival forests. Ann. Appl. Statist. 2(3), 841--860.


Friedman J.H. (2001). Greedy function approximation: a gradient boosting machine, Ann. of Statist., 5:1189-1232.

Ishwaran H., Gerds, T.A. Kogalur U.B., Moore R.D., Gange S.J. and Lau B.M. (2012). Random survival forests for competing risks.

H. Wickham. ggplot2: elegant graphics for data analysis. Springer New York, 2009.