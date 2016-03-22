ggRandomForests: Visually Exploring Random Forests
========================================================
[![DOI](https://zenodo.org/badge/5745/ehrlinger/ggRandomForests.png)](http://dx.doi.org/10.5281/zenodo.11526)
[![CRAN version](http://www.r-pkg.org/badges/version/ggRandomForests)](http://cran.r-project.org/web/packages/ggRandomForests)
![cranlogs](http://cranlogs.r-pkg.org./badges/ggRandomForests)

![active](http://www.repostatus.org/badges/latest/active.svg)
[![Build Status](https://travis-ci.org/ehrlinger/ggRandomForests.svg?branch=master)](https://travis-ci.org/ehrlinger/ggRandomForests)
[![Coverage Status](https://coveralls.io/repos/ehrlinger/ggRandomForests/badge.svg?branch=master&service=github)](https://coveralls.io/github/ehrlinger/ggRandomForests?branch=master)

[ggRandomForests](http://CRAN.R-project.org/package=ggRandomForests)  will help uncover variable associations in the random forests models. The package is designed for use with the [randomForestSRC](http://CRAN.R-project.org/package=randomForestSRC) package (Iswaran et.al. 2014, 2008, 2007) for survival, regression and classification random forests or the [randomForest](http://CRAN.R-project.org/package=randomForest) package (A. Liaw and M. Wiener 2002) and uses the [ggplot2](http://CRAN.R-project.org/package=ggplot2) package (Wickham 2009) for plotting diagnostic and variable association results. [ggRandomForests](http://CRAN.R-project.org/package=ggRandomForests) is  structured to extract data objects from [randomForestSRC](http://CRAN.R-project.org/package=randomForestSRC) or[randomForest](http://CRAN.R-project.org/package=randomForest) objects and provides S3 functions for printing and plotting these objects.
 
The [randomForestSRC](http://CRAN.R-project.org/package=randomForestSRC) package provides a unified treatment of Breiman's (2001) random forests for a variety of data settings. Regression and classification forests are grown when the response is numeric or categorical (factor) while survival and competing risk forests (Ishwaran et al. 2008, 2012) are grown for right-censored survival data.

Many of the figures created by the `ggRandomForests` package are also available directly from within the `randomForestSRC` package. However, `ggRandomForests` offers the following advantages:

 * Separation of data and figures: `ggRandomForests` contains functions that operate on either the `randomForestSRC::rfsrc` forest object directly, or on the output from `randomForestSRC` post processing functions (i.e. `plot.variable`, `var.select`,  `find.interaction`) to generate intermediate `ggRandomForests` data objects. S3 functions are provide to further process these objects and plot results using the `ggplot2` graphics package. Alternatively, users can use these data objects for additional custom plotting or analysis operations.

 * Each data object/figure is a single, self contained object. This allows simple modification and manipulation of the data or `ggplot2` objects to meet users specific needs and requirements. 

 * The use of `ggplot2` for plotting. We chose to use the `ggplot2` package for our figures to allow users flexibility in modifying the figures to their liking. Each S3 plot function returns either a single `ggplot2` object, or a `list` of `ggplot2` objects, allowing users to use additional `ggplot2` functions or themes to modify and customize the figures to their liking. 

The package has recently been extended for Breiman and Cutler's Random Forests for Classification and
Regression package [randomForest](http://CRAN.R-project.org/package=randomForest) where possible. Though methods have been provided for all `gg_*` functions, the unsupported functions will return an error message indicating where support is still lacking.

## References

Breiman, L. (2001). Random forests, Machine Learning, 45:5-32.

Ishwaran H. and Kogalur U.B. (2014). Random Forests for Survival,
Regression and Classification (RF-SRC), R package version 1.5.5.

Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R. R News
7(2), 25--31.

Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S. (2008). Random
survival forests. Ann. Appl. Statist. 2(3), 841--860.

A. Liaw and M. Wiener (2002). Classification and Regression by randomForest. R News 2(3), 18--22.

Wickham, H. ggplot2: elegant graphics for data analysis. Springer New York, 2009.


