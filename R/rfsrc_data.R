#' Cached \code{\link[randomForestSRC]{rfsrc}} objects for examples, diagnostics and vignettes.
#'  
#' Data sets storing \code{\link[randomForestSRC]{rfsrc}} objects corresponding to 
#' training data according to the following naming convention: 
#'\itemize{
#' \item \code{rfsrc_iris} - randomForestSR[C] for the \code{iris} data set. 
#' \item \code{rfsrc_Boston} - randomForestS[R]C for the \code{Boston} housing 
#' data set (\code{MASS} package).
#' \item \code{rfsrc_pbc} - randomForest[S]RC for the \code{pbc} data set
#'  (\code{randomForestSRC} package)  
#' }
#'   
#' @details 
#' Constructing random forests are computationally expensive.
#' We cache \code{\link[randomForestSRC]{rfsrc}} objects to improve the \code{ggRandomForests} 
#' examples, diagnostics and vignettes run times. 
#' (see \code{\link{rfsrc_cache_datasets}} to rebuild a complete set of these data sets.)
#' 
#' For each data set listed, we build a \code{\link[randomForestSRC]{rfsrc}}. Tuning parameters used
#' in each case are documented in the examples. Each data set is built with the 
#' \code{\link{rfsrc_cache_datasets}} with the \code{randomForestSRC} version listed
#' in the \code{ggRandomForests} DESCRIPTION file.
#' 
#' \itemize{
#' \item \code{rfsrc_iris} - The famous (Fisher's or Anderson's) \code{iris} data set gives 
#' the measurements in centimeters of the variables sepal length and width and 
#' petal length and width, respectively, for 50 flowers from each of 3 species 
#' of iris. Build a classification random forest for predicting the species (setosa, 
#' versicolor, and virginica) on 5 variables (columns) and 150 observations (rows).
#' 
#' \item \code{rfsrc_Boston} - The \code{Boston} housing values in suburbs of Boston from the
#' \code{MASS} package. Build a regression random forest for predicting medv (median home 
#' values) on 13 covariates and 506 observations. 
#' 
#' \item \code{rfsrc_pbc} - The \code{pbc} data from the Mayo Clinic trial in primary biliary 
#' cirrhosis (PBC) of the liver conducted between 1974 and 1984. A total of 424 PBC patients, 
#' referred to Mayo Clinic during that ten-year interval, met eligibility criteria for the 
#' randomized placebo controlled trial of the drug D-penicillamine. 312 cases participated in 
#' the randomized trial and contain largely complete data. Data from the \code{randomForestSRC}
#' package. Build a survival random forest for time-to-event death data with 17 covariates and 
#' 312 observations (remaining 106 observations are held out).
#' }
#' 
#' @seealso \code{iris} \code{\link[MASS]{Boston}}
#' \code{\link[randomForestSRC]{pbc}} 
#'  \code{\link[randomForestSRC]{rfsrc}}
#'  \code{\link{rfsrc_cache_datasets}} 
#'  \code{\link{gg_rfsrc}} 
#'  \code{\link{plot.gg_rfsrc}} 
#'  \code{\link{gg_error}} 
#'  \code{\link{plot.gg_error}}
#'
#' @examples
#' \dontrun{
#' #---------------------------------------------------------------------
#' # iris data - classification random forest
#' #---------------------------------------------------------------------
#' # rfsrc grow call
#' rfsrc_iris <- rfsrc(Species ~., data = iris)
#' 
#' # plot the forest generalization error convergence
#' gg_dta <- gg_error(rfsrc_iris)
#' plot(gg_dta)
#' 
#' # Plot the forest predictions
#' gg_dta <- gg_rfsrc(rfsrc_iris)
#' plot(gg_dta)
#' 
#' #---------------------------------------------------------------------
#' # MASS::Boston data - regression random forest 
#' #---------------------------------------------------------------------
#' # Load the data...
#' data(Boston, package="MASS")
#' Boston$chas <- as.logical(Boston$chas)
#' 
#' # rfsrc grow call
#' rfsrc_Boston <- rfsrc(medv~., data=Boston)
#' 
#' # plot the forest generalization error convergence
#' gg_dta <- gg_error(rfsrc_Boston)
#' plot(gg_dta)
#' 
#' # Plot the forest predictions
#' gg_dta <- gg_rfsrc(rfsrc_Boston)
#' plot(gg_dta)
#' 
#' #---------------------------------------------------------------------
#' # randomForestSRC::pbc data - survival random forest
#' #---------------------------------------------------------------------
#' # Load the data...
#' # For simplicity here. We do a bit of data tidying
#' # before running the stored random forest. 
#' data(pbc, package="randomForestSRC")
#'
#' # Remove non-randomized cases
#' dta.train <- pbc[-which(is.na(pbc$treatment)),]
#' 
#' # rfsrc grow call
#' rfsrc_pbc <- rfsrc(Surv(years, status) ~ ., dta.train, nsplit = 10,
#'                    na.action="na.impute")
#'                    
#' # plot the forest generalization error convergence
#' gg_dta <- gg_error(rfsrc_pbc)
#' plot(gg_dta)
#' 
#' # Plot the forest predictions
#' gg_dta <- gg_rfsrc(rfsrc_pbc)
#' plot(gg_dta)
#'   
#' }
#' 
#' @references 
#' #---------------------
#'  randomForestSRC
#' ---------------------
#' 
#' Ishwaran H. and Kogalur U.B. (2014). Random Forests for
#' Survival, Regression and Classification (RF-SRC), R package
#' version 1.5.5.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests
#' for R. R News 7(2), 25-31.
#' 
#' Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S.
#' (2008). Random survival forests. Ann. Appl. Statist. 2(3),
#' 841-860.
#' 
#' #---------------------
#'  Boston data set
#' ---------------------
#' 
#'  Belsley, D.A., E. Kuh, and R.E. Welsch. 1980. Regression Diagnostics. Identifying 
#'  Influential Data and Sources of Collinearity. New York: Wiley.
#'  
#' Harrison, D., and D.L. Rubinfeld. 1978. "Hedonic Prices and the Demand for Clean Air."
#'  J. Environ. Economics and Management 5: 81-102.
#' 
#' #---------------------
#'  Iris data set
#' ---------------------
#' 
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. 
#' Wadsworth \& Brooks/Cole. (has iris3 as iris.)
#' 
#' Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. 
#' Annals of Eugenics, 7, Part II, 179-188.
#' 
#' Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin 
#' of the American Iris Society, 59, 2-5.
#' 
#' #---------------------
#'  pbc data set
#' ---------------------
#' 
#' Flemming T.R and Harrington D.P., (1991) Counting Processes and Survival Analysis. 
#' New York: Wiley.
#' 
#' T Therneau and P Grambsch (2000), Modeling Survival Data: Extending the Cox Model, 
#' Springer-Verlag, New York. ISBN: 0-387-98784-3.
#' 
#' @aliases rfsrc_data rfsrc_iris rfsrc_Boston rfsrc_pbc rfsrc_pbc_test
#' @docType data
#' @keywords datasets
#' @format \code{\link[randomForestSRC]{rfsrc}} object
#' @name rfsrc_data
#' @name rfsrc_iris
#' @name rfsrc_Boston
#' @name rfsrc_pbc
#' @name rfsrc_pbc_test
NULL
