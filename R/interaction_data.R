#' Cached \code{\link[randomForestSRC]{find.interaction}} matrix objects for examples, 
#' diagnostics and vignettes.
#'  
#' Data sets storing \code{\link[randomForestSRC]{find.interaction}} matrix objects corresponding to 
#' training data according to the following naming convention: 
#'\itemize{
#' \item \code{interaction_iris} - from a randomForestSR[C] for the \code{iris} data set. 
#' \item \code{interaction_Boston} - from a randomForestS[R]C for the \code{Boston} housing 
#' data set (\code{MASS} package).
#' \item \code{interaction_pbc} - from a randomForest[S]RC for the \code{pbc} data set
#'  (\code{randomForestSRC} package)  
#' }
#'   
#' @details 
#' Constructing the minimal depth interaction matrices on randomForestsSRC objects are 
#' computationally expensive. We cache \code{\link[randomForestSRC]{find.interaction}} matrix objects
#' to improve the \code{ggRandomForests} examples, diagnostics and vignettes run times. 
#' (see \code{\link{rfsrc_cache_datasets}} to rebuild a complete set of these data sets.)
#' 
#' For each data set listed, we build a \code{\link[randomForestSRC]{rfsrc}} 
#' (see \code{\link{rfsrc_data}}), then calculate the minimal depth variable interaction 
#' table with \code{\link[randomForestSRC]{find.interaction}}. Each data set is built with the 
#' \code{\link{rfsrc_cache_datasets}} with the \code{randomForestSRC} version listed
#' in the \code{ggRandomForests} DESCRIPTION file.
#' 
#' \itemize{
#' \item \code{interaction_iris} - The famous (Fisher's or Anderson's) \code{iris} data set gives 
#' the measurements in centimeters of the variables sepal length and width and 
#' petal length and width, respectively, for 50 flowers from each of 3 species 
#' of iris. Build a classification random forest for predicting the species (setosa, 
#' versicolor, and virginica) on 5 variables (columns) and 150 observations (rows).
#' 
#' \item \code{interaction_airq} - The \code{airquality} data set is from the New York State 
#' Department of Conservation (ozone data) and the National Weather Service 
#' (meteorological data) collected in New York, from May to September 1973. Build regression 
#' random forest for predicting \code{Ozone} on 5 covariates and 153 observations.
#' 
#' \item \code{interaction_mtcars} - The \code{mtcars} data was extracted from the 1974 Motor 
#' Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and 
#' performance for 32 automobiles (1973-74 models). Build a regression random forest for
#' predicting mpg on 10 covariates and 32 observations.
#' 
#' \item \code{interaction_Boston} - The \code{Boston} housing values in suburbs of Boston from the
#' \code{MASS} package. Build a regression random forest for predicting medv (median home 
#' values) on 13 covariates and 506 observations. 
#' 
#' \item \code{interaction_pbc} - The \code{pbc} data from the Mayo Clinic trial in primary biliary 
#' cirrhosis (PBC) of the liver conducted between 1974 and 1984. A total of 424 PBC patients, 
#' referred to Mayo Clinic during that ten-year interval, met eligibility criteria for the 
#' randomized placebo controlled trial of the drug D-penicillamine. 312 cases participated in 
#' the randomized trial and contain largely complete data. Data from the \code{randomForestSRC}
#' package. Build a survival random forest for time-to-event death data with 17 covariates and 
#' 312 observations (remaining 106 observations are held out).
#' 
#' \item \code{interaction_veteran} - Veteran's Administration randomized trial of two treatment 
#' regimens for lung cancer. Build a survival random forest for time-to-event death data 
#' with 6 covariates and 137 observations.
#' }
#' 
#' @seealso \code{iris} \code{\link[MASS]{Boston}}
#' \code{\link[randomForestSRC]{pbc}}
#'  \code{\link[randomForestSRC]{find.interaction}}
#'  \code{\link{rfsrc_data}}
#'  \code{\link{rfsrc_cache_datasets}} 
#'  \code{\link{gg_interaction}} 
#'  \code{\link{plot.gg_interaction}}
#'  
#' @examples
#' \dontrun{
#' #---------------------------------------------------------------------
#' # iris data - classification random forest
#' #---------------------------------------------------------------------
#' # load the rfsrc object from the cached data
#' data(rfsrc_iris, package="ggRandomForests")
#' 
#' # The interaction table 
#' interaction_iris <- find.interaction(rfsrc_iris)
#' 
#' # plot the forest interaction table
#' gg_dta <- gg_interaction(interaction_iris)
#' plot(gg_dta, panel=TRUE)
#' 
#' #---------------------------------------------------------------------
#' # MASS::Boston data - regression random forest 
#' #---------------------------------------------------------------------
#' # load the rfsrc object from the cached data
#' data(rfsrc_Boston, package="ggRandomForests")
#' 
#' # The interaction table 
#' interaction_Boston <- find.interaction(rfsrc_Boston)
#' 
#' # plot the forest interaction table
#' gg_dta <- gg_interaction(interaction_Boston)
#' plot(gg_dta, panel=TRUE)
#' 
#' #---------------------------------------------------------------------
#' # randomForestSRC::pbc data - survival random forest
#' #---------------------------------------------------------------------
#' # load the rfsrc object from the cached data
#' data(rfsrc_pbc, package="ggRandomForests")
#' 
#' # The interaction table 
#' interaction_pbc <- find.interaction(rfsrc_pbc)
#' 
#' # plot the forest interaction table
#' gg_dta <- gg_interaction(interaction_pbc)
#' plot(gg_dta, panel=TRUE)
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
#' @aliases interaction_data interaction_iris interaction_Boston interaction_pbc
#' @docType data
#' @keywords datasets
#' @format \code{\link[randomForestSRC]{find.interaction}} matrix
#' @name interaction_data
#' @name interaction_iris
#' @name interaction_Boston
#' @name interaction_pbc
#' 
NULL
