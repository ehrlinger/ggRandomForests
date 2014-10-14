#' Edgar Anderson's Iris Data
#' 
#' 
#' A cached object constructed from the \code{randomForestSRC::find.interaction} function 
#' for the Iris data set. The randomForestSRC regression forest is stored in the
#'  \code{\link{iris_rf}} object.
#'  
#' @details For ggRandomForests examples and tests, as well as streamlining the 
#' R CMD CHECK for package release, we cache the computationally expensive operations
#' from the randomForestSRC package. 
#' 
#' To test the interaction plots, we build a regression randomForest (\code{\link{iris_rf}}) 
#' with the \code{iris} measurements data, then run the \code{randomForestSRC::find.interaction} 
#' function to determine pairwise variable interaction measures. 
#' 
#' The iris_interation "data set" is a cache of the \code{randomForestSRC::find.interaction} function, which 
#' measures pairwise interactions between variables from the \code{\link{iris_rf}} random 
#' forest model.
#' 
#' This famous (Fisher's or Anderson's) iris data set gives the 
#' measurements in centimeters of the variables sepal length and width and 
#' petal length and width, respectively, for 50 flowers from each of 3 species 
#' of iris. The species are Iris setosa, versicolor, and virginica.
#' 
#' iris is a data frame with 150 cases (rows) and 5 variables (columns) 
#' named Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species.
#' 
#' @references 
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. 
#' Wadsworth \& Brooks/Cole. (has iris3 as iris.)
#' 
#' Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. 
#' Annals of Eugenics, 7, Part II, 179-188.
#' 
#' Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin 
#' of the American Iris Society, 59, 2-5.
#' 
#' Ishwaran H. and Kogalur U.B. (2014). Random Forests for
#' Survival, Regression and Classification (RF-SRC), R package
#' version 1.5.4.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests
#' for R. R News 7(2), 25-31.
#' 
#' Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S.
#' (2008). Random survival forests. Ann. Appl. Statist. 2(3),
#' 841-860.
#' 
#' @examples
#' \dontrun{
#' load(iris_rf, package="ggRandomForests)
#' iris_interaction <- find.interaction(iris_rf)
#' 
#' plot(iris_interaction)
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format rdata matrix from \code{randomForestSRC::find.interaction}
#' @name iris_interaction
NULL
