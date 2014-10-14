#' \code{randomForestSRC::var.select} minimal depth variable selection from the Iris dataset.
#' 
#' @description A minimal depth variable selection object constructed by the 
#' \code{randomForestSRC::var.select} function for the \code{randomForestSRC::rfsrc} classification
#' forest for the Iris data set.
#' 
#' This famous (Fisher's or Anderson's) iris data set gives the 
#' measurements in centimeters of the variables sepal length and width and 
#' petal length and width, respectively, for 50 flowers from each of 3 species 
#' of iris. The species are Iris setosa, versicolor, and virginica.
#' 
#' iris is a data frame with 150 cases (rows) and 5 variables (columns) 
#' named Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species.
#' 
#' @seealso \code{randomForestSRC::var.select} \code{randomForestSRC::rfsrc} \code{iris}
#' 
#' @examples
#' \dontrun{
#' iris_vs <- var.select(Species ~ ., iris)
#' 
#' plot.gg_interaction(iris_vs)
#' }
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
#' @docType data
#' @keywords datasets
#' @format A \code{randomForestSRC::var.select} object for the iris classification random forest 
#' @name iris_vs
NULL
