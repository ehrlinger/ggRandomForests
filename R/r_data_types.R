####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' r_data_types infers correct data classes for each column in a data.frame 
#'
#' @description Read an input data set and infer the correct columnar r data 
#' types. Includes numeric, logical or factor type of the data columns
#'
#' @param dataset a dataframe to modify
#' @param factor_size numeric features may only have a small number of unique 
#' values. Where should we turn these into factors.
#' 
#' @return A \code{data.frame} with datatypes logically encoded. 
#' @importFrom dplyr mutate across where n_distinct
#' @examples
#'
#' ## ------------------------------------------------------------
#' ## -------- pbc data
#' # We need to create this dataset
#' data(pbc, package = "randomForestSRC" )
#' 
#' # Show data types for each column
#' sapply(pbc,class)
#' 
#' # Correct types
#' pbc <- r_data_types(pbc)
#' sapply(pbc,class)
#' 
#' @export
r_data_types = function(dataset, factor_size = 10) {
  ## Make sure NA is correctly encoded
  new_data = dataset
  new_data <-
    new_data |> mutate(across(where(is.character), ~ na_if(., "na")))
  new_data <-
    new_data |> mutate(across(where(is.character), ~ na_if(., "NA")))
  
  ##                   Auto encode logicals and factors
  ## Set modes correctly. For binary variables: transform to logical
  ## Check for range of 0,1, NA
  new_data <- new_data |>
    mutate(across(where(\(x) n_distinct(x, na.rm = TRUE) < 3),
                  ~ as.logical(.)))
  
  # Convert character features to factors
  new_data <- new_data |> mutate(across(where(is.character),
                                         ~ factor(. , exclude = NA)))
  
  ## Convert features with fewer than n=10 unique values
  ## to factor
  new_data <- new_data |>
    mutate(across(
      where(
        \(x) n_distinct(x, na.rm = TRUE) < factor_size &
          n_distinct(x, na.rm = TRUE) > 2 &
          is.numeric(x)
      ),
      ~ factor(. , exclude = NA)
    ))
  
  return(new_data)
}