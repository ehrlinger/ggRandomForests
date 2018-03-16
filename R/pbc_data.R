#'  pbc_data "fixes" some features of the randomForestSRC pbc data set.
#'  
#'  * groks logical and factor variables (< 5 unique values are factors).
#'  * Changes the age variable to be in years
#'  * changes time variable to years
#'  * Modifies the treatment factor
#' 
#' @export

pbc_data <- function(){
  dta <- new.env()
  data(pbc, package="randomForestSRC", envir = dta)
  
  pbc <- dta$pbc
  # For whatever reason, the age variable is in days... makes no sense to me
  for(ind in 1:dim(pbc)[2]){
    if(!is.factor(pbc[,ind])){
      if(length(unique(pbc[which(!is.na(pbc[,ind])),ind])) <= 2) {
        if(sum(range(pbc[,ind],na.rm=TRUE) == c(0,1)) ==2 ){
          pbc[,ind] <- as.logical(pbc[,ind])
        }
      }
    }else{
      if(length(unique(pbc[which(!is.na(pbc[,ind])),ind])) <= 2) {
        if(sum(sort(unique(pbc[,ind])) == c(0,1)) == 2){
          pbc[,ind] <- as.logical(pbc[,ind])
        }
        if(sum(sort(unique(pbc[,ind])) == c(FALSE, TRUE)) == 2){
          pbc[,ind] <- as.logical(pbc[,ind])
        }
      }
    }
    if(!is.logical(pbc[, ind]) & 
       length(unique(pbc[which(!is.na(pbc[,ind])),ind])) <= 5) {
      pbc[,ind] <- factor(pbc[,ind])
    }
  }
  # Convert age to years
  pbc$age <- pbc$age / 364.24
  
  pbc$years <- pbc$days / 364.24
  pbc <- pbc[, -which(colnames(pbc) == "days")]
  pbc$treatment <- as.numeric(pbc$treatment)
  pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
  pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
  pbc$treatment <- factor(pbc$treatment)
  invisible(pbc)
}