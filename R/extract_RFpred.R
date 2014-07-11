####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####    John Ehrlinger, Ph.D.
####    Assistant Staff
####    Dept of Quantitative Health Sciences
####    Learner Research Institute
####    Cleveland Clinic Foundation
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' extract.RFpred get the predicted values from a randomForest classification forest
#' 
#' Only works on randomForest package forests.
#' @param obj a \code{randomForest}
#' @param type Classification by ("response", "prob")
#' @param subset indx of observations of interest
#' @param which.outcome For classification, select the class index of interest
#' 
extract.RFpred <- function (obj, type=c("response", "prob"), subset, which.outcome) 
{
  if(inherits(obj,"matrix")){
    pred <- obj
  }else{
    pred <- obj$votes
    
    if (obj$type == "classification") {
      class.type <- match.arg(type)
      if (missing(subset)) 
        subset <- 1:nrow(pred)
      
      ## Again need to mod this if we don't want the default to be 
      ## only 1 of the multiclass cases
      if (missing(which.outcome)) 
        which.outcome <- 1
      
      prob <- pred[subset, , drop = FALSE]
      return(switch(class.type, prob = prob, 
                    response = bayes.rule(prob)))
    }
    else {
      if (missing(subset)) 
        subset <- 1:length(pred)
      return(pred[subset])
    }
  }
}
resample <- function(x, ...) x[sample.int(length(x), ...)]


bayes.rule<- function(prob){
  levels.class <- colnames(prob)
  factor(levels.class[apply(prob, 1, function(x) {
    if (!all(is.na(x))) {
      resample(which(x == max(x, na.rm = TRUE)), 1)
    }
    else {
      NA
    }
  })], levels = levels.class)
}
