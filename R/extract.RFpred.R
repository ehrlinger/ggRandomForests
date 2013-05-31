extract.RFpred <- function (obj, type, subset, which.outcome) 
{
  if(class(obj) == "matrix"){
    pred <- obj
  }else{
    pred <- obj$votes
  
  if (obj$type == "classification") {
    class.type <- match.arg(type, c("response", "prob"))
    if (missing(subset)) 
      subset <- 1:nrow(pred)
    
    ## Again need to mod this if we don't want the default to be 
    ## only 1 of the multiclass cases
    if (missing(which.outcome)) 
      which.outcome <- 1

    prob <- pred[subset, , drop = FALSE]
    return(switch(class.type, prob = prob, 
                  response = randomForestSRC:::bayes.rule(prob)))
  }
  else {
    if (missing(subset)) 
      subset <- 1:length(pred)
    return(pred[subset])
  }
  }
}