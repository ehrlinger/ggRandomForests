#' randomForestSRC partial dependence (data object) (modified from randomForestSRC V1.6.0)
#'
#' @description calculate the partial dependence of an x-variable on the class probability 
#' (classification), response (regression), mortality (survival), or the expected years lost 
#' (competing risk) from a RF-SRC analysis.
#' 
#' @param x An object of class (\code{rfsrc}, \code{grow}), (\code{rfsrc}, \code{synthetic}), 
#' (\code{rfsrc}, \code{predict}).
#' @param xvar.names Names of the x-variables to be used.
#' @param which.outcome For classification families, an integer or character value specifying 
#' the class to focus on (defaults to the first class). For competing risk families, an integer 
#' value between 1 and J indicating the event of interest, where J is the number of event types. 
#' The default is to use the first event type.
#' @param surv.type For survival families, specifies the predicted value. See details below.
#' @param nvar Number of variables to be plotted. Default is all. 
#' @param npts Maximum number of points used when generating partial plots for continuous variables.
#' @param subset Vector indicating which rows of the x-variable matrix x$xvar to use. All rows are 
#' used if not specified.
#' @param granule Integer value controlling minimum number of unique values required to treat a 
#' variable as continuous. If there are fewer, the variable is treated as a factor
#' @param ... other used arguments. Included for compatibility with plot.variable calls.
#'
#' @details 
#' The vertical axis displays the ensemble predicted value, while x-variables are plotted on the 
#' horizontal axis. 
#' \enumerate{
#'  \item For regression, the predicted response is used.
#' \item For classification, it is the predicted class probability specified by which.outcome.
#' 
#' \item For survival, the choices are:
#'  \itemize{ 
#' \item  Mortality (mort).
#' \item  Relative frequency of mortality (rel.freq).
#' \item  Predicted survival (surv)
#' }
#'
#' \item  For competing risks, the choices are:
#' \itemize{
#' \item    The expected number of life years lost (years.lost).
#' \item  The cumulative incidence function (cif).
#' \item  The cumulative hazard function (chf).
#' }
#' In all three cases, the predicted value is for the event type specified by which.outcome.
#' }
#'
#' The y-value for a variable X, evaluated at \eqn{X=x}, is
#'
#' \deqn{\tilde{f}(x) = \frac{1}{n} \sum_{i=1}^n \hat{f}(x, x_{i,o}),}
#' where \eqn{x_{i,o}} represents the value for all other variables other than \eqn{X} for individual \eqn{i} and
#'  \eqn{\hat{f}} is the predicted value. Generating partial plots can be very slow. Choosing a small 
#'  value for npts can speed up computational times as this restricts the number of distinct x 
#'  values used in computing \eqn{\tilde{f}}.
#' 
#' Calculating partial dependence data can be slow. Setting npts to a smaller number can help.
#' 
#' @author Hemant Ishwaran and Udaya B. Kogalur (Modified by John Ehrlinger)
#' 
#' @references
#' Friedman J.H. (2001). Greedy function approximation: a gradient boosting machine, Ann. of Statist., 5:1189-1232.
#' 
#' Ishwaran H., Kogalur U.B. (2007). Random survival forests for R, Rnews, 7(2):25-31.
#' 
#' Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S. (2008). Random survival forests, Ann. App. Statist., 2:841-860.
#' 
#' Ishwaran H., Gerds T.A., Kogalur U.B., Moore R.D., Gange S.J. and Lau B.M. (2014). Random survival forests for competing risks. 
#' To appear in Biostatistics.
#' 
#' @seealso \code{rfsrc}, \code{rfsrcSyn}, \code{predict.rfsrc}
#' 
#' @examples
#' ## ------------------------------------------------------------
#' ## survival/competing risk
#' ## ------------------------------------------------------------
#' 
#' ## survival
#' \dontrun{
#' data(veteran, package = "randomForestSRC")
#' v.obj <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
#' plot.variable(v.obj, plots.per.page = 3)
#' plot.variable(v.obj, plots.per.page = 2, xvar.names = c("trt", "karno", "age"))
#' plot.variable(v.obj, surv.type = "surv", nvar = 1)
#' plot.variable(v.obj, surv.type = "surv", partial = TRUE, smooth.lines = TRUE)
#' plot.variable(v.obj, surv.type = "rel.freq", partial = TRUE, nvar = 2)
#' 
#' ## example of plot.variable calling a pre-processed plot.variable object
#' p.v <- plot.variable(v.obj, surv.type = "surv", partial = TRUE, smooth.lines = TRUE)
#' plot.variable(p.v)
#' p.v$plots.per.page <- 1
#' p.v$smooth.lines <- FALSE
#' plot.variable(p.v)
#' }
#' \dontrun{
#' ## competing risks
#' data(follic, package = "randomForestSRC")
#' follic.obj <- rfsrc(Surv(time, status) ~ ., follic, nsplit = 3, ntree = 100)
#' plot.variable(follic.obj, which.outcome = 2)
#' }
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#' \dontrun{
#' ## airquality
#' airq.obj <- rfsrc(Ozone ~ ., data = airquality)
#' plot.variable(airq.obj, partial = TRUE, smooth.lines = TRUE)

#' ## motor trend cars
#' mtcars.obj <- rfsrc(mpg ~ ., data = mtcars)
#' plot.variable(mtcars.obj, partial = TRUE, smooth.lines = TRUE)
#' }
#' ## ------------------------------------------------------------
#' ## classification
#' ## ------------------------------------------------------------
#' 
#' ## iris
#' #rfsrc_iris <- rfsrc(Species ~., data = iris)
#' data(rfsrc_iris, package="ggRandomForests")
#' #gg_dta <- partial.rfsrc(rfsrc_iris, )
#' 
#' \dontrun{
#' ## motor trend cars: predict number of carburetors
#' mtcars2 <- mtcars
#' mtcars2$carb <- factor(mtcars2$carb,
#'                        labels = paste("carb", sort(unique(mtcars$carb))))
#' mtcars2.obj <- rfsrc(carb ~ ., data = mtcars2)
#' plot.variable(mtcars2.obj, partial = TRUE)
#' }
#'
#' 
#' @importFrom randomForestSRC predict.rfsrc 
#' @importFrom stats na.omit
#'
#' @export
partial.rfsrc <- function (x, 
                           xvar.names, 
                           which.outcome,
                           surv.type = c("mort","rel.freq", "surv", "years.lost", "cif", "chf"), 
                           nvar, 
                           npts = 25, 
                           subset, 
                           granule=5,
                           ...) {
  
  if (sum(inherits(x, c("rfsrc", "synthetic"), TRUE) == c(1, 2)) == 2) {
    x <- x$rfSyn
  }
  object <- x
  remove(x)
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1,2)) != 2 & 
      sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2){
    stop("this function only works for objects of class `(rfsrc, grow)' or '(rfsrc, predict)'")
  }
  if (object$family == "unsupv") {
    stop("this function does not apply to unsupervised forests")
  }
  if (is.null(object$forest)) {
    stop("forest is empty:  re-run rfsrc (grow) call with forest=TRUE")
  }
  
  # Pull the training data out of the forest and merge in imputed values (if they exist).
  xvar <- object$xvar
  if (!is.null(object$imputed.indv)) {
    xvar[object$imputed.indv, ] <- object$imputed.data[, object$xvar.names]
  }
  
  # How many observations?
  n <- nrow(xvar)
  
  # Are we interesting in a subset of the data
  if (missing(subset)) {
    subset <- 1:n
  }else {
    if (is.logical(subset)) 
      subset <- which(subset)
    subset <- unique(subset[subset >= 1 & subset <= n])
    if (length(subset) == 0) {
      stop("'subset' not set properly")
    }
  }
  
  # Update the training data for these plots.
  xvar <- xvar[subset, , drop = FALSE]
  
  # Update how many observations?
  n <- nrow(xvar)
  
  # What are we interested in plotting?
  fmly <- object$family
  
  ## For survival
  if (grepl("surv", fmly)) {
    event.info <- get.event.info(object, subset)
    cens <- event.info$cens
    event.type <- event.info$event.type
    
    ## For competing risks
    if (fmly == "surv-CR") {
      if (missing(which.outcome)) {
        which.outcome <- 1
      }else {
        if (which.outcome < 1 || which.outcome > max(event.type, na.rm = TRUE)) {
          stop("'which.outcome' is specified incorrectly")
        }
      }
      VIMP <- object$importance[, which.outcome]
      surv.type <- setdiff(surv.type, c("mort", "rel.freq", "surv"))
      pred.type <- match.arg(surv.type, c("years.lost", "cif", "chf"))
    }else {
      ## For standard survival
      which.outcome <- 1
      VIMP <- object$importance
      surv.type <- setdiff(surv.type, c("years.lost", "cif", "chf"))
      pred.type <- match.arg(surv.type, c("mort", "rel.freq", "surv"))
    }
  }else {
    # This is from the randomForestSRC:::get.outcome.target 
    # It only has 1 value...
    outcome.target <- 1
    
    # For classification types
    event.info <- NULL
    if (fmly == "class" || fmly == "class+" || 
        (fmly == "mix+" && is.factor(object$yvar[, outcome.target]))) {
      object.yvar <- data.frame(object$yvar)[, outcome.target]
      if (missing(which.outcome)) {
        which.outcome <- 1
      }else if (is.character(which.outcome)) {
        which.outcome <- match(match.arg(which.outcome, levels(object.yvar)), 
                               levels(object.yvar))
      } else {
        if (which.outcome > length(levels(object.yvar)) | 
            which.outcome < 1) {
          stop("which.outcome is specified incorrectly:", 
               which.outcome)
        }
      }
      pred.type <- "prob"
      VIMP <- object$importance[, 1 + which.outcome]
      remove(object.yvar)
    }else {
      
      ## For regression
      pred.type <- "y"
      which.outcome <- NULL
      VIMP <- object$importance
    }
  }
  
  sorted <- TRUE
  
  ## Which variables are we working with
  if (missing(xvar.names)) {
    # All of them.
    xvar.names <- object$xvar.names
  } else {
    # The specified subset.
    xvar.names <- intersect(xvar.names, object$xvar.names)
    
    # If we supply names, we don't want the result sorted.
    sorted <- FALSE
    
    if (length(xvar.names) == 0) {
      stop("none of the x-variable supplied match available ones:\n", 
           object$xvar.names)
    }
  }
  
  # Sorting output variables?
  if (sorted & !is.null(VIMP)) {
    xvar.names <- xvar.names[rev(order(VIMP[xvar.names]))]
  }
  
  # Specify the highest VIMP(if sorted), or the first....
  # This kind of doesn't make sense to do.
  if (!missing(nvar)) {
    nvar <- max(round(nvar), 1)
    xvar.names <- xvar.names[1:min(length(xvar.names), nvar)]
  }
  nvar <- length(xvar.names)
  
  ## partial only
  class(object$forest) <- c("rfsrc", "partial", class(object)[3])
  
  ## Estimating at how many points?
  npts <- round(npts)
  if (npts < 1) { 
    npts <- 1
  }
  
  # Calculate the estimates for each variable of interest
  prtl <- lapply(1:nvar, function(k) {
    
    # Remove missing training set values.
    x <- na.omit(object$xvar[, object$xvar.names == xvar.names[k]])
    if (is.factor(x)) 
      x <- factor(x, exclude = NULL)
    
    # How many points along x?
    n.x <- length(unique(x))
    
    # If it's a factor, we only want to get estimates for each factor level
    if (!is.factor(x) & n.x > npts) {
      x.uniq <- sort(unique(x))[unique(as.integer(seq(1, n.x, length = min(npts, n.x))))]
    }else {
      x.uniq <- sort(unique(x))
    }
    
    # How many points along x? (Again)
    n.x <- length(x.uniq)
    
    # Initialize....
    yhat <- yhat.se <- vector("list", length=n.x)
    
    # Estimate on...
    newdata.x <- xvar
    
    # Set a minimum number of unique values to classify as continuous
    factor.x <- !(!is.factor(x) & (n.x > granule))
    
    # For each unique x value (x_v)
    for (l in 1:n.x) {
      
      # Set the x_i=x_v 
      newdata.x[, object$xvar.names == xvar.names[k]] <- rep(x.uniq[l], n)
      
      # Pull out the nomogram data for this x_v
      yhat[[l]] <- extract.pred(randomForestSRC::predict.rfsrc(object, 
                                                               newdata.x, 
                                                               importance = "none"), 
                                pred.type, 
                                1:n, which.outcome)
    }
    list(xvar.name = xvar.names[k], yhat = yhat, 
         yhat.se = yhat.se, n.x = n.x, x.uniq = x.uniq, 
         x = x)
  })
  
  
  gg_dta <- list(family = fmly, surv.type=pred.type,
                 event.info = event.info, which.outcome = which.outcome, 
                 n = n, xvar.names = xvar.names, 
                 nvar = nvar)
  
  gg_dta$pData <- prtl
  
  names(gg_dta$pData) <- xvar.names
  
  class(gg_dta) <- c("gg_partial_depend", 
                     fmly)
  invisible(gg_dta)
}

get.event.info <- function(obj, subset = NULL) {
  if (grepl("surv", obj$family)) {
    if (!is.null(obj$yvar)) {
      if (is.null(subset)) {
        subset <- (1:nrow(cbind(obj$yvar)))
      }
      r.dim <- 2
      time <- obj$yvar[subset, 1]
      cens <- obj$yvar[subset, 2]
      if (!all(floor(cens) == abs(cens), na.rm = TRUE)) {
        stop("for survival families censoring variable must be coded as a non-negative integer")
      }
      event <- na.omit(cens)[na.omit(cens) > 0]
      event.type <- unique(event)
    }
    else {
      r.dim <- 0
      event <- event.type <- cens <- cens <- time <- NULL
    }
    time.interest <- obj$time.interest
  }
  else {
    r.dim <- 1
    event <- event.type <- cens <- time.interest <- cens <- time <- NULL
  }
  return(list(event = event, event.type = event.type, cens = cens,
              time.interest = time.interest, time = time, r.dim = r.dim))
}


extract.pred <- function(obj, type, subset, which.outcome, oob = FALSE) {
  if (oob == FALSE) {
    pred <- obj$predicted
    surv <- obj$survival
    chf <- obj$chf
    cif <- obj$cif
  }
  else {
    pred <- obj$predicted.oob
    surv <- obj$survival.oob
    chf <- obj$chf.oob
    cif <- obj$cif.oob
  }
  if (obj$family == "surv") {
    n <- length(pred)
    if (missing(subset)) subset <- 1:n
    surv.type <- match.arg(type, c("mort", "rel.freq", "surv"))
    return(switch(surv.type,
                  "mort" = pred[subset],
                  "rel.freq" = pred[subset] / max(n, na.omit(pred)),
                  "surv" =  100 * surv[subset, ]
    ))
  }
  else if (obj$family == "surv-CR") {
    n <- length(pred)
    if (missing(subset)) subset <- 1:n
    if (missing(which.outcome)) which.outcome <- 1
    cr.type <- match.arg(type, c("years.lost", "cif", "chf"))
    return(switch(cr.type,
                  "years.lost" = pred[subset, which.outcome],
                  "cif" = cif[subset, , which.outcome],
                  "chf" = chf[subset, , which.outcome]
    ))
  }
  else if (obj$family == "class" || obj$family == "class+" || (obj$family ==  "mix+" && ncol(pred) > 1)) {
    class.type <- match.arg(type, c("response", "prob"))
    if (missing(subset)) subset <- 1:nrow(pred)
    if (missing(which.outcome)) which.outcome <- 1
    prob <- pred[subset,, drop = FALSE]
    return(switch(class.type,
                  "prob" = prob[, which.outcome],
                  "response" =  bayes.rule(prob)))
  }
  else {
    if (missing(subset)) subset <- 1:length(pred)
    return(pred[subset])
  }
}

bayes.rule <- function(prob) {
  levels.class <- colnames(prob)
  factor(levels.class[apply(prob, 1, function(x) {
    if (!all(is.na(x))) {
      resample(which(x == max(x, na.rm = TRUE)), 1)
    }
    else {
      NA
    }
  })], 
  levels = levels.class)
}

resample <- function(x, size, ...) {
  if (length(x) <= 1) {
    if (!missing(size) && size == 0) x[FALSE] else x
  }
  else {
    sample(x, size, ...)
  }
}