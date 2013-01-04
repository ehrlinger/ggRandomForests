####**********************************************************************
####**********************************************************************
####
####  GGPLOT FIGURES FOR RANDOM FORESTS FOR SURVIVAL, REGRESSION,
####  AND CLASSIFICATION (RF-SRC)
####  Version 0.5.0
####
####  Copyright 2012, Cleveland Clinic Foundation
####
####  This program is free software; you can redistribute it and/or
####  modify it under the terms of the GNU General Public License
####  as published by the Free Software Foundation; either version 2
####  of the License, or (at your option) any later version.
####
####  This program is distributed in the hope that it will be useful,
####  but WITHOUT ANY WARRANTY; without even the implied warranty of
####  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
####  GNU General Public License for more details.
####
####  You should have received a copy of the GNU General Public
####  License along with this program; if not, write to the Free
####  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
####  Boston, MA  02110-1301, USA.
####
####  ----------------------------------------------------------------
####  Project Partially Funded By: 
####  ----------------------------------------------------------------
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    John Ehrlinger, Ph.D.
####    Assistant Staff
####    Dept of Quantitative Health Sciences
####    Learner Research Institute
####    Cleveland Clinic Foundation
####
####    email:  john.ehrlinger@gmail.com
####    URL:    http://www.kogalur.com
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#'
#' ggplot.variable Plot randomForestSRC Marginal Effect of Variables using 
#' ggplot2

ggplot.survival.rfsrc <- function (x,
                                   plots.one.page = TRUE,
                                   plot.it = TRUE,
                                   subset, collapse = FALSE,
                                   haz.model = c("spline", "ggamma", "nonpar"),
                                   k = 25,
                                   span = "cv",
                                   cens.model = c("km", "rfsrc"),
                                   ...)
{
  
  ## Incoming parameter checks.  All are fatal.
  if (is.null(x)) {
    stop("object x is empty!")
  }
  if (sum(inherits(x, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(x, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, predict)'.")
  }
  if (x$family != "surv") {
    stop("this function only supports right-censored survival settings")
  }
  
  ## predict object does not contain OOB values
  if (sum(inherits(x, c("rfsrc", "predict"), TRUE) == c(1, 2)) == 2) {
    rfsrcPred <- TRUE
  }
  else {
    rfsrcPred <- FALSE
  }
  
  ## grow objects under non-standard bootstrapping are OOB devoid
  ## treat the object as if it were predict
  if (is.null(x$predicted.oob)) {
    rfsrcPred <- TRUE
  }
  
  ## verify the haz.model option
  haz.model <- match.arg(haz.model, c("spline", "ggamma", "nonpar"))
  ##ensure that the glmnet package is available when splines are selected
  if (!missing(subset) && haz.model == "spline") {
    if (!available(glmnet)) {#loads the package, otherwise sets haz.model to "ggamma"
      warning("the 'glmnet' package is required for this option: reverting to 'ggamma' method instead")
      haz.model <- "ggamma"
    }
  }
  
  ## verify the cens.model option
  cens.model <- match.arg(cens.model, c("km", "rfsrc"))
  
  ## use imputed missing time or censoring indicators
  if (!is.null(x$yvar) && !is.null(x$imputed.indv)) {
    x$yvar[x$imputed.indv, ]=x$imputed.data[, 1:2]
  }
  
  ## get the event data
  event.info <- randomForestSRC:::get.event.info(x)
  
  ## Process the subsetted index
  ## Assumes the entire data set is to be used if not specified
  if (missing(subset)) {
    subset <- 1:x$n
    subsetProvided <- FALSE
  }
  else {
    ## convert the user specified subset into a usable form 
    if (is.logical(subset)) subset <- which(subset)
    subset <- unique(subset[subset >= 1 & subset <= x$n])
    plot.it <- subsetProvided <- TRUE
    if (length(subset) == 0) {
      stop("'subset' not set properly.")
    }
  }
  
  ## no point in producing plots if sample size is too small
  if (!rfsrcPred && !subsetProvided && (x$n < 2 | x$ndead < 1)) {
    stop("sample size or number of deaths is too small for meaningful analysis")
  }
  
  ## use OOB values for grow forest
  if (rfsrcPred) {
    mort    <- x$predicted[subset]
    surv.ensb  <- t(x$survival[subset,, drop = FALSE])
    chf.ensb <- x$chf[subset,, drop = FALSE]
    y.lab   <- "Mortality"
    title.1 <- "Survival"
    title.2 <- "Cumulative Hazard"
    title.3 <- "Hazard"
    title.4 <- "Mortality vs Time"
  }
  else {
    mort    <- x$predicted.oob[subset]
    surv.ensb    <- t(x$survival.oob[subset,, drop = FALSE])
    chf.ensb <- x$chf.oob[subset,, drop = FALSE]
    y.lab   <- "OOB Mortality"
    title.1 <- "OOB Survival"
    title.2 <- "OOB Cumulative Hazard"
    title.3 <- "OOB Hazard"
    title.4 <- "OOB Mortality vs Time"
  }
  
  
  ## mean ensemble survival
  if (!subsetProvided) {
    surv.mean.ensb <- apply(surv.ensb, 1, mean, na.rm = TRUE)
  }
  
  ## collapse across the subset?
  if (subsetProvided && collapse) {
    surv.ensb <- apply(surv.ensb, 1, mean, na.rm = TRUE)
    chf.ensb <- rbind(apply(chf.ensb, 2, mean, na.rm = TRUE))
  }
  
  
  ##-------------------survival calculations------------------------
  if (!rfsrcPred && !subsetProvided) {
    
    ## KM estimator
    km.obj <- matrix(unlist(mclapply(1:length(event.info$time.interest),
                                     function(j) {
                                       c(sum(event.info$time >= event.info$time.interest[j], na.rm = TRUE),
                                         sum(event.info$time[event.info$cens != 0] == event.info$time.interest[j], na.rm = TRUE))
                                     })), ncol = 2, byrow = TRUE)
    Y <- km.obj[, 1]
    d <- km.obj[, 2]
    r <- d / (Y + 1 * (Y == 0))
    surv.aalen <- exp(-cumsum(r))
    
    ## Estimate the censoring distribution    
    sIndex <- function(x,y) {sapply(1:length(y), function(j) {sum(x <= y[j])})}
    censTime <- sort(unique(event.info$time[event.info$cens == 0]))
    censTime.pt <- c(sIndex(censTime, event.info$time.interest))
    
    ## check to see if there are censoring cases
    if (length(censTime) > 0) {
      
      ## KM estimator for the censoring distribution
      if (cens.model == "km") {
        censModel.obj <- matrix(unlist(mclapply(1:length(censTime),
                                                function(j) {
                                                  c(sum(event.info$time >= censTime[j], na.rm = TRUE),
                                                    sum(event.info$time[event.info$cens == 0] == censTime[j], na.rm = TRUE))
                                                })), ncol = 2, byrow = TRUE)
        Y <- censModel.obj[, 1]
        d <- censModel.obj[, 2]
        r <- d / (Y + 1 * (Y == 0))
        cens.dist <- c(1, exp(-cumsum(r)))[1 + censTime.pt]
      }
      
      # RFSRC estimator for the censoring distribution
      else {
        newd <- cbind(x$yvar, x$xvar)
        newd[, 2] <- 1 * (newd[, 2] == 0)
        cens.dist <- t(predict(x, newd, outcome = "test")$survival.oob)
      }
    }
    
    ## no censoring cases; assign a default distribution
    else {
      cens.dist <- rep(1, length(censTime.pt))
    }
    
    
    ##-------------------brier calculations------------------------
    ## Brier object
    brier.obj <- matrix(unlist(mclapply(1:x$n, function(i)
    {
      tau <-  event.info$time
      event <- event.info$cens
      t.unq <- event.info$time.interest
      cens.pt <- sIndex(t.unq, tau[i])
      if (cens.model == "km") {
        c1 <- 1 * (tau[i] <= t.unq & event[i] != 0)/c(1, cens.dist)[1 + cens.pt]
        c2 <- 1 * (tau[i] > t.unq)/cens.dist
      }
      else {
        c1 <- 1 * (tau[i] <= t.unq & event[i] != 0)/c(1, cens.dist[, i])[1 + cens.pt]
        c2 <- 1 * (tau[i] > t.unq)/cens.dist[, i]
      } 
      (1 * (tau[i] > t.unq) - surv.ensb[, i])^2 * (c1 + c2)
    })), ncol = length(event.info$time.interest), byrow = TRUE)
    
    ## extract the Brier score stratified by mortality percentiles
    brier.score <- matrix(NA, length(event.info$time.interest), 4)
    mort.perc   <- c(min(mort, na.rm = TRUE) - 1e-5, quantile(mort, (1:4)/4, na.rm = TRUE))
    for (k in 1:4){
      mort.pt <- (mort > mort.perc[k]) & (mort <= mort.perc[k+1])
      brier.score[, k] <- apply(brier.obj[mort.pt,, drop=FALSE], 2, mean, na.rm = TRUE)
    }
    brier.score <- as.data.frame(cbind(brier.score, apply(brier.obj, 2, mean, na.rm = TRUE)))
    colnames(brier.score) <- c("q25", "q50", "q75", "q100", "all") 
    
  }
  
  ##-------------------hazard calculations------------------------
  if (subsetProvided) {
    ## we estimate the hazard function in three (3) different ways
    
    
    ##survival function of generalized gamma
    sggamma <- function(q, mu = 0, sigma = 1, Q)
    {
      sigma <- exp(sigma)#reparametrize sigma to be unconstrained
      q[q < 0] <- 0
      if (Q != 0) {
        y <- log(q)
        w <- (y - mu)/sigma
        expnu <- exp(Q * w) * Q^-2
        ret <- if (Q > 0) 
          pgamma(expnu, Q^-2)
        else 1 - pgamma(expnu, Q^-2)
      }
      else {
        ret <- plnorm(q, mu, sigma)
      }
      1 - ret
    }
    
    ##density of generalized gamma
    dggamma <- function(x, mu = 0, sigma = 1, Q) 
    {
      sigma <- exp(sigma)#reparametrize sigma to be unconstrained
      ret <- numeric(length(x))
      ret[x <= 0] <- 0
      xx <- x[x > 0]
      if (Q != 0) {
        y <- log(xx)
        w <- (y - mu)/sigma
        logdens <- -log(sigma * xx) + log(abs(Q)) + (Q^-2) * 
          log(Q^-2) + Q^-2 * (Q * w - exp(Q * w)) - lgamma(Q^-2)
      }
      else logdens <- dlnorm(xx, mu, sigma, log = TRUE)
      ret[x > 0] <- exp(logdens)
      ret
    }
    
    ##hazard of generalized gamma
    hggamma <- function(x, mu = 0, sigma = 1, Q)
    {
      dggamma(x = x, mu = mu, sigma = sigma, Q = Q) / sggamma(q = x, 
                                                              mu = mu, sigma = sigma, Q = Q)
    }
    
    haz.list <- mclapply(1:nrow(chf.ensb), function(i) { 
      
      ## method (1)
      ## fit a 3-parameter generalized gamma model
      ## basic functions have been shamelessly taken from library(flexsurv)
      
      if (haz.model == "ggamma") {
        ##extract time and S(t)
        x <- event.info$time.interest
        y <- t(surv.ensb)[i, ]
        
        ##smooth H(t)
        ll <- supsmu(x, y, span = span)
        
        ## the optimization function is the mean RSS between
        ## the survival function and the generalized gamma
        fn <- function(z) {
          mean((y - sggamma(x, mu = z[1], sigma = z[2], Q = z[3]))^2, na.rm = TRUE)
        }
        
        ## initialize the parameters and optimize
        init <- c(0, 1, 0)
        optim.obj <- optim(init, fn)
        
        ## extract the final parameters
        if (optim.obj$convergence != 0) warning("fit.ggamma failed to converge")
        parm <- optim.obj$par
        
        ## return the hazard
        list(x = x, y = hggamma(x, parm[1], parm[2], parm[3]))
        
      }
      
      ## method (2)
      ## Royston and Parmar spline approach for log H(t)
      ## log H(t) = s(x, gamma), where x = log(t)
      
      else if (haz.model == "spline") {
        
        
        ##extract the time variable
        tm <- event.info$time.interest
        
        ##shift time to the right to avoid numerical issues with log(0)
        shift.time <- ifelse(min(tm, na.rm = TRUE) < 1e-3, 1e-3, 0)
        ##shift.time <- 0
        
        ##take the log of time: these are the x-values used in the glmnet call
        log.tm <- log(tm + shift.time)
        
        ##translate the CHF by a constant to avoid numerical issues with log
        shift.chf <- 1
        
        ##take the log of the CHF: this is the "response" in the glmnet call
        y <- log(chf.ensb[i, ] + shift.chf)
        
        
        ##define the knots
        k <- max(k, 2)
        knots <- unique(c(seq(min(log.tm), max(log.tm), length = k), 5 * max(log.tm)))
        
        
        ##define the spline basis functions
        m <- length(knots) 
        kmin <- min(knots)
        kmax <- max(knots)
        
        if (m < 2) {
          stop("not enough knots (confirm that the number of unique event times > 2")
        }
        x <- do.call(cbind, mclapply(1:(m+1), function(j) {
          if (j == 1) {
            log.tm
          }
          else {
            lj <- (kmax - knots[j-1]) / (kmax - kmin)
            pmax(log.tm - knots[j-1], 0)^3 - lj * pmax(log.tm - kmin, 0)^3 - (1 - lj) * pmax(log.tm - kmax, 0)^3 
          }
        }))
        
        ## lasso estimation
        ## we use cross-validation with glmnet to estimate the gamma coefficients
        ## from s(x, gamma)
        cv.obj <- tryCatch({cv.glmnet(x, y, alpha = 1)}, error = function(ex){NULL})
        if (!is.null(cv.obj)) {
          coeff <- as.vector(predict(cv.obj, type = "coef", s = "lambda.1se"))
        }
        else {
          warning("glmnet did not converge: setting coefficients to zero")
          coeff <- rep(0, 1+ ncol(x))
        }
        
        ## calculate s(x, gamma)
        sfn <- coeff[1] + x %*% coeff[-1]
        
        ## theoretical s'(x, gamma)
        x.deriv <- do.call(cbind, mclapply(1:m, function(j) {
          lj <- (kmax - knots[j]) / (kmax - kmin)
          3 * (pmax(log.tm - knots[j], 0)^2 - lj * pmax(log.tm - kmin, 0)^2
               - (1 - lj) * pmax(log.tm - kmax, 0)^2)
          
        }))
        sfn.deriv <- coeff[2] + x.deriv %*% coeff[-c(1:2)]
        
        
        ## take the derivative of H(t) to obtain the estimated hazard
        ## this is (ds(x, gamma)/dt) * exp(s(x, gamma))
        ## which equals s'(x, gamma) * (dx/dt) * exp(s(x, gamma))
        ## x=log(t+shift.time), thus dx/dt = 1/(t + shift.time)
        haz <- sfn.deriv * exp(sfn) / (tm + shift.time)
        
        ## negative values are set to 0
        ## smooth the hazard
        ## negative values are set to 0
        haz[haz < 0] <- 0
        haz <- supsmu(tm, haz)$y
        haz[haz < 0] <- 0
        
        ##return the obj
        #supsmu(tm, haz)
        list(x = tm, y = haz)
        
      }
      
      ## method (3)
      ## nonparametric estimate
      ## smooth the derivative of the smoothed H(t) 
      
      else if (haz.model == "nonpar") {
        
        ##extract time and H(t)
        x <- event.info$time.interest
        y <- chf.ensb[i, ]
        
        ##smooth H(t)
        ll <- supsmu(x, y, span = "cv")
        ##smooth the derivative of the smoothed H(t)
        supsmu(x = ll$x[-length(x)], y = diff(ll$y) / diff(ll$x), span = span)
        
      }
      
      
    })
    
  }
  
  
  ## should we display the plots? 
  if (plot.it) {
    
    old.par <- par(no.readonly = TRUE)
    if (plots.one.page) {
      if (rfsrcPred && !subsetProvided) {
        if (!is.null(x$yvar)) {#survival/mortality only
          par(mfrow = c(1,2))
        }
        else {#predict mode but no outcomes: survival only
          par(mfrow = c(1,1))
        }
      }
      else {
        par(mfrow = c(2,2))
      }
    }
    else {#plots on one page
      par(mfrow=c(1,1))
    }
    par(cex = 1.0)
    
    ###----survival plot----
    if (!subsetProvided && x$n > 500) {
      r.pt <- sample(1:x$n, 500, replace = FALSE)
      dta <- as.data.frame(cbind(event.info$time.interest,
                                 surv.ensb[, r.pt]))
    }
    else {
      dta <- as.data.frame(cbind(event.info$time.interest,
                                 surv.ensb))
    }
    
    dta<- melt(dta,value=2:dim(dta)[2],id="V1")
    rg.ticks <- as.data.frame(cbind(x=event.info$time.interest))
    srv <- ggplot(dta)+ geom_step(aes(x=V1, y=value, by=variable), alpha=.1) +
      labs(x= "Time", y = title.1) + theme_bw()+
      geom_rug(aes(x=x),data=rg.ticks, ticksize=-0.03) +
      scale_y_continuous(breaks=seq(0,1,.05)) +
      coord_cartesian(xlim=range(event.info$time.interest),ylim=c(0,1))
    
    if (!rfsrcPred && !subsetProvided) {
      aalen.data <- as.data.frame(cbind(x=event.info$time.interest, y=surv.aalen))
      srv <- srv + geom_step(aes(x=x, y=y), data= aalen.data,
                             lty = 1, col = 3, lwd=1)
    }
    if (!subsetProvided) {
      mean.data <- as.data.frame(cbind(x=event.info$time.interest, y=surv.mean.ensb))
      srv <- srv + geom_step(aes(x=x, y=y),data=mean.data,
                             lty = 1, col = 2, lwd=1)
    }
    show(srv)
    ###----CHF plot----
    if (subsetProvided) {
      dta <- as.data.frame(cbind(event.info$time.interest,
                                 t(chf.ensb)))
      
      dta<- melt(dta,value=2:dim(dta)[2],id="V1")
      chf<- ggplot(dta)+ geom_line(aes(x=V1, y=value, by=variable), alpha=.1, lty=3) +
        labs(x= "Time", y = title.2) + theme_bw()
      hdta <- as.data.frame(cbind(haz.list[[1]]$x,
                                  do.call(cbind, mclapply(haz.list, function(ll){cumsum(ll$y * c(0, diff(ll$x)))}))))
      hdta < melt(hdta,value=2:dim(hdta)[2],id="V1")
      chf<- chf + geom_line(aes(x=V1, y=value, by=variable), data=hdta, col=4, lty=3) +
        geom_rug(aes(x=x),data=rg.ticks, ticksize=-0.03) 
    }
    
    ###----hazard plot----
    if (subsetProvided) {
      plot(range(haz.list[[1]]$x, na.rm = TRUE),
           range(unlist(mclapply(haz.list, function(ll) {ll$y})), na.rm = TRUE),
           type = "n",
           xlab = "Time",
           ylab = title.3, ...)
      void <- lapply(haz.list, function(ll) {
        lines(ll, type = "l", col = 1, lty = 3)
      })
      rug(event.info$time.interest, ticksize=-0.03)
      if (plots.one.page) {
        title(title.3, cex.main = 1.25)
      }
    }
    
    ###----Brier plot----
    if (!rfsrcPred && !subsetProvided) {
      dta <- as.data.frame(cbind(time= event.info$time.interest,
                                 brier.score))
      
      dta<- melt(dta,value=2:dim(dta)[2],id="time")

      brr <- ggplot(dta)+ geom_smooth(aes(x=time, y=value, col=variable), se=FALSE, span=.1) +
        labs(x= "Time", y = "OOB Brier Score") + theme_bw()+
        geom_rug(aes(x=x),data=rg.ticks, ticksize=-0.03) 
      #               lwd  = c(rep(1, 4), 2),
      #               col  = c(rep(1, 4), 2),
      #               lty  = c(1:4, 1), ...)
      #       point.x=round(length(event.info$time.interest)*c(3,4)/4)
      #       text(event.info$time.interest[point.x],brier.score[point.x,1],"0-25",col=4)
      #       text(event.info$time.interest[point.x],brier.score[point.x,2],"25-50",col=4)
      #       text(event.info$time.interest[point.x],brier.score[point.x,3],"50-75",col=4)
      #       text(event.info$time.interest[point.x],brier.score[point.x,4],"75-100",col=4)
      show(brr)
    }
    
    ###----mortality plot----
    if (!subsetProvided && !is.null(x$yvar)) {
      dta <- as.data.frame(cbind(time=event.info$time,cens=as.factor(event.info$cens),
                                 mort))
      
      mrt <- ggplot(dta) + geom_point(aes(x=time, y= mort, col=cens)) + theme_bw() +
        labs(x = "Time", y= y.lab)+
      geom_smooth(aes(x=time, y=mort, col=cens),lty = 3, se=FALSE)
      show(mrt)
    }
    
    ## reset par
    par(old.par)
    
  }
  
  ##invisibly return the brier score
  if (!rfsrcPred && !subsetProvided) {
    
    ##integrated Brier using the trapezoidal rule
    Dint <- function(f, range, grid) {
      a <-  range[1]
      b <-  range[2]
      f <- f[grid >= a & grid <= b]
      grid <- grid[grid >= a & grid <= b]
      m <- length(grid)
      if ((b - a) <= 0 | m < 2) {
        0
      }
      else {
        (1 / ( 2 * diff(range)) ) * sum((f[2:m] + f[1:(m-1)])  * diff(grid))
      }
    }
    
    invisible(cbind(
      time = event.info$time.interest,
      brier.score,
      integrate = unlist(mclapply(1:length(event.info$time.interest),
                                  function(j) {
                                    Dint(f = brier.score[1:j, 4],
                                         range = quantile(event.info$time.interest, probs = c(0.05, 0.95), na.rm = TRUE),
                                         grid = event.info$time.interest[1:j])
                                  }))
    ))
  }
  
}


ggplot.survival <- ggplot.survival.rfsrc
