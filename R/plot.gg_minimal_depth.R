####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#'
#' Plot a \code{\link{gg_minimal_depth}} object for random forest variable
#' ranking.
#'
#' @param x \code{\link{gg_minimal_depth}} object created from a
#' \code{\link[randomForestSRC]{rfsrc}} object
#' @param selection should we restrict the plot to only include variables
#' selected by the minimal depth criteria (boolean).
#' @param type select type of y axis labels c("named","rank")
#' @param lbls a vector of alternative variable names.
#' @param ... optional arguments passed to \code{\link{gg_minimal_depth}}
#'
#' @return \code{ggplot} object
#'
#' @seealso \code{\link[randomForestSRC]{var.select}}
#' \code{\link{gg_minimal_depth}}
#'
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#'
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R,
#' Rnews, 7(2):25-31.
#'
#' Ishwaran H. and Kogalur U.B. (2014). Random Forests for Survival,
#' Regression and Classification (RF-SRC), R package version 1.5.
#'
#' @importFrom ggplot2 .data
#' @examples
#' ## Examples from RFSRC package...
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' ## You can build a randomForest
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' varsel_iris <- var.select(rfsrc_iris)
#'
#' # Get a data.frame containing minimaldepth measures
#' gg_dta <- gg_minimal_depth(varsel_iris)
#'
#' # Plot the gg_minimal_depth object
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' ## -------- air quality data
#' rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' varsel_airq <- var.select(rfsrc_airq)
#'
#' # Get a data.frame containing error rates
#' gg_dta <- gg_minimal_depth(varsel_airq)
#'
#' # Plot the gg_minimal_depth object
#' plot(gg_dta)
#'
#' ## -------- Boston data
#' data(Boston, package = "MASS")
#' rfsrc_boston <- randomForestSRC::rfsrc(medv ~ ., Boston)
#' # Get a data.frame containing error rates
#' plot(gg_minimal_depth(varsel_boston))
#'
#' ## -------- mtcars data
#' rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
#' varsel_mtcars <- var.select(rfsrc_mtcars)
#'
#' # Get a data.frame containing error rates
#' plot(gg_minimal_depth(varsel_mtcars))
#'
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## -------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' varsel_veteran <- var.select(rfsrc_veteran)
#'
#' gg_dta <- gg_minimal_depth(varsel_veteran)
#' plot(gg_dta)
#'
#' ## -------- pbc data
#' #' # We need to create this dataset
#' data(pbc, package = "randomForestSRC", )
#' # For whatever reason, the age variable is in days... makes no sense to me
#' for (ind in seq_len(dim(pbc)[2])) {
#'   if (!is.factor(pbc[, ind])) {
#'     if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'       if (sum(range(pbc[, ind], na.rm = TRUE) == c(0, 1)) == 2) {
#'         pbc[, ind] <- as.logical(pbc[, ind])
#'       }
#'     }
#'   } else {
#'     if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'       if (sum(sort(unique(pbc[, ind])) == c(0, 1)) == 2) {
#'         pbc[, ind] <- as.logical(pbc[, ind])
#'       }
#'       if (sum(sort(unique(pbc[, ind])) == c(FALSE, TRUE)) == 2) {
#'         pbc[, ind] <- as.logical(pbc[, ind])
#'       }
#'     }
#'   }
#'   if (!is.logical(pbc[, ind]) &
#'     length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 5) {
#'     pbc[, ind] <- factor(pbc[, ind])
#'   }
#' }
#' # Convert age to years
#' pbc$age <- pbc$age / 364.24
#'
#' pbc$years <- pbc$days / 364.24
#' pbc <- pbc[, -which(colnames(pbc) == "days")]
#' pbc$treatment <- as.numeric(pbc$treatment)
#' pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
#' pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
#' pbc$treatment <- factor(pbc$treatment)
#' dta_train <- pbc[-which(is.na(pbc$treatment)), ]
#' # Create a test set from the remaining patients
#' pbc_test <- pbc[which(is.na(pbc$treatment)), ]
#'
#' # ========
#' # build the forest:
#' rfsrc_pbc <- randomForestSRC::rfsrc(
#'   Surv(years, status) ~ .,
#'   dta_train,
#'   nsplit = 10,
#'   na.action = "na.impute",
#'   forest = TRUE,
#'   importance = TRUE,
#'   save.memory = TRUE
#' )
#'
#' varsel_pbc <- var.select(rfsrc_pbc)
#'
#' gg_dta <- gg_minimal_depth(varsel_pbc)
#' plot(gg_dta)
#'
#' @export
plot.gg_minimal_depth <- function(x,
                                  selection = FALSE,
                                  type = c("named", "rank"),
                                  lbls,
                                  ...) {
  gg_dta <- x
  if (!inherits(x, "gg_minimal_depth")) {
    gg_dta <- gg_minimal_depth(x, ...)
  }
  type <- match.arg(type)
  arg_set <- as.list(substitute(list(...)))[-1L]

  nvar <- nrow(gg_dta$varselect)
  if (!is.null(arg_set$nvar)) {
    if (is.numeric(arg_set$nvar) && arg_set$nvar > 1) {
      nvar <- arg_set$nvar
      if (nvar < nrow(gg_dta$varselect)) {
        gg_dta$varselect <- gg_dta$varselect[1:nvar, ]
      }
    }
  }

  xl <- c(0, ceiling(max(gg_dta$varselect$depth)) + 1)
  sel_th <- gg_dta$md.obj$threshold

  if (selection) {
    modelsize <- gg_dta$modelsize

    # Labels for the top md vars.
    md_labs <- gg_dta$topvars

    ## Number the variables
    for (ind in seq_len(length(md_labs))) {
      md_labs[ind] <- paste(ind, md_labs[ind], sep = ". ")
    }
    vsel <- gg_dta$varselect[seq_len(modelsize), ]
    vsel$rank <- seq_len(nrow(vsel))

    ## Reorder the minimal depth to place most "important" at top of figure
    vsel$names <- factor(vsel$names, levels = rev(levels(vsel$names)))
    gg_plt <- ggplot2::ggplot(vsel)
    gg_plt <- switch(type,
      rank = gg_plt +
        ggplot2::geom_point(ggplot2::aes(
          y = .data$rank, x = .data$depth, label = .data$rank
        )) +
        ggplot2::coord_cartesian(xlim = xl) +
        ggplot2::geom_text(
          ggplot2::aes(
            y = .data$rank,
            x = .data$depth - 0.7,
            label = .data$rank
          ),
          size = 3,
          hjust = 0
        ),
      named = gg_plt +
        ggplot2::geom_point(ggplot2::aes(y = .data$depth, x = .data$names)) +
        ggplot2::coord_cartesian(ylim = xl)
    )
  } else {
    vsel <- gg_dta$varselect
    vsel$rank <- seq_len(dim(vsel)[1])
    vsel$names <- factor(vsel$names, levels = rev(levels(vsel$names)))
    gg_plt <- ggplot2::ggplot(vsel)
    gg_plt <- switch(type,
      rank = gg_plt +
        ggplot2::geom_point(ggplot2::aes(y = .data$rank, x = .data$depth)) +
        ggplot2::coord_cartesian(xlim = xl),
      named = gg_plt +
        ggplot2::geom_point(ggplot2::aes(y = .data$depth, x = .data$names)) +
        ggplot2::coord_cartesian(ylim = xl)
    )
  }


  if (type == "named") {
    if (!missing(lbls)) {
      if (length(lbls) >= length(vsel$names)) {
        st_lbls <- lbls[as.character(vsel$names)]
        names(st_lbls) <- as.character(vsel$names)
        st_lbls[which(is.na(st_lbls))] <-
          names(st_lbls[which(is.na(st_lbls))])

        gg_plt <- gg_plt +
          ggplot2::scale_x_discrete(labels = st_lbls)
      }
    }

    gg_plt <- gg_plt +
      ggplot2::labs(y = "Minimal Depth of a Variable", x = "")

    if (nvar > gg_dta$modelsize) {
      gg_plt <- gg_plt +
        ggplot2::geom_hline(yintercept = sel_th, lty = 2)
    }
    gg_plt <- gg_plt +
      ggplot2::labs(y = "Minimal Depth of a Variable", x = "") +
      ggplot2::coord_flip()
  } else {
    gg_plt <- gg_plt +
      ggplot2::labs(y = "Rank", x = "Minimal Depth of a Variable")

    if (nvar > gg_dta$modelsize) {
      gg_plt <- gg_plt +
        ggplot2::geom_vline(xintercept = sel_th, lty = 2)
    }
  }
  return(gg_plt)
}
