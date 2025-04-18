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
#' plot.gg_interaction
#' Plot a \code{\link{gg_interaction}} object,
#'
#' @param x gg_interaction object created from a
#' \code{\link[randomForestSRC]{rfsrc}} object
#' @param xvar variable (or list of variables) of interest.
#' @param lbls A vector of alternative variable names.
#' @param ... arguments passed to the \code{\link{gg_interaction}} function.
#'
#' @return \code{ggplot} object
#'
#' @seealso \code{\link[randomForestSRC]{rfsrc}}
#' \code{\link[randomForestSRC]{find.interaction}}
#' \code{\link[randomForestSRC]{max.subtree}}
#' \code{\link[randomForestSRC]{var.select}}
#' \code{\link[randomForestSRC]{vimp}}
#' \code{\link{plot.gg_interaction}}
#'
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#'
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for
#' R, Rnews, 7(2):25-31.
#'
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival,
#' Regression and Classification (RF-SRC), R package version 1.4.
#'
#' @examples
#' \dontrun{
#' ## Examples from randomForestSRC package...
#' ## ------------------------------------------------------------
#' ## find interactions, classification setting
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' ## iris.obj <- rfsrc(Species ~., data = iris)
#' ## TODO: VIMP interactions not handled yet....
#' ## find.interaction(iris.obj, method = "vimp", nrep = 3)
#' ## interaction_iris <- find.interaction(iris.obj)
#' data(interaction_iris, package = "ggRandomForests")
#' gg_dta <- gg_interaction(interaction_iris)
#'
#' plot(gg_dta, xvar = "Petal.Width")
#' plot(gg_dta, xvar = "Petal.Length")
#' plot(gg_dta, panel = TRUE)
#'
#' ## ------------------------------------------------------------
#' ## find interactions, regression setting
#' ## ------------------------------------------------------------
#' ## -------- air quality data
#' ## airq.obj <- rfsrc(Ozone ~ ., data = airquality)
#' ##
#' ## TODO: VIMP interactions not handled yet....
#' ## find.interaction(airq.obj, method = "vimp", nrep = 3)
#' ## interaction_airq <- find.interaction(airq.obj)
#' data(interaction_airq, package = "ggRandomForests")
#' gg_dta <- gg_interaction(interaction_airq)
#'
#' plot(gg_dta, xvar = "Temp")
#' plot(gg_dta, xvar = "Solar.R")
#' plot(gg_dta, panel = TRUE)
#'
#' ## -------- Boston data
#' data(interaction_boston, package = "ggRandomForests")
#' gg_dta <- gg_interaction(interaction_boston)
#'
#' plot(gg_dta, panel = TRUE)
#'
#' ## -------- mtcars data
#' data(interaction_mtcars, package = "ggRandomForests")
#' gg_dta <- gg_interaction(interaction_mtcars)
#'
#' plot(gg_dta, panel = TRUE)
#'
#' ## ------------------------------------------------------------
#' ## find interactions, survival setting
#' ## ------------------------------------------------------------
#' ## -------- pbc data
#' ## data(pbc, package = "randomForestSRC")
#' ## pbc.obj <- rfsrc(Surv(days,status) ~ ., pbc, nsplit = 10)
#' ## interaction_pbc <- find.interaction(pbc.obj, nvar = 8)
#' data(interaction_pbc, package = "ggRandomForests")
#' gg_dta <- gg_interaction(interaction_pbc)
#'
#' plot(gg_dta, xvar = "bili")
#' plot(gg_dta, xvar = "copper")
#' plot(gg_dta, panel = TRUE)
#'
#' ## -------- veteran data
#' data(interaction_veteran, package = "ggRandomForests")
#' gg_dta <- gg_interaction(interaction_veteran)
#'
#' plot(gg_dta, panel = TRUE)
#' }
#'
#' @export
plot.gg_interaction <- function(x, xvar, lbls, ...) {
  object <- x
  if (is.matrix(x)) {
    # Check to make sure it's the right type of matrix...
    if (sum(colnames(x) != rownames(x)) > 0) {
      stop("gg_interaction expects a rfsrc object, or a
           find.interaction object.")
    }
  }

  if (!inherits(object, "gg_interaction")) {
    object <- gg_interaction(x, ...)
  }

  if (missing(xvar)) {
    xvar <- rownames(object)
  }

  if (sum(xvar %in% rownames(object)) == 0) {
    stop(paste(
      "Invalid xvar (",
      xvar,
      ") specified, covariate not found.",
      sep = ""
    ))
  }

  if (length(xvar) > 1) {
    gg_dta <- data.frame(t(object[which(rownames(object) %in% xvar), ]))

    gg_dta$names <- rownames(object)
    gg_dta$rank <- seq_len(dim(gg_dta)[1])

    gathercols <-
      colnames(gg_dta)[-which(colnames(gg_dta) %in% c("rank", "names"))]
    gg_dta <- tidyr::gather(
      gg_dta, "variable", "value",
      tidyr::all_of(gathercols)
    )

    # Correct ordering: Issue #32 code contributed by afb206
    gg_dta$variable <-
      factor(gg_dta$variable, levels = unique(gg_dta$variable))

    gg_dta$value <- as.numeric(gg_dta$value)
    gg_dta$names <- factor(gg_dta$names,
      levels = unique(gg_dta$names)
    )
    gg_plt <- ggplot2::ggplot(gg_dta) +
      ggplot2::geom_point(ggplot2::aes(x = "names", y = "value")) +
      ggplot2::geom_point(
        ggplot2::aes(x = "names", y = "value"),
        data = gg_dta[which(as.character(gg_dta$names) == gg_dta$variable), ],
        shape = 3,
        size = 5,
        color = "red"
      ) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 10),
        axis.text.x = ggplot2::element_text(angle = 90)
      ) +
      ggplot2::labs(x = "", y = "Interactive Minimal Depth")
    if (!missing(lbls)) {
      if (length(lbls) >= length(colnames(object))) {
        st_lbls <- lbls[colnames(object)]
        names(st_lbls) <- colnames(object)
        st_lbls[which(is.na(st_lbls))] <-
          names(st_lbls[which(is.na(st_lbls))])

        gg_plt <- gg_plt +
          ggplot2::scale_x_discrete(labels = st_lbls)
      }
    }


    gg_plt + ggplot2::facet_wrap(~variable)
  } else {
    gg_dta <- data.frame(cbind(
      rank = seq_len(dim(object)[1]),
      t(object[which(rownames(object) %in% xvar), ])
    ))
    colnames(gg_dta)[2] <- "dpth"
    gg_dta$names <- rownames(gg_dta)

    gg_dta$dpth <- as.numeric(gg_dta$dpth)
    gg_dta$names <- factor(gg_dta$names,
      levels = unique(gg_dta$names)
    )
    gg_plt <- ggplot2::ggplot(gg_dta) +
      ggplot2::geom_point(ggplot2::aes(x = "names", y = "dpth")) +
      ggplot2::geom_point(
        ggplot2::aes(x = "names", y = "dpth"),
        data = gg_dta[which(rownames(gg_dta) == xvar), ],
        shape = 3,
        size = 5,
        color = "red"
      ) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 10),
        axis.text.x = ggplot2::element_text(angle = 90)
      ) +
      ggplot2::labs(x = "", y = "Interactive Minimal Depth")

    if (!missing(lbls)) {
      if (length(lbls) >= length(gg_dta$names)) {
        st_lbls <- lbls[as.character(gg_dta$names)]
        names(st_lbls) <- as.character(gg_dta$names)
        st_lbls[which(is.na(st_lbls))] <-
          names(st_lbls[which(is.na(st_lbls))])

        gg_plt <- gg_plt +
          ggplot2::scale_x_discrete(labels = st_lbls)
      }
    }
    gg_plt
  }
}
