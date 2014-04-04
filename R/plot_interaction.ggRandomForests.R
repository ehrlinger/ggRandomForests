#' plot_interaction.ggRandomForests produces a heatmap of Find pairwise interactions between 
#' variables within a randomForestSRC object.
#' 
#' @param object a matrix object from the find.interaction.rfsrc function
#' @param nvar number of variables in plot (default full object matrix) 
#' @param color vector of two colors for heatmap (default c("white", "steelblue"))
#' 
#' @description plot_interaction produces a heatmap visualization for a matrix object 
#' returned from the find.interaction rfsrc function. Use the nvar argument to show 
#' only a subset of the matrix. 
#' 
#' For a maxsubtree interaction matrix, given the default coloring, lighter blocks 
#' indicate more dependence. The graph should be read along the rows, corresponding to
#' the subtree relation sorted into the row first.
#'  
#' @examples
#' airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' int.airq <- find.interaction(airq.obj)
#' pl<-plot_interaction.ggRandomForests(int.airq)
#' pl+theme_bw()
#' @import ggplot2 reshape2
#' @export plot_interaction.ggRandomForests

plot_interaction.ggRandomForests <- function(object, nvar, color){
  if(class(object) !="matrix") stop("plot_interaction expects an matrix object produced with the find_interaction function.")
  tst <- dim(object)
  
  if(tst[1]!= tst[2])stop("plot_interaction expects an square matrix object produced with the find_interaction function.")
  
  if(missing(nvar)) nvar = tst[1]
  
  if(nvar > tst[1]) nvar = tst[1]
  
  if(missing(color)) color <- topo.colors(5)
  
  if(length(color)==1) color <- c("white", color)
  
  # Build the working data.frame with a rownames column for labeling the axis. 
  interdata <- as.data.frame(cbind(rownames(object), as.data.frame(object)))[1:nvar, 1:(nvar+1)]
  
  colnames(interdata)[1]<-"Name"
  
  # Correct the order of the rownames
  interdata$Name <- factor(interdata$Name, levels<- rownames(object)[dim(object)[1]:1])
  interdata <- suppressMessages(melt(interdata))
  
  p <- ggplot(interdata, aes(Name, variable)) + geom_tile(aes(fill = value)) + 
    scale_fill_gradientn(colours=color) + 
    coord_flip()+
    labs(x=NULL, y=NULL)
  
#   pl+theme(legend.position = "none",
#            axis.ticks = element_blank(), 
#            axis.text.x = element_text(angle = 330, hjust = 0)))

  show(p)
  invisible(p)
}

plot_interaction <- plot_interaction.ggRandomForests