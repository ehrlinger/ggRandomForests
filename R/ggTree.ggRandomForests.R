####**********************************************************************
####**********************************************************************
####
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
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#'
#' ggTree.ggRandomForests
#' Create a tree data structure from an randomForestSRC object
#' 
#' @param rfObj randomForestSRC object
#' @param tree return the data built from the number b=tree
#' @param depth only go to the depth level. Treat nodes at this level as terminal.
#' 
#' @return ggTree object
#' 
#' @export ggTree.ggRandomForests ggTree
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' @examples
#' 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' ggrf.obj<- ggTree(iris.obj)
#' plot(ggrf.obj)
#' 
ggTree.ggRandomForests <- function(rfObj, tree=1, depth, ...) {
  ## Check that the input obect is of the correct type.
  if (inherits(rfObj, "rfsrc") == FALSE){
    stop("This function only works for Forests grown with the randomForestSRC package.")
  }
  if (is.null(rfObj$forest)) {
    stop("The function requires the \"forest = TRUE\" attribute when growing the randomForest")
  }
  
  treeNodes <- as.data.frame(rfObj$forest$nativeArray)

  treeData <- treeNodes[which(treeNodes$treeID==tree),]
  treeData<- select(treeData, -treeID, -mwcpSZ, -contPT)
  
  # Create a set of unique node ID numbers.
  treeData$nodeNum <- 1:dim(treeData)[1]
  
  # Now connect nodes together.
  # Default set to connect to previous node. So 2 connects to 1 all the way down.
  treeData$parent <- c(0,1:(dim(treeData)[1]-1))
  
  prntCounter <- 1
  for(ind in treeData$nodeNum){
    if(treeData$parmID[ind] != 0){
      prntCounter <- prntCounter + 1
      treeData$parent[which(treeData$nodeID == prntCounter)[1]] <- ind
    }
  }
  
  prMat <- matrix(0, ncol=dim(treeData)[1], nrow=dim(treeData)[1])
  
  knct<- cbind(treeData$nodeNum, treeData$parent)[-1,]
  for(ind in 1:dim(knct)[1]){
    prMat[knct[ind,1], knct[ind,2]] <- 1
  }
  full <- graph.adjacency(as.matrix(prMat), mode="directed")
  
  lout <- layout.reingold.tilford(full)
  
  # We want a list of all connections between elements.
  edgeList <- data.frame(t(sapply(1:dim(treeData)[1], 
                                  function(ind){do.call(cbind,c(treeData[ind,],
                                                                lout[treeData$parent[ind],], 
                                                                lout[treeData$nodeNum[ind],]))})))
  
  colnames(edgeList) <- c("from", "to", "xFrom", "yFrom", "xTo", "yTo")
  
  mpGGplt<-ggplot(as.data.frame(lout))+ 
    geom_segment(aes(x=xFrom, xend=xTo, y=yFrom, yend=yTo, alpha=weight),
                 data=edgeList[which(edgeList$weight> .03),], color="darkgrey")+
    geom_point(aes(x=V1, y=V2), size=3)+labs(x="", y="")+
    theme(legend.position="none", axis.ticks = element_blank(), 
          axis.text.x = element_blank(), axis.text.y = element_blank())+
    scale_color_brewer(palette = "Set1")
#   
#   dta<-melt(error, id.vars = "indx")
#   
#   class(dta) <- c("ggTree",class(dta))
#   invisible(dta)
}

ggTree <- ggTree.ggRandomForests
