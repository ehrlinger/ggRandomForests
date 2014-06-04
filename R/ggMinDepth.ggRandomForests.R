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
#' ggMinDepth
#' Plot minimal depth values from an RF-S object.  
#'
#' @param rfObject An object of class (rfsrc, grow) or (rfsrc, predict).
#' 
#' @details If subset is not specified, generates the following three plots
#'  (going from top to bottom, left to right):
#' 
#' 
#' Invisibly, the conditional and unconditional Brier scores, and 
#' the integrated Brier score (if they are available).
#' 
#' @export ggMinDepth.ggRandomForests 
#' @export ggMinDepth
#' 
ggMinDepth.ggRandomForests <- function (rfObject){
  if(rfObject){
    vSel <- var.select(rf.obj)
    save(vSel, file=mnDpthFilename)
  }else
    load(mnDpthFilename)
  
  xl <-range(vSel$varselect$depth)
  xl<- c(floor(xl[1]) -1, ceiling(xl[2])+1)
  
  plt.MDall <- ggplot(vSel$varselect)+ 
    geom_point(aes(y=1:dim(vSel$varselect)[1], x=depth)) + 
    labs(title="A.",y="Rank", x="Minimal Depth of a Variable (All Variables)")+
    geom_vline(aes(xintercept=vSel$md.obj$threshold), lty=2)+
    coord_cartesian(x=xl)+#, ylim=c(0,55))+
    theme(plot.title = element_text(face = "bold", hjust = 0)) 
  
  modelSize<-vSel$modelsize
  
  # Labels for the top md vars.
  md.labs <- vSel$topvars
  
  ## Number the variables
  for(ind in 1:length(md.labs)){
    md.labs[ind] <- paste(ind, md.labs[ind], sep=". ")
  }
  
  plt.MDsel <- ggplot(vSel$varselect[1:modelSize,])+ 
    geom_point(aes(y=1:modelSize, x=depth, label=1:modelSize)) + 
    geom_text(aes(y=1:modelSize, x=depth-.7, label=1:modelSize), size=3, hjust=0)+
    labs(title="B.",y="Rank", x="Minimal Depth of a Variable")+
    #scale_y_continuous(breaks=seq(0,35,1))+
    #scale_x_continuous(breaks=seq(0,13,1))+
    geom_vline(aes(xintercept=vSel$md.obj$threshold), lty=2)+
    coord_cartesian(x=xl)+#, y=c(0,20))+
    #  geom_rect(colour="black",fill="white",aes(xmin=5.2,xmax=9.5,ymin=2,ymax=12))+
    theme(plot.title = element_text(face = "bold", hjust = 0)) 
  
  # Populate the text box with min.depth ordered labels
  yPlace= modelSize - 1
  for(ind in 1:length(md.labs)){
    plt.MDsel <- plt.MDsel +
      annotate(x=round(vSel$md.obj$threshold)+2,y=yPlace,geom="text",
               label=md.labs[ind], hjust=0,size=3)
    yPlace <- yPlace - .9
  }
  
  # Display graphic
  grid.arrange(plt.MDall ,plt.MDsel, nrow=2, heights=c(.75,2) )
  
}
ggMinDepth<-ggMinDepth.ggRandomForests                         