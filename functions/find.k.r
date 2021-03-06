##script to plot out the results from fuzzy k analysis


find.k <- function (directory=getwd(),size=3) {
  data <- read.csv('summary.txt',sep="")
  contrast<-colorRampPalette(c("orange3","orangered","orangered4","orchid4","palegreen4","palevioletred"))
  n.phi<-length(unique(data[['Phi']]))
  n.dots<-nrow(data)/n.phi
  library(rgl)
  with(data,plot3d(Class,FPI,Phi, main="FKM~phi,FPI and class",col=rep(contrast(n.phi), each=n.dots) , size=size))
}

