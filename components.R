# This script is to obtain end members froom a matrix. The data will be in 10 columns, approximately 6000 rows.
# First order of business is to figure out how the hell to do it!! The first hint alex provided was to "spherify"
# the data. Papers document a procedure called "whitening" which is a reference to spectral colour (this process is 
# almost exclusively used for spectral images). The whitening process seems to be one in which the data is read in 
# in and the correlation matrix obtained, the means are removed and then multiplied by the inverse of the correlation 
# matrix.  Lots of confusion later, we will try to get the iddentity matrix



##make life easier for me

##take data matrix, find pca find minimum and max for each component. Identify ponts, plot graph.


invert<-solve

##1. Get data

setwd("C:/Users/phug7649/Desktop/TXTBIN")
data<-read.table("whiten.txt", sep=",", na.strings="", header=TRUE)
y<-as.matrix(data[,2:ncol(data)])

##2. remove means and multiply by the inverse of the corrolation matrix.

#x<-(y-colMeans(y))
#a<-(cov(x))
#aa<-solve(a)
##a<-(cor(x))
#sol3cov<-cov(solution3)

#z<-svd(x)
#str(z)

#test<-a%*%aa ##correlation matrix?

#d<-z$ d
#u<-z$ u
#v<-z$ v



#solution3<-x%*%aa


##creating principle components
prin<-princomp(y, cor=TRUE)
class(prin)
components<-prin$scores

##finding maximum and minimum values
cmin<-apply(as.matrix(na.exclude(components)),2,min)
compmin<-apply(as.matrix(na.exclude(components)),2,which.min)
cmax<-apply(as.matrix(na.exclude(components)),2,max)
compmax<-apply(as.matrix(na.exclude(components)),2,which.max)

##plotting components
plot(components)
for (i in 1:ncol(y)){
points(components[compmin[i],1],components[compmin[i],2],col="red")
}

for (i in 1:ncol(y)){
  points(components[compmax[i],1],components[compmax[i],2],col="blue")
}



out<-rdist(0,components[,1]) 
p1<-t(out)
which.max(p1)
p1[5017,]

     
     
##yay

## Need to get the furthest point from zero.
## Need to find the furthest point from that.

##borrowing code from munsellchip.

euc <- function(dat, a1,b1,c1,d1,e1,f1,g1,h1,i1,j1){
  
  if(any(length(x1)!=1,length(y1)!=1,length(z1)!=1)) stop("x1, y1, or z1 should be length 1")
 load.packages(fields)
  
  rdist(components)
  a<-components$Comp.1
  b<-dat$a.
  c<-dat$b.
  d<-dat$L.
  e<-dat$a.
  f<-dat$b.
  g<-dat$L.
  h<-dat$a.
  i<-dat$b.a<-dat$L.
  j<-dat$a.
  
  answer <- sqrt((x-x1)^2+(y-y1)^2+(z-z1)^2)
  
  return(answer)
  
}

#y <- matrix(NA, )
##refLAB <- munslab.df
Munsell <- rep(NA,nrow(ctrial))

for(i in 1:nrow(ctrial)){
  L <- ctrial$L[i]
  A <- ctrial$A[i]
  B <- ctrial$B[i]
  y <-euc(Reference,L,A,B)
  
  
  
  
  Munsell[i]<- which.min(y)
  
  
}






















