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

##yay






















