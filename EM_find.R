# This script is to obtain end members froom a matrix. The data will be in 10 columns, approximately 6000 rows.
# First order of business is to figure out how the hell to do it!! The first hint alex provided was to "spherify"
# the data. Papers document a procedure called "whitening" which is a reference to spectral colour (this process is 
# almost exclusively used for spectral images). The whitening process seems to be one in which the data is read in 
# in and the correlation matrix obtained, the means are removed and then multiplied by the inverse of the correlation 
# matrix.  Lots of confusion later, we will try to get the iddentity matrix

##1. Get data

setwd("C:/Users/phug7649/Desktop/TXTBIN")

y<-as.matrix(read.table("Carbon_comp_6094.txt", sep=",", na.strings="", header=TRUE))
x<-(y-colMeans(y))
a<-(cor(x))
c<-t(x)
b<-(solve(a))
d<-t(a)
#solution<-a%*%c
#solution2<-d%*%x
solution3<-x%*%a
g<-which.max(solution3)


