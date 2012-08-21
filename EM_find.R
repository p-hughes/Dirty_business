# This script is to obtain end members froom a matrix. The data will be in 10 columns, approximately 6000 rows.
# First order of business is to figure out how the hell to do it!! The first hint alex provided was to "spherify"
# the data. Papers document a procedure called "whitening" which is a reference to spectral colour (this process is 
# almost exclusively used for spectral images). The whitening process seems to be one in which the data is read in 
# in and the correlation matrix obtained, the means are removed and then multiplied by the inverse of the correlation 
# matrix.  Lots of confusion later, we will try to get the iddentity matrix

##Junk column
#c<-t(x)
#b<-(solve(a))
#d<-t(a)
#solution<-a%*%c
#solution2<-d%*%x
#g<-which.max(solution3)

##1. Get data

setwd("C:/Users/phug7649/Desktop/TXTBIN")
y<-as.matrix(read.table("whiten.txt", sep=",", na.strings="", header=TRUE))

##2. remove means and multiply by the inverse of the corrolation matrix.

x<-(y-colMeans(y))
a<-(cov(x))
aa<-solve(a)
#a<-(cor(x))
?cov


solution3<-x%*%aa

plot(solution3,main="solution3")
plot(y,main="original data")
str(solution3)
for (i in 1:10)
  

#sprin<- as.matrix(solution3 [,i])
assign(paste0('S3PRIN_', i), i)
head(y)
head(x)






