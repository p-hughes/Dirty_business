vince<-read.csv("C:\\Users\\phug7649\\Desktop\\TXTBIN\\CentInput.csv",sep=",",header=TRUE,skip=1)

#remove factors from vince

spare<-vince
re.fac<-spare[,2:ncol(spare)]

# mean<-as.data.frame(colMeans(vince[,2:ncol(vince)]))
# sx<-cov(cent.105.17[2:ncol(cent.105.17)])
# D2<-mahalanobis(cent.105.17[2:ncol(cent.105.17)],mean,sx)

arrayInd(which.max(dist(vince)),dim(dist(vince)))
dist(vince)


dist.vince<-dist(re.fac)
dist.vince[1:10]
which.min(dist.vince)
dist.vince[20887]

#For a set of k centroids, set a Mahalanobis distance cutoff dc- (mahalanobis is difficult for this data set)

## (dc will be the mean of the distances for the time being).


# 1. for each (i) of the k centroids find its closest neighbour (j) and its nearest neighbour distance dij. 
# (This basically a list with three columns) 2. For the smallest distance dij in the list , combine the two 
# centroids i and j by averaging them. Remove centroids I and j from the list and add a new centroid l with 
# a concatenated name from the names of I and j separated by a colon (:). There are now k-1 centroids.
# 
# 3. Repeat 1 and 2 until all nearest neighbour distances are larger than dc.


dc<-mean(dist.vince)

dismat<-as.matrix(dist(re.fac))
diag(dismat)<-NA
min.dismat<-arrayInd(which.min(dismat),dim(dismat))
max.dismat<-arrayInd(which.max(dismat),dim(dismat))
str(min.dismat)
min.dismat[1]
min.dismat[2]
re.fac[min.dismat[1],]<-mean(re.fac[min.dismat[1],],re.fac[min.dismat[2],])
re.fac[min.dismat[2],]<-NA


while (which.min(dismat)<dc),{
  
  
  
  
}


for (i in 1:nrow(vince)){
  j<-arrayInd(which.min(dist(vince)),dim(dist(vince)))
  m<-mean(j,i)
  rbind(m,data)}
  stop if m>dc
               }