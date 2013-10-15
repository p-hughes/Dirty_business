vince<-read.csv("C:\\Users\\phug7649\\Desktop\\TXTBIN\\CentInput.csv",sep=",",header=TRUE,skip=1)

#remove factors from vince







#For a set of k centroids, set a Mahalanobis distance cutoff dc- (mahalanobis is difficult for this data set)

## (dc will be the mean of the distances for the time being).


# 1. for each (i) of the k centroids find its closest neighbour (j) and its nearest neighbour distance dij. 
# (This basically a list with three columns) 
#
# 2. For the smallest distance dij in the list , combine the two 
# centroids i and j by averaging them. Remove centroids I and j from the list and add a new centroid l with 
# a concatenated name from the names of I and j separated by a colon (:). There are now k-1 centroids.
# 
# 3. Repeat 1 and 2 until all nearest neighbour distances are larger than dc.

spare<-vince  ##so I can bugger up the object (vince) and not worry
spare<-scale(spare[2:ncol(spare)])
re.fac<-spare[,2:ncol(spare)]

## spare without the factor column
dist.vince<-dist(re.fac) ## distance matrix of all the soil groups
dc<-mean(dist.vince) ##mean of distances (I will use this as my completion criterion until told otherwise)

#arrayInd(which.max(dist(vince)),dim(dist(vince)))
#dist(vince)



# dismat<-as.matrix(dist(re.fac))
# diag(dismat)<-NA
# min.dismat<-arrayInd(which.min(dismat),dim(dismat))
# max.dismat<-arrayInd(which.max(dismat),dim(dismat))
# str(min.dismat)
# min.dismat[1]
# min.dismat[2]
# a<-ncol(spare)
# spare[min.dismat[1],2:a]<-(re.fac[min.dismat[1],]+re.fac[min.dismat[2],])/2
# spare[min.dismat[2],]<-NA
# spare<-na.exclude(spare)
# re.fac<-spare[,2:ncol(spare)]
# 
# nrow(spare)
# nrow(vince)
sum<-1
factor<-3
while (sum>0){
  
  dismat<-as.matrix(dist(re.fac))
  diag(dismat)<-NA
  min.dismat<-arrayInd(which.min(dismat),dim(dismat))
  max.dismat<-arrayInd(which.max(dismat),dim(dismat))
  str(min.dismat)
  min.dismat[1]
  min.dismat[2]
  a<-ncol(spare)
  spare[min.dismat[1],2:a]<-(re.fac[min.dismat[1],]+re.fac[min.dismat[2],])/2
  spare[min.dismat[2],]<-NA
  spare<-na.exclude(spare)
  re.fac<-spare[,2:ncol(spare)]
  dcII<-which.min(dismat)*factor
  sum<-dcII-dc
   
  
}
nrow(spare)


