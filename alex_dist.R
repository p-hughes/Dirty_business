vince<-read.csv("C:\\Users\\phug7649\\Desktop\\TXTBIN\\CentInput.csv",sep=",",header=TRUE,skip=1)
vince.doub<-rbind(vince,vince)
vince.comp<-princomp(vince.doub[2:ncol(vince)],cor=T)
vince.comp.scores<-vince.comp$scores
vince.final<-vince.comp.scores[1:nrow(vince),]

##checking to see if rows and columns match up.

check<-vince.doub[1:nrow(vince),]
identical(check,vince)
spare<-as.data.frame(cbind(vince[,1],vince.final))


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



#spare<-vince  ##so I can bugger up the object (vince) and not worry
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
factor<-.05
b<-nrow(spare)
spare1<-spare
concat<-as.character(vince[,1])
pdf()
while (sum>0){
  
  dismat<-as.matrix(dist(re.fac))
  diag(dismat)<-NA
  min.dismat<-arrayInd(which.min(dismat),dim(dismat))
  max.dismat<-arrayInd(which.max(dismat),dim(dismat))
  str(min.dismat)
  min.dismat[1]
  min.dismat[2]
  a<-ncol(spare)
  spare[min.dismat[1],2:a]<-(re.fac[min.dismat[2],]+re.fac[min.dismat[1],])/2
  concat[min.dismat[1]] <- paste0(concat[min.dismat[2]], "-", concat[min.dismat[1]])
  
  spare[min.dismat[2],]<-NA
  concat[min.dismat[2]]<-NA
  
  
  spare<-na.exclude(spare)
  concat<-na.exclude(concat)
  
  re.fac<-spare[,2:ncol(spare)]
  #making an active plot
  plot(spare1[,1],spare1[,2],xlim=c(-3,3),ylim=c(-1,5))
  points(spare[,1],spare[,2],col=10)
  
  spare1<-spare
 
  dcII<-which.min(dismat)*factor
  sum<-dcII-dc
  b<-b-1
  message(b, " bottles of beer on the wall")
   
  
}
dev.off()
nrow(spare)

##Alex wants to concatinate the names of the soil orders so he can see what is related to what.

##I want to plot the points next to the original data or create some kind of distance matrix.



#  plot(vince.final[,1],vince.final[,2])
#  points(spare[,1],spare[,2],col=10)

##USE PASTE0!!!!!!!!!!!!!!!!