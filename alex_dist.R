vince<-read.csv("C:\\Users\\phug7649\\Desktop\\TXTBIN\\CentInput.csv",sep=",",header=TRUE,skip=1)
vince.doub<-rbind(vince,vince)
#vince.comp<-princomp(vince.doub[2:ncol(vince)],cor=T)
vince.comp<-prcomp(vince[2:ncol(vince)],cor=T)

#vince.comp.scores<-vince.comp$scores
vince.comp.scores<-vince.comp$x
#vince.final<-vince.comp.scores[1:nrow(vince),]

##checking to see if rows and columns match up.

#check<-vince.doub[1:nrow(vince),]
#identical(check,vince)##checking to see if the doubling step had worked.
#spare<-as.data.frame(cbind(vince[,1],vince.final))
spare<-as.data.frame(cbind(vince[,1],vince.comp.scores[,c(1:20)]))


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

str(vince.comp)

#spare<-vince  ##so I can bugger up the object (vince) and not worry
spare<-scale(spare[2:ncol(spare)])
#re.fac<-spare[,2:ncol(spare)]




## spare without the factor column
dist.vince<-dist(spare) ## distance matrix of all the soil groups
dc<-mean(dist.vince) ##mean of distances (I will use this as my completion criterion until told otherwise)
#dc<-10###i had better change this back after.

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
# sum<-1
# factor<-1
# b<-nrow(spare)
# spare1<-spare
concat<-as.character(vince[,1])
rownames(spare)=concat
pdf()

min.dist=0
#create a second object to mess with
tree<-as.matrix(spare[,1])

dismat<-as.matrix(dist(spare))
diag(dismat)<-NA
itnumber<-nrow(spare)
original<-spare
n<-"first iteration"
factor=.7
dc<-dc*factor

while (min.dist < dc){
  
  plot(spare[,1],spare[,2],xlim=c(-3,3),ylim=c(-3,5),main=n)
  #points(original[,1],original[,2],colour=9)
  min.dismat<-arrayInd(which.min(dismat),dim(dismat))
  
  itnumber<-itnumber-1#the number of possible remaining iterations
  message(itnumber)
 
  #spare[min.dismat[1],2:a]<-(spare[min.dismat[2],2:ncol(spare)]+spare[min.dismat[1],2:ncol(spare)])/2 
  # The new, averaged, centroid
  average<-(spare[min.dismat[1],]+spare[min.dismat[2],])/2
  
  
  source1<-(spare[min.dismat[1],])
  source2<-(spare[min.dismat[2],])
    
  points(source1[1],source1[2],col="blue")
  points(source2[1],source2[2],col="blue")
  
  points(average[1],average[2],col=10)
  n=colnames(dismat)[min.dismat]
  
  #creating the tree database---this bit is broken!
  tree.1<-
#   n.2=colnames(dismat)[min.dismat[2]]
  
  #remove the old rows
  spare=spare[!rownames(spare) %in% n,]
#   spare=spare[!rownames(spare) %in% n.1,]
  
  #rbind the new centroid to spare
  new.name<-paste0(n, collapse="-")
  #new.name<-paste0(concat[min.dismat[1]],"-",concat[min.dismat[2]])
  spare=rbind(spare, average)
  rownames(spare)[nrow(spare)] <- new.name

  
  
  
  ###spare[min.dismat[1],]<-average##check for a factor column, if there is one, you have to start at column 2
  
  #spare[min.dismat[1],2:ncol(spare)]<-(re.fac[min.dismat[2],]+re.fac[min.dismat[1],])/2#average of closest points in euclidean space
  
  dismat<-as.matrix(dist(spare))
  diag(dismat)<-NA

  min.dist=min(dismat, na.rm=TRUE)
  message(min.dist) 
  points(average[1],average[2],col=10)
 #dismat<-as.matrix(dist(spare)) 
  message(n)
}



# #120,261
# 
# ##this algorithm is banging the previous concatinated object into the line instead of the new row.
dev.off()
# nrow(spare)
# 
# ##Alex wants to concatinate the names of the soil orders so he can see what is related to what.
# 
# ##I want to plot the points next to the original data or create some kind of distance matrix.
# 
# 
# 
# #  plot(vince.final[,1],vince.final[,2])
# #  points(spare[,1],spare[,2],col=10)
# 
# ##USE PASTE0!!!!!!!!!!!!!!!!
# 
# 
# ##budi stuff
# 
# variance<-(vince.comp$sdev)^2
# cumsum(variance)
# perc.variance<-cumsum(variance)/58494.13
#  plot(perc.variance)
#  perc.variance<-cumsum(variance)/58494.13
#  plot(vince[,2],vince[,3])
#  fix(perc.variance)
# screeplot(vince.comp)