


###Set working directory, read in the data###

setwd("C:/Users/phug7649/Desktop/TXTBIN")
#z<-read.table("Alex_Hull.txt", sep=",", na.strings="", header=TRUE)
#z<-read.table("1_15 22_161534_top_14410_princ.txt", sep=",", na.strings="", header=TRUE)
z<-read.table("Carbon_comp_6094.txt", sep=",", na.strings="", header=TRUE)
####Apply values to columns####
#getwd()


source(file.path(getwd(), "R-scripts", "point_euclid.R"))
source(file.path(getwd(), "R-scripts", "qhull_algorithm.R"))


z<- na.exclude(z)
#combs <- combn(seq_len(ncol(z)), 2)

##creating a file to dump values
file.create("bin.csv")
bin<-matrix(NA, 1,1) 
#names(bin)[2:3]<-c("row","distance")


  
 # cz <- chull(zna)
  cz<-quick_hull(z)
  
  ##added script for the "convex bicycle" 
  
 
  
  
##I want the loop to start here



ys<-10
while (ys>0)

{
  ##sum of rows
  
  czr<-z[cz,]
  czr<-czr^2
  czrsum<-rowSums(czr)
  fin<-sqrt(czrsum)
  finm<-as.matrix(fin)  
  
  ##rows with max and min euclidean distance from zero
  refmax<-(which.max(finm))
  #refmin<-(which.min(finm))

  ##getting maximum value and anchoring it to the row number in the master data set (z)
  BLARG<-rownames(z[cz,])==cz[refmax]
  BLARG<-as.matrix(cz[BLARG])
    
  ##getting minimum point and anchoring it to row number in master data set (z)
  #BLURG<-rownames(z[cz,])==cz[refmin]
  #BLURG<-as.matrix(cz[BLURG])
  
  ##retrieving all the principal component data from rows that contain maximum and minimum euclidean distances
  rowx<-z[BLARG,]
  #rowy<-z[BLURG,]
  
  ## retrieving all pc data from cz 
  object<-z[cz,]
  #object<-z[finm,]
  
  ##getting distances
  pcdist<-as.matrix(point_euclid(object,rowx))
  #pcdisty<-as.matrix(point_euclid(object,rowy))
   
  ##creating the factor by which the yardstick length is modified
  factor<-.5
  
  
  ##yardstick
  b<-as.numeric(pcdist[which.max(pcdist),])
  ys<-b*factor
  
  
  ##compare yardstick to the convex hull
  new <- ys < as.vector(pcdist)
  new <- as.matrix(new)
  
  #newy <- ys < as.vector(pcdisty)
  #newy <- as.matrix(newy)
  
  ##Placing maximum (maxi) and minimum (origin) points in the final file
  
  origin <- as.matrix(pcdist == 0)
  or <- as.matrix(pcdist[origin,])
  bin <- rbind(or,bin)
  
  max <- as.matrix(pcdist==b)
  maxi <- as.matrix(pcdist[max,])
  bin <-rbind(maxi,bin)

  
  ##Exclude any values inferior to yardstick (this file should be renamed cz when its time to reiterate)
  #ans <- as.matrix(pcdist[new,])
  finm <- as.matrix(pcdist[new,])
  
 
  cz<-as.numeric(rownames(finm))
  #cz<-cz[finm[,1]]
  
  
  
  ##Check step. Hash this out when the whole thing works.
  czo<-as.matrix(cz)
  
  ##same process, minimum values (second wheel).
  
  #czr<-z[cz,]
  #czr<-czr^2
  #czrsum<-rowSums(czr)
  #fin<-sqrt(czrsum)
  #finm<-as.matrix(fin)
  #refmin<-(which.min(finm))
  #BLURG<-rownames(z[cz,])==cz[refmin]
  #BLURG<-as.matrix(cz[BLURG])
  #rowy<-z[BLURG,]
  #pcdisty<-as.matrix(point_euclid(object,rowy))
  #newy <- ys < as.vector(pcdisty)
  #newy <- as.matrix(newy)
  #origin <- as.matrix(pcdisty == 0)
  #or <- as.matrix(pcdisty[origin,])
  #bin <- rbind(or,bin)
  #max <- as.matrix(pcdisty==b)
  #maxi <- as.matrix(pcdisty[max,])
  #bin <-rbind(maxi,bin)
  #finm <- as.matrix(pcdist[newy,])
  #cz<-as.numeric(rownames(finm))
  
  
  print(ys)
  
  
}
a<-rownames(bin)
s<-as.matrix(unique(a))



