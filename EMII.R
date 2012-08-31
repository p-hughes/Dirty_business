


###Set working directory, read in the data###

setwd("C:/Users/phug7649/Desktop/TXTBIN")
#z<-read.table("Alex_Hull.txt", sep=",", na.strings="", header=TRUE)
#z<-read.table("1_15 22_161534_top_14410_princ.txt", sep=",", na.strings="", header=TRUE)
z<-read.table("Carbon_comp_6094.txt", sep=",", na.strings="", header=TRUE)
####Apply values to columns####
getwd()


source(file.path(getwd(), "R-scripts", "point_euclid.R"))
source(file.path(getwd(), "R-scripts", "qhull_algorithm.R"))


z<- na.exclude(z)
combs <- combn(seq_len(ncol(z)), 2)

##creating a file to dump values
file.create("bin.csv")
bin<-matrix(NA, 1,1) 
#names(bin)[2:3]<-c("row","distance")


  
 # cz <- chull(zna)
  cz<-quick_hull(z)
  
  ##added script for the "convex bicycle" 
  
  ##sum of rows
  
  czr<-z[cz,]
  czr<-czr^2
  czrsum<-rowSums(czr)
  fin<-sqrt(czrsum)
  
  
##I want the loop to start here

  finm<-as.matrix(fin)  
  
  ##rows with max and min euclidean distance from zero
  refmax<-(which.max(finm))
  refmin<-(which.min(finm))

  ##getting maximum value and anchoring it to the row number in the master data set (z)
  BLARG<-rownames(z[cz,])==cz[refmax]
  BLARG<-as.matrix(cz[BLARG])
    
  ##getting minimum point and anchoring it to row number in master data set (z)
  BLURG<-rownames(z[cz,])==cz[refmin]
  BLURG<-as.matrix(cz[BLURG])
  
  ##retrieving all the principal component data from rows that contain maximum and minimum euclidean distances
  rowx<-z[BLARG,]
  rowy<-z[BLURG,]
  
  ## retrieving all pc data from cz
  object<-z[cz,]
  
  ##getting distances
  pcdist<-as.matrix(point_euclid(object,rowx))
   
  ##creating the factor by which the yardstick length is modified
  factor<-.25
  
  ##yardstick
  b<-as.numeric(pcdist[which.max(pcdist),])
  ys<-b*factor
  
  ##compare yardstick to the convex hull
  new <- ys < as.vector(pcdist)
  new<-as.matrix(new)
  
  ##Placing maximum (maxi) and minimum (origin) points in the final file
  
  origin <- as.matrix(pcdist == 0)
  or <- as.matrix(pcdist[origin,])
  bin <- rbind(or,bin)
  
  max <- as.matrix(pcdist==b)
  maxi <- as.matrix(pcdist[max,])
  bin <-rbind(maxi,bin)

  
  ##Exclude any values inferior to yardstick (this file should be renamed cz when its time to reiterate)
  ans <- as.matrix(pcdist[new,])
  
  ##creating object for point max distance from origin
  pcdisty<-as.matrix(point_euclid(z[row.names(ans),],rowy))
  
  ##second wheel of bicycle

##compare yardstick to the convex hull
#pcdisty<-as.matrix(point_euclid(object,rowx))
#new <- ys < as.vector(pcdisty)
#new<-as.matrix(new)

objecty<- #first wheel with pc's'
  
  new <- ys < as.vector(pcdisty)
  new<-as.matrix(new)
  ans <- as.matrix(pcdisty[new,])






  
  
  
  ##making the loop start again
  
  finm<-ans
  czr<-z[row.names(finm),]
  czr<-czr^2
  czrsum<-rowSums(czr)
  fin<-sqrt(czrsum)


