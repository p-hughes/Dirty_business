
                                              ##THE CONVEX BICYCLE##
##A script made to identify a small number of points around the periphery of a data cloud. This should coincide with
##the location of end members. It works by creating an n-dimensional convex hull (thanks seb),finding a point with a 
##maximum distance from zero then finding the maximum distance of this point from all other points in the hull.


###Set working directory, read in the data###

setwd("C:/Users/phug7649/Desktop/TXTBIN")
#z<-read.table("Alex_Hull.txt", sep=",", na.strings="", header=TRUE)
#z<-read.table("1_15 22_161534_top_14410_princ.txt", sep=",", na.strings="", header=TRUE)
z<-read.table("Carbon_comp_6094.txt", sep=",", na.strings="", header=TRUE)
####Apply values to columns####

source(file.path(getwd(), "R-scripts", "point_euclid.R"))
source(file.path(getwd(), "R-scripts", "qhull_algorithm.R"))


z<- na.exclude(z)

##creating a file to dump values
file.create("bin.csv")
bin<-matrix(NA, 1,1) 

##Using sebs script to create hulls
cz<-quick_hull(z)                               ############
                                                ##CONTROLS##
                                                ############
## there are two control methods atm; the first is to define the length of the yardstick. Provides an undefined number
## of end-members. the second is to use an equation which most likely is data specific.

##x is the number of end members you want
eq1 <- function (value) {exp((value + 4.7671)/-64.85)}

factor<-eq1(20)

ys<-10      ##starting parameter for yardstick
#factor<-1  ##creating the factor by which the yardstick length is modified
  
##I want the loop to start here

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

  ##getting maximum value and anchoring it to the row number in the master data set (z)
  BLARG<-rownames(z[cz,])==cz[refmax]
  BLARG<-as.matrix(cz[BLARG])
  
  ##retrieving all the principal component data from rows that contain maximum and minimum euclidean distances
  rowx<-z[BLARG,]
  #rowy<-z[BLURG,]
  
  ## retrieving all pc data from cz 
  object<-z[cz,]
  #object<-z[finm,]
  
  ##getting distances
  pcdist<-as.matrix(point_euclid(object,rowx))
  #pcdisty<-as.matrix(point_euclid(object,rowy))
  
  ##yardstick
  b<-as.numeric(pcdist[which.max(pcdist),])
  ys<-b*factor 
  
  ##compare yardstick to the convex hull
  new <- ys < as.vector(pcdist)
  new <- as.matrix(new)
  
  ##Placing maximum (maxi) and minimum (origin) points in the final file
  
  origin <- as.matrix(pcdist == 0)
  or <- as.matrix(pcdist[origin,])
  bin <- rbind(or,bin)
  
  max <- as.matrix(pcdist==b)
  maxi <- as.matrix(pcdist[max,])
  bin <-rbind(maxi,bin)

  
  ##Exclude any values inferior to yardstick (this file should be renamed cz when its time to reiterate)
  finm <- as.matrix(pcdist[new,])
  
  ##Create a new object to replace the previous convex hull list
  cz<-as.numeric(rownames(finm))
  
  print(ys)
  
  
}

## removing duplicates
a<-rownames(bin)
s<-as.matrix(unique(a))

##Output

write.csv(s, file = "ems.csv")




