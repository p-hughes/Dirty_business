
                                              ##THE CONVEX BICYCLE##
##A script made to identify a small number of points around the periphery of a data cloud. This should coincide with
##the location of end members. It works by creating an n-dimensional convex hull (thanks seb),finding a point with a 
##maximum distance from zero then finding the maximum distance of this point from all other points in the hull.


###Set working directory, read in the data###

setwd("C:/Users/phug7649/Desktop/TXTBIN")
#z<-read.table("Alex_Hull.txt", sep=",", na.strings="", header=TRUE)
#z<-read.table("1_15 22_161534_top_14410_princ.txt", sep=",", na.strings="", header=TRUE)
#z<-read.table("Carbon_comp_6094.txt", sep=",", na.strings="", header=TRUE)
z<-read.table("matcomp.txt", sep=",", na.strings="", header=TRUE)
##test data

#y<-read.csv("random.csv", header=FALSE)


#e<-g
#d<-runif(6027,min=0,max=1)
#g[,1]<-d
#d<-runif(6027,min=0,max=1)
# y[,2]<-d
# d<-runif(6027,min=0,max=1)
# y[,3]<-d
# d<-runif(6027,min=0,max=1)
# y[,4]<-d
# d<-runif(6027,min=0,max=1)
# y[,5]<-d
# d<-runif(6027,min=0,max=1)
# y[,6]<-d
# d<-runif(6027,min=0,max=1)
# y[,7]<-d
# d<-runif(6027,min=0,max=1)
# y[,8]<-d
# d<-runif(6027,min=0,max=1)
# y[,9]<-d
# d<-runif(6027,min=0,max=1)
# y[,10]<-d

# x<-as.matrix(quick_hull(y))
# plot(y[,1],y[,2])
# w<-y[x,]
# write.csv(w, file = "testhull.csv")
# write.csv(y, file = "testdata.csv")
#View(as.matrix(cz))
#czm<-as.matrix(z[cz,])
#write.csv(czm,file="czm.csv")


##end test


  
  ####Apply values to columns####

source("./functions/point_euclid.R")
source("./functions/qhull_algorithm.R")


z<- na.exclude(z)


i<-1
z[,11]<-rowSums(z[,1:10])/10
z[,12]<-z[,1]-z[,11]
ans1<-which.max(z[,12])
z[6023,]
which.max(z[,2])



##creating a file to dump values
file.create("bin.csv")
bin<-matrix(NA, 1,1) 

##Using sebs script to create hulls
cz<-quick_hull(z)                               ############
                                                ##CONTROLS##
                                                ############
## there are two control methods atm; the first is to define the length of the yardstick. Provides an undefined number
## of end-members. the second is to use an equation which most likely is data specific.

##While the script below runs, the number in "eq1" is the number of end members the algorythm gets (approximately)

# eq1 <- function (value) {exp((value + 4.7671)/-64.85)}
# factor<-eq1(35)

ys<-10      ##starting parameter for yardstick
factor<-.2  ##creating the factor by which the yardstick length is modified
  
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
  
#    max <- as.matrix(pcdist==b)
#    maxi <- as.matrix(pcdist[max,])
#    bin <-rbind(maxi,bin)

  
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


sz<-z[s,]
write.csv(sz,file="sz.csv")
##test accuracy of hull. If data is greater than any hull points 

##matlab code

