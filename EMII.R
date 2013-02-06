
                                              ##THE CONVEX BICYCLE##
##A script made to identify a small number of points around the periphery of a data cloud. This should coincide with
##the location of end members. It works by creating an n-dimensional convex hull (thanks seb),finding a point with a 
##maximum distance from zero then finding the maximum distance of this point from all other points in the hull.


###Set working directory, read in the data###

setwd("C:/Users/phug7649/Desktop/TXTBIN")
#z<-read.table("Alex_Hull.txt", sep=",", na.strings="", header=TRUE)
#z<-read.table("1_15 22_161534_top_14410_princ.txt", sep=",", na.strings="", header=TRUE)
#z<-read.table("Carbon_comp_6094.txt", sep=",", na.strings="", header=TRUE)
#z<-read.table("matcomp.txt", sep=",", na.strings="", header=TRUE)
#za<-read.table("extraprin.txt", sep=",", na.strings="", header=TRUE)
#z<-read.table("ed_sur_pc_319_197del.txt",sep=",", na.strings="",header=TRUE) ##row 197 is dodgy. I need to delete, then reorder rows.
#z<-read.table("Edgeroi_Fit_cut_PC_2073.txt",sep=",", na.strings="",header=TRUE)## removed row 1902- dodgy
z<-read.table("Edgeroi_Fit_cut_PC_2072.csv",sep=",", na.strings="",header=TRUE)
#z<-z[-1902,]

check1<-nrow(z)

      ####Apply values to columns####

source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/point_euclid.R")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/qhull_algorithm.R")

##errors sometimes occur if the rows don't line up. This fixes the problem at a cost of disorganising the data
z<- na.exclude(z)
row.names(z)<-NULL
check2<-nrow(z)

if(check1==check2) print("Rows are in order") else stop("rows are now disorganised. Do you wish to proceed?")

##checking the data is not disorganized.
nrow(z)
##the output number should be the same as the number of rows fed into the algorithm.


##Here you need to check you have the right number of principal components.
components<-13
b<-components+1
#z<-z[,2:11] 
z<-z[,2:b]
##creating a file to dump values
file.create("bin.csv")
#bin<-matrix(NA, 1,1)
bin<-c()

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
factor<-.65  ##creating the factor by which the yardstick length is modified (previous run was0.8)
YScrit<-8   ##Creating stopping parameter  (previous run was 8)


##I want the loop to start here

while (ys>YScrit)

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
#  bin <- c(or,bin)
  
  
#    max <- as.matrix(pcdist==b)
#    maxi <- as.matrix(pcdist[max,])
#    bin <-rbind(maxi,bin)

  
  ##Exclude any values inferior to yardstick (this file should be renamed cz when its time to reiterate)
  finm <- as.matrix(pcdist[new,])
  
  ##Create a new object to replace the previous convex hull list
  cz<-as.numeric(rownames(finm))
  
  print(ys)
  
  
}
paste0("your algorithm has returned ",nrow(bin), " end points")


# removing duplicates
a<-rownames(bin)
s<-as.matrix(unique(a))

##Output- row numbers only:
write.csv(s, file="ed_ep_II.csv")
write.csv(s, paste0('ep_',factor,'_',YScrit,'.csv'))

##output row numbers and principle components:
sz<-z[s,]
write.csv(sz,file="ed_ep.csv")

#making a set number of points based on a specific yardstick.
y<-sz[1:20,]
write.csv(y,file="bend.csv")

