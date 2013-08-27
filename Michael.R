#setwd("C:/Users/phug7649/Desktop/kmeans/Paper_2/0-5")

#classes<-read.csv("f1.25 11_class.txt",sep="") ##this is the output from the fuzzy k analysis. Using 0-5 currently 
##as a testbed
Clusters<-11
setwd("C:/Users/phug7649/Desktop/txtbin")

##for the function to work optimally, the specific "subset" file needs to be changed to a generic "input" file. 
##I have created several input files based on the same data set.
#subset0_5<-read.csv("USII_0_5.csv")
input<-read.csv("USII_0_5.csv")
# input<-read.csv("USII_5_10.csv")
# input<-read.csv("USII_10_20.csv")
# input<-read.csv("USII_20_40.csv")
# input<-read.csv("USII_40_60.csv")
# input<-read.csv("USII_60_100.csv")
# input<-read.csv("USII_100plus.csv")

##this data needs the silt fraction removed. This may cause problems down the line
one<-input[,1:9]
two<-input[,11:12]
input<-cbind(one,two)


##reading in the results from FKM in its appropriate directory

setwd("C:/Users/phug7649/Desktop/kmeans/Paper_2/0-5")

clusfind<-read.csv("summary.txt",sep="", header=TRUE)
head(clusfind)

# Spinning 3d Scatterplot

##how do you rename a header? heres how!
#names(classes)[1]<-"natural_key"

#class_input<-merge(input,classes, by= "natural_key",all=TRUE)
y<-ncol(input)

##should have used "row.names=FALSE" when making this csv. I will fix the problem later.
input<-input[,2:y]# remove this when the issue is fixed.
y<-ncol(input)
a<-princomp(input[,2:y], cor=TRUE)
prin<-a$scores
#csprin<-cbind(class_input,prin)


####################################################################################################################
#################################### Time to use the script found in EMII.r ########################################
####################################################################################################################

#I need to make a function out of this...

############################################# THE CONVEX BICYCLE ###################################################

##A script made to identify a small number of points around the periphery of a data cloud. It works by creating an 
##n-dimensional convex hull (thanks seb),finding a point with a maximum distance from zero then finding the maximum 
##distance of this point from all other points in the hull.

####################################################################################################################





##constructing the dataset. Required: 1 column (column.30 with the components arranged after that.)
Column.30<-1:nrow(prin)
z<-cbind(Column.30,prin)
z<-z[,2:ncol(z)]

##scripts required for this algorithm to work...
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/point_euclid.R")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/qhull_algorithm.R")

################################################# control panel ####################################################
## there are two control methods atm; the first is to define the length of the yardstick. Provides an undefined number
## of end-members. the second is to use an equation which most likely is data specific.
ys<-10      ##starting parameter for yardstick
factor<-.55  ##creating the factor by which the yardstick length is modified (previous run was 0.8)
YScrit<-3.2   ##Stopping criteria; when the overall size of the hull is less than this, the algorithm stops.
####################################################################################################################
rm(bin)
file.create("bin.csv")##creating a file to dump values
bin<-c()
cz<-quick_hull(z)##Using sebs script to create hulls 


while (ys>YScrit)##I want the loop to start here
  
{
  
  czr<-z[cz,]##sum of rows
  czr<-czr^2
  czrsum<-rowSums(czr)
  fin<-sqrt(czrsum)
  finm<-as.matrix(fin)      
  refmax<-which.max(finm)##rows with max and min euclidean distance from zero 
  BLARG<-as.data.frame(z)[cz[refmax],]##getting maximum value and anchoring it to the row number in the master data set (z)    
  rowx<-BLARG##retrieving all the principal component data from rows that contain maximum and minimum euclidean distances  
  object<-z[cz,] ## retrieving all pc data from cz    
  pcdist<-as.matrix(point_euclid(object,rowx))##getting distances 
  b<-as.numeric(pcdist[which.max(pcdist),])##max distance
  ys<-b*factor##yardstick
  new <- ys < as.vector(pcdist)##compare yardstick to the convex hull  
  or <- cz[which(pcdist == 0)]##Placing maximum (maxi) and minimum (origin) points in the final file
  bin <- rbind(or,bin)    
  cz <- cz[which(new)]##Exclude any values inferior to yardstick (this file should be renamed cz when its time to reiterate)  
  print(ys) #print the yardstick value to see if the script is running  
  
}
paste0("your algorithm has returned ",nrow(bin), " end points")
paste0("Yardstick factor is ",factor,","," stopping criterion is ",YScrit)
ys<-10
####################################################################################################################

##creating an identity matrix
matrix<-diag(nrow(bin))
##creating end point matrices
points<-input[bin,]
#verify<-cbind(bin,points)

# #creating control file
# crow1<-c("weights","phi","nend","nclass")
# crow2<-c(w,p,ncol(bin),total)
# #writing files

setwd("C:\\Users\\phug7649\\Documents\\MATLAB")


write.table(matrix,"matrix.csv",row.names=FALSE,col.names=FALSE,sep=",")
write.csv(points,"EP.csv",row.names=FALSE)
write.csv(input,"DATA.csv",row.names=FALSE)

checkdata<-read.csv("edg_2072_ep_k_2072_II.csv")
head(checkdata)
data<-read.csv("DATA.csv")
head(data)
checkep<-read.csv("edg_2072_ep_k_5_II.csv")
head(checkep)
ep<-read.csv("EP.csv")
head(ep)
checkmatrix<-read.csv("edg_2073_ep_k_id.csv")
head(checkmatrix)
matII<-read.csv("matrix.csv")
head(matII)



message(paste0("nclass needs to be ", Clusters+nrow(bin)))

shell("matlab -nodesktop -nosplash -wait -r rep")