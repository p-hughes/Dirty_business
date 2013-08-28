#' this function produces data that can be loaded into your root matlab directory, along with a control file 
#' which can then be shelled via the command: "shell("matlab -nodesktop -nosplash -wait -r am")"

akro <- function (input,Clusters,phi,distype,weight,ys=10,factor=.35,YScrit=6) {
  message("Thank you for using the acromeson function.") 
  if (missing(Clusters)) stop('You need Clusters, phi, distype and weight. Define number of clusters')
  if (class(try(phi))=='try-error') stop('You need Clusters, phi, distype and weight. Define phi')
  if (missing(distype)) stop('You need Clusters, phi, distype and weight. Define distype. 1=euclidean, 2=diagonal, 3=mahalonobis')
  if (missing(weight)) stop('You need Clusters, phi, distype and weight. Define weight')
  message(paste0("ys=", ys, ", factor=", factor, ", YScrit=", YScrit))
  y<-ncol(input)
  
  ##should have used "row.names=FALSE" when making this csv. I will fix the problem later.
  #input<-input[,2:y]# remove this when the issue is fixed.
  #any(sapply(input, is.infinite))
  if(sum(is.na(input)) > 0) warning(paste0("There exist(s) ", sum(is.na(input)), " NA(s) in your data."))
  input<-na.exclude(input)
  #str(input)
  y<-ncol(input)
  a<-princomp(input[,2:y], cor=TRUE)
  prin<-a$scores
  loadings<-a$loadings
  scale<-a$scale
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
  
  
  
  
  
  ##constructing the dataset. Required: 1 column (column.30 with the components arranged after that.) These make no sense!
  # Column.30<-1:nrow(prin)
  # z<-cbind(Column.30,prin)
  # z<-z[,2:ncol(z)]
  z<-prin
  
  ##scripts required for this algorithm to work...
  source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/point_euclid.R")
  source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/qhull_algorithm.R")
  
  
  ################################################# control panel ####################################################
  ## there are two control methods atm; the first is to define the length of the yardstick. Provides an undefined number
  ## of end-members. the second is to use an equation which most likely is data specific.
  #     ys<-10      ##starting parameter for yardstick
  #     factor<-.35  ##creating the factor by which the yardstick length is modified (previous run was 0.8)
  #     YScrit<-6   ##Stopping criteria; when the overall size of the hull is less than this, the algorithm stops.
  ####################################################################################################################
  #   rm(bin)
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
  #     ys<-10
  # c(bin)
  
  
  ###################################### Putting the matlab stuff together####################################################
  
  matrix<-diag(nrow(bin))
  ##creating end point matrices
  points<-input[bin,]
  
  ################################How many clusters have you decided on?######################################################
  
  #Clusters<-48
  ##writing files
  setwd("C:\\Users\\phug7649\\Documents\\MATLAB")
  write.table(matrix,"matrix.csv",row.names=FALSE,col.names=FALSE,sep=",")
  write.csv(points,"EP.csv",row.names=FALSE)
  write.csv(input,"DATA.csv",row.names=FALSE)
  
  nclass<-Clusters+nrow(bin)
  ep<-nrow(bin)
  
  # control<-c(ep,nclass,phi,weight,distype)
  akro.parameters<-c(ep,nclass,phi,weight,distype)
  akro.data<-list(matrix=matrix,points=points,input=input, parameters=akro.parameters)  
  ##put the output into akro.write
  message("put the output into akro.write")
  
  akro.data
}  