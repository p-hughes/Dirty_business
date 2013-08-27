######THIS SHOULD BE A FUNCTION!!#####
am()

file<-read.csv("USnoa20170709_60065.csv",sep=",", header=T)

aaaa<-am(file)

am <- function (input,ys=10,factor=.35,YScrit=6,Clusters,phi) {
  if (class(try(Clusters))=='try-error') stop('Define number of clusters')
  if (class(try(phi))=='try-error') stop('Define phi')
  
  y<-ncol(input)
  
  ##should have used "row.names=FALSE" when making this csv. I will fix the problem later.
  #input<-input[,2:y]# remove this when the issue is fixed.
  #any(sapply(input, is.infinite))
  if(sum(is.na(input)) > 0) warning(paste0("There exist(s) ", sum(is.na(input)), " NA(s) in your data."))
  input<-na.exclude(input)
  str(input)
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


message(paste0("nclass needs to be ", Clusters+nrow(bin)))
}
shell("matlab -nodesktop -nosplash -wait -r rep")

############################################################################################################################################
##############################################  PUTTING DATA TOGETHER / MAKING CENTROID TABLE  #############################################

##this will turn matlab output into a pretty graph without having to think about it.
# install.packages(c("Cairo"), repos="http://cran.r-project.org" )
library(Cairo)
library(plyr)
library(ggplot2)
library(grid)
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/make_letter_ids.R")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/which.greater.r")
##input files for matlab:

setwd("C:\\Users\\phug7649\\Documents\\MATLAB")
matrix<-read.table("matrix.csv",sep=",")
end_points<-read.csv("EP.csv",sep=",")
data<-read.csv("DATA.csv",sep=",")
weighting_factor<-read.csv("weighting.csv",sep=",")
number_of_rows<-read.csv("rows.csv",header=FALSE,sep=",")
number_of_end_members<-read.csv("end.csv",header=FALSE,sep=",")
number_of_centroids<-read.csv("cent.csv",header=FALSE,sep=",")
data_distances<-read.csv("mdist.csv",sep=",",header=F)  
w<-weighting_factor[1,1]
##matlab output files:
centroid_table<-read.csv("mcent.csv",header=FALSE,sep=",")
a<-nrow(centroid_table)
mat.max<-make_letter_ids(nrow(matrix), letters[6:26])
number.of.end.points<-number_of_centroids-number_of_end_members


greek.alpha<-c("alpha","beta","gamma","delta","epsilon","zeta","eta","theta","iota","kappa","lambda","mu","nu","xi",
               "omicron","pi","rho","sigma","tau","upsilon","phi","chi","psi","omega")

cent.max<-make_letter_ids(unlist(number_of_centroids)-nrow(matrix),greek.alpha)
max<-c(mat.max,cent.max)
names(data_distances)<-max


# max<-make_letter_ids(nrow(centroid_table))
centroid_table<-cbind(max,centroid_table)
##joining original data to the centroid table, to be used later.
names(centroid_table) <- names(data)
data_cent<-rbind(data,centroid_table)
##creating principal components from the original data.
princomp_main<-princomp(data_cent[,2:ncol(data)],cor=TRUE)
princomp_comp<-princomp_main$scores
##attaching principal components to main data, plotting to ensure we know what it looks like.
#plot(princomp_comp[,1],princomp_comp[,2])

##creating max distance column

id.matrix<-diag(nrow(centroid_table))
id.matrix<-cbind(id.matrix,max)
natural_key<-max
data_cent.prin<-cbind(data_cent,princomp_comp)





##create the id matrix from the matlab data

# which.greater(data_distances[1,1:29], 0.1)
# which(data_distances[1,-ncol(data_distances)]>.5)
##creating max column for data distances

##replacing "which.max" with "which.greater"

aa<-as.matrix(data_distances)
data_distances$50<-apply(aa,1,which.greater, n=0.5)
data_distances$max<-apply(aa,1,which.max)
data_ratio<-data_distances
data_ratio$50<-apply(aa,1,which.greater, n=0.5)
data_ratio$max<-apply(aa,1,which.max)
mat.max<-make_letter_ids(nrow(matrix), letters[6:26])


greek.alpha<-c("alpha","beta","gamma","delta","epsilon","zeta","eta","theta","iota","kappa","lambda","mu","nu","xi",
               "omicron","pi","rho","sigma","tau","upsilon","phi","chi","psi","omega")

cent.max<-make_letter_ids(unlist(number_of_centroids)-nrow(matrix),greek.alpha)
max<-c(mat.max,cent.max)
#max<-make_letter_ids(nrow(centroid_table))
data_distances$max<-max[data_distances$max]
max<-data_distances$max

#alexmax<-c(mat.max,cent.max)
#max<-rbind(c(max,y))
max<-t(max)
max<-c(max,mat.max,cent.max)


data.complete<-cbind(data_cent.prin,max)
datarows<-nrow(data)
position1<-datarows+1
cdatarows<-nrow(data.complete)
centroids.complete<-data.complete[position1:cdatarows,]
emno<-number_of_end_members[1,1]
ceno<-nrow(centroid_table)-emno
soil.id<-rep(c("E", "C"), c(emno, ceno))
centroids.complete<-cbind(centroids.complete,soil.id)

###########################################################################################################################################
################################################### DETERMINING MEMBERSHIP RATIOS  ########################################################

totals<-as.data.frame(table(data_ratio$max))
end.tot<-totals[1:nrow(matrix),]
cent.tot<-totals[nrow(matrix):nrow(totals),]
sum.end<-sum(end.tot[,2])
sum.cent<-sum(cent.tot[,2])  
ratio<-(sum.end/(sum.end+sum.cent))*100
ratio<-round(ratio,digits=2)
(paste0("Weighting ", weighting_factor[1,1],", creating ",ratio, "% end point memberships"))

############################################################################################################################################
#################################################### CREATING PLOTS  #######################################################################
setwd("C:/Users/phug7649/Desktop/txtbin")

###SEB GOING NUTS (more often referred to as a panel plot)
NUTS<-ggplot(data.complete, aes(x=Comp.1, y=Comp.2), group=max)+
  theme_bw() +
  geom_point(colour="grey40")+
  #stat_bin2d(binwidth=c(1, 1),colour=gray) +
  facet_wrap(~ max, nrow=5)+
  geom_point(data=centroids.complete, aes(shape=soil.id),size=4, colour="black")+  
  scale_shape_manual(values=c(16, 17))+
  #theme(plot.background = element_rect(fill = w))+
  #    xlim(-12,8)+
  #    ylim(-7.5,5)+
  
  ggtitle(paste0("Weighting ", weighting_factor[1,1],", creating ",ratio, " percent end point memberships"))+
  # theme(panel.margin = unit(5, "lines"))+
  coord_equal()

NUTS