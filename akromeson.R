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

##A script made to identify a small number of points around the periphery of a data cloud. This should coincide with
##the location of end members. It works by creating an n-dimensional convex hull (thanks seb),finding a point with a 
##maximum distance from zero then finding the maximum distance of this point from all other points in the hull.

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


##this will turn matlab output into a pretty graph without having to think about it.
# install.packages(c("Cairo"), repos="http://cran.r-project.org" )
library(Cairo)
library(plyr)
library(ggplot2)
library(grid)
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/make_letter_ids.R")

##input files for matlab:

setwd("C:\\Users\\phug7649\\Documents\\MATLAB")
matrix<-read.table("matrix.csv",sep=",")
end_points<-read.csv("EP.csv",sep=",")
data<-read.csv("DATA.csv",sep=",")

##matlab output files:
centroid_table<-read.csv("mcent.csv",header=FALSE,sep=",")
a<-nrow(centroid_table)
y<-make_letter_ids(nrow(centroid_table))
centroid_table<-cbind(y,centroid_table)
##joining original data to the centroid table, to be used later.
names(centroid_table) <- names(data)
data_cent<-rbind(data,centroid_table)
##creating principal components from the original data.
princomp_main<-princomp(data_cent[,2:ncol(data)],cor=TRUE)
princomp_comp<-princomp_main$scores
##attaching principal components to main data, plotting to ensure we know what it looks like.
#plot(princomp_comp[,1],princomp_comp[,2])
##creating max distance column
data_distances<-read.csv("mdist.csv",sep=",",header=F)  
id.matrix<-diag(nrow(centroid_table))
max<-y
id.matrix<-cbind(id.matrix,max)
natural_key<-max
data_cent.prin<-cbind(data_cent,princomp_comp)



names(data_distances)<-make_letter_ids(nrow(centroid_table))
weighting_factor<-read.csv("weighting.csv",sep=",")
number_of_rows<-read.csv("rows.csv",header=FALSE,sep=",")
number_of_end_members<-read.csv("end.csv",header=FALSE,sep=",")
number_of_centroids<-read.csv("cent.csv",header=FALSE,sep=",")
w<-weighting_factor[1,1]

##create the id matrix from the matlab data


##creating max column for data distances
aa<-as.matrix(data_distances)
data_distances$max<-apply(aa,1,which.max)
data_ratio<-data_distances
data_ratio$max<-apply(aa,1,which.max)
max<-make_letter_ids(nrow(centroid_table))
data_distances$max<-max[data_distances$max]
max<-data_distances$max

max<-rbind(c(max,y))
max<-t(max)


data.complete<-cbind(data_cent.prin,max)
datarows<-nrow(data)
position1<-datarows+1
cdatarows<-nrow(data.complete)
centroids.complete<-data.complete[position1:cdatarows,]
emno<-number_of_end_members[1,1]
ceno<-nrow(centroid_table)-emno
soil.id<-rep(c("E", "C"), c(emno, ceno))
centroids.complete<-cbind(centroids.complete,soil.id)


totals<-as.data.frame(table(data_ratio$max))
end.tot<-totals[1:nrow(matrix),]
cent.tot<-totals[nrow(matrix):nrow(totals),]
sum.end<-sum(end.tot[,2])
sum.cent<-sum(cent.tot[,2])  
ratio<-(sum.end/(sum.end+sum.cent))*100
ratio<-round(ratio,digits=2)
(paste0("Weighting ", weighting_factor[1,1],", creating ",ratio, "% end point memberships"))

setwd("C:/Users/phug7649/Desktop/txtbin")

###SEB GOING NUTS (more often referred to as a panel plot)
NUTS<-ggplot(data.complete, aes(x=Comp.1, y=Comp.2), group=max)+
  theme_bw() +
  geom_point(colour="grey40")+
  #stat_bin2d(binwidth=c(1, 1),colour=gray) +
  facet_wrap(~ max, nrow=5)+
  geom_point(data=centroids.complete, aes(shape=soil.id),size=4, colour="black")+  
  scale_shape_manual(values=c(16, 17))+
  theme(plot.background = element_rect(fill = w))+
  #    xlim(-12,8)+
  #    ylim(-7.5,5)+
  
  ggtitle(paste0("Weighting ", weighting_factor[1,1],", creating ",ratio, " percent end point memberships"))+
  # theme(panel.margin = unit(5, "lines"))+
  coord_equal()

NUTS

ggsave(paste0("nuts", w, number_of_end_members[1,1],".png"),type="cairo")



#Assign colours in one giant plot for Alex. 


#hclust(centroids.complete[,2:19])
#qplot(Comp.1,Comp.2,data=data.complete,colour=centroids.complete$soil.id)

combined<-ggplot(data.complete,aes(x=Comp.1,y=Comp.2))+
  geom_point(aes(colour=max))+
  geom_point(data=centroids.complete,aes(shape=soil.id),size=4)+
  scale_shape_manual(values=c(16,17))+
  #         xlim(-12,8)+
  #         ylim(-7.5,5)+
  ggtitle(paste0("Weighting ", weighting_factor[1,1],", creating ",ratio, " percent end point memberships"))+
  coord_equal()
combined
ggsave(paste0("combined", w, number_of_end_members[1,1],".png"),type="cairo")

##creating a plot of end point ratios-hard to put into function
# w<-c(5,10,20,40,100,200,400,1000,2000,4000)
# ratio<-c(55.87,48.89,35.17,35.25,19.89,11.3,6.81,5.36,5.02,4.85)
# percent<-cbind(w,ratio)
# plot(percent)

# E.C<-ggplot(data.complete,aes(x=Comp.1,y=Comp.2))+
#   geom_point(aes(colour=soil.id))+
#   geom_point(data=centroids.complete,aes(shape=soil.id),size=4)+
#   scale_shape_manual(values=c(16,17))+
#   #         xlim(-12,8)+
#   #         ylim(-7.5,5)+
#   ggtitle(paste0("Weighting ", weighting_factor[1,1],", creating ",ratio, " percent end point memberships"))+
#   coord_equal()
# E.C
# ggsave(paste0("combined", w, number_of_end_members[1,1],".png"),type="cairo")


##################################################################################################################
############################################### THE ALEX BIT #####################################################
##################################################################################################################

colours<-centroid_table[,5:7]
head(colours)
ctrial<-colours
Reference <- read.csv("reference.csv", header=TRUE, sep=",")

library(munsell)
LAB<-read.table("LAB_Carbon32.txt", sep=",", header=T)

euc <- function(dat, x1, y1, z1){
  
  if(any(length(x1)!=1,length(y1)!=1,length(z1)!=1)) stop("x1, y1, or z1 should be length 1")
  
  x<-dat$L.
  y<-dat$a.
  z<-dat$b.
  
  answer <- sqrt((x-x1)^2+(y-y1)^2+(z-z1)^2)
  
  return(answer)
  
}

Munsell <- rep(NA,nrow(ctrial))

for(i in 1:nrow(ctrial)){
  L <- ctrial$L[i]
  A <- ctrial$A[i]
  B <- ctrial$B[i]
  y <-euc(Reference,L,A,B) 
  
  Munsell[i]<- which.min(y)
    
}

out<-Reference[Munsell,]

munsell<-out[,2:4]
centroid.muns<-cbind(centroid_table,munsell)
the.number.i.need.to.use.to.create.the.below.file<-ncol(centroids.complete)-2
the.other.number.i.need<-ncol(centroids.complete)
the.bit.i.want.to.put.on.the.end.of.centroids.muns<-centroids.complete[,the.number.i.need.to.use.to.create.the.below.file:the.other.number.i.need]


centroid.muns<-cbind(centroid.muns,the.bit.i.want.to.put.on.the.end.of.centroids.muns)

attach(centroid.muns)
newdata <- centroid.muns[order(caco3),]
newdata <- centroid.muns[order(cec_nh4),]
newdata <- centroid.muns[order(clay_tot_psa),]
newdata <- centroid.muns[order(oc),]
newdata <- centroid.muns[order(ph_h2o),]
newdata <- centroid.muns[order(soil.id),]
detach(centroid.muns)
C<-newdata[1:11,]
EP<-newdata[12:nrow(newdata),]

EP<-EP[order(EP$ph_h2o),]
EP<-EP[order(EP$sand_tot_psa),]
EP<-EP[order(EP$clay_tot_psa),]

C<-C[order(C$ph_h2o),]
C<-C[order(C$sand_tot_psa),]
C<-C[order(C$clay_tot_psa),]

newdata<-newdata[order(newdata$ph_h2o),]
newdata<-newdata[order(newdata$sand_tot_psa),]
newdata<-newdata[order(newdata$clay_tot_psa),]

hc.all<-hclust(dist(newdata[c(2:10)]), "ward")
plot(hc.all, hang=-1,labels=newdata$natural_key,main="Dendrogram of all the clusters")


hc <- hclust(dist(EP[,c(2:10)]), "ward")
plot(hc, hang=-1,labels=EP$natural_key,main="Dendrogram of the fixed clusters")

HCE = hclust(dist(C[,2:10]))
plot(HCE, hang=-1,labels=C$natural_key,main="Dendrogram of the non-fixed clusters")
message("Oh Noes!")

install.packages( pkgs = "soiltexture" )
library(soiltexture)
require(soiltexture)
TT.plot( class.sys = "USDA.TT" )

EPtex<-EP[,c("clay_tot_psa","sand_tot_psa","oc")]
names(EPtex)<-c("CLAY","SAND","OC")
head(EPtex)
EPtex$SILT<-100-(EPtex$CLAY+EPtex$SAND)
CLAY<-EPtex$CLAY
SILT<-EPtex$SILT
SAND<-EPtex$SAND
OC<-EPtex$OC
EPtex<-cbind(CLAY,SILT,SAND,OC)

Ctex<-C[,c("clay_tot_psa","sand_tot_psa","oc")]
names(Ctex)<-c("CLAY","SAND","OC")
head(Ctex)
Ctex$SILT<-100-(Ctex$CLAY+Ctex$SAND)
CLAY<-Ctex$CLAY
SILT<-Ctex$SILT
SAND<-Ctex$SAND
OC<-Ctex$OC
Ctex<-cbind(CLAY,SILT,SAND,OC)

ALLtex<-newdata[,c("clay_tot_psa","sand_tot_psa","oc")]
names(ALLtex)<-c("CLAY","SAND","OC")
head(ALLtex)
ALLtex$SILT<-100-(ALLtex$CLAY+ALLtex$SAND)
CLAY<-ALLtex$CLAY
SILT<-ALLtex$SILT
SAND<-ALLtex$SAND
OC<-ALLtex$OC
ALLtex<-cbind(CLAY,SILT,SAND,OC)
ALLtex<-cbind(ALLtex,newdata$soil.id)
ALLtex<-cbind(ALLtex,newdata$natural_key)

  
#TT.plot(EPtex)
TT.plot(

  class.sys = "USDA.TT",
  tri.data = ALLtex,
  main = "Soil texture data-end points(red), centroids(black)",
  labels=newdata$natural_key,
  font = 2, 
  col = ALLtex[,5]
  
) #
geo <- TT.plot(class.sys="USDA.TT",main="Soil texture data-end points(red), centroids(black)")

TT.text(
  
  tri.data = ALLtex,
  geo = geo,
  labels = newdata$natural_key,
  font = 6,
  col = ALLtex[,5]
) #
# dev.off()

pHmean<-mean(centroid.muns$ph_h2o)
Camean<-mean(centroid.muns$caco3)
cecmean<-mean(centroid.muns$cec_nh4)
ocmean<-mean(centroid.muns$oc)
c(pHmean,Camean,cecmean,ocmean)


