##Analysis of USNOA
setwd("C:/Users/phug7649/Desktop/TXTBIN")
raw_data<-read.csv("phil1.txt",sep=",",header=T)
des<-read.table("H_des.txt",sep=",",header=T,na.strings='')
##I need to create two data frames; one containing surface horizons only and the other, the rest of the data. The relationship
##between horizons need to be explored as deeper horizons sometimes 
Layera<-read.csv("USII_0_5.csv",sep=",",header=T)
Layerb<-read.csv("USII_5_10.csv",sep=",",header=T)
Layerc<-read.csv("USII_10_20.csv",sep=",",header=T)
Layerd<-read.csv("USII_20_40.csv",sep=",",header=T)
Layere<-read.csv("USII_40_60.csv",sep=",",header=T)
Layerf<-read.csv("USII_60_100.csv",sep=",",header=T)
Layerg<-read.csv("USII_100plus.csv",sep=",",header=T)
depths<-raw_data[,c(2,28,27)]
USII<-rbind(Layera,Layerb,Layerc,Layerd,Layere,Layerf,Layerg)
USnoa<-rbind(Layerb,Layerc,Layerd,Layere,Layerf,Layerg)
Ordered.US<-USII[order(USII$X),]
##Creating three distinct datasets to analyse- one being the total data, the next being just the surface horizons, 
##the final being data-surface.
check<-merge(USII,depths, by="natural_key")
noadepth<-merge(USnoa,depths, by="natural_key")
adepth<-merge(Layera,depths, by="natural_key")
check$thickness<-check$hzn_bot-check$hzn_top
noadepth$thickness<-noadepth$hzn_bot-noadepth$hzn_top
adepth$thickness<-adepth$hzn_bot-adepth$hzn_top
output<-check[,c(1,3,4,5,6,7,8,9,11,12,14,15)]
outputa<-adepth[,c(1,3,4,5,6,7,8,9,11,12,14,15)]
outputnoa<-noadepth[,c(1,3,4,5,6,7,8,9,11,12,14,15)]

sum(as.numeric(is.na(check[,15])))
output<-na.exclude(output)
sum(as.numeric(is.na(output)))
write.csv(output,"US20130709_71680.csv",row.names=FALSE)
write.csv(outputa,"USa20170709_11704.csv",row.names=FALSE)
write.csv(outputnoa,"USnoa20170709_60065.csv",row.names=FALSE)

##FKM done, reading data

##how many clusters?
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/find.k.r")
Clusters<-12

clusfind<-read.csv("C:\\Users\\phug7649\\Desktop\\kmeans\\US_thickness_analysis_09072013\\summary.txt",sep="",header=TRUE)
clusfinda<-read.csv("C:\\Users\\phug7649\\Desktop\\kmeans\\US_thickness_analysis_09072013\\a\\summary.txt",sep="",header=TRUE)
clusfindnoa<-read.csv("C:\\Users\\phug7649\\Desktop\\kmeans\\US_thickness_analysis_09072013\\noa\\summary.txt",sep="",header=TRUE)
library(rgl)
attach(clusfindnoa)
redblue.colors <- colorRampPalette(c("red", "blue"))
contrast<-colorRampPalette(c("orange3","orangered","orangered4","orchid4","palegreen4","palevioletred"))
plot3d(Class,FPI,Phi, main="FKM~phi,FPI and class",col=rep(contrast(6), each=46) , size=3)
plot3d(Class,FPI,Phi, main="FKM~phi,FPI and class",col=rep(contrast(6), each=46) , size=3)
detach(clusfind)
library(rgl)
attach(clusfinda)
redblue.colors <- colorRampPalette(c("red", "blue"))
contrast<-colorRampPalette(c("orange3","orangered","orangered4","orchid4","palegreen4","palevioletred"))
plot3d(Class,FPI,Phi, main="FKM~phi,FPI and class",col=rep(contrast(6), each=46) , size=3)
plot3d(Class,MPE,Phi, main="FKM~phi,MPE and class",col=rep(contrast(6), each=46) , size=3)
plot3d(Class,S,Phi, main="FKM~phi,S and class",col=rep(contrast(6), each=46) , size=3)

##from the 3d plot, phi of 1.3, clusters of either 8, 11 or 19.



######THIS SHOULD BE A FUNCTION!!#####
input<-read.csv("USnoa20170709_60065.csv",sep=",", header=T)

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
ys<-10      ##starting parameter for yardstick
factor<-.35  ##creating the factor by which the yardstick length is modified (previous run was 0.8)
YScrit<-6   ##Stopping criteria; when the overall size of the hull is less than this, the algorithm stops.
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

###################################### Putting the matlab stuff together####################################################

matrix<-diag(nrow(bin))
##creating end point matrices
points<-input[bin,]

################################How many clusters have you decided on?######################################################

Clusters<-48
##writing files
setwd("C:\\Users\\phug7649\\Documents\\MATLAB")
write.table(matrix,"matrix.csv",row.names=FALSE,col.names=FALSE,sep=",")
write.csv(points,"EP.csv",row.names=FALSE)
write.csv(input,"DATA.csv",row.names=FALSE)

message(paste0("nclass needs to be ", Clusters+nrow(bin)))

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

ggsave(paste0("noapanel", w, number_of_end_members[1,1],".png"),type="cairo")

###########################################################################################################################################
#################################################### ANOTHER PLOT #########################################################################
combined<-ggplot(data.complete,aes(x=Comp.1,y=Comp.2))+
  geom_point(aes(colour=max))+
  geom_point(data=centroids.complete,aes(shape=soil.id),size=4)+
  scale_shape_manual(values=c(16,17))+
  #         xlim(-12,8)+
  #         ylim(-7.5,5)+
  ggtitle(paste0("Weighting ", weighting_factor[1,1],", creating ",ratio, " percent end point memberships"))+
  coord_equal()+
guides(col=guide_legend(ncol=2))
combined
ggsave(paste0("combined", w, number_of_end_members[1,1],".png"),type="cairo")

###########################################################################################################################################
plot3d(centroids.complete$Comp.1,centroids.complete$Comp.2,centroids.complete$Comp.3,)

plot3d(Class,FPI,Phi, main="FKM~phi,FPI and class",col=rep(contrast(6), each=46) , size=3)
with(centroids.complete,plot3d(Comp.1,Comp.2,Comp.3, col=soil.id))

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

centroid.table<-cbind(centroid_table,munsell)
centroid.muns<-cbind(centroid_table,munsell)
## commenting out the following five lines may break it

the.number.i.need.to.use.to.create.the.below.file<-ncol(centroids.complete)-2
the.other.number.i.need<-ncol(centroids.complete)
the.bit.i.want.to.put.on.the.end.of.centroids.muns<-centroids.complete[,the.number.i.need.to.use.to.create.the.below.file:the.other.number.i.need]
centroid.muns<-cbind(centroid.muns,the.bit.i.want.to.put.on.the.end.of.centroids.muns)
# centroid.muns$max<-alexmax

###########################################################################################################################################
######################################### SELECTING THE PRIORITY OF THE CLUSTERS ##########################################################

attach(centroid.muns)
newdata <- centroid.muns[order(caco3),]
newdata <- centroid.muns[order(cec_nh4),]
newdata <- centroid.muns[order(clay_tot_psa),]
newdata <- centroid.muns[order(oc),]
newdata <- centroid.muns[order(ph_h2o),]
newdata <- centroid.muns[order(soil.id),]
detach(centroid.muns)

C<-newdata[1:number.of.end.points[1,1],]
EP<-newdata[(1+number.of.end.points[1,1]):nrow(newdata),]

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

##########################################################################################################################################
####################################### SOIL TEXTURE PLOT ################################################################################

#install.packages( pkgs = "soiltexture" )
library(soiltexture)
#require(soiltexture)
#TT.plot( class.sys = "USDA.TT" )

EPtex<-EP[,c("clay_tot_psa","sand_tot_psa","oc")]
names(EPtex)<-c("CLAY","SAND","OC")
EPtex$SILT<-100-(EPtex$CLAY+EPtex$SAND)
# CLAY<-EPtex$CLAY
# SILT<-EPtex$SILT
# SAND<-EPtex$SAND
# OC<-EPtex$OC
EPtex<-EPtex[,c("CLAY","SILT","SAND","OC")]
EPtex<-cbind(EPtex,fac=2)

Ctex<-C[,c("clay_tot_psa","sand_tot_psa","oc")]
names(Ctex)<-c("CLAY","SAND","OC")
Ctex$SILT<-100-(Ctex$CLAY+Ctex$SAND)
Ctex<-Ctex[,c("CLAY","SILT","SAND","OC")]
Ctex<-cbind(Ctex,fac=1)
# CLAY<-Ctex$CLAY
# SILT<-Ctex$SILT
# SAND<-Ctex$SAND
# OC<-Ctex$OC
# Ctex<-cbind(CLAY,SILT,SAND,OC)


factors<-rbind(EPtex,Ctex)
cent.muns.greek<-centroid.muns
cent.muns.greek$greek<-cent.muns.greek$natural_key
clus.num<-(nrow(centroid.muns)-nrow(end.tot))
all.cent<-cbind(cent.muns.greek[1:clus.num,],greek.cent)

ALLtex<-cent.muns.greek[,c("clay_tot_psa","sand_tot_psa","oc")]
names(ALLtex)<-c("CLAY","SAND","OC")
head(ALLtex)
ALLtex$SILT<-100-(ALLtex$CLAY+ALLtex$SAND)
CLAY<-ALLtex$CLAY
SILT<-ALLtex$SILT
SAND<-ALLtex$SAND
OC<-ALLtex$OC
ALLtex<-cbind(CLAY,SILT,SAND,OC)
ALLtex<-cbind(ALLtex,cent.muns.greek$soil.id)
ALLtex<-cbind(ALLtex,cent.muns.greek$natural_key)


with(cent.muns.greek,plot3d(L,A,B, main="centroid colours",col=max, size=3))

############################################################# THIS MAY BE THE PROBLEM. UNCOMMENT ############################

#ALLtex<-newdata[,c("clay_tot_psa","sand_tot_psa","oc")]
# names(ALLtex)<-c("CLAY","SAND","OC")
# head(ALLtex)
# ALLtex$SILT<-100-(ALLtex$CLAY+ALLtex$SAND)
# CLAY<-ALLtex$CLAY
# SILT<-ALLtex$SILT
# SAND<-ALLtex$SAND
# OC<-ALLtex$OC
# ALLtex<-cbind(CLAY,SILT,SAND,OC)
# ALLtex<-cbind(ALLtex,newdata$soil.id)
# ALLtex<-cbind(ALLtex,newdata$natural_key)

##############I need to find an automatic way to do this!#################
#creating greek symbols as factors

greek <- c("\U03B1", "\U03B2", "\U03B3", "\U03B4", "\U03B5", "\U03B6", "\U03B7", "\U03B8", "\U03B9", "\U03BA", "\U03BB", "\U03BC",
"\U03BD", "\U03BE", "\U03BF", "\U03C1", "\U03C2", "\U03C3", "\U03C4", "\U03C5", "\U03C6", "\U03C7", "\U03C8", "\U03C9")

greek.cent<-make_letter_ids(nrow(cent.tot)-1,greek)

#greek.cent<-cbind(centroid.muns[24:nrow(centroid.muns),],greek)


#TT.plot(EPtex)
# TT.plot(
# 
#   class.sys = "USDA.TT",
#   tri.data = ALLtex,
#   main = "Soil texture data-end points(red), centroids(black)",
#   labels=newdata$natural_key,
#   font = 2, 
#   col = ALLtex[,5]
#   
# ) #

geo <- TT.plot(class.sys="USDA.TT",main="Soil texture data-end points(red), centroids(blue)")

TT.text(
  
  tri.data = ALLtex,
  geo = geo,
  #labels = newdata$natural_key,
  #labels=greek.cent$greek, ##I need to get the greek labels working again. Seb may be my only hope!
  #labels=all.cent$greek,
  labels=c(mat.max,greek.cent),
  font = 6,
  col = c("blue","red")[ALLtex[,5]]
) #
# dev.off()

############################################################################################################################################
#########################################  MAPPING  ########################################################################################

# install.packages("maps")
library(maps)
map('usa')

##getting location data
locations<-read.csv("testIII.txt",header=T)
locations<-subset(locations, country_code == "US")
latitude_std_decimal_degrees<-locations$latitude_std_decimal_degrees
longitude_std_decimal_degrees<-locations$longitude_std_decimal_degrees
Locations<-locations[,c(2,22,23)]
head(Locations)
plot(Locations$longitude_std_decimal_degrees,Locations$latitude_std_decimal_degrees)
map('usa',add=T)
geo.cent<-merge(data.complete,Locations, by= "natural_key",all.x=TRUE)
plot(Locations$longitude_std_decimal_degrees,Locations$latitude_std_decimal_degrees,col=data.complete$max,pch=20,cex=.5)

map('world',add=T)




world_map <- as.data.frame(map("world",  plot = FALSE)[c("x", "y")])  
pdf("USall_max.pdf", height=5, width=10)
for(i in c(mat.max,cent.max)){  
  pl<-ggplot(world_map, aes(x=x, y=y))+
    scale_x_continuous(limits=c(-173, -60))+
    scale_y_continuous(limits=c(15, 70))+
    theme_bw() +
    geom_path(size=.01)+
    geom_point(data=geo.cent, aes(x=longitude_std_decimal_degrees, y=latitude_std_decimal_degrees), colour="grey", alpha=0.1)+
    geom_point(data=geo.cent[geo.cent$max==i,], aes(x=longitude_std_decimal_degrees, y=latitude_std_decimal_degrees), colour="steelblue")+
    geom_point(data=geo.cent[geo.cent$max==i,], aes(x=longitude_std_decimal_degrees, y=latitude_std_decimal_degrees), size=.1, colour="indianred")+
    coord_equal()+
    labs(title=paste0("Max membership ",i))
  print(pl)
}
dev.off()
file.show("USall_max.pdf")
file.show("USIII_max.pdf")

##saving relevent data to csv
nrow(unique(geo.cent))

# some horizons are 30 meters or more! Creating a max value for depth
data<-data.complete[1:59977,1:12]
y=ifelse((data$hzn_top>200),201,data$hzn_top)
data$hzn_top<-y
hist(data$hzn_top)


setwd("C:\\Users\\phug7649\\Desktop\\kmeans\\US_thickness_analysis_09072013\\noa")
write.csv(data.complete,file="data and distances.csv")
write.csv(cent.muns.greek,file="centroid table.csv")
write.csv(data,file="data_depth200.csv", row.names=FALSE)

depths <- centroid.muns[order(centroid.muns$hzn_top),]
orders<-read.csv("C:\\Users\\phug7649\\Desktop\\TXTBIN\\orders.txt",sep=",",header=T)
ord<-orders[,c(2,7)]
data.orders<-merge(data.complete,ord,by="natural_key")
data.orders<-unique(data.orders)
plot(data.orders$Comp.3,data.orders$Comp.4, col=data.orders$tax_order)
plot3d(data.orders$Comp.1,data.orders$Comp.2,data.orders$Comp.3, col=data.orders$tax_order, size=0.1)
plot3d(data.orders$Comp.2,data.orders$Comp.3,data.orders$Comp.4, col=data.orders$tax_order, size=0.1)

plot3d(data.orders$Comp.1,data.orders$Comp.2,data.orders$Comp.3, col=data.orders$max,size=0.1)
plot3d(data.orders$Comp.4,data.orders$Comp.2,data.orders$Comp.3, col=data.orders$max,size=0.1)
plot3d(data.orders$Comp.4,data.orders$Comp.5,data.orders$Comp.3, col=data.orders$max,size=0.1)
plot3d(data.orders$Comp.4,data.orders$Comp.5,data.orders$Comp.6, col=data.orders$max,size=0.1)
plot3d(data.orders$Comp.7,data.orders$Comp.5,data.orders$Comp.6, col=data.orders$max,size=0.1)
plot3d(data.orders$Comp.7,data.orders$Comp.8,data.orders$Comp.6, col=data.orders$max,size=0.1)
plot3d(data.orders$Comp.7,data.orders$Comp.8,data.orders$Comp.9, col=data.orders$max,size=0.1)
plot3d(data.orders$Comp.10,data.orders$Comp.8,data.orders$Comp.9, col=data.orders$max,size=0.1)


plot3d(data.orders$L,data.orders$A,data.orders$B, col=data.orders$tax_order)
thing<-c(data.orders$Comp.1,data.orders$Comp.2,data.orders$Comp.3)
#planes3d(a, b = NULL, c = NULL, d = 0, ...)

##Planes script will really cause problems with data with n <60!
#rgl.planes(centroids.complete[1:10]$Comp.1,centroids.complete[1:10]$Comp.2,centroids.complete[1:10]$Comp.3, col=centroids.complete[1:10]$max,size=0.1)



file.show("Hallelujah-Short.mp3")

##adding more columns of data, may need to estimate some properties...
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/nodup.r")
ions<-read.csv("C:\\Users\\phug7649\\Desktop\\TXTBIN\\Ions.txt", header=T, sep=",")
ionid<-ions[,2:8]
data.ion<-merge(data,ionid,by="natural_key")
#data.ion<-na.exclude(data.ion)
data.ion<-nodup(data.ion)
sum(as.numeric(is.na(data.ion$ca_nh4)))

##how does ca compare to mg?
plot(ions$ca_nh4,ions$mg_nh4)
line.camg<-lm(ions$mg_nh4~ions$ca_nh4)
abline(line.camg)
summary(line.camg)

##how does ca compare to na?
plot(ions$ca_nh4,ions$na_nh4)
line.cana<-lm(ions$na_nh4~ions$ca_nh4)
abline(line.cana)
summary(line.cana)

##how does ca compare to k?
plot(ions$ca_nh4,ions$k_nh4)
line.cak<-lm(ions$k_nh4~ions$ca_nh4)
abline(line.cak)
summary(line.cak)

##how does ca compare to base_sum?
plot(ions$ca_nh4,ions$base_sum)
line.cabase<-lm(ions$base_sum~ions$ca_nh4)
abline(line.cabase)
summary(line.cabase)
col=rep(contrast(6), each=46) , size=3)
newcols <- colorRamp(c("grey90", "grey10")) #generates palette frome light to dark grey for better visibility 
contrast<-colorRampPalette(c("orange3","orangered","orangered4","orchid4","palegreen4","palevioletred"))

subset <- ions[ions$base_sum < quantile(na.exclude(ions$base_sum),probs=0.975),'base_sum']
subset <- ions[ions$ca_nh4 < quantile(na.exclude(ions$ca_nh4),probs=0.975),'ca_nh4']
subset <- ions[ions$cec_sum < quantile(na.exclude(ions$cec_sum),probs=0.975),'cec_sum']


cols <- rgb(newcols(scales::rescale(na.exclude(subset))),maxColorValue=255)
with(na.exclude(ions),plot3d(na_nh4,mg_nh4,base_sum, main="ions in US data" ,col=cols, size=3))
summary(na.exclude(ions$base_sum))

with(data.ion,plot(caco3,ca_nh4))
with(data.ion,plot3d(base_sum,ca_nh4,cec_nh4))
#oc,ph_h20,cec_nh4
#base sum, canh4 and cecnh4- relationship.
#pairs(data.ion)

data.ion$
  source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/find.k.r")
  setwd("C:\\Users\\phug7649\\Desktop\\kmeans\\US_thickness_analysis_09072013\\noa\\90clus")
find.k()

cec.pred<-lm(ions$base_sum~ions$cec_sum)
summary(cec.pred)
y=ifelse((ions$base_sum=="NA"),ions$ca_nh4+ions$mg_nh4+ions$na_nh4+ions$k_nh4,ions$base_sum)
y=ifelse((ions$ca_nh4=="NA"),ions$base_sum-ions$mg_nh4+ions$na_nh4+ions$k_nh4,ions$ca_nh4)

png("ionplot.png", width=2048, height=1024)
pairs(ions[,-(1:2)])
dev.off()

# Stepwise Regression
library(MASS)
fit <- lm(ca_nh4~mg_nh4+na_nh4+k_nh4,data=ions)
step <- stepAIC(fit, direction="both")
step$anova # display results

anova(fit)
an<-anova(fit)
str(an)
glm(ca_nh4~mg_nh4+na_nh4+k_nh4,data=ions)

library(mclust)
install.packages("mclust")

testclust<-mclustBIC(na.exclude(ions[,3:8]))
pred<-predict(step)
plot(pred,ions$ca_nh4)

summary(lm(base_sum~ca_nh4, data=ions))
summary(lm(base_sum~k_nh4, data=ions))
summary(lm(base_sum~na_nh4, data=ions))
summary(lm(base_sum~mg_nh4, data=ions))
summary(lm(k_nh4~ca_nh4, data=ions))
summary(lm(mg_nh4~ca_nh4, data=ions))
summary(lm(na_nh4~ca_nh4, data=ions))
summary(lm(na_nh4~mg_nh4, data=ions))
summary(lm(na_nh4~k_nh4, data=ions))
summary(lm(k_nh4~mg_nh4, data=ions))

