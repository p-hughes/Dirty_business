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
plot(princomp_comp[,1],princomp_comp[,2])
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

##create the id matrix from the matlab data


##creating max column for data distances
aa<-as.matrix(data_distances)
data_distances$max<-apply(aa,1,which.max)
max<-make_letter_ids(nrow(centroid_table))
data_distances$max<-max[data_distances$max]
max<-data_distances$max

max<-rbind(c(max,y))
max<-t(max)


data.complete<-cbind(data_cent.prin,max)
centroids.complete<-data.complete[11752:11780,]
emno<-number_of_end_members[1,1]
ceno<-nrow(centroid_table)-emno
soil.id<-rep(c("E", "C"), c(emno, ceno))
centroids.complete<-cbind(centroids.complete,soil.id)




###SEB GOING NUTS (more often referred to as a panel plot)
ggplot(data.complete, aes(x=Comp.1, y=Comp.2), group=max)+
  theme_bw() +
  geom_point(colour="grey40")+
  

  #stat_bin2d(binwidth=c(1, 1),colour=gray) +

  facet_wrap(~ max, nrow=5)+
  geom_point(data=centroids.complete, aes(shape=soil.id),size=2.5, colour="black")+  
  scale_shape_manual(values=c(16, 17))+
  theme(plot.background = element_rect(fill = "green"))+
 # theme(panel.margin = unit(5, "lines"))+
  coord_equal()

ggsave("NUTS.png", type="cairo")






