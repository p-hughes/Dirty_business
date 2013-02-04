##This is a script that takes the output from Matlab and combines it tinto a single table so it 
##can be analysed.


#setting working directory, reading in the data.

setwd("C:/Users/phug7649/Desktop/edgeroi/New/W_200")
#Soil attributes should go here
attrib<-read.table("b.csv",header=TRUE, sep=",")
dist<-read.csv("C:/Users/phug7649/Documents/MATLAB/mdist.csv",header=FALSE)
c<-read.csv("C:/Users/phug7649/Documents/MATLAB/mcent.csv",header=FALSE)
w<-read.csv("C:/Users/phug7649/Documents/MATLAB/weighting.csv",header=FALSE)
#dist<- read.table("membership table_30.csv",header=TRUE,sep=",",check.names=FALSE)
names(dist)<-c('11a','11b','11c','11d','11e','11f','11g','11h','11i','11j','11k')



##number of rows without the centroid table
drows<-2072
##Number of rows in the centroid table
crows<-11
##making the table the correct length
dist<-dist[1:drows,]
##adding the "max" column
aa<-as.matrix(dist)
library(plyr)
dist$max<-apply(aa,1,which.max)
dist$max<-letters[dist$max]
head(dist)

##connecting the attributes to the distance table
attrib_dist<-cbind(attrib,dist)

#creating labels for centroids (may be redundant)
Soil_ID<-(c("E1","E2","E3","E4","E5","C1","C2","C3","C4","C5","C6"))
#Soil_ID<-data.frame(Soil_ID)
c_soil_id<-cbind(Soil_ID,c)
names(c_soil_id)<-names(attrib)
#attrib_c<-rbind(attrib,c_soil_id)
id<-diag(crows)
thing<-letters[1:crows]
idthing<-data.frame(id,thing)
names(idthing)<-names(dist)
centroids<-cbind(c,idthing)
id<-(centroids$max)
centroids2<-cbind(id,centroids)
names(centroids2)<-names(attrib_dist)
ATTRIB_DIST_cent<-rbind(attrib_dist,centroids2)
summary<-table(ATTRIB_DIST_cent$max)
summary
write.csv(ATTRIB_DIST_cent, paste0('output_',w[1,1],'.csv'))
write.csv(summary,paste0('summary_',w[1,1],'.csv'))


m1<-read.csv("summary_30.csv")
m2<-read.csv("summary_60.csv")
m3<-read.csv("summary_100.csv")
m4<-read.csv("summary_200.csv")
m5<-read.csv("summary_400.csv")

c_out<-cbind(m1,m2[,3],m3[,3],m4[,3],m5[,3])
cout<-c_out[,3:7]
tcout<-t(cout)
plot(tcout[,1])
for (i in 1:5){
  plot(tcout[,i],ylim=c(1,200))
}
# #may as well add principal components
# a<-princomp(ATTRIB_DIST_cent[,2:13],scores=TRUE)
# acomp<-cbind(d,comp)
# 
# comp<-a$scores/10
# comp2<-a$sdev
# head(comp)
# 
# ADc_comp1_2<-cbind(ATTRIB_DIST_cent,comp[,1:2])
# test<-cbind(ADc_comp1_2,d)
 

