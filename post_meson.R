##This is a script that takes the output from Matlab and combines it into a single table so it 
##can be analysed.This assumes the output from matlab is based on an id matrix of the same size 
##as the centroid table.

#Importing original principal components Not neccesary as we have the internal mechanism working now.

setwd("C:/Users/phug7649/Desktop/TXTBIN")
Original_data<-read.table("USII_0_5.csv",header=TRUE, sep=",")
comps<-princomp(Original_data[,3:ncol(Original_data)],cor=TRUE)
scores<-comps$scores

z<-cbind(Original_data[,2],scores)
#importing output from fuzzy k means with extragrades

setwd("C:/Users/phug7649/Desktop/kmeans/Paper_2/0-5")

clusfind<-
head(clusfind)

extra<-read.csv("f1.25 11_class.txt",sep="", header=TRUE)##importing classes, ID, maxcls, n data columns

############################################ Im up to here...########################################################

##WTF?????

excent<-read.csv("excent.csv")
extra<-extra[1:2072,]




#setting working directory (in this case the edgeroi folders), reading in the data.

setwd("C:/Users/phug7649/Desktop/edgeroi/New/W_200")
#Soil attributes should go here
attrib<-read.table("C:/Users/phug7649/Documents/MATLAB/mdist.csv",header=TRUE, sep=",")
dist<-read.csv("C:/Users/phug7649/Documents/MATLAB/mdist.csv",header=FALSE)
c<-read.csv("C:/Users/phug7649/Documents/MATLAB/mcent.csv",header=FALSE)
w<-read.csv("C:/Users/phug7649/Documents/MATLAB/weighting.csv",header=FALSE)
#dist<- read.table("membership table_30.csv",header=TRUE,sep=",",check.names=FALSE)
names(dist)<-c('11a','11b','11c','11d','11e','11f','11g','11h','11i','11j','11k')




##number of rows in the data set
rows<-read.csv("C:/Users/phug7649/Documents/MATLAB/rows.csv",header=FALSE)
end<-read.csv("C:/Users/phug7649/Documents/MATLAB/end.csv",header=FALSE)
cent<-read.csv("C:/Users/phug7649/Documents/MATLAB/cent.csv",header=FALSE)
drows<-rows[1,1]-end[1,1]
##Number of rows in the centroid table
crows<-cent[1,1]
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

##further analysis--DATA SPECIFIC!
##maybe this neeeds to be cut down to w=30 again. Too confusing.
my<-read.csv("summary_1.csv")
mx<-read.csv("summary_5.csv")
ma<-read.csv("summary_15.csv")
m1<-read.csv("summary_30.csv")
m2<-read.csv("summary_60.csv")
m3<-read.csv("summary_100.csv")
m4<-read.csv("summary_200.csv")
m5<-read.csv("summary_400.csv")

##Plot 1

c_out<-cbind(my,mx[,3],ma[,3],m1[,3],m2[,3],m3[,3],m4[,3],m5[,3])
cout<-c_out[,3:10]
tcout<-t(cout)
plot(tcout[,1])
for (i in 1:11){
  plot(tcout[,i],ylim=c(1,500),type="l", main=i)
}

plot(tcout[,1],type='l',ylim=c(1,500), main="Comparison of weighting factor to data distribution within clusters", 
     xaxt="n",ylab="Number of data in cluster",xlab="weighting applied",cex.lab=1.55,cex.axis=1.5)
axis(1,at=1:8,labels=c("w=1","w=5","w=15","w=30", "w=60", "w=100", "w=200","w=400"),cex.axis=1.5) 
apply(tcout[,-1], 2, lines)

c_out<-cbind(m1[,3],m2[,3],m3[,3],m4[,3],m5[,3])
cout<-c_out[,1:5]
tcout<-t(cout)

##version 2

##installing cairo

install.packages(c("Cairo"), repos="http://cran.r-project.org" )
library(Cairo)

opar<-par()

colnames(tcout)<-c("E","E","E","E","E","I","I","I","I","I","I")

#CairoPNG('new-style.png')
par(mar = c(5,5,4,2) +0.1)
plot(tcout[,1],type='n',ylim=c(1,500), main="Comparison of weight factor to data distribution within clusters",
    cex.main=1.65,
     xaxt="n",ylab="Number of data in each cluster",xlab="weighting applied",cex.lab=1.6,cex.axis=1.5)
axis(1,at=1:5,labels=c("w=30", "w=60", "w=100", "w=200","w=400"),cex.axis=1.5) 

apply(tcout[,colnames(tcout)=="E"], 2, lines, lty=1)
apply(tcout[,colnames(tcout)=="I"], 2, lines, lty=2)
legend(1000,4.5, c(colnames(tcout=="E"),colnames(tcout=="I")))

#dev.off()


#plot(x, y, main='Test plot', pch=21, col='blue', bg='lightblue')
#abline(lm(y ~ x), col='red', lwd=2)

       
# for (i in 1:5) {
# lines(tcout[,-i],lty=2)
# }
# for i in 6:ll
# lines(tcout[,-i],lty=1)

head(tcout)


barplot(tN, col=rainbow(5),cex.axis=0.5) # for the Y-axis
barplot(tN, col=rainbow(5),cex.names=0.5) # for the X-axis

 #may as well add principal components, reading in the data and getting rid of that annoying 
 #"X" column R sometimes makes

ADc_comp1_2<- read.csv("output_30_pc.txt")
ADc_comp1_2<-ADc_comp1_2[,setdiff(names(ADc_comp1_2), "Column.1")]
ADc2_comp1_2<- read.csv("output_60.csv")
ADc2_comp1_2<-ADc2_comp1_2[,2:ncol(ADc2_comp1_2)]
AD_60_comp1_2<-cbind(ADc2_comp1_2,ADc_comp1_2[,26:27])


#Time to use those PC's imported from earlier


ecent<-ADc_comp1_2[2073:2083,]
ecent$Soil.ID<-c("E","E","E","E","E","C","C","C","C","C","C")


##Plotting time. I need two sets of plots, one from w=30 and the other from w=60+
library(ggplot2)
library(grid)

##plot1
ECENT <- ggplot(ADc_comp1_2, aes(x=Prin1, y=Prin2))+
  theme_bw()+
  geom_point(color='grey')+
  geom_point(aes(x=Prin1, y=Prin2,shape=Soil.ID), data=ecent,size=6)+
  scale_size_discrete("", range=c(4, 4)) +
  theme(legend.text=element_text(size=20)) +
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  coord_equal()


ECENT
ggsave("ECENT.png",ECENT, type="cairo")



###SEB GOING NUTS (more often referred to as a panel plot)

##Plot1
NUTS<-ggplot(ADc_comp1_2, aes(x=Prin1, y=Prin2), group=max)+
  theme_bw() +
  geom_point(data=ADc_comp1_2[,c("Prin1", "Prin2")], aes(x=Prin1, y=Prin2), inherit.aes=FALSE, colour="grey40")+
  #geom_density2d()+
  #geom_hex()+
  #stat_binhex()+
  #geom_point(colour=..count..)+
  #stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)+
  stat_bin2d(binwidth=c(0.6, 0.6)) +
  #geom_point(alpha=0.5)+
  facet_wrap(~ max, nrow=3,)+
  geom_point(data=ecent, aes( shape=Soil.ID), size=4, colour="black")+  
  scale_fill_gradient(low="grey90", high="grey10")+
  scale_shape_manual(values=c(1, 2))+
  coord_equal()
NUTS
ggsave("NUTS.png",NUTS, type="cairo")

##Plot2

NUTS2<-ggplot(AD_60_comp1_2, aes(x=Prin1, y=Prin2), group=max)+
  theme_bw() +
  geom_point(data=AD_60_comp1_2[,c("Prin1", "Prin2")], aes(x=Prin1, y=Prin2), inherit.aes=FALSE, colour="grey40")+
  #geom_density2d()+
  #geom_hex()+
  #stat_binhex()+
  #geom_point(colour=..count..)+
  #stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)+
  stat_bin2d(binwidth=c(0.6, 0.6)) +
  #geom_point(alpha=0.5)+
  facet_wrap(~ max, nrow=3,)+
  geom_point(data=ecent, aes( shape=Soil.ID), size=4, colour="black")+  
  scale_fill_gradient(low="grey90", high="grey10")+
  scale_shape_manual(values=c(1, 2))+
  coord_equal()
NUTS2
ggsave("NUTS2.png",NUTS2, type="cairo")

## it looks as though clusters of about 1% total row length capture end point clusters. increasing w 
## tidies up the data, but may miss extragrade clusters or worse, give data extreme weight.

##extragrade data for comparison

setwd("C:/Users/phug7649/Desktop/TXTBIN")
extra<-read.table("10_class.txt",sep="", header=T)
excent<-read.csv("excent.csv")
extra<-extra[1:2072,]
setwd("C:/Users/phug7649/Desktop/edgeroi/New/W_200")
#Soil attributes should go here
attrib<-read.table("b.csv",header=TRUE, sep=",")
Soil_ID<-letters[1:10]
CI<-1:10
MaxCls<-letters[1:10]
mat<-diag(10)
matrix<-cbind(Soil_ID,MaxCls,CI,mat)
att_ex<-cbind(attrib,extra)
cent_mat<-cbind(excent,matrix,MaxCls)
names(cent_mat)<-names(att_ex)
explot<-rbind(att_ex,cent_mat)
##pc's needed.
write.csv(explot,"explot.csv")
##input pc's
pc<-read.csv("Ex_pc.txt",header=TRUE)
pccent<-pc[2073:2082,]

NUTS3<-ggplot(pc, aes(x=Prin1, y=Prin2), group=max)+
  theme_bw() +
  geom_point(data=pc[,c("Prin1", "Prin2")], aes(x=Prin1, y=Prin2), inherit.aes=FALSE, colour="grey40")+
  #geom_density2d()+
  #geom_hex()+
  #stat_binhex()+
  #geom_point(colour=..count..)+
  #stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)+
  stat_bin2d(binwidth=c(0.5, 0.5)) +
  #geom_point(alpha=0.5)+
  facet_wrap(~ MaxCls, nrow=3,)+
   geom_point(data=pccent, shape=1, size=4, colour="black")+  
   scale_fill_gradient(low="grey90", high="grey10")+
#   scale_shape_manual(values=c(1:10))+
   coord_equal()
NUTS3
ggsave("NUTS3.png",NUTS3, type="cairo")
table(pc$MaxCls)

write.csv(excent, "excent.csv", row.names=FALSE)

##messing with R's principal components

c<-princomp(attrib[,2:ncol(attrib)],scale=TRUE)
 d<-c$scores
 head(d)
 plot(d[,1],d[,2])

#yep, the principal component analysis is officially fucked.
  

