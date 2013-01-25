##Plotting the edgeroi data in something other than JMP as JMP sux big time when it comes to presenting results.

## Setting working directory, putting in data

setwd("C:/Users/phug7649/Desktop/TXTBIN")
source("./functions/qhull_algorithm.R")
library(ggplot2)
library(grid)

edata<-read.table("../AMed2084pc.txt",sep=",", header=T)
ecent<-read.table("../AMecent.txt",sep=",", header=T)
ccentdata<-read.table("../AMccent.txt",sep=",", header=T)
edata$max<-as.factor(edata$max)
ecent$Centroid_type<-c("E","E","E","E","E")
ccentdata$Centroid_type<-c("C","C","C","C","C","C")
allcent<-rbind(ecent,ccentdata)

eecent<-read.table("EEcentroids.csv",sep=",", header=T)
eememb<-read.table("EEmemberships.csv",sep=",", header=T,check.names=FALSE)
eememb<-eememb[,1:16]
eememb$ex<-eememb$MaxCls=="10*"

edata$max<-as.numeric(edata$max)
edata$type<-edata$max<6





ECENT <- ggplot(edata, aes(x=Prin1, y=Prin2))+
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

CCENT <- ggplot(edata, aes(x=Prin1, y=Prin2))+
  theme_bw()+
  geom_point(color='grey')+
  geom_point(aes(x=Prin1, y=Prin2,shape=Soil.ID), data=ccentdata,size=6)+
  scale_size_discrete("", range=c(4, 4)) +
  theme(legend.text=element_text(size=20)) +
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  coord_equal()

CCENT
ggsave("CCENT.png",CCENT, type="cairo")

ALLCENT <- ggplot(edata, aes(x=Prin1, y=Prin2))+
  theme_bw()+
  geom_point(aes(color=type))+
  geom_point(aes(x=Prin1, y=Prin2,shape=Centroid_type), data=allcent,size=6)+
  scale_size_discrete("", range=c(20, 20)) +
  theme(legend.title=element_text(size=20)) +
  theme(legend.text=element_text(size=20)) +
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  coord_equal()
ALLCENT
ggsave("ALLCENT.png",ALLCENT, type="cairo")


eememb$ex<-as.factor(eememb$ex)
EECENT <- ggplot(eememb, aes(x=`1_PC`, y=`2_PC`))+
  theme_bw()+
  geom_point(aes(color=ex))+
#   geom_point(aes(x=Prin1, y=Prin2,shape=Centroid_type), data=allcent,size=6)+
#   scale_size_discrete("", range=c(20, 20)) +
  theme(legend.title=element_text(size=20)) +
  theme(legend.text=element_text(size=20)) +
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
   coord_equal()
EECENT


###SEB GOING NUTS
NUTS<-ggplot(edata, aes(x=Prin1, y=Prin2))+
  theme_bw() +
  geom_point(data=edata[,-3], aes(x=Prin1, y=Prin2), inherit.aes=FALSE, colour="grey")+
  #geom_density2d()+
  #geom_hex()+
  #stat_binhex()+
  #geom_point(colour=..count..)+
  #stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)+
  geom_point(alpha=0.4)+
  facet_wrap(~ max, nrow=3)+
  coord_equal()
NUTS
ggsave("NUTS.png",NUTS, type="cairo")

###for later

eememb$ex=="TRUE"
eememb$ex<-
  i hate you r
stuf<-eememb$ex=="TRUE"
stuff<-as.matrix(stuf)
str(stuf)
sum(stuf)
table(eememb$ex)

