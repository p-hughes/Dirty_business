##Plotting the edgeroi data in something other than JMP as JMP sux big time when it comes to presenting results.

## Setting working directory, putting in data

#setwd("C:/Users/phug7649/Desktop/TXTBIN")
source("./functions/qhull_algorithm.R")
library(ggplot2)
library(grid)

edata<-read.table("AMed2084pc.txt",sep=",", header=T)
edata$max<-as.factor(edata$max)


AM1 <- ggplot(edata, aes(x=Prin1, y=Prin2))+
  
  theme_bw()+
  scale_color_grey()+
  geom_point(aes(colour=max),size=3)+
  scale_shape_manual('',values=c(1:11))+
  theme(legend.text=element_text(size=20)) +
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  coord_equal()

AM1

###Grey plots
#library(hexbin)
ggplot(edata, aes(x=Prin1, y=Prin2))+
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

###