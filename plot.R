##Script to plot some stuff in 3d

setwd("C:/Users/phug7649/Desktop/TXTBIN")
a<-read.csv("centroids_landuse_order.csv")
dput(a,file="centroids_landuse_order.csv") 
plot(a[,3:5])

library(rgl)
plot3d(a[1:3,3:5],col="black",size=7, main="Dermosols")
plot3d(a[4:6,3:5],col="red",size=7,main="Chromosols")
plot3d(a[7:9,3:5],col="green",size=7,main="Kurosols")

plot3d(a[,3:5],col=rep(1:3,each=3),size=7, main="All three")

plot3d(a[1:3,3:5],col="black",size=7)
plot3d(a[4:6,3:5],col="red",size=7)

plot3d(a[1:3,3:5],col="black",size=7, main="Dermosols")

dist(a,method="euclidean")

library(ggplot2)

ggplot(a,aes(x=pc1, y=pc2, colour=pc3, shape=order))+
  geom_point(size=8)+
  scale_color_continuous(low="blue", high="orange")+
  facet_wrap(~Landuse, ncol=1)+
  ggtitle("Effect of land use on soil order")

##Furthest dist: 
max(dist(a,method="euclidean")) ## 2.17
which.max(dist(a,method="euclidean"))  ## 4,8 which is chrom nat/kur crop
##closest dist
min(dist(a,method="euclidean")) ##0.09
which.min(dist(a,method="euclidean")) ## 3,6 which is derm crop and chrom crop
##the remaining chromosols on the verge of becoming dermosols?

##is derm crop(3) similar to chrom nat(4)?
#1.31060452 which is very different

##Is derm nat (1) similar to chrom nat (4)?
#0.79014388 which is close to the minimum

## The major effect here is landuse. Soil type is not as important.I hypothesise that soil 
## mixing and the drop in pH has such a profound effect that it is hardly recognisable as its original soil.


  