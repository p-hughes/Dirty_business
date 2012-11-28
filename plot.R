##Script to plot some stuff in 3d

setwd("C:/Users/phug7649/Desktop/TXTBIN")
a<-read.csv("centroids_landuse_order.csv")
plot(a[,3:5])

library(rgl)
plot3d(a[1:3,3:5],col="black",size=7, main="Dermosols")
plot3d(a[4:6,3:5],col="red",size=7,main="Chromosols")
plot3d(a[7:9,3:5],col="green",size=7,main="Kurosols")

plot3d(a[,3:5],col=rep(1:3,each=3),size=7, main="All three")

plot3d(a[1:3,3:5],col="black",size=7)
plot3d(a[4:6,3:5],col="red",size=7)