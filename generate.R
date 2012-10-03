##for consistent results, set the seed.
setwd("C:/Users/phug7649/Desktop/TXTBIN")

source(file.path(getwd(), "R-scripts", "qhull_algorithm.R"))
# dir.create("output")
# write.csv(women, "output/women.csv")
set.seed(20120927)
de<-3
points<-5
ends<-1
a<-data.frame(id="a_EX",x=rnorm(ends, 5,de), y=rnorm(ends, 5,de))
b<-data.frame(id="b_EX",x=rnorm(ends, 100,de), y=rnorm(ends, 5,de))
c<-data.frame(id="c_EX",x=rnorm(ends, 50,de), y=rnorm(ends, 95,de))
d<-data.frame(id="d",x=rnorm(points, 50,de), y=rnorm(points, 15,de))
e<-data.frame(id="e",x=rnorm(points, 30,de), y=rnorm(points, 40,de))
f<-data.frame(id="f",x=rnorm(points, 50,de), y=rnorm(points, 40,de))
g<-data.frame(id="g",x=rnorm(points, 70,de), y=rnorm(points, 40,de))
h<-data.frame(id="h",x=rnorm(points, 50,de), y=rnorm(points, 66.7,de))
i<-data.frame(id="x_ORPH",x=rnorm(1, 80,de), y=rnorm(1, 15,de))
j<-data.frame(id="y_ORPH",x=rnorm(1, 20,de), y=rnorm(1, 15,de))
##Alex/budis suggestion


colours()

data<-rbind(a,b,c,d,e,f,g,h,i,j)
plot(data[,2],data[,3],main="soil classification by fuzzy sets",xlab="x",ylab="y",col=0)
text(data$x,data$y,data[,1])
hull<-quick_hull(data[,2:3])
lines(data[c(hull,hull[1]),2:3],col="red")
hist (data[,3],main="Histogram of y column",nclass=10,col="cornflowerblue")
hist (data[,2],main="Histogram of x column",nclass=10,col="steelblue")

write.csv(data,file="corrected.csv")

library(ggplot2)
ggplot(data, aes(x=x, y=y, alpha=x))+
  geom_point(aes(colour=y))+
  scale_colour_continuous(low="blue", high="orange")+
  geom_rug()
text(data$x,data$y,data[,1])

qplot(data=data,y, type=)
?ggplot2

###Non-normal y column
set.seed(20120927)
de<-2
a<-data.frame(id="a",x=rnorm(1, 2,de), y=rnorm(1, 2,de))
b<-data.frame(id="b",x=rnorm(20, 20,de), y=rnorm(20, 2,de))
c<-data.frame(id="c",x=rnorm(3, 40,de), y=rnorm(3, 2,de))
d<-data.frame(id="d",x=rnorm(5, 2,de), y=rnorm(5, 15,de))
e<-data.frame(id="e",x=rnorm(30, 20,de), y=rnorm(30, 14,de))
f<-data.frame(id="f",x=rnorm(10, 36,de), y=rnorm(10, 13,de))
g<-data.frame(id="g",x=rnorm(20, 30,de), y=rnorm(20, 2,de))
h<-data.frame(id="h",x=rnorm(25, 10,de), y=rnorm(25, 14,de))
i<-data.frame(id="a",x=rnorm(17, 15,de), y=rnorm(17, 2,de))
j<-data.frame(id="a",x=rnorm(15, 33,de), y=rnorm(15, 15,de))

data<-rbind(a,b,c,d,e,f,g,h)
plot(data[,2],data[,3])
hist(data[,2])
hist(data[,3])
qqnorm(data[,2])
qqline(data[,2])
qqnorm(data[,3])
qqline(data[,3])

plot(data[,2],data[,3])
NNhull<-quick_hull(data[2:3])
lines(data[c(NNhull,NNhull[1]),2:3],col="red")


# z<-data
# 
# source(file.path(getwd(), "R-scripts", "point_euclid.R"))
# 
# 
# 
# z<- na.exclude(z)
# z<-z[,2:3]
# 
# ##creating a file to dump values
# file.create("bin.csv")
# bin<-matrix(NA, 1,1) 
# 
# ##Using sebs script to create hulls
# cz<-quick_hull(z)                               ############
# ##CONTROLS##
# ############
# ## there are two control methods atm; the first is to define the length of the yardstick. Provides an undefined number
# ## of end-members. the second is to use an equation which most likely is data specific.
# 
# ##While the script below runs, the number in "eq1" is the number of end members the algorythm gets (approximately)
# 
# # eq1 <- function (value) {exp((value + 4.7671)/-64.85)}
# # factor<-eq1(35)
# 
# ys<-10      ##starting parameter for yardstick
# factor<-.5  ##creating the factor by which the yardstick length is modified
# 
# ##I want the loop to start here. Set the threshold value (0 will include too many close points)
# 
# while (ys>1)
#   
# {
#   ##sum of rows
#   czr<-z[cz,]
#   czr<-czr^2
#   czrsum<-rowSums(czr)
#   fin<-sqrt(czrsum)
#   finm<-as.matrix(fin)  
#   
#   ##rows with max and min euclidean distance from zero
#   refmax<-(which.max(finm))
#   
#   ##getting maximum value and anchoring it to the row number in the master data set (z)
#   BLARG<-rownames(z[cz,])==cz[refmax]
#   BLARG<-as.matrix(cz[BLARG])
#   
#   ##retrieving all the principal component data from rows that contain maximum and minimum euclidean distances
#   rowx<-z[BLARG,]
#   #rowy<-z[BLURG,]
#   
#   ## retrieving all pc data from cz 
#   object<-z[cz,]
#   #object<-z[finm,]
#   
#   ##getting distances
#   pcdist<-as.matrix(point_euclid(object,rowx))
#   #pcdisty<-as.matrix(point_euclid(object,rowy))
#   
#   ##yardstick
#   b<-as.numeric(pcdist[which.max(pcdist),])
#   ys<-b*factor 
#   
#   ##compare yardstick to the convex hull
#   new <- ys < as.vector(pcdist)
#   new <- as.matrix(new)
#   
#   ##Placing maximum (maxi) and minimum (origin) points in the final file
#   
#   origin <- as.matrix(pcdist == 0)
#   or <- as.matrix(pcdist[origin,])
#   bin <- rbind(or,bin)
#   
#   #    max <- as.matrix(pcdist==b)
#   #    maxi <- as.matrix(pcdist[max,])
#   #    bin <-rbind(maxi,bin)
#   
#   
#   ##Exclude any values inferior to yardstick (this file should be renamed cz when its time to reiterate)
#   finm <- as.matrix(pcdist[new,])
#   
#   ##Create a new object to replace the previous convex hull list
#   cz<-as.numeric(rownames(finm))
#   
#   print(ys)
#   
#   
# }
# 
# # ## removing duplicates
# # a<-rownames(bin)
# # s<-as.matrix(unique(a))
# # 
# # ##Output
# # 
# # write.csv(s, file = "ems.csv")
# # 
# # 
# # sz<-z[s,]
# # write.csv(sz,file="sz.csv")
# # ##test accuracy of hull. If data is greater than any hull points 
# # 
# # ##matlab code
# # ##Use only if you are performing an analysis on extrogrades.
# # y<-sz[1:20,]
# # write.csv(y,file="bend.csv")