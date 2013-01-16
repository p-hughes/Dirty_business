##for consistent results, set the seed.
#setwd("C:/Users/phug7649/Desktop/TXTBIN")

source("./functions/qhull_algorithm.R")
library(ggplot2)
set.seed(20120927)



# de<-2
# points<-5
# ends<-1
# a<-data.frame(id="EM1",x=rnorm(ends, 5,de), y=rnorm(ends, 5,de),class=1)
# b<-data.frame(id="EM2",x=rnorm(ends, 100,de), y=rnorm(ends, 5,de),class=1)
# c<-data.frame(id="EM3",x=rnorm(ends, 50,de), y=rnorm(ends, 95,de),class=1)
# d<-data.frame(id="d",x=rnorm(points, 50,de), y=rnorm(points, 15,de),class=2)
# e<-data.frame(id="e",x=rnorm(points, 30,de), y=rnorm(points, 40,de),class=2)
# f<-data.frame(id="f",x=rnorm(points, 50,de), y=rnorm(points, 40,de),class=2)
# g<-data.frame(id="g",x=rnorm(points, 70,de), y=rnorm(points, 40,de),class=2)
# h<-data.frame(id="h",x=rnorm(points, 50,de), y=rnorm(points, 66.7,de),class=2)
# i<-data.frame(id="EX1",x=rnorm(1, 80,de), y=rnorm(1, 15,de),class=3)
# j<-data.frame(id="EX2",x=rnorm(1, 20,de), y=rnorm(1, 15,de),class=3)
# 
# 
# 
# colours()
# 
# data<-rbind(a,b,c,d,e,f,g,h,i,j)
# plot(data[,2],data[,3],main="soil classification by fuzzy sets",xlab="x",ylab="y",col=0)
# text(data$x,data$y,data[,1])
# hull<-quick_hull(data[,2:3])
# lines(data[c(hull,hull[1]),2:3],col="red")
# hist (data[,3],main="Histogram of y column",nclass=10,col="cornflowerblue")
# hist (data[,2],main="Histogram of x column",nclass=10,col="steelblue")
# 
# write.csv(data,file="triangle.csv")
# 
# ##funky plot time
# 
# hull<-quick_hull(data[,2:3])
# hull_data <- data[c(hull,hull[1]),]
# 
# ggplot(data, aes(x=x, y=y))+
#   #theme_grey()+
#   theme_bw()+
#   geom_text(aes(label=id))+#, colour=factor(class)))+
# #  scale_colour_manual(values=rbgpal(3))+
# #  scale_colour_brewer(palette="Set1")+
# #  geom_rug(aes(colour=factor(class)))+
#   geom_path(data=hull_data,colour="red")+
#   coord_equal()
# 
# #text(data$x,data$y,data[,1])
# 
# qplot(data=data,y, type=)
# ?ggplot2

##Alex/budis suggestion- A square data set.

set.seed(20120927)
de<-2
points<-20
ends<-1

f<-data.frame(id="EM1",x=rnorm(ends, 5,de), y=rnorm(ends, 5,de))
g<-data.frame(id="EM2",x=rnorm(ends, 100,de), y=rnorm(ends, 5,de))
h<-data.frame(id="EM3",x=rnorm(ends, 5,de), y=rnorm(ends, 95,de))
i<-data.frame(id="EM4",x=rnorm(ends, 95,de), y=rnorm(ends, 95,de))

a<-data.frame(id="a",x=rnorm(points, 50,de), y=rnorm(points, 25,de))
b<-data.frame(id="b",x=rnorm(points, 25,de), y=rnorm(points, 50,de))
c<-data.frame(id="c",x=rnorm(points, 50,de), y=rnorm(points, 50,de))
d<-data.frame(id="d",x=rnorm(points, 75,de), y=rnorm(points, 50,de))
e<-data.frame(id="e",x=rnorm(points, 50,de), y=rnorm(points, 75,de))

w<-data.frame(id="EX1",x=rnorm(1, 80,de), y=rnorm(1, 15,de))
y<-data.frame(id="EX2",x=rnorm(1, 20,de), y=rnorm(1, 15,de))
x<-data.frame(id="EX3",x=rnorm(1, 80,de), y=rnorm(1, 80,de))
z<-data.frame(id="EX4",x=rnorm(1, 20,de), y=rnorm(1, 80,de))

 

data<-rbind(a,b,c,d,e,f,g,h,i,w,x,y,z)

##plotting

hull<-quick_hull(data[,2:3])
hull_data <- data[c(hull,hull[1]),]

ggplot(data, aes(x=x, y=y))+
  #theme_grey()+
  theme_bw()+
  geom_text(aes(label=id))+
  geom_path(data=hull_data,colour="red")+
  coord_equal()

hist (data[,3],main="Histogram of y column",nclass=10,col="cornflowerblue")
hist (data[,2],main="Histogram of x column",nclass=10,col="steelblue")

write.csv(data,file="square.csv")

ggplot(data, aes(x=x, y=y))+
  #theme_grey()+
  theme_bw()+
  geom_text(aes(label=id,colour=id))+
  geom_path(data=hull_data,colour="red")+
  coord_equal()



##Input the text output from fuzzy k means.
##5_class

data2<-read.table(text="id       MaxCls      CI      5a       5b       5c       5d       5e       5* 
  a          5d     0.19363  0.02631  0.04049  0.04417  0.85053  0.01713  0.02137
                  a          5d     0.08901  0.01215  0.02039  0.02254  0.93353  0.00790  0.00349
                  a          5d     0.04958  0.00716  0.01152  0.01266  0.96308  0.00447  0.00110
                  a          5d     0.08437  0.01160  0.01940  0.02143  0.93706  0.00745  0.00306
                  a          5d     0.37571  0.05180  0.06932  0.07456  0.69952  0.02957  0.07523
                  b          5*     0.57573  0.04948  0.11794  0.12522  0.07971  0.07816  0.54949
                  b          5*     0.64893  0.05332  0.13569  0.14485  0.08561  0.08461  0.49592
                  b          5*     0.71004  0.05600  0.14944  0.16245  0.09800  0.08171  0.45241
                  b          5*     0.60155  0.05081  0.12317  0.13223  0.08810  0.07501  0.53068
                  b          5*     0.67028  0.05450  0.14156  0.14989  0.08213  0.09232  0.47961
                  c          5b     0.37320  0.00985  0.79877  0.17197  0.00919  0.00934  0.00088
                  c          5b     0.18232  0.00260  0.90489  0.08722  0.00264  0.00258  0.00006
                  c          5c     0.80853  0.01400  0.38042  0.57189  0.01580  0.01516  0.00273
                  c          5b     0.39630  0.00647  0.79178  0.18808  0.00653  0.00670  0.00045
                  c          5c     0.19412  0.00399  0.09068  0.89657  0.00478  0.00380  0.00017
                  d          5a     0.55765  0.55273  0.11037  0.10457  0.07226  0.05337  0.10670
                  d          5a     0.22240  0.82700  0.04940  0.04564  0.02749  0.03063  0.01984
                  d          5a     0.15379  0.87928  0.03306  0.03114  0.02187  0.02146  0.01319
                  d          5a     0.30484  0.75616  0.06100  0.05669  0.03600  0.04332  0.04684
                  d          5a     0.28066  0.77766  0.05832  0.05526  0.04003  0.03300  0.03575
                  e          5e     0.06540  0.00986  0.01620  0.01477  0.00624  0.95080  0.00212
                  e          5e     0.12826  0.01885  0.02975  0.02746  0.01235  0.90149  0.01010
                  e          5e     0.03320  0.00514  0.00832  0.00760  0.00325  0.97512  0.00058
                  e          5e     0.06548  0.01026  0.01581  0.01453  0.00647  0.95033  0.00259
                  e          5e     0.03213  0.00499  0.00805  0.00735  0.00315  0.97592  0.00054
                  EM1        5*     0.16363  0.01805  0.02601  0.02721  0.03713  0.01811  0.87349
                  EM2        5*     0.15973  0.03268  0.02320  0.02331  0.03129  0.01657  0.87296
                  EM3        5*     0.17712  0.01937  0.02869  0.02851  0.01985  0.04035  0.86323
                  EM4        5*     0.18474  0.03687  0.02686  0.02573  0.01852  0.03838  0.85364
                  EX1        5*     0.40893  0.08768  0.05660  0.05709  0.08558  0.03430  0.67875
                  EX3        5*     0.44406  0.09691  0.06184  0.05805  0.03669  0.09366  0.65285
                  EX2        5*     0.38518  0.03530  0.05677  0.06059  0.09947  0.03357  0.71429
                  EX4        5*     0.32813  0.03174  0.05202  0.05147  0.03202  0.08043  0.75230"
,header=TRUE)
data<-rbind(a,b,c,d,e,f,g,h,i,w,x,y,z)
square_mem <- cbind(data2[,1:2],data[c("x","y")])
centroids <- read.csv("output.csv")
names(centroids)[1] <- "MaxCls"

ggplot(square_mem, aes(x=x, y=y, colour=MaxCls))+
  theme_bw()+
  geom_text(aes(label=id))+
  geom_path(data=hull_data,colour="red")+
  geom_point(data=centroids, colour="black", size=4)+
  coord_equal()

data<-rbind(centroids,data)
ggplot(data, aes(x=x, y=y))+
  #theme_grey()+
  theme_bw()+
  geom_text(aes(label=id,colour=id))+
  geom_path(data=hull_data,colour="red")+
  coord_equal()
data3<-1:9

data3<-read.table(text="
5.34763995739063  6.30850486340641
99.2727579856495	1.89752825066717
7.13103086981294	90.1718221955097
94.8196867565331	95.3581000272730
50.1318047930393	23.4707188977868
75.2746403046578	48.2415777375755
50.6140589081322	74.9821186099562
25.4550234230729	48.8920780603341
49.8546504636677	48.9590239074639")

cent<-read.table("clipboard",sep="")



###Non-normal y column

# set.seed(20120927)
# de<-2
# a<-data.frame(id="a",x=rnorm(1, 2,de), y=rnorm(1, 2,de))
# b<-data.frame(id="b",x=rnorm(20, 20,de), y=rnorm(20, 2,de))
# c<-data.frame(id="c",x=rnorm(3, 40,de), y=rnorm(3, 2,de))
# d<-data.frame(id="d",x=rnorm(5, 2,de), y=rnorm(5, 15,de))
# e<-data.frame(id="e",x=rnorm(30, 20,de), y=rnorm(30, 14,de))
# f<-data.frame(id="f",x=rnorm(10, 36,de), y=rnorm(10, 13,de))
# g<-data.frame(id="g",x=rnorm(20, 30,de), y=rnorm(20, 2,de))
# h<-data.frame(id="h",x=rnorm(25, 10,de), y=rnorm(25, 14,de))
# i<-data.frame(id="a",x=rnorm(17, 15,de), y=rnorm(17, 2,de))
# j<-data.frame(id="a",x=rnorm(15, 33,de), y=rnorm(15, 15,de))
# 
# data<-rbind(a,b,c,d,e,f,g,h)
# plot(data[,2],data[,3])
# hist(data[,2])
# hist(data[,3])
# qqnorm(data[,2])
# qqline(data[,2])
# qqnorm(data[,3])
# qqline(data[,3])
# 
# plot(data[,2],data[,3])
# NNhull<-quick_hull(data[2:3])
# lines(data[c(NNhull,NNhull[1]),2:3],col="red")


##Convex bicycle script

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

# ##centroids from fuzzy k w/extragrades
#  j<-data.frame(id="X",x= 28,y=-50)
#  k<-data.frame(id="X",x= 50,y=-50.6074)
#  l<-data.frame(id="X",x=- 1.35437,y=-4.39542)
#  m<-data.frame(id="X",x= 70.0477,y= -50.1104)
#  n<-data.frame(id="X",x= 0,y= 0)