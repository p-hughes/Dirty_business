##this script is to 

setwd("C:/Users/phug7649/Desktop/txtbin")
data<-read.csv("phil1.txt")
ec<-read.csv("ec.txt")
data<-merge(data,ec, by="natural_key", all=TRUE )
data<-(unique(data))

##y is a temporary column


##fixing up pH data
#First: replace missing pH values (1:5 in water) with paste pH values which are roughly equivalent). 
#y is a bin column which will contain the results of this action.
y=ifelse(is.na(data[,4]),data[,5],data[,4])
data[,4]<-y
head(data)
##replacing NA with 1 so that the regression will include na values for ec
#y is a bin column which will contain the results of this action.
y=ifelse(is.na(data[,32]),1,data[,32])
data[,32]<-y
##Next: convert CaCl2 pH to water pH via budi's equation.
#y is a bin column which will contain the results of this action.
y=ifelse(is.na(data[,4]),(data[,3]-0.14*log(data[,32]))/9,data[,4])
data[,4]<-y

y=ifelse(is.na(data[,6]),ifelse(data[,4]<7.5,0,NA),data[,6])
data[,6]<-y

##Using composition to ensure the sand/silt clay fraction is as complete as possible
y=ifelse(is.na(data[,11]),100-data[,12]+data[,13],data[,11])
data[,11]<-y

y=ifelse(is.na(data[,12]),100-data[,11]+data[,13],data[,12])
data[,12]<-y

y=ifelse(is.na(data[,13]),100-data[,12]+data[,11],data[,13])
data[,13]<-y

#check step
data <- data[,c(1,4,6:8,11:13,26:31)]
check<-na.exclude(data)

##have a look at the CN ratio. May be able to pick up missing C or N values.

head(data)
plot(data[,6],data[,4])












