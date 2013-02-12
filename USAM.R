setwd("C:/Users/phug7649/Desktop/txtbin")
data<-read.csv("phil1.txt")





y=ifelse(is.na(data[,4]),data[,5],data[,4])
z<-na.exclude(y)
length(z)-length(y)
m<-is.na(y)
r<-sum(m)
p<-is.na(data[,4])
q<-sum(p)  
q-r



