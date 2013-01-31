##stupid little script to save me from the worlds largest if statement. Gets output from matlab and
##finds the largest value in each row.

#a<-read.table("matmem.csv",sep=",")
#a<-read.table("edg_dist_2084.txt",sep=",",header=TRUE)
#a<-read.table("std.txt",sep=",",header=TRUE)

setwd("C:/Users/phug7649/Desktop/TXTBIN")
a<-read.table("Distances_2072.csv",sep=",",header=TRUE,check.names = FALSE)
aa<-as.matrix(a)
library(plyr)
a$max<-apply(aa,1,which.max)
a$max<-letters[a$max]
head(a)
write.csv(a, "max_2072.csv")