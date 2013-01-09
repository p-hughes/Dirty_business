##stupid little script to save me from the worlds largest if statement. Gets output from matlab and
##finds the largest value in each row.

#a<-read.table("matmem.csv",sep=",")
#a<-read.table("edg_dist_2084.txt",sep=",",header=TRUE)
setwd("C:/Users/phug7649/Desktop/TXTBIN")
a<-read.table("std.txt",sep=",",header=TRUE)
aa<-as.matrix(a)
library(plyr)
a$max<-apply(aa,1,which.max)
head(a)
write.csv(a, "std.csv")