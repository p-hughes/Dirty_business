##stupid little script to save me from the worlds largest if statement. Gets output from matlab and
##finds the largest value in each row.

a<-read.table("matmem.csv",sep=",")
aa<-as.matrix(a)
library(plyr)
a$max<-apply(aa,1,which.max)
head(a)
write.csv(a, "matmem_max.csv")