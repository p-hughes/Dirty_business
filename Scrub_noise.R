## this is a script which removes small numbers of categories from a large data set, and places them in a large
## homogenous group.

a<-500

setwd("C:/Users/phug7649/Desktop/TXTBIN")
data2<-read.table("SACF.txt",sep=",",header=TRUE,stringsAsFactors=F)
tmp <-data2[,3] 
table(tmp)
criteria <- names(table(tmp))[table(tmp)<a]
tmp[tmp %in% criteria] <- 'Mixed'
table(tmp)
write.table(tmp, "EALLOUT.txt")
