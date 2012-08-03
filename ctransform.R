
setwd("C:/Users/phug7649/Desktop/TXTBIN")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/NAtest.r")
NAtest("Carbontop_II_6095.txt")
a<-read.table("Carbontop_II_6095.txt", sep=",", na.strings="", header=TRUE)
a$oc<-sqrt(a$oc)
write.csv(a,"Carbontop_II_6095_sqrt.csv")
a<-read.table("Carbontop_II_6095.txt", sep=",", na.strings="", header=TRUE)
a$oc<-log(a$oc)
write.csv(a,"Carbontop_II_6095_log.csv")

