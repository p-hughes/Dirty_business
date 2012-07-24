setwd("C:/Users/bmal4866/Desktop/TXTBIN")
pedcut <- read.csv("CLAB_imp_92132_8082.txt")

pedcutsub <- with(pedcut, data.frame(peiid, hzdept))
pedcutsub <- data.frame(row=1:nrow(pedcutsub), pedcutsub)
library(plyr)
del_pedon<- function(x){
  if (min(x$hzdept, na.rm=TRUE) != 0){# | any(is.na(x$hzdept )){
    x<- x[-which(x$peiid==x$peiid),]
  } else {
    x<- x[which(x$peiid==x$peiid),]
  }
}
#data<- ddply(pedcut,'peiid',transform,new_pedid=i, .progress="text")
data<-ddply(pedcutsub,'peiid',del_pedon, .progress="text")
roworder <- order(data$row)
data <- data[roworder,]
pedcutsub[which(!pedcutsub$row %in% data$row),]

newpedcut <- data.frame(rowid=data$row, pedcut[data$row,])

write.table(newpedcut, "pedcut.txt",row.names=FALSE)

