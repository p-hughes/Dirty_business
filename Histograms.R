##alex wants some horizon thickness data. This script works after the the akro_check script.
##source(akro_check_buggered.r)
raw_data<-read.csv("phil1.txt",sep=",",header=T)
head(raw_data)
hzn<-raw_data[,c(2,27,28)]
head(hzn)
hzn$thick<-hzn$hzn_bot-hzn$hzn_top
head(data.complete)
cent.thick<-merge(data.complete,hzn,by= "natural_key")
head(cent.thick)
nrow(cent.thick)

cent.thick<-droplevels(cent.thick)
length(levels(cent.thick$max))
##really REALLY useful chunk of code. selects something within something else
##cent.thick$max %in% c("alpha")

library(plyr)
pdf("thickness_histogram.pdf")
d_ply(cent.thick, .(max), summarise, 
      hist(thick, xlim=c(0,60),ylim= c(0,400),col = "royalblue", border= "white", main=paste0("Thickness: ", unique(max)))
      )
dev.off()