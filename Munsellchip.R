#setwd("C:/Users/mnel6409/Seb Dropbox/Dropbox/BIOM Extra Tutorial")
#setwd("C:/Users/bmal4866/Desktop/TXTBIN")
setwd("C:/Users/phug7649/Desktop/TXTBIN")
## This is where the munsell colours are.

install.packages("munsell")
library(munsell)

##Lets make our hues
#mnsl_US <- read.table("US_MUNS.txt", header=T, sep=",")

##input file name
mnsl_US <- read.table("cfix_92132.txt", header=T, sep=",")

##Remove NA's and make the data readable for r functions'

mnsl_US[mnsl_US=="*"] <- NA
mnsl_US[mnsl_US=="N"] <- NA
mnsl_US[mnsl_US==""] <- NA
mnsl_US <- droplevels(mnsl_US)

## much of this data was in the factor column. Even though 
## deleted, quotes and NA's can be rememberedby the program 
## as levels. Droplevels removes these unnecesary quotes 
## and NA's.

##unsure as to why we need ggplot. Its cool though.

install.packages("ggplot2")
library(ggplot2)

##time to diverge from the munsell colour converting software.

x<-read.table("Ctrial.txt", sep=",", header=T)


##This x needs to have each row matched with a munsell tile that is closest in colour. 
##Euclidean distances would be the best method.



mnsl_hues <- hue_slice(levels(mnsl_US$phcolor_colorhue))
head(mnsl_US)

###This is sebs work^

mnsl_data<-mnsl_hues$data
mnsl_LUV<-as.matrix(mnsl_data[,c("L","U","V")])
mnsl_LUV[is.na(mnsl_LUV)]<-FALSE
mnsl_LAB<-convertColor(mnsl_LUV,"Luv","Lab")

colconvert<-data.frame(mnsl_data$name,mnsl_LAB)
names(colconvert)<-c("munsell","L","A","B")
munsLAB.df<-colconvert[!is.na(colconvert$munsell),]

munsl_cols <- with(mnsl_US, paste(phcolor_colorhue, " ", phcolor_colorvalue, "/", phcolor_colorchroma, sep=""))
munsl_cols[munsl_cols=="NA NA/NA"] <- NA
#munsl_cols[!munsl_cols%in%munsLAB.df$munsell] <- NA
mnsl_CIELAB <- data.frame(ID=1:length(munsl_cols), munsell=munsl_cols)#, stringsAsFactors=F, L=NA, A=NA, B=NA)
# #mnsl_CIELAB$munsell <- droplevels(mnsl_CIELAB$munsell)
# #levels(mnsl_CIELAB$munsell) <- levels(munsLAB.df$munsell)
# 
# library(doSMP)
# library(plyr)
# 
# work <- startWorkers(6)
# registerDoSMP(work)
# 
# mnsl_yay <- ddply(mnsl_CIELAB, .variables="munsell", function(x){munsLAB.df[munsLAB.df$munsell==x$munsell,][,-1]}, 
#                   .parallel=F, .progress="text")
# 
# stopWorkers(work)
library(plyr)

yay <- join(mnsl_CIELAB, munsLAB.df)
write.table(yay, "cfix_LAB_92132.txt")