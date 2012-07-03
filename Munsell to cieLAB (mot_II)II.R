# half <- function(number){
#   number/2
# }
# ?levels
# ?unique
# ?merge

library(munsell)

#setwd("C:/Users/mnel6409/Seb Dropbox/Dropbox/BIOM Extra Tutorial")
setwd("C:/Users/bmal4866/Desktop/TXTBIN")

##Lets make our hues
##Read in data
#mnsl_US <- read.table("US_MUNS.txt", header=T, sep=",")
mnsl_US <- read.table("US_nodup_col_mot.txt", header=T, sep=",")
#Replace all "*" with NA
mnsl_US[mnsl_US=="*"] <- NA
mnsl_US[mnsl_US==""] <- NA
#Remove all unused levels of mnsl_US (mainly "*")
mnsl_US <- droplevels(mnsl_US)

#make reference dataframe with all reference hues taken from levels of
mnsl_hues <- hue_slice(levels(mnsl_US$phmottles_colorhue))

###This is sebs work^

mnsl_data<-mnsl_hues$data
mnsl_LUV<-as.matrix(mnsl_data[,c("L","U","V")])
mnsl_LUV[is.na(mnsl_LUV)]<-FALSE
mnsl_LAB<-convertColor(mnsl_LUV,"Luv","Lab")

colconvert<-data.frame(mnsl_data$name,mnsl_LAB)
names(colconvert)<-c("munsell","L","A","B")
munsLAB.df<-colconvert[!is.na(colconvert$munsell),]

munsl_cols <- with(mnsl_US, paste(phmottles_colorhue, " ", phmottles_colorvalue, "/", phmottles_colorchroma, sep=""))
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
write.table(yay, "nodup_mot_lab.txt")
