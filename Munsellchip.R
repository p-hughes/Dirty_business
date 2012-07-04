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

LAB<-read.table("Ctrial.txt", sep=",", header=T)






mnsl_hues <- hue_slice(levels(mnsl_US$phcolor_colorhue))
head(mnsl_US)

###This is sebs work, converting LUV data to LAB

mnsl_data<-mnsl_hues$data
mnsl_LUV<-as.matrix(mnsl_data[,c("L","U","V")])
mnsl_LUV[is.na(mnsl_LUV)]<-FALSE
mnsl_LAB<-convertColor(mnsl_LUV,"Luv","Lab")

colconvert<-data.frame(mnsl_data$name,mnsl_LAB)
names(colconvert)<-c("munsell","L","A","B")
munsLAB.df<-colconvert[!is.na(colconvert$munsell),]

##This x needs to have each row matched with a munsell tile that is closest in colour. 
##Euclidean distances would be the best method.




euc <- function(dat, x1, y1, z1){
  
  
  x<-dat$Main_L
  y<-dat$Main_A
  z<-dat$Main_B
  
  answer <- sqrt((x-x1)^2+(y-y1)^2+(z-z1)^2)
  
  return(answer)
  
}

y <- matrix(NA, )
##refLAB <- munslab.df
for(i in 1:nrow(munsLAB.df)){
  L <- munsLAB.df$L[i]
  A <- munsLAB.df$A[i]
  B <- munsLAB.df$B[i]
}
  y[i,] <-euc(LAB,L,A,B)
  
#for(i in 1:nrow(mns_data)){
#  L <- mnsl_data$L[i]
#  U <- mnsl_data$U[i]
#  V <- mnsl_data$V[i]
    
#  y[i,] <-euc(LAB,L,U,V) 
  
  
  y<-euc(LAB,1,2,3)
  
  
  which.min
  
  ysquare <- function(number)
  {
    answer <- number * number
    return(answer)
  }
  square(2)
  
  
  dist(x, mnsl_data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

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