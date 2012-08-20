##The purpose of this script is to interrogate data and retrieve the convex hulls. The coordinates of the convex hulls also coincide


###Set working directory, read in the data###

setwd("C:/Users/phug7649/Desktop/TXTBIN")
#z<-read.table("Alex_Hull.txt", sep=",", na.strings="", header=TRUE)
z<-read.table("1_15 22_161534_top_14410_princ.txt", sep=",", na.strings="", header=TRUE)
####Apply values to columns####

z<- na.exclude(z)
combs <- combn(seq_len(ncol(z)), 2)
  
###The almighty for loop###

for (j in 1:ncol(combs)){
  
  a<-combs[1,j]
  i<-combs[2,j]
  
  zna <- z[,c(a,i)]
  
  cz <- chull(zna)
  
  ##creating individually named files for the output##
  
  png(file.path(getwd(), "chulls", paste0("output_", a, "_", i, ".png")))
  plot(zna)
  lines(zna[c(cz, cz[1]),], col="red")
  dev.off()
  
  #eval(parse(text=paste('output_',i,a,'<-zna[c(cz, cz[1]),]',sep='')))
  assign(paste0('output_', i,"_", a), zna[c(cz, cz[1]),])
}

###inserting euclidean function ###NOTE### This script only functions for data with column names "Prin1" and "Prin7". I'm working
###on the problem...

Distances<-(dist(output))
Distances
which.max(Distances)

##my attempt at getting max and min values A bit clunky but it works!
b<-1
i<-which.max(z[,b])
z[i,b]
i

i<-which.min(z[,b])
z[i,b]
i

##Jose's fix
zmin<-apply(as.matrix(na.exclude(z)),2,min)
names(zmin)<-apply(as.matrix(na.exclude(z)),2,which.min)
zmin

zmax<-apply(as.matrix(na.exclude(z)),2,max)
names(zmax)<-apply(as.matrix(na.exclude(z)),2,which.max)
zmax

###Convex hulls in n dimensions

library(geometry)
install.packages("geometry")
ps<-(as.matrix(na.exclude(z), ncols=10))
ps <- matrix(z, ncol=10)  # generate points on a sphere
ps <- as.matrix(z)

##this will cause problems...

y<-convhulln(ps, options = "Tv")







