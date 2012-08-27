##The purpose of this script is to interrogate data and retrieve the convex hulls. The coordinates of the convex hulls also coincide


###Set working directory, read in the data###

setwd("C:/Users/phug7649/Desktop/TXTBIN")
#z<-read.table("Alex_Hull.txt", sep=",", na.strings="", header=TRUE)
#z<-read.table("1_15 22_161534_top_14410_princ.txt", sep=",", na.strings="", header=TRUE)
z<-read.table("Carbon_comp_6094.txt", sep=",", na.strings="", header=TRUE)
####Apply values to columns####

z<- na.exclude(z)
combs <- combn(seq_len(ncol(z)), 2)
file.create("bin.csv")
bin<-matrix(NA, 1,1) 
#names(bin)[2:3]<-c("row","distance")
###The almighty for loop###

for (j in 1:ncol(combs)){
  
  a<-combs[1,j]
  i<-combs[2,j]
  zna <-z[,c(1,2)]
  zna <- z[,c(a,i)]
  
  cz <- chull(zna)
  
  ##added script for the "convex bicycle"
  
  #which.max(rdist(z[cz,]))
  #z[cz,]
  #head(z)
  #str(z)
  #head(cz)
  #str(odist)
  #czmax<-apply(z[cz,],2,which.max)
  
  ##sum of rows
  czr<-z[cz,]
  czr<-czr^2
  czrsum<-rowSums(czr)
  fin<-sqrt(czrsum)
  finm<-as.matrix(fin)
  
  ##max euclidean distance
  
  refmax<-which.max(fin)
  refmin<-which.min(fin)
  
  ##distance between refmax and others
  
  stref<-abs(finm-refmax)
  names(stref)[1:2]<-c("row","distance")
  
  ob2<-refmax/2
  new <- stref > as.vector(ob2)
  
  
  ans <- as.matrix(stref[new,])
  
  bin<-rbind(ans,bin)
 
  
  
  
  #rownames(group)
  #odist<-rdist(z[cz,sum()])
  #cziimax<-which.max(odist)
  
  ##czmax<-apply(as.matrix(na.exclude(z[cz,])),2,max)
 # czmin<-apply(as.matrix(na.exclude(cz)),2,min)  
  #ref<-rdist(z[czmax,],z[czmin,])
  
  
  
  #ref2<-rdist(z[czmax,],z[cz,])
  #tref2 <- t(ref2)
  #s<-as.matrix(cz)
  #stref<-merge(s,tref2, by = "row.names", all=TRUE)
  #names(stref)[2:3]<-c("row","distance")
  

  #s$ref2<-tref2
  
  #new <- stref$distance > as.vector(ob2)
  #ans <- stref[new,]
  
#   cat(which(!ans), "\n")
  
 # write.csv(ans,file.path(getwd(), "comph", paste0("comph_", a, "_", i, ".csv")))
  
  

  
  
  ##creating individually named files for the output##
  
  png(file.path(getwd(), "chulls", paste0("output_", a, "_", i, ".png")))
  plot(zna,
       xlim=c(-15,15),
       ylim=c(-15,15)) 
  lines(zna[c(cz, cz[1]),], col="red")
  dev.off()
  
  #eval(parse(text=paste('output_',i,a,'<-zna[c(cz, cz[1]),]',sep='')))
#   assign(paste0('output_', i,"_", a), zna[c(cz, cz[1]),])
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

#y<-convhulln(ps, options = "Tv")







