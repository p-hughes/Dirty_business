


##this will turn matlab output into a pretty graph without having to think about it.
library(Cairo)
library(plyr)
library(ggplot2)
library(grid)
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/make_letter_ids.R")

##input files for matlab:

setwd("C:\\Users\\phug7649\\Documents\\MATLAB")
matrix <- read.table("matrix.csv", sep=",")
end_points <- read.csv("EP.csv", sep=",")
data <- read.csv("DATA.csv", sep=",")

### I need to find out how many end points, how many centroids. This needs to be converted into letters (end points)

##matlab output files:
centroid_table <- read.csv("mcent.csv",header=FALSE,sep=",")
natural_key <- make_letter_ids(nrow(centroid_table))
centroid_table<-cbind(natural_key,centroid_table)

##joining original data to the centroid table, to be used in PCA 2 lines later.
names(centroid_table) <- names(data)
data_cent<-rbind(data,centroid_table)
##creating principal components from the original data.
princomp_main<-princomp(data_cent[,2:ncol(data)],cor=TRUE)
princomp_comp<-princomp_main$scores

##creating max distance column
#data distances is the distance of each point to all of the centroids.
data_distances<-read.csv("mdist.csv",sep=",",header=F)  
id.matrix<-diag(nrow(centroid_table))
id.matrix<-cbind(id.matrix,natural_key)

data_cent.prin<-cbind(data_cent,princomp_comp)