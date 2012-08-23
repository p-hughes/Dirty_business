
library(plyr)
setwd("C:/Users/phug7649/Desktop/TXTBIN")
original_data <- read.table("extragrades.txt",sep=",", na.strings="", header=TRUE)



y<-as.matrix(data[,2:ncol(original_data)])
prin<-princomp(y, cor=TRUE)
class(prin)
components<-prin$scores
princomp_data <- components

#Function for alply

apply_hull_combination <- function(mat, combns){
  i <- combns[1]
  j <- combns[2]
  
  hull <- chull(mat[,i], mat[,j])
}

quick_hull <- function(data_mat){
  
  combs <- combn(ncol(data_mat), 2)
  hull_list <- alply(combs, 2, apply_hull_combination, mat=data_mat)
  
  quick_hull_points <- unique(unlist(hull_list))
  
}

sub_hull <- intersect(quick_hull(original_data), quick_hull(princomp_data))

original_sub_L <- original_data[quick_hull(original_data), ]
original_sub <- original_data[sub_hull, ]
princomp_sub_L <- princomp_data[quick_hull(princomp_data), ]
princomp_sub <- princomp_data[sub_hull, ]

original_dist <- dist(original_sub)

##hull coordinates listed

original.xy <- data.frame(t(combn(rownames(original_sub), 2)), as.numeric(original_dist))
names(original.xy) <- c("p1", "p2", "distance")
original.xy_sorted <- original.xy[order(original.xy$distance),]

original.xy_sorted[(nrow(original.xy_sorted)%/%2-5):(nrow(original.xy_sorted)%/%2+5), ]

# ##Qhull (really slow) version
library(geometry)
# 
 sample_hull <- convhulln(data_mat)
 hull_points <- unique(as.vector(sample_hull))

#BEFORE I FORGET:

# Get the set of all euclidean distances (mahalanobis, maybe).
library(fields)
ref<-rdist(components[,1],components[,2])##once you have your hull file, substitute.This one produces 6095^2 numbers.

# Find the values closest to the median (hoping these are the most evenly spaced distances.
#    which should work, given these are all hull points.
# Get the number of points we want and hey presto.
# Maybe get furthest values as well? Maybe not. If points are evenly spaced, then no. If not, then yes?