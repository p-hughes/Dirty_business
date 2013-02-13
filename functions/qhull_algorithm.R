library(plyr)

quick_hull <- function(data_mat, choose=2){

  combs <- combn(ncol(data_mat), choose)
  
  if(choose <= 2){
    hull_list <- alply(combs, 2, apply_hull_combination, mat=data_mat)
  } else {
    if(!require(geometry)) stop("Package 'geometry' required for combinations greater than 2")
    hull_list <- alply(combs, 2, apply_convhull, mat=data_mat)
  }

  quick_hull_points <- unique(unlist(hull_list))
  
}

# #Functions for alply

apply_hull_combination <- function(mat, combns){
  hull <- chull(mat[,combns[1]], mat[,combns[2]])
}

apply_convhull <- function(mat, combns){
  hull <- convhulln(mat[,as.numeric(combns)])
}

#BEFORE I FORGET:

# Get the set of all euclidean distances (mahalanobis, maybe).
# Find the values closest to the median (hoping these are the most evenly spaced distances.
#    which should work, given these are all hull points.
# Get the number of points we want and hey presto.
# Maybe get furthest values as well? Maybe not. If points are evenly spaced, then no. If not, then yes?



