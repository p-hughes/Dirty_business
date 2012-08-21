library(plyr)

set.seed(210812)
rows <- 6000
cols <- 10
data_mat <- matrix(rnorm(cols*rows), ncol=cols)

combs <- combn(ncol(data_mat), 2)

#Function for alply

apply_hull_combination <- function(mat, combns){
  i <- combns[1]
  j <- combns[2]
  
  hull <- chull(mat[,i], mat[,j])
}

hull_list <- alply(combs, 2, apply_hull_combination, mat=data_mat)

quick_hull_points <- unique(unlist(hull_list))


# ##Qhull (really slow) version
# library(geometry)
# 
# sample_hull <- convhulln(data_mat)
# hull_points <- unique(as.vector(sample_hull))
