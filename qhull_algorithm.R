set.seed(210812)

rows <- 6000
cols <- 1
sample_mat <- matrix(rnorm(cols*rows), ncol=cols)


# euc_dists <- data.frame(id=seq_len(rows), dist=sqrt(rowSums(sample_mat)^2))
# #Extract only the furthest 10% of points from 0
# top10_perc <- euc_dists[with(euc_dists, order(dist)),][seq_len(ceiling(rows/10)),]$id

combs <- t(combn(cols, 2))
hull_list <- list()

for(i in 1:nrow(combs)){
  hull_list[[i]] <- chull(sample_mat[, combs[i,1]], sample_mat[, combs[i,2]])
}

test_hull_points <- unique(unlist(hull_list))

library(geometry)
#sample_hull <- convhulln(sample_mat[top10_perc,], options="p")
sample_hull <- convhulln(sample_mat)

hull_points <- unique(as.vector(sample_hull))
