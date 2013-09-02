##meson- the function that turns matlab output into a lovely table.

meson<-function(matlab="C:\\Users\\phug7649\\Documents\\MATLAB",
                matrix= read.table(paste(matlab, "matrix.csv", sep="\\"),sep=","),
                end_points=read.csv(paste(matlab, "EP.csv", sep="\\"),sep=","),
                data=read.table(paste(matlab, "DATA.csv", sep="\\"),sep=",",header=TRUE),
                weighting_factor=read.csv(paste(matlab, "weighting.csv", sep="\\"),sep=","),
                number_of_rows=read.csv(paste(matlab, "rows.csv", sep="\\"),header=FALSE,sep=","),
                number_of_end_members=read.csv(paste(matlab, "end.csv", sep="\\"),header=FALSE,sep=","),
                number_of_centroids=read.csv(paste(matlab, "cent.csv", sep="\\"),header=FALSE,sep=","),
                data_distances=read.csv(paste(matlab, "mdist.csv", sep="\\"),header=F,sep=","),
                centroid_table=read.table(paste(matlab, "mcent.csv", sep="\\"),sep=",")){ 
  
  source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/make_letter_ids.R")
  source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/which.greater.r") 
  
  w<-weighting_factor[1,1]
  a<-nrow(centroid_table)
  mat.max<-make_letter_ids(nrow(matrix), letters[6:26])
  
  
  
  number.of.end.points<-number_of_centroids-number_of_end_members #this looks like the number of central clusters
  
  ############################# making the "max" column work with end and central points ###########################
  
  greek.alpha<-c("alpha","beta","gamma","delta","epsilon","zeta","eta","theta","iota","kappa","lambda","mu","nu","xi",
                 "omicron","pi","rho","sigma","tau","upsilon","phi","chi","psi","omega")
  cent.max<-make_letter_ids(unlist(number_of_centroids)-nrow(matrix),greek.alpha)
  max<-c(mat.max,cent.max)
  names(data_distances)<-max
  
  centroid_table<-cbind(max,centroid_table)
  
  
  ##joining original data to the centroid table, to be used later.
  names(centroid_table) <- names(data)
  data_cent<-rbind(data,centroid_table)
  ##creating principal components from the original data.
  princomp_main<-princomp(data_cent[,2:ncol(data)],cor=TRUE)
  princomp_comp<-princomp_main$scores
  
  
  
  ##creating max distance column
  
  id.matrix<-diag(nrow(centroid_table))
  id.matrix<-cbind(id.matrix,max)
  natural_key<-max
  data_cent.prin<-cbind(data_cent,princomp_comp)  
  
  ##############################################  PUTTING DATA TOGETHER / MAKING CENTROID TABLE  #############################################  
  aa<-as.matrix(data_distances)
  data_distances$fifty<-apply(aa,1,which.greater, n=0.5)
  data_distances$max<-apply(aa,1,which.max)
  data_ratio<-data_distances
  data_ratio$fifty<-apply(aa,1,which.greater, n=0.5)
  data_ratio$max<-apply(aa,1,which.max)
  
  
  
  mat.max<-make_letter_ids(nrow(matrix)-1, letters[6:26])
  c.max<-make_letter_ids(nrow(matrix)-1, letters[6:26])
  cent.max<-make_letter_ids(unlist(number_of_centroids)-(nrow(matrix)-1),greek.alpha)####here!!
  max<-c(mat.max,cent.max)
  data_distances$max<-max[data_distances$max]
  max<-data_distances$max
  data_distances$fifty[!is.na(data_distances$fifty)] <- data_distances$max[!is.na(data_distances$fifty)]
  ##data_distances$fifty is still numeric. Needs to be in greek.
  
  
  max<-t(max)
  max<-c(max,mat.max,cent.max) ##cent.max is the problem. Line 95-> matrix. Line 44
  #browser()----data.complete should have a fifty column now...
  data.complete<-cbind(data_cent.prin,max)
  fifty<-data_distances$fifty
  maxend<-data.complete$max[(nrow(data_distances)+1):nrow(data.complete)]
  data.complete$fifty<-c(fifty,maxend)
  datarows<-nrow(data) ##rows of data
  position1<-datarows+1  ##+1
  cdatarows<-nrow(data.complete) ##rows plus centroids
  centroids.complete<-data.complete[position1:cdatarows,]
  emno<-number_of_end_members[1,1]
  ceno<-nrow(centroid_table)-emno
  soil.id<-rep(c("E", "C"), c(emno, ceno))
  centroids.complete<-cbind(centroids.complete,soil.id)
  message("output is centroids.complete, data.complete and data_ratio")
  Tables<-list(centroids.complete=centroids.complete,data.complete=data.complete,data_ratio=data_ratio)
  
  
}