weighting<-function(data,
                    matlab="C:/Users/phug7649/Documents/MATLAB",
                    matrix=read.table(file.path(matlab,"matrix.csv"),sep=",",header=TRUE),
                    weighting_factor=read.csv(paste(matlab, "weighting.csv", sep="\\"),sep=",")){
  
  
  data.table<-data[['data_ratio']]
  totals<-as.data.frame(table(data.table$max))
  end.tot<-totals[1:nrow(matrix),]
  cent.tot<-totals[nrow(matrix):nrow(totals),]
  sum.end<-sum(end.tot[,2])
  sum.cent<-sum(cent.tot[,2])  
  ratio<-(sum.end/(sum.end+sum.cent))*100
  ratio<-round(ratio,digits=2)
  message((paste0("Weighting ", weighting_factor[1,1],", creating ",ratio, "% end point memberships")))
  weighting<-list(ratio=ratio,totals=totals)
}