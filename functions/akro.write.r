akro.write<-function(akro, destination="C:\\Users\\phug7649\\Documents\\MATLAB"){
  write.csv(akro$matrix, paste(destination, "matrix.csv", sep="\\"), row.names=FALSE)
  write.csv(akro$points, paste(destination, "EP.csv", sep="\\"), row.names=FALSE)
  write.csv(akro$input, paste(destination, "DATA.csv", sep="\\"), row.names=FALSE)
  write.table(akro$parameters,paste(destination, "control.txt", sep="\\"),row.names=F,col.names=F)
  shell("matlab -nodesktop -nosplash -wait -r am")
}