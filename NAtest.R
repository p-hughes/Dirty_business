setwd("C:/Users/phug7649/Desktop/TXTBIN")
NAtest <- function (file) {
  test<-read.table(file,sep=",")
  z<-na.exclude(test)
  if(nrow(test)==nrow(z)) {
    print("all is well")
    } else {
      print("WARNING: NA's present! ABORT ABORT")
      }
}
# Line 3 if this goes astray: #test<-read.table("CarbonK1_nogap_top_K.txt",sep=",")
NAtest("CarbonK1_nogap_top_K.txt")