NAtest <- function(file, ..., sep=",", header=TRUE, na.rm=TRUE){
  
  test <- read.table(file, sep=sep, header=header, ...)
  orig_rows <- nrow(test)
  
  if(na.rm){
    test <- na.exclude(test)
    if(orig_rows == nrow(test)){
      message("No NAs present: All is well")
      } else {
        warning("NAs present! ABORT ABORT")
        }
    }
  
  
  return(test)
  }
# Line 3 if this goes astray: #test<-read.table("CarbonK1_nogap_top_K.txt",sep=",")
# NAtest("CarbonK1_nogap_top_K.txt")