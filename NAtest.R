NAtest <- function(file, ..., sep=",", header=TRUE, na.rm=TRUE, rowstoname=TRUE){
  
  #Collects any number in filename longer than 4 digits
  filenumber <- as.numeric(regmatches(file, regexpr("[[:digit:]]{4,}", file)))
  
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
  if(rowstoname){
    if(nrow(test) != filenumber){
      warning(paste0("Filename suggests ", filenumber, " rows, actually has ", nrow(test), " rows!"))
      } else {
        message(paste0("File has same number of rows as it suggests: ", filenumber))
      }
    }
  }
  
  return(test)
  }
# Line 3 if this goes astray: #test<-read.table("CarbonK1_nogap_top_K.txt",sep=",")
# NAtest("CarbonK1_nogap_top_K.txt")