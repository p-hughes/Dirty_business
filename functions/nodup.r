
#setwd("C:/Users/phug7649/Desktop/TXTBIN")

##This script only works with data from the NCRS- I will be using known fields.

#dup_data <- data.ion


nodup <- function (dup_data) {
  
  
  
  #sub_dup_data<-with(dup_data, data.frame(natural_key, ph_h2o))
  nodup_data <- which(!duplicated(dup_data))
  dup_data[nodup_data,]

}

# write.csv(US_no_dup, "Column_ID_ndup.csv", row.names=FALSE)
# write.table(US_no_dup, "Textures.txt")

#US_no_dup<-nodup(data.ion)
