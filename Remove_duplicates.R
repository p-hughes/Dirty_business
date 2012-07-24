#setwd("C:/Users/bmal4866/Desktop/TXTBIN")
setwd("C:/Users/phug7649/Desktop/TXTBIN")



US_data <- read.table("Texture_improved.txt", header=T, sep=",")
#US_data <- read.csv("USDATA_for_r.csv", header=T, sep=",")

#head(US_data,10)

#min(which(!is.na(US_data$peiid)))

#US_data[124:134,]

sub_US_data<-with(US_data, data.frame(upedonid, hzdept, hzdepb))

nodup_US_data <- which(!duplicated(sub_US_data))
#optional
#nodup_US_data <- nodup_US_data[-1]

US_no_dup <- US_data[nodup_US_data,]

write.csv(US_no_dup, "Textures.csv", row.names=FALSE)
write.table(US_no_dup, "Textures.txt")


