setwd("C:/Users/phug7649/Desktop/TXTBIN")

library(munsell); library(plyr)

##Lets make our hues
#mnsl_US <- read.table("US_MUNS.txt", header=T, sep=",")

##input file name
#mnsl_US <- read.table("cfix_92132.txt", header=T, sep=",")
#mnsl_US <- read.table("Munsell_161193.txt", header=T, sep=",")
mnsl_US <- read.table("col_fixed_161193.txt", header=T, sep=",")

#Remove all missing values
mnsl_US[mnsl_US=="*"] <- NA
#mnsl_US[mnsl_US=="N"] <- NA
mnsl_US[mnsl_US==""] <- NA

#Remove all empty factor levels
mnsl_US <- droplevels(mnsl_US)

#Create Munsell table
mnsl_hues <- hue_slice(levels(mnsl_US$phcolor_colorhue))
head(mnsl_US)

#Extract munsell table
mnsl_data <- mnsl_hues$data
#Keep only LUV data
mnsl_LUV <- as.matrix(mnsl_data[, c("L", "U", "V")])
mnsl_LUV[is.na(mnsl_LUV)] <- FALSE
#Convert LUV colours to CIElab
mnsl_LAB <- convertColor(mnsl_LUV, "Luv", "Lab")

colconvert <- data.frame(mnsl_data$name, mnsl_LAB)
names(colconvert) <- c("munsell", "L", "A", "B")
munsLAB.df <- colconvert[!is.na(colconvert$munsell),]

munsl_cols <- with(mnsl_US, paste(phcolor_colorhue, " ", phcolor_colorvalue, "/", phcolor_colorchroma, sep=""))
#Remove invalid Munsell colours DO NOT REMOVE!! USE AN IF STATEMENT AND ADD 1 TO THE FINAL COLUMN, THEN RE
munsl_cols[munsl_cols == "NA NA/NA"] <- NA
#munsl_cols[!munsl_cols%in%munsLAB.df$munsell] <- NA
mnsl_CIELAB <- data.frame(ID=1:length(munsl_cols), munsell=munsl_cols)

#Output final CIELAB colours
final.df <- join(mnsl_CIELAB, munsLAB.df)
write.table(final.df, "cielab_fixed_161193.txt")
