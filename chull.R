c_hull <- chull(.data)
?chull
setwd("C:/Users/bmal4866/Desktop/TXTBIN")
#z<-data.frame(rnorm(10), rnorm(10))
z<-read.table("prin1_2.txt", sep=",", na.strings="", header=TRUE)
zna1 <- is.na(z[,1])
zna2 <- is.na(z[,2])
znas <- zna1|zna2

zna<-z[!znas,]

cz <- chull(zna)

plot(zna)
lines(zna[c(cz, cz[1]),], col="red")

####

z<-read.table("prin1_3.txt", sep=",", na.strings="", header=TRUE)
zna1 <- is.na(z[,1])
zna2 <- is.na(z[,2])
znas <- zna1|zna2

zna<-z[!znas,]

cz <- chull(zna)

plot(zna)
lines(zna[c(cz, cz[1]),], col="red")
####

z<-read.table("prin1_4.txt", sep=",", na.strings="", header=TRUE)
zna1 <- is.na(z[,1])
zna2 <- is.na(z[,2])
znas <- zna1|zna2

zna<-z[!znas,]

cz <- chull(zna)

plot(zna)
lines(zna[c(cz, cz[1]),], col="red")

###

z<-read.table("prin1_5.txt", sep=",", na.strings="", header=TRUE)
zna1 <- is.na(z[,1])
zna2 <- is.na(z[,2])
znas <- zna1|zna2

zna<-z[!znas,]

cz <- chull(zna)

plot(zna)
lines(zna[c(cz, cz[1]),], col="red")

####

z<-read.table("prin2_3.txt", sep=",", na.strings="", header=TRUE)
zna1 <- is.na(z[,1])
zna2 <- is.na(z[,2])
znas <- zna1|zna2

zna<-z[!znas,]

cz <- chull(zna)

plot(zna)
lines(zna[c(cz, cz[1]),], col="red")

###
z<-read.table("prin2_4.txt", sep=",", na.strings="", header=TRUE)
zna1 <- is.na(z[,1])
zna2 <- is.na(z[,2])
znas <- zna1|zna2

zna<-z[!znas,]

cz <- chull(zna)

plot(zna)
lines(zna[c(cz, cz[1]),], col="red")

####

z<-read.table("prin2_5.txt", sep=",", na.strings="", header=TRUE)
zna1 <- is.na(z[,1])
zna2 <- is.na(z[,2])
znas <- zna1|zna2

zna<-z[!znas,]

cz <- chull(zna)

plot(zna)
lines(zna[c(cz, cz[1]),], col="red")


###

z<-read.table("prin3_4.txt", sep=",", na.strings="", header=TRUE)
zna1 <- is.na(z[,1])
zna2 <- is.na(z[,2])
znas <- zna1|zna2

zna<-z[!znas,]

cz <- chull(zna)

plot(zna)
lines(zna[c(cz, cz[1]),], col="red")

####
z<-read.table("prin3_5.txt", sep=",", na.strings="", header=TRUE)
zna1 <- is.na(z[,1])
zna2 <- is.na(z[,2])
znas <- zna1|zna2

zna<-z[!znas,]

cz <- chull(zna)

plot(zna)
lines(zna[c(cz, cz[1]),], col="red")

###

z<-read.table("prin4_5.txt", sep=",", na.strings="", header=TRUE)
zna1 <- is.na(z[,1])
zna2 <- is.na(z[,2])
znas <- zna1|zna2

zna<-z[!znas,]

cz <- chull(zna)

plot(zna)
lines(zna[c(cz, cz[1]),], col="red")