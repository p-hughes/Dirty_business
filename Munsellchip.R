#setwd("C:/Users/mnel6409/Seb Dropbox/Dropbox/BIOM Extra Tutorial")
#setwd("C:/Users/bmal4866/Desktop/TXTBIN")
setwd("C:/Users/phug7649/Desktop/TXTBIN")
## This is where the munsell colours are.
##install.packages("munsell")
ctrial <- read.table("Ctrial.txt", header=TRUE, sep=",")
Reference <- read.csv("reference.csv", header=TRUE, sep=",")

library(munsell)


###Warning!! this is a test script. It will damage the file if left in####################

#generate all munsell hues
munsell_cols <- with(hue_slice()$data, paste(hue, " ", value, "/", chroma, sep=""))
#Pick random colours
random_colours <- sample(munsell_cols, nrow(ctrial), replace=TRUE)
#Make munsell column of random colours
ctrial$class <- random_colours

##########################################################################################

#mnsl_US[mnsl_US=="*"] <- NA
#mnsl_US[mnsl_US=="N"] <- NA
#mnsl_US[mnsl_US==""] <- NA
#mnsl_US <- droplevels(mnsl_US)

## much of this data was in the factor column. Even though 
## deleted, quotes and NA's can be rememberedby the program 
## as levels. Droplevels removes these unnecesary quotes 
## and NA's.

##unsure as to why we need ggplot. Its cool though.

install.packages("ggplot2")
library(ggplot2)

##time to diverge from the munsell colour converting software.

LAB<-read.table("Ctrial.txt", sep=",", header=T)

##This x needs to have each row matched with a munsell tile that is closest in colour. 
##Euclidean distances would be the best method.


euc <- function(dat, x1, y1, z1){
  
  if(any(length(x1)!=1,length(y1)!=1,length(z1)!=1)) stop("x1, y1, or z1 should be length 1")
  
  x<-dat$L.
  y<-dat$a.
  z<-dat$b.
  
  answer <- sqrt((x-x1)^2+(y-y1)^2+(z-z1)^2)
  
  return(answer)
  
}

#y <- matrix(NA, )
##refLAB <- munslab.df
Munsell <- rep(NA,nrow(ctrial))

for(i in 1:nrow(ctrial)){
  L <- ctrial$Main_L[i]
  A <- ctrial$Main_A[i]
  B <- ctrial$Main_B[i]
  y <-euc(Reference,L,A,B)
  

  Munsell[i]<- which.min(y)
  

}
  
Reference[Munsell,]

  y<-euc(LAB,1,2,3)    
  which.min
  dist(x, mnsl_data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

library(plyr)

yay <- join(mnsl_CIELAB, munsLAB.df)
write.table(yay, "cfix_LAB_92132.txt")