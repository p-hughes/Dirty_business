##this script is to 

setwd("C:/Users/phug7649/Desktop/txtbin")
library(ggplot2)


ph<-read.csv("phinfo.txt")
ec<-read.csv("ec.txt")

##this data may be obsolete. I will add each column manually. 
#data<-read.csv("phil1.txt")
#data<-merge(data,ec, by="natural_key", all=TRUE )
#data<-(unique(data))

##some research on the nasis pH data

##there is a ph value of 35 in here. I need to remove this at some stage
#regression for Cacl2~KCl 

lmph<-lm(ph[,4]~ph[,2])
#Regression inversed
ilmph<-lm(ph[,2]~ph[,4])
summary(lmph)
#getting the regression formula
ilmph

##Simple plot
plot(ph[,2],ph[,4],xlim=c(0,15),ylim=c(0,15),xlab="CaCl2", ylab="KCl", main="pH measurement in different media")
abline(lmph, col="red")

##Awesome plot
png("hextbin.png", type="cairo", width=4196*2, height=2048*2)
d <- ggplot(ph, aes(x = ph_cacl2, y = ph_kcl))
d + geom_hex(binwidth = c(0.25, 0.25)) +
  stat_smooth(method="lm", colour="red", size=5)+
  coord_equal()+
  theme_bw()+
  labs(x=expression(pH~(CaCl[2])), y=expression(ph~(KCl)), fill="Frequency\n") + 
  theme(axis.title=element_text(size=72), 
        legend.text=element_text(size=72), 
        legend.key.size=unit(10, "cm"), 
        legend.title=element_text(size=80),
        axis.text=element_text(size=50))
dev.off()

##Applying regression to data to estimate pH
y=ifelse(is.na(ph[,2]),0.5995 + 0.9735*ph[,4],ph[,2])
ph[,2]<-y
hist(y) ##Still need to get rid of that dodgy pH value!
kcl_reg<-na.exclude(y)

##Merging ec data so pH can be accurately estimated from CaCl2
ph_ec<-merge(ph,ec, by="natural_key", all=TRUE )

##removing duplicates
sub_ph_ec<-with(ph_ec, data.frame(natural_key))
nodup_ph_ec <- which(!duplicated(sub_ph_ec))
ph_ec <- ph_ec[nodup_ph_ec,]

##Adding sp data
#replace missing pH values (1:5 in water) with paste pH values (which are roughly equivalent). 

y=ifelse(is.na(ph_ec[,3]),ph_ec[,6],ph[,3])
ph_ec[,3]<-y
head(ph_ec)
sp_ph<-na.exclude(y)

##plot carbonates before you fuck with the data!

##estimating pH from Caco3 and ec
#first, NA needs to be converted to 1 so absence of carbonates will be ignored, rather than returning NA

y=ifelse(is.na(ph_ec[,9]),1,ph_ec[,9])
check<-na.exclude(y)##check step. "check" should be the identical to y.
identical(check,y)
ph_ec[,9]<-y

##Next: convert CaCl2 pH to water pH via budi's equation.
y=ifelse(is.na(ph_ec[,3]),(ph_ec[,2]-0.14*log(ph_ec[,9]))/9,ph_ec[,3])
ec_ph<-na.exclude(y)
ph_ec[,3]<-y

## Creating a usable Caco3 column.
y=ifelse(is.na(ph_ec[,7]),ifelse(ph_ec[,3]<7.5,0,NA),ph_ec[,7])
caco3<-na.exclude(y)
ph_ec[,7]<-y





###########################################################################################################################

##fixing up pH data

#y is a bin column which will contain the results of this action.

##replacing NA with 1 so that the regression will include na values for ec
#y is a bin column which will contain the results of this action.


#y is a bin column which will contain the results of this action.




##Using composition to ensure the sand/silt clay fraction is as complete as possible
y=ifelse(is.na(data[,11]),100-data[,12]+data[,13],data[,11])
data[,11]<-y

y=ifelse(is.na(data[,12]),100-data[,11]+data[,13],data[,12])
data[,12]<-y

y=ifelse(is.na(data[,13]),100-data[,12]+data[,11],data[,13])
data[,13]<-y

#check step
data <- data[,c(1,4,6:8,11:13,26:31)]
check<-na.exclude(data)

##have a look at the CN ratio. May be able to pick up missing C or N values.

head(data)
plot(data[,6],data[,4])






# a<-ph[,1:2]
# b<-ph[,4]
# c<-cbind(a,b)
# d<-na.exclude(c)
# ##adding each data column together to see how much extra data a linear model would produce
# y=ifelse(is.na(c[,2]),c[,3],c[,2])
# e<-na.exclude(y)


# ##Estimating 
# plot(ph[,4],ph[,2],xlim=c(0.5995,15),ylim=c(0,15),ylab="CaCl2", xlab="KCl", main="pH measurement in different media")
# abline(ilmph, col="red")


















